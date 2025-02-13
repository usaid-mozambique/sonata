calc_tx_active(conn_ddb, enddate = "2024-10-26")


# IMPORT ------------------------------------------------------------------

#  import tx_active

df_tx <- dir_data %>%
  return_latest("tx_active") %>%
  read_csv()


tx_start_date <- "2024-12-01"

df_tx <- df_tx %>%
  mutate(start_date = tx_start_date)

# MUNGE -------------------------------------------------------------------

# add a function to pull the lookup tables
# default = ALL
# table = "Regimen"
# store = TRUE/FALSE;  store in Dataout

# need from FF and JL how they store lookup tables in case it is different rfom mine
# LOOKUP TBL FUNCTION --------------


df_clin <- conn_ddb %>%
  tbl("clinical_consultation") %>%
  as_tibble()

df_patient <- conn_ddb %>%
  tbl("patient") %>%
  as_tibble()

# 1) option to pull from list
df_location <- sonata::data_location_lookup

df_regimen <- sonata::data_type_id_lookup %>%
  filter(column_name == "regimen_id")

df_form <- sonata::data_type_id_lookup %>%
  filter(column_name == "form_id")

df_disp_mode <-  sonata::data_type_id_lookup %>%
  filter(column_name == "mode_dispensation_id")

# # 2) option to pull in from Documents
# document_folder <- "Documents"
#
# document_folder %>%
#   return_latest("location") %>%
#   read_csv()
#



# 1) Recode gender --------------

#chang to Portuguese
recode_gender <- function(data, gender_column) {
  df_new <- data %>%
    mutate(
      !!sym(gender_column) := case_when(
        !!sym(gender_column) %in% c("M", "m") ~ "Male",
        !!sym(gender_column) %in% c("F", "f") ~ "Female",
        TRUE ~ "Unknown"
      )
    )

  return(df_new)
}


df_tx %>%
  recode_gender(gender_column = "gender")

# 2) Calculates age (flexible to timepoint) ----------------------

# function to calculate age dynamically
calculate_age <- function(birth_date, ref_date = Sys.Date()) {

  # Calculate the difference in years
  age <- as.numeric(difftime(ref_date, birth_date, units = "weeks")) %/% 52.5
  return(age)
}

infer_date_format <- function(date_string) {
  if (grepl("^\\d{1,2}/\\d{1,2}/\\d{4}$", date_string)) {
    return("%m/%d/%Y") # Month/Day/4-digit Year
  } else if (grepl("^\\d{1,2}/\\d{1,2}/\\d{2}$", date_string)) {
    return("%m/%d/%y") # Month/Day/2-digit Year
  } else if (grepl("^\\d{4}-\\d{2}-\\d{2}$", date_string)) {
    return("%Y-%m-%d") # ISO format
  } else if (grepl("^\\d{2}-\\d{2}-\\d{4}$", date_string)) {
    return("%d-%m-%Y") # Day-Month-4-digit Year
  } else {
    stop("Unknown date format.")
  }
}

# Example usage
date_string <- "3/25/1982"
format <- infer_date_format(date_string)
print(format) # Outputs: "%m/%d/%Y"

# Apply the inferred format
as.Date(date_string, format)


# 3) Recodes age to MER age groups -------------------------------------------

# flag ages that are 14 going to 15

#function to recode age to MER age bands - maybe change this so the "recoded age" name is dynamic but this is fine for now
recode_age <- function(data, age_column) {
  df_new <- data %>%
    mutate(
      recoded_age = case_when(
        !!sym(age_column) < 1 ~ "<01",
        !!sym(age_column) >= 1 & !!sym(age_column) <= 4 ~ "01-04",
        !!sym(age_column) >= 5 & !!sym(age_column) <= 9 ~ "05-09",
        !!sym(age_column) >= 10 & !!sym(age_column) <= 14 ~ "10-14",
        !!sym(age_column) >= 15 & !!sym(age_column) <= 19 ~ "15-19",
        !!sym(age_column) >= 20 & !!sym(age_column) <= 24 ~ "20-24",
        !!sym(age_column) >= 25 & !!sym(age_column) <= 29 ~ "25-29",
        !!sym(age_column) >= 30 & !!sym(age_column) <= 34 ~ "30-34",
        !!sym(age_column) >= 35 & !!sym(age_column) <= 39 ~ "35-39",
        !!sym(age_column) >= 40 & !!sym(age_column) <= 44 ~ "40-44",
        !!sym(age_column) >= 45 & !!sym(age_column) <= 49 ~ "45-49",
        !!sym(age_column) >= 50 ~ "50+",
        TRUE ~ NA_character_
      )
    )

  return(df_new)
}

tx_start_date <- "2024-12-01"

#birthdate is transposing incorrectly
df_tx %>%
  mutate(birthdate2 = as.Date(birthdate, "%m/%d/%Y")) %>%
  mutate(
    age_at_start = calculate_age(birth_date = birthdate2, ref_date = tx_start_date),
    #age_at_encounter = calculate_age(birth_date = birthdate, ref_date = encounter_date),
    age_today = calculate_age(birth_date = birthdate2, ref_date = lubridate::today())) %>%
  recode_age(age_column = "age_at_start") %>%
  count(birthdate,birthdate2, age_today, age_at_start, recoded_age)


# 4) Recode location_uid ------------------------------------------------------

recode_location  <- function(data, df_location) {

  # Unit test: Ensure both dataframes have "location_uuid" column
  testthat::test_that("Both input data and df_location must have the column 'location_uuid'", {
    testthat::expect_true("location_uuid" %in% colnames(data),
                info = "The main dataset is missing 'location_uuid'")
    testthat::expect_true("location_uuid" %in% colnames(df_location),
                info = "The location dataset is missing 'location_uuid'")
  })

  df_new <- data %>%
    dplyr::left_join(sonata::data_location_lookup, by = c("location_uuid"))

  return(df_new)

}

df_tx %>%
  recode_location(sonata::data_location_lookup)


# 5) Recodes regimen_id -----------------------------------------------------

# just do a normal left_join() with type_id_lookup


recode_regimen  <- function(data, df_regimen, keep_id = FALSE) {

  # Unit test: Ensure both dataframes have "regimen_id" column
  testthat::test_that("Input data must have the column 'regimen_id' and df_regimen must have `id_type_lookup`", {
    testthat::expect_true("regimen_id" %in% colnames(data),
                info = "The main dataset is missing 'regimen_id'")
    testthat::expect_true("id_type_lookup" %in% colnames(df_regimen),
                info = "The location dataset is missing 'id_type_lookup'")
  })

  df_new <- data %>%
    dplyr::left_join(df_regimen %>% dplyr::select(id_type_lookup, id_type_desc), by = c("regimen_id" = "id_type_lookup")) %>%
    dplyr::rename(regimen_name = id_type_desc)

  if (keep_id == FALSE) {
    df_new <- df_new %>%
      dplyr::relocate(regimen_name, .after = regimen_id) %>%
      dplyr::select(-regimen_id)
  }



  return(df_new)

}

df_tx %>%
  recode_regimen(df_regimen, TRUE)

# 6) Recodes form_id -----------------------------------------------------


recode_form  <- function(data, df_form, keep_id = FALSE) {

  # Unit test: Ensure both dataframes have "form_id" column
  testthat::test_that("Input data must have the column 'form_id' and df_form must have `id_type_lookup`", {
    testthat::expect_true("form_id" %in% colnames(data),
                info = "The main dataset is missing 'form_id'")
    testthat::expect_true("id_type_lookup" %in% colnames(df_form),
                info = "The location dataset is missing 'id_type_lookup'")
  })

  df_new <- data %>%
    dplyr::left_join(df_form %>% dplyr::select(id_type_lookup, id_type_desc), by = c("form_id" = "id_type_lookup")) %>%
    dplyr::rename(form_name = id_type_desc)

  if (keep_id == FALSE) {
    df_new <- df_new %>%
      dplyr::relocate(form_name, .after = form_id) %>%
      dplyr::select(-form_id)
  }

  return(df_new)

}

df_tx %>%
  recode_form(df_form, TRUE)

# 7) Recodes mode_dispensation_id -----------------------------------------------


recode_disp_mode  <- function(data, df_disp_mode, keep_id = FALSE) {

  # Unit test: Ensure both dataframes have "location_uuid" column
  testthat::test_that("Input data must have the column 'mode_dispensation_id' and df_disp_mode must have `id_type_lookup`", {
    testthat::expect_true("mode_dispensation_id" %in% colnames(data),
                info = "The main dataset is missing 'mode_dispensation_id'")
    testthat::expect_true("id_type_lookup" %in% colnames(df_disp_mode),
                info = "The location dataset is missing 'id_type_lookup'")
  })

  df_new <- data %>%
    dplyr::left_join(df_disp_mode %>% dplyr::select(id_type_lookup, id_type_desc), by = c("mode_dispensation_id" = "id_type_lookup")) %>%
    dplyr::rename(mode_dispensation_name = id_type_desc)

  if (keep_id == FALSE) {
    df_new <- df_new %>%
      dplyr::relocate(mode_dispensation_name, .after = mode_dispensation_id) %>%
      dplyr::select(-mode_dispensation_id)
  }


  return(df_new)

}

df_tx %>%
  recode_disp_mode(df_disp_mode, TRUE) %>%
  count(mode_dispensation_name)


# FUNCTION ---------------------------------------------------------------


#input df to test
df_tx_input <- df_tx %>%
  mutate(birthdate = as.Date(birthdate, "%m/%d/%Y")) %>%
  mutate(
    #age_at_consult = calculate_age(birth_date = birthdate, ref_date = consultation_date),
    #age_at_encounter = calculate_age(birth_date = birthdate, ref_date = encounter_date),
    age_at_start = calculate_age(birth_date = birthdate, ref_date = tx_start_date))


# Main function to process user-specified recodings
process_recoding <- function(data, columns_to_recode, options = list()) {

  # Loop through the specified columns
  for (col in columns_to_recode) {
    # Call the corresponding subfunction based on the column name
    if (col == "age" && !is.null(options$age_column)) {
      data <- recode_age(data, options$age_column)
    } else if (col == "gender" && !is.null(options$gender_column)) {
      data <- recode_gender(data, options$gender_column)
    } else if (col == "location" && !is.null(options$df_location)) {
      data <- recode_location(data, options$df_location)
    } else if (col == "regimen" && !is.null(options$df_regimen)) {
      data <- recode_regimen(data, options$df_regimen)
    } else if (col == "form" && !is.null(options$df_form)) {
      data <- recode_form(data, options$df_form)
    } else if (col == "disp_mode" && !is.null(options$df_disp_mode)) {
      data <- recode_disp_mode(data, options$df_disp_mode)
    } else {
      message(paste("No recoding function or missing parameter for column:", col))
    }
  }

  return(data)
}


process_recoding(
  data = df_tx_input,
  columns_to_recode = c("age", "gender", "location", "regimen", "form", "disp_mode"),
  options = list(
    age_column = "age_at_start",
    gender_column = "gender",
    df_location = df_location,
    df_regimen = df_regimen,
    df_form = df_form,
    df_disp_mode = df_disp_mode
  )
)


#questions
# do we want to reassign? Yes
# is the input always standard in the tx_active? - if not, need to build in unit tests to check for vars
# where does this live?

# Main function to process recodings automatically
process_recoding2 <- function(data, options = list()) {

  # Automatically detect columns based on naming patterns
  columns_to_recode <- list(
    age = grep("age", colnames(data), value = TRUE),
    gender = "gender",
    location_uuid = "location_uuid",
    regimen_id = "regimen_id",
    form_id = "form_id",
    mode_dispensation_id = "mode_dispensation_id"
  )

  # Loop through detected columns and apply corresponding recode functions
  for (col_type in names(columns_to_recode)) {
    col_name <- columns_to_recode[[col_type]]

    # Skip if the column does not exist
    if (!col_name %in% colnames(data)) {
      message(paste("Column", col_name, "for", col_type, "not found in the data frame. Skipping."))
      next
    }

    # Call the corresponding subfunction based on the column type
    if (col_type == "age" && !is.null(options$age_column)) {
      data <- recode_age(data, options$age_column)
    } else if (col_type == "gender" && !is.null(options$gender_column)) {
      data <- recode_gender(data, options$gender_column)
    } else if (col_type == "location_uuid" && !is.null(options$df_location)) {
      data <- recode_location(data, options$df_location)
    } else if (col_type == "regimen_id" && !is.null(options$df_regimen)) {
      data <- recode_regimen(data, options$df_regimen)
    } else if (col_type == "form_id" && !is.null(options$df_form)) {
      data <- recode_form(data, options$df_form)
    } else if (col_type == "mode_dispensation_id" && !is.null(options$df_disp_mode)) {
      data <- recode_disp_mode(data, options$df_disp_mode)
    } else {
      message(paste("No recoding function or missing parameter for column type:", col_type))
    }
  }

  return(data)
}

# Example call to process_recoding

# arg for local or db
#  think about a way to connect to lookups within process_recode rather than store to env

process_recoding2(
  data = df_tx_input,
  options = list(
    age_column = "age_at_start",
    gender_column = "gender",
    df_location = sonata::data_location_lookup,
    df_regimen = df_regimen,
    df_form = df_form,
    df_disp_mode = df_disp_mode
  )
)


# MORE AUTOMATED -------------------------------------------------------------

# Main function to process recodings automatically
process_recoding3 <- function(data) {

  # Automatically detect columns based on naming patterns
  age_column <- grep("age", colnames(data), value = TRUE)
  gender_column <- "gender"
  location_column <- "location_uuid"
  regimen_column <- "regimen_id"
  form_column <- "form_id"
  disp_mode_column <- "mode_dispensation_id"

  # Apply recoding functions only if the respective column exists
  if (length(age_column) > 0) {
    data <- recode_age(data, age_column[1]) # Use the first match for "age"
  } else {
    message("No column matching 'age' found. Skipping age recoding.")
  }

  if (gender_column %in% colnames(data)) {
    data <- recode_gender(data, gender_column)
  } else {
    message("No column 'gender' found. Skipping gender recoding.")
  }

  if (location_column %in% colnames(data)) {
    data <- recode_location(data, location_column)
  } else {
    message("No column 'location_uuid' found. Skipping location recoding.")
  }

  if (regimen_column %in% colnames(data)) {
    data <- recode_regimen(data, regimen_column)
  } else {
    message("No column 'regimen' found. Skipping regimen recoding.")
  }

  if (form_column %in% colnames(data)) {
    data <- recode_form(data, form_column)
  } else {
    message("No column 'form' found. Skipping form recoding.")
  }

  if (disp_mode_column %in% colnames(data)) {
    data <- recode_disp_mode(data, disp_mode_column)
  } else {
    message("No column 'disp_mode' found. Skipping disp_mode recoding.")
  }

  return(data)
}

# Example call to process_recoding
processed_data <- process_recoding3(df_tx_input)





