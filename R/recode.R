#' Recodificar location_uuid's
#'
#' @description
#' `recode_location()` devolve um conjunto de informações mais facilmente compreensíveis sobre a localização
#'
#' @param df Quadro de dados contendo a variável a recodificar
#' @param df_location Objecto de tabela de pesquisa usado para recodificação
#'
#' @return `recode_location` devolve um quadro de dados com a coluna location_uuid recodificada
#' @export
#'
#' @examples
#' \dontrun{
#'
#'  df <- recode_location(
#'           df = df,
#'           df_location = data_location_lookup)}

recode_location  <- function(df, df_location = data_location_lookup) {

  # Unit test: Ensure both dataframes have "location_uuid" column
  testthat::test_that("Both input df and df_location must have the column 'location_uuid'", {
    testthat::expect_true("location_uuid" %in% colnames(df),
                info = "The main dataset is missing 'location_uuid'")
    testthat::expect_true("location_uuid" %in% colnames(df_location),
                info = "The location dataset is missing 'location_uuid'")
  })

  df_location = data_location_lookup |>
    dplyr::select(location_uuid,
                  sisma_id,
                  datim_id,
                  province,
                  district,
                  us = mozart_name)

  df <- df %>%
    dplyr::left_join(df_location,
                     by = c("location_uuid")) |>
    dplyr::relocate(c("location_uuid", "sisma_id", "datim_id", "province", "district", "us"),
                    .before = tidyselect::everything())

  return(df)

}


#' Recodificar idade
#'
#' @description
#' `recode_age()` devolve a idade e a faixa etária do cliente
#'
#' @param df Quadro de dados contendo a variável a recodificar
#' @param age_column Variável no quadro de dados introduzido contendo a idade do cliente
#' @param variable_name Nome da variável que será criada no quadro de dados com a faixa etária do client (default = 'age_band')
#'
#' @return `recode_age` devolve um quadro de dados com a coluna idade recodificada
#' @export
#'
#' @examples
#' \dontrun{
#'
#'  df <- recode_age(
#'           df = df,
#'           age_column = "age")}

recode_age <- function(df, age_column = "age", variable_name = "age_band") {

  df <- df |>
    dplyr::mutate(
      !!dplyr::sym(variable_name) := dplyr::case_when(
        !!dplyr::sym(age_column) < 1 ~ "<01",
        !!dplyr::sym(age_column) >= 1 & !!dplyr::sym(age_column) <= 4 ~ "01-04",
        !!dplyr::sym(age_column) >= 5 & !!dplyr::sym(age_column) <= 9 ~ "05-09",
        !!dplyr::sym(age_column) >= 10 & !!dplyr::sym(age_column) <= 14 ~ "10-14",
        !!dplyr::sym(age_column) >= 15 & !!dplyr::sym(age_column) <= 19 ~ "15-19",
        !!dplyr::sym(age_column) >= 20 & !!dplyr::sym(age_column) <= 24 ~ "20-24",
        !!dplyr::sym(age_column) >= 25 & !!dplyr::sym(age_column) <= 29 ~ "25-29",
        !!dplyr::sym(age_column) >= 30 & !!dplyr::sym(age_column) <= 34 ~ "30-34",
        !!dplyr::sym(age_column) >= 35 & !!dplyr::sym(age_column) <= 39 ~ "35-39",
        !!dplyr::sym(age_column) >= 40 & !!dplyr::sym(age_column) <= 44 ~ "40-44",
        !!dplyr::sym(age_column) >= 45 & !!dplyr::sym(age_column) <= 49 ~ "45-49",
        !!dplyr::sym(age_column) >= 50 & !!dplyr::sym(age_column) <= 54 ~ "50-54",
        !!dplyr::sym(age_column) >= 55 & !!dplyr::sym(age_column) <= 59 ~ "55-59",
        !!dplyr::sym(age_column) >= 60 & !!dplyr::sym(age_column) <= 64 ~ "60-64",
        !!dplyr::sym(age_column) >= 65 ~ "65+",
        TRUE ~ NA_character_
      )
    ) |>

    dplyr::relocate(!!dplyr::sym(variable_name), .after = age_column)

  return(df)

}

#' Recodificar sexo
#'
#' @description
#' `recode_sex()` devolve o sexo do cliente escrito por extenso
#'
#' @param df Quadro de dados contendo a variável a recodificar
#' @param sex_column Variável no quadro de dados introduzido contendo o sexo do cliente
#'
#' @return `recode_sex` devolve um quadro de dados com a coluna sexo recodificada
#' @export
#'
#' @examples
#' \dontrun{
#'
#'  df <- recode_sex(
#'           df = df,
#'           sex_column = "sex")}

recode_sex <- function(df, sex_column = "sex") {
  df <- df %>%
    dplyr::mutate(
      !!dplyr::sym(sex_column) := dplyr::case_when(
        !!dplyr::sym(sex_column) %in% c("M", "m") ~ "Male",
        !!dplyr::sym(sex_column) %in% c("F", "f") ~ "Female",
        TRUE ~ "Unknown"
      )
    )

  return(df)

}


#' Recodificar regimen TARV
#'
#' @description
#' `recode_regimen()` devolve uma apresentação mais compreensível do regime ART do cliente
#'
#' @param df Quadro de dados contendo a variável a recodificar
#' @param df_regimen  Objecto de tabela de pesquisa usado para recodificação
#' @param keep_id Manter a coluna regimen_id após a recodificação (Logical T/F)
#'
#' @return `recode_regimen` devolve um quadro de dados com a coluna regimen recodificada
#' @export
#'
#' @examples
#' \dontrun{
#'
#'  df <- recode_regimen(
#'           df = df,
#'           df_regimen = data_type_id_lookup,
#'           keep_id = FALSE)}

recode_regimen  <- function(df, df_regimen = data_type_id_lookup, keep_id = FALSE) {

  df_regimen <- data_type_id_lookup |>
    dplyr::filter(table_name == "medication")

  # Unit test: Ensure both dataframes have "regimen_id" column
  testthat::test_that("Input df must have the column 'regimen_id' and df_regimen must have `id_type_lookup`", {
    testthat::expect_true("regimen_id" %in% colnames(df),
                info = "The main dataset is missing 'regimen_id'")
    testthat::expect_true("id_type_lookup" %in% colnames(df_regimen),
                info = "The location dataset is missing 'id_type_lookup'")
  })

  df <- df %>%
    dplyr::left_join(df_regimen %>% dplyr::select(id_type_lookup, id_type_desc), by = c("regimen_id" = "id_type_lookup")) %>%
    dplyr::rename(regimen_name = id_type_desc)

  if (keep_id == FALSE) {
    df <- df %>%
      dplyr::relocate(regimen_name, .after = regimen_id) %>%
      dplyr::select(-regimen_id)
  }

  return(df)

}


#' Recodificar modo de dispensa TARV
#'
#' @description
#' `recode_disp_mode()` retorna uma apresentação mais compreensível do modo de dispensa do ARV
#'
#' @param df Quadro de dados contendo a variável a recodificar
#' @param df_disp_mode  Objecto de tabela de pesquisa usado para recodificação
#' @param keep_id Manter a coluna regimen_id após a recodificação (Logical T/F)
#'
#' @return `recode_disp_mode` devolve um quadro de dados com a coluna regimen recodificada
#' @export
#'
#' @examples
#' \dontrun{
#'
#'  df <- recode_disp_mode(
#'           df = df,
#'           df_disp_mode = data_type_id_lookup,
#'           keep_id = FALSE)}

recode_disp_mode  <- function(df, df_disp_mode = data_type_id_lookup, keep_id = FALSE) {

  # Unit test: Ensure both dataframes have "location_uuid" column
  testthat::test_that("Input df must have the column 'mode_dispensation_id' and df_disp_mode must have `id_type_lookup`", {
    testthat::expect_true("mode_dispensation_id" %in% colnames(df),
                          info = "The main dataset is missing 'mode_dispensation_id'")
    testthat::expect_true("id_type_lookup" %in% colnames(df_disp_mode),
                          info = "The location dataset is missing 'id_type_lookup'")
  })

  df_disp_mode <- data_type_id_lookup |>
    dplyr::filter(column_name == "mode_dispensation_id")

  df <- df %>%
    dplyr::left_join(df_disp_mode %>% dplyr::select(id_type_lookup, id_type_desc), by = c("mode_dispensation_id" = "id_type_lookup")) %>%
    dplyr::rename(mode_dispensation_name = id_type_desc)

  if (keep_id == FALSE) {
    df <- df %>%
      dplyr::relocate(mode_dispensation_name, .after = mode_dispensation_id) %>%
      dplyr::select(-mode_dispensation_id)
  }


  return(df)

}


#' Recodificar formulario primario
#'
#' @description
#' `recode_form()` devolve uma apresentação mais compreensível do documento de fonte primária
#'
#' @param df Quadro de dados contendo a variável a recodificar
#' @param df_form  Objecto de tabela de pesquisa usado para recodificação
#' @param keep_id Manter a coluna regimen_id após a recodificação (Logical T/F)
#'
#' @return `recode_form` devolve um quadro de dados com a coluna regimen recodificada
#' @export
#'
#' @examples
#' \dontrun{
#'
#'  df <- recode_form(
#'           df = df,
#'           df_form = data_type_id_lookup,
#'           keep_id = FALSE)}

recode_form  <- function(df, df_form = data_type_id_lookup, keep_id = FALSE) {

  # Unit test: Ensure both dataframes have "form_id" column
  testthat::test_that("Input df must have the column 'form_id' and df_form must have `id_type_lookup`", {
    testthat::expect_true("form_id" %in% colnames(df),
                          info = "The main dataset is missing 'form_id'")
    testthat::expect_true("id_type_lookup" %in% colnames(df_form),
                          info = "The location dataset is missing 'id_type_lookup'")
  })

  df_form <- data_type_id_lookup |>
    dplyr::filter(column_name == "form_id")

  df <- df %>%
    dplyr::left_join(df_form %>% dplyr::select(id_type_lookup, id_type_desc), by = c("form_id" = "id_type_lookup")) %>%
    dplyr::rename(form_name = id_type_desc)

  if (keep_id == FALSE) {
    df <- df %>%
      dplyr::relocate(form_name, .after = form_id) %>%
      dplyr::select(-form_id)
  }

  return(df)

}


#' Recodificar colunas
#'
#' @description
#' `recode_cols()` devolve várias colunas recodificadas, tal como especificado pela entrada “cols” fornecida pelo utilizador
#'
#' @description
#' `recode_cols` reatribui valores para colunas codificadas em quadros de dados MozART 2.0 comuns
#'
#' @param df Quadro de dados contendo colunas a serem recodificadas
#' @param cols Vector de strings contendo os nomes das colunas a recodificar
#' @param options Lista de parâmetros opcionais para personalizar a recodificação
#'
#' @return `recode_cols` devolve um quadro de dados com colunas recodificadas
#' @export
#'
#' @examples
#' \dontrun{
#'  # Caso de uso para recodificar dispensation_id, regimen_id e location_uuid
#'  df <- recode_cols(
#'           df = df,
#'           cols = c("mode_dispensation_id",
#'                    "regimen_id",
#'                    "location_uuid"))}

recode_cols <- function(df,
                        cols = c("mode_dispensation_id",
                                 "regimen_id",
                                 "location_uuid",
                                 "age",
                                 "form_id",
                                 "sex"),
                        options = list(
                          df_disp_mode = data_type_id_lookup,
                          df_regimen = data_type_id_lookup,
                          df_form = data_type_id_lookup,
                          df_location = data_location_lookup,
                          age_column = "age",
                          sex_column = "sex")
)

{

  # Loop through the specified columns
  for (col in cols) {
    # Call the corresponding subfunction based on the column name
    if (col == "age" && !is.null(options$age_column)) {
      df <- recode_age(df, options$age_column)
    } else if (col == "sex" && !is.null(options$sex_column)) {
      df <- recode_sex(df, options$sex_column)
    } else if (col == "location_uuid" && !is.null(options$df_location)) {
      df <- recode_location(df, options$df_location)
    } else if (col == "regimen_id" && !is.null(options$df_regimen)) {
      df <- recode_regimen(df, options$df_regimen)
    } else if (col == "mode_dispensation_id" && !is.null(options$df_disp_mode)) {
      df <- recode_disp_mode(df, options$df_disp_mode)
    } else if (col == "form_id" && !is.null(options$df_form)) {
      df <- recode_form(df, options$df_form)
    } else {
      message(paste("No recoding function or missing parameter for column:", col))
    }
  }

  return(df)

}
