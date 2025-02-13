pull_lookup_table <- function(db, table = "All", store = TRUE) {

  # Fetch all table names from the database
  tbl_list <- db %>%
    DBI::dbGetQuery("SHOW TABLES") %>%
    tibble::as_tibble() %>%
    dplyr::filter(stringr::str_detect(name, "lookup")) %>%
    dplyr::pull(name)

  # Check if the user requested all tables
  if (table == "All") {
    results <- list()

    for (tbl in tbl_list) {
      message(glue::glue("Processing table: {tbl}"))

      # Fetch the table and convert it to a tibble
      df <- db %>%
        tbl(tbl) %>%
        tibble::as_tibble()

      # Optionally store the table as a CSV file
      if (store) {
        readr::write_delim(df, glue::glue("Documents/df_{tbl}_lookup.csv"), delim = ",")
      }

      # Store the dataframe in the results list
      results[[tbl]] <- df
    }

    return(results) # Return all dataframes in a named list
  } else {
    # If a specific table is requested, fetch and process it
    message(glue::glue("Processing table: {table}"))

    df <- db %>%
      tbl(table) %>%
      tibble::as_tibble()

    if (store) {
      readr::write_delim(df, glue::glue("Documents/df_{table}_lookup.csv"), delim = ",")
    }

    return(df)
  }
}
