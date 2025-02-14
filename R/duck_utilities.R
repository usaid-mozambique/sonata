#' @title Initiate duckdb connection
#'
#' @param dbfile database file
#' @param ... test fix
#'
duck_connection <- function(dbfile = ":memory:", ...) {
  # Connection
  conn = NULL

  # (stringr::str_detect(dbfile, ".duckdb$|.db$") &
  #     !fs::file_exists(dbfile))
  # ))

  # Validation
  if(dbfile != ":memory:" &
     stringr::str_detect(dbfile, ".duckdb$|.db$", negate = TRUE)) {
    usethis::ui_stop(stringr::str_glue("DB [{dbfile}] is not valid or does not exists. Double check the input"))
  }

  # Establish connection
  conn = tryCatch(
    duckdb::dbConnect(
      drv = duckdb::duckdb(dbdir = dbfile),
      ...
    ),
    warning = function(wrn){stop(conditionMessage(wrn))},
    error = function(err){stop(conditionMessage(err))},
    finally = print("QUACK QUACK!!!")
  )

  return(conn)
}

#' @title shutdown duckdb connection
#'
#' @param dbfile database connection
#'
duck_stop <- function(dbfile = ":memory:") {
  tryCatch(
    duckdb::duckdb_shutdown(drv = duckdb::duckdb(dbdir = ddb_file)),
    warning = function(wrn){stop(conditionMessage(wrn))},
    error = function(err){stop(conditionMessage(err))},
    finally = print("QUACK QUACK!!!")
  )
}

#' @title List Duck DB Tables
#'
#' @param conn database connection
#'
duck_tables <- function(conn) {
  DBI::dbExecute(
    conn,
    statement = "SHOW TABLES;"
  )
}
