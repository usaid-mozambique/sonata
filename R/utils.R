#' MozART 2.0 Utilities
#'
#' @param db_name Database name
#' @param db_user Database user
#' @param db_pass Database password
#' @param db_host Database host
#' @param db_port Database port
#'
#' @return A connection to the database
#' @export
#'
#' @examples
#' \dontrun{
#'
#'  df <- mysql_connection()}

mysql_connection <- function(db_name,
                             db_user,
                             db_pass,
                             db_host = "localhost",
                             db_port = 3306) {
  ## Connections
  DBI::dbConnect(
    drv = RMySQL::MySQL(),
    host = db_host,
    port = as.integer(db_port),
    dbname = db_name,
    username = db_user,
    password = db_pass
  )
}
