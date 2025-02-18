#' Ligar ao MozART 2.0
#'
#' @param db_name Nome da base de dados
#' @param db_user Utilizador da base de dados
#' @param db_pass Senha da base de dados
#' @param db_host Host da base de dados
#' @param db_port Porta da base de dados
#'
#' @return Uma conexão à base de dados
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
