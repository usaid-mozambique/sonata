#' Passar de forma segura credenciais e estabelecer coneção ao MozART 2.0
#'
#' @description
#' `mysql_connection()` cria uma ligação com o MozART 2.0 com base num conjunto de credenciais armazenadas de forma segura e definidas pelo utilizador
#'
#' @param db_name Nome da base de dados
#' @param db_user Utilizador da base de dados
#' @param db_pass Senha da base de dados
#' @param db_host Host da base de dados
#' @param db_port Porta da base de dados
#'
#' @return Uma conexão ao MozART 2.0
#' @export
#'
#' @examples
#' \dontrun{
#'
#' con <- mysql_connection(db_name = acct_db$dbname,
#'                         db_user = acct_db$username,
#'                         db_pass = acct_db$password,
#'                         db_host = acct_db$host,
#'                         db_port = acct_db$port)}

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
