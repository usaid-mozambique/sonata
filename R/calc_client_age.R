#' Calcular a idade do cliente em anos
#'
#' @param birth_date Coluna do quadro de dados utilizada para introduzir a data de nascimento do cliente (default = birthdate)
#' @param ref_date Data de referência para calcular a idade do cliente (default = Sys.Date())
#'
#' @return Quadro de dados com a idade do cliente em anos
#' @export
#'
#' @examples
#' \dontrun{
#'
#'  df <- calculate_age(df)}

calc_client_age <- function(birth_date, ref_date = Sys.Date()) {

  # Calculate the difference in years
  age <- as.numeric(difftime(ref_date, birth_date, units = "weeks")) %/% 52.5

  return(age)

}
