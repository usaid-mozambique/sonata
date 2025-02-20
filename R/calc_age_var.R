#' Calcular a idade do cliente
#'
#' @param df Quadro de dados contendo a data de nasicmento do cliente
#' @param birth_date Coluna do quadro de dados utilizada para introduzir a data de nascimento do cliente (default = birthdate)
#' @param ref_date Data de referência para calcular a idade do cliente (default = Sys.Date())
#' @param variable_name Nome da variável que será criada no quadro de dados com a idade do cliente em anos (default = 'age')
#'
#' @return Quadro de dados com a idade do cliente em anos
#' @export
#'
#' @examples
#' \dontrun{
#'
#'  df <- calc_age_var(df)}

calc_age_var <- function(df, birth_date = "birthdate", ref_date = Sys.Date(), variable_name = "age") {

  # Ensure birth_date is treated as a Date column
  df <- df |>
    dplyr::mutate(
      !!variable_name := as.numeric(difftime(ref_date, as.Date(.data[[birth_date]]), units = "weeks")) %/% 52.5
    )

  return(df)

}
