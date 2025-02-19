#' Detectar casos de transição etária de clients
#'
#' @param df Quadro de dados contendo a data de nasicmento do cliente
#' @param ref_date Data de referência para calcular a idade projectada do cliente
#'
#' @return Quadro de dados com variáveis idade, idade projectada, faixa etária, faixa etária projectada, e colunas que indicam casos de transição etária
#' @export
#'
#' @examples
#' \dontrun{
#'
#'  df <- project_age_out(df)}

project_age_out <- function(df, ref_date) {

  df <- df |>
    recode_age() |>
    calc_age_var(ref_date = ref_date,
                 variable_name = "age_proj") |>
    recode_age(age_column = "age_proj",
               variable_name = "age_band_proj") |>
    dplyr::mutate(age_out_ped = dplyr::case_when(age < 15 & age_proj >= 15 ~ 1,
                                                 .default = 0),
                  age_out_band = dplyr::case_when(age_band != age_band_proj ~ 1,
                                                  .default = 0)) |>
    dplyr::relocate(tidyselect::starts_with("age"), .after = birthdate)

  return(df)

}
