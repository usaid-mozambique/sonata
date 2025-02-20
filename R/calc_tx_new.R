#' Query clientes iniciados em TARV
#'
#' @param con Ligação à base de dados MozART 2.0
#' @param opendate Data de abertura do período (introduzir como “AAAA-MM-DD”)
#' @param enddate Data de fecho do período (introduzir como “AAAA-MM-DD”)
#' @param filter_by_location Lógico. Se `TRUE`, o query corre sobre uma unidade sanitária específica.  Se `FALSE`,  o query corre sobre todas as unidade sanitária contidas na base de dados
#' @param location_uuid location_uuid da unidade sanitária a filtrar quando filter_by_location é definido como `TRUE`
#'
#' @return Um quadro de dados contendo uma listagem individual dos pacientes iniciados em TARV
#' @export
#'
#' @examples
#' \dontrun{
#'
#'  df <- calc_tx_new(
#'           con,
#'           opendate = "2024-06-21",
#'           enddate = "2024-09-20",
#'           filter_by_location = TRUE,
#'           location_uuid = location_meripo)}

calc_tx_new <- function(con, opendate, enddate, filter_by_location = FALSE, location_uuid = '4be5f1a9-832c-4717-be41-ef4b6311c0ef') {

  # conditionally include the location filter
  location_condition <- if (filter_by_location) {
    paste("o.location_uuid = '", location_uuid, "' AND", sep = "")
  } else {
    ""
  }

  # define query
  sql_query <- paste(
    "SELECT
    o.location_uuid,
    o.encounter_uuid,
    o.patient_uuid,
    p.birthdate,
    p.gender,
    o.form_id,
    o.encounter_date,
    o.observation_date,
    o.value_datetime

  FROM observation o",
    "JOIN patient p ON o.patient_uuid = p.patient_uuid",  # Join with the patient table
    "WHERE",
    location_condition,  # Dynamic location condition
    "o.concept_id = 1190 AND o.observation_date >= '2018-01-01'"
  )

  df <- DBI::dbGetQuery(con, sql_query) |>
    dplyr::mutate(
      art_start_date = dplyr::case_when(
        value_datetime < as.Date("1980-01-01") ~ encounter_date,
        observation_date < as.Date("1980-01-01") ~ encounter_date,
        TRUE ~ value_datetime
      )
    ) |>
    dplyr::arrange(patient_uuid, art_start_date) |>
    dplyr::group_by(patient_uuid) |>
    dplyr::slice_head(n = 1) |>
    dplyr::ungroup() |>
    dplyr::filter(art_start_date > opendate & art_start_date < enddate) |>
    dplyr::mutate(
      enddate = lubridate::as_date(lubridate::ymd(enddate)),
      dplyr::across(c(observation_date, value_datetime, art_start_date), ~lubridate::as_date(lubridate::ymd_hms(.)))) |>
    calc_age_var(ref_date = enddate) |>
    dplyr::select(!c(encounter_date, observation_date, value_datetime)) |>
    dplyr::relocate(age, .after = birthdate)

  return(df)

}
