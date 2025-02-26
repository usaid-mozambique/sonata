#' Query uuids dos novos inícios ao TARV
#'
#' @description `pull_new_uuid()` devolve um vector dos uuid's dos clientes activos em TARV a partir de uma data definida pelo utilizador
#'
#' @param con Ligação à base de dados MozART 2.0
#' @param opendate Data de abertura do período (introduzir como “AAAA-MM-DD”)
#' @param enddate Data de fecho do período (introduzir como “AAAA-MM-DD”)
#' @param filter_by_location Se `TRUE`, o query corre sobre uma unidade sanitária específica.  Se `FALSE`, o query corre sobre todas as unidade sanitária contidas no MozART 2.0
#' @param location_uuid location_uuid da unidade sanitária a filtrar quando filter_by_location é definido como `TRUE`
#'
#' @return `pull_new_uuid` devolve um vector contendo os patient_uuid unicos dos pacientes activos em TARV
#' @export
#'
#' @examples
#' \dontrun{
#'
#' # Caso de uso simples executado sobre todas US
#' uuids <- pull_new_uuid(con, opendate = "2024-06-21", enddate = "2024-09-20", filter_by_location = FALSE)
#'
#' # # Caso de uso simples executado sobre uma US
#' uuids <- pull_new_uuid(con, opendate = "2024-06-21", enddate = "2024-09-20",
#'                        filter_by_location = TRUE, location_uuid = 'e3eb1a1b-be07-4af2-9360-5d7046910576')
#' }

pull_new_uuid <- function(con,
                          opendate,
                          enddate,
                          filter_by_location = TRUE,
                          location_uuid = 'e5f01eee-2392-49b4-a5bf-5cf593fc8f21') {

  # Conditionally include the location filter
  location_condition <- if (filter_by_location) {
    paste("o.location_uuid = '", location_uuid, "' AND", sep = "")
  } else {
    ""
  }

  # Define SQL query
  sql_query <- paste(
    "SELECT o.patient_uuid, o.value_datetime, o.encounter_date, o.observation_date
    FROM observation o
    WHERE",
    location_condition,  # Dynamic location condition
    "o.concept_id = 1190 AND o.observation_date >= '2018-01-01'"
  )

  # Execute the SQL query and extract unique patient UUIDs
  unique_patient_uuids <- DBI::dbGetQuery(con, sql_query) |>
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
    dplyr::pull(patient_uuid) |>
    unique()

  # Format as a comma-separated string
  patient_uuids_str <- paste(sprintf("'%s'", unique_patient_uuids), collapse = ",")

  return(patient_uuids_str)

}
