#' Query um vector de patient_uuid deduplicado para pacientes activos no TARV
#'
#' @param con Ligação à base de dados MozART 2.0
#' @param enddate Data de fecho do período (introduzir como “AAAA-MM-DD”)
#' @param defaulter_tolerance Número de dias de tolerância antes dos faltosos serem considerados inactivos (abandonos)
#' @param filter_by_location Lógico. Se `TRUE`, o query corre sobre uma unidade sanitária específica.  Se `FALSE`,  o query corre sobre todas as unidade sanitária contidas na base de dados
#' @param location_uuid location_uuid da unidade sanitária a filtrar quando filter_by_location é definido como `TRUE`
#'
#' @return `fetch_active_uuid` devolve um vector contendo os patient_uuid unicos dos pacientes activos em TARV
#' @export
#'
#' @examples
#' \dontrun{
#'
#'  df <- fetch_active_uuid(
#'           con,
#'           enddate = '2024-09-20',
#'           defaulter_tolerance = 28,
#'           filter_by_location = TRUE,
#'           location_uuid = location_meripo)}

fetch_active_uuid <- function(con, enddate, defaulter_tolerance = 28, filter_by_location = FALSE, location_uuid = '4be5f1a9-832c-4717-be41-ef4b6311c0ef') {

  # ARV pickup cutoff definitions and ceilings.
  next_pickup_date_cutoff <- as.character(as.Date(enddate) - lubridate::days(defaulter_tolerance))
  medication_pickup_date_cutoff <- as.character(as.Date(enddate) - lubridate::days(defaulter_tolerance))
  ceiling <- as.character(as.Date(enddate) + lubridate::years(1))

  # Partitioning based on calendar year
  partition_current <- as.character(lubridate::year(enddate))
  partition_previous <- as.character(lubridate::year(as.Date(enddate) - lubridate::years(1)))
  partition_next <- as.character(lubridate::year(as.Date(enddate) + lubridate::years(1)))

  # Conditionally include the location filter
  location_condition <- if (filter_by_location) {
    paste("m.location_uuid = '", location_uuid, "' AND", sep = "")
  } else {
    ""
  }

  sql_query <- paste(
    "WITH med_dispensations AS (
      SELECT
        m.patient_uuid,
        ROW_NUMBER() OVER (PARTITION BY m.patient_uuid ORDER BY m.next_pickup_date DESC, m.medication_pickup_date DESC) AS row_num
      FROM
        medication PARTITION(medication_y", partition_previous, ", medication_y", partition_current, ", medication_y", partition_next, ") m
      WHERE
        ", location_condition, "
        (medication_pickup_date <= '", enddate, " 00:00:00') AND (
        (m.next_pickup_date >= '", next_pickup_date_cutoff, " 00:00:00' AND m.next_pickup_date <= '", ceiling, " 00:00:00' AND m.form_id = 130) OR
        (m.medication_pickup_date >= '", medication_pickup_date_cutoff, " 00:00:00' AND m.form_id = 166)))

    SELECT patient_uuid
    FROM med_dispensations
    WHERE row_num = 1;",
    sep = ""
  )

  # Execute the SQL query and return only unique patient UUIDs
  unique_patient_uuids <- DBI::dbGetQuery(con, sql_query) |>
    dplyr::pull(patient_uuid) |>
    unique()

  patient_uuids_str <- paste(sprintf("'%s'", unique_patient_uuids), collapse = ",")

  return(patient_uuids_str)

}
