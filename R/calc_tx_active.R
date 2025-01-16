#' Query estatísticas anónimas para clientes activos em TARV
#'
#' @param con Ligação à base de dados MozART 2.0
#' @param enddate Data de fecho do período (introduzir como “AAAA-MM-DD”)
#' @param defaulter_tolerance Número de dias de tolerância antes dos faltosos serem considerados inactivos (abandonos)
#' @param filter_by_location Lógico. Se `TRUE`, o query corre sobre uma unidade sanitária específica.  Se `FALSE`,  o query corre sobre todas as unidade sanitária contidas na base de dados
#' @param location_uuid location_uuid da unidade sanitária a filtrar quando filter_by_location é definido como `TRUE`
#'
#' @return Um quadro de dados contendo uma listagem individual dos pacientes activos em TARV
#' @export
#'
#' @examples
#' \dontrun{
#'
#'  df <- calc_tx_active(
#'           con,
#'           enddate = '2024-09-20',
#'           defaulter_tolerance = 28,
#'           filter_by_location = TRUE,
#'           location_uuid = location_meripo)}

calc_tx_active <- function(con, enddate, defaulter_tolerance = 28, filter_by_location = FALSE, location_uuid = '4be5f1a9-832c-4717-be41-ef4b6311c0ef') {

  # ARV pickup cutoff definitions and ceilings.
  next_pickup_date_cutoff <- as.character(as.Date(enddate) - lubridate::days(defaulter_tolerance))
  medication_pickup_date_cutoff <- as.character(as.Date(enddate) - lubridate::days(defaulter_tolerance))
  ceiling <- as.Date(enddate) + lubridate::years(1)

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
    m.location_uuid,
    m.encounter_uuid,
    m.patient_uuid,
    p.birthdate,
    p.gender,
    m.form_id,
    m.regimen_id,
    m.mode_dispensation_id,
    m.medication_pickup_date,
    m.next_pickup_date,
    ROW_NUMBER() OVER (PARTITION BY p.patient_uuid ORDER BY m.next_pickup_date DESC, m.medication_pickup_date DESC) AS row_num
    FROM
    medication PARTITION(medication_y", partition_previous, ", medication_y", partition_current, ", medication_y", partition_next, ") m
    LEFT JOIN patient p ON m.patient_uuid = p.patient_uuid
    WHERE
    ", location_condition, "
    (medication_pickup_date <= '", enddate, " 00:00:00') AND (
    (m.next_pickup_date >= '", next_pickup_date_cutoff, " 00:00:00' AND m.next_pickup_date <= '", ceiling, " 00:00:00' AND m.form_id = 130) OR
    (m.medication_pickup_date >= '", medication_pickup_date_cutoff, " 00:00:00' AND m.form_id = 166)))

    SELECT *
    FROM med_dispensations
    WHERE row_num = 1;",
    sep = ""
  )

  # Execute the SQL query and return the result
  df <- DBI::dbGetQuery(con, sql_query) |>
    dplyr::mutate(
      enddate = lubridate::as_date(lubridate::ymd(enddate)),
      dplyr::across(c(medication_pickup_date, next_pickup_date), ~ lubridate::as_date(lubridate::ymd_hms(.))),
      dplyr::across(c(birthdate, enddate), ~ lubridate::as_date(lubridate::ymd(.))),
      age = calc_client_age(birth_date = birthdate, ref_date = enddate)
    ) |>
    dplyr::select(!row_num) |>
    dplyr::relocate(age, .after = birthdate)


  return(df)

}
