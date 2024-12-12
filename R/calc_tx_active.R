#' Calculate line-level listing for patients active on ART
#'
#' @param con MozART 2.0 database connection
#' @param enddate Period close date (input as 'YYYY-MM-DD')
#' @param defaulter_tolerance Number of tolerance days before defaulters are considered inactive
#' @param filter_by_location Logical. If `TRUE`, filters query to a specific location.  If `FALSE` queries for all locations.
#' @param location_uuid Health facility location_uuid to filter by when filter_by_location is `TRUE`
#'
#' @return A data frame containing line-level listing of patients active on ART
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
    dplyr::mutate(enddate = enddate) |>
    dplyr::mutate(dplyr::across(c(medication_pickup_date, next_pickup_date), ~ lubridate::as_date(ymd_hms(.)))) |>
    dplyr::select(!row_num)


  return(df)

}
