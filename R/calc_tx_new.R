#' Calculate anonymous line list for Newly initiated on ART
#'
#' @param con MozART 2.0 database connection
#' @param opendate Period open date (input as 'YYYY-MM-DD')
#' @param enddate Period close date (input as 'YYYY-MM-DD')
#' @param filter_by_location Logical. If `TRUE`, filters query to a specific location.  If `FALSE` queries for all locations.
#' @param location_uuid Health facility location_uuid to filter by when filter_by_location is `TRUE`
#'
#' @return A data frame containing line-level listing of patients newly initiated on ART
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
    encounter_uuid,
    encounter_date,
    form_id,
    encounter_type,
    patient_uuid,
    encounter_created_date,
    location_uuid,
    concept_id,
    observation_date,
    value_datetime,
    obs_uuid,
    source_database",
    "FROM observation o",
    "WHERE",
    location_condition,  # dynamic location condition
    "concept_id = 1190 AND observation_date >= '2018-01-01'"
  )

  # execute sql query, munge, and return result
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
    dplyr::mutate(dplyr::across(c(value_datetime, art_start_date, observation_date), ~ lubridate::as_date(lubridate::ymd_hms(.))))

  return(df)

}
