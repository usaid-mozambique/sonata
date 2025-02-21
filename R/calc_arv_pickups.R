#' Query levantamentos TARV
#'
#' @description
#' `calc_arv_pickups()` devolve uma listagem dos levantamentos ARV feito during um período especificado pelo utilizador
#'
#' @param con Ligação à base de dados MozART 2.0
#' @param opendate Data da abertura do período (introduzir como “AAAA-MM-DD”)
#' @param enddate Data do fecho do período (introduzir como “AAAA-MM-DD”)
#' @param arv_pickup_source Fonte de levantamento de ARV. Opções são "FILA", "Reception", "Any". Default é "FILA
#' * `"FILA"` por defeito, devolve levantamentos documentados na FILA ou fonte alternativa da farmácia
#' * `"Reception"` devolve levantamentos documentados na Ficha de Seguimento, Mapa dos Levantamentos
#' * `"Any"` devolve todos levantamentos
#' @param filter_by_location `FALSE` por defeito
#' * `TRUE` executa sobre uma US específica
#' * `FALSE` executa sobre todas as US contidas no MozART 2.0
#' @param location_uuid location_uuid da US a filtrar quando filter_by_location é definido como `TRUE`
#'
#' @return Um quadro de dados contendo uma listagem individual dos levantamentos TARV
#' @export
#'
#' @examples
#' \dontrun{
#'
#'  df <- calc_arv_pickups(
#'           con
#'           opendate = '2024-06-20',
#'           enddate = '2024-09-20',
#'           arv_pickup_source = "FILA",
#'           filter_by_location = TRUE,
#'           location_uuid = location_meripo)}

calc_arv_pickups <- function(con,
                             enddate,
                             opendate,  # New argument for start date
                             arv_pickup_source = "FILA",
                             filter_by_location = FALSE,
                             location_uuid = '4be5f1a9-832c-4717-be41-ef4b6311c0ef') {

  # Convert enddate and opendate to Date format
  enddate <- as.Date(enddate)
  opendate <- as.Date(opendate)

  # Partitioning based on calendar year
  partition_current <- as.character(lubridate::year(enddate))
  partition_previous <- as.character(lubridate::year(enddate) - 1)
  partition_next <- as.character(lubridate::year(enddate) + 1)

  # Conditionally include the location filter
  location_condition <- if (filter_by_location) {
    paste("m.location_uuid = '", location_uuid, "' AND", sep = "")
  } else {
    ""
  }

  # Define the form_id condition based on arv_pickup_source argument
  form_id_condition <- switch(arv_pickup_source,
                              "FILA" = "m.form_id = 130",
                              "Reception" = "m.form_id = 166",
                              "Any" = "m.form_id IN (130, 166)",
                              "m.form_id = 130"  # Default to FILA if invalid input
  )

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
    m.medication_pickup_date BETWEEN '", opendate, " 00:00:00' AND '", enddate, " 00:00:00'
    AND ", form_id_condition, ")

    SELECT * FROM med_dispensations;",
    sep = ""
  )

  # Execute the SQL query and return the result
  df <- DBI::dbGetQuery(con, sql_query) |>
    dplyr::mutate(
      enddate = lubridate::as_date(lubridate::ymd(enddate)),
      dplyr::across(c(medication_pickup_date, next_pickup_date), ~ lubridate::as_date(lubridate::ymd_hms(.))),
      dplyr::across(c(birthdate, enddate), ~ lubridate::as_date(lubridate::ymd(.)))) |>
    calc_age_var(ref_date = enddate) |>
    dplyr::select(!row_num) |>
    dplyr::relocate(age, .after = birthdate)

  return(df)

}
