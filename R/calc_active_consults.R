#' Query consultas clínicas realizadas
#'
#' @param con Ligação à base de dados MozART 2.0
#' @param enddate XXX
#' @param defaulter_tolerance XXX
#' @param filter_by_location XXX
#' @param location_uuid XXX
#' @param client_status Um vector contendo os patient_uuid unicos dos pacientes activos em TARV
#'
#' @return `calc_consults` devolve um quadro de dados com consultas clínicas para pacientes activos em TARV
#' @export
#'
#' @examples
#' \dontrun{
#'
#'  df <- calc_consults(
#'           con,
#'           patient_uuids = tx_uuids)}

calc_consults <- function(con,
                          enddate = Sys.Date(),
                          defaulter_tolerance = 28,
                          filter_by_location = FALSE,
                          location_uuid = NULL,
                          client_status = "All") {

  # Initialize SQL query
  sql_query <- "SELECT * FROM clinical_consultation WHERE form_id = 163"

  # Apply filtering based on client_status
  if (client_status == "Active") {
    # Generate patient_uuids dynamically using pull_active_uuid()
    patient_uuids <- pull_active_uuid(
      con = con,
      enddate = enddate,
      defaulter_tolerance = defaulter_tolerance,
      filter_by_location = filter_by_location,
      location_uuid = location_uuid
    )

    # Ensure patient_uuids is correctly formatted
    if (length(patient_uuids) == 0) {
      stop("No active patients found.")
    }

    # Add active patient filtering to SQL query
    sql_query <- sprintf("%s AND patient_uuid IN (%s)", sql_query, patient_uuids)
  }

  # Conditionally add location filter
  if (filter_by_location && !is.null(location_uuid)) {
    sql_query <- paste0(sql_query, sprintf(" AND location_uuid = '%s'", location_uuid))
  }

  # Fetch the data
  df <- DBI::dbGetQuery(con, sql_query)

  return(df)

}
