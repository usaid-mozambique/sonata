#' Query consultas clínicas realizadas
#'
#' @param con Ligação à base de dados MozART 2.0
#' @param enddate Data de fecho do período (introduzir como “AAAA-MM-DD”)
#' @param opendate Data de abertura do período (introduzir como “AAAA-MM-DD”)
#' @param client_status XXX
#' @param defaulter_tolerance Número de dias de tolerância antes dos faltosos serem considerados inactivos (abandonos)
#' @param filter_by_location Lógico. Se `TRUE`, o query corre sobre uma unidade sanitária específica.  Se `FALSE`,  o query corre sobre todas as unidade sanitária contidas na base de dados
#' @param location_uuid location_uuid da unidade sanitária a filtrar quando filter_by_location é definido como `TRUE`
#'
#' @return Um quadro de dados contendo uma listagem das consultas clínicas realizadas
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
                          opendate = NULL,
                          client_status = "All",
                          defaulter_tolerance = 28,
                          filter_by_location = FALSE,
                          location_uuid = NULL
)
{

  # Create base SQL query
  sql_query <- "SELECT * FROM clinical_consultation WHERE form_id = 163"

  # Filter based on client_status
  if (client_status == "Active") {
    # Pull active clients
    patient_uuids <- pull_active_uuid(
      con = con,
      enddate = enddate,
      defaulter_tolerance = defaulter_tolerance,
      filter_by_location = filter_by_location,
      location_uuid = location_uuid
    )

    # Format patient_uuids
    if (length(patient_uuids) == 0) {
      stop("No active patients found.")
    }

    # Status filter
    sql_query <- sprintf("%s AND patient_uuid IN (%s)", sql_query, patient_uuids)
  }

  # Location filter
  if (filter_by_location && !is.null(location_uuid)) {
    sql_query <- paste0(sql_query, sprintf(" AND location_uuid = '%s'", location_uuid))
  }

  # Opendate filter
  if (!is.null(opendate)) {
    sql_query <- paste0(sql_query, sprintf(" AND consultation_date >= '%s'", opendate))
  }

  # Execute query
  df <- DBI::dbGetQuery(con, sql_query)

  return(df)

}
