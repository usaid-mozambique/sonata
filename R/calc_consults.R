#' Query consultas clínicas HIV
#'
#' @description
#' `calc_consults()` devolve uma listagem das consultas clínicas do HIV realizadas por um grupo de clientes definido pelo utilizador (activos em TARV ou todos clientes)
#'
#' @param con Ligação à base de dados MozART 2.0
#' @param client_status Estado do cliente em TARV
#' * `"All"`, por defeito, devolve todos clientes tendo realizado consultas clínicas
#' * `"Active"`, devolve apenas os clientes activos em TARV
#' @param defaulter_tolerance Número de dias de tolerância antes dos faltosos serem considerados inactivos (abandonos)
#' * `"28"` por defeito
#' @param opendate Data da abertura do período (introduzir como “AAAA-MM-DD”)
#' @param enddate Data do fecho do período usado para avialar o estado activo do client
#' * `"Sys.Date"` Por defeito
#' * Data customizada em formato (introduzir como “AAAA-MM-DD”)
#' * Número customizado (por exemplo `"59"` na definição de MISAU)
#' @param filter_by_location Se `TRUE`, o query corre sobre uma unidade sanitária específica.  Se `FALSE`, o query corre sobre todas as unidade sanitária contidas no MozART 2.0
#' @param location_uuid location_uuid da unidade sanitária a filtrar quando filter_by_location é definido como `TRUE`
#'
#' @return Um quadro de dados contendo uma listagem das consultas clínicas HIV realizadas
#' @export
#'
#' @examples
#' \dontrun{
#'  # Caso de uso simples executado sobre todas US
#'  df <- calc_consults(con)
#'
#'  # Caso de uso de activos, tolerância 59 dias, executado sobre uma US
#'  df <- calc_consults(
#'           con,
#'           enddate = '2024-09-20',
#'           client_status = "Active",
#'           defaulter_tolerance = 59,
#'           filter_by_location = TRUE,
#'           location_uuid = 'e3eb1a1b-be07-4af2-9360-5d7046910576')
#'
#'  # Caso de uso de activos, limitando as consultas a 2022 para frente
#'  df <- calc_consults(
#'           con,
#'           client_status = "Active",
#'           enddate = '2024-09-20',
#'           opendate = '2022-01-10',
#'           filter_by_location = FALSE)}

calc_consults <- function(con,
                          client_status = "All",
                          defaulter_tolerance = 28,
                          opendate = NULL,
                          enddate = Sys.Date(),
                          filter_by_location = TRUE,
                          location_uuid = 'e5f01eee-2392-49b4-a5bf-5cf593fc8f21') {

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
