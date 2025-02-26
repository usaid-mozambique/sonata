#' Query uuids para activos ou novos inícios ao TARV
#'
#' @description Esta função funciona como um invólucro para devolver UUIDs de clientes com base no seu
#' estado (“Active” ou “New”). Ela chama `pull_active_uuid()` para clientes ativos ou
#' `pull_new_uuid()` para novos clientes, dependendo do argumento `client_status`.
#'
#' @param con Ligação à base de dados MozART 2.0
#' @param opendate Data de abertura do período (introduzir como “AAAA-MM-DD”)
#' @param enddate Data de fecho do período (introduzir como “AAAA-MM-DD”)
#' @param client_status Character. Defines which UUIDs to pull. Options: `"Active"` (default)
#'   to retrieve active clients, `"New"` to retrieve new clients.
#' @param defaulter_tolerance Numeric. Number of days used to define a defaulter
#'   when `client_status = "Active"`. Defaults to 28 days.
#' @param filter_by_location Se `TRUE`, o query corre sobre uma unidade sanitária específica.  Se `FALSE`, o query corre sobre todas as unidade sanitária contidas no MozART 2.0
#' @param location_uuid location_uuid da unidade sanitária a filtrar quando filter_by_location é definido como `TRUE`
#'
#' @return Um vector de UUIDs de clientes que correspondem aos critérios especificados.
#' @export
#'
#' @examples
#' \dontrun{
#' # Caso de uso simples executado sobre todas US
#' active_uuids <- pull_client_uuid(con,
#'                                  enddate = "2024-09-20",
#'                                  filter_by_location = FALSE)
#'
#' # Devolver novos clientes a partir de uma data de abertura especificada
#' new_uuids <- pull_client_uuid(con,
#'                               opendate = "2024-01-01",
#'                               enddate = "2024-09-20",
#'                               client_status = "New")
#'
#' # Devolver clientes activos para uma US específica com uma tolerância de 59 dias
#' active_uuids <- pull_client_uuid(con,
#'                                  enddate = "2024-09-20",
#'                                  defaulter_tolerance = 59,
#'                                  location_uuid = "custom-location-uuid")}

pull_client_uuid <- function(con,
                             opendate = NULL,  # Moved opendate before enddate
                             enddate = Sys.Date(),
                             client_status = "Active",
                             defaulter_tolerance = 28,
                             filter_by_location = TRUE,
                             location_uuid = 'e5f01eee-2392-49b4-a5bf-5cf593fc8f21') {

  # Validate client_status input
  if (!client_status %in% c("Active", "New")) {
    stop("Invalid client_status. Choose either 'Active' or 'New'.")
  }

  # Select and run the appropriate function based on client_status
  if (client_status == "Active") {
    uuids <- pull_active_uuid(
      con = con,
      enddate = enddate,
      defaulter_tolerance = defaulter_tolerance,
      filter_by_location = filter_by_location,
      location_uuid = location_uuid
    )
  } else { # client_status == "New"
    if (is.null(opendate)) {
      stop("opendate must be provided when client_status = 'New'.")
    }

    uuids <- pull_new_uuid(
      con = con,
      opendate = opendate,  # Pass opendate to pull_new_uuid
      enddate = enddate,
      filter_by_location = filter_by_location,
      location_uuid = location_uuid
    )
  }

  return(uuids)
}
