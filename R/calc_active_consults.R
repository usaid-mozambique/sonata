#' Query um quadro de dados com consultas clínicas para pacientes activos em TARV
#'
#' @param con Ligação à base de dados MozART 2.0
#' @param patient_uuids Mm vector contendo os patient_uuid unicos dos pacientes activos em TARV
#'
#' @return `calc_active_consults` devolve um quadro de dados com consultas clínicas para pacientes activos em TARV
#' @export
#'
#' @examples
#' \dontrun{
#'
#'  df <- calc_active_consults(
#'           con,
#'           patient_uuids = tx_uuids)}

calc_active_consults <- function(con, patient_uuids) {

  # Fetch data from the clinical_consultation table with form_id = 163 and filtered patient_uuids
  sql_query <- sprintf("
  SELECT *
  FROM clinical_consultation
  WHERE form_id = 163 AND patient_uuid IN (%s)
", patient_uuids)

  # Fetch the data
  df <- dbGetQuery(con, sql_query)

  return(df)

}
