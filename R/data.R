#' Referência: location_uuid
#'
#' Source: \href{https://github.com/usaid-mozambique/sonata/blob/main/R/data.R}{`R/data.R`}.
#'
#' @usage data(data_location_lookup)
#'
#' @format Quadro de dados com variáveis:
#' \describe{
#'   \item{location_uuid}{código único de identificação do local}
#'   \item{datim_id}{Código único de identificação do Datim do local}
#'   \item{sisma_id}{Código único de identificação do SISMA do local}
#'   \item{province}{Província onde se situa o local}
#'   \item{district}{Distrito em que o local está situado}
#'   \item{datim_name}{Nome do local no Datim}
#'   \item{mozart_name}{Nome do local no MozART}
#' }
"data_location_lookup"


#' Referência: data_type_id_lookup
#'
#' Source: \href{https://github.com/usaid-mozambique/sonata/blob/main/R/data.R}{`R/data.R`}.
#'
#' @usage data(data_type_id_lookup)
#'
#' @format Quadro de dados com variáveis:
#' \describe{
#'   \item{id}{id}
#'   \item{table_name}{Nome do formulário específico (por exemplo, form)}
#'   \item{column_name}{Código único do formulário específico (por exemplo, form_id)}
#'   \item{id_type_lookup}{Código numérico do formulário específico (por exemplo, 60)}
#'   \item{id_type_desc}{Descrição do formulário específico - inglês}
#'   \item{id_type_lookup_pt}{Descrição do formulário específico - português}
#'   \item{notes}{Notas}
#' }
"data_type_id_lookup"


#' Referência: data_observation_lookup
#'
#' Source: \href{https://github.com/usaid-mozambique/sonata/blob/main/R/data.R}{`R/data.R`}.
#'
#' @usage data(data_observation_lookup)
#'
#' @format Quadro de dados com variáveis:
#' \describe{
#'   \item{concept_id}{Identificador do conceito}
#'   \item{concept_name}{Nome do conceito}
#' }
"data_observation_lookup"

