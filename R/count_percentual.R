#' Cria frequências de colunas em relação à decisão
#'
#' @param df data.frame
#' @param ... colunas
#' @param decisao coluna decisão
#'
#' @return tabela de frequências
#' @export
#'
count_percentual <- function(df,...,decisao){

  df <- dplyr::select(df,...,decisao)

  df %>%
    dplyr::group_by(...,decisao) %>%
    dplyr::summarize(n = n()) %>%
    dplyr::group_by_at(dplyr::vars(-c(decisao,n))) %>%
    dplyr::mutate(percentual = n*100/sum(n),
                  percentual = round(percentual,2))
}
