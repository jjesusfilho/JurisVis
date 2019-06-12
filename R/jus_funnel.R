#' Cria funil echarts a partir dos critérios de exclusão para chegar na base de análise.
#'
#' @param estagios vetor com etpas de filtragens
#' @param subtracoes vetor com quantidades de processos excluídos
#' @param titulo Título do gráfico
#'
#' @return funil echarts.
#' @export
#'
jus_funnel<-function(estagios=NULL,subtracoes=NULL,titulo=""){

  valores = purrr::accumulate(subtracoes,`-`)

  data.frame(stage = estagios,value=subtracoes) %>%
    echarts4r::e_charts(width="70%") %>%
    echarts4r::e_title(titulo) %>%
    echarts4r::e_funnel(value, stage,legend=FALSE) %>%
    echarts4r::e_tooltip()

}
