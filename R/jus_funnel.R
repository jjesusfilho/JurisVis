#' Builds a funnel plot showing criteria and number of dockets removed along the research
#'
#' @param stages vector with filtering stages.
#' @param initial initial number of dockets.
#' @param removals vector with number of dockets removed at each stage.
#'     It should be one element shorter than stages.
#' @param title funnel title.
#'
#' @return echarts funnel.
#' @export
#'
jus_funnel<-function(stages=NULL,initial=NULL,removals=NULL,title=""){

  values= purrr::accumulate(c(initial,removals),`-`)

  data.frame(stage = stages,value=values) %>%
    echarts4r::e_charts(width="70%") %>%
    echarts4r::e_title(title) %>%
    echarts4r::e_funnel(value, stage,legend=FALSE) %>%
    echarts4r::e_tooltip()

}
