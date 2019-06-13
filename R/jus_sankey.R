#' Plots echarts sankey diagram from a data.frame
#'
#' @param df a data.frame of categorical variables
#'
#' @return A sankey ordered by the data.frame column names.
#' @export
#'
#' @examples
#' \dontrun{
#' titanic <- as.data.frame(Titanic) %>%
#'            tidyr::uncount(Freq)
#' jus_sankey(titanic)
#' }
jus_sankey <- function(df){

  names(df) %>%
    rep(each=2) %>%
    utils::tail(-1) %>%
    utils::head(-1) %>%
    matrix(ncol=2,byrow=TRUE) %>%
    tibble::as_tibble() %>%
    stats::setNames(c("x","y")) %>%
    purrr::pmap_dfr(function(x,y) {
      df %>%
        dplyr::select(source=x,target=y) %>%
        dplyr::group_by(source,target) %>%
        dplyr::summarize(value=dplyr::n())
    }) %>%
    as.data.frame() %>%
    echarts4r::e_charts() %>%
    echarts4r::e_sankey(source,target,value)
}
