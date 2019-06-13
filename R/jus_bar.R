#' Bar plot showing the relationship between the predicted and one predictor variable.
#'
#' @param df data.frame with predictor and predicted variables.
#' @param group predictor.
#' @param decision predicted variable.
#' @param title plot title.
#'
#' @return bar plot with group in the x axis and decision count in the y axis.
#' @export
#'
jus_bar <- function(df, group, decision, title) {

  g <- rlang::enexpr(group)

  d <- rlang::enexpr(decision)





dd <- df %>%
    dplyr::select(group := !!g, decision := !!d)


v <- unique(dd$decision) %>%
  as.character()


  dd %>%
    dplyr::count(group, decision) %>%
    tidyr::spread(key=decision, value=n) %>%
    dplyr::mutate(total = .data[[v[[1]]]] + .data[[v[[2]]]]) %>%
    dplyr::arrange(dplyr::desc(total)) %>%
     echarts4r::e_charts(group) %>%
     echarts4r::e_bar_(v[1], name =v[1], stack = "grp") %>%
     echarts4r::e_bar_(v[2], name = v[2], stack = "grp") %>%
     echarts4r::e_tooltip() %>%
     echarts4r::e_title(title)
}

