#' Echarts for discrete scale
#'
#' @param df Data.frame
#' @param grupo Groping variable
#' @param variavel discrete items
#' @param titulo Title
#'
#' @details It only charts  2, 3, 5, and 7
#'     levels. It also removes NAs.
#'
#' @return Chart that can also be downloaded as png
#' @export
#'

discrete_bar <- function(df, grupo, variavel, titulo)
{

  g <- rlang::enexpr(grupo)
  d <- rlang::enexpr(variavel)

  dd <- df %>%
    dplyr::select(`:=`(grupo, !!g), `:=`(variavel, !!d))

  categorias <- dd$variavel %>%
    na.omit() %>%
    unique() %>%
    length()

 cor= viridis::viridis_pal(option="B")(categorias)%>%
      jsonlite::toJSON() %>%
  paste0('{"color":',.,'}')


  dd <- dd %>% dplyr::count(grupo,variavel) %>%
    stats::na.omit() %>%
    tidyr::spread(key = variavel,  value = n) %>%
    janitor::clean_names()


  if (categorias == 7){

    v <- names(dd)[2:8]


    dd %>% dplyr::mutate(total = .data[[v[[1]]]] + .data[[v[[2]]]] + .data[[v[[3]]]] + .data[[v[[4]]]] + .data[[v[[5]]]]+ .data[[v[[6]]]] + .data[[v[[7]]]]) %>%
      dplyr::mutate(total= rowSums(.[2:8])) %>%
      dplyr::mutate_at(2:8,list(~(.*100/total) %>% round(1))) %>%
      dplyr::mutate(total = NULL) %>%
      echarts4r::e_charts(grupo) %>%
      echarts4r::e_bar_(v[1], name = v[1], stack = "grp") %>%
      echarts4r::e_bar_(v[2], name = v[2], stack = "grp") %>%
      echarts4r::e_bar_(v[3], name = v[3], stack = "grp") %>%
      echarts4r::e_bar_(v[4], name = v[4], stack = "grp") %>%
      echarts4r::e_bar_(v[5], name = v[5], stack = "grp") %>%
      echarts4r::e_bar_(v[6], name = v[6], stack = "grp") %>%
      echarts4r::e_bar_(v[7], name = v[7], stack = "grp") %>%
      echarts4r::e_tooltip() %>%
      echarts4r::e_x_axis(axisLabel = list(interval = 0)) %>%
      echarts4r::e_y_axis(max=100) %>%
      echarts4r::e_toolbox_feature() %>%
      echarts4r::e_title(titulo) %>%
      echarts4r::e_flip_coords() %>%
      echarts4r::e_theme_custom(cor)

  } else if (categorias == 5){

    v <- names(dd)[2:6]


    dd %>% dplyr::mutate(total = .data[[v[[1]]]] + .data[[v[[2]]]] + .data[[v[[3]]]] + .data[[v[[4]]]] + .data[[v[[5]]]]) %>%
      dplyr::mutate(total= rowSums(.[2:6])) %>%
      dplyr::mutate_at(2:6,list(~(.*100/total) %>% round(1))) %>%
      dplyr::mutate(total = NULL) %>%
      echarts4r::e_charts(grupo) %>%
      echarts4r::e_bar_(v[1], name = v[1], stack = "grp") %>%
      echarts4r::e_bar_(v[2], name = v[2], stack = "grp") %>%
      echarts4r::e_bar_(v[3], name = v[3], stack = "grp") %>%
      echarts4r::e_bar_(v[4], name = v[4], stack = "grp") %>%
      echarts4r::e_bar_(v[5], name = v[5], stack = "grp") %>%
      echarts4r::e_tooltip() %>%
      echarts4r::e_x_axis(axisLabel = list(interval = 0)) %>%
      echarts4r::e_y_axis(max=100) %>%
      echarts4r::e_toolbox_feature() %>%
      echarts4r::e_title(titulo) %>%
      echarts4r::e_flip_coords() %>%
      echarts4r::e_theme_custom(cor)

  } else if (categorias == 3) {

    v <- names(dd)[2:4]


    dd %>% dplyr::mutate(total = .data[[v[[1]]]] + .data[[v[[2]]]] + .data[[v[[3]]]]) %>%
      dplyr::mutate(total= rowSums(.[2:4])) %>%
      dplyr::mutate_at(2:4,list(~(.*100/total) %>% round(1))) %>%
      dplyr::mutate(total = NULL) %>%
      echarts4r::e_charts(grupo) %>%
      echarts4r::e_bar_(v[1], name = v[1], stack = "grp") %>%
      echarts4r::e_bar_(v[2], name = v[2], stack = "grp") %>%
      echarts4r::e_bar_(v[3], name = v[3], stack = "grp") %>%
      echarts4r::e_tooltip() %>%
      echarts4r::e_x_axis(axisLabel = list(interval = 0)) %>%
      echarts4r::e_y_axis(max=100) %>%
      echarts4r::e_toolbox_feature() %>%
      echarts4r::e_title(titulo) %>%
      echarts4r::e_flip_coords()%>%
      echarts4r::e_theme_custom(cor)



  } else if (categorias == 2){
    v <- names(dd)[2:3]


    dd %>% dplyr::mutate(total = .data[[v[[1]]]] + .data[[v[[2]]]]) %>%
      dplyr::mutate(total= rowSums(.[2:3])) %>%
      dplyr::mutate_at(2:3,list(~(.*100/total) %>% round(1))) %>%
      dplyr::mutate(total = NULL) %>%
      echarts4r::e_charts(grupo) %>%
      echarts4r::e_bar_(v[1], name = v[1], stack = "grp") %>%
      echarts4r::e_bar_(v[2], name = v[2], stack = "grp") %>%
      echarts4r::e_tooltip() %>%
      echarts4r::e_x_axis(axisLabel = list(interval = 0)) %>%
      echarts4r::e_y_axis(max=100) %>%
      echarts4r::e_toolbox_feature() %>%
      echarts4r::e_title(titulo) %>%
      echarts4r::e_flip_coords()%>%
      echarts4r::e_theme_custom(cor)



  } else {
    stop("Os níveis devem ser 2,3,5 ou 7")
  }

}
