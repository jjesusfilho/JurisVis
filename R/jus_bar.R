#' Gráfico de barras com variável binária dependente e outra dependente
#'
#' @param base data.frame com dependente e independentes
#' @param grupo variável categórica a ocupar eixo x
#' @param decisao variável dependente binária
#' @param titulo título do gráfico
#'
#' @return gráfico de barras com VI no eixo x e VD no eixo y.
#' @export
#'
jus_bar <- function(base, grupo, decisao, titulo) {
  g <- rlang::enexpr(grupo)
  d <- rlang::enexpr(decisao)


  df <- base %>%
    dplyr::select(grupo := !!g, decisao := !!d)

  variaveis <- df %>%
    dplyr::distinct(decisao) %>%
    dplyr::pull(decisao) %>%
    as.character()

  df %>%
    dplyr::count(grupo, decisao) %>%
    tidyr::spread(key=decisao, value=n) %>%
    dplyr::mutate(total = .data[[variaveis[[1]]]] + .data[[variaveis[[2]]]]) %>%
    dplyr::arrange(dplyr::desc(total)) %>%
    echarts4r::e_charts(grupo) %>%
    echarts4r::e_bar(.data[[variaveis[[1]]]], name = variaveis[[1]], stack = "grp") %>%
    echarts4r::e_bar(.data[[variaveis[[2]]]], name = variaveis[[2]], stack = "grp") %>%
    echarts4r::e_tooltip() %>%
    echarts4r::e_title(titulo)
}
