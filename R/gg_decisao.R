#' plota gg_plot
#'
#' @param df
#' @param fator
#' @param decisao
#' @param faceta
#' @param titulo
#'
#' @return
#' @export
#'
#' @examples
gg_decisao <- function(df,fator,decisao,faceta,titulo=""){

  fator <- rlang::enexpr(fator)
  decisao <- rlang::enexpr(decisao)
  faceta <- rlang::enexpr(faceta)

  d <- j3 %>%
    dplyr::select(fator = !!fator, faceta = !!faceta, decisao = !!decisao) %>%
    dplyr::count(fator,faceta,decisao)


  ggplot2::ggplot(d, ggplot2::aes(x = fator, y = n, fill = decisao)) +
    ggplot2::geom_bar(stat = "identity",
                      position = "dodge",
                      colour = "black") +
    ggplot2::scale_fill_manual(values = c("red", "darkgreen"), name = "Decisão:") +
    geom_text(aes(x=fator,y=n,label=n),color="white",position=position_dodge(.9),vjust=0,hjust=1)+
    ggplot2::facet_grid( ~ faceta) +
    ggplot2::coord_flip() +
    ggplot2::theme(
      strip.background = ggplot2::element_rect(fill = "lightblue", colour = "black"),
      legend.position = "bottom"
    ) +
    ggplot2::labs(
      title = titulo,
      x = fator,
      y = "Número de decisões",
      caption = "Fonte: TRF3"
    )

}
