#' Cria série temporal com tempo de processamento com um ou mais assuntos.
#'
#' @param base dataframe com assunto, data de entrada ou fato e data do julgamento.
#' @param data_entrada data de entrada
#' @param assunto assunto
#' @param data_julgamento data do julgamento
#' @param etiqueta etiqueta para assunto, ex. crimes, temas do consumidor etc.
#'
#' @detail Criei essa função inicialmente para comparar o tempo de
#'     processamento entre diferentes crimes, mas elas pode ser
#'     usada para qualquer assunto processual. Esse gráfico somente
#'     pode ser usado por acadêmicos e entidades sem fim econômico.
#'
#' @return Série temporal com highcharts.
#' @export
#'
jus_time <-
  function(base,
           data_entrada,
           assunto,
           data_julgamento,
           etiqueta) {
    suppressWarnings({
      di <- rlang::enexpr(data_entrada)
      de <- rlang::enexpr(assunto)
      dd <- rlang::enexpr(data_julgamento)

      df <- base %>%
        dplyr::select(data_entrada := !!di, assunto := !!de, data_julgamento := !!dd)

      df <- df %>%
        dplyr::mutate(tempo_processamento = tjsp::lapso(data_entrada, data_julgamento)) %>%
        dlyr::select(-data_julgamento) %>%
        dplyr::mutate(assunto = assunto,
                      ind = dplyr::row_number()) %>%
        tidyr::spread(assunto, tempo_processamento) %>%
        dplyr::mutate(ind = NULL) %>%
        tidyquant::tq_transmute(mutate_fun = apply.monthly,
                                FUN = mean,
                                na.rm = TRUE)

      tema <- unique(df$assunto)

      df <- purrr::map(df[-1],  ~ data.frame(date = df$data_entrada, .x))

      df <-
        purrr::map(df,  ~ timetk::tk_xts(.x, date_var = date) %>% round(1))

      titulo_tema <- if (length(tema) == 1) {
        glue::glue('Tempo entre {referencia} e o julgamento para o {tema} ao longo dos meses')

      } else{
        glue::glue(
          'Comparação entre as datas dos assuntos {glue::glue_collapse(tema," , ",last=" e ")} e respectivos julgamentos ao longo dos meses'
        )
      }

      cores <- viridis::viridis(length(df))

      hc <- highcharter::highchart(type = "stock") %>%
        highcharter::hc_title(text = titulo_tema)


      for (i in seq_along(df)) {
        hc <- hc %>%
          highcharter::hc_add_series(df[[i]], color = cores[i], name = names(df[i]))
      }
    })
    hc

  }
