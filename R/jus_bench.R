#' Builds a time series benchmarking processing time.
#'
#' @param df dataframe with the matter, file date (could be a crime date), and decision date.
#' @param entry_date application, loading or crime date
#' @param matter matter
#' @param decision_date decision or any other reference date.
#' @param label label to the matter.
#' @param peridiocity c("monthly", "yearly", "weekly")
#'
#'
#' @return time series with processing benchmark.
#' @export
#'
jus_bench <-
  function(df,
           entry_date,
           matter,
           decision_date,
           label,
           peridiocity="monthly") {
    suppressWarnings({
      di <- rlang::enexpr(entry_date)
      de <- rlang::enexpr(matter)
      dd <- rlang::enexpr(decision_date)

      per<-switch(peridiocity,
                  'weekly'=xts::apply.weekly,
                  'monthly'=xts::apply.monthly,
                  'yearly'=xts::apply.yearly
      )

      df <- df %>%
        dplyr::select(entry_date := !!di, matter := !!de, decision_date := !!dd)

      df <- df %>%
        dplyr::mutate(processing_time = lubridate::interval(entry_date, decision_date) %>%
                        lubridate::time_length("month")) %>%
        dplyr::select(-decision_date) %>%
        dplyr::mutate(matter = matter,
                      ind = dplyr::row_number()) %>%
        tidyr::spread(matter, processing_time) %>%
        dplyr::mutate(ind = NULL) %>%
        tidyquant::tq_transmute(mutate_fun = xts::apply.monthly,
                                FUN = mean,
                                na.rm = TRUE)

      subject <- unique(df$matter)

      df <- purrr::map(df[-1],  ~ data.frame(date = df$entry_date, .x))

      df <-
        purrr::map(df,  ~ timetk::tk_xts(.x, date_var = date) %>%
                     round(1))

      subject_title <- if (length(subject) == 1) {
        glue::glue('Tempo entre {referencia} e o julgamento para o {theme} ao longo dos meses')

      } else{
        glue::glue(
          'Comparação entre as datas dos assuntos {glue::glue_collapse(theme," , ",last=" e ")} e respectivos julgamentos ao longo dos meses'
        )
      }

      cores <- viridis::viridis(length(df))

      hc <- highcharter::highchart(type = "stock") %>%
        highcharter::hc_title(text = subject_title)


      for (i in seq_along(df)) {
        hc <- hc %>%
          highcharter::hc_add_series(df[[i]], color = cores[i], name = names(df[i]))
      }
    })
    hc

  }
