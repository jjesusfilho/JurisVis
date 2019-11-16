lapso <-
  function (data_inicial = NULL,
            data_final = NULL,
            unidade = "month")
  {
    if (!lubridate::is.Date(data_inicial) |
        !lubridate::is.Date(data_final)) {
      stop("Dates must be in the format iso 8601: yyyy-mm-dd")
    }
    periodo <-
      switch(
        unidade,
        ano = "year",
        mes = "month",
        semana = "week",
        dia = "day"
      )
    lubridate::interval(data_inicial, data_final) %>%
      lubridate::time_length(periodo)
  }
