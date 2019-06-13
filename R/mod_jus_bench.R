# Module UI

#' @title   mod_jus_bench_ui and mod_jus_bench_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#' @param data internal
#'
#' @rdname mod_jus_bench
#'
#' @keywords internal
#' @export
#' @importFrom shiny NS tagList
#' @importFrom highcharter highchartOutput renderHighchart
mod_jus_bench_ui <- function(id){
  ns <- NS(id)
  tagList(

  highchartOutput("bench")
  )
}

# Module Server

#' @rdname mod_jus_bench
#' @export
#' @keywords internal

mod_jus_bench_server <- function(input, output, session,timedata){
  ns <- session$ns

  output$bench<-renderHighchart({
    jus_bench()

  })

}

## To be copied in the UI
# mod_jus_bench_ui("jus_bench_ui_1")

## To be copied in the server
# callModule(mod_jus_bench_server, "jus_bench_ui_1")

