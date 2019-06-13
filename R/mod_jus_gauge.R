# Module UI

#' @title   mod_jus_gauge_ui and mod_jus_gauge_server
#' @description  gauge to show predicted results.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_jus_gauge
#'
#' @keywords internal
#' @export
#' @importFrom shiny NS tagList
mod_jus_gauge_ui <- function(id){
  ns <- NS(id)
  tagList(

  )
}

# Module Server

#' @rdname mod_jus_gauge
#' @export
#' @keywords internal

mod_jus_gauge_server <- function(input, output, session){
  ns <- session$ns
}

## To be copied in the UI
# mod_jus_gauge_ui("jus_gauge_ui_1")

## To be copied in the server
# callModule(mod_jus_gauge_server, "jus_gauge_ui_1")

