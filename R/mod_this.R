# Module UI
  
#' @title   mod_this_ui and mod_this_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_this
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_this_ui <- function(id){
  ns <- NS(id)
  tagList(
  
  )
}
    
# Module Server
    
#' @rdname mod_this
#' @export
#' @keywords internal
    
mod_this_server <- function(input, output, session){
  ns <- session$ns
}
    
## To be copied in the UI
# mod_this_ui("this_ui_1")
    
## To be copied in the server
# callModule(mod_this_server, "this_ui_1")
 
