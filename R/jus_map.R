#' Plots leaflet map
#'
#' This is a page module ...
#' @param id id
#' @param ... Any additional parameters passed to the module
#' @export
#' @importFrom shiny NS fluidRow textInput tagList renderUI
#'     selectInput eventReactive observeEvent observe
#' @importFrom shinydashboard box
#' @importFrom leaflet leafletOutput addTiles leaflet
#'     setView addMarkers clearMarkers leafletProxy
#'     labelOptions fitBounds icons
#' @importFrom dplyr filter pull mutate

mapaui <- function(id){
  ns <- NS(id)
  tagList(

    uiOutput(ns("comarca")),

    leafletOutput(ns("mapa"))


  )
}
#' server
#' @param input The input object
#' @param output The output object
#' @param session The session object
#' @param ... Any additional parameters passed to the module
#' @export
#' @importFrom shiny NS fluidRow textInput tagList renderUI
#'     selectInput eventReactive observeEvent observe
#' @importFrom shinydashboard box
#' @importFrom leaflet leafletOutput addTiles leaflet
#'     setView addMarkers clearMarkers leafletProxy
#'     labelOptions fitBounds icons
#' @importFrom dplyr filter pull mutate
mapa <- function(input, output, session, df){
  ns <- session$ns

  observe({

    output$comarca <- renderUI({

      selectInput("comarcaUI",
                  label = "Selecione a comarca",
                  selected = "São Paulo",
                  choices = unique(df$comarca))

    })


  })



  resposta <-eventReactive(input$mostrar,{

     df %>%
      dplyr::filter(comarca == input$comarca) %>%
      dplyr::filter(decisao %in% c("provido","improvido","parcial")) %>%
      dplyr::mutate(decisao=ifelse(decisao == "improvido","improvido","provido"))
  })



  observe({
    output$mapa <- leaflet::renderLeaflet({
      leaflet() %>%
        addTiles() %>%
        fitBounds(-51.39139,-24.75958,-45.06320,-20.63579)

    })
  })


  observeEvent(input$mostrar,{

    l_mapa<-resposta()

    proxy<-leaflet::leafletProxy("mapa",data=l_mapa) %>%
      leaflet::clearMarkers() %>%
      leaflet::addMarkers(lng=~mean(lng),lat=~mean(lat),
                          popup=sprintf("Comarca: %s <br> Foro: %s <br> Vara: %s <br> Casos: %d",input$comarca,,input$foro,nrow()),
                          labelOptions = labelOptions(noHide = T, direction = 'top', textOnly = T,textsize="7px"),
                          icon=~icons(
                            iconUrl="https://cdn4.iconfinder.com/data/icons/geo-points-1/154/geo-point-location-gps-car-place-512.png",iconWidth = 30,  shadowHeight = 20))


  })

  observeEvent(input$mostrar,{

    ll_mapa<-resposta()

    proxy <- leafletProxy("mapa",data=ll_mapa)

    proxy %>%
      leaflet::setView(lng=mean(ll_mapa$lng),lat=mean(ll_mapa$lng), zoom=6)

  })




}

