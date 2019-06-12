#' Modulo
#'
#' This is a page module ...
#' @param input The input object
#' @param output The output object
#' @param session The session object
#' @param data Global data passed on by global.R
#' @param ... Any additional parameters passed to the module
#' @examples
#' @export
#' @importFrom shiny NS fluidRow
#' @importFrom shiny textInput
#' @importFrom shinydashboard box
#' @importFrom shiny tagList
#' @importFrom leaflet leafletOutput

mapaui <- function(id){
  ns <- NS(id)
  tagList(


    leafletOutput(ns("mapa"))


  )
}

mapa <- function(input, output, session,conn){
  ns <- session$ns

  observeEvent(input$comarca,{

    foros <-  df %>%
      filter(comarca==input$comarca) %>%
      pull("foro") %>%
      unique()

    output$foro <- renderUI({


      selectInput("foroUI",
                  "Selecione o foro:",
                  choices = foros)

    })
  })

  observeEvent(input$foro,{

    varas <- df %>%
      dplyr::filter(comarca == input$comarca) %>%
      dplyr::filter(foro == input$foro) %>%
      dplyr::pull("vara") %>%
      unique()

    output$vara <- renderUI({

      selectInput("varaUI",
                  "Selecione a vara:",
                  choices = varas)

    })

  })

  resposta<-eventReactive(input$mostrar,{

     df %>%
      dplyr::filter(comarca == input$comarca) %>%
      dplyr::filter(foro == input$foro) %>%
      dplyr::filter(vara == input$vara) %>%
      dplyr::filter(decisao %in% c("provido","improvido","parcial")) %>%
      dplyr::mutate(decisao=ifelse(decisao=="improvido","improvido","provido"))
  })



  observe({
    output$mapa <- leaflet::renderLeaflet({
      leaflet::leaflet() %>%
        leaflet::addTiles() %>%
        leaflet::fitBounds(-51.39139,-24.75958,-45.06320,-20.63579)

    })
  })


  observeEvent(input$mostrar,{

    l_mapa<-resposta()

    proxy<-leaflet::leafletProxy("mapa",data=l_mapa) %>%
      leaflet::clearMarkers() %>%
      leaflet::addMarkers(lng=~mean(longitude),lat=~mean(latitude),
                          popup=sprintf("Comarca: %s <br> Foro: %s <br> Vara: %s <br> Casos: %d",input$comarca,,input$foro,nrow()),
                          labelOptions = labelOptions(noHide = T, direction = 'top', textOnly = T,textsize="7px"),
                          icon=~icons(
                            iconUrl="https://cdn4.iconfinder.com/data/icons/geo-points-1/154/geo-point-location-gps-car-place-512.png",iconWidth = 30,  shadowHeight = 20))


  })

  observeEvent(input$mostrar,{

    ll_mapa<-resposta()

    proxy <- leaflet::leafletProxy("mapa",data=ll_mapa)

    proxy %>%
      leaflet::setView(lng=mean(ll_mapa$longitude),lat=mean(ll_mapa$latitude), zoom=6)

  })




}

