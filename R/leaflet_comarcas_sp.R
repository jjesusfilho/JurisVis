#' Plota mapa interativo das comarcas de São Paulo com número de casos
#'
#' @param df dataframe com colunas comarca, n,lat e lng
#'
#' @return mapa interativo
#' @export
#'
leaflet_comarcas_sp <- function(df = NULL){


  icone_forum <- leaflet::makeIcon(
    iconUrl = "https://cdn3.iconfinder.com/data/icons/mapicons/icons/justice.png",
    iconWidth = 20, iconHeight = 20,
    iconAnchorX = 22, iconAnchorY = 94,
    #shadowUrl = "http://leafletjs.com/examples/custom-icons/leaf-shadow.png",
    shadowWidth = 50, shadowHeight = 64,
    shadowAnchorX = 4, shadowAnchorY = 62
  )


  leaflet::leaflet() %>%
  leaflet::addTiles() %>%
  leaflet::fitBounds(-51.39139,-24.75958,-45.06320,-20.63579) %>%
  leaflet::addMarkers(lng=df$lng,lat=df$lat,
                      popup=sprintf("Comarca: %s <br> Casos: %d",dfs$comarca,df$n),
                      labelOptions = leaflet::labelOptions(noHide = T, direction = 'top', textOnly = T,textsize="7px"),
                      icon = icone_forum)

}
