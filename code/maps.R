leaflet_basemap <- function(){
  basemap <- leaflet() %>%
    # add different provider tiles
    addProviderTiles(
      "OpenStreetMap",
      # give the layer a name
      group = "OpenStreetMap"
    ) %>%
    addProviderTiles(
      "Esri.WorldStreetMap",
      group = "Esri.WorldStreetMap"
    ) %>%
    addProviderTiles(
      "CartoDB.Positron",
      group = "CartoDB.Positron"
    ) %>%
    addProviderTiles(
      "Esri.WorldImagery",
      group = "Esri.WorldImagery"
    ) %>%
    # add a layers control
    addLayersControl(
      baseGroups = c(
        "OpenStreetMap", "Stamen.Toner",
        "Stamen.Terrain", "Esri.WorldStreetMap",
        "Wikimedia", "CartoDB.Positron", "Esri.WorldImagery"
      ),
      # position it on the topleft
      position = "topleft"
    )
  
  return(basemap)
}

leaflet_map_ods <- function(basemap,data){
  basemap %>%
    addCircleMarkers(
      data = data,
      radius = 0.05)
}
