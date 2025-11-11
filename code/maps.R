leaflet_basemap <- function() {
  basemap <- leaflet() %>%
    addProviderTiles("CartoDB.Positron")

  return(basemap)
}

leaflet_map_ods <- function(basemap, data) {
  basemap %>%
    addCircleMarkers(
      data = data,
      radius = 0.05
    )
}

map_od_rate_bg <- function(blocks, ods) {
  blocks$count <- lengths(st_intersects(blocks, ods))
  blocks$od_rate <- blocks$count / blocks$estimate * 1000
  # blocks$od_rate <- ifelse(
  #   is.na(blocks$od_rate) | is.infinite(blocks$od_rate) | blocks$od_rate == 0,
  #   0.00001,
  #   blocks$od_rate
  # )

  blocks %>%
    ggplot() +
    geom_sf(aes(fill = od_rate), linewidth = 0) +
    theme_void() +
    scale_fill_viridis_c(
      trans = "log",
      breaks = c(1, 5, 10, 20, 50, 100),
      name = "Opioid Overdoses\nper 1,000 persons",
      guide = guide_legend(
        keyheight = unit(3, units = "mm"),
        keywidth = unit(12, units = "mm"),
        label.position = "bottom",
        title.position = "top",
        nrow = 1
      )
    ) +
    theme(
      legend.position = c(0.2, 0.09)
    )
}
