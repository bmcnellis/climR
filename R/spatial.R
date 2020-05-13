#' @title Spatial supporting functions
#'
#' @description  Wrappers and supporting functions for the other spatial analysis packages (sp, rgdal, etc.)
#'
#' @name spatial
#' @author Brandon McNellis
NULL
#' @rdname spatial
#' @export
gg_shapefile <- function(shp) {
  require(ggplot2)
  require(plyr)

  state_overlay <- spData::us_states[which(spData::us_states$REGION == 'West'), ]
  state_overlay_spatial <- sf::as_Spatial(state_overlay)
  shp_sf <- sf_as_st(state_overlay_spatial)

  shp_over <- over()

  shp@data$Fire_Name <- as.character(shp@data$Fire_Name)
  shp@data$id = rownames(shp@data)
  shp.points = fortify(shp, region = "id")
  shp.df = join(shp.points, shp@data, by = "id")

  plot0 <- ggplot() +
    geom_sf(aes_(), data = state_overlay)

  plot0 <- plot0 +
    geom_polygon(data = shp.df, aes(long, lat, group = group, fill = Fire_Name)) +
    geom_path(color = "red") +
    theme(legend.position = 'none')

  plot0 <- ggplot() +
    geom_polygon(data = shp.df, aes(long, lat, group = group, fill = Fire_Name)) +
    geom_path(color = "black") +
    coord_equal() +
    theme(legend.position = 'none')

  plot0
}
