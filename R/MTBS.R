#' @title Functions for MTBS manipulation
#'
#' @description  NA
#'
#' @name MTBS
#' @author Brandon McNellis
NULL
#' @rdname MTBS
#' @export
get_MTBS <- function(dir) {

  shp_fl <- list.files(dir)[grep(x = list.files(dir), pattern = "\\.shp$")]
  MTBS_shp <- rgdal::readOGR(paste0(dir, '/', shp_fl))

  return(MTBS_shp)
}
#' @rdname MTBS
#' @export
filter_MTBS_by_Cleland <- function(shp, sections) {
  eco_map <- ClelandEcoregions::Cleland2007_eco_map

  sects <- eco_map[[2]]
  sects$MAP_UNIT_S <- as.character(sects$MAP_UNIT_S)

  out_df <- data.frame(matrix(nrow = 0, ncol = 8), stringsAsFactors = F)
  colnames(out_df) <- c('section', 'Fire_ID', 'Fire_Name', 'Year', 'StartMonth', 'StartDay', 'Fire_Type', 'Acres')

  for (i in seq_along(sections)) {
    cat('\n', round(i / length(sections) * 100, 2), ' %     ')
    ii <- sections[i]
    stopifnot(ii %in% sects$MAP_UNIT_S)

    i_sect <- sects[which(sects$MAP_UNIT_S == ii), ]
    stop()
    i_fires <- over(i_sect, shp)
    i_fires <- i_fires[complete.cases(i_fires), ]
    i_fires <- data.frame(section = rep(ii, nrow(i_fires)), i_fires, stringsAsFactors = F)

    out_df <- rbind(out_df, i_fires)

  }

  return(out_df)
}
#' @rdname MTBS
#' @export
filter_MTBS_by_year <- function(shp, years) {
  shp_sub <- shp[which(shp$Year %in% years), ]
  return(shp_sub)
}
