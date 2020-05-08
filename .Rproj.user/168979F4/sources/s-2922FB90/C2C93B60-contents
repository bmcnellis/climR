
plot_locations <- read.csv(paste0(getwd(), '/inst/example_datasets/JSFP/plot_locations.csv'),
                           stringsAsFactors = F)


df0 <- data.frame(
  plot = paste0(plot_locations$plot),
  UTM_zone = plot_locations$zone ,
  UTM_E = plot_locations$UTM_E,
  UTM_N = plot_locations$UTM_N,
  datum = plot_locations$datum,
  stringsAsFactors = F
)

sp2 <- new('SpatialPointsDataFrame')
new_CRS <- CRS('+proj=longlat +ellps=WGS84 +datum=WGS84')
for (i in seq(nrow(df0))) {
  i_CRS <- paste0('+proj=utm +zone=',
                  unlist(df0[i, 'UTM_zone']),
                  ' +ellps=WGS84 +datum=',
                  unlist(df0[i, 'datum']),
                  ' +units=m +no_defs')

  i0_CRS <- CRS(i_CRS)

  dd0 <- data.frame(matrix(data = unlist(df0[i, 'plot']), nrow = 1), stringsAsFactors = F)
  colnames(dd0) <- 'plot'

  sp0 <- SpatialPointsDataFrame(
    coords = df0[i, c('UTM_E', 'UTM_N')],
    data = dd0,
    proj4string = i0_CRS)

  sp1 <- spTransform(sp0, new_CRS)

  if (i > 1) {
    sp2 <- rbind(sp2, sp1)
  } else {
    sp2 <- sp1
  }

}

fl0 <- '/media/bem/working/JSFP/climate/PRISM_ppt_30yr_normal_800mM2_annual_bil/PRISM_ppt_30yr_normal_800mM2_annual_bil.bil'
fl1 <- '/media/bem/working/JSFP/climate/PRISM_tmean_30yr_normal_800mM2_annual_bil/PRISM_tmean_30yr_normal_800mM2_annual_bil.bil'
prism1 <- extract_PRISM(fl0, sp2, by = 'plot')
prism2 <- extract_PRISM(fl1, sp2, by = 'plot')
colnames(prism1) <- c('plot', '30_yr_ppt_normal')
colnames(prism2) <- c('plot', '30_yr_tmean_normal')

PRISM_30yr_normals <- dplyr::left_join(prism1, prism2, by = 'plot')
PRISM_30yr_normals[, c(2, 3)] <- round(PRISM_30yr_normals[, c(2, 3)], 2)
use_data(PRISM_30yr_normals, overwrite = T)

sp3 <- as.data.frame(sp2)
colnames(sp3) <- c('PLOT', 'lon', 'lat')

plot_coordinates <- sp3
use_data(plot_coordinates, overwrite = T)
