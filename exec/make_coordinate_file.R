library(sp)

plot_locations <- read.csv('/mnt/bem/working/JSFP/climate/plot_locations.csv')

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

sp2$plot <- JFSPdata1::key_plots(sp2$plot, key_fl = '/mnt/bem/working/JSFP/data_7May2020/JFSPdata1/inst/keys/plot_key.csv')
plot_locations <- sp2
save(plot_locations, file = '/mnt/bem/working/JSFP/climate/plot_locations.Rdata')
