# BEM 2020
library(gridMET)
library(ncdf4)
library(reshape2)


WTH_dir <- '/media/bem/working/RSFIA_data/gridMET'
save_path <- ''
vars <- c('pdsi')
yrs <- c(2000, 2004)
# This function aggregates the variables accross years:
ag_FUN <- function(x) {
  mean(x, na.rm = T)
}


fls <- list.files(WTH_dir)
data("plot_coordinates")
latlon_daily_mat <- subset_gridMET_by_latlon(coords = plot_coordinates, by = 'PLOT', data_dir = WTH_dir, vars = vars, yrs = yrs)
pts_df <- collapse_gridMET_subset(latlon_daily_mat, plot_coordinates, by = 'PLOT', FUN = ag_FUN)


write.csv(pts_df, file = save_path, row.names = F)
