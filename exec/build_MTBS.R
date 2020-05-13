library(climR)
MTBS_dir <- '/mnt/bem/working/JSFP/MTBS/mtbs_perimeter_data'

MTBS_shp <- get_MTBS(MTBS_dir)

mtbs_2000 <- filter_MTBS_by_year(MTBS_shp, 2000)
gg_shapefile(mtbs_2000)

GB_sects <- c('341A', '341G', '341D', '341E', '341F')
CP_sects <- c('313A', '341B', '313B')
MO_sects <- c('322A')
SO_sects <- c('322B', '322C')
CH_sects <- c('321A')

GB_filt <- filter_MTBS_by_Cleland(MTBS_shp, GB_sects)
CP_filt <- filter_MTBS_by_Cleland(MTBS_shp, CP_sects)
MO_filt <- filter_MTBS_by_Cleland(MTBS_shp, MO_sects)
SO_filt <- filter_MTBS_by_Cleland(MTBS_shp, SO_sects)
CH_filt <- filter_MTBS_by_Cleland(MTBS_shp, CH_sects)
