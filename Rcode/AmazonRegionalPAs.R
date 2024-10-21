############### Preparing regional Amazon protected areas ########################
# Date: 10-9-24
# updated: 10-21-24
# Author: Ian McCullough, immccull@gmail.com
###################################################################################

#### R libraries ####
library(terra)
library(tidyr)

#### Input data ####
setwd("C:/Users/immccull/Documents/AmazonClimateCorridors")

# regions
morrone2022_4674_amazon <- terra::vect("Regions/Morrone2022/Morrone2022_amazon_4674.shp")

# dissolved protected area complexes (done in QGIS
# includes reserva florestal, indigenous, Los Amigos, nacional, departamental, bosque protector)
dissolved_PA <- terra::vect("protected_areas/Amazon_merged_PAs/Amazon_merged_PAs_dissolved.shp")

# DEM
DEM <- terra::rast("DEM/SA_DEM_mosaic.tif")
DEM_4674 <- terra::rast("DEM/SA_DEM_mosaic_4674.tif")
DEM_4674_morrone <- terra::rast("DEM/SA_DEM_mosaic_4674_morrone.tif")

# study area
amazon_study_area <- terra::vect("RAISG/Limites2023/Limites/Lim_Biogeografico.shp")

# if already run
dissolved_PA_elev_df_full <- read.csv("protected_areas/Amazon_merged_PAs/dissolved_PA_elev_stats.csv")

#### Main program ####
# calculate PA complex area and eliminate areas below 10km2
dissolved_PA$areasqkm <- terra::expanse(dissolved_PA, "km")
summary(dissolved_PA$areasqkm)
hist(dissolved_PA$areasqkm)
dissolved_PA_10km <- subset(dissolved_PA, dissolved_PA$areasqkm > 10)
nrow(dissolved_PA)
nrow(dissolved_PA_10km)
dissolved_PA_10km$PAcomplexID <- seq(1, nrow(dissolved_PA_10km), 1)

plot(amazon_study_area)
plot(dissolved_PA_10km, add=T, col='lightgreen')

## calculate elevation stats for PA complexes
# first have to reproject DEM and crop/mask to study area to make it more manageable
#DEM_4674 <- terra::project(DEM, "EPSG:4674", filename="DEM/SA_DEM_mosaic_4674.tif", overwrite=T) 
#DEM_4674_morrone <- terra::crop(DEM_4674, morrone2022_4674_amazon, mask=T, filename="DEM/SA_DEM_mosaic_4674_morrone.tif", overwrite=T)

# dissolved_PA_elev_mean <- terra::extract(DEM_4674_morrone, dissolved_PA_10km, fun="mean", na.rm=T)
# dissolved_PA_elev_median <- terra::extract(DEM_4674_morrone, dissolved_PA_10km, fun="median", na.rm=T)
# dissolved_PA_elev_min <- terra::extract(DEM_4674_morrone, dissolved_PA_10km, fun="min", na.rm=T)
# dissolved_PA_elev_max <- terra::extract(DEM_4674_morrone, dissolved_PA_10km, fun="max", na.rm=T)
# 
# dissolved_PA_elev_df <- cbind.data.frame(dissolved_PA_elev_mean,
#                                          dissolved_PA_elev_min,
#                                          dissolved_PA_elev_median,
#                                          dissolved_PA_elev_max)
# 
# dissolved_PA_elev_df <- dissolved_PA_elev_df[,c(1,2,4,6,8)]
# names(dissolved_PA_elev_df) <- c('ID','mean_elev_m','min_elev_m','median_elev_m','max_elev_m')
# #dissolved_PA_elev_df$range_elev_m <- dissolved_PA_elev_df$max_elev_m - dissolved_PA_elev_min
# summary(dissolved_PA_elev_df)
# 
# dissolved_PA_elev_df_full <- cbind.data.frame(dissolved_PA_elev_df, dissolved_PA_10km)
# 
# # range was calculating funny (maybe because of NA rows)
# # was generating an ID and range m column, but environment did not seem to count both columns
# test <- dissolved_PA_elev_df_full %>% 
#   drop_na(mean_elev_m)
# test$range_elev_m <- test$max_elev_m - test$min_elev_m
# test <- test[,c(1:5, 55, 54, 6:53)]

#write.csv(test, file='protected_areas/Amazon_merged_PAs/dissolved_PA_elev_stats.csv', row.names=F)

## Calculate elevation stats per Morrone ecoregion ##
# testing loop operations
# morrone2022_4674_amazon$regionID <- seq(1, nrow(morrone2022_4674_amazon), 1)
# region1 <- subset(morrone2022_4674_amazon, morrone2022_4674_amazon$regionID==1)
# region1_PA <- terra::intersect(region1, dissolved_PA)
# region1_PA_complex <- terra::intersect(region1, dissolved_PA_10km)
# region1_PA_complex_pct <- sum(terra::expanse(region1_PA_complex, "km"))/sum(terra::expanse(region1, "km"))*100
# at least in this region, seems that PA complexes capture almost all of the total PA
# was 99.9%
# plot(region1)
# plot(region1_PA, add=T, col='red')
# plot(region1_PA_complex, add=T, col='blue')
# sum(terra::expanse(region1_PA, "km"))
# sum(terra::expanse(region1_PA_complex, "km"))
# sum(terra::expanse(region1_PA_complex, "km"))/sum(terra::expanse(region1_PA, "km"))

# loop through regions to calculate % protection
PA_df_list <- list()
for (i in 1:nrow(morrone2022_4674_amazon)){
  region <- subset(morrone2022_4674_amazon, morrone2022_4674_amazon$regionID==i)
  region_PA <- terra::intersect(region, dissolved_PA)
  region_PA_complex <- terra::intersect(region, dissolved_PA_10km)
  region_PA_pct <- sum(terra::expanse(region_PA, "km"))/sum(terra::expanse(region, "km"))*100
  region_PA_complex_pct <- sum(terra::expanse(region_PA_complex, "km"))/sum(terra::expanse(region, "km"))*100
  n_PA_complexes <- nrow(region_PA_complex)
  n_PA <- nrow(region_PA)
  tmp_df <- data.frame(PA_complexes_pct=region_PA_complex_pct, PA_all_pct=region_PA_pct,
                       nPA_all=n_PA, nPA_complexes=n_PA_complexes, 
                       regionID=i, region=region$Provincias)
  PA_df_list[[i]] <- tmp_df
}
test <- do.call(rbind.data.frame, PA_df_list)
#write.csv(test, file="RAISG/morrone2022_PA_stats.csv", row.names=F)
hist(test$PA_complexes_pct, main='Regional protection', xlab='% protection')
hist(test$nPA_complexes, main='Regional protection', xlab='Number of PA complexes')
                
#### PA complexes by region: exploring nodes ####
# region1 <- subset(morrone2022_4674_amazon, morrone2022_4674_amazon$regionID==1)
# region1_PA_complex <- terra::intersect(region1, dissolved_PA_10km)
# region1_PA_complex_elev_mean <- terra::extract(DEM_4674_morrone, region1_PA_complex, fun="mean", na.rm=T)
# region1_PA_complex_elev_min <- terra::extract(DEM_4674_morrone, region1_PA_complex, fun="min", na.rm=T)
# region1_PA_complex_elev_max <- terra::extract(DEM_4674_morrone, region1_PA_complex, fun="max", na.rm=T)
# region1_PA_complex_elev_median <- terra::extract(DEM_4674_morrone, region1_PA_complex, fun="median", na.rm=T)
# 
# region1_PA_complex_elev_df <- cbind.data.frame(region1_PA_complex_elev_mean,
#                                           region1_PA_complex_elev_min,
#                                           region1_PA_complex_elev_median,
#                                           region1_PA_complex_elev_max)
# 
# region1_PA_complex_elev_df <- region1_PA_complex_elev_df[,c(1,2,4,6,8)]
# names(region1_PA_complex_elev_df) <- c('ID','mean_elev_m','min_elev_m','median_elev_m','max_elev_m')
# region1_PA_complex_elev_df$range_elev_m <- region1_PA_complex_elev_df$max_elev_m - region1_PA_complex_elev_df$min_elev_m
# test2 <- region1_PA_complex_elev_df
# test2$max_elev_m_rank <- rank(-test2$max_elev_m, na.last=T, ties.method="first")
# test2$max_elev_m_rank_pct <- (test2$max_elev_m_rank/nrow(test2))*100
# end_threshold <- 10 #percentile
# test2$node <- ifelse(test2$max_elev_m_rank_pct <= end_threshold, "End","Start")
# 
# 
# region1_shp <- cbind(region1_PA_complex, test2) 
# plot(region1)
# plot(region1_shp, "node", add=T)
# 
# # WHAT region is this?
# plot(morrone2022_4674_amazon)
# plot(region1, add=T, col='dodgerblue')

region_elev_list <- list()
end_threshold <- 20# percent; this percentile and higher are considered end nodes based on PA max elevation
for (i in 1:nrow(morrone2022_4674_amazon)){
  region <- subset(morrone2022_4674_amazon, morrone2022_4674_amazon$regionID==i)
  region_PA_complex <- terra::intersect(region, dissolved_PA_10km)
  region_PA_complex_elev_mean <- terra::extract(DEM_4674_morrone, region_PA_complex, fun="mean", na.rm=T)
  region_PA_complex_elev_min <- terra::extract(DEM_4674_morrone, region_PA_complex, fun="min", na.rm=T)
  region_PA_complex_elev_max <- terra::extract(DEM_4674_morrone, region_PA_complex, fun="max", na.rm=T)
  region_PA_complex_elev_median <- terra::extract(DEM_4674_morrone, region_PA_complex, fun="median", na.rm=T)
  
  region_PA_complex_elev_df <- cbind.data.frame(region_PA_complex_elev_mean,
                                                region_PA_complex_elev_min,
                                                region_PA_complex_elev_median,
                                                region_PA_complex_elev_max)
  
  region_PA_complex_elev_df <- region_PA_complex_elev_df[,c(1,2,4,6,8)]
  names(region_PA_complex_elev_df) <- c('ID','mean_elev_m','min_elev_m','median_elev_m','max_elev_m')
  region_PA_complex_elev_df$range_elev_m <- region_PA_complex_elev_df$max_elev_m - region_PA_complex_elev_df$min_elev_m
  region_PA_complex_elev_df$max_elev_m_rank <- rank(-region_PA_complex_elev_df$max_elev_m, na.last=T, ties.method="first")
  region_PA_complex_elev_df$max_elev_m_rank_pct <- (region_PA_complex_elev_df$max_elev_m_rank/nrow(region_PA_complex_elev_df))*100
  
  region_PA_complex_elev_df$node <- ifelse(region_PA_complex_elev_df$max_elev_m_rank_pct <= end_threshold, "End","Start")
  region_PA_complex_elev_df$region <- region$Provincias
  region_PA_complex_elev_df$areasqkm <- terra::expanse(region_PA_complex, "km")
  region_PA_complex_elev_df$PAcomplexID <- region_PA_complex$PAcomplexID
  region_elev_list[[i]] <- region_PA_complex_elev_df
}
test2 <- do.call(rbind.data.frame, region_elev_list)