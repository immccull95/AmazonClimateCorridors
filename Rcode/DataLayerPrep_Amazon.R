### Assembling data layers for Amazon climate corridor 
# Date: 5-30-24
# updated: 11-17-24: calculate LULC stats

#### R libraries ####
library(terra)
library(dplyr)

#### Input data ####
setwd("C:/Users/immccull/Documents/AmazonClimateCorridors")

southamerica <- terra::vect("RAISG/SA_Countries/SouthAmerica.shp")

amazon_study_area <- terra::vect("RAISG/Limites2023/Limites/Lim_Biogeografico.shp")
amazon_study_area <- terra::project(amazon_study_area, "EPSG:29172")

area <- terra::expanse(amazon_study_area, unit="km")

## DEM
DEM <- terra::rast("DEM/SA_DEM_mosaic.tif")

## canopy height (Potapov et al. 2019)
canopy <- terra::rast("C:/Users/immccull/Documents/GLAD_GCH_2019/Forest_height_2019_SAM.tif")

## protected areas
bosque_protector <- terra::vect("RAISG/Anps2023-1/Anps/ANP_BosqueProtector.shp")
departamental <- terra::vect("RAISG/Anps2023-1/Anps/ANP_departamental.shp")
nacional <- terra::vect("RAISG/Anps2023-1/Anps/ANP_nacional.shp")
reserva_florestal <- terra::vect("RAISG/Anps2023-1/Anps/ANP_ReservaFlorestal.shp")
indigenous_territories <- terra::vect("RAISG/Tis2023-1/Tis/Tis_territoriosIndigenas.shp")
losamigos <- terra::vect("protected_areas/LosAmigos/Los amigos conservation concesion.shp")

# put into same CRS as elevation data
bosque_protector <- terra::project(bosque_protector, "EPSG:4326")
departamental <- terra::project(departamental, "EPSG:4326")
nacional <- terra::project(nacional, "EPSG:4326")
reserva_florestal <- terra::project(reserva_florestal, "EPSG:4326")
indigenous_territories <- terra::project(indigenous_territories, "EPSG:4326")
losamigos <- terra::project(losamigos, "EPSG:4326")

# Need to add in Los Amigos manually; does not really matter into which layer
test <- terra::union(bosque_protector, losamigos)
test[61,1] <- 'Perú'
test[61,5] <- 'LosAmigos'
bosque_protector <- terra::project(test, "EPSG:4326")

# LULC
LULC <- terra::rast("LCC/LCC_amazon_mosaic_500m.tif")

#### Main program ####
# calculate polygon size for each, remove any below 5km2
bosque_protector$areasqkm <- terra::expanse(bosque_protector, unit="km")
departamental$areasqkm <- terra::expanse(departamental, unit="km")
nacional$areasqkm <- terra::expanse(nacional, unit="km")
reserva_florestal$areasqkm <- terra::expanse(reserva_florestal, unit="km")
indigenous_territories$areasqkm <- terra::expanse(indigenous_territories, unit="km")

bosque_protector_5km <- subset(bosque_protector, bosque_protector$areasqkm >= 5)
departamental_5km <- subset(departamental, departamental$areasqkm >= 5)
nacional_5km <- subset(nacional, nacional$areasqkm >= 5)
reserva_florestal_5km <- subset(reserva_florestal, reserva_florestal$areasqkm >= 5)
indigenous_territories_5km <- subset(indigenous_territories, indigenous_territories$areasqkm >= 5)

# update: increase minimum threshold to 10km2
bosque_protector_10km <- subset(bosque_protector, bosque_protector$areasqkm >= 10)
departamental_10km <- subset(departamental, departamental$areasqkm >= 10)
nacional_10km <- subset(nacional, nacional$areasqkm >= 10)
reserva_florestal_10km <- subset(reserva_florestal, reserva_florestal$areasqkm >= 10)
indigenous_territories_10km <- subset(indigenous_territories, indigenous_territories$areasqkm >= 10)


# calculate elevation stats for remaining polygons >= 5km2
# Note: bosque protector has LosAmigos
# bosque_protector_5km_elev_mean <- terra::extract(DEM, bosque_protector_5km, fun="mean", na.rm=T)
# bosque_protector_5km_elev_median <- terra::extract(DEM, bosque_protector_5km, fun="median", na.rm=T)
# bosque_protector_5km_elev_min <- terra::extract(DEM, bosque_protector_5km, fun="min", na.rm=T)
# bosque_protector_5km_elev_max <- terra::extract(DEM, bosque_protector_5km, fun="max", na.rm=T)
# 
# bosque_protector_5km_elev <- cbind.data.frame(bosque_protector_5km_elev_min, bosque_protector_5km_elev_median, bosque_protector_5km_elev_max, bosque_protector_5km_elev_mean)
# bosque_protector_5km_elev <- bosque_protector_5km_elev[,c(1,2,4,6,8)]
# names(bosque_protector_5km_elev) <- c('rowID','min_m', 'median_m','max_m','mean_m')
# bosque_protector_5km_elev$rowID <- seq(1, nrow(bosque_protector_5km_elev), 1)
# bosque_protector_5km$rowID <- seq(1, nrow(bosque_protector_5km), 1)
# bosque_protector_5km_elev <- merge(bosque_protector_5km, bosque_protector_5km_elev, by='rowID')
# bosque_protector_5km_elev$ElevZone <- ifelse(bosque_protector_5km_elev$mean_m <= 500, "Lowland", NA)
# bosque_protector_5km_elev$ElevZone <- ifelse(bosque_protector_5km_elev$mean_m >= 1500, "Highland", bosque_protector_5km_elev$ElevZone)
# bosque_protector_5km_elev_df <- as.data.frame(bosque_protector_5km_elev)
# table(bosque_protector_5km_elev_df$ElevZone)
# #write.csv(bosque_protector_5km_elev_df, file="RAISG/Anps2023-1/Anps/bosque_protector_5km_wElev.csv", row.names=F)
# #writeVector(bosque_protector_5km_elev, "RAISG/Anps2023-1/Anps/bosque_protector_5km_wElev.shp", overwrite=T)
# 
# departamental_5km_elev_mean <- terra::extract(DEM, departamental_5km, fun="mean", na.rm=T)
# departamental_5km_elev_median <- terra::extract(DEM, departamental_5km, fun="median", na.rm=T)
# departamental_5km_elev_min <- terra::extract(DEM, departamental_5km, fun="min", na.rm=T)
# departamental_5km_elev_max <- terra::extract(DEM, departamental_5km, fun="max", na.rm=T)
# 
# departamental_5km_elev <- cbind.data.frame(departamental_5km_elev_min, departamental_5km_elev_median, departamental_5km_elev_max, departamental_5km_elev_mean)
# departamental_5km_elev <- departamental_5km_elev[,c(1,2,4,6,8)]
# names(departamental_5km_elev) <- c('rowID','min_m', 'median_m','max_m','mean_m')
# departamental_5km_elev$rowID <- seq(1, nrow(departamental_5km_elev), 1)
# departamental_5km$rowID <- seq(1, nrow(departamental_5km), 1)
# departamental_5km_elev <- merge(departamental_5km, departamental_5km_elev, by='rowID')
# departamental_5km_elev$ElevZone <- ifelse(departamental_5km_elev$mean_m <= 500, "Lowland", NA)
# departamental_5km_elev$ElevZone <- ifelse(departamental_5km_elev$mean_m >= 1500, "Highland", departamental_5km_elev$ElevZone)
# departamental_5km_elev_df <- as.data.frame(departamental_5km_elev)
# table(departamental_5km_elev_df$ElevZone)
# #write.csv(departamental_5km_elev_df, file="RAISG/Anps2023-1/Anps/departamental_5km_wElev.csv", row.names=F)
# #writeVector(departamental_5km_elev, "RAISG/Anps2023-1/Anps/departamental_5km_wElev.shp", overwrite=T)
# 
# nacional_5km_elev_mean <- terra::extract(DEM, nacional_5km, fun="mean", na.rm=T)
# nacional_5km_elev_median <- terra::extract(DEM, nacional_5km, fun="median", na.rm=T)
# nacional_5km_elev_min <- terra::extract(DEM, nacional_5km, fun="min", na.rm=T)
# nacional_5km_elev_max <- terra::extract(DEM, nacional_5km, fun="max", na.rm=T)
# 
# nacional_5km_elev <- cbind.data.frame(nacional_5km_elev_min, nacional_5km_elev_median, nacional_5km_elev_max, nacional_5km_elev_mean)
# nacional_5km_elev <- nacional_5km_elev[,c(1,2,4,6,8)]
# names(nacional_5km_elev) <- c('rowID','min_m', 'median_m','max_m','mean_m')
# nacional_5km_elev$rowID <- seq(1, nrow(nacional_5km_elev), 1)
# nacional_5km$rowID <- seq(1, nrow(nacional_5km), 1)
# nacional_5km_elev <- merge(nacional_5km, nacional_5km_elev, by='rowID')
# nacional_5km_elev$ElevZone <- ifelse(nacional_5km_elev$mean_m <= 500, "Lowland", NA)
# nacional_5km_elev$ElevZone <- ifelse(nacional_5km_elev$mean_m >= 1500, "Highland", nacional_5km_elev$ElevZone)
# nacional_5km_elev_df <- as.data.frame(nacional_5km_elev)
# table(nacional_5km_elev_df$ElevZone)
# #write.csv(nacional_5km_elev_df, file="RAISG/Anps2023-1/Anps/nacional_5km_wElev.csv", row.names=F)
# #writeVector(nacional_5km_elev, "RAISG/Anps2023-1/Anps/nacional_5km_wElev.shp", overwrite=T)
# 
# reserva_florestal_5km_elev_mean <- terra::extract(DEM, reserva_florestal_5km, fun="mean", na.rm=T)
# reserva_florestal_5km_elev_median <- terra::extract(DEM, reserva_florestal_5km, fun="median", na.rm=T)
# reserva_florestal_5km_elev_min <- terra::extract(DEM, reserva_florestal_5km, fun="min", na.rm=T)
# reserva_florestal_5km_elev_max <- terra::extract(DEM, reserva_florestal_5km, fun="max", na.rm=T)
# 
# reserva_florestal_5km_elev <- cbind.data.frame(reserva_florestal_5km_elev_min, reserva_florestal_5km_elev_median, reserva_florestal_5km_elev_max, reserva_florestal_5km_elev_mean)
# reserva_florestal_5km_elev <- reserva_florestal_5km_elev[,c(1,2,4,6,8)]
# names(reserva_florestal_5km_elev) <- c('rowID','min_m', 'median_m','max_m','mean_m')
# reserva_florestal_5km_elev$rowID <- seq(1, nrow(reserva_florestal_5km_elev), 1)
# reserva_florestal_5km$rowID <- seq(1, nrow(reserva_florestal_5km), 1)
# reserva_florestal_5km_elev <- merge(reserva_florestal_5km, reserva_florestal_5km_elev, by='rowID')
# reserva_florestal_5km_elev$ElevZone <- ifelse(reserva_florestal_5km_elev$mean_m <= 500, "Lowland", NA)
# reserva_florestal_5km_elev$ElevZone <- ifelse(reserva_florestal_5km_elev$mean_m >= 1500, "Highland", reserva_florestal_5km_elev$ElevZone)
# reserva_florestal_5km_elev_df <- as.data.frame(reserva_florestal_5km_elev)
# table(reserva_florestal_5km_elev_df$ElevZone)
# #write.csv(reserva_florestal_5km_elev_df, file="RAISG/Anps2023-1/Anps/reserva_florestal_5km_wElev.csv", row.names=F)
# #writeVector(reserva_florestal_5km_elev, "RAISG/Anps2023-1/Anps/reserva_florestal_5km_wElev.shp", overwrite=T)
# 
# indigenous_territories_5km_elev_mean <- terra::extract(DEM, indigenous_territories_5km, fun="mean", na.rm=T)
# indigenous_territories_5km_elev_median <- terra::extract(DEM, indigenous_territories_5km, fun="median", na.rm=T)
# indigenous_territories_5km_elev_min <- terra::extract(DEM, indigenous_territories_5km, fun="min", na.rm=T)
# indigenous_territories_5km_elev_max <- terra::extract(DEM, indigenous_territories_5km, fun="max", na.rm=T)
# 
# indigenous_territories_5km_elev <- cbind.data.frame(indigenous_territories_5km_elev_min, indigenous_territories_5km_elev_median, indigenous_territories_5km_elev_max, indigenous_territories_5km_elev_mean)
# indigenous_territories_5km_elev <- indigenous_territories_5km_elev[,c(1,2,4,6,8)]
# names(indigenous_territories_5km_elev) <- c('rowID','min_m', 'median_m','max_m','mean_m')
# indigenous_territories_5km_elev$rowID <- seq(1, nrow(indigenous_territories_5km_elev), 1)
# indigenous_territories_5km$rowID <- seq(1, nrow(indigenous_territories_5km), 1)
# indigenous_territories_5km_elev <- merge(indigenous_territories_5km, indigenous_territories_5km_elev, by='rowID')
# indigenous_territories_5km_elev$ElevZone <- ifelse(indigenous_territories_5km_elev$mean_m <= 500, "Lowland", NA)
# indigenous_territories_5km_elev$ElevZone <- ifelse(indigenous_territories_5km_elev$mean_m >= 1500, "Highland", indigenous_territories_5km_elev$ElevZone)
# indigenous_territories_5km_elev_df <- as.data.frame(indigenous_territories_5km_elev)
# table(indigenous_territories_5km_elev_df$ElevZone)
# #write.csv(indigenous_territories_5km_elev_df, file="RAISG/Anps2023-1/Anps/indigenous_territories_5km_wElev.csv", row.names=F)
# #writeVector(indigenous_territories_5km_elev, "RAISG/Anps2023-1/Anps/indigenous_territories_5km_wElev.shp", overwrite=T)
# 
# #### Prepare nodes for connectivity analyses ####
# bosque_protector_5km_elev_df <- read.csv("RAISG/Anps2023-1/Anps/bosque_protector_5km_wElev.csv")
# bosque_protector_5km_elev <- terra::vect("RAISG/Anps2023-1/Anps/bosque_protector_5km_wElev.shp")
# 
# indigenous_territories_5km_elev_df <- read.csv("RAISG/Anps2023-1/Anps/indigenous_territories_5km_wElev.csv")
# indigenous_territories_5km_elev <- terra::vect("RAISG/Anps2023-1/Anps/indigenous_territories_5km_wElev.shp")
# 
# reserva_florestal_5km_elev_df <- read.csv("RAISG/Anps2023-1/Anps/reserva_florestal_5km_wElev.csv")
# reserva_florestal_5km_elev <- terra::vect("RAISG/Anps2023-1/Anps/reserva_florestal_5km_wElev.shp")
# 
# departamental_5km_elev_df <- read.csv("RAISG/Anps2023-1/Anps/departamental_5km_wElev.csv")
# departamental_5km_elev <- terra::vect("RAISG/Anps2023-1/Anps/departamental_5km_wElev.shp")
# 
# nacional_5km_elev_df <- read.csv("RAISG/Anps2023-1/Anps/nacional_5km_wElev.csv")
# nacional_5km_elev <- terra::vect("RAISG/Anps2023-1/Anps/nacional_5km_wElev.shp")
# 
# colnames_keep <- c('pais','nombre','categoria','categoriaa',
#                    'areasqkm','min_m','median_m','max_m','mean_m','ElevZone')
# 
# all_nodes_df <- rbind.data.frame(bosque_protector_5km_elev_df[,colnames_keep],
#                                  nacional_5km_elev_df[,colnames_keep],
#                                  departamental_5km_elev_df[,colnames_keep],
#                                  reserva_florestal_5km_elev_df[,colnames_keep],
#                                  indigenous_territories_5km_elev_df[,colnames_keep])
# all_nodes_shp <- rbind(bosque_protector_5km_elev[,colnames_keep],
#                        nacional_5km_elev[,colnames_keep],
#                        departamental_5km_elev[,colnames_keep],
#                        reserva_florestal_5km_elev[,colnames_keep],
#                        indigenous_territories_5km_elev[,colnames_keep])
# 
# end_nodes_shp <- subset(all_nodes_shp, all_nodes_shp$ElevZone=='Highland')
# start_nodes_shp <- subset(all_nodes_shp, all_nodes_shp$ElevZone=='Lowland')

## Update July 2024: experiment with reducing number of end nodes
#end_nodes_shp_dissolved <- terra::aggregate(end_nodes_shp) # use Q; much faster

# end_nodes_dissolved_5km <- terra::vect("end_nodes/tump/end_nodes_dissolved_5km.shp")
# end_nodes_dissolved_5km_area <- terra::expanse(end_nodes_dissolved_5km, "km")
# summary(end_nodes_dissolved_5km_area)
# hist(end_nodes_dissolved_5km_area)
# 
# end_nodes_dissolved_5km_df <- as.data.frame(end_nodes_dissolved_5km)
# end_nodes_dissolved_5km_df$dissolved_areasqkm <- terra::expanse(end_nodes_dissolved_5km, "km")
# nrow(subset(end_nodes_dissolved_5km_df, dissolved_areasqkm < 10))
# 
# # play around with size of end nodes without dissolving
# end_nodes_shp_df <- as.data.frame(end_nodes_shp)
# summary(end_nodes_shp_df$areasqkm)
# nrow(subset(end_nodes_shp_df, areasqkm < 50)) #tried 10, 25, 50 km2
# 
# # play around with size of start nodes
# start_nodes_shp_df <- as.data.frame(start_nodes_shp)
# summary(start_nodes_shp_df$areasqkm)
# nrow(subset(start_nodes_shp_df, areasqkm < 50)) #tried 10, 25, 50 km2


#writeVector(end_nodes_shp, filename='end_nodes/end_nodes_amazon_polygons.shp', overwrite=T)
#writeVector(start_nodes_shp, filename='start_nodes/start_nodes_amazon_polygons.shp', overwrite=T)

end_nodes_shp_pts <- terra::centroids(end_nodes_shp, inside=T)
start_nodes_shp_pts <- terra::centroids(start_nodes_shp, inside=T)
#writeVector(end_nodes_shp_pts, filename='end_nodes/end_nodes_amazon_points.shp', overwrite=T)
#writeVector(start_nodes_shp_pts, filename='start_nodes/start_nodes_amazon_points.shp', overwrite=T)

# Update: Sep 2024: increase minimum PA size to 10m
# calculate elevation stats for remaining polygons >= 10km2
# Note: bosque protector has LosAmigos
bosque_protector_10km_elev_mean <- terra::extract(DEM, bosque_protector_10km, fun="mean", na.rm=T)
bosque_protector_10km_elev_median <- terra::extract(DEM, bosque_protector_10km, fun="median", na.rm=T)
bosque_protector_10km_elev_min <- terra::extract(DEM, bosque_protector_10km, fun="min", na.rm=T)
bosque_protector_10km_elev_max <- terra::extract(DEM, bosque_protector_10km, fun="max", na.rm=T)

bosque_protector_10km_elev <- cbind.data.frame(bosque_protector_10km_elev_min, bosque_protector_10km_elev_median, bosque_protector_10km_elev_max, bosque_protector_10km_elev_mean)
bosque_protector_10km_elev <- bosque_protector_10km_elev[,c(1,2,4,6,8)]
names(bosque_protector_10km_elev) <- c('rowID','min_m', 'median_m','max_m','mean_m')
bosque_protector_10km_elev$rowID <- seq(1, nrow(bosque_protector_10km_elev), 1)
bosque_protector_10km$rowID <- seq(1, nrow(bosque_protector_10km), 1)
bosque_protector_10km_elev <- merge(bosque_protector_10km, bosque_protector_10km_elev, by='rowID')
bosque_protector_10km_elev$ElevZone <- ifelse(bosque_protector_10km_elev$mean_m <= 500, "Lowland", NA)
bosque_protector_10km_elev$ElevZone <- ifelse(bosque_protector_10km_elev$mean_m >= 1500, "Highland", bosque_protector_10km_elev$ElevZone)
bosque_protector_10km_elev_df <- as.data.frame(bosque_protector_10km_elev)
table(bosque_protector_10km_elev_df$ElevZone)
#write.csv(bosque_protector_10km_elev_df, file="RAISG/Anps2023-1/Anps/bosque_protector_10km_wElev.csv", row.names=F)
#writeVector(bosque_protector_10km_elev, "RAISG/Anps2023-1/Anps/bosque_protector_10km_wElev.shp", overwrite=T)

departamental_10km_elev_mean <- terra::extract(DEM, departamental_10km, fun="mean", na.rm=T)
departamental_10km_elev_median <- terra::extract(DEM, departamental_10km, fun="median", na.rm=T)
departamental_10km_elev_min <- terra::extract(DEM, departamental_10km, fun="min", na.rm=T)
departamental_10km_elev_max <- terra::extract(DEM, departamental_10km, fun="max", na.rm=T)

departamental_10km_elev <- cbind.data.frame(departamental_10km_elev_min, departamental_10km_elev_median, departamental_10km_elev_max, departamental_10km_elev_mean)
departamental_10km_elev <- departamental_10km_elev[,c(1,2,4,6,8)]
names(departamental_10km_elev) <- c('rowID','min_m', 'median_m','max_m','mean_m')
departamental_10km_elev$rowID <- seq(1, nrow(departamental_10km_elev), 1)
departamental_10km$rowID <- seq(1, nrow(departamental_10km), 1)
departamental_10km_elev <- merge(departamental_10km, departamental_10km_elev, by='rowID')
departamental_10km_elev$ElevZone <- ifelse(departamental_10km_elev$mean_m <= 500, "Lowland", NA)
departamental_10km_elev$ElevZone <- ifelse(departamental_10km_elev$mean_m >= 1500, "Highland", departamental_10km_elev$ElevZone)
departamental_10km_elev_df <- as.data.frame(departamental_10km_elev)
table(departamental_10km_elev_df$ElevZone)
#write.csv(departamental_10km_elev_df, file="RAISG/Anps2023-1/Anps/departamental_10km_wElev.csv", row.names=F)
#writeVector(departamental_10km_elev, "RAISG/Anps2023-1/Anps/departamental_10km_wElev.shp", overwrite=T)

nacional_10km_elev_mean <- terra::extract(DEM, nacional_10km, fun="mean", na.rm=T)
nacional_10km_elev_median <- terra::extract(DEM, nacional_10km, fun="median", na.rm=T)
nacional_10km_elev_min <- terra::extract(DEM, nacional_10km, fun="min", na.rm=T)
nacional_10km_elev_max <- terra::extract(DEM, nacional_10km, fun="max", na.rm=T)

nacional_10km_elev <- cbind.data.frame(nacional_10km_elev_min, nacional_10km_elev_median, nacional_10km_elev_max, nacional_10km_elev_mean)
nacional_10km_elev <- nacional_10km_elev[,c(1,2,4,6,8)]
names(nacional_10km_elev) <- c('rowID','min_m', 'median_m','max_m','mean_m')
nacional_10km_elev$rowID <- seq(1, nrow(nacional_10km_elev), 1)
nacional_10km$rowID <- seq(1, nrow(nacional_10km), 1)
nacional_10km_elev <- merge(nacional_10km, nacional_10km_elev, by='rowID')
nacional_10km_elev$ElevZone <- ifelse(nacional_10km_elev$mean_m <= 500, "Lowland", NA)
nacional_10km_elev$ElevZone <- ifelse(nacional_10km_elev$mean_m >= 1500, "Highland", nacional_10km_elev$ElevZone)
nacional_10km_elev_df <- as.data.frame(nacional_10km_elev)
table(nacional_10km_elev_df$ElevZone)
#write.csv(nacional_10km_elev_df, file="RAISG/Anps2023-1/Anps/nacional_10km_wElev.csv", row.names=F)
#writeVector(nacional_10km_elev, "RAISG/Anps2023-1/Anps/nacional_10km_wElev.shp", overwrite=T)

reserva_florestal_10km_elev_mean <- terra::extract(DEM, reserva_florestal_10km, fun="mean", na.rm=T)
reserva_florestal_10km_elev_median <- terra::extract(DEM, reserva_florestal_10km, fun="median", na.rm=T)
reserva_florestal_10km_elev_min <- terra::extract(DEM, reserva_florestal_10km, fun="min", na.rm=T)
reserva_florestal_10km_elev_max <- terra::extract(DEM, reserva_florestal_10km, fun="max", na.rm=T)

reserva_florestal_10km_elev <- cbind.data.frame(reserva_florestal_10km_elev_min, reserva_florestal_10km_elev_median, reserva_florestal_10km_elev_max, reserva_florestal_10km_elev_mean)
reserva_florestal_10km_elev <- reserva_florestal_10km_elev[,c(1,2,4,6,8)]
names(reserva_florestal_10km_elev) <- c('rowID','min_m', 'median_m','max_m','mean_m')
reserva_florestal_10km_elev$rowID <- seq(1, nrow(reserva_florestal_10km_elev), 1)
reserva_florestal_10km$rowID <- seq(1, nrow(reserva_florestal_10km), 1)
reserva_florestal_10km_elev <- merge(reserva_florestal_10km, reserva_florestal_10km_elev, by='rowID')
reserva_florestal_10km_elev$ElevZone <- ifelse(reserva_florestal_10km_elev$mean_m <= 500, "Lowland", NA)
reserva_florestal_10km_elev$ElevZone <- ifelse(reserva_florestal_10km_elev$mean_m >= 1500, "Highland", reserva_florestal_10km_elev$ElevZone)
reserva_florestal_10km_elev_df <- as.data.frame(reserva_florestal_10km_elev)
table(reserva_florestal_10km_elev_df$ElevZone)
#write.csv(reserva_florestal_10km_elev_df, file="RAISG/Anps2023-1/Anps/reserva_florestal_10km_wElev.csv", row.names=F)
#writeVector(reserva_florestal_10km_elev, "RAISG/Anps2023-1/Anps/reserva_florestal_10km_wElev.shp", overwrite=T)

indigenous_territories_10km_elev_mean <- terra::extract(DEM, indigenous_territories_10km, fun="mean", na.rm=T)
indigenous_territories_10km_elev_median <- terra::extract(DEM, indigenous_territories_10km, fun="median", na.rm=T)
indigenous_territories_10km_elev_min <- terra::extract(DEM, indigenous_territories_10km, fun="min", na.rm=T)
indigenous_territories_10km_elev_max <- terra::extract(DEM, indigenous_territories_10km, fun="max", na.rm=T)

indigenous_territories_10km_elev <- cbind.data.frame(indigenous_territories_10km_elev_min, indigenous_territories_10km_elev_median, indigenous_territories_10km_elev_max, indigenous_territories_10km_elev_mean)
indigenous_territories_10km_elev <- indigenous_territories_10km_elev[,c(1,2,4,6,8)]
names(indigenous_territories_10km_elev) <- c('rowID','min_m', 'median_m','max_m','mean_m')
indigenous_territories_10km_elev$rowID <- seq(1, nrow(indigenous_territories_10km_elev), 1)
indigenous_territories_10km$rowID <- seq(1, nrow(indigenous_territories_10km), 1)
indigenous_territories_10km_elev <- merge(indigenous_territories_10km, indigenous_territories_10km_elev, by='rowID')
indigenous_territories_10km_elev$ElevZone <- ifelse(indigenous_territories_10km_elev$mean_m <= 500, "Lowland", NA)
indigenous_territories_10km_elev$ElevZone <- ifelse(indigenous_territories_10km_elev$mean_m >= 1500, "Highland", indigenous_territories_10km_elev$ElevZone)
indigenous_territories_10km_elev_df <- as.data.frame(indigenous_territories_10km_elev)
table(indigenous_territories_10km_elev_df$ElevZone)
#write.csv(indigenous_territories_10km_elev_df, file="RAISG/Anps2023-1/Anps/indigenous_territories_10km_wElev.csv", row.names=F)
#writeVector(indigenous_territories_10km_elev, "RAISG/Anps2023-1/Anps/indigenous_territories_10km_wElev.shp", overwrite=T)

#### Prepare nodes for connectivity analyses ####
bosque_protector_10km_elev_df <- read.csv("RAISG/Anps2023-1/Anps/bosque_protector_10km_wElev.csv")
bosque_protector_10km_elev <- terra::vect("RAISG/Anps2023-1/Anps/bosque_protector_10km_wElev.shp")

indigenous_territories_10km_elev_df <- read.csv("RAISG/Anps2023-1/Anps/indigenous_territories_10km_wElev.csv")
indigenous_territories_10km_elev <- terra::vect("RAISG/Anps2023-1/Anps/indigenous_territories_10km_wElev.shp")

reserva_florestal_10km_elev_df <- read.csv("RAISG/Anps2023-1/Anps/reserva_florestal_10km_wElev.csv")
reserva_florestal_10km_elev <- terra::vect("RAISG/Anps2023-1/Anps/reserva_florestal_10km_wElev.shp")

departamental_10km_elev_df <- read.csv("RAISG/Anps2023-1/Anps/departamental_10km_wElev.csv")
departamental_10km_elev <- terra::vect("RAISG/Anps2023-1/Anps/departamental_10km_wElev.shp")

nacional_10km_elev_df <- read.csv("RAISG/Anps2023-1/Anps/nacional_10km_wElev.csv")
nacional_10km_elev <- terra::vect("RAISG/Anps2023-1/Anps/nacional_10km_wElev.shp")

colnames_keep <- c('pais','nombre','categoria','categoriaa',
                   'areasqkm','min_m','median_m','max_m','mean_m','ElevZone')

all_nodes_df <- rbind.data.frame(bosque_protector_10km_elev_df[,colnames_keep],
                                 nacional_10km_elev_df[,colnames_keep],
                                 departamental_10km_elev_df[,colnames_keep],
                                 reserva_florestal_10km_elev_df[,colnames_keep],
                                 indigenous_territories_10km_elev_df[,colnames_keep])
all_nodes_shp <- rbind(bosque_protector_10km_elev[,colnames_keep],
                       nacional_10km_elev[,colnames_keep],
                       departamental_10km_elev[,colnames_keep],
                       reserva_florestal_10km_elev[,colnames_keep],
                       indigenous_territories_10km_elev[,colnames_keep])

end_nodes_shp <- subset(all_nodes_shp, all_nodes_shp$ElevZone=='Highland')
start_nodes_shp <- subset(all_nodes_shp, all_nodes_shp$ElevZone=='Lowland')

#writeVector(end_nodes_shp, filename='end_nodes/end_nodes_amazon_polygons10km.shp', overwrite=T)
#writeVector(start_nodes_shp, filename='start_nodes/start_nodes_amazon_polygons10km.shp', overwrite=T)

end_nodes_shp_pts <- terra::centroids(end_nodes_shp, inside=T)
start_nodes_shp_pts <- terra::centroids(start_nodes_shp, inside=T)
#writeVector(end_nodes_shp_pts, filename='end_nodes/end_nodes_amazon_points10km.shp', overwrite=T)
#writeVector(start_nodes_shp_pts, filename='start_nodes/start_nodes_amazon_points10km.shp', overwrite=T)

#### LULC statistics ####
morrone_2022_amazonclip <- terra::vect("Regions/Morrone2022/Amazon_macroregions_fixedgeom_Amazonclip.shp")
LULC <- terra::crop(LULC, amazon_study_area, mask=T)
LULC_freq <- as.data.frame(freq(LULC))
LULC_freq$pct <- (LULC_freq$count/sum(LULC_freq$count))*100
LULC_freq$Type <- c('NA','Forest','Shrubland','Grassland','Cropland','Developed','Bare','Water','Wetland','Mangrove')
#write.csv(LULC_freq[,c(2:5)], "LCC/LCC_freq_table.csv", row.names=F)

# import Zonal Histogram output from QGIS
# similar to ArcGIS Tabulate Area, but does not calculate %
zonalhist <- terra::vect("LCC/ZonalHistogram/LCC500m_ZonalHistogram_provinces.shp")
#zonalhist$areasqkm <- terra::expanse(zonalhist, "km")
zonalhist <- as.data.frame(zonalhist)
zonalhist$nCells <- rowSums(zonalhist[,c(12:21)], na.rm=T)
zonalhist <- zonalhist[,c(1,6,12:22)]

zonalhist$ZHISTO_0_pct <- (zonalhist$ZHISTO_0/zonalhist$nCells)*100
zonalhist$ZHISTO_10_pct <- (zonalhist$ZHISTO_10/zonalhist$nCells)*100
zonalhist$ZHISTO_20_pct <- (zonalhist$ZHISTO_20/zonalhist$nCells)*100
zonalhist$ZHISTO_30_pct <- (zonalhist$ZHISTO_30/zonalhist$nCells)*100
zonalhist$ZHISTO_40_pct <- (zonalhist$ZHISTO_40/zonalhist$nCells)*100
zonalhist$ZHISTO_50_pct <- (zonalhist$ZHISTO_50/zonalhist$nCells)*100
zonalhist$ZHISTO_60_pct <- (zonalhist$ZHISTO_60/zonalhist$nCells)*100
zonalhist$ZHISTO_80_pct <- (zonalhist$ZHISTO_80/zonalhist$nCells)*100
zonalhist$ZHISTO_90_pct <- (zonalhist$ZHISTO_90/zonalhist$nCells)*100
zonalhist$ZHISTO_95_pct <- (zonalhist$ZHISTO_95/zonalhist$nCells)*100

# clean up for export
nicer_LULC <- zonalhist[,c(1,2,15:23)]
colnames(nicer_LULC) <- c('Province','Dominion','Forest','Shrubland','Grassland','Cropland',
                          'Developed','Bare','Water','Wetland','Mangrove')
#rowSums(nicer_LULC[,c(3:11)], na.rm=T) # test to see if they add up to 100%; should be right on or very close
#write.csv(nicer_LULC, "LCC/LCC_freq_table_byProvince.csv", row.names=F)
