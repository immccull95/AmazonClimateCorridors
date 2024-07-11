### Assembling data layers for Amazon climate corridor 
# Date: 5-30-24
# updated: 6-3-24 (add Los Amigos)

#### R libraries ####
library(terra)
library(dplyr)

#### Input data ####
setwd("C:/Users/immccull/Documents/AmazonClimateCorridors")

southamerica <- terra::vect("RAISG/SA_Countries/SouthAmerica.shp")

amazon_study_area <- terra::vect("RAISG/Limites2023/Limites/Lim_Biogeografico.shp")

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

# calculate elevation stats for remaining polygons >= 5km2
# Note: bosque protector has LosAmigos
bosque_protector_5km_elev_mean <- terra::extract(DEM, bosque_protector_5km, fun="mean", na.rm=T)
bosque_protector_5km_elev_median <- terra::extract(DEM, bosque_protector_5km, fun="median", na.rm=T)
bosque_protector_5km_elev_min <- terra::extract(DEM, bosque_protector_5km, fun="min", na.rm=T)
bosque_protector_5km_elev_max <- terra::extract(DEM, bosque_protector_5km, fun="max", na.rm=T)

bosque_protector_5km_elev <- cbind.data.frame(bosque_protector_5km_elev_min, bosque_protector_5km_elev_median, bosque_protector_5km_elev_max, bosque_protector_5km_elev_mean)
bosque_protector_5km_elev <- bosque_protector_5km_elev[,c(1,2,4,6,8)]
names(bosque_protector_5km_elev) <- c('rowID','min_m', 'median_m','max_m','mean_m')
bosque_protector_5km_elev$rowID <- seq(1, nrow(bosque_protector_5km_elev), 1)
bosque_protector_5km$rowID <- seq(1, nrow(bosque_protector_5km), 1)
bosque_protector_5km_elev <- merge(bosque_protector_5km, bosque_protector_5km_elev, by='rowID')
bosque_protector_5km_elev$ElevZone <- ifelse(bosque_protector_5km_elev$mean_m <= 500, "Lowland", NA)
bosque_protector_5km_elev$ElevZone <- ifelse(bosque_protector_5km_elev$mean_m >= 1500, "Highland", bosque_protector_5km_elev$ElevZone)
bosque_protector_5km_elev_df <- as.data.frame(bosque_protector_5km_elev)
table(bosque_protector_5km_elev_df$ElevZone)
#write.csv(bosque_protector_5km_elev_df, file="RAISG/Anps2023-1/Anps/bosque_protector_5km_wElev.csv", row.names=F)
#writeVector(bosque_protector_5km_elev, "RAISG/Anps2023-1/Anps/bosque_protector_5km_wElev.shp", overwrite=T)

departamental_5km_elev_mean <- terra::extract(DEM, departamental_5km, fun="mean", na.rm=T)
departamental_5km_elev_median <- terra::extract(DEM, departamental_5km, fun="median", na.rm=T)
departamental_5km_elev_min <- terra::extract(DEM, departamental_5km, fun="min", na.rm=T)
departamental_5km_elev_max <- terra::extract(DEM, departamental_5km, fun="max", na.rm=T)

departamental_5km_elev <- cbind.data.frame(departamental_5km_elev_min, departamental_5km_elev_median, departamental_5km_elev_max, departamental_5km_elev_mean)
departamental_5km_elev <- departamental_5km_elev[,c(1,2,4,6,8)]
names(departamental_5km_elev) <- c('rowID','min_m', 'median_m','max_m','mean_m')
departamental_5km_elev$rowID <- seq(1, nrow(departamental_5km_elev), 1)
departamental_5km$rowID <- seq(1, nrow(departamental_5km), 1)
departamental_5km_elev <- merge(departamental_5km, departamental_5km_elev, by='rowID')
departamental_5km_elev$ElevZone <- ifelse(departamental_5km_elev$mean_m <= 500, "Lowland", NA)
departamental_5km_elev$ElevZone <- ifelse(departamental_5km_elev$mean_m >= 1500, "Highland", departamental_5km_elev$ElevZone)
departamental_5km_elev_df <- as.data.frame(departamental_5km_elev)
table(departamental_5km_elev_df$ElevZone)
#write.csv(departamental_5km_elev_df, file="RAISG/Anps2023-1/Anps/departamental_5km_wElev.csv", row.names=F)
#writeVector(departamental_5km_elev, "RAISG/Anps2023-1/Anps/departamental_5km_wElev.shp", overwrite=T)

nacional_5km_elev_mean <- terra::extract(DEM, nacional_5km, fun="mean", na.rm=T)
nacional_5km_elev_median <- terra::extract(DEM, nacional_5km, fun="median", na.rm=T)
nacional_5km_elev_min <- terra::extract(DEM, nacional_5km, fun="min", na.rm=T)
nacional_5km_elev_max <- terra::extract(DEM, nacional_5km, fun="max", na.rm=T)

nacional_5km_elev <- cbind.data.frame(nacional_5km_elev_min, nacional_5km_elev_median, nacional_5km_elev_max, nacional_5km_elev_mean)
nacional_5km_elev <- nacional_5km_elev[,c(1,2,4,6,8)]
names(nacional_5km_elev) <- c('rowID','min_m', 'median_m','max_m','mean_m')
nacional_5km_elev$rowID <- seq(1, nrow(nacional_5km_elev), 1)
nacional_5km$rowID <- seq(1, nrow(nacional_5km), 1)
nacional_5km_elev <- merge(nacional_5km, nacional_5km_elev, by='rowID')
nacional_5km_elev$ElevZone <- ifelse(nacional_5km_elev$mean_m <= 500, "Lowland", NA)
nacional_5km_elev$ElevZone <- ifelse(nacional_5km_elev$mean_m >= 1500, "Highland", nacional_5km_elev$ElevZone)
nacional_5km_elev_df <- as.data.frame(nacional_5km_elev)
table(nacional_5km_elev_df$ElevZone)
#write.csv(nacional_5km_elev_df, file="RAISG/Anps2023-1/Anps/nacional_5km_wElev.csv", row.names=F)
#writeVector(nacional_5km_elev, "RAISG/Anps2023-1/Anps/nacional_5km_wElev.shp", overwrite=T)

reserva_florestal_5km_elev_mean <- terra::extract(DEM, reserva_florestal_5km, fun="mean", na.rm=T)
reserva_florestal_5km_elev_median <- terra::extract(DEM, reserva_florestal_5km, fun="median", na.rm=T)
reserva_florestal_5km_elev_min <- terra::extract(DEM, reserva_florestal_5km, fun="min", na.rm=T)
reserva_florestal_5km_elev_max <- terra::extract(DEM, reserva_florestal_5km, fun="max", na.rm=T)

reserva_florestal_5km_elev <- cbind.data.frame(reserva_florestal_5km_elev_min, reserva_florestal_5km_elev_median, reserva_florestal_5km_elev_max, reserva_florestal_5km_elev_mean)
reserva_florestal_5km_elev <- reserva_florestal_5km_elev[,c(1,2,4,6,8)]
names(reserva_florestal_5km_elev) <- c('rowID','min_m', 'median_m','max_m','mean_m')
reserva_florestal_5km_elev$rowID <- seq(1, nrow(reserva_florestal_5km_elev), 1)
reserva_florestal_5km$rowID <- seq(1, nrow(reserva_florestal_5km), 1)
reserva_florestal_5km_elev <- merge(reserva_florestal_5km, reserva_florestal_5km_elev, by='rowID')
reserva_florestal_5km_elev$ElevZone <- ifelse(reserva_florestal_5km_elev$mean_m <= 500, "Lowland", NA)
reserva_florestal_5km_elev$ElevZone <- ifelse(reserva_florestal_5km_elev$mean_m >= 1500, "Highland", reserva_florestal_5km_elev$ElevZone)
reserva_florestal_5km_elev_df <- as.data.frame(reserva_florestal_5km_elev)
table(reserva_florestal_5km_elev_df$ElevZone)
#write.csv(reserva_florestal_5km_elev_df, file="RAISG/Anps2023-1/Anps/reserva_florestal_5km_wElev.csv", row.names=F)
#writeVector(reserva_florestal_5km_elev, "RAISG/Anps2023-1/Anps/reserva_florestal_5km_wElev.shp", overwrite=T)

indigenous_territories_5km_elev_mean <- terra::extract(DEM, indigenous_territories_5km, fun="mean", na.rm=T)
indigenous_territories_5km_elev_median <- terra::extract(DEM, indigenous_territories_5km, fun="median", na.rm=T)
indigenous_territories_5km_elev_min <- terra::extract(DEM, indigenous_territories_5km, fun="min", na.rm=T)
indigenous_territories_5km_elev_max <- terra::extract(DEM, indigenous_territories_5km, fun="max", na.rm=T)

indigenous_territories_5km_elev <- cbind.data.frame(indigenous_territories_5km_elev_min, indigenous_territories_5km_elev_median, indigenous_territories_5km_elev_max, indigenous_territories_5km_elev_mean)
indigenous_territories_5km_elev <- indigenous_territories_5km_elev[,c(1,2,4,6,8)]
names(indigenous_territories_5km_elev) <- c('rowID','min_m', 'median_m','max_m','mean_m')
indigenous_territories_5km_elev$rowID <- seq(1, nrow(indigenous_territories_5km_elev), 1)
indigenous_territories_5km$rowID <- seq(1, nrow(indigenous_territories_5km), 1)
indigenous_territories_5km_elev <- merge(indigenous_territories_5km, indigenous_territories_5km_elev, by='rowID')
indigenous_territories_5km_elev$ElevZone <- ifelse(indigenous_territories_5km_elev$mean_m <= 500, "Lowland", NA)
indigenous_territories_5km_elev$ElevZone <- ifelse(indigenous_territories_5km_elev$mean_m >= 1500, "Highland", indigenous_territories_5km_elev$ElevZone)
indigenous_territories_5km_elev_df <- as.data.frame(indigenous_territories_5km_elev)
table(indigenous_territories_5km_elev_df$ElevZone)
#write.csv(indigenous_territories_5km_elev_df, file="RAISG/Anps2023-1/Anps/indigenous_territories_5km_wElev.csv", row.names=F)
#writeVector(indigenous_territories_5km_elev, "RAISG/Anps2023-1/Anps/indigenous_territories_5km_wElev.shp", overwrite=T)

#### Prepare nodes for connectivity analyses ####
bosque_protector_5km_elev_df <- read.csv("RAISG/Anps2023-1/Anps/bosque_protector_5km_wElev.csv")
bosque_protector_5km_elev <- terra::vect("RAISG/Anps2023-1/Anps/bosque_protector_5km_wElev.shp")

indigenous_territories_5km_elev_df <- read.csv("RAISG/Anps2023-1/Anps/indigenous_territories_5km_wElev.csv")
indigenous_territories_5km_elev <- terra::vect("RAISG/Anps2023-1/Anps/indigenous_territories_5km_wElev.shp")

reserva_florestal_5km_elev_df <- read.csv("RAISG/Anps2023-1/Anps/reserva_florestal_5km_wElev.csv")
reserva_florestal_5km_elev <- terra::vect("RAISG/Anps2023-1/Anps/reserva_florestal_5km_wElev.shp")

departamental_5km_elev_df <- read.csv("RAISG/Anps2023-1/Anps/departamental_5km_wElev.csv")
departamental_5km_elev <- terra::vect("RAISG/Anps2023-1/Anps/departamental_5km_wElev.shp")

nacional_5km_elev_df <- read.csv("RAISG/Anps2023-1/Anps/nacional_5km_wElev.csv")
nacional_5km_elev <- terra::vect("RAISG/Anps2023-1/Anps/nacional_5km_wElev.shp")

colnames_keep <- c('pais','nombre','categoria','categoriaa',
                   'areasqkm','min_m','median_m','max_m','mean_m','ElevZone')

all_nodes_df <- rbind.data.frame(bosque_protector_5km_elev_df[,colnames_keep],
                                 nacional_5km_elev_df[,colnames_keep],
                                 departamental_5km_elev_df[,colnames_keep],
                                 reserva_florestal_5km_elev_df[,colnames_keep],
                                 indigenous_territories_5km_elev_df[,colnames_keep])
all_nodes_shp <- rbind(bosque_protector_5km_elev[,colnames_keep],
                       nacional_5km_elev[,colnames_keep],
                       departamental_5km_elev[,colnames_keep],
                       reserva_florestal_5km_elev[,colnames_keep],
                       indigenous_territories_5km_elev[,colnames_keep])

end_nodes_shp <- subset(all_nodes_shp, all_nodes_shp$ElevZone=='Highland')
start_nodes_shp <- subset(all_nodes_shp, all_nodes_shp$ElevZone=='Lowland')
#writeVector(end_nodes_shp, filename='end_nodes/end_nodes_amazon_polygons.shp', overwrite=T)
#writeVector(start_nodes_shp, filename='start_nodes/start_nodes_amazon_polygons.shp', overwrite=T)

end_nodes_shp_pts <- terra::centroids(end_nodes_shp, inside=T)
start_nodes_shp_pts <- terra::centroids(start_nodes_shp, inside=T)
#writeVector(end_nodes_shp_pts, filename='end_nodes/end_nodes_amazon_points.shp', overwrite=T)
#writeVector(start_nodes_shp_pts, filename='start_nodes/start_nodes_amazon_points.shp', overwrite=T)
