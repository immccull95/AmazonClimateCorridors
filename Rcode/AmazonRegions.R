################## Exploring Amazon regionalization ###############################
# Date: 7-30-24
# updated: 10-9-24 with new Morrone (2022) regions
# Author: Ian McCullough, immccull@gmail.com
###################################################################################

#### R libraries ####
library(terra)

#### Input data ####
setwd("C:/Users/immccull/Documents/AmazonClimateCorridors")

# Biogeographical regions
# from: https://mapress.com/zt/article/view/zootaxa.3802.2.12/36874
morrone <- terra::vect("Regions/Morrone/Lowenberg_Neto_2014.shp")
morrone2022 <- terra::vect("Regions/Morrone2022/NeotropicMap_SIRGAS2000.shp")

# DEM
DEM <- terra::rast("DEM/SA_DEM_mosaic.tif")

# study area
amazon_study_area <- terra::vect("RAISG/Limites2023/Limites/Lim_Biogeografico.shp")

#### Main program ####
# put into same CRS as elevation data
morrone_4326 <- terra::project(morrone, "EPSG:4326")
amazon_study_area_4326 <- terra::project(amazon_study_area, "EPSG:4326")

# identify Amazonian Morrone regions
plot(morrone_4326)
plot(amazon_study_area_4326, add=T, col='blue')

#morrone_4326_amazon <- terra::intersect(morrone_4326, amazon_study_area_4326)
#plot(morrone_4326_amazon)
#unique(morrone_4326_amazon$Province_1)

provinces_keep <- c('Pará province','Xingu-Tapajós province','Madeira province',
                    'Rondônia province','Ucayali province','Napo province',
                    'Imerí province','Pantepui province','Roraima province',
                    'Guianan Lowlands province')
morrone_4326_amazon <- subset(morrone_4326, morrone_4326$Province_1 %in% provinces_keep)

plot(amazon_study_area_4326)
plot(morrone_4326_amazon, add=T, col='green')

## Calculate elevational gradients within each region
# warning: can take hours
morrone_4326_amazon_elev_mean <- terra::extract(DEM, morrone_4326_amazon, fun="mean", na.rm=T)
morrone_4326_amazon_elev_min <- terra::extract(DEM, morrone_4326_amazon, fun="min", na.rm=T)
morrone_4326_amazon_elev_max <- terra::extract(DEM, morrone_4326_amazon, fun="max", na.rm=T)
morrone_4326_amazon_elev_median <- terra::extract(DEM, morrone_4326_amazon, fun="median", na.rm=T)

morrone_amazon_elev_df <- cbind.data.frame(morrone_4326_amazon_elev_mean, morrone_4326_amazon_elev_min,
                                           morrone_4326_amazon_elev_max, morrone_4326_amazon_elev_median)
morrone_amazon_elev_df <- morrone_amazon_elev_df[,c(2,4,6,8)]
names(morrone_amazon_elev_df) <- c('elev_mean_m','elev_min_m','elev_max_m','elev_median_m')
morrone_amazon_elev_df$province <- morrone_4326_amazon$Province_1
write.csv(morrone_amazon_elev_df, "Regions/Morrone/Morrone_regions_elev_stats.csv", row.names=F)

## Sep 2024 update: repeat for newer Morrone ecoregions
# https://www.scielo.br/j/aabc/a/hPft4CK6RV8QBr8nP7bxhRQ/# 

provinces_keep_2022 <- c('Guianan Lowlands province','Roraima province','Guianan province',
                         'Imeri province','Xingu-Tapajos province','Madeira province',
                         'Rondonia province','Ucayali province','Para province','Napo province')

morrone2022_4326 <- terra::project(morrone2022, "EPSG:4326")
morrone2022_4326_amazon <- subset(morrone2022_4326, morrone2022_4326$Provincias %in% provinces_keep_2022)

plot(amazon_study_area_4326)
plot(morrone2022_4326_amazon, add=T, col='forestgreen')

morrone2022_4674_amazon <- terra::project(morrone2022_4326_amazon, "EPSG:4674")
#writeVector(morrone2022_4674_amazon, filename='Regions/Morrone2022/Morrone2022_amazon_4674.shp')

# Calculate elevational gradients within each region
# warning: can take hours
morrone2022_4326_amazon_elev_mean <- terra::extract(DEM, morrone2022_4326_amazon, fun="mean", na.rm=T)
morrone2022_4326_amazon_elev_min <- terra::extract(DEM, morrone2022_4326_amazon, fun="min", na.rm=T)
morrone2022_4326_amazon_elev_max <- terra::extract(DEM, morrone2022_4326_amazon, fun="max", na.rm=T)
morrone2022_4326_amazon_elev_median <- terra::extract(DEM, morrone2022_4326_amazon, fun="median", na.rm=T)

morrone_amazon_elev_df <- cbind.data.frame(morrone2022_4326_amazon_elev_mean, morrone2022_4326_amazon_elev_min,
                                           morrone2022_4326_amazon_elev_max, morrone2022_4326_amazon_elev_median)
morrone_amazon_elev_df <- morrone_amazon_elev_df[,c(2,4,6,8)]
names(morrone_amazon_elev_df) <- c('elev_mean_m','elev_min_m','elev_max_m','elev_median_m')
morrone_amazon_elev_df$province <- morrone2022_4326_amazon$Provincias
#write.csv(morrone_amazon_elev_df, "Regions/Morrone2022/Morrone2022_regions_elev_stats.csv", row.names=F)
