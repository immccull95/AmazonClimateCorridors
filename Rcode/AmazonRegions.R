################## Exploring Amazon regionalization ###############################
# Date: 7-30-24
# updated: 11-8-24 with new Morrone (2022) regions and subregions
# Author: Ian McCullough, immccull@gmail.com
###################################################################################

#### R libraries ####
library(terra)

#### Input data ####
setwd("C:/Users/immccull/Documents/AmazonClimateCorridors")

# Biogeographical regions
# from: https://mapress.com/zt/article/view/zootaxa.3802.2.12/36874
#morrone <- terra::vect("Regions/Morrone/Lowenberg_Neto_2014.shp")
morrone2022 <- terra::vect("Regions/Morrone2022/NeotropicMap_SIRGAS2000.shp")
morrone2022_amazonclip <- terra::vect("Regions/Morrone2022/Amazon_macroregions_fixedgeom_Amazonclip.shp")
morrone2022_amazonclip_4674 <- terra::project(morrone2022_amazonclip, "EPSG:4674")

# DEM
DEM <- terra::rast("DEM/SA_DEM_mosaic.tif")
DEM_4674 <- terra::rast("DEM/SA_DEM_mosaic_4674.tif")
DEM_4674_morrone <- terra::rast("DEM/SA_DEM_mosaic_4674_morrone.tif")

# study area
amazon_study_area <- terra::vect("RAISG/Limites2023/Limites/Lim_Biogeografico.shp")

# dissolved protected areas with macroregion (south, boreal, choacoan) as attribute
# done in QGIS (Spatial join)
# some protected area complexes have null macroregion because outside these 3 regions
dissolved_PA_10km <- terra::vect("protected_areas/Amazon_merged_PAs/Amazon_merged_PAs_dissolved10km_wRegion.shp")

#### Main program ####
# put into same CRS as elevation data
morrone_4326 <- terra::project(morrone2022, "EPSG:4326")
amazon_study_area_4326 <- terra::project(amazon_study_area, "EPSG:4326")

# identify Amazonian Morrone regions
plot(morrone_4326)
plot(amazon_study_area_4326, add=T, col='blue')

#morrone_4326_amazon <- terra::intersect(morrone_4326, amazon_study_area_4326)
#plot(morrone_4326_amazon)
#unique(morrone_4326_amazon$Province_1)

# provinces_keep <- c('Pará province','Xingu-Tapajós province','Madeira province',
#                     'Rondônia province','Ucayali province','Napo province',
#                     'Imerí province','Pantepui province','Roraima province',
#                     'Guianan Lowlands province')
provinces_keep <- c('Guianan Lowlands province','Guianan province',
  'Imeri province','Napo province','Para province',
  'Roraima province','Madeira province','Rondonia province',
  'Ucayali province','Yungas province','Xingu-Tapajos province')

morrone_4326_amazon <- subset(morrone_4326, morrone_4326$Provincias %in% provinces_keep)

plot(amazon_study_area_4326)
plot(morrone_4326_amazon, add=T, col='green')

south <- terra::vect("Regions/Morrone2022/south.shp")
boreal <- terra::vect("Regions/Morrone2022/boreal.shp")
chacoan <- terra::vect("Regions/Morrone2022/chacoan.shp")

south_dissolved <- terra::aggregate(south, dissolve=T)
boreal_dissolved <- terra::aggregate(boreal, dissolve=T)

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
morrone_amazon_elev_df$province <- morrone_4326_amazon$Provincias
#write.csv(morrone_amazon_elev_df, "Regions/Morrone/Morrone2022_regions_elev_stats.csv", row.names=F)

# dissolve macro-regions: boreal, south (chacoan is only one region anyway)
# very slow
# boreal_elev_mean <- terra::extract(DEM, boreal_dissolved, fun="mean", na.rm=T)
# boreal_elev_min <- terra::extract(DEM, boreal_dissolved, fun="min", na.rm=T)
# boreal_elev_max <- terra::extract(DEM, boreal_dissolved, fun="max", na.rm=T)
# boreal_elev_median <- terra::extract(DEM, boreal_dissolved, fun="median", na.rm=T)
# 
# boreal_df <- cbind.data.frame(boreal_elev_mean, boreal_elev_min,
#                               boreal_elev_max, boreal_elev_median)
# boreal_df <- boreal_elev_df[,c(2,4,6,8)]
# names(boreal_df) <- c('elev_mean_m','elev_min_m','elev_max_m','elev_median_m')
# #write.csv(boreal_df, "Regions/Morrone/boreal_region_elev_stats.csv", row.names=F)

south_dissolved_4326 <- terra::project(south_dissolved, "EPSG:4326")
boreal_dissolved_4326 <- terra::project(boreal_dissolved, "EPSG:4326")
chacoan_4326 <- terra::project(chacoan, "EPSG:4326")

# Crop out portions of DEM in respective regions
DEM_south <- terra::crop(DEM, south_dissolved_4326, mask=T, filename="DEM/DEM_south_region.tif", overwrite=T)
DEM_boreal <- terra::crop(DEM, boreal_dissolved_4326, mask=T, filename="DEM/DEM_boreal_region.tif", overwrite=T)
DEM_chacoan <- terra::crop(DEM, chacoan_4326, mask=T, filename="DEM/DEM_chacoan_region.tif", overwrite=T)

# Extract DEM within protected area complexes
dissolved_PA_10km_4326 <- terra::project(dissolved_PA_10km, "EPSG:4326")
#DEM_PAs <- terra::crop(DEM, dissolved_PA_10km_4326, mask=T)
#terra::project(DEM_PAs, "EPSG:4674", filename='DEM/PA_complexes_4674.tif')
DEM_PAs <- terra::rast("DEM/PA_complexes_4674.tif")
south_4674 <- terra::project(south, "EPSG:4674")
boreal_4674 <- terra::project(boreal, "EPSG:4674")
chacoan_4674 <- terra::project(chacoan, "EPSG:4674")

plot(DEM_PAs)
plot(south_4674, add=T)

DEM_PAS_south <- terra::crop(DEM_PAs, south_4674, mask=T)
plot(DEM_PAS_south)
plot(south_4674, add=T)
summary(DEM_PAS_south)
hist(DEM_PAS_south)
global(DEM_PAS_south, quantile, probs=seq(0,1,0.1), na.rm=T)

DEM_PAS_chacoan <- terra::crop(DEM_PAs, chacoan_4674, mask=T)
plot(DEM_PAS_chacoan)
plot(chacoan_4674, add=T)
summary(DEM_PAS_chacoan)
hist(DEM_PAS_chacoan)
global(DEM_PAS_chacoan, quantile, probs=seq(0,1,0.1), na.rm=T)

DEM_PAS_boreal <- terra::crop(DEM_PAs, boreal_4674, mask=T)
plot(DEM_PAS_boreal)
plot(boreal_4674, add=T)
global(DEM_PAS_boreal, quantile, probs=seq(0,1,0.1), na.rm=T)

## What are we doing for start/end nodes?
# Experimenting with Chacoan region
chacoan_quant <- as.data.frame(t(global(DEM_PAS_chacoan, quantile, probs=seq(0,1,0.1), na.rm=T)))
chacoan_quant$percentile <- seq(0,1,0.1)*100
chacoan_quant$RK <- c(seq(1,10,1),NA)
names(chacoan_quant) <- c('DEM','percentile','RK')

chacoan_mat <- c(chacoan_quant[1,1], chacoan_quant[2,1], chacoan_quant[1,3],
                 chacoan_quant[2,1], chacoan_quant[3,1], chacoan_quant[2,3],
                 chacoan_quant[3,1], chacoan_quant[4,1], chacoan_quant[3,3],
                 chacoan_quant[4,1], chacoan_quant[5,1], chacoan_quant[4,3],
                 chacoan_quant[5,1], chacoan_quant[6,1], chacoan_quant[5,3],
                 chacoan_quant[6,1], chacoan_quant[7,1], chacoan_quant[6,3],
                 chacoan_quant[7,1], chacoan_quant[8,1], chacoan_quant[7,3],
                 chacoan_quant[8,1], chacoan_quant[9,1], chacoan_quant[8,3],
                 chacoan_quant[9,1], chacoan_quant[10,1], chacoan_quant[9,3],
                 chacoan_quant[10,1], chacoan_quant[11,1], chacoan_quant[10,3])
chacoan_mat <- matrix(chacoan_mat, ncol=3, byrow=T)
chacoan_RK <- terra::classify(DEM_PAS_chacoan, chacoan_mat, include.lowest=T)
plot(chacoan_RK)
plot(chacoan_4326, add=T)
chacoan_RK_90 <- terra::ifel(chacoan_RK>=9, 1, NA)
plot(chacoan_RK_90)
plot(chacoan_4326, add=T)
plot(dissolved_PA_10km_4326, add=T)
# this is probably too much territory and too many patches
chacoan_RK_90_patches <- terra::patches(chacoan_RK_90, directions=8)
plot(chacoan_RK_90_patches)
plot(chacoan_4326, add=T)

## We could possibly use our standard thresholds for Boreal and South regions because those elevations exist there
boreal_lowland <- terra::ifel(DEM_PAS_boreal <= 500, 1, NA)
boreal_highland <- terra::ifel(DEM_PAS_boreal >= 1500, 1, NA)
plot(boreal_4674)
plot(boreal_lowland, col='gold', legend=F, add=T)
plot(boreal_highland, col='dodgerblue', add=T, legend=F)
legend(-53,11, pch=c(15,15), legend=c("Lowland (< 500m)","Highland (> 1500m)"), 
       col=c("gold","dodgerblue"), bty='n', ncol=1)
hist(DEM_PAS_boreal, main='Boreal', xlab='Elevation (m)')

hist(DEM_PAS_chacoan, main='Chacoan', xlab='Elevation (m)')


south_lowland <- terra::ifel(DEM_PAS_south <= 500, 1, NA)
south_highland <- terra::ifel(DEM_PAS_south >= 1500, 1, NA)
plot(south_4674)
#plot(dissolved_PA_10km, col='gray', add=T)
plot(south_lowland, col='gold', add=T, legend=F)
plot(south_highland, col='dodgerblue', add=T, legend=F)
legend(-80,-20, pch=c(15,15), legend=c("Lowland (< 500m)","Highland (> 1500m)"), 
       col=c("gold","dodgerblue"), bty='n', ncol=1, cex=0.75)
hist(DEM_PAS_south, main='South', xlab='Elevation (m)')

#south_highland_polygons <- terra::as.polygons(south_highland) #took maybe a minute, but returned one large feature. Making aggregate=F threw error that raster is too large
#writeRaster(south_highland, filename='DEM/south_highland.tif', overwrite=T)

chacoan_over500m <- terra::ifel(DEM_PAS_chacoan >= 500, 1, NA)
chacoan_over600m <- terra::ifel(DEM_PAS_chacoan >= 600, 1, NA)
chacoan_over700m <- terra::ifel(DEM_PAS_chacoan >= 700, 1, NA)
chacoan_over800m <- terra::ifel(DEM_PAS_chacoan >= 800, 1, NA)
plot(chacoan_4674, lwd=2)
plot(dissolved_PA_10km, add=T, col='gray')
plot(chacoan_4674, lwd=2, add=T)
plot(chacoan_over500m, col='forestgreen', add=T, legend=F)
plot(chacoan_over600m, col='orange', add=T, legend=F)
plot(chacoan_over700m, col='purple', add=T, legend=F)
plot(chacoan_over800m, col='blue', add=T, legend=F)

## Sep 2024 update: repeat for newer Morrone ecoregions
# Nov 2024 update: add Yungas province
# https://www.scielo.br/j/aabc/a/hPft4CK6RV8QBr8nP7bxhRQ/# 

provinces_keep_2022 <- c('Guianan Lowlands province','Roraima province','Guianan province',
                         'Imeri province','Xingu-Tapajos province','Madeira province', 'Yungas province',
                         'Rondonia province','Ucayali province','Para province','Napo province')

#morrone2022_4326 <- terra::project(morrone2022, "EPSG:4326")
#morrone2022_4326_amazon <- subset(morrone2022_4326, morrone2022_4326$Provincias %in% provinces_keep_2022)
# 
# plot(amazon_study_area_4326)
# plot(morrone2022_4326_amazon, add=T, col='forestgreen')
# 
morrone2022_4674_amazon <- terra::project(morrone2022_4326_amazon, "EPSG:4674")
# #writeVector(morrone2022_4674_amazon, filename='Regions/Morrone2022/Morrone2022_amazon_4674.shp')
# 
# Calculate elevational gradients within each region
# warning: can take hours
morrone2022_4674_amazon_elev_mean <- terra::extract(DEM_4674, morrone2022_amazonclip_4674, fun="mean", na.rm=T)
morrone2022_4674_amazon_elev_min <- terra::extract(DEM_4674, morrone2022_amazonclip_4674, fun="min", na.rm=T)
morrone2022_4674_amazon_elev_max <- terra::extract(DEM_4674, morrone2022_amazonclip_4674, fun="max", na.rm=T)
morrone2022_4674_amazon_elev_median <- terra::extract(DEM_4674, morrone2022_amazonclip_4674, fun="median", na.rm=T)

morrone_amazon_elev_df <- cbind.data.frame(morrone2022_4674_amazon_elev_mean, morrone2022_4674_amazon_elev_min,
                                           morrone2022_4674_amazon_elev_max, morrone2022_4674_amazon_elev_median)
morrone_amazon_elev_df <- morrone_amazon_elev_df[,c(2,4,6,8)]
names(morrone_amazon_elev_df) <- c('elev_mean_m','elev_min_m','elev_max_m','elev_median_m')
morrone_amazon_elev_df$province <- morrone2022_amazonclip_4674$Provincias
morrone_amazon_elev_df$elev_range_m <- morrone_amazon_elev_df$elev_max_m - morrone_amazon_elev_df$elev_min_m
#write.csv(morrone_amazon_elev_df, "Regions/Morrone2022/Morrone2022_regions_elev_stats.csv", row.names=F)
