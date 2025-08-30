################# Amazon least cost path ecological characteristics ###############
# Date: 11-14-24
# updated: 8-29-25; new least cost paths
# Author: Ian McCullough, immccull@gmail.com
###################################################################################

#### R libraries ####
library(terra)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(reshape2)
library(factoextra)
library(scales)

#### Input data ####
setwd("C:/Users/immccull/Documents/AmazonClimateCorridors")
# buffered (1km) least cost paths
#LCP_1km <- terra::vect("LeastCostPaths/CombinedLeastCostPaths/CombinedLeastCostPaths_amazon_1kmbuff.shp")
LCP_1km <- terra::vect("LeastCostPaths/CombinedLeastCostPaths/CombinedLeastCostPaths_amazon_2K_1kmbuff.shp")
#test <- terra::vect("LeastCostPaths/CombinedLeastCostPaths/CombinedLeastCostPaths_amazon_wRegions.shp")

# regions
morrone_2022_neotropics <- terra::vect("Regions/Morrone2022/NeotropicMap_SIRGAS2000.shp")
morrone_2022_neotropics <- terra::project(morrone_2022_neotropics, "EPSG:29172")
morrone_2022_amazonclip <- terra::vect("Regions/Morrone2022/Amazon_macroregions_fixedgeom_Amazonclip.shp")
#plot(morrone_2022_amazonclip)

# Amazon study area
amazon_study_area <- terra::vect("RAISG/Limites2023/Limites/Lim_Biogeografico.shp")
amazon_study_area <- terra::project(amazon_study_area, "EPSG:29172")
#plot(amazon_study_area)

# LULC
LULC <- terra::rast("LCC/LCC_amazon_mosaic_500m.tif")

# forest only
forest <- terra::rast("LCC/LCC_500m_forest.tif")

# Protected areas
protected_areas <- terra::vect("protected_areas/Amazon_merged_PAs/Amazon_merged_PAs_dissolved10km_wRegion.shp")
protected_areas <- terra::project(protected_areas, "EPSG:29172")

# Roads
roads <- terra::vect("RAISG/vias2023-1/vias/buffered/roads_buffer_500m_union.shp")

# canopy
canopy <- terra::rast("GCH/GCH_amazon_29172_500m.tif")

# Terrain
DEM <- terra::rast("DEM/buffered100km/DEM_amazon_100kmbuff.tif")


#### Main program ####
# calculate forest patches
#forest_patches <- terra::patches(forest, directions=8, allowGaps=F,
#                                 filename="LCC/forest_patches_500m.tif")
#forest_patches <- terra::rast("LCC/forest_patches_500m.tif")
#forest_patches_polygons <- terra::as.polygons(forest_patches, aggregate=T, values=T, na.rm=T)
#terra::writeVector(forest_patches_polygons, filename="LCC/forest_patches_500m_polygons.shp", overwrite=T)
forest_patches <- terra::vect("LCC/forest_patches_500m_polygons.shp")

## Assign LCP ID
LCP_1km$LCP_ID <- seq(1,nrow(LCP_1km),1)

## Canopy height
LCP_canopy_mean <- terra::extract(canopy, LCP_1km, fun='mean', na.rm=T)
LCP_canopy_min <- terra::extract(canopy, LCP_1km, fun='min', na.rm=T)
LCP_canopy_max <- terra::extract(canopy, LCP_1km, fun='max', na.rm=T)
LCP_canopy <- cbind.data.frame(LCP_canopy_mean[,2], LCP_canopy_min[,2], LCP_canopy_max[,2])
colnames(LCP_canopy) <- c('canopy_mean','canopy_min','canopy_max')
LCP_canopy$canopy_range <- LCP_canopy$canopy_max - LCP_canopy$canopy_min
LCP_canopy$Origin <- LCP_1km$start_PAco
LCP_canopy$Cost <- LCP_1km$cost
LCP_canopy$LCP_ID <- LCP_1km$LCP_ID

summary(LCP_canopy)
hist(LCP_canopy$canopy_mean, main='Mean forest canopy height', xlab='m')
#write.csv(LCP_canopy, "LeastCostPaths/LCP_characteristics/LCP_canopy.csv", row.names=F)
#write.csv(LCP_canopy, "LeastCostPaths/LCP_characteristics/LCP_canopy_2K.csv", row.names=F)

## Terrain
LCP_elevation_mean <- terra::extract(DEM, LCP_1km, fun='mean', na.rm=T)
LCP_elevation_min <- terra::extract(DEM, LCP_1km, fun='min', na.rm=T)
LCP_elevation_max <- terra::extract(DEM, LCP_1km, fun='max', na.rm=T)
LCP_elevation <- cbind.data.frame(LCP_elevation_mean[,2], LCP_elevation_min[,2], LCP_elevation_max[,2])
colnames(LCP_elevation) <- c('elevation_mean','elevation_min','elevation_max')
LCP_elevation$elevation_range <- LCP_elevation$elevation_max - LCP_elevation$elevation_min
LCP_elevation$LCP_ID <- LCP_1km$LCP_ID
summary(LCP_elevation)
hist(LCP_elevation$elevation_mean, main='LCP mean elevation', xlab='Elevation (m)')
hist(LCP_elevation$elevation_range, main='LCP elevation range', xlab='Elevation (m)')
#write.csv(LCP_elevation, "LeastCostPaths/LCP_characteristics/LCP_elevation.csv", row.names=F)
#write.csv(LCP_elevation, "LeastCostPaths/LCP_characteristics/LCP_elevation_2K.csv", row.names=F)


## LCP protection
LCP_area <- terra::expanse(LCP_1km, unit='km')

# code below does not work (too much data)
# used QGIS overlap analysis instead
# LCP_protection <- terra::intersect(LCP_1km, protected_areas)
# LCP_protection_df <- as.data.frame(LCP_protection)
# 
# plot(amazon_study_area)
# plot(LCP_protection, add=T, col='red')
# 
# # Number of PAs overlapping with each LCP
# LCP_protection_count <- LCP_protection_df[,c('LCP_ID','cost')] %>% #was throwing error that some values were duplicated, but these were columns we don't care about for this particular calculation anyway
#   dplyr::group_by(LCP_ID) %>%
#   dplyr::summarize(nPA=n()) %>%
#   as.data.frame()
# summary(LCP_protection_count)
# hist(LCP_protection_count$nPA, main='Number of protected areas', 
#      xlab='Protected areas', breaks=seq(0,9,1), xlim=c(0,10))
# 
# # need to deal with overlapping LCPs within PAs
# protected_areas_dissolved <- terra::aggregate(protected_areas)
# #terra::writeVector(protected_areas_dissolved, filename='Amazon_merged_PAs/protected_areas/Amazon_PAcomplexes10km_dissolved.shp')
# 
# plot(amazon_study_area)
# plot(protected_areas, add=T, col='dodgerblue')
# plot(protected_areas_dissolved, add=T, col='red')
# 
# LCP_protection_dissolved <- terra::intersect(LCP_1km, protected_areas_dissolved)
# LCP_protection_dissolved_area <- terra::expanse(LCP_protection_dissolved, unit='km')
# LCP_protection_dissolved_df <- as.data.frame(LCP_protection_dissolved)
# LCP_protection_dissolved_df$PAareasqkm <- LCP_protection_dissolved_area
# 
# plot(amazon_study_area)
# plot(protected_areas_dissolved, add=T, col='forestgreen')
# plot(LCP_1km, add=T, col='turquoise')
# plot(LCP_protection_dissolved, add=T, col='gold')
# 
# # % protection per LCP
# LCP_area <- terra::expanse(LCP_1km, unit='km')
# 
# LCP_protection_dissolved_df$LCP_areasqkm <- LCP_area
# LCP_protection_dissolved_df$pct_protected <- (LCP_protection_dissolved_df$PAareasqkm/LCP_protection_dissolved_df$LCP_areasqkm)*100
# summary(LCP_protection_dissolved_df)
# hist(LCP_protection_dissolved_df$pct_protected, main='LCP protection', xlab='Percentage')

## 11-18-24 update
# tried looping through each LCP individually, but crashed at terra::intersect for large datasets
# need start polygons to get PAcomplexID and start node province
combined_start_polygons <- terra::vect("start_nodes/dominions/start_polygons/combined_start_polygons/combined_start_polygons.shp")
combined_start_polygons_df <- as.data.frame(combined_start_polygons)[,c(2:4)]
names(combined_start_polygons_df) <- c('startDominion','startProvince','PAcomplexID')

#combined_end_polygons <- terra::vect("end_nodes/dominions/end_polygons/combined_end_polygons/combined_end_polygons.shp")
combined_end_polygons <- terra::vect("end_nodes/dominions/end_polygons/combined_end_polygons/combined_end_polygons_2K.shp")
combined_end_polygons_df <- as.data.frame(combined_end_polygons)#[,c(2:4)]
#names(combined_end_polygons_df) <- c('endDominion','endProvince','PAcomplexID')

#all_PAs <- terra::vect("protected_areas/Amazon_merged_PAs/Amazon_merged_PAs_dissolved.shp")
#all_PAs <- terra::project(all_PAs, "EPSG:29172")
#protection_list <- list()

#overlap <- terra::vect("LeastCostPaths/LCP_characteristics/LCP_protection_OverlapAnalysis.shp")
overlap <- terra::vect("LeastCostPaths/LCP_characteristics/LCP_protection_OverlapAnalysis_2K.shp")
overlap_df <- as.data.frame(overlap)

protection_df <- merge(overlap_df, combined_start_polygons_df, by.x='start_PAco', by.y='PAcomplexID')
#protection_df <- merge(protection_df, combined_end_polygons_df, by.x='end_PAcomp', by.y='PAcomplexID')
#protection_df <- protection_df[,c(2,1, 6:11,13:17)]
#names(protection_df) <- c('startPAcomplexID','endPAcomplexID','cost',
#                          'start_lowland_areasqkm','end_highland_areasqkm',
#                          'lengthkm','mainProvince','mainDominion','pct_protection',
#                          'startDominion','startProvince','endDominion','endProvince')
protection_df <- protection_df[,c(1,5,6,7,8,9,11,12,13)]
names(protection_df) <- c('startPAcomplexID','cost','start_lowland_areasqkm','endID',
                          'lengthkm','end_protec','pct_protection','startDominion',
                          'startProvince')

#write.csv(protection_df, "LeastCostPaths/LCP_characteristics/LCP_protection.csv", row.names=F)
write.csv(protection_df, "LeastCostPaths/LCP_characteristics/LCP_protection_2K.csv", row.names=F)


## does not work; terra::intersect keeps crashing with large datasets
# for (i in 1:nrow(LCP_1km)){
# #for (i in 1:10){ #testing loop before running whole thing
#   test_LCP <- LCP_1km[i]
#   test_LCP_PAs <- terra::intersect(test_LCP, all_PAs)
#   test_LCP_area <- terra::expanse(test_LCP, "km")
#   test_LCP_PAs_area <- sum(terra::expanse(test_LCP_PAs, "km"))
#   test_LCP_PA_pct <- (test_LCP_PAs_area/test_LCP_area)*100
#   test_LCP <- merge(test_LCP, combined_start_polygons_df, by.x='start_PAco', by.y='PAcomplexI', all=F)
#   test_LCP <- merge(test_LCP, combined_end_polygons_df, by.x='end_PAcomp', by.y='PAcomplexI', all=F)
#   output_df <- data.frame(LCP_ID = test_LCP$LCP_ID,
#                           startPAcomplexID = test_LCP$start_PAco,
#                           startareasqkm = test_LCP$lowland_ar,
#                           endPAcomplexID = test_LCP$end_PAcomp,
#                           endareasqkm = test_LCP$highland_a,
#                           lengthkm = test_LCP$lengthkm,
#                           startProvince = test_LCP$province.x,
#                           majorityProvince = test_LCP$Provincias,
#                           endProvince = test_LCP$province.y,
#                           startDominion = test_LCP$dominion.x,
#                           endDominion = test_LCP$dominion.y,
#                           cost = test_LCP$cost)
#   protection_list[[i]] <- output_df
#   output_df = NA
#   test_LCP = NA
#   test_LCP_area = NA
#   test_LCP_PA_pct = NA
#   test_LCP_PAs = NA
# }
# protection_df <- do.call(rbind.data.frame, protection_list)

## Percent forest 
# Note, this may be an overestimate at 500 m resolution (would explain why some LCPs were calculated as high as 110% forest)
# Ideally, we would use the 10 m data, but that would take forever to process
forest <- terra::rast("LCC/LCC_500m_forest.tif")

LCP_forest <- terra::extract(forest, LCP_1km, fun='table', na.rm=T)
colnames(LCP_forest) <- c('ID','ForestCells')
LCP_forest$LCP_areasqkm <- LCP_area 
LCP_forest$Forest_areasqkm <- LCP_forest$ForestCells*250000/1e6
LCP_forest$pct_forest <- (LCP_forest$Forest_areasqkm/LCP_forest$LCP_areasqkm)*100
LCP_forest$pct_forest <- ifelse(LCP_forest$pct_forest >=100, 100, LCP_forest$pct_forest) #make anything over 100% as 100%
LCP_forest$LCP_ID <- LCP_1km$LCP_ID
hist(LCP_forest$pct_forest)
#write.csv(LCP_forest, "LeastCostPaths/LCP_characteristics/LCP_pct_forest.csv", row.names=F)
#write.csv(LCP_forest, "LeastCostPaths/LCP_characteristics/LCP_pct_forest_2K.csv", row.names=F)


## Forest patches crossed
forest_patches$patch_areasqkm <- terra::expanse(forest_patches, unit='km')
LCP_forest_patches <- terra::intersect(LCP_1km, forest_patches)
LCP_forest_patches_df <- as.data.frame(LCP_forest_patches)
#LCP_forest_patches_df$patch_area <- terra::expanse(LCP_forest_patches, unit='km') #would just give intersecting area, not whole patch area
LCP_forest_patches_summary <- LCP_forest_patches_df[,c('LCP_ID','start_PAco','patch_areasqkm')] %>%
  dplyr::group_by(LCP_ID) %>%
  dplyr::summarize(nForestPatches=n(),
                   minForestPatchArea=min(patch_areasqkm, na.rm=T),
                   medianForestPatchArea=median(patch_areasqkm, na.rm=T),
                   meanForestPatchArea=mean(patch_areasqkm, na.rm=T),
                   maxForestPatchArea=max(patch_areasqkm, na.rm=T)) %>%
  as.data.frame()

summary(LCP_forest_patches_summary)
hist(LCP_forest_patches_summary$nForestPatches, main='Forest patches crossed')
#write.csv(LCP_forest_patches_summary, "LeastCostPaths/LCP_characteristics/LCP_forest_patches.csv", row.names=F)
#write.csv(LCP_forest_patches_summary, "LeastCostPaths/LCP_characteristics/LCP_forest_patches_2K.csv", row.names=F)

## Road crossings
# this was fast!
LCP_roads <- terra::intersect(LCP_1km, roads)
LCP_roads_df <- as.data.frame(LCP_roads)
LCP_roads_summary <- LCP_roads_df[,c('LCP_ID','tipo')] %>%
  dplyr::group_by(LCP_ID) %>%
  dplyr::summarize(nTotalRoads=n()) %>%
  as.data.frame()
hist(LCP_roads_summary$nTotalRoads)
summary(LCP_roads_summary$nTotalRoads)
#write.csv(LCP_roads_summary, "LeastCostPaths/LCP_characteristics/LCP_roads.csv", row.names=F)
#write.csv(LCP_roads_summary, "LeastCostPaths/LCP_characteristics/LCP_roads_2K.csv", row.names=F)

## Petroleo (oil and gas 2024 dataset from RAISG) (petroleo2024)
# Done quickly (< 30 sec) in QGIS; afraid terra::intersect would have crashed with a dataset this large (as often it does)
#petro <- terra::vect("RAISG/petroleo2024/petroleo_29172_LCP_OverlapAnalysis.shp")
petro <- terra::vect("RAISG/petroleo2024/petroleo_29172_LCP_OverlapAnalysis_2K.shp")
summary(petro$petroleo_1)
hist(petro$petroleo_1)
petro_df <- data.frame(LCP_ID = LCP_1km$LCP_ID, petro_pct = petro$petroleo_1)
#write.csv(petro_df, "LeastCostPaths/LCP_characteristics/LCP_petroleo.csv", row.names=F)
#write.csv(petro_df, "LeastCostPaths/LCP_characteristics/LCP_petroleo_2K.csv", row.names=F)

## Illegal mines (MIneriaIlegal2023 dataset from RAISG) (MIneriaIlegal2023)
# Done quickly (< 1 min) in QGIS; afraid terra::intersect would have crashed with a dataset this large (as often it does)
#mines <- terra::vect("RAISG/MIneriaIlegal2023/MIneriaIlegal/MineriaIlegal_pol_29172_LCP_OverlapAnalysis.shp")
mines <- terra::vect("RAISG/MIneriaIlegal2023/MIneriaIlegal/MineriaIlegal_pol_29172_LCP_OverlapAnalysis_2K.shp")
summary(mines$MineriaI_1)
hist(mines$MineriaI_1)
mines_df <- data.frame(LCP_ID = LCP_1km$LCP_ID, illegalmines_pct = mines$MineriaI_1)
#write.csv(mines_df, "LeastCostPaths/LCP_characteristics/LCP_illegalmines.csv", row.names=F)
#write.csv(mines_df, "LeastCostPaths/LCP_characteristics/LCP_illegalmines_2K.csv", row.names=F)

#LCP_KBAs <- terra::vect("LeastCostPaths/LCP_characteristics/LCP_KBAs_OverlapAnalysis.shp")
LCP_KBAs <- terra::vect("LeastCostPaths/LCP_characteristics/LCP_KBAs_OverlapAnalysis_2K.shp")
LCP_KBAs$LCP_ID <- LCP_1km$LCP_ID
LCP_KBAs_df <- data.frame(LCP_KBAs[,c('LCP_ID','KBA_2917_1')])
names(LCP_KBAs_df) <- c('LCP_ID','KBAs_pct')
summary(LCP_KBAs_df$KBAs_pct)
hist(LCP_KBAs_df$KBAs_pct)
#write.csv(LCP_KBAs_df, "LeastCostPaths/LCP_characteristics/LCP_KBAs.csv", row.names=F)
#write.csv(LCP_KBAs_df, "LeastCostPaths/LCP_characteristics/LCP_KBAs_2K.csv", row.names=F)


#### If already run, combine individual datasets ####
LCP_roads_summary <- read.csv("LeastCostPaths/LCP_characteristics/LCP_roads.csv")
LCP_forest_patches_summary <- read.csv("LeastCostPaths/LCP_characteristics/LCP_forest_patches.csv")
LCP_forest <- read.csv("LeastCostPaths/LCP_characteristics/LCP_pct_forest.csv")
LCP_elevation <- read.csv("LeastCostPaths/LCP_characteristics/LCP_elevation.csv")
#LCP_protection <- terra::vect("LeastCostPaths/LCP_characteristics/LCP_protection_OverlapAnalysis.shp")
LCP_protection <- read.csv("LeastCostPaths/LCP_characteristics/LCP_protection.csv")
LCP_protection$LCP_ID <- seq(1,nrow(LCP_protection),1)
LCP_protection_df <- as.data.frame(LCP_protection)
LCP_canopy <- read.csv("LeastCostPaths/LCP_characteristics/LCP_canopy.csv")
LCP_mines <- read.csv("LeastCostPaths/LCP_characteristics/LCP_illegalmines.csv")
LCP_petro <- read.csv("LeastCostPaths/LCP_characteristics/LCP_petroleo.csv")
LCP_KBAs <- read.csv("LeastCostPaths/LCP_characteristics/LCP_KBAs.csv")

# updated Aug 2025 datasets
LCP_roads_summary <- read.csv("LeastCostPaths/LCP_characteristics/LCP_roads_2K.csv")
LCP_forest_patches_summary <- read.csv("LeastCostPaths/LCP_characteristics/LCP_forest_patches_2K.csv")
LCP_forest <- read.csv("LeastCostPaths/LCP_characteristics/LCP_pct_forest_2K.csv")
LCP_elevation <- read.csv("LeastCostPaths/LCP_characteristics/LCP_elevation_2K.csv")
LCP_protection <- read.csv("LeastCostPaths/LCP_characteristics/LCP_protection_2K.csv")
LCP_protection$LCP_ID <- seq(1,nrow(LCP_protection),1)
LCP_protection_df <- as.data.frame(LCP_protection)
LCP_canopy <- read.csv("LeastCostPaths/LCP_characteristics/LCP_canopy_2K.csv")
LCP_mines <- read.csv("LeastCostPaths/LCP_characteristics/LCP_illegalmines_2K.csv")
LCP_petro <- read.csv("LeastCostPaths/LCP_characteristics/LCP_petroleo_2K.csv")
LCP_KBAs <- read.csv("LeastCostPaths/LCP_characteristics/LCP_KBAs_2K.csv")

merger_list <- list(LCP_canopy[,c(1:4,7)], LCP_elevation, LCP_protection_df, 
                    LCP_roads_summary, LCP_forest_patches_summary, LCP_forest[,c(5:6)],
                    LCP_mines, LCP_petro, LCP_KBAs)
LCP_export <- Reduce(function(x, y) merge(x, y, all=T), merger_list)
LCP_export["nTotalRoads"][is.na(LCP_export["nTotalRoads"])] <- 0 #NAs are 0s (no roads turned up in intersection)
LCP_export$nTotalRoads_perkm <- LCP_export$nTotalRoads/LCP_export$lengthkm
LCP_export$nForestPatches_perkm <- LCP_export$nForestPatches/LCP_export$lengthkm
#write.csv(LCP_export, "LeastCostPaths/LCP_characteristics/LCP_combined_characteristics.csv", row.names=F)
#write.csv(LCP_export, "LeastCostPaths/LCP_characteristics/LCP_combined_characteristics_2K.csv", row.names=F)

## if already run all the merging above:
#LCP_export <- read.csv("LeastCostPaths/LCP_characteristics/LCP_combined_characteristics.csv")
LCP_export <- read.csv("LeastCostPaths/LCP_characteristics/LCP_combined_characteristics_2K.csv")


hist(LCP_export$canopy_mean)
hist(LCP_export$elevation_range)
hist(LCP_export$start_lowland_areasqkm)
hist(LCP_export$end_highland_areasqkm)
hist(LCP_export$lengthkm)
hist(LCP_export$pct_protection)
hist(LCP_export$nTotalRoads)
hist(LCP_export$nForestPatches)
hist(LCP_export$meanForestPatchArea)
hist(LCP_export$pct_forest)
hist(LCP_export$nForestPatches_perkm)
hist(LCP_export$nTotalRoads_perkm)
hist(LCP_export$illegalmines_pct)
hist(LCP_export$petro_pct)
hist(LCP_export$KBAs_pct)

# create summary table
LCP_export_summary <- LCP_export %>%
  dplyr::group_by(startProvince) %>%
  dplyr::summarize(minCanopy_m=min(canopy_mean, na.rm=T),
                   meanCanopy_m=mean(canopy_mean, na.rm=T),
                   maxCanopy_m=max(canopy_mean, na.rm=T),
                   minElevation_range=min(elevation_range, na.rm=T),
                   meanElevation_range=mean(elevation_range, na.rm=T),
                   maxElevation_range=max(elevation_range, na.rm=T),
                   minLengthkm=min(lengthkm, na.rm=T),
                   meanLengthkm=mean(lengthkm, na.rm=T),
                   maxLengthkm=max(lengthkm, na.rm=T),
                   minProtection_pct=min(pct_protection, na.rm=T),
                   meanProtection_pct=mean(pct_protection, na.rm=T),
                   maxProtection_pct=max(pct_protection, na.rm=T),
                   minTotalRoads=min(nTotalRoads, na.rm=T),
                   meanTotalRoads=mean(nTotalRoads, na.rm=T),
                   maxTotalRoads=max(nTotalRoads, na.rm=T),
                   minTotalRoads_perkm=min(nTotalRoads_perkm, na.rm=T),
                   meanTotalRoads_perkm=mean(nTotalRoads_perkm, na.rm=T),
                   maxTotalRoads_perkm=max(nTotalRoads_perkm, na.rm=T),
                   minForestPatches=min(nForestPatches, na.rm=T),
                   meanForestPatches=mean(nForestPatches, na.rm=T),
                   maxForestPatches=max(nForestPatches, na.rm=T),
                   minForestPatches_perkm=min(nForestPatches_perkm, na.rm=T),
                   meanForestPatches_perkm=mean(nForestPatches_perkm, na.rm=T),
                   maxForestPatches_perkm=max(nForestPatches_perkm, na.rm=T),
                   minForestPatch_areasqkm=min(meanForestPatchArea, na.rm=T),
                   meanForestPatch_areasqkm=mean(meanForestPatchArea, na.rm=T),
                   maxForestPatch_areasqkm=max(meanForestPatchArea, na.rm=T),
                   minForest_pct=min(pct_forest, na.rm=T),
                   meanForest_pct=mean(pct_forest, na.rm=T),
                   maxForest_pct=max(pct_forest, na.rm=T),
                   minPetro_pct=min(petro_pct, na.rm=T),
                   meanPetro_pct=mean(petro_pct, na.rm=T),
                   maxPetro_pct=max(petro_pct, na.rm=T),
                   minIllegalMines_pct=min(illegalmines_pct, na.rm=T),
                   meanIllegalMines_pct=mean(illegalmines_pct, na.rm=T),
                   maxIllegalMines_pct=max(illegalmines_pct, na.rm=T),
                   minKBAs_pct=min(KBAs_pct, na.rm=T),
                   meanKBAs_pct=mean(KBAs_pct, na.rm=T),
                   maxKBAs_pct=max(KBAs_pct, na.rm=T),
                   nLCPs=n()) %>%
  as.data.frame()
LCP_export_summary$Provincias <- c('Guianan Lowlands','Guianan','Imeri','Madeira','Napo',
                                   'Para','Rondonia','Roraima','Ucayali','Xingu-Tapajos','Yungas')
LCP_export_summary$Prov_abbr <- c('GL','GUI','IME','MAD','NAP','PAR','RON','ROR','UCA','XT','YUN')
#write.csv(LCP_export_summary, "LeastCostPaths/LCP_characteristics/LCP_combined_characteristics_summary.csv", row.names=F)
#write.csv(LCP_export_summary, "LeastCostPaths/LCP_characteristics/LCP_combined_characteristics_summary_2K.csv", row.names=F)


## Some visuals
LCP_export$Start <- LCP_export$startProvince
double <- LCP_export
double$Start <- 'All'

LCP_export_double <- rbind.data.frame(LCP_export, double)
LCP_export_double$Start <- factor(LCP_export_double$Start, levels=c('Guianan Lowlands province','Guianan province','Imeri province', 'Madeira province',
                                                                          'Napo province','Para province','Rondonia province','Roraima province',
                                                                    'Ucayali province','Xingu-Tapajos province','Yungas province','All'))
site_names <- c('GL','GUI','IME','MAD','NAP','PAR','RON','ROR',
                'UCA','XT','YUN','All')
plot_colors <- c('forestgreen','dodgerblue','orange','gold','salmon','purple','lightgreen','tan','firebrick','navy','turquoise','gray')


length_plot <-ggplot(LCP_export_double, aes(x=Start, y=lengthkm, fill=Start)) +
  geom_boxplot()+
  theme_classic()+
  theme(axis.text.x=element_text(color='black', size=8, angle=70, hjust=1),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_fill_manual(values=plot_colors)+
  scale_y_continuous(name='Length (km)')+
  scale_x_discrete(name='', labels=site_names)+
  ggtitle('A) Length')
length_plot

elevation_plot <-ggplot(LCP_export_double, aes(x=Start, y=elevation_range, fill=Start)) +
  geom_boxplot()+
  theme_classic()+
  theme(axis.text.x=element_text(color='black', size=8, angle=70, hjust=1),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_fill_manual(values=plot_colors)+
  scale_y_continuous(name='Elevation gain (m)', limits=c())+
  scale_x_discrete(name='', labels=site_names)+
  ggtitle('B) Elevation')
elevation_plot

pct_protected_plot <-ggplot(LCP_export_double, aes(x=Start, y=pct_protection, fill=Start)) +
  geom_boxplot()+
  theme_classic()+
  theme(axis.text.x=element_text(color='black', size=8, angle=70, hjust=1),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_fill_manual(values=plot_colors)+
  scale_y_continuous(name='Protected area (%)')+
  scale_x_discrete(name='', labels=site_names)+
  ggtitle('C) Protected area (%)')
pct_protected_plot

canopy_plot <-ggplot(LCP_export_double, aes(x=Start, y=canopy_mean, fill=Start)) +
  geom_boxplot()+
  theme_classic()+
  theme(axis.text.x=element_text(color='black', size=8, angle=70, hjust=1),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_fill_manual(values=plot_colors)+
  scale_y_continuous(name='Mean canopy height (m)')+
  scale_x_discrete(name='', labels=site_names)+
  ggtitle('D) Canopy height')
canopy_plot

forest_pct_plot <-ggplot(LCP_export_double, aes(x=Start, y=pct_forest, fill=Start)) +
  geom_boxplot()+
  theme_classic()+
  theme(axis.text.x=element_text(color='black', size=8, angle=70, hjust=1),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_fill_manual(values=plot_colors)+
  scale_y_continuous(name='Forest (%)')+
  scale_x_discrete(name='', labels=site_names)+
  ggtitle('E) Forest cover')
forest_pct_plot

forest_patches_plot <-ggplot(LCP_export_double, aes(x=Start, y=nForestPatches, fill=Start)) +
  geom_boxplot()+
  theme_classic()+
  theme(axis.text.x=element_text(color='black', size=8, angle=70, hjust=1),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_fill_manual(values=plot_colors)+
  scale_y_continuous(name='Patch crossings')+
  scale_x_discrete(name='', labels=site_names)+
  ggtitle('F) Forest patches')
forest_patches_plot

forest_patches_perkm_plot <-ggplot(LCP_export_double, aes(x=Start, y=nForestPatches_perkm, fill=Start)) +
  geom_boxplot()+
  theme_classic()+
  theme(axis.text.x=element_text(color='black', size=8, angle=70, hjust=1),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_fill_manual(values=plot_colors)+
  scale_y_continuous(name='Patch crossings/km')+
  scale_x_discrete(name='', labels=site_names)+
  ggtitle('G) Forest patches/km')
forest_patches_perkm_plot

roads_plot <-ggplot(LCP_export_double, aes(x=Start, y=nTotalRoads, fill=Start)) +
  geom_boxplot()+
  theme_classic()+
  theme(axis.text.x=element_text(color='black', size=8, angle=70, hjust=1),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_fill_manual(values=plot_colors)+
  scale_y_continuous(name='Road crossings')+
  scale_x_discrete(name='', labels=site_names)+
  ggtitle('H) Road crossings')
roads_plot

roads_perkm_plot <-ggplot(LCP_export_double, aes(x=Start, y=nTotalRoads_perkm, fill=Start)) +
  geom_boxplot()+
  theme_classic()+
  theme(axis.text.x=element_text(color='black', size=8, angle=70, hjust=1),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_fill_manual(values=plot_colors)+
  scale_y_continuous(name='Road crossings/km')+
  scale_x_discrete(name='', labels=site_names)+
  ggtitle('I) Road crossings/km')
roads_perkm_plot

startarea_plot <-ggplot(LCP_export_double, aes(x=Start, y=start_lowland_areasqkm, fill=Start)) +
  geom_boxplot()+
  theme_classic()+
  theme(axis.text.x=element_text(color='black', size=8, angle=70, hjust=1),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_fill_manual(values=plot_colors)+
  scale_y_continuous(name='Lowland start area (sq km)')+
  scale_x_discrete(name='', labels=site_names)+
  ggtitle('J) Lowland start area')
startarea_plot

endarea_plot <-ggplot(LCP_export_double, aes(x=Start, y=end_highland_areasqkm, fill=Start)) +
  geom_boxplot()+
  theme_classic()+
  theme(axis.text.x=element_text(color='black', size=8, angle=70, hjust=1),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_fill_manual(values=plot_colors)+
  scale_y_continuous(name='Highland end area (sq km)')+
  scale_x_discrete(name='', labels=site_names)+
  ggtitle('K) Highland end area')
endarea_plot

petro_plot <-ggplot(LCP_export_double, aes(x=Start, y=petro_pct, fill=Start)) +
  geom_boxplot()+
  theme_classic()+
  theme(axis.text.x=element_text(color='black', size=8, angle=70, hjust=1),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_fill_manual(values=plot_colors)+
  scale_y_continuous(name='Oil and gas (%)')+
  scale_x_discrete(name='', labels=site_names)+
  ggtitle('J) Oil and gas')
petro_plot

mines_plot <-ggplot(LCP_export_double, aes(x=Start, y=illegalmines_pct, fill=Start)) +
  geom_boxplot()+
  theme_classic()+
  theme(axis.text.x=element_text(color='black', size=8, angle=70, hjust=1),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_fill_manual(values=plot_colors)+
  scale_y_continuous(name='Illegal mines (%)')+
  scale_x_discrete(name='', labels=site_names)+
  ggtitle('K) Illegal mines')
mines_plot

KBAs_plot <-ggplot(LCP_export_double, aes(x=Start, y=KBAs_pct, fill=Start)) +
  geom_boxplot()+
  theme_classic()+
  theme(axis.text.x=element_text(color='black', size=8, angle=70, hjust=1),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_fill_manual(values=plot_colors)+
  scale_y_continuous(name='KBAs (%)')+
  scale_x_discrete(name='', labels=site_names)+
  ggtitle('L) KBAs')
KBAs_plot

# Export multiplanel plot
#jpeg(filename='Manuscript/multiplot_LCP_characteristics.jpeg', height=10, width=8, units='in', res=300)
jpeg(filename='Manuscript/multiplot_LCP_characteristics_2K.jpeg', height=10, width=8, units='in', res=300)
grid.arrange(length_plot, elevation_plot, pct_protected_plot, canopy_plot, 
             forest_pct_plot, forest_patches_plot, forest_patches_perkm_plot,
             roads_plot, roads_perkm_plot, petro_plot, mines_plot, KBAs_plot,
             nrow=4)
dev.off()

#### LCP prioritization based on PCA ####
# 11-21-24 update: added KBAs, illegal mines and oil gas, but drove down % explained by first 3 PCs by about 5-6%
# first check correlations and distributions for select variables
pca_variables <- c('canopy_mean','elevation_range','start_lowland_areasqkm','end_highland_areasqkm',
                   'pct_protection','nForestPatches_perkm','nTotalRoads_perkm','meanForestPatchArea',
                   'pct_forest','petro_pct','illegalmines_pct','KBAs_pct','LCP_ID','Start') 
pca_variable_df <- LCP_export[,pca_variables]

hist(pca_variable_df$canopy_mean)
hist(pca_variable_df$elevation_range)
hist(pca_variable_df$start_lowland_areasqkm)
hist(pca_variable_df$end_highland_areasqkm)
hist(pca_variable_df$pct_protection)
hist(pca_variable_df$nForestPatches_perkm)
hist(pca_variable_df$nTotalRoads_perkm)
hist(pca_variable_df$meanForestPatchArea)
hist(pca_variable_df$pct_forest)
hist(pca_variable_df$illegalmines_pct)
hist(pca_variable_df$petro_pct)
hist(pca_variable_df$KBAs_pct)

cor(pca_variable_df[,c(1:12)], method='spearman') #check correlations among input variables

# rescale road and forest patch variables so higher numbers are worse
pca_variable_df$nForestPatches_perkm_inv <- scales::rescale(pca_variable_df$nForestPatches_perkm, to=c(1,0.01))
pca_variable_df$nTotalRoads_perkm_inv <- scales::rescale(pca_variable_df$nTotalRoads_perkm, to=c(1,0.01))

# rescale oil/gas and illegal mine variables so higher numbers are worse
pca_variable_df$illegalmines_pct_inv <- scales::rescale(pca_variable_df$illegalmines_pct, to=c(1,0.01))
pca_variable_df$petro_pct_inv <- scales::rescale(pca_variable_df$petro_pct, to=c(1,0.01))


## rescale all variables prior to PCA
pca_data <- as.data.frame(scale(pca_variable_df[,c(1:5,8,9,12,15:18)]))
rownames(pca_data) <- pca_variable_df$LCP_ID
cor(pca_variable_df[,c(1:12, 15:18)], method='spearman')

## run PCA
pca_LCP <- princomp(~ canopy_mean + elevation_range + start_lowland_areasqkm + end_highland_areasqkm +
                      pct_protection + nForestPatches_perkm_inv + nTotalRoads_perkm_inv + meanForestPatchArea +
                      pct_forest + illegalmines_pct_inv + petro_pct_inv + KBAs_pct,
                    data=pca_data, cor=F, scores=T)
par(mfrow=c(1,1))
screeplot(pca_LCP, type='l')
summary(pca_LCP)
loadings(pca_LCP)


fviz_pca_var(pca_LCP,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

fviz_pca_biplot(pca_LCP, addEllipses = T, label='var', 
                habillage=pca_variable_df$Start)

fviz_pca_ind(pca_LCP, addEllipses = F, label='noe', axes=c(1,2),
             habillage=pca_variable_df$Start)

## calculate LCP prioritization index
pca_scores <- as.data.frame(pca_LCP$scores)
pca_scores$priority_index <- sqrt((pca_scores$Comp.1 ^2) + (pca_scores$Comp.2 ^2) +
                                    (pca_scores$Comp.3 ^2))
summary(pca_scores$priority_index)
hist(pca_scores$priority_index)
pca_scores$Start <- pca_variable_df$Start

# set thresholds for corridor priority index
low_cutoff <- 2
high_cutoff <- 4
pca_scores$priority_index_level <- ifelse(pca_scores$priority_index < low_cutoff, 'Low', NA)
pca_scores$priority_index_level <- ifelse(pca_scores$priority_index < high_cutoff & pca_scores$priority_index > low_cutoff, 'Medium', pca_scores$priority_index_level)
pca_scores$priority_index_level <- ifelse(pca_scores$priority_index > high_cutoff, 'High', pca_scores$priority_index_level)
pca_scores$LCP_ID <- pca_variable_df$LCP_ID

pca_scores$priority_index_rank <- as.integer(rank(desc(pca_scores$priority_index)))

#write.csv(pca_scores, file='LeastCostPaths/LCP_prioritization/LCP_priority_index.csv', row.names=F)

jpeg('Manuscript/CorridorPriorityIndex_histogram.jpeg',width = 7,height = 5,units = 'in',res=600)
par(mar = c(4, 4, 4, 2))
hist(pca_scores$priority_index, main='Corridor priority index',
     xlab='Priority', las=1, breaks=seq(0,14,0.5),#, cex.main=2, cex.lab=1.5, cex.axis=1.5,
     col=c('gray','gray','gray','gray',
           'orangered','orangered','orangered','orangered',
           'gold','gold','gold','gold','gold','gold','gold','gold','gold','gold','gold','gold','gold','gold','gold','gold'))
abline(v=2, lty='dashed', lwd=2)
abline(v=4, lty='dashed', lwd=2)
text(0.4, 450, "Low", cex=1)
text(3, 450, "Medium", cex=1)
text(5, 450, "High", cex=1)
dev.off()

## Summarize priority levels by province
# Based on starting province for now
priority_level_summary <- pca_scores %>% 
  dplyr::group_by(priority_index_level, Start) %>% 
  tally() %>%
  as.data.frame()

# without regional totals
pca_scores %>% 
  dplyr::group_by(priority_index_level) %>% 
  tally()

priority_level_summary <- dcast(priority_level_summary, priority_index_level ~ Start)
priority_level_summary[is.na(priority_level_summary)] <- 0 #NAs are true 0s
#write.csv(priority_level_summary, "LeastCostPaths/LCP_prioritization/LCP_priority_level_summary.csv", row.names=F)

# summaries for ecological characteristics
tmp_df <- cbind.data.frame(pca_scores[,c(13,15,17)], pca_variable_df)
priority_level_summary2 <- tmp_df %>% 
  dplyr::group_by(priority_index_level, Start) %>% 
  dplyr::summarize(meanCanopy = mean(canopy_mean, na.rm=T),
                   meanForest = mean(pct_forest, na.rm=T),
                   meanForestPatchArea = mean(meanForestPatchArea, na.rm=T),
                   meanForestPatches_perkm = mean(nForestPatches_perkm, na.rm=T),
                   meanElevationrange = mean(elevation_range, na.rm=T),
                   meanRoads_perkm = mean(nTotalRoads_perkm, na.rm=T),
                   meanStartareasqkm = mean(start_lowland_areasqkm, na.rm=T),
                   meanEndareasqkm = mean(end_highland_areasqkm, na.rm=T),
                   meanProtection = mean(pct_protection, na.rm=T),
                   meanKBA_pct = mean(KBAs_pct, na.rm=T),
                   meanpetro_pct = mean(petro_pct, na.rm=T),
                   meanmines_pct = mean(illegalmines_pct, na.rm=T),
                   nLCPS = n()) %>%
  as.data.frame()
write.csv(priority_level_summary2, "LeastCostPaths/LCP_prioritization/LCP_priority_level_characteristics_summary.csv", row.names=F)

## Merge LCP priority index values to LCP shapefiles for mapping in QGIS
LCP_shp <- terra::vect("LeastCostPaths/CombinedLeastCostPaths/CombinedLeastCostPaths_amazon_wRegions.shp")
LCP_shp_1km <- terra::vect("LeastCostPaths/CombinedLeastCostPaths/CombinedLeastCostPaths_amazon_1kmbuff.shp")
LCP_priority_shp <- cbind(LCP_shp, pca_scores[,c(13:17)])
#terra::writeVector(LCP_priority_shp, filename="LeastCostPaths/LCP_prioritization/LCP_prioritization.shp", overwrite=T)
LCP_priority_shp_1km <- cbind(LCP_shp_1km, pca_scores[,c(13:17)])
#terra::writeVector(LCP_priority_shp_1km, filename="LeastCostPaths/LCP_prioritization/LCP_prioritization_1kmbuff.shp", overwrite=T)
