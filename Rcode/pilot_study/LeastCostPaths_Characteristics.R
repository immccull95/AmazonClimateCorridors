##################### Amazon least cost path characteristics ######################
# Date: 4-22-24
# updated: 4-24-24
# Author: Ian McCullough, immccull@gmail.com
###################################################################################

#### R libraries ####
library(terra)
library(sf)
library(dplyr)
library(leastcostpath)

#### Input data ####
setwd("C:/Users/immccull/Documents/Amazon")

# Land cover
lcc <- terra::rast("LCC/LCC_western_amazon_compressed.tif")

# Study area
amazon_study_area <- terra::vect("RAISG/StudyArea/western_Amazon_study_area1_29172.shp")

# DEM
DEM <- terra::rast("DEM/SA_DEM_mosaic.tif")

# Least cost paths (from LeastCostPaths script)
LCP_combined <- terra::vect("LeastCostPaths/CombinedLeastCostPaths/CombinedLeastCostPaths_1000mBuff.shp")
LCP_combined_lines <- terra::vect("LeastCostPaths/CombinedLeastCostPaths/CombinedLeastCostPaths.shp")

# Protected areas
protected_areas <- terra::vect("protected_areas/wdpa_SA_pilot_wElev.shp")
protected_areas <- terra::project(protected_areas, "EPSG:29172")

# Roads
roads_union <- terra::vect("RAISG/vias2023-1/vias/study_area/roads_buffer_union.shp")

# Land cover, resampled to 100 m
lcc_100m <- terra::rast('LCC/lcc_100m_29172.tif')
forest <- terra::rast('LCC/LCC_100m_forest.tif')


############ Main program ############

## Elevation
LCP_combined$LCP_ID <- seq(1, nrow(LCP_combined), 1)

# only run once; too slow to do over and over
# LCP_elevation_mean <- terra::extract(DEM, LCP_combined, fun='mean', na.rm=T)
# LCP_elevation_min <- terra::extract(DEM, LCP_combined, fun='min', na.rm=T)
# LCP_elevation_max <- terra::extract(DEM, LCP_combined, fun='max', na.rm=T)
# LCP_elevation <- cbind.data.frame(LCP_elevation_mean[,2], LCP_elevation_min[,2], LCP_elevation_max[,2])
# colnames(LCP_elevation) <- c('elevation_mean','elevation_min','elevation_max')
# LCP_elevation$elevation_range <- LCP_elevation$elevation_max - LCP_elevation$elevation_min
# LCP_elevation$LCP_ID <- LCP_combined$LCP_ID
# summary(LCP_elevation)
# hist(LCP_elevation$elevation_mean, main='LCP mean elevation', xlab='Elevation (m)')
# hist(LCP_elevation$elevation_range, main='LCP elevation range', xlab='Elevation (m)')

#write.csv(LCP_elevation, "LeastCostPaths/LCP_characteristics/LCP_elevation.csv", row.names=F)
LCP_elevation <- read.csv("LeastCostPaths/LCP_characteristics/LCP_elevation.csv")


## Protection
# tried in SF; didn't work either
# LCP_combined_sf <- st_as_sf(LCP_combined)
# protected_areas_sf <- st_as_sf(protected_areas)
# LCP_protection <- st_intersection(LCP_combined_sf, protected_areas_sf)

## Number of protected areas
# used QGIS to fix geometries
protected_areas <- terra::vect("protected_areas/wdpa_SA_pilot_wElev_fixgeom.shp")
#LCP_protection <- terra::vect("LeastCostPaths/CombinedLeastCostPaths/CombinedLeastCostPaths_1000mBuff_fixedgeom.shp")
# 
# LCP_protection <- terra::intersect(LCP_combined, protected_areas) #crashes; also crashes after fixing geometries both layers 
# #LCP_protection_df <- as.data.frame(LCP_protection)
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

# need to deal with overlapping LCPs within PAs
#protected_areas_dissolved <- terra::aggregate(protected_areas) #crashes
protected_areas_dissolved <- terra::vect("protected_areas/wdpa_SA_pilot_wElev_dissolved_fixgeom.shp") #used Dissolve in QGIS
protected_areas_dissolved <- terra::aggregate(protected_areas_dissolved) #make into single polygon

plot(amazon_study_area)
plot(protected_areas, add=T, col='dodgerblue')
plot(protected_areas_dissolved, add=T, col='red')

LCP_protection_dissolved_df <- read.csv("LeastCostPaths/LCP_characteristics/LCP_protection_pct.csv")

# LCP_protection_dissolved <- terra::intersect(LCP_combined, protected_areas_dissolved)
# LCP_protection_dissolved_area <- terra::expanse(LCP_protection_dissolved, unit='km')
# LCP_protection_dissolved_df <- as.data.frame(LCP_protection_dissolved)
# LCP_protection_dissolved_df$PAareasqkm <- LCP_protection_dissolved_area
# LCP_protection_dissolved_df$LCP_area <- terra::expanse(LCP_combined, unit="km")
# LCP_protection_dissolved_df$PA_pct <- (LCP_protection_dissolved_df$PAareasqkm/LCP_protection_dissolved_df$LCP_area)*100
summary(LCP_protection_dissolved_df$PA_pct)
#fix decimal precision (i.e,. set 100.000001 to 100)
#LCP_protection_dissolved_df$PA_pct <- ifelse(LCP_protection_dissolved_df$PA_pct > 100, 100, LCP_protection_dissolved_df$PA_pct)
hist(LCP_protection_dissolved_df$PA_pct, main='Corridor protection', xlim=c(0,100), xlab='Protection (%)',
     breaks=seq(0,100,1))
#write.csv(LCP_protection_dissolved_df, "LeastCostPaths/LCP_characteristics/LCP_protection_pct.csv", row.names=F)

## export shapefile of LCPs with % protection
LCP_export <- cbind(LCP_combined_lines, LCP_protection_dissolved_df)
LCP_export <- subset(LCP_export, LCP_export$PA_pct >= 95)
#writeVector(LCP_export, "LeastCostPaths/LCP_characteristics/LCP_protection.shp", overwrite=T)

## Roads
# If this crashes, can use sum line lengths in QGIS
# nacional <- terra::vect("RAISG/vias2023-1/vias/study_area/vias_nacional_clip_29172.shp")
# departamental <- terra::vect("RAISG/vias2023-1/vias/study_area/vias_departamental_clip_29172.shp")
# ferrea <- terra::vect("RAISG/vias2023-1/vias/study_area/vias_ferrea_clip_29172.shp")
# 
# LCP_nacional <- terra::intersect(LCP_combined, nacional)
# LCP_departamental <- terra::intersect(LCP_combined, departamental)
# LCP_ferrea <- terra::intersect(LCP_combined, ferrea)
# 
# LCP_nacional_df <- as.data.frame(LCP_nacional)
# LCP_nacional_df <- LCP_nacional_df %>%
#   dplyr::group_by(LCP_ID) %>%
#   dplyr::summarize(nNacional = n()) %>%
#   as.data.frame()
# 
# LCP_departamental_df <- as.data.frame(LCP_departamental)
# LCP_departamental_df <- LCP_departamental_df %>%
#   dplyr::group_by(LCP_ID) %>%
#   dplyr::summarize(nDepartamental = n()) %>%
#   as.data.frame()
# 
# LCP_ferrea_df <- as.data.frame(LCP_ferrea)
# LCP_ferrea_df <- LCP_ferrea_df %>%
#   dplyr::group_by(LCP_ID) %>%
#   dplyr::summarize(nFerrea = n()) %>%
#   as.data.frame()
# 
# LCP_roads_df <- data.frame(LCP_combined$LCP_ID)
# names(LCP_roads_df) <- 'LCP_ID'
# df_list <- list(LCP_roads_df, LCP_nacional_df, LCP_departamental_df, LCP_ferrea_df)
# LCP_roads_df <- Reduce(function(x, y) merge(x, y, all=T), df_list)
# LCP_roads_df[is.na(LCP_roads_df)] <- 0 #NA is true 0
# LCP_roads_df$nTotalRoads <- rowSums(LCP_roads_df[,c(2:4)])
# 
# # Calculate roads per km
# LCP_roads_df$LCP_length_km <- (terra::perim(LCP_combined_lines))/1000 #OK since still ordered 1-97
# LCP_roads_df$nNacional_perkm <- LCP_roads_df$nNacional/LCP_roads_df$LCP_length_km
# LCP_roads_df$nDepartamental_perkm <- LCP_roads_df$nDepartamental/LCP_roads_df$LCP_length_km
# LCP_roads_df$nFerrea_perkm <- LCP_roads_df$nFerrea/LCP_roads_df$LCP_length_km
# LCP_roads_df$nTotalRoads_perkm <- LCP_roads_df$nTotalRoads/LCP_roads_df$LCP_length_km
LCP_roads_df <- read.csv("LeastCostPaths/LCP_characteristics/LCP_roads.csv")
summary(LCP_roads_df)

hist(LCP_roads_df$nTotalRoads, main='Total road crossings per corridor', xlab='Total roads',
     xlim=c(0,80), breaks=seq(0,80,1))

hist(LCP_roads_df$nTotalRoads_perkm, main='Total road crossings per km of corridor', xlab='Total roads per km',
     xlim=c(0,0.5), breaks=seq(0,0.5,0.01))

#write.csv(LCP_roads_df, "LeastCostPaths/LCP_characteristics/LCP_roads.csv", row.names=F)


## Forest patches
forest_1000m <- terra::aggregate(forest, fact=10, fun="mean")
forest_patches <- terra::patches(forest, directions=8, allowGaps=F,
                                 filename='LCC/forest_patches_100m.tif')
forest_polygons <- terra::as.polygons(forest_patches, aggregate=T, na.rm=T)

## LCP density
lcp_density <- create_lcp_density(forest_1000m, LCP_combined_lines)
plot(lcp_density)
#writeRaster(lcp_density, filename='LeastCostPaths/LCP_density/LCP_density.tif', overwrite=T)

### Create lookup table ###
lookup <- merge(LCP_elevation[,c(4,5)], LCP_roads_df[,c(1,5,6,10)], by='LCP_ID')
lookup <- merge(lookup, LCP_protection_dissolved_df[,c(4,6,8,16,19)], by="LCP_ID")
lookup <- lookup[,c(7,8,4,2,9,3,5,1)]
names(lookup) <- c('Start','End','Length_km','Elevation_gain_m','Protection_pct',
                   'Total_roads','Roads_perkm','LCP_ID')

## update: get countries
start_points <- terra::vect("start_nodes/start_nodes_pilot_5km_points.shp")
start_points$LCP_ID <- seq(1,97,1)
end_points <- terra::vect("end_nodes/end_nodes_pilot_5km_points.shp")

start_pais <- as.data.frame(start_points[,c("LCP_ID","Name_1")])

lookup <- merge(lookup, start_pais, by='LCP_ID')
names(lookup) <- c('LCP_ID','Start','End','Length_km','Elevation_gain_m','Protection_pct',
                   'Total_roads','Roads_perkm','Start_country')


##
end_pais <- as.data.frame(end_points[,c("Name_1","NAME")])

lookup <- merge(lookup, end_pais, by.x='End', by.y='NAME')
names(lookup) <- c('End','LCP_ID','Start','Length_km',
                   'Elevation_gain_m','Protection_pct','Total_roads',
                   'Roads_perkm','Start_country','End_country')
lookup <- lookup[,c(3,9,1,10,4,5,6,7,8)]

#write.csv(lookup, file="LeastCostPaths/LCP_lookup_table.csv", row.names=F)
