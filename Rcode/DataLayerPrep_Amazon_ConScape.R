################### Amazon: prepare data layers for ConScape ######################
# Date: 6-21-24
# updated:
# Author: Ian McCullough, immccull@gmail.com
###################################################################################

#### R libraries ####
library(terra)

#### Input data ####
setwd("C:/Users/immccull/Documents/AmazonClimateCorridors")

# Amazon study area
amazon_study_area <- terra::vect("RAISG/Limites2023/Limites/Lim_Biogeografico.shp")
amazon_study_area <- terra::project(amazon_study_area, "EPSG:29172")

# South America
southamerica <- terra::vect("RAISG/SA_countries/SouthAmerica.shp")
southamerica <- terra::project(southamerica, "EPSG:29172")

# Conductance surface (modified based on canopy height)
#conductance <- terra::rast("Conductance/amazon_conductance_250m.tif")
conductance <- terra::rast("Conductance/amazon_conductance_500m.tif")

# Start and end nodes
# start_pts <- terra::vect("start_nodes/start_nodes_amazon_points.shp")
# start_pts <- terra::project(start_pts, "EPSG:29172")
start_polygons <- terra::vect("start_nodes/start_nodes_amazon_polygons.shp")
start_polygons <- terra::project(start_polygons, "EPSG:29172")

# end_pts <- terra::vect("end_nodes/end_nodes_amazon_points.shp")
# end_pts <- terra::project(end_pts, "EPSG:29172")
end_polygons <- terra::vect("end_nodes/end_nodes_amazon_polygons.shp")
end_polygons <- terra::project(end_polygons, "EPSG:29172")

DEM <- terra::rast("DEM/SA_DEM_mosaic.tif")

#### Main program ####
#plot(southamerica)
#plot(amazon_study_area, add=T, col='green')

plot(conductance)
plot(start_polygons, add=T, col='orange')
plot(end_polygons, add=T, col='dodgerblue')

## convert start and end polygons to raster
#start_polygons_rast <- terra::rasterize(start_polygons, conductance,values=1)
#end_polygons_rast <- terra::rasterize(end_polygons, conductance,values=1)
#polygons_rast_mosaic <- terra::merge(start_polygons_rast, end_polygons_rast, first=T, na.rm=T)


## identify low and high zones of protected areas
# first create masks of lowland and highland areas
amazon_study_area_4326 <- terra::project(amazon_study_area, "EPSG:4326")
conductance_4326 <- terra::project(conductance, "EPSG:4326")
DEM_cropped <- terra::crop(DEM, conductance_4326)
low_DEM_mask <- terra::ifel(DEM_cropped <= 500, 1, NA)
low_DEM_mask <- terra::crop(low_DEM_mask, amazon_study_area_4326, mask=T)
plot(amazon_study_area_4326)
plot(low_DEM_mask, add=T)
low_DEM_mask_polygons <- terra::as.polygons(low_DEM_mask, na.rm=T) #slow!
low_DEM_mask_polygons <- terra::project(low_DEM_mask_polygons, "EPSG:29172")
#terra::writeVector(low_DEM_mask_polygons, filename="ConScape/staging/low_DEM_mask_polygons_500m.shp", overwrite=T)

high_DEM_mask <- terra::ifel(DEM_cropped >= 2500 & DEM_cropped <= 3500, 1, NA)
high_DEM_mask <- terra::crop(high_DEM_mask, amazon_study_area_4326, mask=T)
plot(amazon_study_area_4326)
plot(high_DEM_mask, add=T)
high_DEM_mask_polygons <- terra::as.polygons(high_DEM_mask, na.rm=T)
high_DEM_mask_polygons <- terra::project(high_DEM_mask_polygons, "EPSG:29172")
#terra::writeVector(high_DEM_mask_polygons, filename="ConScape/staging/high_DEM_mask_polygons_500m.shp", overwrite=T)

# Now that we have the elevation zones, need to extract PA portions of those
# R seems to crash here...try Q
# low_PA_zones <- terra::intersect(start_polygons, low_DEM_mask_polygons)
# plot(amazon_study_area)
# plot(start_polygons, add=T, col='gray')
# plot(low_PA_zones, add=T, col='dodgerblue')
# low_PA_zones$lowareasqkm <- terra::expanse(low_PA_zones, unit="km")
# low_PA_zones$rowID <- seq(1, nrow(low_PA_zones), 1)
# low_PA_zones$pct_lowland <- (low_PA_zones$lowareasqkm/low_PA_zones$areasqkm)*100
# summary(low_PA_zones$pct_lowland)
# hist(low_PA_zones$pct_lowland)
# 
# high_PA_zones <- terra::intersect(end_polygons, high_DEM_mask_polygons)
# plot(amazon_study_area)
# plot(end_polygons, add=T, col='gray')
# plot(high_PA_zones, add=T, col='dodgerblue')
# high_PA_zones$highareasqkm <- terra::expanse(high_PA_zones, unit="km")
# high_PA_zones$rowID <- seq(1, nrow(high_PA_zones), 1)
# #high_tmp <- data.frame(rowID = high_PA_zones$rowID, highareasqkm=high_PA_zones$highareasqkm)
# high_PA_zones$pct_highland <- (high_PA_zones$highareasqkm/high_PA_zones$areasqkm)*100
# summary(high_PA_zones$pct_highland)
# hist(high_PA_zones$pct_highland)
