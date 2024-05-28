##################### Amazon Circuitscape preparations ############################
# Date: 4-23-24
# updated: 4-25-24
# Author: Ian McCullough, immccull@gmail.com
###################################################################################

#### R libraries ####
library(stars)
library(sf)
library(leaflet)
library(terra)

#### Input data ####
setwd("C:/Users/immccull/Documents/Amazon")

# Study area
amazon_study_area <- terra::vect("RAISG/StudyArea/western_Amazon_study_area1_29172.shp")

# Conductance surface (lc: land cover)
lc <- terra::rast("Conductance/western_amazon_conductance.tif")

# All start and end polygons
start_all <- terra::vect("start_nodes/start_nodes_pilot_5km_polygons.shp")
end_all <- terra::vect("end_nodes/end_nodes_pilot_5km_polygons.shp")

start_points <- terra::vect("start_nodes/start_nodes_pilot_5km_points.shp")
end_points <- terra::vect("end_nodes/end_nodes_pilot_5km_points.shp")

#### Main program ####
# create point grid within start node polygons
template <- terra::rast(amazon_study_area, resolution = c(1000,1000))

# Fake values, for demo only
values(template) <- 1:ncell(template)

# Mask to polygon
template <- terra::mask(template, amazon_study_area)

plot(amazon_study_area)
plot(template, add=T, alpha=0.5)

# To points: This is now a vector
points <- as.points(template, na.rm = T)

# Intersect to keep only points on the shape
points <- points[amazon_study_area]
 
# # Check
# distance(points[20], points[21])
# 
# plot(amazon_study_area)
# plot(points, add=T, col='red') 

start_points <- terra::intersect(points, start_all)
plot(amazon_study_area)
plot(start_points, add=T, col='orange')

# Make an AOI layer
aoi <- ext(lc) 

## resample conductance since it keeps crashing otherwise
lc_resample <- terra::aggregate(lc, fact=5, fun='mean', na.rm=T)

# Turn all the end polygons into a raster, use the LC layer as a template to match resolutions
#end_ras <- rasterize(end_all,lc, field=999)
end_ras <- rasterize(end_all, lc_resample, field=999)
# Create a polygon version of this new layer and plot it to check it looks sensible
polygons <- end_ras
plot(amazon_study_area)
plot(polygons, add=T)

# Create the source layer - rasterize them as before
#sources <- rasterize(start_points, lc)
sources <- rasterize(start_points, lc_resample)

plot(amazon_study_area)
plot(sources, add=T, col='red')

# We need end nodes within the end polygons -> the extent of the polygon will take this value
# Create end layer
end_v <- crop(end_all, aoi)
#end_p <- terra::intersect(end_points, amazon_study_area)
# These are our "grounds" where the current will travel to
#grounds <- rasterize(end_points,lc, field=1) # make the area the current
grounds <- rasterize(end_points,lc_resample, field=1)
# Write the objects to a julia folder
#dir.create("julia")
#dir.create("julia/resample500m")

# Resistance surface
#writeRaster(lc, "julia/cellmap.asc", overwrite=T, gdal="DECIMAL_PRECISION=0", NAflag=-9999)
writeRaster(lc_resample, "julia/resample500m/cellmap.asc", overwrite=T, gdal="DECIMAL_PRECISION=0", NAflag=-9999)

# Sources
#writeRaster(sources, "julia/sources.asc",  overwrite=T, gdal="DECIMAL_PRECISION=0", NAflag=-9999)
writeRaster(sources, "julia/resample500m/sources.asc",  overwrite=T, gdal="DECIMAL_PRECISION=0", NAflag=-9999)

# Grounds
#writeRaster(grounds, "julia/grounds.asc", overwrite=T, gdal="DECIMAL_PRECISION=0", NAflag=-9999)
writeRaster(grounds, "julia/resample500m/grounds.asc", overwrite=T, gdal="DECIMAL_PRECISION=0", NAflag=-9999)


# Polygon
#writeRaster(polygons, "julia/regions_grid.asc", overwrite=T, gdal="DECIMAL_PRECISION=0", NAflag=-9999)
writeRaster(polygons, "julia/resample500m/regions_grid.asc", overwrite=T, gdal="DECIMAL_PRECISION=0", NAflag=-9999)


#dir.create("julia/output")
#dir.create("julia/resample500m/output")


#### inspect output
# this took slightly over 2 mins to run in Julia
#current_500m <- terra::rast("julia/resample500m/output/amazon_pilot_500m_curmap.asc")
#current_500m_cropped <- terra::crop(current_500m, amazon_study_area, mask=T)
#writeRaster(current_500m_cropped, filename='julia/resample500m/output/current_cropped.tif', overwrite=T)
current_500m_cropped <- terra::rast('julia/resample500m/output/current_cropped.tif')


voltage_500m <- terra::rast("julia/resample500m/output/amazon_pilot_500m_voltmap.asc")
voltage_500m_cropped <- terra::crop(voltage_500m, amazon_study_area, mask=T)
#writeRaster(voltage_500m_cropped, filename='julia/resample500m/output/voltage_cropped.tif', overwrite=T)

plot(amazon_study_area)
plot(current_500m_cropped, add=T)

plot(amazon_study_area)
plot(voltage_500m_cropped, add=T)

## Calculated accumulated current in each end node:
# Per the Circuitscape documentation:
#For the advanced modeling mode, a single map will be written showing current 
# densities at each cell resulting from the current source and ground configurations in the input files

# Therefore, we just need to extract and sum the data for each end node
end_node_current <- terra::extract(current_500m_cropped, end_all, fun="sum", na.rm=T)
names(end_node_current) <- c('ID','TotalCurrent')
hist(end_node_current$TotalCurrent, main='End node total current',
     xlab='Total current', xlim=c(0,3e+09),
     breaks=seq(0,2.6e+09, 100000000))
end_node_current$rank <- rank(-end_node_current$TotalCurrent, ties.method='average')

end_points_current <- cbind(end_points, end_node_current)
#View(as.data.frame(end_points_current))
end_all_current <- cbind(end_all, end_node_current)

#writeVector(end_points_current, filename='julia/resample500m/output/end_points_current.shp', overwrite=T)
#writeVector(end_all_current, filename='julia/resample500m/output/end_all_current.shp', overwrite=T)
#write.csv(as.data.frame(end_all_current), file='end_nodes/end_node_current.csv', row.names=F)

