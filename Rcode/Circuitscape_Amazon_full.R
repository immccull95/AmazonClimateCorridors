################# Set up for Amazon Circuitscape in Julia #######################
# Date: 11-20-23
# updated: 
# Author: Ian McCullough, immccull@gmail.com, Chris Beirne (chrisbeirne@osaconservation.org)
###################################################################################

# Resources (compiled by Chris)
#https://docs.circuitscape.org/Circuitscape.jl/latest/
#https://www.mdpi.com/2073-445X/10/3/301/htm
#https://docs.circuitscape.org/Omniscape.jl/dev/usage/

#### R libraries ####
# for some reason, installing packages kept failing, but using a different CRAN mirror seemed to help, e.g.,:
# install.packages('leaflet', repos='http://cran.us.r-project.org')
# also had to install e1071 in this manner
library(stars)
library(sf)
library(leaflet)
library(terra)

#### Input data ####
setwd("C:/Users/immccull/Documents/AmazonClimateCorridors")

# Study area
morrone2022_amazonclip <- terra::vect("Regions/Morrone2022/Amazon_macroregions_fixedgeom_Amazonclip.shp")

# Conductance surface (lc: land cover)
lc <- terra::rast("Conductance/amazon_conductance_500m.tif")
lc <- terra::aggregate(lc, fact=20, fun="mean", na.rm=T) #10km res

# All start and end polygons
start_all <- terra::vect("start_nodes/start_nodes_amazon_polygons.shp")
start_all <- terra::project(start_all, "EPSG:29172")
end_all <- terra::vect("end_nodes/end_nodes_amazon_polygons.shp")
end_all <- terra::project(end_all, "EPSG:29172")

plot(lc)
plot(start_all, add=T, col='forestgreen')
plot(end_all, add=T, col='gold')

# Give each one an ID code
end_all$NODE_CODE <- paste0("H",sprintf("%04d", 1:nrow(end_all)))

# Centroids of end areas
end_all_sf <- sf::st_as_sf(end_all)
end_points <- sf::st_point_on_surface(end_all_sf)

# Try adding in some other locations (to account for large polygons)
set.seed(1200)
test <- st_sample(end_all_sf, 500, type = "hexagonal")

# create point grid within start node polygons
template <- rast(morrone2022_amazonclip, resolution = c(1000,1000))

# Fake values, for demo only
values(template) <- 1:ncell(template)

# Mask to polygon
template <- mask(template, morrone2022_amazonclip)

plot(morrone2022_amazonclip)
plot(template, add=T, alpha=0.5)

# To points: This is now a vector
points <- as.points(template, na.rm = T)

# Intersect to keep only points on the shape
points <- points[morrone2022_amazonclip]

# Check
distance(points[20], points[21]) #should be 1000

plot(morrone2022_amazonclip)
plot(points, add=T, col='red')

start_points <- terra::intersect(points, start_all)
plot(morrone2022_amazonclip)
plot(start_points, add=T, col='orange')

# Make an AOI layer
aoi <- ext(lc) 

# Turn all the end polygons into a raster, use the LC layer as a template to match resolutions
end_ras <- rasterize(end_all,lc, field=999)
#end_ras <- rasterize(end_all, lc_20, field=999)
# Create a polygon version of this new layer and plot it to check it looks sensible
polygons <- end_ras
plot(morrone2022_amazonclip)
plot(polygons, add=T) #should see end nodes

# Create the source layer - rasterize them as before
#sources <- rasterize(start_p,lc) # note leaving the field blank makes all of the values 1 
sources <- rasterize(start_points, lc)

# So now we have 999 for end nodes, and 1 for start nodes

# We need end nodes within the end polygons -> the extent of the polygon will take this value
# Create end layer
end_v <- crop(end_all, aoi)
end_p <- centroids(end_v, inside=T)
#end_p <- terra::intersect(end_nodes, morrone2022_amazonclip)
# These are our "grounds" where the current will travel to
# Basically just one dot if we only use one big Amazon blob
grounds <- rasterize(end_p,lc, 
                     field=1) # make the area the current

# Write the objects to a julia folder
#dir.create("julia/")

# Conductance surface
writeRaster(lc, "julia/cellmap.asc", overwrite=TRUE, gdal="DECIMAL_PRECISION=0", NAflag=-9999)

# Sources
writeRaster(sources, "julia/sources.asc",  overwrite=TRUE, gdal="DECIMAL_PRECISION=0", NAflag=-9999)

# Grounds
writeRaster(grounds, "julia/grounds.asc", overwrite=TRUE, gdal="DECIMAL_PRECISION=0", NAflag=-9999)

# Polygons
writeRaster(polygons, "julia/regions_grid.asc", overwrite=TRUE, gdal="DECIMAL_PRECISION=0", NAflag=-9999)

#dir.create("julia/output")

## crop/mask output to study area
current <- terra::rast("julia/output/Amazon_10km_cgamg_new_curmap.asc")
plot(current)

current_cropped <- terra::crop(current, morrone2022_amazonclip, mask=T)
plot(current_cropped)
#writeRaster(current_cropped, filename="julia/output/current10km_cropped.tif", overwrite=T)
