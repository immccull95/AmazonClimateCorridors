################# Set up for Amazon Circuitscape in Julia #######################
# Date: 11-20-23
# updated: 8-22-25; new end nodes
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

setwd("C:/Users/immccull/Documents/AmazonClimateCorridors")

# Study area
morrone2022_amazonclip <- terra::vect("Regions/Morrone2022/Amazon_macroregions_fixedgeom_Amazonclip.shp")

# Conductance surface (lc: land cover)
lc <- terra::rast("Conductance/amazon_conductance_500m.tif")
#lc <- terra::rast("Conductance/amazon_conductance_1km.tif")
#lc <- terra::aggregate(lc, fact=20, fun="mean", na.rm=T) #10km res from 500m
lc <- terra::aggregate(lc, fact=2, fun="mean", na.rm=T) #1km res from 500m
#terra::writeRaster(lc, "Conductance/amazon_conductance_1km.tif", overwrite=T)

# All start and end polygons
start_all <- terra::vect("start_nodes/start_nodes_amazon_polygons.shp")
start_all <- terra::project(start_all, "EPSG:29172")
end_all <- terra::vect("end_nodes/end_nodes_amazon_polygons.shp")
end_all <- terra::project(end_all, "EPSG:29172")

# Aug 2025 update: new end nodes (2000 m lower elevation limit, high forest landscape integrity index)
end_all <- terra::vect("end_nodes/dominions/end_polygons/combined_end_polygons/combined_end_polygons_2K.shp")

plot(lc)
plot(start_all, add=T, col='forestgreen')
plot(end_all, add=T, col='gold')

# Give each one an ID code
end_all$NODE_CODE <- paste0("H",sprintf("%04d", 1:nrow(end_all)))

# Centroids of end areas and start areas
end_all_sf <- sf::st_as_sf(end_all)
end_points <- sf::st_point_on_surface(end_all_sf)

start_all_sf <- sf::st_as_sf(start_all)
start_points <- sf::st_point_on_surface(start_all_sf)

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
#writeRaster(lc, "julia/cellmap.asc", overwrite=TRUE, gdal="DECIMAL_PRECISION=0", NAflag=-9999)
writeRaster(lc, "julia/1km/cellmap.asc", overwrite=TRUE, gdal="DECIMAL_PRECISION=0", NAflag=-9999)

# Sources
#writeRaster(sources, "julia/sources.asc",  overwrite=TRUE, gdal="DECIMAL_PRECISION=0", NAflag=-9999)
writeRaster(sources, "julia/1km/sources.asc",  overwrite=TRUE, gdal="DECIMAL_PRECISION=0", NAflag=-9999)

# Grounds
#writeRaster(grounds, "julia/grounds.asc", overwrite=TRUE, gdal="DECIMAL_PRECISION=0", NAflag=-9999)
writeRaster(grounds, "julia/1km/grounds.asc", overwrite=TRUE, gdal="DECIMAL_PRECISION=0", NAflag=-9999)

# Polygons
#writeRaster(polygons, "julia/regions_grid.asc", overwrite=TRUE, gdal="DECIMAL_PRECISION=0", NAflag=-9999)
writeRaster(polygons, "julia/1km/regions_grid.asc", overwrite=TRUE, gdal="DECIMAL_PRECISION=0", NAflag=-9999)

#dir.create("julia/output")

## crop/mask output to study area
#current <- terra::rast("julia/output/Amazon_10km_cgamg_new_curmap.asc")
current <- terra::rast("julia/1km/output/Amazon_1km_cgamg_new_curmap.asc")
plot(current)

current_cropped <- terra::crop(current, morrone2022_amazonclip, mask=T)
plot(current_cropped)
#writeRaster(current_cropped, filename="julia/output/current10km_cropped.tif", overwrite=T)
#writeRaster(current_cropped, filename="julia/1km/output/current1km_cropped.tif", overwrite=T)

# identify high-current areas
# f you Hijmans...quantile is already a function
quantz <- stats::quantile(current_cropped, probs=seq(0, 1, 0.1), na.rm=T) 

#high_current <- terra::ifel(current_cropped >= 1.034001e+03, 1, NA)
high_current <- terra::ifel(current_cropped >= 3.154100e-01, 1, NA) #new Aug 2025
plot(high_current)
#writeRaster(high_current, filename="julia/1km/output/high_current1km_cropped.tif", overwrite=T)

# identify unprotected high-current areas
# mask out areas in protected areas; focus restoration/conservation on unprotected areas
# Protected areas
protected_areas <- terra::vect("protected_areas/Amazon_merged_PAs/Amazon_merged_PAs_dissolved10km_wRegion.shp")
protected_areas <- terra::project(protected_areas, "EPSG:29172")
pa_mask <- terra::rasterize(protected_areas, high_current)

protected_high_current <- terra::mask(high_current, pa_mask, inverse=F)

unprotected_high_current <- terra::mask(high_current, pa_mask, inverse=T)
plot(unprotected_high_current)
#writeRaster(unprotected_high_current, "julia/1km/output/high_current1km_unprotected_cropped.tif", overwrite=T)

#hc_unprotected_patches <- terra::patches(unprotected_high_current, directions=8, allowGaps=F,
#                                         filename="julia/1km/output/high_current1km_unprotected_patches.tif")

hc_unprotected_patches <- terra::rast("julia/1km/output/high_current1km_unprotected_patches.tif")

#hc_protected_patches <- terra::patches(protected_high_current, directions=8, allowGaps=F,
#                                         filename="julia/1km/output/high_current1km_protected_patches.tif")

hc_protected_patches <- terra::rast("julia/1km/output/high_current1km_protected_patches.tif")

# what percentage of high current areas is protected?
a <- sum(expanse(as.polygons(hc_protected_patches), "km"))
b <- sum(expanse(as.polygons(hc_unprotected_patches), "km"))

#a <- expanse(protected_high_current, "km") #slow
#b <- expanse(high_current, "km")
b/a

## patch analysis of high-current areas
library(landscapemetrics)
#lsm_l_np(hc_protected_patches, directions=8)# same as just entering input name in console when allowGaps=F in terra::patches

# produces output in hectares
unprotected_patch_area <- lsm_p_area(hc_unprotected_patches, directions=8)
summary(unprotected_patch_area$value)
test <- as.data.frame(unprotected_patch_area)
test$sqkm <- test$value*0.01
hist(unprotected_patch_area$value)
summary(test)

protected_patch_area <- lsm_p_area(hc_protected_patches, directions=8)
summary(protected_patch_area$value)
test2 <- as.data.frame(protected_patch_area)
test2$sqkm <- test2$value*0.01
summary(test2)
