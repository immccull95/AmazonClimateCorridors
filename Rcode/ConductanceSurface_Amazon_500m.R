######################## Amazon conductance surface ###############################
# Date: 6-4-24
# updated: 6-5-24
# Author: Ian McCullough, immccull@gmail.com
###################################################################################

#### R libraries ####
library(terra)

#### Input data ####
setwd("C:/Users/immccull/Documents/AmazonClimateCorridors")

# Amazon study area
amazon_study_area <- terra::vect("RAISG/Limites2023/Limites/Lim_Biogeografico.shp")

# Land cover mosaics at different resolutions
lc_mosaic_500m <- terra::rast("LCC/LCC_amazon_mosaic_500m.tif")
lc_mosaic_250m <- terra::rast("LCC/LCC_amazon_mosaic_250m.tif")
lc_mosaic_100m <- terra::rast("LCC/LCC_amazon_mosaic_100m.tif")

## canopy height (Potapov et al. 2019)
canopy <- terra::rast("C:/Users/immccull/Documents/GLAD_GCH_2019/Forest_height_2019_SAM.tif")

# Roads
departamental <- terra::vect("RAISG/vias2023-1/vias/vias_departamental.shp")
ferrea <- terra::vect("RAISG/vias2023-1/vias/vias_ferrea.shp")
nacional <- terra::vect("RAISG/vias2023-1/vias/vias_nacional.shp")

#### Main program ####
# info on ESA land cover types
# https://esa-worldcover.s3.eu-central-1.amazonaws.com/v200/2021/docs/WorldCover_PUM_V2.0.pdf

# Reproject with roads to CRS of land cover mosaics
nacional <- terra::project(nacional, "EPSG:29172")
departamental <- terra::project(departamental, "EPSG:29172")
ferrea <- terra::project(ferrea, "EPSG:29172")

# not even going to try buffering roads in R; did so in Q
nacional_buff <- terra::vect("RAISG/vias2023-1/vias/buffered/vias_nacional_500m_buff.shp")
departamental_buff <- terra::vect("RAISG/vias2023-1/vias/buffered/vias_departamental_500m_buff.shp")
ferrea_buff <- terra::vect("RAISG/vias2023-1/vias/buffered/vias_ferrea_500m_buff.shp")

# combine roads
# union is way too slow
#test <- terra::union(ferrea_buff, nacional_buff)
#roads_union <- terra::union(test, departamental_buff)
nacional_buff$tipo <- 'nacional'
departamental_buff$tipo <- 'departamental'
ferrea_buff$tipo <- 'ferrea'

roadcolumns <- c('pais','tipo')
roads_union <- rbind(nacional_buff[,roadcolumns],
                     departamental_buff[,roadcolumns],
                     ferrea_buff[,roadcolumns])

#writeVector(roads_union, filename="RAISG/vias2023-1/vias/buffered/roads_buffer_500m_union.shp", overwrite=T)

plot(roads_union)
roads_union$road_field <- 222 #assign some obviously ridiculous number

# Convert roads to raster before integrating with LULC
#roads_raster <- terra::rasterize(roads_union, lc_mosaic_500m, field='road_field', background=NA)
#terra::writeRaster(roads_raster, filename='RAISG/vias2023-1/vias/buffered/roads_buffer_raster_500m.tif', overwrite=T)
roads_raster <- terra::rast("RAISG/vias2023-1/vias/buffered/roads_buffer_raster_500m.tif")

# Mosaic land cover and roads
lc_road_mosaic <- terra::mosaic(lc_mosaic_500m, roads_raster, fun="max") #max: this is why we set road to very high value, so it will supersede any existing land cover value
plot(lc_road_mosaic)

# Reclassify by conductance values
#10: tree cover
#20: shrubland
#30: grassland
#40: cropland
#50: built-up
#60: bare/sparse vegetation
#70: snow and ice
#80: permanent water bodies
#90: herbaceous wetland
#95: mangroves
#100: moss and lichen

m2     <- c(0, NA,
            10, 1000,
            20, 150,
            30, 30,
            40, 30,
            50, NA,
            60, 40,
            70, NA,
            80, 30,
            90, 20,
            95, 500,
            100,2,
            222,50 #roads
)
m2 <- matrix(m2, ncol=2, byrow = T)

lc_re <- classify(lc_road_mosaic, m2, right=T)
amazon_study_area_29172 <- terra::project(amazon_study_area, "EPSG:29172")
lc_re_masked <- terra::mask(lc_re, amazon_study_area_29172, inverse=F) 

## Incorporate canopy height
# dataset is huge, but need to reproject/resample before using it
#amazon_study_area_4326 <- terra::project(amazon_study_area, "EPSG:4326")
lc_mosaic_500m_4326 <- terra::project(lc_mosaic_500m, "EPSG:4326") #just need the extent
canopy_amazon <- terra::crop(canopy, lc_mosaic_500m_4326, filename='tump/canopy_tump.tif', overwrite=T)
ext(canopy_amazon)

# Divide extent of canopy_amazon into 4 quadrants (based on visual subsets; they are not the same size...this is OK; goal is to subdivide to save memory)
a <- ext(-86.0635, -60, -22.3605, -5)
a <- as.polygons(a, crs=crs(canopy_amazon))
b <- ext(-86.0635, -60, -5, 13.0005)
b <- as.polygons(b, crs=crs(canopy_amazon))
c <- ext(-60, -38.05825, -22.3605, -5)
c <- as.polygons(c, crs=crs(canopy_amazon))
d <- ext(-60, -38.05825, -5, 13.0005)
d <- as.polygons(d, crs=crs(canopy_amazon))

# verify what they look like
plot(canopy_amazon)
plot(a, add=T, col='red')
plot(b, add=T, col='blue')
plot(c, add=T, col='gold')
plot(d, add=T, col='green')

# crop canopy_amazon into 4 sections
canopy_amazon_a <- terra::crop(canopy_amazon, a)
canopy_amazon_b <- terra::crop(canopy_amazon, b)
canopy_amazon_c <- terra::crop(canopy_amazon, c)
canopy_amazon_d <- terra::crop(canopy_amazon, d)

# now reproject
canopy_amazon_a_29172 <- terra::project(canopy_amazon_a, "EPSG:29172", method="bilinear", res=c(500,500),
                                        filename='tump/GCH_amazon_29172_500m_a.tif', overwrite=T)
canopy_amazon_b_29172 <- terra::project(canopy_amazon_b, "EPSG:29172", method="bilinear", res=c(500,500),
                                        filename='tump/GCH_amazon_29172_500m_b.tif', overwrite=T)
canopy_amazon_c_29172 <- terra::project(canopy_amazon_c, "EPSG:29172", method="bilinear", res=c(500,500),
                                        filename='tump/GCH_amazon_29172_500m_c.tif', overwrite=T)
canopy_amazon_d_29172 <- terra::project(canopy_amazon_d, "EPSG:29172", method="bilinear", res=c(500,500),
                                        filename='tump/GCH_amazon_29172_500m_d.tif', overwrite=T)

abcd_list <- list(canopy_amazon_a_29172, canopy_amazon_b_29172, canopy_amazon_c_29172, canopy_amazon_d_29172)
canopy_amazon_sprc <- terra::sprc(abcd_list)
#canopy_amazon_mosaic <- terra::mosaic(canopy_amazon_sprc, fun="max", filename="GCH/GCH_amazon_29172_500m.tif", overwrite=T) #fun probably doesn't matter here
#plot(canopy_amazon_mosaic)
#plot(amazon_study_area_29172, add=T)

# this next line won't work due to memory limitations (too many cells, presumably)
# canopy_amazon_29172 <- terra::project(canopy_amazon, "EPSG:29172", method="bilinear", res=c(500,500),
#                                       filename='GCH/GCH_amazon_29172_500m.tif', overwrite=T)
canopy_amazon_29172 <- terra::rast("GCH/GCH_amazon_29172_500m.tif")

# create forest mask
just_forest <- terra::ifel(lc_re ==1000, 1, NA)
plot(just_forest)
#ext(just_forest) <- ext(canopy_amazon_29172)# not needed if resample; need resample to get same number of rows and columns
just_forest <- terra::resample(just_forest, canopy_amazon_29172)
#writeRaster(just_forest, "LCC/LCC_500m_forest.tif", overwrite=T)
canopy_amazon_clamp <- terra::clamp(canopy_amazon_29172, upper=30, values=T) #set everything above 30 m to 30; documentation says heights above 30 are not reliable

# create canopy height modifier
plot(canopy_amazon_clamp)
canopyheight_modifier <- canopy_amazon_clamp/30
plot(canopyheight_modifier)

# keep non-forest areas the same too!                      
final_modifier <- terra::mask(canopyheight_modifier, just_forest) #now modifier lines up with forested areas only
# keep non-forest as they are -> 1 (as we ultimately take the product)
final_modifier <- terra::subst(final_modifier, NA, 1)

# finally, must resample lcc before applying modifier
lc_re_resamp <- terra::resample(lc_re, just_forest)
conductance_final <- lc_re_resamp  * final_modifier
plot(conductance_final)
#writeRaster(conductance_final, filename='Conductance/amazon_conductance_500m.tif', overwrite=T)

