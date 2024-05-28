##################### Create Amazon conductance surface ###########################
# Date: 4-9-24
# updated: 4-15-24
# Author: Ian McCullough, immccull@gmail.com
###################################################################################

#### R libraries ####
library(terra)

#### Input data ####
setwd("C:/Users/immccull/Documents/Amazon")

# Land cover
lcc <- terra::rast("LCC/LCC_western_amazon_compressed.tif")

# Study area
amazon_study_area <- terra::vect("RAISG/StudyArea/western_Amazon_study_area1.shp")

## canopy height (Potapov et al. 2019)
canopy <- terra::rast("C:/Users/immccull/Documents/GLAD_GCH_2019/Forest_height_2019_SAM.tif")

# Roads
departamental <- terra::vect("RAISG/vias2023-1/vias/vias_departamental.shp")
ferrea <- terra::vect("RAISG/vias2023-1/vias/vias_ferrea.shp")
nacional <- terra::vect("RAISG/vias2023-1/vias/vias_nacional.shp")

# 12 degree grid for splitting up data
#grid12deg <- terra::vect("tump/grid12deg.shp") # too big!
#grid10deg <- terra::vect("tump/grid10deg.shp")
#grid5deg <- terra::vect("tump/grid5deg.shp")

#### Main program ####
amazon_study_area_29172 <- terra::project(amazon_study_area, "EPSG:29172")
amazon_study_area_4326 <- terra::project(amazon_study_area, "EPSG:4326")
#writeVector(amazon_study_area_29172, filename='RAISG/StudyArea/western_Amazon_study_area1_29172.shp')


#see Table 3 for description of LULC types: 
# https://esa-worldcover.s3.eu-central-1.amazonaws.com/v200/2021/docs/WorldCover_PUM_V2.0.pdf

## Deal with roads 
nacional <- terra::project(nacional, "EPSG:29172")
departamental <- terra::project(departamental, "EPSG:29172")
ferrea <- terra::project(ferrea, "EPSG:29172")

nacional_clip <- terra::intersect(nacional, amazon_study_area_29172)
ferrea_clip <- terra::intersect(ferrea, amazon_study_area_29172)
departamental_clip <- terra::intersect(departamental, amazon_study_areaamazon_study_area_29172)

# Must reproject to a CRS that uses meters before buffering in Q
# nacional_clip_29172 <- terra::project(nacional_clip, "EPSG:29172")
# departamental_clip_29172 <- terra::project(departamental_clip, "EPSG:29172")
#ferrea_clip_29172 <- terra::project(ferrea_clip, "EPSG:29172")

# need to buffer in Q (R crashes when try terra::buffer on such a large dataset)
#writeVector(nacional_clip, "RAISG/vias2023-1/vias/study_area/vias_nacional_clip_29172.shp", overwrite=T)
#writeVector(departamental, "RAISG/vias2023-1/vias/study_area/vias_departamental_clip_29172.shp", overwrite=T)
#writeVector(ferrea_clip, "RAISG/vias2023-1/vias/study_area/vias_ferrea_clip_29172.shp", overwrite=T)

#nacional_clip_buffer <- terra::buffer(nacional_clip_29172, width=250) #don't run; crashes R
ferrea_clip_buffer <- terra::buffer(ferrea_clip_29172, width=250) #function uses meters even for a degree unit CRS
#departamental_clip_buffer <- terra::buffer(departamental_clip_29172, width=125) #don't run; crashes R

# these were buffered in Q (buffer tool, 250m distance)
nacional_clip_buffer <- terra::vect("RAISG/vias2023-1/vias/study_area/vias_nacional_clip_29172_buff250m.shp")
departamental_clip_buffer <- terra::vect("RAISG/vias2023-1/vias/study_area/vias_departamental_clip_29172_buff250m.shp")

test <- terra::union(ferrea_clip_buffer, nacional_clip_buffer)
roads_union <- terra::union(test, departamental_clip_buffer)
#writeVector(roads_union, filename="RAISG/vias2023-1/vias/study_area/roads_buffer_union.shp", overwrite=T)

plot(roads_union)
roads_union$road_field <- 222 #assign some obviously ridiculous number

# Convert roads to raster before integrating with LULC
#roads_raster <- terra::rasterize(roads_union, lcc_re_masked_100m_29172, field='road_field', background=NA)
#terra::writeRaster(roads_raster, filename='RAISG/vias2023-1/vias/study_area/roads_buffer_raster.tif', overwrite=T)
roads_raster <- terra::rast("RAISG/vias2023-1/vias/study_area/roads_buffer_raster.tif")


## Resample land cover
roads_raster_4326 <- terra::project(roads_raster, "EPSG:4326") #need this to resample LULC
#lcc_100m <- terra::resample(lcc, roads_raster_4326, method='near', 
#                            filename='LCC/lcc_100m.tif', overwrite=T)
lcc_100m <- terra::rast("LCC/lcc_100m.tif")
#lcc_100m_29172 <- terra::project(lcc_100m, "EPSG:29172", filename='LCC/lcc_100m_29172.tif', overwrite=T,
#                                 res=c(100,100))
lcc_100m_29172 <- terra::rast("LCC/lcc_100m_29172.tif")

# need rasters to have same extent and resolution
lcc_100m_29172_aligned <- terra::resample(lcc_100m_29172, roads_raster)
#writeRaster(lcc_100m_29172_aligned, filename='LCC/lcc_100m_29172_westernAmazon.tif')

##
# mosaic in roads
# use max function so road supersedes other land covers with its very high 222 number
lcc_road_mosaic <- terra::mosaic(lcc_100m_29172_aligned, roads_raster, fun="max")

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

# Reclassify the full thing
#lcc_re <- classify(lcc, m2, right=T)
lcc_re <- classify(lcc_road_mosaic, m2, right=T)
amazon_study_area_4326 <- terra::project(amazon_study_area, "EPSG:4326")
lcc_re_masked <- terra::mask(lcc_re, amazon_study_area_29172, inverse=F) 
#writeRaster(lcc_re_masked, filename='tump/lcc_re_masked.tif', overwrite=T)
#lcc_re_masked <- terra::rast('tump/lcc_re_masked.tif')


# try dividing up data
# still too big
# north <- grid12deg[c(1,3),]
# north <- terra::aggregate(north)
# north <- terra::project(north, "EPSG:4326")
# plot(north)

# grid5deg_4326 <- terra::project(grid5deg, "EPSG:4326")
# q1 <- grid5deg_4326[c(8),]
# test_mask <- terra::crop(lcc_re_masked, q1, mask=T)
# test_mask_29172 <- terra::project(test_mask, "EPSG:29172", method="bilinear", res=c(100,100),
#                                   filename='tump/test5deg.tif')

#lcc_re_masked_north <- terra::mask(lcc_re_masked, north, filename='lcc_re_masked_north.tif')
#lcc_re_masked_north <- terra::rast('lcc_re_masked_north.tif')
#lcc_re_masked_north_29172 <- terra::project(lcc_re_masked_north, "EPSG:29172", filename='LCC/lcc_re_masked_north_29172.tif')

## Integrate canopy height
# dataset is huge, but need to reproject/resample before using it
canopy_amazon <- terra::crop(canopy, amazon_study_area_4326, mask=T)
#canopy_amazon_29172 <- terra::project(canopy_amazon, "EPSG:29172", method="bilinear", res=c(100,100),
#                                      filename='GCH/GCH_clip_29172.tif', overwrite=T)
canopy_amazon_29172 <- terra::rast("GCH/GCH_clip_29172.tif")

# create forest mask
just_forest <- terra::ifel(lcc_re_masked ==1000, 1, NA)
plot(just_forest)
#ext(just_forest) <- ext(canopy_amazon_29172)# not needed if resample; need resample to get same number of rows and columns
just_forest <- terra::resample(just_forest, canopy_amazon_29172)
#writeRaster(just_forest, "LCC/LCC_100m_forest.tif")
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
lcc_re_masked_resamp <- terra::resample(lcc_re_masked, just_forest)
conductance_final <- lcc_re_masked_resamp  * final_modifier
plot(conductance_final)
writeRaster(conductance_final, filename='Conductance/western_amazon_conductance.tif', overwrite=T)
