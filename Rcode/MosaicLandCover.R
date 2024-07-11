### Mosaic land cover tiles from European Space Agency
# Date: 6-3-24
# updated:

#### R libraries ####
library(terra)

#### Input data ####
setwd("C:/Users/immccull/Documents/AmazonClimateCorridors")

# ESA land cover documentation: https://worldcover2020.esa.int/documentation
lc_list <- list.files(path="LCC/amazon_tiles", pattern=".tif", full.names=T)
lc_list <- lapply(lc_list, terra::rast)

# study area
study_area <- terra::vect("RAISG/Limites2023/Limites/Lim_Biogeografico.shp")
study_area <- terra::project(study_area, "EPSG:29172")

#### Main program ####
## too slow and not what we really want anyway
#lc_sprc <- terra::sprc(lc_list)
#lc_mosaic <- terra::mosaic(lc_sprc, fun="max", filename="LCC/LCC_amazon_mosaic_10m.tif")

## try reprojecting and resampling to 100m
for (i in 1:length(lc_list)){
  reproj <- terra::project(lc_list[[i]], "EPSG:29172", res=100, method="near",
                           filename=paste0('LCC/LCC_100m/amazon_tile_', i,'_100m.tif'), overwrite=T)
}

lc_list_100m <- list.files(path="LCC/LCC_100m", pattern=".tif", full.names=T)
lc_list_100m <- lapply(lc_list_100m, terra::rast)
lc_sprc_100m <- terra::sprc(lc_list_100m)
lc_mosaic_100m <- terra::mosaic(lc_sprc_100m, fun="max", filename="LCC/LCC_amazon_mosaic_100m.tif")

plot(study_area)
plot(lc_mosaic_100m)
plot(study_area, add=T)

lc_mosaic_100m_mask <- terra::crop(lc_mosaic_100m, study_area, mask=T)
plot(lc_mosaic_100m_mask)
ncell(lc_mosaic_100m_mask)/ncell(lc_mosaic_100m)

# compare cell counts for context
conductance <- terra::rast("Conductance/western_amazon_conductance.tif")
ncell(conductance)/ncell(lc_mosaic_100m)

## try reprojecting and resampling to 250m
for (i in 1:length(lc_list)){
  reproj <- terra::project(lc_list[[i]], "EPSG:29172", res=250, method="near",
                           filename=paste0('LCC/LCC_250m/amazon_tile_', i,'_250m.tif'), overwrite=T)
}

lc_list_250m <- list.files(path="LCC/LCC_250m", pattern=".tif", full.names=T)
lc_list_250m <- lapply(lc_list_250m, terra::rast)
lc_sprc_250m <- terra::sprc(lc_list_250m)
lc_mosaic_250m <- terra::mosaic(lc_sprc_250m, fun="max", filename="LCC/LCC_amazon_mosaic_250m.tif")

plot(lc_mosaic_500m)
plot(study_area, add=T)

lc_mosaic_250m_mask <- terra::crop(lc_mosaic_250m, study_area, mask=T)
plot(lc_mosaic_250m_mask)
ncell(lc_mosaic_250m_mask)/ncell(lc_mosaic_250m)


## try reprojecting and resampling to 500m
for (i in 1:length(lc_list)){
  reproj <- terra::project(lc_list[[i]], "EPSG:29172", res=500, method="near",
                           filename=paste0('LCC/LCC_500m/amazon_tile_', i,'_500m.tif'), overwrite=T)
}

lc_list_500m <- list.files(path="LCC/LCC_500m", pattern=".tif", full.names=T)
lc_list_500m <- lapply(lc_list_500m, terra::rast)
lc_sprc_500m <- terra::sprc(lc_list_500m)
lc_mosaic_500m <- terra::mosaic(lc_sprc_500m, fun="max", filename="LCC/LCC_amazon_mosaic_500m.tif")

plot(lc_mosaic_500m)
plot(study_area, add=T)

lc_mosaic_500m_mask <- terra::crop(lc_mosaic_500m, study_area, mask=T)
plot(lc_mosaic_500m_mask)
ncell(lc_mosaic_500m_mask)/ncell(lc_mosaic_500m)
