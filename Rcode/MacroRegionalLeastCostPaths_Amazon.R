######################## Amazon least cost paths ##################################
# Date: 10-23-24
# updated: 8-22-25; recalculate highland habitat beginning at 2000m
# Author: Ian McCullough, immccull@gmail.com
###################################################################################

#### R libraries ####
library(terra)
library(leastcostpath)
library(sf)

#### Input data ####
setwd("C:/Users/immccull/Documents/AmazonClimateCorridors")

# regions
#morrone2022_4674_amazon <- terra::vect("Regions/Morrone2022/Morrone2022_amazon_4674.shp")
#morrone2022_amazon <- terra::project(morrone2022_4674_amazon, "EPSG:29172")
morrone_2022_neotropics <- terra::vect("Regions/Morrone2022/NeotropicMap_SIRGAS2000.shp")
morrone_2022_neotropics <- terra::project(morrone_2022_neotropics, "EPSG:29172")
morrone_2022_amazonclip <- terra::vect("Regions/Morrone2022/Amazon_macroregions_fixedgeom_Amazonclip.shp")

chacoan <- terra::vect("Regions/Morrone2022/chacoan.shp")
boreal <- terra::vect("Regions/Morrone2022/boreal.shp")
south <- terra::vect("Regions/Morrone2022/south.shp")

# Forest landscape integrity index (downloaded Aug 2025) for S America
# Source: https://www.nature.com/articles/s41467-020-19493-3#Sec4
#FLII_SA <- terra::rast("FLII/flii_SouthAmerica.tif")
#plot(FLII_SA)
#FLII_SA_high <- terra::ifel(FLII_SA >= 9600, 1, NA)
#plot(FLII_SA_high)
#writeRaster(FLII_SA_high, "FLII/FLII_SA_highintegrity.tif", overwrite=T)
FLII_SA_high <- terra::rast("FLII/FLII_SA_highintegrity.tif")

# # update 11-4-24: create 3 regional subsets
# chacoan <- subset(morrone_2022_neotropics, morrone_2022_neotropics$Provincias=="Xingu-Tapajos province")
# #terra::writeVector(chacoan, filename='Regions/Morrone2022/chacoan.shp', overwrite=T)
# boreal <- subset(morrone_2022_neotropics, morrone_2022_neotropics$Provincias %in% 
#                    c('Guianan Lowlands province','Guianan province',
#                      'Imeri province','Napo province','Para province',
#                      'Roraima province'))
# #terra::writeVector(boreal, filename='Regions/Morrone2022/boreal.shp', overwrite=T)
# south <- subset(morrone_2022_neotropics, morrone_2022_neotropics$Provincias %in% 
#                   c('Madeira province','Rondonia province',
#                     'Ucayali province','Yungas province'))
# #terra::writeVector(south, filename='Regions/Morrone2022/south.shp', overwrite=T)

# Amazon study area
amazon_study_area <- terra::vect("RAISG/Limites2023/Limites/Lim_Biogeografico.shp")
amazon_study_area <- terra::project(amazon_study_area, "EPSG:29172")
amazon_study_area_4326 <- terra::project(amazon_study_area, "EPSG:4326")
south <- terra::intersect(south, amazon_study_area) #some is outside Amazon basin

plot(amazon_study_area)
plot(chacoan, add=T, col='dodgerblue')
plot(boreal, add=T, col='forestgreen')
plot(south, add=T, col='gold')

# Conductance surface (modified based on canopy height)
#conductance <- terra::rast("Conductance/amazon_conductance_250m.tif")
conductance <- terra::rast("Conductance/amazon_conductance_500m.tif")

# DEM
#DEM_4674_morrone <- terra::rast("DEM/SA_DEM_mosaic_4674_morrone.tif")
#DEM <- terra::rast("DEM/SA_DEM_mosaic.tif")
DEM_studyarea_29172 <- terra::rast("DEM/buffered100km/DEM_amazon_100kmbuff.tif")
DEM15s <- terra::rast("DEM/rm470dn6126/data_EPSG_4326/sa_dem_15s.tif")

# protected areas (dissolved complexes of 10km2 or more individual reserves)
protected_areas <- terra::vect("protected_areas/Amazon_merged_PAs/Amazon_merged_PAs_dissolved10km_wRegion.shp")
protected_areas <- terra::project(protected_areas, "EPSG:29172")

#### Main program ####
## start and end nodes

# first need to crop DEM to dominions and reproject
dominions_merged <- terra::vect(c(boreal, south, chacoan))
dominions_merged_dissolved <- terra::aggregate(dominions_merged)
dominions_merged_dissolved_100kmbuff <- terra::buffer(dominions_merged_dissolved, width=100000)

plot(dominions_merged_dissolved_100kmbuff)
dominions_merged_dissolved_100kmbuff_4326 <- terra::project(dominions_merged_dissolved_100kmbuff, "EPSG:4326")
#DEM_studyarea <- terra::crop(DEM, dominions_merged_dissolved_100kmbuff_4326)
#plot(DEM_studyarea)
#DEM_studyarea_29172 <- terra::project(DEM_studyarea, "EPSG:29172", method="bilinear",
#                                      filename="DEM/buffered100km/DEM_amazon_100kmbuff.tif", overwrite=T)

#DEM15s_studyarea <- terra::crop(DEM15s, dominions_merged_dissolved_100kmbuff_4326)
#plot(DEM15s_studyarea)
#DEM15s_studyarea_29172 <- terra::project(DEM15s_studyarea, "EPSG:29172", method="bilinear",
#                                         filename="DEM/buffered100km/DEM15s_amazon_100kmbuff.tif", overwrite=T)


# identify lowland and highland elevations
#lowland <- terra::ifel(DEM_studyarea_29172 <= 500, 1, NA)
#highland <- terra::ifel(DEM_studyarea_29172 <= 3500 & DEM_studyarea_29172 >= 2500, 1, NA)
#lowland <- terra::ifel(DEM15s_studyarea_29172 <= 500, 1, NA)
#highland <- terra::ifel(DEM15s_studyarea_29172 <= 3500 & DEM15s_studyarea_29172 >= 2500, 1, NA)

# update Aug 2025: redefine lower limit of highland as 2000 m
#highland <- terra::ifel(DEM_studyarea_29172 <= 3500 & DEM_studyarea_29172 >= 2000, 1, NA)
highland <- terra::rast("DEM/highland_all_90m_29172_2000m.tif")

#terra::writeRaster(highland, "DEM/highland_all_90m_29172.tif", overwrite=T)
# save new highland layer with 2000 m lower limit
#terra::writeRaster(highland, "DEM/highland_all_90m_29172_2000m.tif", overwrite=T)

# identify those elevations within protected areas
#lowland_protected <- terra::mask(lowland, protected_areas, inverse=F, filename='start_nodes/dominions/lowland.tif', overwrite=T)
#highland_protected <- terra::mask(highland, protected_areas, inverse=F, filename='end_nodes/dominions/highland.tif', overwrite=T)
lowland_protected <- terra::rast("start_nodes/dominions/lowland.tif")
#highland_protected <- terra::rast("end_nodes/dominions/highland.tif")
#lowland_protected <- terra::crop(lowland, protected_areas, mask=T, filename='start_nodes/dominions/lowland15s.tif', overwrite=T)
#highland_protected <- terra::crop(highland, protected_areas, mask=T, filename='end_nodes/dominions/highland15s.tif', overwrite=T)

#highland_protected <- terra::mask(highland, protected_areas, inverse=F, filename='end_nodes/dominions/highland_protected2K.tif', overwrite=T)
highland_protected <- terra::rast("end_nodes/dominions/highland_protected2K.tif")

# what if resample 90 m DEM lowland and highland to coarser res?
# 10.54637 = 1000/94.81933, but fact argument only accepts integers
# fun does not really matter because all values are 1, so used mean
#lowland_protected1km <- terra::aggregate(lowland_protected, fact=11, fun="mean")

# it is possible that polygon conversion fails because each cell becomes a unique polygon
# therefore, aggregating first into contiguous "patches" maybe could address that
# but this will likely be very slow, so try if the QGIS spatial join fails
#lowland_protected1km_patches <- terra::patches(lowland_protected1km, directions=8, allowGaps=F,
#                                               filename='start_nodes/dominions/lowland1km_patches.tif', overwrite=T)

# convert protected lowland and highland elevations to polygon
# does not work: Error: [as.polygons] the raster is too large
# try doing in in QGIS, but that crashed
#lowland_protected_polygons <- terra::as.polygons(lowland_protected, aggregate=F, filename='start_nodes/dominions/lowland_polygons.shp')
#highland_protected_polygons <- terra::as.polygons(highland_protected, aggregate=F, filename='end_nodes/dominions/highland_polygons.shp')

# trying breaking up into regions
lowland_protected_boreal <- terra::crop(lowland_protected, boreal, mask=T, filename='start_nodes/dominions/lowland_protected_boreal.tif', overwrite=T)
lowland_protected_south <- terra::crop(lowland_protected, south, mask=T, filename='start_nodes/dominions/lowland_protected_south.tif', overwrite=T)
lowland_protected_chacoan <- terra::crop(lowland_protected, chacoan, mask=T, filename='start_nodes/dominions/lowland_protected_chacoan.tif', overwrite=T)

## How much highland habitat is in each region?
# From QGIS zonal histogram (used 90m DEM)
highland_protected_ZN <- terra::vect("end_nodes/dominions/highland_protected_zonalhist.shp")
highland_protected_ZN$areasqkm <- terra::expanse(highland_protected_ZN, "km")
highland_protected_ZN <- as.data.frame(highland_protected_ZN)
highland_protected_ZN$highland_protected_pct <- ((highland_protected_ZN$HISTO_1*0.0081)/highland_protected_ZN$areasqkm)*100

# Do the same for all highland habitat (includes unprotected)
highland_all_ZN <- terra::vect("end_nodes/dominions/highland_all_zonalhist.shp")
highland_all_ZN$areasqkm <- terra::expanse(highland_all_ZN, "km")
highland_all_ZN <- as.data.frame(highland_all_ZN)
highland_all_ZN$highland_all_pct <- ((highland_all_ZN$HISTO_1*0.0081)/highland_all_ZN$areasqkm)*100

# update Aug 2025: with all highland habitat using 2000 m as lower elevational threshold
options(scipen = 999)
highland_all_ZN_2K <- terra::vect("end_nodes/dominions/highland_all_zonalhist_2000m.shp")
highland_all_ZN_2K$areasqkm <- terra::expanse(highland_all_ZN_2K, "km")
highland_all_ZN_2K <- as.data.frame(highland_all_ZN_2K)
highland_all_ZN_2K$highland_all_pct <- ((highland_all_ZN_2K$HISTO_1*0.0081)/highland_all_ZN_2K$areasqkm)*100

highland_protected_ZN_2K <- terra::vect("end_nodes/dominions/highland_protected_zonalhist_2000m.shp")
highland_protected_ZN_2K$areasqkm <- terra::expanse(highland_protected_ZN_2K, "km")
highland_protected_ZN_2K <- as.data.frame(highland_protected_ZN_2K)
highland_protected_ZN_2K$highland_protected_pct <- ((highland_protected_ZN_2K$HISTO_1*0.0081)/highland_protected_ZN_2K$areasqkm)*100


# Compare % highland habitat across the 3 above datasets
comp <- cbind.data.frame(highland_protected_ZN[,c(1,14)],
                         highland_all_ZN[,c(1,14)],
                         highland_all_ZN_2K[,c(1,14)],
                         highland_protected_ZN_2K[,c(1,14)])
comp <- comp[,c(1,2,4,6,8)]
names(comp) <- c('Provincias','highland_protected_2500m','highland_all_2500m','highland_all_2000m','highland_protected_2000m')

# prepare for export
highland_protected_exp <- highland_protected_ZN[,c('Provincias','areasqkm','highland_protected_pct')]
highland_all_exp <- highland_all_ZN[,c('Provincias','areasqkm','highland_all_pct')]

names(highland_protected_exp) <- c('Province','highland_protected_areasqkm','highland_protected_pct')
names(highland_all_exp) <- c('Province','highland_all_areasqkm','highland_all_pct')
highland_exp <- merge(highland_protected_exp, highland_all_exp, by='Province')
#write.csv(highland_exp, file="Regions/Morrone2022/Morrone2022_regions_highland25003500m_stats.csv")
#write.csv(comp, file="Regions/Morrone2022/Morrone2022_regions_highland20003500m_stats.csv")

# still too big
#lowland_protected_boreal_polygons <- terra::as.polygons(lowland_protected_boreal, aggregate=F, filename='start_nodes/dominions/lowland_protected_boreal_polygons.shp')

## Perhaps can iteratively convert to polygons by protected area complex
# loop fails if there is no lowland area within a PA; can first extract only PAs that contain lowland
# warning: this approach with extract is slow at original resolution
# seemed to get hung up on patch aggregation or polygon conversion
# decided that coarsening was better than aggregating everything into a single patch/polygon
# steps below before the loop are fast at coarser res
# fact argument in terra::aggregate only accepts integers, so result is 1043 m res
lowland_protected1km <- terra::aggregate(lowland_protected, fact=11, fun="mean")
protected_areas_wLowland <- terra::extract(lowland_protected1km, protected_areas, fun="mean", na.rm=T)
protected_areas_wLowland_sub <- subset(protected_areas_wLowland, sa_dem_0 >=1)
protected_areas_wLowland_shp <- subset(protected_areas, protected_areas$PAcomplexI %in% protected_areas_wLowland_sub$ID)

#for (i in 1:nrow(protected_areas)){
for (i in 1:nrow(protected_areas_wLowland_shp)){
  # extract PA of interest 
  test_PA <- protected_areas_wLowland_shp[i]
  # extract lowland area within PA of interest
  test_PA_lowland <- terra::crop(lowland_protected, test_PA, mask=T)
  # aggregate lowland area within PA of interest into contiguous patches
  # edit: too slow, so removed this step
  # otherwise, conversion to polygons makes each cell a single polygon
  #test_PA_lowland_patches <- terra::patches(test_PA_lowland, directions=8)
  # convert contiguous patches to polygon
  test_PA_lowland_polygons <- as.polygons(test_PA_lowland, aggregate=T, na.rm=T)
  # calculate area of polygons to identify largest within PA of interest
  test_PA_lowland_polygons_df <- as.data.frame(test_PA_lowland_polygons)
  test_PA_lowland_polygons_df$lowland_areasqkm <- terra::expanse(test_PA_lowland_polygons, "km")
  # removing step that identified largest lowland patch within PA; too slow, so went with single patch for whole PA
  #test_PA_lowland_polygons_df$lowland_arearank <- rank(-test_PA_lowland_polygons_df$lowland_areasqkm)
  test_PA_lowland_polygons_df$dominion <- test_PA$IDDominio
  test_PA_lowland_polygons_df$province <- test_PA$Provincias
  test_PA_lowland_polygons_df$PAcomplexID <- test_PA$PAcomplexI
  #test_PA_start_polygon <- merge(test_PA_lowland_polygons, test_PA_lowland_polygons_df, by='patches')
  #test_PA_start_polygon <- subset(test_PA_start_polygon, test_PA_start_polygon$arearank==1)
  test_PA_start_polygon <- cbind(test_PA_lowland_polygons, test_PA_lowland_polygons_df)[,c(3:6)]
  test_PA_start_point <- terra::centroids(test_PA_start_polygon, inside=T)
  polygon_name <- paste0("start_nodes/dominions/start_polygons/startpolygon_PAcomplexID_", test_PA_start_point$PAcomplexID, '.shp')
  point_name <- paste0("start_nodes/dominions/start_points/startpoint_PAcomplexID_", test_PA_start_point$PAcomplexID, '.shp')
  terra::writeVector(test_PA_start_polygon, filename=polygon_name, overwrite=T)
  terra::writeVector(test_PA_start_point, filename=point_name, overwrite=T)
}

## Aggregate start node points and polygons into single respective shapefile
start_pt_list <- list.files(path='C:/Users/immccull/Documents/AmazonClimateCorridors/start_nodes/dominions/start_points', pattern='.shp', full.names=T)
start_pt_list <- lapply(start_pt_list, terra::vect)
x <- terra::vect(start_pt_list)
x <- terra::intersect(x, morrone_2022_amazonclip) #only use PAs in study regions
xIDs <- x$PAcomplexI
#writeVector(x, filename='start_nodes/dominions/start_points/combined_start_points/combined_start_points.shp', overwrite=T)

start_poly_list <- list.files(path='C:/Users/immccull/Documents/AmazonClimateCorridors/start_nodes/dominions/start_polygons', pattern='.shp', full.names=T)
start_poly_list <- lapply(start_poly_list, terra::vect)
xx <- terra::vect(start_poly_list)
xx <- subset(xx, xx$PAcomplexI %in% xIDs)
#writeVector(xx, filename='start_nodes/dominions/start_polygons/combined_start_polygons/combined_start_polygons.shp', overwrite=T)

## repeat for end nodes
highland_protected1km <- terra::aggregate(highland_protected, fact=11, fun="mean")
protected_areas_whighland <- terra::extract(highland_protected1km, protected_areas, fun="mean", na.rm=T)
protected_areas_whighland_sub <- subset(protected_areas_whighland, sa_dem_0 >=1)
protected_areas_whighland_shp <- subset(protected_areas, protected_areas$PAcomplexI %in% protected_areas_whighland_sub$ID)

#for (i in 1:nrow(protected_areas)){
for (i in 1:nrow(protected_areas_whighland_shp)){
  # extract PA of interest 
  test_PA <- protected_areas_whighland_shp[i]
  # extract highland area within PA of interest
  test_PA_highland <- terra::crop(highland_protected, test_PA, mask=T)
  # aggregate highland area within PA of interest into contiguous patches
  # edit: too slow, so removed this step
  # otherwise, conversion to polygons makes each cell a single polygon
  #test_PA_highland_patches <- terra::patches(test_PA_highland, directions=8)
  # convert contiguous patches to polygon
  test_PA_highland_polygons <- as.polygons(test_PA_highland, aggregate=T, na.rm=T)
  # calculate area of polygons to identify largest within PA of interest
  test_PA_highland_polygons_df <- as.data.frame(test_PA_highland_polygons)
  test_PA_highland_polygons_df$highland_areasqkm <- terra::expanse(test_PA_highland_polygons, "km")
  # removing step that identified largest highland patch within PA; too slow, so went with single patch for whole PA
  #test_PA_highland_polygons_df$highland_arearank <- rank(-test_PA_highland_polygons_df$highland_areasqkm)
  test_PA_highland_polygons_df$dominion <- test_PA$IDDominio
  test_PA_highland_polygons_df$province <- test_PA$Provincias
  test_PA_highland_polygons_df$PAcomplexID <- test_PA$PAcomplexI
  #test_PA_end_polygon <- merge(test_PA_highland_polygons, test_PA_highland_polygons_df, by='patches')
  #test_PA_end_polygon <- subset(test_PA_end_polygon, test_PA_end_polygon$arearank==1)
  test_PA_end_polygon <- cbind(test_PA_highland_polygons, test_PA_highland_polygons_df)[,c(3:6)]
  test_PA_end_point <- terra::centroids(test_PA_end_polygon, inside=T)
  polygon_name <- paste0("end_nodes/dominions/end_polygons/endpolygon_PAcomplexID_", test_PA_end_point$PAcomplexID, '.shp')
  point_name <- paste0("end_nodes/dominions/end_points/endpoint_PAcomplexID_", test_PA_end_point$PAcomplexID, '.shp')
  terra::writeVector(test_PA_end_polygon, filename=polygon_name, overwrite=T)
  terra::writeVector(test_PA_end_point, filename=point_name, overwrite=T)
}

## Aggregate end node points and polygons into single respective shapefile
end_pt_list <- list.files(path='C:/Users/immccull/Documents/AmazonClimateCorridors/end_nodes/dominions/end_points', pattern='.shp', full.names=T)
end_pt_list <- lapply(end_pt_list, terra::vect)
x <- terra::vect(end_pt_list)
#writeVector(x, filename='end_nodes/dominions/end_points/combined_end_points/combined_end_points.shp', overwrite=T)

end_poly_list <- list.files(path='C:/Users/immccull/Documents/AmazonClimateCorridors/end_nodes/dominions/end_polygons', pattern='.shp', full.names=T)
end_poly_list <- lapply(end_poly_list, terra::vect)
xx <- terra::vect(end_poly_list)
#writeVector(xx, filename='end_nodes/dominions/end_polygons/combined_end_polygons/combined_end_polygons.shp', overwrite=T)

# Aug 2025 update: use highland area defined as 2000-3500m, regardless of protection
# but only using areas of high forest landscape integrity
highland_amazon <- terra::crop(highland, dominions_merged_dissolved_100kmbuff, mask=T)
plot(highland_amazon)

# reduce FLII data before reprojecting
FLII_SA_high_amazon <- terra::crop(FLII_SA_high, amazon_study_area_4326, mask=T)
plot(amazon_study_area_4326)
plot(FLII_SA_high_amazon, add=T)

# reproject to same CRS as highland data
FLII_amazon_high_29172 <- terra::project(FLII_SA_high_amazon, "EPSG:29172", method="near",
                                         res=300, filename="FLII/FLII_amazon_high_29172.tif", overwrite=T)
#original resolution was 300m; figured nearest neighbor resampling was OK because we are effectively using categorical data (high integrity only)

# need extents to match, so try converting to polygon so can use as mask
test <- terra::as.polygons(FLII_amazon_high_29172, aggregate=T, na.rm=T)
FLII_amazon_high_29172_clip <- terra::crop(highland_amazon, test, mask=T)

# check:
plot(amazon_study_area)
plot(highland_amazon, add=T)
plot(FLII_amazon_high_29172_clip, add=T, col='orange')

highland_amazon_highFLII_patches <- terra::patches(FLII_amazon_high_29172_clip, directions=8, allowGaps=F)
highland_amazon_highFLII_polygons <- as.polygons(highland_amazon_highFLII_patches, aggregate=T, na.rm=T)
#writeVector(highland_amazon_highFLII_polygons, "end_nodes/dominions/end_polygons/combined_end_polygons/combined_end_polygons_2K.shp")

highland_amazon_highFLII_centers <- terra::centroids(highland_amazon_highFLII_polygons, inside=T)
#writeVector(highland_amazon_highFLII_centers, "end_nodes/dominions/end_points/combined_end_points/combined_end_points_2K.shp")


# # testing loop operations
# # fails if there is no lowland area within a PA; can first extract only PAs with lowland?
# # warning: this approach is slow without coarsening resolution
# extract lowland area within PA of interest
# since we are interested in the biggest area anyway, maybe OK to coarsen to speed up processing
# # fact argument only accepts integers, so resulting res is 1043 m
# lowland_protected1km <- terra::aggregate(lowland_protected, fact=11, fun="mean")
# protected_areas_wLowland <- terra::extract(lowland_protected1km, protected_areas, fun="mean", na.rm=T)
# protected_areas_wLowland_sub <- subset(protected_areas_wLowland, sa_dem_0 >=1)
# protected_areas_wLowland_shp <- subset(protected_areas, protected_areas$PAcomplexI %in% protected_areas_wLowland_sub$ID)
# 
# # extract PA of interest
# test_PA <- protected_areas[484] #100 fails because PA has no lowland
# test_PA_lowland <- terra::crop(lowland_protected1km, test_PA, mask=T)
# # aggregate lowland area within PA of interest into contiguous patches
# # this part might really be slowing the process down, so could consider skipping it with as.polygons aggregate=T, but that will make just one big polygon
# test_PA_lowland_patches <- terra::patches(test_PA_lowland, directions=8)
# # convert contiguous patches to polygon
# test_PA_lowland_polygons <- as.polygons(test_PA_lowland_patches, aggregate=F, na.rm=T)
# # calculate area of polygons to identify largest within PA of interest
# test_PA_lowland_polygons_df <- as.data.frame(test_PA_lowland_polygons)
# test_PA_lowland_polygons_df$lowland_areasqkm <- terra::expanse(test_PA_lowland_polygons, "km")
# test_PA_lowland_polygons_df$lowland_arearank <- rank(-test_PA_lowland_polygons_df$lowland_areasqkm)
# test_PA_lowland_polygons_df$dominion <- test_PA$IDDominio
# test_PA_lowland_polygons_df$province <- test_PA$Provincias
# test_PA_lowland_polygons_df$PAcomplexID <- test_PA$PAcomplexI
# test_PA_start_polygon <- merge(test_PA_lowland_polygons, test_PA_lowland_polygons_df, by='patches')
# test_PA_start_polygon <- subset(test_PA_start_polygon, test_PA_start_polygon$lowland_arearank==1)
# test_PA_start_point <- terra::centroids(test_PA_start_polygon, inside=T)
# polygon_name <- paste0("start_polygons/startpolygon_PAcomplexID_", test_PA_start_point$PAcomplexID, '.shp')
# point_name <- paste0("start_points/startpoint_PAcomplexID_", test_PA_start_point$PAcomplexID, '.shp')
#
# plot(test_PA)
# plot(test_PA_lowland_polygons, add=T, col='gold')
# plot(test_PA_start_polygon, add=T, col='orange')
# plot(test_PA_start_point, add=T, col='red')

#### Least cost paths ####
start_points <- terra::vect("start_nodes/dominions/start_points/combined_start_points/combined_start_points.shp")
#end_points <- terra::vect("end_nodes/dominions/end_points/combined_end_points/combined_end_points.shp")
# with new 2000m lower elevation threshold, high forest integrity areas
end_points <- terra::vect("end_nodes/dominions/end_points/combined_end_points/combined_end_points_2K.shp")

start_points$start_ID <- paste0("start_ID_", seq(1,nrow(start_points)))
end_points$end_ID <- paste0("end_ID_", seq(1,nrow(end_points)))

#also need to remove start nodes not in conductance surface area (likely in the water)
# no such end nodes because they are far inland
test <- terra::extract(conductance, start_points) 
test2 <- terra::extract(conductance, end_points)#none
names(test) <- c('ID','cond')
nas <- which(is.na(test), arr.ind=T)[,1] #get row IDs of NAs
pts <- test[-c(nas), ]
start_points$rowID <- seq(1,nrow(start_points),1)
start_points <- subset(start_points, !(start_points$rowID %in% pts$ID))

#plot(conductance)
#plot(amazon_study_area, add=T)
#plot(morrone_2022_amazonclip, add=T)
#plot(start_points, add=T, col='black', pch=20)
#plot(end_points, add=T, col='blue', pch=20)
#plot(protected_areas, add=T, col='gold')

# divide up by region
table(as.data.frame(start_points$IDDominio))
start_points_boreal <- terra::intersect(start_points, boreal)
#end_points_boreal <- terra::intersect(end_points, boreal)
plot(boreal)
plot(start_points_boreal, add=T, col='red')

start_points_south <- terra::intersect(start_points, south)
start_points_chacoan <- terra::intersect(start_points, chacoan)

# this plot shows end points are outside our 3 focal regions
plot(morrone_2022_amazonclip)
plot(start_points_boreal, add=T, col='forestgreen')
plot(start_points_south, add=T, col='gold')
plot(start_points_chacoan, add=T, col='orange')
plot(end_points, add=T, col='dodgerblue')

# buffer regions by 100km
boreal_buffer100km <- terra::buffer(boreal, width=100000)
south_buffer100km <- terra::buffer(south, width=100000)
chacoan_buffer100km <- terra::buffer(chacoan, width=100000)
allregions_buffer100km <- terra::buffer(morrone_2022_amazonclip, width=100000)
# extract conductance surface for the buffered area
conductance_borealbuff <- terra::crop(conductance, boreal_buffer100km, mask=T)
plot(conductance_borealbuff)
plot(boreal, add=T)

conductance_southbuff <- terra::crop(conductance, south_buffer100km, mask=T)
plot(conductance_southbuff)
plot(south, add=T)

conductance_chacoanbuff <- terra::crop(conductance, chacoan_buffer100km, mask=T)
plot(conductance_chacoanbuff)
plot(chacoan, add=T)

conductance_allbuff <- terra::crop(conductance, allregions_buffer100km, mask=T)
plot(conductance_allbuff)
plot(morrone_2022_amazonclip, add=T)

# Calculate full conductance matrix (may strain memory)
# This is commented out so we can speed up processing by calculating
# conductance matrix for necessary area around nodes
# buffered condmat has some end nodes cut off
#condmat <- create_cs(conductance_allbuff, neighbours=8, dem=NULL, max_slope=NULL)

#condmat <- create_cs(conductance, neighbours=8, dem=NULL, max_slope=NULL)

## Aug 2025 notes
# still calculating LCPs by region and not using full conductance matrix 
# to speed up processing
# changed output file names to remove end node PAcomplexID
# because some end nodes are not in protected areas
# can later intersect end node points with PAs

## Least cost paths for boreal region
#condmat_boreal <- create_cs(conductance_borealbuff, neighbours=8, dem=NULL, max_slope=NULL)
lcp_neighbors <- 5
knear <- as.data.frame(terra::nearby(x=start_points_boreal, y=end_points, k=lcp_neighbors))
nearest_distance <- as.data.frame(terra::nearest(start_points, end_points)) #essentially, use nearest function for k=1
end_points$newrowID <- seq(1, nrow(end_points), 1) #needed for subsetting within loop

# tripped up: i=7,8 but found error source, 69 (end point outside condmat; switched to using whole thing, hope that doesn't make it run out of memory)
for (i in 1:nrow(start_points_boreal)) { #like this will run completely without crashing...let's see how far we get
#for (i in 1:2) { #testing loop
#for (i in 132:nrow(start_points_boreal)) { #if need to start from not the first iteration
  # First, extract start point and k nearest end points
  test_start <- start_points_boreal[i] 
  test_end <- as.data.frame(terra::nearby(test_start, end_points, k=lcp_neighbors))
  #test_end <- subset(end_pts, end_pts$newrowID %in% test_end[,2:6])
  end1 <- subset(end_points, end_points$newrowID==test_end[,2])
  end2 <- subset(end_points, end_points$newrowID==test_end[,3])
  end3 <- subset(end_points, end_points$newrowID==test_end[,4])
  end4 <- subset(end_points, end_points$newrowID==test_end[,5])
  end5 <- subset(end_points, end_points$newrowID==test_end[,6])
  
  ## this chunk might not be needed at regional scale
  # # because full study area conductance matrix runs out of memory/is very slow
  # # calculate part of it based on buffered box around start and end points
  tstart <- st_as_sf(test_start)
  t1 <- st_as_sf(end1)
  t2 <- st_as_sf(end2)
  t3 <- st_as_sf(end3)
  t4 <- st_as_sf(end4)
  t5 <- st_as_sf(end5)
  t1 <- rbind(st_coordinates(tstart),st_coordinates(t1),st_coordinates(t2),
              st_coordinates(t3),st_coordinates(t4),st_coordinates(t5))
  t1 <- t1 %>%
    as.data.frame %>%
    sf::st_as_sf(coords = c(1,2))
  #plot(t1)
  # BUFFER
  t2 <- st_buffer(t1, 200000) #may use 150000; crashed a lot with 200000; used 200000 for the one that failed
  # CROP RASTER
  tmp_ras <- terra::crop(conductance, st_bbox(t2))
  # Calculate conductance matrix on this cropped raster
  condmat <- create_cs(tmp_ras, neighbours=8, dem=NULL, max_slope=NULL)

  # calculate least cost paths and save output
  lcp1 <- create_lcp(condmat, origin=test_start, destination=end1, cost_distance=T)
  lcp1$start_PAcomplexID <- test_start$PAcomplexI
  lcp1$lowland_areasqkm <- test_start$lowland_ar
  lcp1$end_ID <- end1$end_ID
  lcp1$highland_areasqkm <- end1$highland_a
  lcp1$lengthkm <- terra::perim(lcp1)/1000
  
  # check if end point is in a protected area
  end1PA <- terra::intersect(end1, protected_areas)
  lcp1$end_protected <- ifelse(dim(end1PA)[1] < 1, "No", "Yes")
  
  #lcp1name <- paste0("LeastCostPaths/dominions/boreal/", "LCP_", test_start$PAcomplexI, "_", end1$PAcomplexI,"_a", ".shp")
  lcp1name <- paste0("LeastCostPaths/dominions/boreal/", "LCP_", test_start$PAcomplexI, "_a", ".shp")
  terra::writeVector(lcp1, filename=lcp1name, overwrite=T)
  
  #lcp1namecsv <- paste0("LeastCostPaths/dominions/boreal/", "LCP_", test_start$PAcomplexI, "_", end1$PAcomplexI,"_a", ".csv")
  lcp1namecsv <- paste0("LeastCostPaths/dominions/boreal/", "LCP_", test_start$PAcomplexI, "_a", ".csv")
  csv1 <- as.data.frame(lcp1)
  csv1$iteration <- i
  write.csv(csv1, lcp1namecsv, row.names=F)
  
  #plotname1 <- paste0("LeastCostPaths/dominions/boreal/", "LCP_", test_start$PAcomplexI, "_", end1$PAcomplexI,"_a", ".jpeg")
  plotname1 <- paste0("LeastCostPaths/dominions/boreal/", "LCP_", test_start$PAcomplexI, "_a", ".jpeg")
  jpeg(filename=plotname1, height=6, width=6, units='in', res=300)
  plot(conductance_allbuff)
  plot(boreal, add=T)
  plot(test_start, add=T, pch=19)
  plot(end1, add=T, pch=15)
  plot(lcp1, add=T, lwd=2, col='blue')
  dev.off()
  
  lcp2 <- create_lcp(condmat, origin=test_start, destination=end2, cost_distance=T)
  lcp2$start_PAcomplexID <- test_start$PAcomplexI
  lcp2$lowland_areasqkm <- test_start$lowland_ar
  lcp2$end_ID <- end2$end_ID
  lcp2$highland_areasqkm <- end2$highland_a
  lcp2$lengthkm <- terra::perim(lcp2)/1000
  
  # check if end point is in a protected area
  end2PA <- terra::intersect(end2, protected_areas)
  lcp2$end_protected <- ifelse(dim(end2PA)[1] < 1, "No", "Yes")
  
  #lcp2name <- paste0("LeastCostPaths/dominions/boreal/", "LCP_", test_start$PAcomplexI, "_", end2$PAcomplexI,"_b", ".shp")
  lcp2name <- paste0("LeastCostPaths/dominions/boreal/", "LCP_", test_start$PAcomplexI, "_b", ".shp")
  terra::writeVector(lcp2, filename=lcp2name, overwrite=T)
  
  #lcp2namecsv <- paste0("LeastCostPaths/dominions/boreal/", "LCP_", test_start$PAcomplexI, "_", end2$PAcomplexI,"_b", ".csv")
  lcp2namecsv <- paste0("LeastCostPaths/dominions/boreal/", "LCP_", test_start$PAcomplexI, "_b", ".csv")
  csv1 <- as.data.frame(lcp2)
  csv1$iteration <- i
  write.csv(csv1, lcp2namecsv, row.names=F)
  
  #plotname2 <- paste0("LeastCostPaths/dominions/boreal/", "LCP_", test_start$PAcomplexI, "_", end2$PAcomplexI,"_b", ".jpeg")
  plotname2 <- paste0("LeastCostPaths/dominions/boreal/", "LCP_", test_start$PAcomplexI, "_b", ".jpeg")
  jpeg(filename=plotname2, height=6, width=6, units='in', res=300)
  plot(conductance_allbuff)
  plot(boreal, add=T)
  plot(test_start, add=T, pch=19)
  plot(end2, add=T, pch=15)
  plot(lcp2, add=T, lwd=2, col='blue')
  dev.off()
  
  lcp3 <- create_lcp(condmat, origin=test_start, destination=end3, cost_distance=T)
  lcp3$start_PAcomplexID <- test_start$PAcomplexI
  lcp3$lowland_areasqkm <- test_start$lowland_ar
  lcp3$end_ID <- end3$end_ID
  lcp3$highland_areasqkm <- end3$highland_a
  lcp3$lengthkm <- terra::perim(lcp3)/1000
  
  # check if end point is in a protected area
  end3PA <- terra::intersect(end3, protected_areas)
  lcp3$end_protected <- ifelse(dim(end3PA)[1] < 1, "No", "Yes")
  
  #lcp3name <- paste0("LeastCostPaths/dominions/boreal/", "LCP_", test_start$PAcomplexI, "_", end3$PAcomplexI,"_c", ".shp")
  lcp3name <- paste0("LeastCostPaths/dominions/boreal/", "LCP_", test_start$PAcomplexI, "_c", ".shp")
  terra::writeVector(lcp3, filename=lcp3name, overwrite=T)
  
  #lcp3namecsv <- paste0("LeastCostPaths/dominions/boreal/", "LCP_", test_start$PAcomplexI, "_", end3$PAcomplexI,"_c", ".csv")
  lcp3namecsv <- paste0("LeastCostPaths/dominions/boreal/", "LCP_", test_start$PAcomplexI, "_c", ".csv")
  csv1 <- as.data.frame(lcp3)
  csv1$iteration <- i
  write.csv(csv1, lcp3namecsv, row.names=F)
  
  #plotname3 <- paste0("LeastCostPaths/dominions/boreal/", "LCP_", test_start$PAcomplexI, "_", end3$PAcomplexI,"_c", ".jpeg")
  plotname3 <- paste0("LeastCostPaths/dominions/boreal/", "LCP_", test_start$PAcomplexI, "_c", ".jpeg")
  jpeg(filename=plotname3, height=6, width=6, units='in', res=300)
  plot(conductance_allbuff)
  plot(boreal, add=T)
  plot(test_start, add=T, pch=19)
  plot(end3, add=T, pch=15)
  plot(lcp3, add=T, lwd=2, col='blue')
  dev.off()
  
  # THIS NEXT PART NOT MODIFIED WITH AUG 2025 UPDATES
  # turning off 4 and 5 for now to save time
  # lcp4 <- create_lcp(condmat, origin=test_start, destination=end4, cost_distance=T)
  # lcp4$start_PAcomplexID <- test_start$PAcomplexI
  # lcp4$lowland_areasqkm <- test_start$lowland_ar
  # lcp4$end_PAcomplexID <- end4$PAcomplexI
  # lcp4$highland_areasqkm <- end4$highland_a
  # lcp4$lengthkm <- terra::perim(lcp4)/1000
  # 
  # lcp4name <- paste0("LeastCostPaths/dominions/boreal/", "LCP_", test_start$PAcomplexI, "_", end4$PAcomplexI,"_d", ".shp")
  # terra::writeVector(lcp4, filename=lcp4name, overwrite=T)
  # 
  # lcp4namecsv <- paste0("LeastCostPaths/dominions/boreal/", "LCP_", test_start$PAcomplexI, "_", end4$PAcomplexI,"_d", ".csv")
  # csv1 <- as.data.frame(lcp4)
  # csv1$iteration <- i
  # write.csv(csv1, lcp4namecsv, row.names=F)
  # 
  # plotname4 <- paste0("LeastCostPaths/dominions/boreal/", "LCP_", test_start$PAcomplexI, "_", end4$PAcomplexI,"_d", ".jpeg")
  # jpeg(filename=plotname4, height=6, width=6, units='in', res=300)
  # plot(conductance_allbuff)
  # plot(boreal, add=T)
  # plot(test_start, add=T, pch=19)
  # plot(end4, add=T, pch=15)
  # plot(lcp4, add=T, lwd=2, col='blue')
  # dev.off()
  # 
  # lcp5 <- create_lcp(condmat, origin=test_start, destination=end5, cost_distance=T)
  # lcp5$start_PAcomplexID <- test_start$PAcomplexI
  # lcp5$lowland_areasqkm <- test_start$lowland_ar
  # lcp5$end_PAcomplexID <- end5$PAcomplexI
  # lcp5$highland_areasqkm <- end5$highland_a
  # lcp5$lengthkm <- terra::perim(lcp5)/1000
  # 
  # lcp5name <- paste0("LeastCostPaths/dominions/boreal/", "LCP_", test_start$PAcomplexI, "_", end5$PAcomplexI,"_e", ".shp")
  # terra::writeVector(lcp5, filename=lcp5name, overwrite=T)
  # 
  # lcp5namecsv <- paste0("LeastCostPaths/dominions/boreal/", "LCP_", test_start$PAcomplexI, "_", end5$PAcomplexI,"_e", ".csv")
  # csv1 <- as.data.frame(lcp5)
  # csv1$iteration <- i
  # write.csv(csv1, lcp5namecsv, row.names=F)
  # 
  # plotname5 <- paste0("LeastCostPaths/dominions/boreal/", "LCP_", test_start$PAcomplexI, "_", end5$PAcomplexI,"_e", ".jpeg")
  # jpeg(filename=plotname5, height=6, width=6, units='in', res=300)
  # plot(conductance_allbuff)
  # plot(boreal, add=T)
  # plot(test_start, add=T, pch=19)
  # plot(end5, add=T, pch=15)
  # plot(lcp5, add=T, lwd=2, col='blue')
  # dev.off()
  
  test_start = NULL
  plotname1 = NULL
  csv1 = NULL
  lcp1name = NULL
  lcp1namecsv = NULL
  lcp1 = NULL
  plotname2 = NULL
  csv2 = NULL
  lcp2name = NULL
  lcp2namecsv = NULL
  lcp2 = NULL
  plotname3 = NULL
  csv3 = NULL
  lcp3name = NULL
  lcp3namecsv = NULL
  lcp3 = NULL
  plotname4 = NULL
  csv4 = NULL
  lcp4name = NULL
  lcp4namecsv = NULL
  lcp4 = NULL
  plotname5 = NULL
  csv5 = NULL
  lcp5name = NULL
  lcp5namecsv = NULL
  lcp5 = NULL
  
  end1 = NULL
  end2 = NULL
  end3 = NULL
  end4 = NULL
  end5 = NULL
  
  tstart = NULL
  t1 = NULL
  t2 = NULL
  t3 = NULL
  t4 = NULL
  t5 = NULL
  tmp_ras = NULL
  tmp_condmat = NULL
  
  end1PA = NULL
  end2PA = NULL
  end3PA = NULL
  end4PA = NULL
  end5PA = NULL
}

lcp_list <- list.files(path='C:/Users/immccull/Documents/AmazonClimateCorridors/LeastCostPaths/dominions/boreal', pattern='.shp', full.names=T)
lcp_list <- lapply(lcp_list, terra::vect)
x <- terra::vect(lcp_list)
x <- terra::project(x, "EPSG:29172")
#writeVector(x, filename='LeastCostPaths/CombinedLeastCostPaths/CombinedLeastCostPaths_boreal.shp', overwrite=T)
#writeVector(x, filename='LeastCostPaths/CombinedLeastCostPaths/CombinedLeastCostPaths_boreal_2K.shp', overwrite=T)

### South region
for (i in 1:nrow(start_points_south)) { #if need to start from not the first iteration
  # First, extract start point and k nearest end points
  test_start <- start_points_south[i] 
  test_end <- as.data.frame(terra::nearby(test_start, end_points, k=lcp_neighbors))
  #test_end <- subset(end_pts, end_pts$newrowID %in% test_end[,2:6])
  end1 <- subset(end_points, end_points$newrowID==test_end[,2])
  end2 <- subset(end_points, end_points$newrowID==test_end[,3])
  end3 <- subset(end_points, end_points$newrowID==test_end[,4])
  end4 <- subset(end_points, end_points$newrowID==test_end[,5])
  end5 <- subset(end_points, end_points$newrowID==test_end[,6])
  
  ## this chunk might not be needed at regional scale
  # # because full study area conductance matrix runs out of memory/is very slow
  # # calculate part of it based on buffered box around start and end points
  tstart <- st_as_sf(test_start)
  t1 <- st_as_sf(end1)
  t2 <- st_as_sf(end2)
  t3 <- st_as_sf(end3)
  t4 <- st_as_sf(end4)
  t5 <- st_as_sf(end5)
  t1 <- rbind(st_coordinates(tstart),st_coordinates(t1),st_coordinates(t2),
              st_coordinates(t3),st_coordinates(t4),st_coordinates(t5))
  t1 <- t1 %>%
    as.data.frame %>%
    sf::st_as_sf(coords = c(1,2))
  #plot(t1)
  # BUFFER
  t2 <- st_buffer(t1, 200000) #may use 150000; crashed a lot with 200000; used 200000 for the one that failed
  # CROP RASTER
  tmp_ras <- terra::crop(conductance, st_bbox(t2))
  # Calculate conductance matrix on this cropped raster
  condmat <- create_cs(tmp_ras, neighbours=8, dem=NULL, max_slope=NULL)
  
  # calculate least cost paths and save output
  lcp1 <- create_lcp(condmat, origin=test_start, destination=end1, cost_distance=T)
  lcp1$start_PAcomplexID <- test_start$PAcomplexI
  lcp1$lowland_areasqkm <- test_start$lowland_ar
  lcp1$end_ID <- end1$end_ID
  lcp1$highland_areasqkm <- end1$highland_a
  lcp1$lengthkm <- terra::perim(lcp1)/1000
  
  # check if end point is in a protected area
  end1PA <- terra::intersect(end1, protected_areas)
  lcp1$end_protected <- ifelse(dim(end1PA)[1] < 1, "No", "Yes")
  
  #lcp1name <- paste0("LeastCostPaths/dominions/south/", "LCP_", test_start$PAcomplexI, "_", end1$PAcomplexI,"_a", ".shp")
  lcp1name <- paste0("LeastCostPaths/dominions/south/", "LCP_", test_start$PAcomplexI, "_a", ".shp")
  terra::writeVector(lcp1, filename=lcp1name, overwrite=T)
  
  #lcp1namecsv <- paste0("LeastCostPaths/dominions/south/", "LCP_", test_start$PAcomplexI, "_", end1$PAcomplexI,"_a", ".csv")
  lcp1namecsv <- paste0("LeastCostPaths/dominions/south/", "LCP_", test_start$PAcomplexI, "_a", ".csv")
  csv1 <- as.data.frame(lcp1)
  csv1$iteration <- i
  write.csv(csv1, lcp1namecsv, row.names=F)
  
  #plotname1 <- paste0("LeastCostPaths/dominions/south/", "LCP_", test_start$PAcomplexI, "_", end1$PAcomplexI,"_a", ".jpeg")
  plotname1 <- paste0("LeastCostPaths/dominions/south/", "LCP_", test_start$PAcomplexI, "_a", ".jpeg")
  jpeg(filename=plotname1, height=6, width=6, units='in', res=300)
  plot(conductance_allbuff)
  plot(south, add=T)
  plot(test_start, add=T, pch=19)
  plot(end1, add=T, pch=15)
  plot(lcp1, add=T, lwd=2, col='blue')
  dev.off()
  
  lcp2 <- create_lcp(condmat, origin=test_start, destination=end2, cost_distance=T)
  lcp2$start_PAcomplexID <- test_start$PAcomplexI
  lcp2$lowland_areasqkm <- test_start$lowland_ar
  lcp2$end_ID <- end2$end_ID
  lcp2$highland_areasqkm <- end2$highland_a
  lcp2$lengthkm <- terra::perim(lcp2)/1000
  
  # check if end point is in a protected area
  end2PA <- terra::intersect(end2, protected_areas)
  lcp2$end_protected <- ifelse(dim(end2PA)[1] < 1, "No", "Yes")
  
  #lcp2name <- paste0("LeastCostPaths/dominions/south/", "LCP_", test_start$PAcomplexI, "_", end2$PAcomplexI,"_b", ".shp")
  lcp2name <- paste0("LeastCostPaths/dominions/south/", "LCP_", test_start$PAcomplexI, "_b", ".shp")
  terra::writeVector(lcp2, filename=lcp2name, overwrite=T)
  
  #lcp2namecsv <- paste0("LeastCostPaths/dominions/south/", "LCP_", test_start$PAcomplexI, "_", end2$PAcomplexI,"_b", ".csv")
  lcp2namecsv <- paste0("LeastCostPaths/dominions/south/", "LCP_", test_start$PAcomplexI, "_b", ".csv")
  csv1 <- as.data.frame(lcp2)
  csv1$iteration <- i
  write.csv(csv1, lcp2namecsv, row.names=F)
  
  #plotname2 <- paste0("LeastCostPaths/dominions/south/", "LCP_", test_start$PAcomplexI, "_", end2$PAcomplexI,"_b", ".jpeg")
  plotname2 <- paste0("LeastCostPaths/dominions/south/", "LCP_", test_start$PAcomplexI, "_b", ".jpeg")
  jpeg(filename=plotname2, height=6, width=6, units='in', res=300)
  plot(conductance_allbuff)
  plot(south, add=T)
  plot(test_start, add=T, pch=19)
  plot(end2, add=T, pch=15)
  plot(lcp2, add=T, lwd=2, col='blue')
  dev.off()
  
  lcp3 <- create_lcp(condmat, origin=test_start, destination=end3, cost_distance=T)
  lcp3$start_PAcomplexID <- test_start$PAcomplexI
  lcp3$lowland_areasqkm <- test_start$lowland_ar
  lcp3$end_ID <- end3$end_ID
  lcp3$highland_areasqkm <- end3$highland_a
  lcp3$lengthkm <- terra::perim(lcp3)/1000
  
  # check if end point is in a protected area
  end3PA <- terra::intersect(end3, protected_areas)
  lcp3$end_protected <- ifelse(dim(end3PA)[1] < 1, "No", "Yes")
  
  #lcp3name <- paste0("LeastCostPaths/dominions/south/", "LCP_", test_start$PAcomplexI, "_", end3$PAcomplexI,"_c", ".shp")
  lcp3name <- paste0("LeastCostPaths/dominions/south/", "LCP_", test_start$PAcomplexI, "_c", ".shp")
  terra::writeVector(lcp3, filename=lcp3name, overwrite=T)
  
  #lcp3namecsv <- paste0("LeastCostPaths/dominions/south/", "LCP_", test_start$PAcomplexI, "_", end3$PAcomplexI,"_c", ".csv")
  lcp3namecsv <- paste0("LeastCostPaths/dominions/south/", "LCP_", test_start$PAcomplexI, "_c", ".csv")
  csv1 <- as.data.frame(lcp3)
  csv1$iteration <- i
  write.csv(csv1, lcp3namecsv, row.names=F)
  
  #plotname3 <- paste0("LeastCostPaths/dominions/south/", "LCP_", test_start$PAcomplexI, "_", end3$PAcomplexI,"_c", ".jpeg")
  plotname3 <- paste0("LeastCostPaths/dominions/south/", "LCP_", test_start$PAcomplexI, "_c", ".jpeg")
  jpeg(filename=plotname3, height=6, width=6, units='in', res=300)
  plot(conductance_allbuff)
  plot(south, add=T)
  plot(test_start, add=T, pch=19)
  plot(end3, add=T, pch=15)
  plot(lcp3, add=T, lwd=2, col='blue')
  dev.off()
  
  # turning off 4 and 5 for now to save time
  # not updated with Aug 2025 output file names
  # lcp4 <- create_lcp(condmat, origin=test_start, destination=end4, cost_distance=T)
  # lcp4$start_PAcomplexID <- test_start$PAcomplexI
  # lcp4$lowland_areasqkm <- test_start$lowland_ar
  # lcp4$end_PAcomplexID <- end4$PAcomplexI
  # lcp4$highland_areasqkm <- end4$highland_a
  # lcp4$lengthkm <- terra::perim(lcp4)/1000
  # 
  # lcp4name <- paste0("LeastCostPaths/dominions/south/", "LCP_", test_start$PAcomplexI, "_", end4$PAcomplexI,"_d", ".shp")
  # terra::writeVector(lcp4, filename=lcp4name, overwrite=T)
  # 
  # lcp4namecsv <- paste0("LeastCostPaths/dominions/south/", "LCP_", test_start$PAcomplexI, "_", end4$PAcomplexI,"_d", ".csv")
  # csv1 <- as.data.frame(lcp4)
  # csv1$iteration <- i
  # write.csv(csv1, lcp4namecsv, row.names=F)
  # 
  # plotname4 <- paste0("LeastCostPaths/dominions/south/", "LCP_", test_start$PAcomplexI, "_", end4$PAcomplexI,"_d", ".jpeg")
  # jpeg(filename=plotname4, height=6, width=6, units='in', res=300)
  # plot(conductance_allbuff)
  # plot(south, add=T)
  # plot(test_start, add=T, pch=19)
  # plot(end4, add=T, pch=15)
  # plot(lcp4, add=T, lwd=2, col='blue')
  # dev.off()
  # 
  # lcp5 <- create_lcp(condmat, origin=test_start, destination=end5, cost_distance=T)
  # lcp5$start_PAcomplexID <- test_start$PAcomplexI
  # lcp5$lowland_areasqkm <- test_start$lowland_ar
  # lcp5$end_PAcomplexID <- end5$PAcomplexI
  # lcp5$highland_areasqkm <- end5$highland_a
  # lcp5$lengthkm <- terra::perim(lcp5)/1000
  # 
  # lcp5name <- paste0("LeastCostPaths/dominions/south/", "LCP_", test_start$PAcomplexI, "_", end5$PAcomplexI,"_e", ".shp")
  # terra::writeVector(lcp5, filename=lcp5name, overwrite=T)
  # 
  # lcp5namecsv <- paste0("LeastCostPaths/dominions/south/", "LCP_", test_start$PAcomplexI, "_", end5$PAcomplexI,"_e", ".csv")
  # csv1 <- as.data.frame(lcp5)
  # csv1$iteration <- i
  # write.csv(csv1, lcp5namecsv, row.names=F)
  # 
  # plotname5 <- paste0("LeastCostPaths/dominions/south/", "LCP_", test_start$PAcomplexI, "_", end5$PAcomplexI,"_e", ".jpeg")
  # jpeg(filename=plotname5, height=6, width=6, units='in', res=300)
  # plot(conductance_allbuff)
  # plot(south, add=T)
  # plot(test_start, add=T, pch=19)
  # plot(end5, add=T, pch=15)
  # plot(lcp5, add=T, lwd=2, col='blue')
  # dev.off()
  
  test_start = NULL
  plotname1 = NULL
  csv1 = NULL
  lcp1name = NULL
  lcp1namecsv = NULL
  lcp1 = NULL
  plotname2 = NULL
  csv2 = NULL
  lcp2name = NULL
  lcp2namecsv = NULL
  lcp2 = NULL
  plotname3 = NULL
  csv3 = NULL
  lcp3name = NULL
  lcp3namecsv = NULL
  lcp3 = NULL
  plotname4 = NULL
  csv4 = NULL
  lcp4name = NULL
  lcp4namecsv = NULL
  lcp4 = NULL
  plotname5 = NULL
  csv5 = NULL
  lcp5name = NULL
  lcp5namecsv = NULL
  lcp5 = NULL
  
  end1 = NULL
  end2 = NULL
  end3 = NULL
  end4 = NULL
  end5 = NULL
  
  tstart = NULL
  t1 = NULL
  t2 = NULL
  t3 = NULL
  t4 = NULL
  t5 = NULL
  tmp_ras = NULL
  tmp_condmat = NULL
  
  end1PA = NULL
  end2PA = NULL
  end3PA = NULL
  end4PA = NULL
  end5PA = NULL
}

lcp_list <- list.files(path='C:/Users/immccull/Documents/AmazonClimateCorridors/LeastCostPaths/dominions/south', pattern='.shp', full.names=T)
lcp_list <- lapply(lcp_list, terra::vect)
x <- terra::vect(lcp_list)
x <- terra::project(x, "EPSG:29172")
#writeVector(x, filename='LeastCostPaths/CombinedLeastCostPaths/CombinedLeastCostPaths_south.shp', overwrite=T)
#writeVector(x, filename='LeastCostPaths/CombinedLeastCostPaths/CombinedLeastCostPaths_south_2K.shp', overwrite=T)

### Chacoan region

for (i in 1:nrow(start_points_chacoan)) { #if need to start from not the first iteration
  # First, extract start point and k nearest end points
  test_start <- start_points_chacoan[i] 
  test_end <- as.data.frame(terra::nearby(test_start, end_points, k=lcp_neighbors))
  #test_end <- subset(end_pts, end_pts$newrowID %in% test_end[,2:6])
  end1 <- subset(end_points, end_points$newrowID==test_end[,2])
  end2 <- subset(end_points, end_points$newrowID==test_end[,3])
  end3 <- subset(end_points, end_points$newrowID==test_end[,4])
  end4 <- subset(end_points, end_points$newrowID==test_end[,5])
  end5 <- subset(end_points, end_points$newrowID==test_end[,6])
  
  ## this chunk might not be needed at regional scale
  # # because full study area conductance matrix runs out of memory/is very slow
  # # calculate part of it based on buffered box around start and end points
  tstart <- st_as_sf(test_start)
  t1 <- st_as_sf(end1)
  t2 <- st_as_sf(end2)
  t3 <- st_as_sf(end3)
  t4 <- st_as_sf(end4)
  t5 <- st_as_sf(end5)
  t1 <- rbind(st_coordinates(tstart),st_coordinates(t1),st_coordinates(t2),
              st_coordinates(t3),st_coordinates(t4),st_coordinates(t5))
  t1 <- t1 %>%
    as.data.frame %>%
    sf::st_as_sf(coords = c(1,2))
  #plot(t1)
  # BUFFER
  t2 <- st_buffer(t1, 200000) #may use 150000; crashed a lot with 200000; used 200000 for the one that failed
  # CROP RASTER
  tmp_ras <- terra::crop(conductance, st_bbox(t2))
  # Calculate conductance matrix on this cropped raster
  condmat <- create_cs(tmp_ras, neighbours=8, dem=NULL, max_slope=NULL)
  
  # calculate least cost paths and save output
  lcp1 <- create_lcp(condmat, origin=test_start, destination=end1, cost_distance=T)
  lcp1$start_PAcomplexID <- test_start$PAcomplexI
  lcp1$lowland_areasqkm <- test_start$lowland_ar
  lcp1$end_ID <- end1$end_ID
  lcp1$highland_areasqkm <- end1$highland_a
  lcp1$lengthkm <- terra::perim(lcp1)/1000
  
  # check if end point is in a protected area
  end1PA <- terra::intersect(end1, protected_areas)
  lcp1$end_protected <- ifelse(dim(end1PA)[1] < 1, "No", "Yes")
  
  #lcp1name <- paste0("LeastCostPaths/dominions/chacoan/", "LCP_", test_start$PAcomplexI, "_", end1$PAcomplexI,"_a", ".shp")
  lcp1name <- paste0("LeastCostPaths/dominions/chacoan/", "LCP_", test_start$PAcomplexI, "_a", ".shp")
  terra::writeVector(lcp1, filename=lcp1name, overwrite=T)
  
  #lcp1namecsv <- paste0("LeastCostPaths/dominions/chacoan/", "LCP_", test_start$PAcomplexI, "_", end1$PAcomplexI,"_a", ".csv")
  lcp1namecsv <- paste0("LeastCostPaths/dominions/chacoan/", "LCP_", test_start$PAcomplexI, "_a", ".csv")
  csv1 <- as.data.frame(lcp1)
  csv1$iteration <- i
  write.csv(csv1, lcp1namecsv, row.names=F)
  
  #plotname1 <- paste0("LeastCostPaths/dominions/chacoan/", "LCP_", test_start$PAcomplexI, "_", end1$PAcomplexI,"_a", ".jpeg")
  plotname1 <- paste0("LeastCostPaths/dominions/chacoan/", "LCP_", test_start$PAcomplexI, "_a", ".jpeg")
  jpeg(filename=plotname1, height=6, width=6, units='in', res=300)
  plot(conductance_allbuff)
  plot(chacoan, add=T)
  plot(test_start, add=T, pch=19)
  plot(end1, add=T, pch=15)
  plot(lcp1, add=T, lwd=2, col='blue')
  dev.off()
  
  lcp2 <- create_lcp(condmat, origin=test_start, destination=end2, cost_distance=T)
  lcp2$start_PAcomplexID <- test_start$PAcomplexI
  lcp2$lowland_areasqkm <- test_start$lowland_ar
  lcp2$end_ID <- end2$end_ID
  lcp2$highland_areasqkm <- end2$highland_a
  lcp2$lengthkm <- terra::perim(lcp2)/1000
  
  # check if end point is in a protected area
  end2PA <- terra::intersect(end2, protected_areas)
  lcp2$end_protected <- ifelse(dim(end2PA)[1] < 1, "No", "Yes")
  
  #lcp2name <- paste0("LeastCostPaths/dominions/chacoan/", "LCP_", test_start$PAcomplexI, "_", end2$PAcomplexI,"_b", ".shp")
  lcp2name <- paste0("LeastCostPaths/dominions/chacoan/", "LCP_", test_start$PAcomplexI, "_b", ".shp")
  terra::writeVector(lcp2, filename=lcp2name, overwrite=T)
  
  #lcp2namecsv <- paste0("LeastCostPaths/dominions/chacoan/", "LCP_", test_start$PAcomplexI, "_", end2$PAcomplexI,"_b", ".csv")
  lcp2namecsv <- paste0("LeastCostPaths/dominions/chacoan/", "LCP_", test_start$PAcomplexI, "_b", ".csv")
  csv1 <- as.data.frame(lcp2)
  csv1$iteration <- i
  write.csv(csv1, lcp2namecsv, row.names=F)
  
  #plotname2 <- paste0("LeastCostPaths/dominions/chacoan/", "LCP_", test_start$PAcomplexI, "_", end2$PAcomplexI,"_b", ".jpeg")
  plotname2 <- paste0("LeastCostPaths/dominions/chacoan/", "LCP_", test_start$PAcomplexI, "_b", ".jpeg")
  jpeg(filename=plotname2, height=6, width=6, units='in', res=300)
  plot(conductance_allbuff)
  plot(chacoan, add=T)
  plot(test_start, add=T, pch=19)
  plot(end2, add=T, pch=15)
  plot(lcp2, add=T, lwd=2, col='blue')
  dev.off()
  
  lcp3 <- create_lcp(condmat, origin=test_start, destination=end3, cost_distance=T)
  lcp3$start_PAcomplexID <- test_start$PAcomplexI
  lcp3$lowland_areasqkm <- test_start$lowland_ar
  lcp3$end_ID <- end3$end_ID
  lcp3$highland_areasqkm <- end3$highland_a
  lcp3$lengthkm <- terra::perim(lcp3)/1000
  
  # check if end point is in a protected area
  end3PA <- terra::intersect(end3, protected_areas)
  lcp3$end_protected <- ifelse(dim(end3PA)[1] < 1, "No", "Yes")
  
  #lcp3name <- paste0("LeastCostPaths/dominions/chacoan/", "LCP_", test_start$PAcomplexI, "_", end3$PAcomplexI,"_c", ".shp")
  lcp3name <- paste0("LeastCostPaths/dominions/chacoan/", "LCP_", test_start$PAcomplexI, "_c", ".shp")
  terra::writeVector(lcp3, filename=lcp3name, overwrite=T)
  
  #lcp3namecsv <- paste0("LeastCostPaths/dominions/chacoan/", "LCP_", test_start$PAcomplexI, "_", end3$PAcomplexI,"_c", ".csv")
  lcp3namecsv <- paste0("LeastCostPaths/dominions/chacoan/", "LCP_", test_start$PAcomplexI, "_c", ".csv")
  csv1 <- as.data.frame(lcp3)
  csv1$iteration <- i
  write.csv(csv1, lcp3namecsv, row.names=F)
  
  #plotname3 <- paste0("LeastCostPaths/dominions/chacoan/", "LCP_", test_start$PAcomplexI, "_", end3$PAcomplexI,"_c", ".jpeg")
  plotname3 <- paste0("LeastCostPaths/dominions/chacoan/", "LCP_", test_start$PAcomplexI, "_c", ".jpeg")
  jpeg(filename=plotname3, height=6, width=6, units='in', res=300)
  plot(conductance_allbuff)
  plot(chacoan, add=T)
  plot(test_start, add=T, pch=19)
  plot(end3, add=T, pch=15)
  plot(lcp3, add=T, lwd=2, col='blue')
  dev.off()
  
  # turning off 4 and 5 for now to save time
  # not updated with Aug 2025 file name changes
  # lcp4 <- create_lcp(condmat, origin=test_start, destination=end4, cost_distance=T)
  # lcp4$start_PAcomplexID <- test_start$PAcomplexI
  # lcp4$lowland_areasqkm <- test_start$lowland_ar
  # lcp4$end_PAcomplexID <- end4$PAcomplexI
  # lcp4$highland_areasqkm <- end4$highland_a
  # lcp4$lengthkm <- terra::perim(lcp4)/1000
  # 
  # lcp4name <- paste0("LeastCostPaths/dominions/chacoan/", "LCP_", test_start$PAcomplexI, "_", end4$PAcomplexI,"_d", ".shp")
  # terra::writeVector(lcp4, filename=lcp4name, overwrite=T)
  # 
  # lcp4namecsv <- paste0("LeastCostPaths/dominions/chacoan/", "LCP_", test_start$PAcomplexI, "_", end4$PAcomplexI,"_d", ".csv")
  # csv1 <- as.data.frame(lcp4)
  # csv1$iteration <- i
  # write.csv(csv1, lcp4namecsv, row.names=F)
  # 
  # plotname4 <- paste0("LeastCostPaths/dominions/chacoan/", "LCP_", test_start$PAcomplexI, "_", end4$PAcomplexI,"_d", ".jpeg")
  # jpeg(filename=plotname4, height=6, width=6, units='in', res=300)
  # plot(conductance_allbuff)
  # plot(chacoan, add=T)
  # plot(test_start, add=T, pch=19)
  # plot(end4, add=T, pch=15)
  # plot(lcp4, add=T, lwd=2, col='blue')
  # dev.off()
  # 
  # lcp5 <- create_lcp(condmat, origin=test_start, destination=end5, cost_distance=T)
  # lcp5$start_PAcomplexID <- test_start$PAcomplexI
  # lcp5$lowland_areasqkm <- test_start$lowland_ar
  # lcp5$end_PAcomplexID <- end5$PAcomplexI
  # lcp5$highland_areasqkm <- end5$highland_a
  # lcp5$lengthkm <- terra::perim(lcp5)/1000
  # 
  # lcp5name <- paste0("LeastCostPaths/dominions/chacoan/", "LCP_", test_start$PAcomplexI, "_", end5$PAcomplexI,"_e", ".shp")
  # terra::writeVector(lcp5, filename=lcp5name, overwrite=T)
  # 
  # lcp5namecsv <- paste0("LeastCostPaths/dominions/chacoan/", "LCP_", test_start$PAcomplexI, "_", end5$PAcomplexI,"_e", ".csv")
  # csv1 <- as.data.frame(lcp5)
  # csv1$iteration <- i
  # write.csv(csv1, lcp5namecsv, row.names=F)
  # 
  # plotname5 <- paste0("LeastCostPaths/dominions/chacoan/", "LCP_", test_start$PAcomplexI, "_", end5$PAcomplexI,"_e", ".jpeg")
  # jpeg(filename=plotname5, height=6, width=6, units='in', res=300)
  # plot(conductance_allbuff)
  # plot(chacoan, add=T)
  # plot(test_start, add=T, pch=19)
  # plot(end5, add=T, pch=15)
  # plot(lcp5, add=T, lwd=2, col='blue')
  # dev.off()
  
  test_start = NULL
  plotname1 = NULL
  csv1 = NULL
  lcp1name = NULL
  lcp1namecsv = NULL
  lcp1 = NULL
  plotname2 = NULL
  csv2 = NULL
  lcp2name = NULL
  lcp2namecsv = NULL
  lcp2 = NULL
  plotname3 = NULL
  csv3 = NULL
  lcp3name = NULL
  lcp3namecsv = NULL
  lcp3 = NULL
  plotname4 = NULL
  csv4 = NULL
  lcp4name = NULL
  lcp4namecsv = NULL
  lcp4 = NULL
  plotname5 = NULL
  csv5 = NULL
  lcp5name = NULL
  lcp5namecsv = NULL
  lcp5 = NULL
  
  end1 = NULL
  end2 = NULL
  end3 = NULL
  end4 = NULL
  end5 = NULL
  
  tstart = NULL
  t1 = NULL
  t2 = NULL
  t3 = NULL
  t4 = NULL
  t5 = NULL
  tmp_ras = NULL
  tmp_condmat = NULL
  
  end1PA = NULL
  end2PA = NULL
  end3PA = NULL
  end4PA = NULL
  end5PA = NULL
}

lcp_list <- list.files(path='C:/Users/immccull/Documents/AmazonClimateCorridors/LeastCostPaths/dominions/chacoan', pattern='.shp', full.names=T)
lcp_list <- lapply(lcp_list, terra::vect)
x <- terra::vect(lcp_list)
x <- terra::project(x, "EPSG:29172")
#writeVector(x, filename='LeastCostPaths/CombinedLeastCostPaths/CombinedLeastCostPaths_chacoan.shp', overwrite=T)
#writeVector(x, filename='LeastCostPaths/CombinedLeastCostPaths/CombinedLeastCostPaths_chacoan_2K.shp', overwrite=T)


### Combine least cost paths across regions
lcp_list <- list.files(path='C:/Users/immccull/Documents/AmazonClimateCorridors/LeastCostPaths/CombinedLeastCostPaths', pattern='2K.shp', full.names=T)
lcp_list <- lapply(lcp_list, terra::vect)
x <- terra::vect(lcp_list)
x <- terra::project(x, "EPSG:29172")
#writeVector(x, filename='LeastCostPaths/CombinedLeastCostPaths/CombinedLeastCostPaths_amazon_2K.shp', overwrite=T)


## Calculate LCP density (per 5 sq km) (1 km ran out of memory)
conductance_crop <- terra::crop(conductance, amazon_study_area, mask=T)
#conductance_crop_1km <- terra::aggregate(conductance_crop, fact=2, fun='mean', na.rm=T) #fun doesn't matter; only care about res
conductance_crop_10km <- terra::aggregate(conductance_crop, fact=20, fun='mean', na.rm=T)
lcps_dens <- create_lcp_density(conductance_crop_10km, lcps = x)
#terra::writeRaster(lcps_dens, filename="LeastCostPaths/LCP_density/LCP_density10km.tif", overwrite=T)
#terra::writeRaster(lcps_dens, filename="LeastCostPaths/LCP_density/LCP_density10km_2K.tif", overwrite=T)

conductance_crop_100km <- terra::aggregate(conductance_crop, fact=200, fun='mean', na.rm=T)
lcps_dens100 <- create_lcp_density(conductance_crop_100km, lcps = x)
#terra::writeRaster(lcps_dens100, filename="LeastCostPaths/LCP_density/LCP_density100km.tif", overwrite=T)

## Create layer of end nodes that actually were selected for climate corridors
endz <- x$end_PAcomp
end_ptz <- terra::vect("end_nodes/dominions/end_points/combined_end_points/combined_end_points.shp")
end_ptz_endz <- subset(end_ptz, end_ptz$PAcomplexI %in% endz)
#terra::writeVector(end_ptz_endz, "end_nodes/dominions/end_points/combined_end_points/combined_end_points_corridorlink.shp")

# For Aug 2025 update: create subset of protected end nodes and end nodes
# that corridors actually ended in
end_protected <- subset(x, x$end_protec=='Yes')
end_unprotected <- subset(x, x$end_protec=='No')
nrow(end_protected)/nrow(end_points)
#writeVector(end_protected, file="end_nodes/dominions/end_points/combined_end_points_2Kprotected.shp", overwrite=T)
#writeVector(end_unprotected, file="end_nodes/dominions/end_points/combined_end_points_2Kunprotected.shp", overwrite=T)

endz <- unique(x$end_ID)
end_pts_corridor_link <- subset(end_points, end_points$end_ID %in% endz)  
nrow(end_pts_corridor_link)/nrow(end_points)
#writeVector(end_pts_corridor_link, file="end_nodes/dominions/end_points/combined_end_points/combined_end_points_corridorlink2K.shp")

###### old: before decisions made on 11/7 ####
# Start and end nodes
# these are stored in region by region shapefiles (pts and polygons)
# eventually use loop to call them in?
# using one region for now
# pts <- terra::vect("protected_areas/Amazon_merged_PAs/regional_PA_complexes/PA_complex_Guianan Lowlands province_pts.shp")
# pts <- terra::project(pts, "EPSG:29172")
# region <- pts$Provincias[1]
# region <- subset(morrone2022_amazon, morrone2022_amazon$Provincias==region)
# 
# #### Main program ####
# # assign unique ID to all start and end nodes (this code works because pts and polygons derived from same sources)
# start_pts$start_ID <- paste0("start_ID_", seq(1,nrow(start_pts)))
# end_pts$end_ID <- paste0("end_ID_", seq(1,nrow(end_pts)))
# # start_polygons$start_ID <- paste0("start_ID_", seq(1,nrow(start_polygons)))
# # end_polygons$end_ID <- paste0("end_ID_", seq(1,nrow(end_polygons)))
# 
# test <- terra::extract(conductance, pts) #also need to remove start nodes not in conductance surface area (likely in the water)
# names(test) <- c('ID','cond')
# nas <- which(is.na(test), arr.ind=T)[,1] #get row IDs of NAs
# pts <- start_pts[-c(nas), ]
# 
# start_pts <- subset(pts, pts$node=='Start')
# end_pts <- subset(pts, pts$node=='End')
# 
# # end_pts <- terra::intersect(end_pts, amazon_study_area)
# # start_polygons <- subset(start_polygons, start_polygons$start_ID %in% start_pts$start_ID)
# # end_polygons <- subset(end_polygons, end_polygons$end_ID %in% end_pts$end_ID)
# 
# plot(conductance)
# #plot(amazon_study_area, add=T)
# plot(morrone2022_amazon, add=T)
# plot(start_pts, add=T, col='black', pch=20)
# plot(end_pts, add=T, col='blue', pch=20)
# 
# ### Least cost paths
# # conductance matrix
# 
# # For the regional approach, try buffering by 100 km
# region_buffered <- terra::buffer(region, width=100000)
# conductance_region <- terra::crop(conductance, region_buffered, mask=T)
# ncell(conductance_region)
# 
# plot(conductance_region)
# plot(region, add=T)
# plot(start_pts, add=T)
# plot(end_pts, add=T, col='orange')
# 
# # conductance matrix: may run out of memory here or during LCP loop, 
# # but more likely to be OK using regional approach
# # I am not aware of a way to save this intermediate output (writeRaster does not work for this type of file)
# condmat <- create_cs(conductance_region, neighbours=8, dem=NULL, max_slope=NULL)
# lcp_neighbors <- 5
# knear <- as.data.frame(terra::nearby(x=start_pts, y=end_pts, k=lcp_neighbors))
# nearest_distance <- as.data.frame(terra::nearest(start_pts, end_pts)) #essentially, use nearest function for k=1
# 
# end_pts$newrowID <- seq(1, nrow(end_pts), 1) #needed for subsetting within loop
# #end_polygons$newrowID <- seq(1, nrow(end_polygons), 1)
# 
# 
# for (i in 1:nrow(start_pts)) { #like this will run completely without crashing...let's see how far we get
# #for (i in 1:2) { #testing loop
# #for (i in 1:nrow(start_pts)) { #if need to start from not the first iteration
#   # First, extract start point and k nearest end points
#   test_start <- start_pts[i] 
#   test_end <- as.data.frame(terra::nearby(test_start, end_pts, k=lcp_neighbors))
#   #test_end <- subset(end_pts, end_pts$newrowID %in% test_end[,2:6])
#   end1 <- subset(end_pts, end_pts$newrowID==test_end[,2])
#   end2 <- subset(end_pts, end_pts$newrowID==test_end[,3])
#   end3 <- subset(end_pts, end_pts$newrowID==test_end[,4])
#   end4 <- subset(end_pts, end_pts$newrowID==test_end[,5])
#   end5 <- subset(end_pts, end_pts$newrowID==test_end[,6])
#   
#   ## this chunk might not be needed at regional scale
#   # # because full study area conductance matrix runs out of memory
#   # # calculate part of it based on buffered box around start and end points
#   # tstart <- st_as_sf(test_start)
#   # t1 <- st_as_sf(end1)
#   # t2 <- st_as_sf(end2)
#   # t3 <- st_as_sf(end3)
#   # t4 <- st_as_sf(end4)
#   # t5 <- st_as_sf(end5)
#   # t1 <- rbind(st_coordinates(tstart),st_coordinates(t1),st_coordinates(t2),
#   #             st_coordinates(t3),st_coordinates(t4),st_coordinates(t5))
#   # t1 <- t1 %>%
#   #   as.data.frame %>%
#   #   sf::st_as_sf(coords = c(1,2))
#   # #plot(t1)
#   # # BUFFER
#   # t2 <- st_buffer(t1, 150000) #crashed a lot with 200000; used 200000 for the one that failed
#   # # CROP RASTER
#   # tmp_ras <- terra::crop(conductance, st_bbox(t2))
#   # # Calculate conductance matrix on this cropped raster
#   # tmp_condmat <- create_cs(tmp_ras, neighbours=8, dem=NULL, max_slope=NULL)
#   # #tmp_condmat <- condmat #may work at lower resolutions (issue is if loop crashes, have to recalculate), but actually seems slower to calculate LCPs
#   # 
#   # calculate least cost paths and save output
#   lcp1 <- create_lcp(condmat, origin=test_start, destination=end1, cost_distance=T)
#   lcp1$start_PAcomplexID <- test_start$PAcomplex0
#   lcp1$start_pais <- test_start$pais
#   lcp1$start_min_elev_m <- test_start$min_elev_m
#   lcp1$end_PAcomplexID <- end1$PAcomplex0
#   lcp1$end_pais <- end1$pais
#   lcp1$end_max_elev_m <- end1$max_elev_m
#   lcp1$lengthkm <- terra::perim(lcp1)/1000
#   lcp1$region <- test_start$Provincias
#   
#   lcp1name <- paste0("LeastCostPaths/MorroneRegional/", "LCP_", test_start$PAcomplex0, "_", end1$PAcomplex0,"_a", ".shp")
#   terra::writeVector(lcp1, filename=lcp1name, overwrite=T)
#   
#   lcp1namecsv <- paste0("LeastCostPaths/MorroneRegional/", "LCP_", test_start$PAcomplex0, "_", end1$PAcomplex0,"_a", ".csv")
#   csv1 <- as.data.frame(lcp1)
#   csv1$iteration <- i
#   write.csv(csv1, lcp1namecsv, row.names=F)
#   
#   plotname1 <- paste0("LeastCostPaths/MorroneRegional/", "LCP_", test_start$PAcomplex0, "_", end1$PAcomplex0,"_a", ".jpeg")
#   jpeg(filename=plotname1, height=6, width=6, units='in', res=300)
#   plot(conductance_region)
#   plot(region, add=T)
#   plot(test_start, add=T, pch=19)
#   plot(end1, add=T, pch=15)
#   plot(lcp1, add=T)
#   dev.off()
#   
#   lcp2 <- create_lcp(condmat, origin=test_start, destination=end2, cost_distance=T)
#   lcp2$start_PAcomplexID <- test_start$PAcomplex0
#   lcp2$start_pais <- test_start$pais
#   lcp2$start_min_elev_m <- test_start$min_elev_m
#   lcp2$end_PAcomplexID <- end1$PAcomplex0
#   lcp2$end_pais <- end1$pais
#   lcp2$end_max_elev_m <- end1$max_elev_m
#   lcp2$lengthkm <- terra::perim(lcp2)/1000
#   lcp2$region <- test_start$Provincias
#   
#   lcp2name <- paste0("LeastCostPaths/MorroneRegional/", "LCP_", test_start$PAcomplex0, "_", end2$PAcomplex0,"_b", ".shp")
#   terra::writeVector(lcp2, filename=lcp2name, overwrite=T)
#   
#   lcp2namecsv <- paste0("LeastCostPaths/MorroneRegional/", "LCP_", test_start$PAcomplex0, "_", end2$PAcomplex0,"_b", ".csv")
#   csv2 <- as.data.frame(lcp2)
#   csv2$iteration <- i
#   write.csv(csv2, lcp2namecsv, row.names=F)
#   
#   plotname2 <- paste0("LeastCostPaths/MorroneRegional/", "LCP_", test_start$PAcomplex0, "_", end2$PAcomplex0,"_b", ".jpeg")
#   jpeg(filename=plotname2, height=6, width=6, units='in', res=300)
#   plot(conductance_region)
#   plot(region, add=T)
#   plot(test_start, add=T, pch=19)
#   plot(end2, add=T, pch=15)
#   plot(lcp2, add=T)
#   dev.off()
#   
#   lcp3 <- create_lcp(condmat, origin=test_start, destination=end3, cost_distance=T)
#   lcp3$start_PAcomplexID <- test_start$PAcomplex0
#   lcp3$start_pais <- test_start$pais
#   lcp3$start_min_elev_m <- test_start$min_elev_m
#   lcp3$end_PAcomplexID <- end1$PAcomplex0
#   lcp3$end_pais <- end1$pais
#   lcp3$end_max_elev_m <- end1$max_elev_m
#   lcp3$lengthkm <- terra::perim(lcp3)/1000
#   lcp3$region <- test_start$Provincias
#   
#   lcp3name <- paste0("LeastCostPaths/MorroneRegional/", "LCP_", test_start$PAcomplex0, "_", end3$PAcomplex0,"_c", ".shp")
#   terra::writeVector(lcp3, filename=lcp3name, overwrite=T)
#   
#   lcp3namecsv <- paste0("LeastCostPaths/MorroneRegional/", "LCP_", test_start$PAcomplex0, "_", end3$PAcomplex0,"_c", ".csv")
#   csv3 <- as.data.frame(lcp3)
#   csv3$iteration <- i
#   write.csv(csv3, lcp3namecsv, row.names=F)
#   
#   plotname3 <- paste0("LeastCostPaths/MorroneRegional/", "LCP_", test_start$PAcomplex0, "_", end3$PAcomplex0,"_c", ".jpeg")
#   jpeg(filename=plotname3, height=6, width=6, units='in', res=300)
#   plot(conductance_region)
#   plot(region, add=T)
#   plot(test_start, add=T, pch=19)
#   plot(end3, add=T, pch=15)
#   plot(lcp3, add=T)
#   dev.off()
#   
#   lcp4 <- create_lcp(condmat, origin=test_start, destination=end4, cost_distance=T)
#   lcp4$start_PAcomplexID <- test_start$PAcomplex0
#   lcp4$start_pais <- test_start$pais
#   lcp4$start_min_elev_m <- test_start$min_elev_m
#   lcp4$end_PAcomplexID <- end1$PAcomplex0
#   lcp4$end_pais <- end1$pais
#   lcp4$end_max_elev_m <- end1$max_elev_m
#   lcp4$lengthkm <- terra::perim(lcp4)/1000
#   lcp4$region <- test_start$Provincias
#   
#   lcp4name <- paste0("LeastCostPaths/MorroneRegional/", "LCP_", test_start$PAcomplex0, "_", end4$PAcomplex0,"_d", ".shp")
#   terra::writeVector(lcp4, filename=lcp4name, overwrite=T)
#   
#   lcp4namecsv <- paste0("LeastCostPaths/MorroneRegional/", "LCP_", test_start$PAcomplex0, "_", end4$PAcomplex0,"_d", ".csv")
#   csv4 <- as.data.frame(lcp4)
#   csv4$iteration <- i
#   write.csv(csv4, lcp4namecsv, row.names=F)
#   
#   plotname4 <- paste0("LeastCostPaths/MorroneRegional/", "LCP_", test_start$PAcomplex0, "_", end4$PAcomplex0,"_d", ".jpeg")
#   jpeg(filename=plotname4, height=6, width=6, units='in', res=300)
#   plot(conductance_region)
#   plot(region, add=T)
#   plot(test_start, add=T, pch=19)
#   plot(end4, add=T, pch=15)
#   plot(lcp4, add=T)
#   dev.off()
#   
#   lcp5 <- create_lcp(condmat, origin=test_start, destination=end5, cost_distance=T)
#   lcp5$start_PAcomplexID <- test_start$PAcomplex0
#   lcp5$start_pais <- test_start$pais
#   lcp5$start_min_elev_m <- test_start$min_elev_m
#   lcp5$end_PAcomplexID <- end1$PAcomplex0
#   lcp5$end_pais <- end1$pais
#   lcp5$end_max_elev_m <- end1$max_elev_m
#   lcp5$lengthkm <- terra::perim(lcp5)/1000
#   lcp5$region <- test_start$Provincias
#   
#   lcp5name <- paste0("LeastCostPaths/MorroneRegional/", "LCP_", test_start$PAcomplex0, "_", end5$PAcomplex0,"_e", ".shp")
#   terra::writeVector(lcp5, filename=lcp5name, overwrite=T)
#   
#   lcp5namecsv <- paste0("LeastCostPaths/MorroneRegional/", "LCP_", test_start$PAcomplex0, "_", end5$PAcomplex0,"_e", ".csv")
#   csv5 <- as.data.frame(lcp5)
#   csv5$iteration <- i
#   write.csv(csv5, lcp5namecsv, row.names=F)
#   
#   plotname5 <- paste0("LeastCostPaths/MorroneRegional/", "LCP_", test_start$PAcomplex0, "_", end5$PAcomplex0,"_e", ".jpeg")
#   jpeg(filename=plotname5, height=6, width=6, units='in', res=300)
#   plot(conductance_region)
#   plot(region, add=T)
#   plot(test_start, add=T, pch=19)
#   plot(end5, add=T, pch=15)
#   plot(lcp5, add=T)
#   dev.off()
#   
#   test_start = NULL
#   plotname1 = NULL
#   csv1 = NULL
#   lcp1name = NULL
#   lcp1namecsv = NULL
#   lcp1 = NULL
#   plotname2 = NULL
#   csv2 = NULL
#   lcp2name = NULL
#   lcp2namecsv = NULL
#   lcp2 = NULL
#   plotname3 = NULL
#   csv3 = NULL
#   lcp3name = NULL
#   lcp3namecsv = NULL
#   lcp3 = NULL
#   plotname4 = NULL
#   csv4 = NULL
#   lcp4name = NULL
#   lcp4namecsv = NULL
#   lcp4 = NULL
#   plotname5 = NULL
#   csv5 = NULL
#   lcp5name = NULL
#   lcp5namecsv = NULL
#   lcp5 = NULL
#   
#   end1 = NULL
#   end2 = NULL
#   end3 = NULL
#   end4 = NULL
#   end5 = NULL
#   
#   tstart = NULL
#   t1 = NULL
#   t2 = NULL
#   t3 = NULL
#   t4 = NULL
#   t5 = NULL
#   tmp_ras = NULL
#   tmp_condmat = NULL
# }
# 
# ### Combine individual least cost paths ####
# lcp_list <- list.files(path='C:/Users/immccull/Documents/AmazonClimateCorridors/LeastCostPaths/MorroneRegional', pattern='.shp', full.names=T)
# lcp_list <- lapply(lcp_list, terra::vect)
# x <- terra::vect(lcp_list)
# x <- terra::project(x, "EPSG:29172")
# #writeVector(x, filename='LeastCostPaths/CombinedLeastCostPaths/CombinedLeastCostPathsRegional.shp', overwrite=T)
# 
# plot(conductance)
# plot(amazon_study_area, add=T)
# plot(x, add=T)
# plot(end_pts, add=T, col='dodgerblue', pch=20)
# #plot(start_pts, add=T, col='gray80', pch=20)
# 
# 
# ## Calculate LCP density (per 5 sq km) (1 km ran out of memory)
# conductance_crop <- terra::crop(conductance, amazon_study_area, mask=T)
# #conductance_crop_1km <- terra::aggregate(conductance_crop, fact=2, fun='mean', na.rm=T) #fun doesn't matter; only care about res
# conductance_crop_5km <- terra::aggregate(conductance_crop, fact=10, fun='mean', na.rm=T)
# lcps_dens <- create_lcp_density(conductance_crop_5km, lcps = x)
