##################### Amazon least cost paths  ####################################
# Date: 4-15-24
# updated: 4-19-24: added Los Amigos, Peru
# Author: Ian McCullough, immccull@gmail.com
###################################################################################

#### R libraries ####
library(leastcostpath)
library(terra)
library(sf)

#### Input data ####
setwd("C:/Users/immccull/Documents/Amazon")

# Study area
amazon_study_area <- terra::vect("RAISG/StudyArea/western_Amazon_study_area1_29172.shp")

# Conductance surface
conductance <- terra::rast("Conductance/western_amazon_conductance.tif")

# Start and end nodes
start_all <- terra::vect("protected_areas/WDPA_SouthAmerica_lowland_pilot_5km.shp")
end_all <- terra::vect("protected_areas/WDPA_SouthAmerica_highland_pilot_5km.shp")
#end_all <- terra::rast("end_nodes/mh_1500m_else_NA.tif") #medium and high forest landscape integrity
mh_all <- terra::vect("end_nodes/mh_1500m_polygons.shp")

# DEM
#DEM <- terra::rast("DEM/SA_DEM_mosaic.tif")

#### Main program ####
## Prepare start and end nodes
# test using centroids of start and end PAs

# convert forest integrity into patches
# update: this is too slow; particularly considering QGIS Polygonize (raster to vector in GDAL raster conversion took < 30 seconds)
#end_patches <- terra::patches(end_all, directions=8, allowGaps=F, filename="end_nodes/mh_1500m_patches.tif")
#end_polygons <- terra::as.polygons(end_patches, na.rm=T)

# Give each one an ID code
#end_all$END_CODE <- paste0("E",sprintf("%04d", 1:nrow(end_all)))
start_all$START_CODE <- paste0("S",sprintf("%04d", 1:nrow(start_all)))

# Calculate elevation in end nodes to identify highland end nodes
#end_all_elev_mean <- terra::extract(DEM, end_all, fun="mean", na.rm=T)
#names(end_all_elev_mean) <- c('ID','mean_elev_m')
#end_highland <- subset(end_all_elev_mean, mean_elev_m <= 3500 & mean_elev_m >= 1500 )
#end_highland_polygons <- subset(end_all, end_all$fid %in% end_highland$ID)
#writeVector(end_highland_polygons, filename='end_nodes/mh_highland_polygons.shp', overwrite=T)

## Not used if only using protected areas as end nodes
# Calculate % protection of each highland end node
# update: much faster to use Overlap analysis in QGIS; provides area overlap and pct overlap
# had to fix geometries of input PA polygons first, however
# end_highland_polygons_wProt <- terra::vect("end_nodes/mh_highland_polygons_wProtection.shp")
# names(end_highland_polygons_wProt) <- c('fid','DN','END_CODE','PA_area','PA_pct')
# summary(end_highland_polygons_wProt$PA_pct)
# hist(end_highland_polygons_wProt$PA_pct, breaks=seq(0,100,1) ,xlab='Protection (%)',
#      main='End node protection (%)')

# Centroids of start and end areas
start_all <- terra::project(start_all, "EPSG:29172")
start_points <- terra::centroids(start_all, inside=T)
#writeVector(start_all, filename='start_nodes/start_nodes_pilot_5km_polygons.shp', overwrite=T)
#writeVector(start_points, filename='start_nodes/start_nodes_pilot_5km_points.shp', overwrite=T)

end_all <- terra::project(end_all, "EPSG:29172")
end_points <- terra::centroids(end_all, inside=T)
end_points$newrowID <- rownames(as.data.frame(end_points))
#writeVector(end_all, filename='end_nodes/end_nodes_pilot_5km_polygons.shp', overwrite=T)
#writeVector(end_points, filename='end_nodes/end_nodes_pilot_5km_points.shp', overwrite=T)

plot(amazon_study_area)
plot(start_points, add=T, col='forestgreen')
plot(end_points, add=T, col='dodgerblue')

plot(amazon_study_area)
plot(start_all, add=T, col='forestgreen')
#plot(start_points, add=T)
plot(end_points, add=T, col='dodgerblue')

# note names cutoff due to length (I guess)
start_all$lowarea_pct <- (start_all$lowareasqk/start_all$areasqkm)*100
summary(start_all$areasqkm)
summary(start_all$lowarea_pct)
hist(start_all$areasqkm, main='Full start node area', xlab='sq km',
     breaks=seq(0,43000,100))
hist(start_all$lowarea_pct, main='Start node percentage < 500 m', xlab='Percentage',
     breaks=seq(0,100,1))

end_all$higharea_pct <- (end_all$highareasq/end_all$areasqkm)*100
summary(end_all$areasqkm)
summary(end_all$higharea_pct)
hist(end_all$areasqkm, main='Full end node area', xlab='sq km',
     breaks=seq(0,22000,100))
hist(end_all$higharea_pct, main='End node percentage within 2500 m to 3500 m', xlab='Percentage',
     breaks=seq(0,100,1))

# end_highland_polygons_wProt_29172 <- terra::project(end_highland_polygons_wProt, "EPSG:29172")
# end_highland_polygons_wProt_29172$areasqkm <- terra::expanse(end_highland_polygons_wProt_29172, unit="km")
# end_highland_polygons_wProt_29172_5km <- subset(end_highland_polygons_wProt_29172, end_highland_polygons_wProt_29172$areasqkm >= 5)
# #writeVector(end_highland_polygons_wProt_29172_5km, filename='end_nodes/mh_highland_polygons_wProtection_5km.shp', overwrite=T)
# 
# hist(end_highland_polygons_wProt_29172_5km$PA_pct, breaks=seq(0,100,1) ,xlab='Protection (%)',
#      main='End node protection (%)')
# mtext(side=3, paste0('n=', nrow(end_highland_polygons_wProt_29172_5km)))
# summary(end_highland_polygons_wProt_29172_5km$PA_pct)
# summary(end_highland_polygons_wProt_29172_5km$areasqkm)
# 
# end_points <- terra::centroids(end_highland_polygons_wProt_29172_5km, inside=T)
# end_points$rowID <- rownames(as.data.frame(end_points))

# plot(amazon_study_area)
# plot(start_all, add=T, col='gray')
# plot(start_points, add=T, col='black')

## Least cost paths
# create conductance matrix
# however, this runs into memory limitations when running the entire thing
#condmat <- create_cs(conductance, neighbours=8, dem=NULL, max_slope=NULL)

# Nearest neighbor distance for each start point
knear <- as.data.frame(terra::nearest(start_points, end_points))
#650000/max(knear$distance)
#max(knear$distance) #could set this as buffer distance, but still may be too wide (take forever) or need to couch this even more for some very distant point pairs

# failed: WDPAID: 19769 (18th iteration)
#for (i in 1:nrow(start_points)) { #like this will run completely without crashing...
for (i in 86:nrow(start_points)) {
  # First, extract start point and nearest end point
  test_start <- start_points[i] #39 is longest, 64 is shortest
  test_end <- terra::nearby(test_start, end_points, k=1) #only use closest end node
  test_end <- as.data.frame(test_end)
  test_end <- subset(end_points, end_points$newrowID==test_end[,2])
  
  # Union start and end points and buffer result to crop out subset of conductance surface
  # but close together points do not need a very wide buffer
  # test_union <- terra::union(test_start, test_end)
  # test_union_line <- terra::as.lines(test_union)
  # #buffdist <- knear[i,5]*0.65 #meters #650000/max(knear$distance) = 0.61, so perhaps this 0.65 multiple is conservative enough
  # # but could be too small for nearby points, so only do this for larger distances
  # #buffdist <- ifelse(buffdist <= median(knear$distance), median(knear$distance), buffdist)
  # basedist <- knear[i,5]
  # 
  # # if we use a dynamic buffer, idea is to increase for short distance pairs
  # # and trim for long-distance pairs, but what is the rule?
  # buffdist <- ifelse(basedist <= 300000, basedist*2, basedist*0.6)
  # 
  # test_union_buffer <- terra::buffer(test_union_line, width=buffdist) #min=knear[64,5], max=1062295
  # test_union_buffer <- terra::aggregate(test_union_buffer) #dissolve point buffers into one polygon
  # 
  # # take subset of conductance surface so can make more manageable conductance matrix
  # test_union_condsurf <- terra::crop(conductance, test_union_buffer, mask=T) 
  # test_union_condmat <- create_cs(test_union_condsurf, neighbours=8, dem=NULL, max_slope=NULL)
  
  ## Try another way based on Chris' code from earlier project
  # uses sf tools and bounding box to buffer instead of terra circular buffer tool (perhaps more efficient)
  t1 <- st_as_sf(test_start)
  t2 <- st_as_sf(test_end)
  t1 <- rbind(st_coordinates(t1),st_coordinates(t2))
  t1 <- t1 %>%
    as.data.frame %>%
    sf::st_as_sf(coords = c(1,2))
  #plot(t1)
  # BUFFER
  t2 <- st_buffer(t1, 150000) #crashed a lot with 200000; used 200000 for the one that failed
  # CROP RASTER
  tmp_ras <- terra::crop(conductance, st_bbox(t2))
  # Calculate conductance matrix on this cropped raster
  tmp_condmat <- create_cs(tmp_ras, neighbours=8, dem=NULL, max_slope=NULL)
  
  # calculate least cost path
  test_lcp <- create_lcp(tmp_condmat, origin=test_start, destination=test_end, cost_distance=T)
  test_lcp$start_WDPAID <- test_start$WDPAID
  test_lcp$start_name <- test_start$NAME
  test_lcp$end_WDPAID <- test_end$WDPAID
  test_lcp$end_name <- test_end$NAME
  test_lcp$start_fullareasqkm <- test_start$areasqkm
  test_lcp$start_lowareasqkm <- test_start$lowareasqk #note: name truncated from using saved shp
  test_lcp$end_fullareasqkm <- test_end$areasqkm
  test_lcp$end_highareasqkm <- test_end$highareasq #note: name truncated from using saved shp
  test_lcp$start_lowland_pct <- (test_lcp$start_lowareasqkm/test_lcp$start_fullareasqkm)*100
  test_lcp$end_highland_pct <- (test_lcp$end_highareasqkm/test_lcp$end_fullareasqkm)*100
  test_lcp$lengthkm <- terra::perim(test_lcp)/1000
  
  # save output
  lcpname <- paste0("LeastCostPaths/", "LCP_S", test_start$WDPAID, "_E", test_end$WDPAID, ".shp")
  terra::writeVector(test_lcp, filename=lcpname, overwrite=T)
  
  lcpnamecsv <- paste0("LeastCostPaths/", "LCP_S", test_start$WDPAID, "_E", test_end$WDPAID, ".csv")
  csv <- as.data.frame(test_lcp)
  #csv$START_CODE <- test_start$START_CODE
  #csv$START_WDPAID <- test_start$WDPAID
  #csv$END_CODE <- test_end$END_CODE
  write.csv(csv, lcpnamecsv, row.names=F)
  
  plotname <- paste0("LeastCostPaths/", "LCP_S", test_start$WDPAID, "_E", test_end$WDPAID, ".jpeg")
  jpeg(filename=plotname, height=6, width=6, units='in', res=300)
    plot(amazon_study_area)
    plot(tmp_ras, add=T)
    plot(test_start, add=T, pch=19)
    plot(test_end, add=T, pch=15)
    plot(test_lcp, add=T)
  dev.off()
  
  plotname = NULL
  csv = NULL
  lcpname = NULL
  lcpnamecsv = NULL
  test_start = NULL
  test_end = NULL
  test_lcp = NULL
  test_union = NULL
  test_union_buffer = NULL
  test_union_line = NULL
  test_union_condmat = NULL
  test_union_condsurf = NULL
  basedist = NULL
  buffdist = NULL
  t1 = NULL
  t2 = NULL
  tmp_ras = NULL
  tmp_condmat = NULL
}



### if want multiple LCPs per origin 
# test_end1 <- subset(end_points, end_points$fid==test_end[2])
# test_end2 <- subset(end_points, end_points$fid==test_end[3])
# test_end3 <- subset(end_points, end_points$fid==test_end[4])
# plot(amazon_study_area)
# plot(test_start, add=T, col='blue')
# plot(test_end1, add=T, col='red')
# plot(test_end2, add=T, col='gold')
# plot(test_end3, add=T, col='green')


### Combine individual least cost paths ####
lcp_list <- list.files(path='C:/Users/immccull/Documents/Amazon/LeastCostPaths', pattern='.shp', full.names=T)
lcp_list <- lapply(lcp_list, terra::vect)
x <- terra::vect(lcp_list)
x <- terra::project(x, "EPSG:29172")

# no longer needed (built into LCP attributes)
# csv_list <- list.files(path='C:/Users/immccull/Documents/Amazon/LeastCostPaths', pattern='.csv', full.names=T)
# csv_list <- lapply(csv_list, read.csv)
# lcp_attributes <- do.call(rbind.data.frame, csv_list)

#writeVector(x, filename='LeastCostPaths/CombinedLeastCostPaths/CombinedLeastCostPaths.shp', overwrite=T)

plot(amazon_study_area)
plot(start_points, add=T, col='forestgreen')
end_points_x <- subset(end_points, end_points$WDPAID %in% x$end_WDPAID)
plot(end_points_x, add=T, col='dodgerblue')
plot(x, add=T, col='black')

mh_all_29172 <- terra::project(mh_all, "EPSG:29172")
mh_all_29172_dissolved <- terra::aggregate(mh_all_29172)
plot(amazon_study_area)
plot(mh_all_29172_dissolved, add=T, col='gray')
plot(start_all, add=T, col='forestgreen')
plot(end_all, add=T, col='dodgerblue')
plot(x, add=T, col='black')

# plot(amazon_study_area)
# plot(start_all, add=T, col='forestgreen')
# plot(end_points, add=T, col='dodgerblue')
# plot(x, add=T, col='black')
