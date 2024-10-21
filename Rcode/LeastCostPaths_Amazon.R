######################## Amazon least cost paths ##################################
# Date: 6-6-24
# updated: 7-11-24; export combined LCP shapefile
# Author: Ian McCullough, immccull@gmail.com
###################################################################################

#### R libraries ####
library(terra)
library(leastcostpath)
library(sf)

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
start_pts <- terra::vect("start_nodes/start_nodes_amazon_points.shp")
start_pts <- terra::project(start_pts, "EPSG:29172")
start_polygons <- terra::vect("start_nodes/start_nodes_amazon_polygons.shp")
start_polygons <- terra::project(start_polygons, "EPSG:29172")

end_pts <- terra::vect("end_nodes/end_nodes_amazon_points.shp")
end_pts <- terra::project(end_pts, "EPSG:29172")
end_polygons <- terra::vect("end_nodes/end_nodes_amazon_polygons.shp")
end_polygons <- terra::project(end_polygons, "EPSG:29172")

#### Main program ####
# assign unique ID to all start and end nodes (this code works because pts and polygons derived from same sources)
start_pts$start_ID <- paste0("start_ID_", seq(1,nrow(start_pts)))
end_pts$end_ID <- paste0("end_ID_", seq(1,nrow(end_pts)))
start_polygons$start_ID <- paste0("start_ID_", seq(1,nrow(start_polygons)))
end_polygons$end_ID <- paste0("end_ID_", seq(1,nrow(end_polygons)))

# only retain start and end nodes within study area
start_pts <- terra::intersect(start_pts, amazon_study_area)

test <- terra::extract(conductance, start_pts) #also need to remove start nodes not in conductance surface area (likely in the water)
names(test) <- c('ID','cond')
nas <- which(is.na(test), arr.ind=T)[,1] #get row IDs of NAs
start_pts <- start_pts[-c(nas), ]

end_pts <- terra::intersect(end_pts, amazon_study_area)
start_polygons <- subset(start_polygons, start_polygons$start_ID %in% start_pts$start_ID)
end_polygons <- subset(end_polygons, end_polygons$end_ID %in% end_pts$end_ID)

plot(conductance)
plot(amazon_study_area, add=T)
plot(start_pts, add=T, col='black', pch=20)
plot(end_pts, add=T, col='blue', pch=20)

### Least cost paths
# conductance matrix
# this could run out of memory due to large number of cells
# to reduce the likelihood of this, we can buffer the study area by 500km
# and then clip out non-land areas to make the area even smaller
# amazon_study_area_500km_buff <- terra::buffer(amazon_study_area, width=500000)
# amazon_study_area_500km_buff_land <- terra::intersect(amazon_study_area_500km_buff, southamerica)
# amazon_study_area_500km_buff_land <- terra::aggregate(amazon_study_area_500km_buff_land) #get rid of country boundaries
# # plot(conductance)
# # plot(amazon_study_area, add=T)
# # plot(amazon_study_area_500km_buff_land, add=T)
# conductance_buffered <- terra::crop(conductance, amazon_study_area_500km_buff_land,
#                                     mask=T)
# plot(conductance_buffered)
# plot(amazon_study_area, add=T)
# ncell(conductance_buffered)

# conductance matrix: may run out of memory
# worked at 250 m (and likely would at 500 m), but then LCP loop ran out of memory
#condmat <- create_cs(conductance_buffered, neighbours=8, dem=NULL, max_slope=NULL)
#terra::writeRaster(condmat, "tump/condmat.tif", overwite=T)# doesn't work for "conductanceMatrix"

knear <- as.data.frame(terra::nearby(x=start_pts, y=end_pts, k=5))
nearest_distance <- as.data.frame(terra::nearest(start_pts, end_pts)) #essentially, use nearest function for k=1

end_pts$newrowID <- seq(1, nrow(end_pts), 1) #needed for subsetting within loop
end_polygons$newrowID <- seq(1, nrow(end_polygons), 1)

#problems: 185, 198 (in ocean?) #250 m res
# at 500 m res, crashed at i=538, but probably a memory issue
#for (i in 1:nrow(start_pts)) { #like this will run completely without crashing...let's see how far we get
for (i in 538:nrow(start_pts)) { #if need to start from not the first iteration
#for (i in 1:2) { #test: OK
  # First, extract start point and k nearest end points
  test_start <- start_pts[i] 
  test_end <- as.data.frame(terra::nearby(test_start, end_pts, k=5)) #only use closest end node
  #test_end <- subset(end_pts, end_pts$newrowID %in% test_end[,2:6])
  end1 <- subset(end_pts, end_pts$newrowID==test_end[,2])
  end2 <- subset(end_pts, end_pts$newrowID==test_end[,3])
  end3 <- subset(end_pts, end_pts$newrowID==test_end[,4])
  end4 <- subset(end_pts, end_pts$newrowID==test_end[,5])
  end5 <- subset(end_pts, end_pts$newrowID==test_end[,6])
  
  # because full study area conductance matrix runs out of memory
  # calculate part of it based on buffered box around start and end points
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
  t2 <- st_buffer(t1, 150000) #crashed a lot with 200000; used 200000 for the one that failed
  # CROP RASTER
  tmp_ras <- terra::crop(conductance, st_bbox(t2))
  # Calculate conductance matrix on this cropped raster
  tmp_condmat <- create_cs(tmp_ras, neighbours=8, dem=NULL, max_slope=NULL)
  #tmp_condmat <- condmat #may work at lower resolutions (issue is if loop crashes, have to recalculate), but actually seems slower to calculate LCPs
  
  # calculate least cost paths and save output
  lcp1 <- create_lcp(tmp_condmat, origin=test_start, destination=end1, cost_distance=T)
  lcp1$start_name <- test_start$nombre
  lcp1$start_pais <- test_start$pais
  lcp1$end_name <- end1$nombre
  lcp1$end_pais <- end1$pais
  lcp1$lengthkm <- terra::perim(lcp1)/1000
  
  lcp1name <- paste0("LeastCostPaths/", "LCP_", test_start$start_ID, "_", end1$end_ID,"_a", ".shp")
  terra::writeVector(lcp1, filename=lcp1name, overwrite=T)
  
  lcp1namecsv <- paste0("LeastCostPaths/", "LCP_", test_start$start_ID, "_", end1$end_ID,"_a", ".csv")
  csv1 <- as.data.frame(lcp1)
  csv1$iteration <- i
  write.csv(csv1, lcp1namecsv, row.names=F)
  
  plotname1 <- paste0("LeastCostPaths/", "LCP_", test_start$start_ID, "_", end1$end_ID,"_a", ".jpeg")
  jpeg(filename=plotname1, height=6, width=6, units='in', res=300)
  plot(conductance)
  plot(amazon_study_area, add=T)
  plot(test_start, add=T, pch=19)
  plot(end1, add=T, pch=15)
  plot(lcp1, add=T)
  dev.off()
  
  lcp2 <- create_lcp(tmp_condmat, origin=test_start, destination=end2, cost_distance=T)
  lcp2$start_name <- test_start$nombre
  lcp2$start_pais <- test_start$pais
  lcp2$end_name <- end2$nombre
  lcp2$end_pais <- end2$pais
  lcp2$lengthkm <- terra::perim(lcp2)/1000
  
  lcp2name <- paste0("LeastCostPaths/", "LCP_", test_start$start_ID, "_", end2$end_ID,"_b", ".shp")
  terra::writeVector(lcp2, filename=lcp2name, overwrite=T)
  
  lcp2namecsv <- paste0("LeastCostPaths/", "LCP_", test_start$start_ID, "_", end2$end_ID,"_b", ".csv")
  csv2 <- as.data.frame(lcp2)
  csv2$iteration <- i
  write.csv(csv2, lcp2namecsv, row.names=F)
  
  plotname2 <- paste0("LeastCostPaths/", "LCP_", test_start$start_ID, "_", end2$end_ID,"_b", ".jpeg")
  jpeg(filename=plotname2, height=6, width=6, units='in', res=300)
  plot(conductance)
  plot(amazon_study_area, add=T)
  plot(test_start, add=T, pch=19)
  plot(end2, add=T, pch=15)
  plot(lcp2, add=T)
  dev.off()
  
  lcp3 <- create_lcp(tmp_condmat, origin=test_start, destination=end3, cost_distance=T)
  lcp3$start_name <- test_start$nombre
  lcp3$start_pais <- test_start$pais
  lcp3$end_name <- end3$nombre
  lcp3$end_pais <- end3$pais
  lcp3$lengthkm <- terra::perim(lcp3)/1000
  
  lcp3name <- paste0("LeastCostPaths/", "LCP_", test_start$start_ID, "_", end3$end_ID,"_c", ".shp")
  terra::writeVector(lcp3, filename=lcp3name, overwrite=T)
  
  lcp3namecsv <- paste0("LeastCostPaths/", "LCP_", test_start$start_ID, "_", end3$end_ID,"_c", ".csv")
  csv3 <- as.data.frame(lcp3)
  csv3$iteration <- i
  write.csv(csv3, lcp3namecsv, row.names=F)
  
  plotname3 <- paste0("LeastCostPaths/", "LCP_", test_start$start_ID, "_", end3$end_ID,"_c", ".jpeg")
  jpeg(filename=plotname3, height=6, width=6, units='in', res=300)
  plot(conductance)
  plot(amazon_study_area, add=T)
  plot(test_start, add=T, pch=19)
  plot(end3, add=T, pch=15)
  plot(lcp3, add=T)
  dev.off()
  
  lcp4 <- create_lcp(tmp_condmat, origin=test_start, destination=end4, cost_distance=T)
  lcp4$start_name <- test_start$nombre
  lcp4$start_pais <- test_start$pais
  lcp4$end_name <- end4$nombre
  lcp4$end_pais <- end4$pais
  lcp4$lengthkm <- terra::perim(lcp4)/1000
  
  lcp4name <- paste0("LeastCostPaths/", "LCP_", test_start$start_ID, "_", end4$end_ID,"_d", ".shp")
  terra::writeVector(lcp4, filename=lcp4name, overwrite=T)
  
  lcp4namecsv <- paste0("LeastCostPaths/", "LCP_", test_start$start_ID, "_", end4$end_ID,"_d", ".csv")
  csv4 <- as.data.frame(lcp4)
  csv4$iteration <- i
  write.csv(csv4, lcp4namecsv, row.names=F)
  
  plotname4 <- paste0("LeastCostPaths/", "LCP_", test_start$start_ID, "_", end4$end_ID,"_d", ".jpeg")
  jpeg(filename=plotname4, height=6, width=6, units='in', res=300)
  plot(conductance)
  plot(amazon_study_area, add=T)
  plot(test_start, add=T, pch=19)
  plot(end4, add=T, pch=15)
  plot(lcp4, add=T)
  dev.off()
  
  lcp5 <- create_lcp(tmp_condmat, origin=test_start, destination=end5, cost_distance=T)
  lcp5$start_name <- test_start$nombre
  lcp5$start_pais <- test_start$pais
  lcp5$end_name <- end5$nombre
  lcp5$end_pais <- end5$pais
  lcp5$lengthkm <- terra::perim(lcp5)/1000
  
  lcp5name <- paste0("LeastCostPaths/", "LCP_", test_start$start_ID, "_", end5$end_ID,"_e", ".shp")
  terra::writeVector(lcp5, filename=lcp5name, overwrite=T)
  
  lcp5namecsv <- paste0("LeastCostPaths/", "LCP_", test_start$start_ID, "_", end5$end_ID,"_e", ".csv")
  csv5 <- as.data.frame(lcp5)
  csv5$iteration <- i
  write.csv(csv5, lcp5namecsv, row.names=F)
  
  plotname5 <- paste0("LeastCostPaths/", "LCP_", test_start$start_ID, "_", end5$end_ID,"_e", ".jpeg")
  jpeg(filename=plotname5, height=6, width=6, units='in', res=300)
  plot(conductance)
  plot(amazon_study_area, add=T)
  plot(test_start, add=T, pch=19)
  plot(end5, add=T, pch=15)
  plot(lcp5, add=T)
  dev.off()
  
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
}

### Combine individual least cost paths ####
lcp_list <- list.files(path='C:/Users/immccull/Documents/AmazonClimateCorridors/LeastCostPaths', pattern='.shp', full.names=T)
lcp_list <- lapply(lcp_list, terra::vect)
x <- terra::vect(lcp_list)
x <- terra::project(x, "EPSG:29172")
#writeVector(x, filename='LeastCostPaths/CombinedLeastCostPaths/CombinedLeastCostPaths.shp', overwrite=T)

plot(conductance)
plot(amazon_study_area, add=T)
plot(x, add=T)
plot(end_pts, add=T, col='dodgerblue', pch=20)
#plot(start_pts, add=T, col='gray80', pch=20)


## Calculate LCP density (per 5 sq km) (1 km ran out of memory)
conductance_crop <- terra::crop(conductance, amazon_study_area, mask=T)
#conductance_crop_1km <- terra::aggregate(conductance_crop, fact=2, fun='mean', na.rm=T) #fun doesn't matter; only care about res
conductance_crop_5km <- terra::aggregate(conductance_crop, fact=10, fun='mean', na.rm=T)
lcps_dens <- create_lcp_density(conductance_crop_5km, lcps = x)
