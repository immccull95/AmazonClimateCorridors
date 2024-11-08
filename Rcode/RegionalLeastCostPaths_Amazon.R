######################## Amazon least cost paths ##################################
# Date: 10-23-24
# updated: 
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

# update 11-4-24: create 3 regional subsets
chacoan <- subset(morrone_2022_neotropics, morrone_2022_neotropics$Provincias=="Xingu-Tapajos province")
#terra::writeVector(chacoan, filename='Regions/Morrone2022/chacoan.shp', overwrite=T)
boreal <- subset(morrone_2022_neotropics, morrone_2022_neotropics$Provincias %in% 
                   c('Guianan Lowlands province','Guianan province',
                     'Imeri province','Napo province','Para province',
                     'Roraima province'))
#terra::writeVector(boreal, filename='Regions/Morrone2022/boreal.shp', overwrite=T)
south <- subset(morrone_2022_neotropics, morrone_2022_neotropics$Provincias %in% 
                  c('Madeira province','Rondonia province',
                    'Ucayali province','Yungas province'))
#terra::writeVector(south, filename='Regions/Morrone2022/south.shp', overwrite=T)

# Amazon study area
amazon_study_area <- terra::vect("RAISG/Limites2023/Limites/Lim_Biogeografico.shp")
amazon_study_area <- terra::project(amazon_study_area, "EPSG:29172")

plot(amazon_study_area)
plot(chacoan, add=T, col='dodgerblue')
plot(boreal, add=T, col='forestgreen')
plot(south, add=T, col='gold')

# Conductance surface (modified based on canopy height)
#conductance <- terra::rast("Conductance/amazon_conductance_250m.tif")
conductance <- terra::rast("Conductance/amazon_conductance_500m.tif")

# Start and end nodes
# these are stored in region by region shapefiles (pts and polygons)
# eventually use loop to call them in?
# using one region for now
pts <- terra::vect("protected_areas/Amazon_merged_PAs/regional_PA_complexes/PA_complex_Guianan Lowlands province_pts.shp")
pts <- terra::project(pts, "EPSG:29172")
region <- pts$Provincias[1]
region <- subset(morrone2022_amazon, morrone2022_amazon$Provincias==region)

#### Main program ####
# assign unique ID to all start and end nodes (this code works because pts and polygons derived from same sources)
start_pts$start_ID <- paste0("start_ID_", seq(1,nrow(start_pts)))
end_pts$end_ID <- paste0("end_ID_", seq(1,nrow(end_pts)))
# start_polygons$start_ID <- paste0("start_ID_", seq(1,nrow(start_polygons)))
# end_polygons$end_ID <- paste0("end_ID_", seq(1,nrow(end_polygons)))

test <- terra::extract(conductance, pts) #also need to remove start nodes not in conductance surface area (likely in the water)
names(test) <- c('ID','cond')
nas <- which(is.na(test), arr.ind=T)[,1] #get row IDs of NAs
pts <- start_pts[-c(nas), ]

start_pts <- subset(pts, pts$node=='Start')
end_pts <- subset(pts, pts$node=='End')

# end_pts <- terra::intersect(end_pts, amazon_study_area)
# start_polygons <- subset(start_polygons, start_polygons$start_ID %in% start_pts$start_ID)
# end_polygons <- subset(end_polygons, end_polygons$end_ID %in% end_pts$end_ID)

plot(conductance)
#plot(amazon_study_area, add=T)
plot(morrone2022_amazon, add=T)
plot(start_pts, add=T, col='black', pch=20)
plot(end_pts, add=T, col='blue', pch=20)

### Least cost paths
# conductance matrix

# For the regional approach, try buffering by 100 km
region_buffered <- terra::buffer(region, width=100000)
conductance_region <- terra::crop(conductance, region_buffered, mask=T)
ncell(conductance_region)

plot(conductance_region)
plot(region, add=T)
plot(start_pts, add=T)
plot(end_pts, add=T, col='orange')

# conductance matrix: may run out of memory here or during LCP loop, 
# but more likely to be OK using regional approach
# I am not aware of a way to save this intermediate output (writeRaster does not work for this type of file)
condmat <- create_cs(conductance_region, neighbours=8, dem=NULL, max_slope=NULL)
lcp_neighbors <- 5
knear <- as.data.frame(terra::nearby(x=start_pts, y=end_pts, k=lcp_neighbors))
nearest_distance <- as.data.frame(terra::nearest(start_pts, end_pts)) #essentially, use nearest function for k=1

end_pts$newrowID <- seq(1, nrow(end_pts), 1) #needed for subsetting within loop
#end_polygons$newrowID <- seq(1, nrow(end_polygons), 1)


for (i in 1:nrow(start_pts)) { #like this will run completely without crashing...let's see how far we get
#for (i in 1:2) { #testing loop
#for (i in 1:nrow(start_pts)) { #if need to start from not the first iteration
  # First, extract start point and k nearest end points
  test_start <- start_pts[i] 
  test_end <- as.data.frame(terra::nearby(test_start, end_pts, k=lcp_neighbors))
  #test_end <- subset(end_pts, end_pts$newrowID %in% test_end[,2:6])
  end1 <- subset(end_pts, end_pts$newrowID==test_end[,2])
  end2 <- subset(end_pts, end_pts$newrowID==test_end[,3])
  end3 <- subset(end_pts, end_pts$newrowID==test_end[,4])
  end4 <- subset(end_pts, end_pts$newrowID==test_end[,5])
  end5 <- subset(end_pts, end_pts$newrowID==test_end[,6])
  
  ## this chunk might not be needed at regional scale
  # # because full study area conductance matrix runs out of memory
  # # calculate part of it based on buffered box around start and end points
  # tstart <- st_as_sf(test_start)
  # t1 <- st_as_sf(end1)
  # t2 <- st_as_sf(end2)
  # t3 <- st_as_sf(end3)
  # t4 <- st_as_sf(end4)
  # t5 <- st_as_sf(end5)
  # t1 <- rbind(st_coordinates(tstart),st_coordinates(t1),st_coordinates(t2),
  #             st_coordinates(t3),st_coordinates(t4),st_coordinates(t5))
  # t1 <- t1 %>%
  #   as.data.frame %>%
  #   sf::st_as_sf(coords = c(1,2))
  # #plot(t1)
  # # BUFFER
  # t2 <- st_buffer(t1, 150000) #crashed a lot with 200000; used 200000 for the one that failed
  # # CROP RASTER
  # tmp_ras <- terra::crop(conductance, st_bbox(t2))
  # # Calculate conductance matrix on this cropped raster
  # tmp_condmat <- create_cs(tmp_ras, neighbours=8, dem=NULL, max_slope=NULL)
  # #tmp_condmat <- condmat #may work at lower resolutions (issue is if loop crashes, have to recalculate), but actually seems slower to calculate LCPs
  # 
  # calculate least cost paths and save output
  lcp1 <- create_lcp(condmat, origin=test_start, destination=end1, cost_distance=T)
  lcp1$start_PAcomplexID <- test_start$PAcomplex0
  lcp1$start_pais <- test_start$pais
  lcp1$start_min_elev_m <- test_start$min_elev_m
  lcp1$end_PAcomplexID <- end1$PAcomplex0
  lcp1$end_pais <- end1$pais
  lcp1$end_max_elev_m <- end1$max_elev_m
  lcp1$lengthkm <- terra::perim(lcp1)/1000
  lcp1$region <- test_start$Provincias
  
  lcp1name <- paste0("LeastCostPaths/MorroneRegional/", "LCP_", test_start$PAcomplex0, "_", end1$PAcomplex0,"_a", ".shp")
  terra::writeVector(lcp1, filename=lcp1name, overwrite=T)
  
  lcp1namecsv <- paste0("LeastCostPaths/MorroneRegional/", "LCP_", test_start$PAcomplex0, "_", end1$PAcomplex0,"_a", ".csv")
  csv1 <- as.data.frame(lcp1)
  csv1$iteration <- i
  write.csv(csv1, lcp1namecsv, row.names=F)
  
  plotname1 <- paste0("LeastCostPaths/MorroneRegional/", "LCP_", test_start$PAcomplex0, "_", end1$PAcomplex0,"_a", ".jpeg")
  jpeg(filename=plotname1, height=6, width=6, units='in', res=300)
  plot(conductance_region)
  plot(region, add=T)
  plot(test_start, add=T, pch=19)
  plot(end1, add=T, pch=15)
  plot(lcp1, add=T)
  dev.off()
  
  lcp2 <- create_lcp(condmat, origin=test_start, destination=end2, cost_distance=T)
  lcp2$start_PAcomplexID <- test_start$PAcomplex0
  lcp2$start_pais <- test_start$pais
  lcp2$start_min_elev_m <- test_start$min_elev_m
  lcp2$end_PAcomplexID <- end1$PAcomplex0
  lcp2$end_pais <- end1$pais
  lcp2$end_max_elev_m <- end1$max_elev_m
  lcp2$lengthkm <- terra::perim(lcp2)/1000
  lcp2$region <- test_start$Provincias
  
  lcp2name <- paste0("LeastCostPaths/MorroneRegional/", "LCP_", test_start$PAcomplex0, "_", end2$PAcomplex0,"_b", ".shp")
  terra::writeVector(lcp2, filename=lcp2name, overwrite=T)
  
  lcp2namecsv <- paste0("LeastCostPaths/MorroneRegional/", "LCP_", test_start$PAcomplex0, "_", end2$PAcomplex0,"_b", ".csv")
  csv2 <- as.data.frame(lcp2)
  csv2$iteration <- i
  write.csv(csv2, lcp2namecsv, row.names=F)
  
  plotname2 <- paste0("LeastCostPaths/MorroneRegional/", "LCP_", test_start$PAcomplex0, "_", end2$PAcomplex0,"_b", ".jpeg")
  jpeg(filename=plotname2, height=6, width=6, units='in', res=300)
  plot(conductance_region)
  plot(region, add=T)
  plot(test_start, add=T, pch=19)
  plot(end2, add=T, pch=15)
  plot(lcp2, add=T)
  dev.off()
  
  lcp3 <- create_lcp(condmat, origin=test_start, destination=end3, cost_distance=T)
  lcp3$start_PAcomplexID <- test_start$PAcomplex0
  lcp3$start_pais <- test_start$pais
  lcp3$start_min_elev_m <- test_start$min_elev_m
  lcp3$end_PAcomplexID <- end1$PAcomplex0
  lcp3$end_pais <- end1$pais
  lcp3$end_max_elev_m <- end1$max_elev_m
  lcp3$lengthkm <- terra::perim(lcp3)/1000
  lcp3$region <- test_start$Provincias
  
  lcp3name <- paste0("LeastCostPaths/MorroneRegional/", "LCP_", test_start$PAcomplex0, "_", end3$PAcomplex0,"_c", ".shp")
  terra::writeVector(lcp3, filename=lcp3name, overwrite=T)
  
  lcp3namecsv <- paste0("LeastCostPaths/MorroneRegional/", "LCP_", test_start$PAcomplex0, "_", end3$PAcomplex0,"_c", ".csv")
  csv3 <- as.data.frame(lcp3)
  csv3$iteration <- i
  write.csv(csv3, lcp3namecsv, row.names=F)
  
  plotname3 <- paste0("LeastCostPaths/MorroneRegional/", "LCP_", test_start$PAcomplex0, "_", end3$PAcomplex0,"_c", ".jpeg")
  jpeg(filename=plotname3, height=6, width=6, units='in', res=300)
  plot(conductance_region)
  plot(region, add=T)
  plot(test_start, add=T, pch=19)
  plot(end3, add=T, pch=15)
  plot(lcp3, add=T)
  dev.off()
  
  lcp4 <- create_lcp(condmat, origin=test_start, destination=end4, cost_distance=T)
  lcp4$start_PAcomplexID <- test_start$PAcomplex0
  lcp4$start_pais <- test_start$pais
  lcp4$start_min_elev_m <- test_start$min_elev_m
  lcp4$end_PAcomplexID <- end1$PAcomplex0
  lcp4$end_pais <- end1$pais
  lcp4$end_max_elev_m <- end1$max_elev_m
  lcp4$lengthkm <- terra::perim(lcp4)/1000
  lcp4$region <- test_start$Provincias
  
  lcp4name <- paste0("LeastCostPaths/MorroneRegional/", "LCP_", test_start$PAcomplex0, "_", end4$PAcomplex0,"_d", ".shp")
  terra::writeVector(lcp4, filename=lcp4name, overwrite=T)
  
  lcp4namecsv <- paste0("LeastCostPaths/MorroneRegional/", "LCP_", test_start$PAcomplex0, "_", end4$PAcomplex0,"_d", ".csv")
  csv4 <- as.data.frame(lcp4)
  csv4$iteration <- i
  write.csv(csv4, lcp4namecsv, row.names=F)
  
  plotname4 <- paste0("LeastCostPaths/MorroneRegional/", "LCP_", test_start$PAcomplex0, "_", end4$PAcomplex0,"_d", ".jpeg")
  jpeg(filename=plotname4, height=6, width=6, units='in', res=300)
  plot(conductance_region)
  plot(region, add=T)
  plot(test_start, add=T, pch=19)
  plot(end4, add=T, pch=15)
  plot(lcp4, add=T)
  dev.off()
  
  lcp5 <- create_lcp(condmat, origin=test_start, destination=end5, cost_distance=T)
  lcp5$start_PAcomplexID <- test_start$PAcomplex0
  lcp5$start_pais <- test_start$pais
  lcp5$start_min_elev_m <- test_start$min_elev_m
  lcp5$end_PAcomplexID <- end1$PAcomplex0
  lcp5$end_pais <- end1$pais
  lcp5$end_max_elev_m <- end1$max_elev_m
  lcp5$lengthkm <- terra::perim(lcp5)/1000
  lcp5$region <- test_start$Provincias
  
  lcp5name <- paste0("LeastCostPaths/MorroneRegional/", "LCP_", test_start$PAcomplex0, "_", end5$PAcomplex0,"_e", ".shp")
  terra::writeVector(lcp5, filename=lcp5name, overwrite=T)
  
  lcp5namecsv <- paste0("LeastCostPaths/MorroneRegional/", "LCP_", test_start$PAcomplex0, "_", end5$PAcomplex0,"_e", ".csv")
  csv5 <- as.data.frame(lcp5)
  csv5$iteration <- i
  write.csv(csv5, lcp5namecsv, row.names=F)
  
  plotname5 <- paste0("LeastCostPaths/MorroneRegional/", "LCP_", test_start$PAcomplex0, "_", end5$PAcomplex0,"_e", ".jpeg")
  jpeg(filename=plotname5, height=6, width=6, units='in', res=300)
  plot(conductance_region)
  plot(region, add=T)
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
lcp_list <- list.files(path='C:/Users/immccull/Documents/AmazonClimateCorridors/LeastCostPaths/MorroneRegional', pattern='.shp', full.names=T)
lcp_list <- lapply(lcp_list, terra::vect)
x <- terra::vect(lcp_list)
x <- terra::project(x, "EPSG:29172")
#writeVector(x, filename='LeastCostPaths/CombinedLeastCostPaths/CombinedLeastCostPathsRegional.shp', overwrite=T)

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
