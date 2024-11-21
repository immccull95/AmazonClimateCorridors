################## Exploring Amazon regionalization ###############################
# Date: 7-30-24
# updated: 11-8-24 with new Morrone (2022) regions and subregions
# Author: Ian McCullough, immccull@gmail.com
###################################################################################

#### R libraries ####
library(terra)
library(factoextra)
library(dplyr)

#### Input data ####
setwd("C:/Users/immccull/Documents/AmazonClimateCorridors")

# Biogeographical regions
# from: https://mapress.com/zt/article/view/zootaxa.3802.2.12/36874
#morrone <- terra::vect("Regions/Morrone/Lowenberg_Neto_2014.shp")
morrone2022 <- terra::vect("Regions/Morrone2022/NeotropicMap_SIRGAS2000.shp")
morrone2022_amazonclip <- terra::vect("Regions/Morrone2022/Amazon_macroregions_fixedgeom_Amazonclip.shp")
morrone2022_amazonclip_4674 <- terra::project(morrone2022_amazonclip, "EPSG:4674")

# DEM
DEM <- terra::rast("DEM/SA_DEM_mosaic.tif")
DEM_4674 <- terra::rast("DEM/SA_DEM_mosaic_4674.tif")
DEM_4674_morrone <- terra::rast("DEM/SA_DEM_mosaic_4674_morrone.tif")

# study area
amazon_study_area <- terra::vect("RAISG/Limites2023/Limites/Lim_Biogeografico.shp")

# dissolved protected areas with macroregion (south, boreal, choacoan) as attribute
# done in QGIS (Spatial join)
# some protected area complexes have null macroregion because outside these 3 regions
dissolved_PA_10km <- terra::vect("protected_areas/Amazon_merged_PAs/Amazon_merged_PAs_dissolved10km_wRegion.shp")

#### Main program ####
# put into same CRS as elevation data
morrone_4326 <- terra::project(morrone2022, "EPSG:4326")
amazon_study_area_4326 <- terra::project(amazon_study_area, "EPSG:4326")

# identify Amazonian Morrone regions
plot(morrone_4326)
plot(amazon_study_area_4326, add=T, col='blue')

#morrone_4326_amazon <- terra::intersect(morrone_4326, amazon_study_area_4326)
#plot(morrone_4326_amazon)
#unique(morrone_4326_amazon$Province_1)

# provinces_keep <- c('Pará province','Xingu-Tapajós province','Madeira province',
#                     'Rondônia province','Ucayali province','Napo province',
#                     'Imerí province','Pantepui province','Roraima province',
#                     'Guianan Lowlands province')
provinces_keep <- c('Guianan Lowlands province','Guianan province',
  'Imeri province','Napo province','Para province',
  'Roraima province','Madeira province','Rondonia province',
  'Ucayali province','Yungas province','Xingu-Tapajos province')

morrone_4326_amazon <- subset(morrone_4326, morrone_4326$Provincias %in% provinces_keep)

plot(amazon_study_area_4326)
plot(morrone_4326_amazon, add=T, col='green')

south <- terra::vect("Regions/Morrone2022/south.shp")
boreal <- terra::vect("Regions/Morrone2022/boreal.shp")
chacoan <- terra::vect("Regions/Morrone2022/chacoan.shp")

south_dissolved <- terra::aggregate(south, dissolve=T)
boreal_dissolved <- terra::aggregate(boreal, dissolve=T)

## Calculate elevational gradients within each region
# warning: can take hours
# morrone_4326_amazon_elev_mean <- terra::extract(DEM, morrone_4326_amazon, fun="mean", na.rm=T)
# morrone_4326_amazon_elev_min <- terra::extract(DEM, morrone_4326_amazon, fun="min", na.rm=T)
# morrone_4326_amazon_elev_max <- terra::extract(DEM, morrone_4326_amazon, fun="max", na.rm=T)
# morrone_4326_amazon_elev_median <- terra::extract(DEM, morrone_4326_amazon, fun="median", na.rm=T)
# 
# morrone_amazon_elev_df <- cbind.data.frame(morrone_4326_amazon_elev_mean, morrone_4326_amazon_elev_min,
#                                            morrone_4326_amazon_elev_max, morrone_4326_amazon_elev_median)
# morrone_amazon_elev_df <- morrone_amazon_elev_df[,c(2,4,6,8)]
# names(morrone_amazon_elev_df) <- c('elev_mean_m','elev_min_m','elev_max_m','elev_median_m')
# morrone_amazon_elev_df$province <- morrone_4326_amazon$Provincias
#write.csv(morrone_amazon_elev_df, "Regions/Morrone/Morrone2022_regions_elev_stats.csv", row.names=F)

# dissolve macro-regions: boreal, south (chacoan is only one region anyway)
# very slow
# boreal_elev_mean <- terra::extract(DEM, boreal_dissolved, fun="mean", na.rm=T)
# boreal_elev_min <- terra::extract(DEM, boreal_dissolved, fun="min", na.rm=T)
# boreal_elev_max <- terra::extract(DEM, boreal_dissolved, fun="max", na.rm=T)
# boreal_elev_median <- terra::extract(DEM, boreal_dissolved, fun="median", na.rm=T)
# 
# boreal_df <- cbind.data.frame(boreal_elev_mean, boreal_elev_min,
#                               boreal_elev_max, boreal_elev_median)
# boreal_df <- boreal_elev_df[,c(2,4,6,8)]
# names(boreal_df) <- c('elev_mean_m','elev_min_m','elev_max_m','elev_median_m')
# #write.csv(boreal_df, "Regions/Morrone/boreal_region_elev_stats.csv", row.names=F)

south_dissolved_4326 <- terra::project(south_dissolved, "EPSG:4326")
boreal_dissolved_4326 <- terra::project(boreal_dissolved, "EPSG:4326")
chacoan_4326 <- terra::project(chacoan, "EPSG:4326")

# Crop out portions of DEM in respective regions
DEM_south <- terra::crop(DEM, south_dissolved_4326, mask=T, filename="DEM/DEM_south_region.tif", overwrite=T)
DEM_boreal <- terra::crop(DEM, boreal_dissolved_4326, mask=T, filename="DEM/DEM_boreal_region.tif", overwrite=T)
DEM_chacoan <- terra::crop(DEM, chacoan_4326, mask=T, filename="DEM/DEM_chacoan_region.tif", overwrite=T)

# Extract DEM within protected area complexes
dissolved_PA_10km_4326 <- terra::project(dissolved_PA_10km, "EPSG:4326")
#DEM_PAs <- terra::crop(DEM, dissolved_PA_10km_4326, mask=T)
#terra::project(DEM_PAs, "EPSG:4674", filename='DEM/PA_complexes_4674.tif')
DEM_PAs <- terra::rast("DEM/PA_complexes_4674.tif")
south_4674 <- terra::project(south, "EPSG:4674")
boreal_4674 <- terra::project(boreal, "EPSG:4674")
chacoan_4674 <- terra::project(chacoan, "EPSG:4674")

plot(DEM_PAs)
plot(south_4674, add=T)

DEM_PAS_south <- terra::crop(DEM_PAs, south_4674, mask=T)
plot(DEM_PAS_south)
plot(south_4674, add=T)
summary(DEM_PAS_south)
hist(DEM_PAS_south)
global(DEM_PAS_south, quantile, probs=seq(0,1,0.1), na.rm=T)

DEM_PAS_chacoan <- terra::crop(DEM_PAs, chacoan_4674, mask=T)
plot(DEM_PAS_chacoan)
plot(chacoan_4674, add=T)
summary(DEM_PAS_chacoan)
hist(DEM_PAS_chacoan)
global(DEM_PAS_chacoan, quantile, probs=seq(0,1,0.1), na.rm=T)

DEM_PAS_boreal <- terra::crop(DEM_PAs, boreal_4674, mask=T)
plot(DEM_PAS_boreal)
plot(boreal_4674, add=T)
global(DEM_PAS_boreal, quantile, probs=seq(0,1,0.1), na.rm=T)

## Get area of each region
region_areasqkm <- terra::expanse(morrone2022_4674_amazon, "km")
region_clipped_areasqkm <- terra::expanse(morrone2022_amazonclip_4674, "km")
sum(region_clipped_areasqkm)
amazon_biogeog_areasqkm <- terra::expanse(amazon_study_area, "km")
sum(region_clipped_areasqkm)/amazon_biogeog_areasqkm #% of provinces that cover Amazon biogeographic region

## What are we doing for start/end nodes?
# Experimenting with Chacoan region
# chacoan_quant <- as.data.frame(t(global(DEM_PAS_chacoan, quantile, probs=seq(0,1,0.1), na.rm=T)))
# chacoan_quant$percentile <- seq(0,1,0.1)*100
# chacoan_quant$RK <- c(seq(1,10,1),NA)
# names(chacoan_quant) <- c('DEM','percentile','RK')
# 
# chacoan_mat <- c(chacoan_quant[1,1], chacoan_quant[2,1], chacoan_quant[1,3],
#                  chacoan_quant[2,1], chacoan_quant[3,1], chacoan_quant[2,3],
#                  chacoan_quant[3,1], chacoan_quant[4,1], chacoan_quant[3,3],
#                  chacoan_quant[4,1], chacoan_quant[5,1], chacoan_quant[4,3],
#                  chacoan_quant[5,1], chacoan_quant[6,1], chacoan_quant[5,3],
#                  chacoan_quant[6,1], chacoan_quant[7,1], chacoan_quant[6,3],
#                  chacoan_quant[7,1], chacoan_quant[8,1], chacoan_quant[7,3],
#                  chacoan_quant[8,1], chacoan_quant[9,1], chacoan_quant[8,3],
#                  chacoan_quant[9,1], chacoan_quant[10,1], chacoan_quant[9,3],
#                  chacoan_quant[10,1], chacoan_quant[11,1], chacoan_quant[10,3])
# chacoan_mat <- matrix(chacoan_mat, ncol=3, byrow=T)
# chacoan_RK <- terra::classify(DEM_PAS_chacoan, chacoan_mat, include.lowest=T)
# plot(chacoan_RK)
# plot(chacoan_4326, add=T)
# chacoan_RK_90 <- terra::ifel(chacoan_RK>=9, 1, NA)
# plot(chacoan_RK_90)
# plot(chacoan_4326, add=T)
# plot(dissolved_PA_10km_4326, add=T)
# # this is probably too much territory and too many patches
# chacoan_RK_90_patches <- terra::patches(chacoan_RK_90, directions=8)
# plot(chacoan_RK_90_patches)
# plot(chacoan_4326, add=T)
# 
# ## We could possibly use our standard thresholds for Boreal and South regions because those elevations exist there
# boreal_lowland <- terra::ifel(DEM_PAS_boreal <= 500, 1, NA)
# boreal_highland <- terra::ifel(DEM_PAS_boreal >= 1500, 1, NA)
# plot(boreal_4674)
# plot(boreal_lowland, col='gold', legend=F, add=T)
# plot(boreal_highland, col='dodgerblue', add=T, legend=F)
# legend(-53,11, pch=c(15,15), legend=c("Lowland (< 500m)","Highland (> 1500m)"), 
#        col=c("gold","dodgerblue"), bty='n', ncol=1)
# hist(DEM_PAS_boreal, main='Boreal', xlab='Elevation (m)')
# 
# hist(DEM_PAS_chacoan, main='Chacoan', xlab='Elevation (m)')
# 
# 
# south_lowland <- terra::ifel(DEM_PAS_south <= 500, 1, NA)
# south_highland <- terra::ifel(DEM_PAS_south >= 1500, 1, NA)
# plot(south_4674)
# #plot(dissolved_PA_10km, col='gray', add=T)
# plot(south_lowland, col='gold', add=T, legend=F)
# plot(south_highland, col='dodgerblue', add=T, legend=F)
# legend(-80,-20, pch=c(15,15), legend=c("Lowland (< 500m)","Highland (> 1500m)"), 
#        col=c("gold","dodgerblue"), bty='n', ncol=1, cex=0.75)
# hist(DEM_PAS_south, main='South', xlab='Elevation (m)')
# 
# #south_highland_polygons <- terra::as.polygons(south_highland) #took maybe a minute, but returned one large feature. Making aggregate=F threw error that raster is too large
# #writeRaster(south_highland, filename='DEM/south_highland.tif', overwrite=T)
# 
# chacoan_over500m <- terra::ifel(DEM_PAS_chacoan >= 500, 1, NA)
# chacoan_over600m <- terra::ifel(DEM_PAS_chacoan >= 600, 1, NA)
# chacoan_over700m <- terra::ifel(DEM_PAS_chacoan >= 700, 1, NA)
# chacoan_over800m <- terra::ifel(DEM_PAS_chacoan >= 800, 1, NA)
# plot(chacoan_4674, lwd=2)
# plot(dissolved_PA_10km, add=T, col='gray')
# plot(chacoan_4674, lwd=2, add=T)
# plot(chacoan_over500m, col='forestgreen', add=T, legend=F)
# plot(chacoan_over600m, col='orange', add=T, legend=F)
# plot(chacoan_over700m, col='purple', add=T, legend=F)
# plot(chacoan_over800m, col='blue', add=T, legend=F)

## Sep 2024 update: repeat for newer Morrone ecoregions
# Nov 2024 update: add Yungas province
# https://www.scielo.br/j/aabc/a/hPft4CK6RV8QBr8nP7bxhRQ/# 

provinces_keep_2022 <- c('Guianan Lowlands province','Roraima province','Guianan province',
                         'Imeri province','Xingu-Tapajos province','Madeira province', 'Yungas province',
                         'Rondonia province','Ucayali province','Para province','Napo province')

#morrone2022_4326 <- terra::project(morrone2022, "EPSG:4326")
#morrone2022_4326_amazon <- subset(morrone2022_4326, morrone2022_4326$Provincias %in% provinces_keep_2022)
# 
# plot(amazon_study_area_4326)
# plot(morrone2022_4326_amazon, add=T, col='forestgreen')
# 
morrone2022_4674_amazon <- terra::project(morrone2022_4326_amazon, "EPSG:4674")
# #writeVector(morrone2022_4674_amazon, filename='Regions/Morrone2022/Morrone2022_amazon_4674.shp')
# 
# Calculate elevational gradients within each region
# warning: can take hours
morrone2022_4674_amazon_elev_mean <- terra::extract(DEM_4674, morrone2022_amazonclip_4674, fun="mean", na.rm=T)
morrone2022_4674_amazon_elev_min <- terra::extract(DEM_4674, morrone2022_amazonclip_4674, fun="min", na.rm=T)
morrone2022_4674_amazon_elev_max <- terra::extract(DEM_4674, morrone2022_amazonclip_4674, fun="max", na.rm=T)
morrone2022_4674_amazon_elev_median <- terra::extract(DEM_4674, morrone2022_amazonclip_4674, fun="median", na.rm=T)

morrone_amazon_elev_df <- cbind.data.frame(morrone2022_4674_amazon_elev_mean, morrone2022_4674_amazon_elev_min,
                                           morrone2022_4674_amazon_elev_max, morrone2022_4674_amazon_elev_median)
morrone_amazon_elev_df <- morrone_amazon_elev_df[,c(2,4,6,8)]
names(morrone_amazon_elev_df) <- c('elev_mean_m','elev_min_m','elev_max_m','elev_median_m')
morrone_amazon_elev_df$province <- morrone2022_amazonclip_4674$Provincias
morrone_amazon_elev_df$elev_range_m <- morrone_amazon_elev_df$elev_max_m - morrone_amazon_elev_df$elev_min_m
#write.csv(morrone_amazon_elev_df, "Regions/Morrone2022/Morrone2022_regions_elev_stats.csv", row.names=F)

## Get 10th and 90th percentile of elevation per region
region_pct_list <- list()
for (i in 1:nrow(morrone2022_amazonclip_4674)){
  test_region <- morrone2022_amazonclip_4674[i]
  test_region_DEM <- terra::crop(DEM_4674, test_region, mask=T)
  test_region_DEM_df <- as.data.frame(t(global(test_region_DEM, quantile, probs=seq(0,1,0.1), na.rm=T)))
  names(test_region_DEM_df) <- test_region$Provincias
  region_pct_list[[i]] <- test_region_DEM_df
}
region_pct_df <- test <- do.call(cbind.data.frame, region_pct_list)
region_pct_df$pct <- seq(0,1,0.1) #percentile
#write.csv(region_pct_df, "Regions/Morrone2022/Morrone2022_regions_elevpct_stats.csv", row.names=F)
region_pct_df <- read.csv("Regions/Morrone2022/Morrone2022_regions_elevpct_stats.csv")

# Apply the 10th and 90th percentiles as dynamic cutoffs for lowland and highland habitat, respectively
highlow_region_list <- list()

# For a given region, identify lowland and highland habitat 
# based on 10th and 90th percentile cutoffs
for (i in 1:nrow(morrone2022_amazonclip_4674)){
  test_region <- morrone2022_amazonclip_4674[i]
  test_region_name <- test_region$Provincias
  test_region_data <- region_pct_df[,test_region_name]
  test_region_cut10 <- test_region_data[2] #10th percentile
  test_region_cut90 <- test_region_data[10] #90th percentile
  test_region_DEM <- terra::crop(DEM_4674, test_region, mask=T)
  test_region_lowland <- terra::ifel(test_region_DEM <= test_region_cut10, 1, NA)
  test_region_highland <- terra::ifel(test_region_DEM >= test_region_cut90, 1, NA)
  lowland_name <- paste0("Regions/Morrone2022/lowland/lowland_",gsub( " .*$", "", test_region_name), ".tif")
  highland_name <- paste0("Regions/Morrone2022/highland/highland_",gsub( " .*$", "", test_region_name), ".tif")
  terra::writeRaster(test_region_lowland, filename=lowland_name, overwrite=T)
  terra::writeRaster(test_region_highland, filename=highland_name, overwrite=T)
  
  # Calculate lowland and highland area
  # and percentage of region (should be about 10% each)
  test_region_lowland_areasqkm <- terra::expanse(test_region_lowland, "km")[2]
  test_region_highland_areasqkm <- terra::expanse(test_region_highland, "km")[2]
  test_region_areasqkm <- terra::expanse(test_region, "km")
  test_region_lowland_pct <- test_region_lowland_areasqkm/test_region_areasqkm
  test_region_highland_pct <- test_region_highland_areasqkm/test_region_areasqkm
  
  # Calculate area and percentage of lowland and highland within protected areas
  test_region_lowland_protected <- terra::mask(test_region_lowland, dissolved_PA_10km, inverse=F)
  test_region_highland_protected <- terra::mask(test_region_highland, dissolved_PA_10km, inverse=F)
  test_region_lowland_protected_areasqkm <- terra::expanse(test_region_lowland_protected, "km")[2]
  test_region_highland_protected_areasqkm <- terra::expanse(test_region_highland_protected, "km")[2]
  test_region_protected_areas <- terra::intersect(dissolved_PA_10km, test_region)
  test_region_protected_pct <- sum(terra::expanse(test_region_protected_areas, "km"))/test_region_areasqkm
  test_region_lowland_protected_pct <- test_region_lowland_protected_areasqkm/test_region_areasqkm
  test_region_highland_protected_pct <- test_region_highland_protected_areasqkm/test_region_areasqkm
  
  test_region_df <- data.frame(region = test_region_name,
                               region_areasqkm = test_region_areasqkm, 
                               lowland_areasqkm = test_region_lowland_areasqkm$area,
                               highland_areasqkm = test_region_highland_areasqkm$area,
                               lowland_pct = test_region_lowland_pct$area,
                               highland_pct = test_region_highland_pct$area,
                               region_protected_pct = test_region_protected_pct,
                               lowland_protected_areasqkm = test_region_lowland_protected_areasqkm$area,
                               highland_protected_areasqkm = test_region_highland_protected_areasqkm$area,
                               lowland_protected_pct = test_region_lowland_protected_pct$area,
                               highland_protected_pct = test_region_highland_protected_pct$area)
  
  highlow_region_list[[i]] <- test_region_df
}

highlow_region_df <- do.call(rbind.data.frame, highlow_region_list)
#write.csv(highlow_region_df, "Regions/Morrone2022/Morrone2022_regions_highlandlowland_stats.csv", row.names=F)

#############
# plot(test_region)
# plot(test_region_lowland, add=T, col='gold')
# plot(test_region_highland, add=T, col='dodgerblue')
# plot(test_region_lowland_protected, add=T, col='green')
# plot(test_region_highland_protected, add=T, col='red')

## 11-20-24 update
# Compare Amazon regional characteristics
frag_stats <- read.csv("LCC/province_fragmentation_stats.csv")
elev_stats <- read.csv("Regions/Morrone2022/Morrone2022_regions_elev_stats.csv")
#hilo_stats <- read.csv("Regions/Morrone2022/Morrone2022_regions_highlandlowland_stats.csv")
highland_stats <- read.csv("Regions/Morrone2022/Morrone2022_regions_highland25003500m_stats.csv")
protect_stats <- read.csv("RAISG/morrone2022_PA_stats.csv")
lulc_stats <- read.csv("LCC/LCC_freq_table_byProvince.csv")
extractive_stats <- terra::vect("Regions/Morrone2022/MinesOilGas/MinesOilGas_OverlapAnalysis.shp")

# modify extractive shp into df
extractive_stats <- as.data.frame(extractive_stats[,c(1,13,15)])
names(extractive_stats) <- c('Province','IllegalMine_pct','OilGas_pct')

# standardize columns so all dfs have a "Province" column
names(elev_stats)[names(elev_stats) == 'province'] <- 'Province'
names(protect_stats)[names(protect_stats) == 'region'] <- 'Province'
#names(hilo_stats)[names(hilo_stats) == 'region'] <- 'Province'

merger_list <- list(frag_stats, elev_stats, highland_stats, 
                    protect_stats, lulc_stats, extractive_stats)
region_df <- Reduce(function(x, y) merge(x, y, all=T), merger_list)

plot(region_df$PA_complexes_pct ~ region_df$Forest, pch=16, 
     xlab='Forest (%)', ylab='Protection (%)')

plot(region_df$PA_complexes_pct ~ region_df$elev_range_m, pch=16, 
     xlab='Elevation range (m)', ylab='Protection (%)')

plot(region_df$Forest ~ region_df$elev_range_m, pch=16, 
     xlab='Elevation range (m)', ylab='Forest (%)')

plot(region_df$Forest ~ region_df$meanCAI, pch=16, 
     xlab='Mean core area index (%)', ylab='Forest (%)')

plot(region_df$PA_complexes_pct ~ region_df$meanCAI, pch=16, 
     xlab='Mean core area index (%)', ylab='Protection (%)')

main_var <- region_df[,c('Province','meanPatch_sqkm','meanCAI','meanENN',
                         'elev_range_m','highland_all_pct','highland_protected_pct',
                         'PA_complexes_pct','Forest','IllegalMine_pct','OilGas_pct')]
cor(main_var[,c(2:11)], method='spearman')

## Assign variable ranks
main_var$meanPatch_sqkm_rank <- as.integer(rank(desc(main_var$meanPatch_sqkm)))
main_var$Forest_rank <- as.integer(rank(desc(main_var$Forest)))
main_var$meanCAI_rank <- as.integer(rank(desc(main_var$meanCAI)))
#main_var$meanENN_rank <- as.integer(rank(desc(main_var$meanENN)))
main_var$elev_range_m_rank <- as.integer(rank(desc(main_var$elev_range_m)))
main_var$highland_all_pct_rank <- as.integer(rank(desc(main_var$highland_all_pct)))
main_var$highland_protected_pct_rank <- as.integer(rank(desc(main_var$highland_protected_pct)))
main_var$PA_complexes_pct_rank <- as.integer(rank(desc(main_var$PA_complexes_pct)))
main_var$IllegalMine_pct_rank <- as.integer(rank(desc(main_var$IllegalMine_pct)))
main_var$OilGas_pct_rank <- as.integer(rank(desc(main_var$OilGas_pct)))

main_var$avg_rank <- rowMeans(main_var[,c(12:20)])
main_var$Province_again <- main_var$Province

## Try a PCA to develop a "climate resilience score"?
## rescale all variables prior to PCA
pca_data <- as.data.frame(scale(main_var[,c(2,3,5:9)]))
rownames(pca_data) <- main_var$Province

## run PCA
province_pca <- princomp(~ meanPatch_sqkm + meanCAI + elev_range_m + highland_all_pct +
                      highland_protected_pct + PA_complexes_pct + Forest,
                    data=pca_data, cor=F, scores=T)
par(mfrow=c(1,1))
screeplot(province_pca, type='l')
summary(province_pca)
loadings(province_pca)

fviz_pca_var(province_pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

## calculate climate resilience index
pca_scores <- as.data.frame(province_pca$scores)
pca_scores$resilience_index <- sqrt((pca_scores$Comp.1 ^2) + (pca_scores$Comp.2 ^2) +
                                    (pca_scores$Comp.3 ^2))
summary(pca_scores$resilience_index)
hist(pca_scores$resilience_index)
pca_scores$Province <- main_var$Province

# The results don't make sense for some reason; why is Para so high when its metrics are all so poor?
pca_scores$resilience_index_rank <- as.integer(rank(desc(pca_scores$resilience_index)))

#write.csv(pca_scores, file='LeastCostPaths/LCP_prioritization/LCP_priority_index.csv', row.names=F)

