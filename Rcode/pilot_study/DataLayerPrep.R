### Assembling data layers for Amazon study area (pilot)
# Date: 4-4-24
# updated: 4-18-24

#### R libraries ####
library(terra)
library(dplyr)

#### Input data ####
setwd("C:/Users/immccull/Documents/Amazon")

southamerica <- terra::vect("RAISG/SA_Countries/SouthAmerica.shp")

amazon_study_area <- terra::vect("RAISG/StudyArea/western_Amazon_study_area1.shp")

area <- terra::expanse(amazon_study_area, unit="km")

516343/area
1032074/area

## DEM
# dem0 <- terra::rast("DEM/sa_dem_0.tif")
# dem1 <- terra::rast("DEM/sa_dem_1.tif")
# dem2 <- terra::rast("DEM/sa_dem_2.tif")
# dem3 <- terra::rast("DEM/sa_dem_3.tif")
# dem4 <- terra::rast("DEM/sa_dem_4.tif")
# dem5 <- terra::rast("DEM/sa_dem_5.tif")
# dem6 <- terra::rast("DEM/sa_dem_6.tif")
# dem7 <- terra::rast("DEM/sa_dem_7.tif")
# dem8 <- terra::rast("DEM/sa_dem_8.tif")
# dem9 <- terra::rast("DEM/sa_dem_9.tif")

DEM <- terra::rast("DEM/SA_DEM_mosaic.tif")

## canopy height (Potapov et al. 2019)
canopy <- terra::rast("C:/Users/immccull/Documents/GLAD_GCH_2019/Forest_height_2019_SAM.tif")

## WDPA polygons (downloaded January 2024)
wdpa_0 <- terra::vect("C:/Users/immccull/Documents/WDPA_Jan2024/WDPA_WDOECM_Jan2024_Public_all_shp_0/WDPA_WDOECM_Jan2024_Public_all_shp-polygons.shp")
wdpa_1 <- terra::vect("C:/Users/immccull/Documents/WDPA_Jan2024/WDPA_WDOECM_Jan2024_Public_all_shp_1/WDPA_WDOECM_Jan2024_Public_all_shp-polygons.shp")
wdpa_2 <- terra::vect("C:/Users/immccull/Documents/WDPA_Jan2024/WDPA_WDOECM_Jan2024_Public_all_shp_2/WDPA_WDOECM_Jan2024_Public_all_shp-polygons.shp")

# If have run code below already:
wdpa_SA <- terra::vect('protected_areas/WDPA_SouthAmerica.shp')

## extra protected area not in WDPA
losamigos <- terra::vect("protected_areas/LosAmigos/Los amigos conservation concesion.shp")


#### Main program ####
## mosaic individual South American DEM chunks into one
#DEM_mosaic <- terra::mosaic(dem0, dem1, dem2, dem3, dem4, dem5, dem6, dem7, dem8, dem9,
#                            fun="mean", filename="DEM/SA_DEM_mosaic.tif", overwrite=T)

## clip global canopy height dataset to S American boundary
canopy_SA <- terra::mask(canopy, southamerica, inverse=F,
                         filename="GCH/SA_GCH.tif")

## clip protected areas to S America, then merge into 1 shp
wdpa_0_SA <- terra::intersect(wdpa_0, southamerica)
wdpa_1_SA <- terra::intersect(wdpa_1, southamerica)
wdpa_2_SA <- terra::intersect(wdpa_2, southamerica)

wdpa_SA <- rbind(wdpa_0_SA, wdpa_1_SA, wdpa_2_SA)

# some basic filtering
wdpa_SA <- subset(wdpa_SA, wdpa_SA$MARINE %in% c(0,1)) #keep terrestrial and coastal, remove marine
wdpa_SA <- subset(wdpa_SA, wdpa_SA$STATUS %in% c('Designated','Established','Inscribed')) #exclude "Proposed"; no "Adopted" in this dataset
wdpa_SA <- subset(wdpa_SA, wdpa_SA$IUCN_CAT %in% c('Ia','Ib','II','III','IV','V','VI')) #remove 'Not Reported', 'Not Applicable' and 'Not Assigned
length(unique(wdpa_SA$WDPAID))
#writeVector(wdpa_SA, filename='protected_areas/WDPA_SouthAmerica.shp', overwrite=T)

## Get elevation for protected areas
amazon_study_area_4326 <- terra::project(amazon_study_area, "EPSG:4326")
wdpa_SA_pilot <- terra::intersect(wdpa_SA, amazon_study_area_4326)

# add in missing Los Amigos in Peru
losamigos_4326 <- terra::project(losamigos, "EPSG:4326")
# losamigos_4326$WDPAID <- '9999999999'
# losamigos_4326$WDPA_ID <- '9999999999'
# losamigos_4326$PA_DEF <- NA
# losamigos_4326$NAME <- 'LosAmigos'
wdpa_SA_pilot <- rbind(wdpa_SA_pilot, losamigos_4326)
wdpa_SA_pilot[287,1] <- 9999999 #WDPAID
wdpa_SA_pilot[287,2] <- 9999999 #WD_PAID
wdpa_SA_pilot[287,3] <- NA #PA_DEF
wdpa_SA_pilot[287,4] <- 'LosAmigos' #NAME
wdpa_SA_pilot[287,27] <- 'PER' #PARENT_ISO
wdpa_SA_pilot[287,28] <- 'PER' #ISO3
wdpa_SA_pilot[287,34] <- 'PER' #iso_alpha3
wdpa_SA_pilot[287,37] <- 'PERU' #Name_1

wdpa_SA_elev_mean <- terra::extract(DEM, wdpa_SA_pilot, fun="mean", na.rm=T)
wdpa_SA_elev_min <- terra::extract(DEM, wdpa_SA_pilot, fun="min", na.rm=T)
wdpa_SA_elev_median <- terra::extract(DEM, wdpa_SA_pilot, fun="median", na.rm=T)
wdpa_SA_elev_max <- terra::extract(DEM, wdpa_SA_pilot, fun="max", na.rm=T)

wdpa_SA_elev <- cbind.data.frame(wdpa_SA_elev_min, wdpa_SA_elev_median, wdpa_SA_elev_max, wdpa_SA_elev_mean)
wdpa_SA_elev <- wdpa_SA_elev[,c(1,2,4,6,8)]
names(wdpa_SA_elev) <- c('rowID','min_m', 'median_m','max_m','mean_m')
wdpa_SA_pilot$rowID <- seq(1, nrow(wdpa_SA), 1)
wdpa_SA_pilot <- merge(wdpa_SA_pilot, wdpa_SA_elev, by='rowID')
wdpa_SA_pilot$areasqkm <- terra::expanse(wdpa_SA_pilot, unit="km")
wdpa_SA_pilot_df <- as.data.frame(wdpa_SA_pilot)
#write.csv(wdpa_SA_pilot_df, file="protected_areas/wdpa_SA_pilot_wElev.csv", row.names=F)
#writeVector(wdpa_SA_pilot, "protected_areas/wdpa_SA_pilot_wElev.shp", overwrite=T)

# wdpa_SA_elev_mean <- terra::extract(DEM, wdpa_SA, fun="mean", na.rm=T)
# wdpa_SA_elev_min <- terra::extract(DEM, wdpa_SA, fun="min", na.rm=T)
# wdpa_SA_elev_median <- terra::extract(DEM, wdpa_SA, fun="median", na.rm=T)
# wdpa_SA_elev_max <- terra::extract(DEM, wdpa_SA, fun="max", na.rm=T)
# 
# wdpa_SA_elev <- cbind.data.frame(wdpa_SA_elev_min, wdpa_SA_elev_median, wdpa_SA_elev_max, wdpa_SA_elev_mean)
# wdpa_SA_elev <- wdpa_SA_elev[,c(1,2,4,6,8)]
# names(wdpa_SA_elev) <- c('rowID','min_m', 'median_m','max_m','mean_m')
# wdpa_SA$rowID <- seq(1, nrow(wdpa_SA), 1)
# wdpa_SA <- merge(wdpa_SA, wdpa_SA_elev, by='rowID')

#lowland_PA <- subset(wdpa_SA, wdpa_SA$mean_m <= 500)
#highland_PA <- subset(wdpa_SA, wdpa_SA$mean_m >= 1500 & wdpa_SA$mean_m <= 3500)

#writeVector(lowland_PA, filename="protected_areas/WDPA_SouthAmerica_lowland.shp", overwrite=T)
#writeVector(highland_PA, filename="protected_areas/WDPA_SouthAmerica_highland.shp", overwrite=T)

# plot(southamerica)
# plot(lowland_PA, add=T, col='green')
# plot(highland_PA, add=T, col='dodgerblue')

#amazon_study_area_4326 <- terra::project(amazon_study_area, "EPSG:4326")
#lowland_PA_pilot <- terra::intersect(lowland_PA, amazon_study_area_4326)
#highland_PA_pilot <- terra::intersect(highland_PA, amazon_study_area_4326)

# lowland_PA_pilot <- subset(wdpa_SA_pilot, wdpa_SA_pilot$min_m <= 500)
# highland_PA_pilot <- subset(wdpa_SA_pilot, wdpa_SA_pilot$mean_m >= 2500 & wdpa_SA_pilot$mean_m <= 3500)

#writeVector(lowland_PA_pilot, filename="protected_areas/WDPA_SouthAmerica_lowland_pilot.shp", overwrite=T)
#writeVector(highland_PA_pilot, filename="protected_areas/WDPA_SouthAmerica_highland_pilot.shp", overwrite=T)

#lowland_PA_pilot <- terra::vect("protected_areas/WDPA_SouthAmerica_lowland_pilot.shp")
#highland_PA_pilot <- terra::vect("protected_areas/WDPA_SouthAmerica_highland_pilot.shp")

## Update: identify low and high zones of protected areas
# first create masks of lowland and highland areas
DEM_pilot <- terra::crop(DEM, amazon_study_area_4326, mask=T)
low_DEM_mask <- terra::ifel(DEM_pilot <= 500, 1, NA)
plot(amazon_study_area_4326)
plot(low_DEM_mask, add=T)
low_DEM_mask_polygons <- terra::as.polygons(low_DEM_mask, na.rm=T)

#high_DEM_mask <- terra::clamp(DEM_pilot, lower=2500, upper=3500, values=F)
high_DEM_mask <- terra::ifel(DEM_pilot >= 2500 & DEM_pilot <= 3500, 1, NA)
plot(amazon_study_area_4326)
plot(high_DEM_mask, add=T)
high_DEM_mask_polygons <- terra::as.polygons(high_DEM_mask, na.rm=T)

low_PA_zones <- terra::intersect(wdpa_SA_pilot, low_DEM_mask_polygons)
plot(amazon_study_area_4326)
plot(wdpa_SA_pilot, add=T, col='gray')
plot(low_PA_zones, add=T, col='red')
low_PA_zones$lowareasqkm <- terra::expanse(low_PA_zones, unit="km")
low_tmp <- data.frame(rowID = low_PA_zones$rowID, lowareasqkm=low_PA_zones$lowareasqkm)
length(unique(low_PA_zones$WDPAID))

high_PA_zones <- terra::intersect(wdpa_SA_pilot, high_DEM_mask_polygons)
plot(amazon_study_area_4326)
plot(wdpa_SA_pilot, add=T, col='gray')
plot(high_PA_zones, add=T, col='dodgerblue')
high_PA_zones$highareasqkm <- terra::expanse(high_PA_zones, unit="km")
high_tmp <- data.frame(rowID = high_PA_zones$rowID, highareasqkm=high_PA_zones$highareasqkm)
length(unique(high_PA_zones$WDPAID))

# Merge in low and high area to WDPA data frame
wdpa_SA_pilot_df <- left_join(wdpa_SA_pilot_df, low_tmp, by='rowID')
wdpa_SA_pilot_df <- left_join(wdpa_SA_pilot_df, high_tmp, by='rowID')

# NAs represent actual zeros (i.e., no high area in particular protected area)
wdpa_SA_pilot_df[c("lowareasqkm", "highareasqkm")][is.na(wdpa_SA_pilot_df[c("lowareasqkm", "highareasqkm")])] <- 0

# Calculate % of each PA that is highland or lowland
wdpa_SA_pilot_df$low_prop <- wdpa_SA_pilot_df$lowareasqkm/wdpa_SA_pilot_df$areasqkm
wdpa_SA_pilot_df$high_prop <- wdpa_SA_pilot_df$highareasqkm/wdpa_SA_pilot_df$areasqkm
summary(wdpa_SA_pilot_df$low_prop)
summary(wdpa_SA_pilot_df$high_prop)

# identify duplicates by WDPAID
low_dupes <- as.data.frame(low_PA_zones) %>% 
  group_by(WDPAID) %>% 
  filter(n() > 1) %>%
  as.data.frame()
low_dupes

high_dupes <- as.data.frame(high_PA_zones) %>% 
  group_by(WDPAID) %>% 
  filter(n() > 1) %>%
  as.data.frame()
high_dupes

# to deal with duplicates (seem to be when a protected area bleeds across an intl border)
# keep polygon with larger area (comprising overall large percentage of protected area)
# this will keep attributes from larger polygon
lowland_deduped <- as.data.frame(low_PA_zones) %>%
  group_by(WDPAID) %>%
  filter(areasqkm == max(areasqkm, na.rm=T)) %>%
  as.data.frame()
lowland_deduped_5km <- subset(lowland_deduped, areasqkm >= 5)
nrow(lowland_deduped_5km)
length(unique(lowland_deduped_5km$WDPAID)) #should match previous row
lowland_deduped_5km_shp <- subset(low_PA_zones, low_PA_zones$rowID %in% lowland_deduped_5km$rowID)

highland_deduped <- as.data.frame(high_PA_zones) %>%
  group_by(WDPAID) %>%
  filter(areasqkm == max(areasqkm, na.rm=T)) %>%
  as.data.frame()
highland_deduped_5km <- subset(highland_deduped, areasqkm >= 5)
nrow(highland_deduped_5km)
length(unique(highland_deduped_5km$WDPAID)) #should match previous row
highland_deduped_5km_shp <- subset(high_PA_zones, high_PA_zones$rowID %in% highland_deduped_5km$rowID)

## Export for least cost path modeling
#writeVector(lowland_deduped_5km_shp, filename="protected_areas/WDPA_SouthAmerica_lowland_pilot_5km.shp", overwrite=T)
#writeVector(highland_deduped_5km_shp, filename="protected_areas/WDPA_SouthAmerica_highland_pilot_5km.shp", overwrite=T)


### Populated places layer ###
#https://www.naturalearthdata.com/downloads/10m-cultural-vectors/10m-populated-places/

pop <- terra::vect("PopulatedPlaces/ne_10m_populated_places.shp")
pop_wAmazon <- subset(pop, pop$SOV0NAME %in% c('Ecuador','Peru','Colombia','Bolivia','Brazil','Venezuela'))
caps_wAmazon <- subset(pop_wAmazon, pop_wAmazon$NAMEASCII %in% c('Lima','Quito','La Paz','Bogota','Brasilia','Caracas'))
caps_wAmazon_29172 <- terra::project(caps_wAmazon, "EPSG:29172")

plot(amazon_study_area)
plot(caps_wAmazon_29172, add=T)
#writeVector(caps_wAmazon_29172, filename='PopulatedPlaces/wAmazon_capitals.shp', overwrite=T)

######### OLD ###############
#writeVector(lowland_PA_pilot_5km_deduped, filename="protected_areas/WDPA_SouthAmerica_lowland_pilot_5km.shp", overwrite=T)
#writeVector(highland_PA_pilot_5km, filename="protected_areas/WDPA_SouthAmerica_highland_pilot_5km.shp", overwrite=T)

# # could possibly use aggregate by WDPAID
# low_dupes_shp <- subset(lowland_PA_pilot_5km, lowland_PA_pilot_5km$WDPAID %in% low_dupes$WDPAID)
# testagg <- terra::aggregate(low_dupes_shp, by='WDPAID', fun="max")
# View(as.data.frame(testagg))



# After inspecting, there are duplicate WDPAID, which often seem to be from slivers
# that go across national borders
# I think it will be better to dissolve these now rather than create multiple
# corridors from basically the same protected area, but some skewed by slivers
# will just have to deal with missing or incorrect attributes
# lowland_PA_pilot_dissolved <- terra::aggregate(lowland_PA_pilot, by='WDPAID')
# highland_PA_pilot_dissolved <- terra::aggregate(highland_PA_pilot, by='WDPAID')
# 
# lowland_PA_pilot_dissolved_elev_mean <- terra::extract(DEM, lowland_PA_pilot_dissolved, fun='mean', na.rm=T)
# lowland_PA_pilot_dissolved_elev_min <- terra::extract(DEM, lowland_PA_pilot_dissolved, fun='min', na.rm=T)
# lowland_PA_pilot_dissolved_elev_median <- terra::extract(DEM, lowland_PA_pilot_dissolved, fun='median', na.rm=T)
# lowland_PA_pilot_dissolved_elev_max <- terra::extract(DEM, lowland_PA_pilot_dissolved, fun='max', na.rm=T)
# 
# lowland_PA_pilot_dissolved_elev <- cbind.data.frame(lowland_PA_pilot_dissolved_elev_min, lowland_PA_pilot_dissolved_elev_median, lowland_PA_pilot_dissolved_elev_max, lowland_PA_pilot_dissolved_elev_mean)
# lowland_PA_pilot_dissolved_elev <- lowland_PA_pilot_dissolved_elev[,c(1,2,4,6,8)]
# names(lowland_PA_pilot_dissolved_elev) <- c('rowID','min_m', 'median_m','max_m','mean_m')
# 
# # get rid of unwanted columns and append elevation data
# lowland_PA_pilot_dissolved <- lowland_PA_pilot_dissolved[,c(1,19:48)]
# lowland_PA_pilot_dissolved$min_m <- lowland_PA_pilot_dissolved_elev$min_m
# lowland_PA_pilot_dissolved$median_m <- lowland_PA_pilot_dissolved_elev$median_m
# lowland_PA_pilot_dissolved$max_m <- lowland_PA_pilot_dissolved_elev$max_m
# lowland_PA_pilot_dissolved$mean_m <- lowland_PA_pilot_dissolved_elev$mean_m
# 
# highland_PA_pilot_dissolved_elev_mean <- terra::extract(DEM, highland_PA_pilot_dissolved, fun='mean', na.rm=T)
# highland_PA_pilot_dissolved_elev_min <- terra::extract(DEM, highland_PA_pilot_dissolved, fun='min', na.rm=T)
# highland_PA_pilot_dissolved_elev_median <- terra::extract(DEM, highland_PA_pilot_dissolved, fun='median', na.rm=T)
# highland_PA_pilot_dissolved_elev_max <- terra::extract(DEM, highland_PA_pilot_dissolved, fun='max', na.rm=T)
# 
# highland_PA_pilot_dissolved_elev <- cbind.data.frame(highland_PA_pilot_dissolved_elev_min, highland_PA_pilot_dissolved_elev_median, highland_PA_pilot_dissolved_elev_max, highland_PA_pilot_dissolved_elev_mean)
# highland_PA_pilot_dissolved_elev <- highland_PA_pilot_dissolved_elev[,c(1,2,4,6,8)]
# names(highland_PA_pilot_dissolved_elev) <- c('rowID','min_m', 'median_m','max_m','mean_m')
# 
# # get rid of unwanted columns and append elevation data
# highland_PA_pilot_dissolved <- highland_PA_pilot_dissolved[,c(1,19:48)]
# highland_PA_pilot_dissolved$min_m <- highland_PA_pilot_dissolved_elev$min_m
# highland_PA_pilot_dissolved$median_m <- highland_PA_pilot_dissolved_elev$median_m
# highland_PA_pilot_dissolved$max_m <- highland_PA_pilot_dissolved_elev$max_m
# highland_PA_pilot_dissolved$mean_m <- highland_PA_pilot_dissolved_elev$mean_m
# 
# #writeVector(lowland_PA_pilot_dissolved, filename='protected_areas/WDPA_SouthAmerica_lowland_pilot_dissolvedWDPAID.shp')
# #writeVector(highland_PA_pilot_dissolved, filename='protected_areas/WDPA_SouthAmerica_highland_pilot_dissolvedWDPAID.shp')
# 
# plot(southamerica)
# plot(amazon_study_area_4326, add=T, col='gray')
# plot(lowland_PA_pilot, add=T, col='green')
# plot(highland_PA_pilot, add=T, col='dodgerblue')
# 
# plot(amazon_study_area_4326, col='gray')
# plot(lowland_PA_pilot, add=T, col='green')
# plot(highland_PA_pilot, add=T, col='dodgerblue')
