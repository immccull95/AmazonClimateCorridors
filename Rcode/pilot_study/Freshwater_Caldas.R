##################### Amazon freshwater connectivity ######################
# Date: 4-25-24
# updated:
# Author: Ian McCullough, immccull@gmail.com
###################################################################################

#### R libraries ####
library(terra)

#### Input data ####
# https://figshare.com/articles/dataset/Identifying_the_current_and_future_status_of_freshwater_connectivity_corridors_in_the_Amazon_Basin/21267363
# Caldas et al. 2023: https://doi.org/10.1111/csp2.12853

setwd("C:/Users/immccull/Documents/Amazon")

# Study area
amazon_study_area <- terra::vect("RAISG/StudyArea/western_Amazon_study_area1_29172.shp")

ffr_current <- terra::vect("Freshwater/FFR_current/FFR_current.shp")

#### Main program ####
ffr_current_29172 <- terra::project(ffr_current, "EPSG:29172")

ffr_current_29172_clip <- terra::intersect(ffr_current_29172, amazon_study_area)

plot(amazon_study_area)
plot(ffr_current_29172_clip, add=T, col='dodgerblue')

ffr_current_29172_clip_free <- subset(ffr_current_29172_clip, ffr_current_29172_clip$CSI_FF123 ==1)
ffr_current_29172_clip_good <- subset(ffr_current_29172_clip, ffr_current_29172_clip$CSI_FF123 ==2)



