######################## Amazon forest fragmentation ##############################
# Date: 11-19-24
# updated: 11-20-24
# Author: Ian McCullough, immccull@gmail.com
###################################################################################

#### R libraries ####
library(terra)
library(landscapemetrics)
library(ggplot2)

#### Input data ####
setwd("C:/Users/immccull/Documents/AmazonClimateCorridors")

# regions
morrone_2022_amazonclip <- terra::vect("Regions/Morrone2022/Amazon_macroregions_fixedgeom_Amazonclip.shp")

# LULC
#LULC <- terra::rast("LCC/LCC_amazon_mosaic_100m.tif")
#forest <- terra::ifel(LULC==10, 1, NA)
forest_patches_500m <- terra::rast("LCC/forest_patches_500m.tif")
forest_500m <- terra::rast("LCC/LCC_500m_forest.tif")

#### Main program ####
# Testing fragmentation analysis
# Warning: likely very slow
# test_region <- morrone_2022_amazonclip[11]
# test_region_buff <- terra::buffer(test_region, width=10000)
# test_region_forest <- terra::crop(forest, test_region_buff, mask=T)
# test_region_forest_patches <- terra::patches(test_region_forest, directions=8, allowGaps=F,
#                                              filename='tump/yungas_forest_patches.tif')

lsm_list <- list()
lsm_all_list <- list()
edge_depth <- 2 #pixels deep

for (i in 1:nrow(morrone_2022_amazonclip)){
  test_region <- morrone_2022_amazonclip[i]
  #test_region_buff <- terra::buffer(test_region, width=10000)
  #test_region_forest_patches <- terra::crop(forest_patches_500m, test_region, mask=T)
  #test_region_forest_polygons <- terra::as.polygons(test_region_forest_patches)
  test_region_forest <- terra::crop(forest_500m, test_region, mask=T)
  #nPatches <- dim(test_region_forest_polygons)[1]
  nPatches <- lsm_l_np(test_region_forest)
  patcharea <- lsm_p_area(test_region_forest, directions=8) #uses hectares; 1 ha = 0.01 sqkm
  cai <- lsm_p_cai(test_region_forest, directions=8, edge_depth=edge_depth)
  enn <- lsm_p_enn(test_region_forest, directions=8)
  pd <- lsm_l_pd(test_region_forest, directions=8)
  # summary table
  output_df <- data.frame(Province=test_region$Provincias,
                          nPatches=nPatches$value,
                          minPatch_sqkm=(min(patcharea$value, na.rm=T))*0.01,
                          medianPatch_sqkm=(median(patcharea$value, na.rm=T))*0.01,
                          meanPatch_sqkm=(mean(patcharea$value, na.rm=T))*0.01,
                          maxPatch_sqkm=(max(patcharea$value, na.rm=T))*0.01,
                          minCAI=min(cai$value, na.rm=T),
                          medianCAI=median(cai$value, na.rm=T),
                          meanCAI=mean(cai$value, na.rm=T),
                          maxCAI=max(cai$value, na.rm=T),
                          minENN=min(enn$value, na.rm=T),
                          medianENN=median(enn$value, na.rm=T),
                          meanENN=mean(enn$value, na.rm=T),
                          maxENN=max(enn$value, na.rm=T),
                          pd=pd$value, #per 100 ha=1 sq km
                          edge_depth=edge_depth)
  # table with data for individual patches
  allpatch_df <- data.frame(Province=test_region$Provincias,
                            cai=cai$value,
                            enn=enn$value,
                            areasqkm=(patcharea$value)*0.01)
  
  # store output for export
  lsm_list[[i]] <- output_df
  lsm_all_list[[i]] <- allpatch_df
  patcharea = NA
  cai = NA
  pd = NA
  enn = NA
  test_region = NA
  nPatches = NA
  test_region_forest = NA
  output_df = NA
  allpatch_df = NA
}

lsm_df <- do.call(rbind.data.frame, lsm_list)
#write.csv(lsm_df, file='LCC/province_fragmentation_stats.csv', row.names=F)
lsm_all_df <- do.call(rbind.data.frame, lsm_all_list)
#write.csv(lsm_df, file='LCC/province_fragmentation_allpatch_stats.csv', row.names=F)


## Some visuals
double <- lsm_all_df
double$Province <- 'All'

lsm_all_double <- rbind.data.frame(lsm_all_df, double)
lsm_all_double$Province <- factor(lsm_all_double$Province, levels=c('Guianan Lowlands province','Guianan province','Imeri province', 'Madeira province',
                                                                    'Napo province','Para province','Rondonia province','Roraima province',
                                                                    'Ucayali province','Xingu-Tapajos province','Yungas province','All'))
site_names <- c('GL','GUI','IME','MAD','NAP','PAR','RON','ROR',
                'UCA','XT','YUN','All')
plot_colors <- c('forestgreen','dodgerblue','orange','gold','salmon','purple','lightgreen','tan','firebrick','navy','turquoise','gray')

CAI_plot <-ggplot(lsm_all_double, aes(x=Province, y=cai, fill=Province)) +
  geom_boxplot()+
  theme_classic()+
  theme(axis.text.x=element_text(color='black', size=8, angle=70, hjust=1),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_fill_manual(values=plot_colors)+
  scale_y_continuous(name='Core area index (%)')+
  scale_x_discrete(name='', labels=site_names)+
  ggtitle('A) Core area index')
CAI_plot

lsm_all_double$cai_fudge <- lsm_all_double$cai + 0.01
CAI_log_plot <-ggplot(lsm_all_double, aes(x=Province, y=log(cai_fudge), fill=Province)) +
  geom_boxplot()+
  theme_classic()+
  theme(axis.text.x=element_text(color='black', size=8, angle=70, hjust=1),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_fill_manual(values=plot_colors)+
  scale_y_continuous(name='log(Core area index (%))')+
  scale_x_discrete(name='', labels=site_names)+
  ggtitle('A) Core area index')
CAI_log_plot

ENN_plot <-ggplot(lsm_all_double, aes(x=Province, y=log(enn), fill=Province)) +
  geom_boxplot()+
  theme_classic()+
  theme(axis.text.x=element_text(color='black', size=8, angle=70, hjust=1),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_fill_manual(values=plot_colors)+
  scale_y_continuous(name='log(Nearest neighbor distance (m))')+
  scale_x_discrete(name='', labels=site_names)+
  ggtitle('B) Nearest neighbor distance')
ENN_plot

patcharea_plot <-ggplot(lsm_all_double, aes(x=Province, y=log(areasqkm), fill=Province)) +
  geom_boxplot()+
  theme_classic()+
  theme(axis.text.x=element_text(color='black', size=8, angle=70, hjust=1),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_fill_manual(values=plot_colors)+
  scale_y_continuous(name='log(Area (sq km))')+
  scale_x_discrete(name='', labels=site_names)+
  ggtitle('C) Patch area')
patcharea_plot

# some potentially interesting patch stats
cai_90 <- subset(lsm_all_df, cai >= 90)
cai_50 <- subset(lsm_all_df, cai >= 50)

table(cai_90$Province)
table(cai_50$Province)
nrow(cai_90)/nrow(lsm_all_df)
nrow(cai_50)/nrow(lsm_all_df)

patch100km <- subset(lsm_all_df, areasqkm >= 100)
table(patch100km$Province)
patch10km <- subset(lsm_all_df, areasqkm >= 10)
table(patch10km$Province)

nrow(patch100km)/nrow(lsm_all_df)
nrow(patch10km)/nrow(lsm_all_df)
