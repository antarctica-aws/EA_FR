## step14 is using the retsortaion opportunity map from Griscom et al to extrapolate the cabron caccumulation rate 

setwd('/gpfs/data1/duncansongp/amberliang/EA_data')
`%notin%` <- Negate(`%in%`)


#####---------------version 1: Martin Jung et al (2021) restor opportunity map only--------------######


rateMap2 <- raster('NCS_Refor11_map_with_description_zip/cookpatton_EA_reprj2.tif')

restorMap2 <- raster('NatureMap_prioritymaps/ea_reprj.tif')
restorMap2[restorMap2<50]<-NA
## reproject to equal area  [much faster in qgis]
prj <- '+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs'

poly <- readOGR('AOI/EA_one.shp') %>% 
  spTransform(., CRS(prj))
# rateMap2_prj <- projectRaster(rateMap2, crs = prj)

#resampel rate map to match 1km raster 
ras1km <- raster('NCS_Refor11_map_with_description_zip/wwf_ea_1km.tif')

rateMap2_res <- resample( rateMap2,ras1km, method='bilinear')
restorMap2_res <- resample( restorMap2,ras1km, method='bilinear')

# #get the NR rate over restorRA2 
rateMap2_res[is.na(values(restorMap2_res))] <- NA   # 14528 1km pixels for >=50    3850 for >= 70

names(rateMap2_res) <- 'cookpatton_NR_rate'

print(sum(is.na(rateMap2_res[])))   #4451441
print(length(rateMap2_res[]))
print(length(rateMap2_res[])- sum(is.na(rateMap2_res[])))
# writeRaster(rateMap2_res,'NCS_Refor11_map_with_description_zip/cookpatton_griscom+natureMap1km_reprj.tif', overwrite=TRUE)



#####-----------------version 2: Griscom et al 2017 map only [choosen because match with Cook-Patton]-----------------######

rateMap2 <- raster('NCS_Refor11_map_with_description_zip/cookpatton_griscom_reprj.tif')

restorMap2 <- raster('NatureMap_prioritymaps/ea_reprj.tif')
# restorMap2[restorMap2<50]<-NA
## reproject to equal area  [much faster in qgis]
prj <- '+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs'

poly <- readOGR('AOI/EA_one.shp') %>% 
  spTransform(., CRS(prj))
# rateMap2_prj <- projectRaster(rateMap2, crs = prj)

#resampel rate map to match 1km raster 
ras1km <- raster('NCS_Refor11_map_with_description_zip/wwf_ea_1km.tif')

rateMap2_res <- resample( rateMap2,ras1km, method='bilinear')
restorMap2_res <- resample( restorMap2,ras1km, method='bilinear')

# #get the NR rate over restorRA2 
# rateMap2_res[is.na(values(restorMap2_res))] <- NA   # 14528 1km pixels for >=50    3850 for >= 70

names(rateMap2_res) <- 'cookpatton_NR_rate'

print(sum(is.na(rateMap2_res[])))   #5683640
print(length(rateMap2_res[]))
print(length(rateMap2_res[])- sum(is.na(rateMap2_res[])))

# writeRaster(rateMap2_res,'NCS_Refor11_map_with_description_zip/cookpatton_griscom+natureMap1km_reprj.tif', overwrite=TRUE)


#####-----------------version 3: Griscom et al 2017 map + Jung et al map -----------------######


rateMap2 <- raster('NCS_Refor11_map_with_description_zip/cookpatton_griscom_reprj.tif')

restorMap2 <- raster('NatureMap_prioritymaps/ea_reprj.tif')
restorMap2[restorMap2<50]<-NA
## reproject to equal area  [much faster in qgis]
prj <- '+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs'

poly <- readOGR('AOI/EA_one.shp') %>% 
  spTransform(., CRS(prj))
# rateMap2_prj <- projectRaster(rateMap2, crs = prj)

#resampel rate map to match 1km raster 
ras1km <- raster('NCS_Refor11_map_with_description_zip/wwf_ea_1km.tif')

rateMap2_res <- resample( rateMap2,ras1km, method='bilinear')
restorMap2_res <- resample( restorMap2,ras1km, method='bilinear')

# #get the NR rate over restorRA2 
rateMap2_res[is.na(values(restorMap2_res))] <- NA   # 14528 1km pixels for >=50    3850 for >= 70

names(rateMap2_res) <- 'cookpatton_NR_rate'


print(sum(is.na(rateMap2_res[])))   #5735308 (51668 less)
print(length(rateMap2_res[]))
print(length(rateMap2_res[])- sum(is.na(rateMap2_res[])))

# writeRaster(rateMap2_res,'NCS_Refor11_map_with_description_zip/cookpatton_griscom+natureMap1km_reprj.tif', overwrite=TRUE)




#--------------------------------divided by biome level calculation-----------------------------

###divide by biomes

biome1 <- readOGR('AOI/ea_tsmbf.geojson') %>% 
  spTransform(., CRS(prj))
biome7 <- readOGR('AOI/ea_tsgss.geojson') %>% 
  spTransform(., CRS(prj))
biome10 <- readOGR('AOI/ea_mf.geojson') %>% 
  spTransform(., CRS(prj))

restorBiome1_v2<- raster::crop(rateMap2_res, biome1) %>% raster::mask(.,biome1)
restorBiome7_v2 <- raster::crop(rateMap2_res, biome7) %>% raster::mask(.,biome7)
restorBiome10_v2 <- raster::crop(rateMap2_res, biome10) %>% raster::mask(.,biome10)


#calculate carbon per unit area for NR: unit area x NRrate
restorBiome1_df2 <- as.data.frame(restorBiome1_v2) %>% 
  dplyr::filter(!is.na(cookpatton_NR_rate)) %>%  #n =67561
  dplyr::rename(NRrate = cookpatton_NR_rate)
dim(restorBiome1_df2)
restorBiome7_df2 <- as.data.frame(restorBiome7_v2) %>% 
  dplyr::filter(!is.na(cookpatton_NR_rate)) %>%  #n = 58440
  dplyr::rename(NRrate =cookpatton_NR_rate)
dim(restorBiome7_df2)
restorBiome10_df2 <- as.data.frame(restorBiome10_v2) %>% 
  dplyr::filter(!is.na(cookpatton_NR_rate)) %>%  #n = 4854
  dplyr::rename(NRrate = cookpatton_NR_rate)
dim(restorBiome10_df2)


pixelSize2  <-  1.043275*1.043275*100 #1*1*100  #unit is hectare
#biome 1 
cBiome1_df2 <- restorBiome1_df2 %>%  
  dplyr::mutate(NR_carbon_9yrs= NRrate * pixelSize2*9,  #unit is Mg
                NR_carbon_29yrs= NRrate * pixelSize2*29,
                NR_carbon_30yrs= NRrate * pixelSize2*30, 
                n=length(NRrate), biome='TSMBF')
cBiome1_df2 %>% head

cBiome1_v2 <- cBiome1_df2 %>% 
  dplyr::mutate(meanNRrate = mean(NRrate, na.rm=T),
                totalNR_C9yrs = sum(NR_carbon_9yrs)/1000000000,
                totalNR_C29yrs = sum(NR_carbon_29yrs)/1000000000,
                totalNR_C30yrs = sum(NR_carbon_30yrs)/1000000000,
                n = unique(n), 
                areaMillHa = n * pixelSize2/ 1000000, #unit is million hectare
                biome='TSMBF') %>% 
  dplyr::select(n, areaMillHa, biome,  meanNRrate, totalNR_C9yrs, totalNR_C29yrs, totalNR_C30yrs ) %>% dplyr::distinct()
cBiome1_v2

#biome 7 
cBiome7_df2 <- restorBiome7_df2 %>%  
  dplyr::mutate(NR_carbon_9yrs= NRrate * pixelSize2*9,  #unit is Mg
                NR_carbon_29yrs= NRrate * pixelSize2*29,
                NR_carbon_30yrs= NRrate * pixelSize2*30, 
                n=length(NRrate), biome ='TSGSS')
cBiome7_df2 %>% head
cBiome7_v2 <- cBiome7_df2 %>% 
  dplyr::mutate(meanNRrate = mean(NRrate, na.rm=T),
                totalNR_C9yrs = sum(NR_carbon_9yrs)/1000000000,
                totalNR_C29yrs = sum(NR_carbon_29yrs)/1000000000,
                totalNR_C30yrs = sum(NR_carbon_30yrs)/1000000000,
                n = unique(n), areaMillHa = n * pixelSize2/ 1000000, #unit is million hectare
                biome='TSGSS') %>% 
  dplyr::select(n,areaMillHa, biome,  meanNRrate, totalNR_C9yrs, totalNR_C29yrs, totalNR_C30yrs ) %>%  dplyr::distinct()
cBiome7_v2

#biome10
cBiome10_df2 <- restorBiome10_df2 %>%  
  dplyr::mutate(NR_carbon_9yrs= NRrate * pixelSize2*9,  #unit is Mg
                NR_carbon_29yrs= NRrate * pixelSize2*29, 
                NR_carbon_30yrs= NRrate * pixelSize2*30, 
                n=length(NRrate),biome='MGS' )
cBiome10_df2 %>% head
cBiome10_v2 <- cBiome10_df2 %>% 
  dplyr::mutate(meanNRrate = mean(NRrate, na.rm=T),
                totalNR_C9yrs = sum(NR_carbon_9yrs)/1000000000,  #Gt
                totalNR_C29yrs = sum(NR_carbon_29yrs)/1000000000,
                totalNR_C30yrs = sum(NR_carbon_30yrs)/1000000000,
                n = unique(n), areaMillHa = n * pixelSize2/ 1000000, #unit is million hectare
                biome='MGS') %>% 
  dplyr::select(n,areaMillHa,biome,  meanNRrate, totalNR_C9yrs, totalNR_C29yrs,  totalNR_C30yrs) %>% dplyr::distinct()
cBiome10_v2

cEA2 <- rbind(cBiome1_v2, cBiome7_v2, cBiome10_v2) #%>% dplyr::mutate(FR='NR')
# save(cEA2, file='ATT_results_save/att_imp/NR_SR_LR_Cstock.Rdata')

# calculate carbon per unit areas for AR and ARR 
# ** Cook Patton et al used  0.47 for AGB to carbon 
ATTtable <- read.csv('ATT_calc/att_table_v2.csv')

cf <- 0.47

ATT_imp <- ATTtable %>% dplyr::left_join(cEA2, by='biome') %>% 
  dplyr::mutate(aveFRrate= Average +meanNRrate,
                longtermFRrate = longTerm + meanNRrate, 
                shorttermFRrate = shortTerm + meanNRrate) %>% 
  dplyr::mutate(aveFR_C9yrs = aveFRrate *9*(n*pixelSize2) /1000000000*cf,   #Gt
                aveFR_C29yrs= aveFRrate *29*(n*pixelSize2) /1000000000*cf,
                stFR_C9yrs= shorttermFRrate *9*(n*pixelSize2) /1000000000*cf,
                ltFR_C29yrs= longtermFRrate *29*(n*pixelSize2) /1000000000*cf,
                ltFR_C30yrs= longtermFRrate *30*(n*pixelSize2) /1000000000*cf) %>% 
  dplyr::select(biome, FR,  longtermFRrate,stFR_C9yrs,ltFR_C29yrs, ltFR_C30yrs)

ATT_nr0 <- ATTtable %>% dplyr::left_join(cEA2, by='biome') %>% 
  dplyr::mutate(aveFRrate= Average +meanNRrate,
                longtermFRrate = longTerm + meanNRrate, 
                shorttermFRrate = shortTerm + meanNRrate) %>% 
  dplyr::mutate(aveFR_C9yrs = aveFRrate *9*(n*pixelSize2) /1000000000*cf,   #Gt
                aveFR_C29yrs= aveFRrate *29*(n*pixelSize2) /1000000000*cf,
                stFR_C9yrs= shorttermFRrate *9*(n*pixelSize2) /1000000000*cf,
                ltFR_C29yrs= longtermFRrate *29*(n*pixelSize2) /1000000000*cf,
                ltFR_C30yrs= longtermFRrate *30*(n*pixelSize2) /1000000000*cf) %>% 
  dplyr::mutate(FR='NR') 

sum(unique(ATT_nr0$n)*pixelSize2 )  #for getting total restor area in hectare


ATT_nr <- ATTtable %>% dplyr::left_join(cEA2, by='biome') %>% 
  dplyr::mutate(aveFRrate= Average +meanNRrate,
                longtermFRrate = longTerm + meanNRrate, 
                shorttermFRrate = shortTerm + meanNRrate) %>% 
  dplyr::mutate(aveFR_C9yrs = aveFRrate *9*(n*pixelSize2) /1000000000*cf,   #Gt
                aveFR_C29yrs= aveFRrate *29*(n*pixelSize2) /1000000000*cf,
                stFR_C9yrs= shorttermFRrate *9*(n*pixelSize2) /1000000000*cf,
                ltFR_C29yrs= longtermFRrate *29*(n*pixelSize2) /1000000000*cf,
                ltFR_C30yrs= longtermFRrate *30*(n*pixelSize2) /1000000000*cf) %>% 
  dplyr::mutate(FR='NR') %>% 
  dplyr::select(biome, FR, meanNRrate, totalNR_C9yrs, totalNR_C29yrs,  totalNR_C30yrs)


colnames(ATT_imp) <- colnames(ATT_nr)

ATT_comb <- ATT_imp %>% rbind(ATT_nr) %>% 
  pivot_longer(cols = c(totalNR_C9yrs, totalNR_C29yrs,  totalNR_C30yrs),
               names_to = "time_horizon",
               values_to = "C_stock") %>% dplyr::distinct() %>% dplyr::arrange(biome) %>% data.frame()

ATT_comb 
# save(ATT_comb, file='ATT_results_save/att_imp/allFR_SR_LR_Cstock.Rdata')

#summing up short / long term by FR 
ATT_comb %>% dplyr::group_by(time_horizon, FR) %>% dplyr::summarise(sumC =sum(C_stock)) %>% print()   #total by FR x biome


ATT_comb %>% dplyr::group_by(time_horizon, biome) %>% dplyr::summarise(maxC =max(C_stock)) %>% dplyr::arrange(biome)%>% print()  #picking out the best by biome


ATT_comb %>% dplyr::group_by(time_horizon, biome) %>% dplyr::summarise(maxC =max(C_stock)) %>%   #summing C for two time horizon using the best FR
  dplyr::group_by(time_horizon) %>% dplyr::summarise(sumC =sum(maxC))%>% print()

#---------make stacked bar chart--------------
library(viridis)
library(hrbrthemes)
restoredC_dynamic <- ATT_comb %>% dplyr::filter(time_horizon %notin% c("totalNR_C30yrs")) %>% 
  dplyr::mutate(time_horizon=factor(time_horizon, levels =c('totalNR_C9yrs', 'totalNR_C29yrs')),
                FR =factor(FR, levels=c('NR', 'ANR', 'AR')),
                biome=factor(biome, labels = c('Montane grasslands & shurblands', 
                                               'Tropical & subtropical grasslands, savannas, & shrublands',
                                               'Tropical & subtropical moist broadleaf forests'))) %>% 
  ggplot(., aes(fill=FR, y=C_stock, x=biome)) + 
  geom_bar(position="dodge", stat="identity") +
  facet_wrap(vars(time_horizon), labeller = labeller(time_horizon = 
                                                       c("totalNR_C9yrs" = "C stored in restored area for 2030",
                                                         "totalNR_C29yrs" = "C stored in restored area for 2050")
  ))+
  scale_fill_manual(values = c("#159122", "#663695", "#1071f4" ),name='Forest restortaion stretagies', 
                    labels=c('Natural Regeneration','Assisted Natural Regeneration','Active Restoration'))+
  theme_bw()+
  theme_ipsum() +
  theme(strip.text = element_text(size=12), axis.text.x = element_text(angle=25, vjust=1, hjust=1),
        plot.margin = margin(0,0,0,4, "cm"))+ 
  xlab("")+ylab("Restored C stock (Gt)")

restoredC_dynamic
ggsave('Fig/Fig4_carbon_implictaion_dynamic.png',restoredC_dynamic,width = 3000, height = 2000, units = 'px', dpi=300 )


#--------make average vaelus ---------------

ATT_ave <- ATTtable %>% dplyr::left_join(cEA2, by='biome') %>% 
  dplyr::mutate(aveFRrate= Average +meanNRrate,
                longtermFRrate = longTerm + meanNRrate, 
                shorttermFRrate = shortTerm + meanNRrate) %>% 
  dplyr::mutate(aveFR_C9yrs = aveFRrate *9*(n*pixelSize2) /1000000000*cf,   #Gt
                aveFR_C29yrs= aveFRrate *29*(n*pixelSize2) /1000000000*cf,
                aveFR_C30yrs= aveFRrate *29*(n*pixelSize2) /1000000000*cf,
                stFR_C9yrs= shorttermFRrate *9*(n*pixelSize2) /1000000000*cf,
                ltFR_C29yrs= longtermFRrate *29*(n*pixelSize2) /1000000000*cf) %>% 
  dplyr::select(biome, FR , aveFRrate, aveFR_C9yrs, aveFR_C29yrs, aveFR_C30yrs)

colnames(ATT_ave) <- colnames(ATT_nr)

ATT_comb2 <- ATT_ave %>% rbind(ATT_nr) %>% 
  pivot_longer(cols = c(totalNR_C9yrs, totalNR_C29yrs, totalNR_C30yrs),
               names_to = "time_horizon",
               values_to = "C_stock") %>% dplyr::arrange(biome)
ATT_comb2

#summing up short / long term by FR 
ATT_comb2 %>% dplyr::group_by(time_horizon, FR) %>% dplyr::summarise(sumC =sum(C_stock)) %>% print()

ATT_comb2 %>% dplyr::group_by(time_horizon, biome) %>% dplyr::summarise(maxC =max(C_stock))%>% print()  #picking out the best by biome

ATT_comb2 %>% dplyr::group_by(time_horizon, biome) %>% dplyr::summarise(maxC =max(C_stock)) %>%   #summing C for two time horizon using the best FR
  dplyr::group_by(time_horizon) %>% dplyr::summarise(sumC =sum(maxC))%>% print()




restoredC_ave <- ATT_comb2 %>% dplyr::filter(time_horizon %notin% c("totalNR_C30yrs")) %>% 
  dplyr::mutate(time_horizon=factor(time_horizon, levels =c('totalNR_C9yrs', 'totalNR_C29yrs')),
                FR =factor(FR, levels=c('NR', 'ANR', 'AR')),
                biome=factor(biome, labels = c('Montane grasslands & shurblands', 
                                               'Tropical & subtropical grasslands, savannas, & shrublands',
                                               'Tropical & subtropical moist broadleaf forests'))) %>% 
  ggplot(., aes(fill=FR, y=C_stock, x=biome)) + 
  geom_bar(position="dodge", stat="identity") +
  facet_wrap(vars(time_horizon), labeller = labeller(time_horizon = 
                                                       c("totalNR_C9yrs" = "C stored in restored area for 2030",
                                                         "totalNR_C29yrs" = "C stored in restored area for 2050")
  ))+
  scale_fill_manual(values = c("#159122", "#663695", "#1071f4" ),name='Forest restortaion stretagies', 
                    labels=c('Natural Regeneration','Assisted Natural Regeneration','Active Restoration'))+
  theme_bw()+
  theme_ipsum() +
  theme(strip.text = element_text(size=12), axis.text.x = element_text(angle=25, vjust=1, hjust=1),
        plot.margin = margin(0,0,0,4, "cm"))+ 
  xlab("")+ylab("Restored C stock (Gt)")

restoredC_ave


ggsave('Fig/Fig4_carbon_implictaion_average.png',restoredC_ave,width = 3000, height = 2000, units = 'px', dpi=300 )

