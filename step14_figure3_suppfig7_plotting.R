## step14 is using the retsortaion opportunity map from Griscom et al to extrapolate the cabron caccumulation rate 
        #this SE version uses the ATT SE and ropagate to the final estimations#

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

rateMap2_res <- raster::resample( rateMap2,ras1km, method='bilinear')
restorMap2_res <- raster::resample( restorMap2,ras1km, method='bilinear')

# #get the NR rate over restorRA2 
# rateMap2_res[is.na(values(restorMap2_res))] <- NA   # 14528 1km pixels for >=50    3850 for >= 70

names(rateMap2_res) <- 'cookpatton_NR_rate'

print(sum(is.na(rateMap2_res[])))   #5683640
print(length(rateMap2_res[]))
print(length(rateMap2_res[])- sum(is.na(rateMap2_res[])))

# writeRaster(rateMap2_res,'NCS_Refor11_map_with_description_zip/cookpatton_griscom+natureMap1km_reprj.tif', overwrite=TRUE)



##------------------do the same for the error ratio map--------------------------

rateMap3 <- raster('NCS_Refor11_map_with_description_zip/cookpattonER_griscom_reprj.tif')

restorMap2 <- raster('NatureMap_prioritymaps/ea_reprj.tif')
# restorMap2[restorMap2<50]<-NA
## reproject to equal area  [much faster in qgis]
prj <- '+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs'

poly <- readOGR('AOI/EA_one.shp') %>% 
  spTransform(., CRS(prj))
# rateMap2_prj <- projectRaster(rateMap2, crs = prj)

#resampel rate map to match 1km raster 
ras1km <- raster('NCS_Refor11_map_with_description_zip/wwf_ea_1km.tif')

rateMap3_res <- raster::resample( rateMap3,ras1km, method='bilinear')
restorMap2_res <- raster::resample( restorMap2,ras1km, method='bilinear')

# #get the NR rate over restorRA2 
# rateMap2_res[is.na(values(restorMap2_res))] <- NA   # 14528 1km pixels for >=50    3850 for >= 70

names(rateMap3_res) <- 'cookpatton_NR_rate_ER'

print(sum(is.na(rateMap3_res[])))   #5683640
print(length(rateMap3_res[]))
print(length(rateMap3_res[])- sum(is.na(rateMap3_res[])))

#####   stack rateMap2 & rateMap3   ###

rateMap2_res <- stack(rateMap2_res, rateMap3_res)


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
  dplyr::rename(NRrate = cookpatton_NR_rate) %>% 
  dplyr::mutate(NRse = NRrate*cookpatton_NR_rate_ER) %>% dplyr::select(-c(cookpatton_NR_rate_ER))
dim(restorBiome1_df2)
restorBiome7_df2 <- as.data.frame(restorBiome7_v2) %>% 
  dplyr::filter(!is.na(cookpatton_NR_rate)) %>%  #n = 58440
  dplyr::rename(NRrate =cookpatton_NR_rate) %>% 
  dplyr::mutate(NRse = NRrate*cookpatton_NR_rate_ER) %>% dplyr::select(-c(cookpatton_NR_rate_ER))
dim(restorBiome7_df2)
restorBiome10_df2 <- as.data.frame(restorBiome10_v2) %>% 
  dplyr::filter(!is.na(cookpatton_NR_rate)) %>%  #n = 4854
  dplyr::rename(NRrate = cookpatton_NR_rate) %>% 
  dplyr::mutate(NRse = NRrate*cookpatton_NR_rate_ER) %>% dplyr::select(-c(cookpatton_NR_rate_ER))
dim(restorBiome10_df2)

### calculate C stock in NR ###
pixelSize2  <-  1.043275*1.043275*100 #1*1*100  #unit is hectare, from sq km to ha
#biome 1 
cBiome1_df2 <- restorBiome1_df2 %>%  
  dplyr::mutate(NR_carbon_9yrs= NRrate * pixelSize2*9,  #unit is Mg
                NR_carbon_29yrs= NRrate * pixelSize2*29,
                NR_carbon_30yrs= NRrate * pixelSize2*30, 
                NR_carbonSE_9yrs= NRse * pixelSize2*9,  #unit is Mg
                NR_carbonSE_29yrs= NRse * pixelSize2*29,
                NR_carbonSE_30yrs= NRse * pixelSize2*30, 
                n=length(NRrate), biome='TSMBF')
cBiome1_df2 %>% head

cBiome1_v2 <- cBiome1_df2 %>% 
  dplyr::mutate(meanNRrate = mean(NRrate, na.rm=T),
                meanNRse = mean(NRse, na.rm=T),
                totalNR_C9yrs = sum(NR_carbon_9yrs)/1000000000,
                totalNR_C29yrs = sum(NR_carbon_29yrs)/1000000000,
                totalNR_C30yrs = sum(NR_carbon_30yrs)/1000000000,
                totalNRse_C9yrs = sum(NR_carbonSE_9yrs, na.rm=T)/1000000000,
                totalNRse_C29yrs = sum(NR_carbonSE_29yrs, na.rm=T)/1000000000,
                totalNRse_C30yrs = sum(NR_carbonSE_30yrs, na.rm=T)/1000000000,
                n = unique(n), 
                areaMillHa = n * pixelSize2/ 1000000, #unit is million hectare
                biome='TSMBF') %>% 
  dplyr::select(n, areaMillHa, biome,  meanNRrate,meanNRse, totalNR_C9yrs, totalNR_C29yrs, totalNR_C30yrs ,
                totalNRse_C9yrs, totalNRse_C29yrs, totalNRse_C30yrs) %>% dplyr::distinct()
cBiome1_v2

#biome 7 
cBiome7_df2 <- restorBiome7_df2 %>%  
  dplyr::mutate(NR_carbon_9yrs= NRrate * pixelSize2*9,  #unit is Mg
                NR_carbon_29yrs= NRrate * pixelSize2*29,
                NR_carbon_30yrs= NRrate * pixelSize2*30, 
                NR_carbonSE_9yrs= NRse * pixelSize2*9,  #unit is Mg
                NR_carbonSE_29yrs= NRse * pixelSize2*29,
                NR_carbonSE_30yrs= NRse * pixelSize2*30, 
                n=length(NRrate), biome ='TSGSS')
cBiome7_df2 %>% head
cBiome7_v2 <- cBiome7_df2 %>% 
  dplyr::mutate(meanNRrate = mean(NRrate, na.rm=T),
                meanNRse = mean(NRse, na.rm=T),
                totalNR_C9yrs = sum(NR_carbon_9yrs)/1000000000,
                totalNR_C29yrs = sum(NR_carbon_29yrs)/1000000000,
                totalNR_C30yrs = sum(NR_carbon_30yrs)/1000000000,
                totalNRse_C9yrs = sum(NR_carbonSE_9yrs, na.rm=T)/1000000000,
                totalNRse_C29yrs = sum(NR_carbonSE_29yrs, na.rm=T)/1000000000,
                totalNRse_C30yrs = sum(NR_carbonSE_30yrs, na.rm=T)/1000000000,
                n = unique(n), areaMillHa = n * pixelSize2/ 1000000, #unit is million hectare
                biome='TSGSS') %>% 
  dplyr::select(n, areaMillHa, biome,  meanNRrate,meanNRse, totalNR_C9yrs, totalNR_C29yrs, totalNR_C30yrs ,
                totalNRse_C9yrs, totalNRse_C29yrs, totalNRse_C30yrs) %>% dplyr::distinct()
cBiome7_v2

#biome10
cBiome10_df2 <- restorBiome10_df2 %>%  
  dplyr::mutate(NR_carbon_9yrs= NRrate * pixelSize2*9,  #unit is Mg
                NR_carbon_29yrs= NRrate * pixelSize2*29, 
                NR_carbon_30yrs= NRrate * pixelSize2*30, 
                NR_carbonSE_9yrs= NRse * pixelSize2*9,  #unit is Mg
                NR_carbonSE_29yrs= NRse * pixelSize2*29,
                NR_carbonSE_30yrs= NRse * pixelSize2*30, 
                n=length(NRrate),biome='MGS' )
cBiome10_df2 %>% head
cBiome10_v2 <- cBiome10_df2 %>% 
  dplyr::mutate(meanNRrate = mean(NRrate, na.rm=T),
                meanNRse = mean(NRse, na.rm=T),
                totalNR_C9yrs = sum(NR_carbon_9yrs)/1000000000,  #Gt
                totalNR_C29yrs = sum(NR_carbon_29yrs)/1000000000,
                totalNR_C30yrs = sum(NR_carbon_30yrs)/1000000000,
                totalNRse_C9yrs = sum(NR_carbonSE_9yrs, na.rm=T)/1000000000,
                totalNRse_C29yrs = sum(NR_carbonSE_29yrs, na.rm=T)/1000000000,
                totalNRse_C30yrs = sum(NR_carbonSE_30yrs, na.rm=T)/1000000000,
                n = unique(n), areaMillHa = n * pixelSize2/ 1000000, #unit is million hectare
                biome='MGS') %>% 
  dplyr::select(n, areaMillHa, biome,  meanNRrate,meanNRse, totalNR_C9yrs, totalNR_C29yrs, totalNR_C30yrs ,
                totalNRse_C9yrs, totalNRse_C29yrs, totalNRse_C30yrs) %>% dplyr::distinct()
cBiome10_v2

cEA2 <- rbind(cBiome1_v2, cBiome7_v2, cBiome10_v2) #%>% dplyr::mutate(FR='NR')
# save(cEA2, file='ATT_results_save/att_imp/NR_SR_LR_Cstock+se.Rdata')

# calculate carbon per unit areas for AR and ARR 
# ** Cook Patton et al used  0.47 for AGB to carbon 

cf <- 0.47
ATTtable <- read.csv('ATT_calc/att_table_v44.csv')
ATTtable[,4:9] <- ATTtable[,4:9]*cf


ATT_imp <- ATTtable %>% dplyr::left_join(cEA2, by='biome') %>% #this is for aggregtaing the C for short- and long-term FR
  dplyr::mutate(aveFRrate= Average +meanNRrate,
                longtermFRrate = longTerm + meanNRrate, 
                shorttermFRrate = shortTerm + meanNRrate) %>% 
  dplyr::mutate(aveFRrate_se= Average_se + meanNRse,            #adding NR SE and AR/ANR SE for FR SE 
                longtermFRrate_se = longTerm_se + meanNRse, 
                shorttermFRrate_se = shortTerm_se + meanNRse) %>% 
  dplyr::mutate(aveFR_C9yrs = aveFRrate *9*(n*pixelSize2) /1000000000,   #Gt
                aveFR_C29yrs= aveFRrate *29*(n*pixelSize2) /1000000000,
                stFR_C9yrs= shorttermFRrate *9*(n*pixelSize2) /1000000000,
                ltFR_C29yrs= longtermFRrate *29*(n*pixelSize2) /1000000000,
                ltFR_C30yrs= longtermFRrate *30*(n*pixelSize2) /1000000000) %>% 
  dplyr::mutate(aveFR_C9yrs_se = aveFRrate_se *9*(n*pixelSize2) /1000000000,   #Gt
                aveFR_C29yrs_se= aveFRrate_se *29*(n*pixelSize2) /1000000000,
                stFR_C9yrs_se= shorttermFRrate_se *9*(n*pixelSize2) /1000000000,
                ltFR_C29yrs_se= longtermFRrate_se*29*(n*pixelSize2) /1000000000,
                ltFR_C30yrs_se= longtermFRrate_se *30*(n*pixelSize2) /1000000000) %>% 
  dplyr::select(biome, FR,n, stFR_C9yrs, ltFR_C29yrs, ltFR_C30yrs, stFR_C9yrs_se, ltFR_C29yrs_se, ltFR_C30yrs_se)

ATT_nr0 <- ATTtable %>% dplyr::left_join(cEA2, by='biome') %>% #for getting the rates to compile Table 1
  dplyr::mutate(aveFRrate= Average +meanNRrate,
                longtermFRrate = longTerm + meanNRrate, 
                shorttermFRrate = shortTerm + meanNRrate) %>% 
  dplyr::mutate(aveFR_C9yrs = aveFRrate *9*(n*pixelSize2) /1000000000,   #Gt
                aveFR_C29yrs= aveFRrate *29*(n*pixelSize2) /1000000000,
                stFR_C9yrs= shorttermFRrate *9*(n*pixelSize2) /1000000000,
                ltFR_C29yrs= longtermFRrate *29*(n*pixelSize2) /1000000000,
                ltFR_C30yrs= longtermFRrate *30*(n*pixelSize2) /1000000000) %>% 
  dplyr::mutate(FR='NR') 

sum(unique(ATT_nr0$n)*pixelSize2 )  #for getting total restor area in hectare   14242556


ATT_nr <- ATTtable %>% dplyr::left_join(cEA2, by='biome') %>% 
  dplyr::mutate(aveFRrate= Average +meanNRrate,
                longtermFRrate = longTerm + meanNRrate, 
                shorttermFRrate = shortTerm + meanNRrate) %>% 
  dplyr::mutate(aveFRrate_se= Average_se + meanNRse,            #adding NR SE and AR/ANR SE for FR SE 
                longtermFRrate_se = longTerm_se + meanNRse, 
                shorttermFRrate_se = shortTerm_se + meanNRse) %>% 
  dplyr::mutate(aveFR_C9yrs = aveFRrate *9*(n*pixelSize2) /1000000000,   #Gt
                aveFR_C29yrs= aveFRrate *29*(n*pixelSize2) /1000000000,
                stFR_C9yrs= shorttermFRrate *9*(n*pixelSize2) /1000000000,
                ltFR_C29yrs= longtermFRrate *29*(n*pixelSize2) /1000000000,
                ltFR_C30yrs= longtermFRrate *30*(n*pixelSize2) /1000000000) %>% 
  dplyr::mutate(aveFR_C9yrs_se = aveFRrate_se *9*(n*pixelSize2) /1000000000,   #Gt
                aveFR_C29yrs_se= aveFRrate_se *29*(n*pixelSize2) /1000000000,
                stFR_C9yrs_se= shorttermFRrate_se *9*(n*pixelSize2) /1000000000,
                ltFR_C29yrs_se= longtermFRrate_se*29*(n*pixelSize2) /1000000000,
                ltFR_C30yrs_se= longtermFRrate_se *30*(n*pixelSize2) /1000000000) %>% 
  dplyr::mutate(FR='NR') %>%    #up till here same, just select the NR-related columns
  dplyr::select(biome, FR, n, totalNR_C9yrs, totalNR_C29yrs, totalNR_C30yrs, totalNRse_C9yrs, totalNRse_C29yrs, totalNRse_C30yrs)


colnames(ATT_imp) <- colnames(ATT_nr)

ATT_comb <- ATT_imp %>% rbind(ATT_nr) %>% 
  pivot_longer(cols = c(totalNR_C9yrs, totalNR_C29yrs,  totalNR_C30yrs, totalNRse_C9yrs, totalNRse_C29yrs,  totalNRse_C30yrs),
               names_to = "time_horizon",
               values_to = "C_stock") %>% dplyr::distinct() %>% dplyr::arrange(biome) %>% data.frame() %>% 
  dplyr::mutate(value = ifelse(grepl('se',time_horizon, fixed = TRUE), 'C_SE','C_stock')) %>% 
  dplyr::mutate(time_horizon = sub("^[^_]*_", "", time_horizon)) %>% 
  pivot_wider(names_from = value, values_from= C_stock)

ATT_comb %>% data.frame()
# save(ATT_comb, file='ATT_results_save/att_imp/allFR_SR_LR_Cstock_SE.Rdata')

#summing up short / long term by FR 
ATT_comb %>% dplyr::group_by(time_horizon, FR) %>% dplyr::summarise(biome=unique(biome),sumC =sum(C_stock), SE= sum(C_SE)) %>% 
  dplyr::arrange(biome) %>% print()   #total by FR x biome


ATT_comb %>% dplyr::group_by(time_horizon, biome) %>% dplyr::top_n(1, C_stock) %>% 
  dplyr::arrange(biome)%>% print()   #picking out the best by biome, right side of Table 1
  
# ATT_comb %>% dplyr::group_by(time_horizon, biome) %>%   dplyr::summarise(maxC =max(C_stock)) %>% dplyr::arrange(biome)%>% print()  

ATT_comb %>% dplyr::group_by(time_horizon, biome) %>% dplyr::top_n(1, C_stock)%>%   #summing C for two time horizon using the best FR
  dplyr::group_by(time_horizon) %>% dplyr::summarise(sumC =sum(C_stock), seC = sum(C_SE))%>% print()

#---------make stacked bar chart--------------
library(viridis)
library(hrbrthemes)
restoredC_dynamic <- ATT_comb %>% dplyr::filter(time_horizon %notin% c("C30yrs")) %>% 
  dplyr::mutate(time_horizon=factor(time_horizon, levels =c('C9yrs', 'C29yrs')),
                FR =factor(FR, levels=c('NR', 'ANR', 'AR')),
                biome=factor(biome,#levels = c('TSMBF','TSGSS','MGS'),
                             labels = c('Montane shrublands & forests','Savannas & dry forests','Tropical moist forests'))) %>% 
  ggplot(., aes(fill=FR, y=C_stock, x=biome)) + 
  geom_bar(position="dodge", stat="identity", width = 0.8) +
  geom_errorbar( aes(x=biome, ymin=C_stock-C_SE, ymax=C_stock+C_SE), position=position_dodge(width = 0.8),
                 width=0.1, size=0.3, colour="black", alpha=0.9)+
  facet_wrap(vars(time_horizon), labeller = labeller(time_horizon = 
                                                       c("C9yrs" = "C removal capacity by restoring suitable areas by 2030",
                                                         "C29yrs" = "C removal capacity by restoring suitable areas by 2050") ))+
  scale_fill_manual(values = c("#159122", "#663695", "#1071f4" ),name='Forest restortaion stretagies', 
                    labels=c('Natural Regeneration','Assisted Natural Regeneration','Active Restoration'))+
  theme_bw()+
  theme_ipsum() +
  # theme(strip.text = element_text(size=12), axis.text.x = element_text(angle=25, vjust=1, hjust=1),
  #       plot.margin = margin(0,0,0,4, "cm"))+ 
  xlab("")+ylab("C removal capacity (Gt C)")+#legend.position = "bottom",
  theme( plot.margin = margin(0,0,0,5, "cm"))+
  theme(strip.background.x=element_blank() ,strip.background.y =element_rect(fill="grey", color='white'),
        strip.text = element_text(size=18,family = 'serif' ),
        # strip.text.x = element_blank(), axis.title=element_blank(),
        # text=element_text(size=16, family = 'serif'),
        axis.title.y = element_text( size =18, angle = 90, hjust = .5, vjust = 1,family = 'serif'),
        axis.text.y = element_text(size=16, vjust=1, hjust=1, family = 'serif'),
        axis.text.x = element_text(angle=16,size=16, vjust=1, hjust=1, family = 'serif'))+
  theme(legend.text=element_text(size=18,family = 'serif' ),legend.title=element_text(size=18,family = 'serif'))

restoredC_dynamic


tag_facet2 <-  function(p, open="", close = ")", tag_pool =letters, x = 0, y = 0.3, hjust = 0, vjust = 0.3,  
                        fontface = 2, family="serif", ...){
  gb <- ggplot_build(p)
  lay <- gb$layout$layout
  nm <- names(gb$layout$facet$params$rows)
  
  tags <- paste0(open,tag_pool[unique(lay$COL)],close)
  
  tl <- lapply(tags, grid::textGrob, x=x, y=y,
               hjust=hjust, vjust=vjust, gp=grid::gpar(fontface=fontface, family="serif"))
  
  g <- ggplot_gtable(gb)
  g <- gtable::gtable_add_rows(g, grid::unit(1,"line"), pos = 0)
  lm <- unique(g$layout[grepl("panel",g$layout$name), "l"])
  g <- gtable::gtable_add_grob(g, grobs = tl, t=1, l=lm)
  grid::grid.newpage()
  grid::grid.draw(g)
  return(g)
}

t <- tag_facet2(restoredC_dynamic)

ggsave('Fig/Fig4_carbon_implictaion_dynamic_SE_recalc5_modified.png',t,width =9000, height =4200, units = 'px', dpi=500 )


#--------make average values ---------------

ATT_ave <- ATTtable %>% dplyr::left_join(cEA2, by='biome') %>% 
  dplyr::mutate(aveFRrate= Average +meanNRrate,
                longtermFRrate = longTerm + meanNRrate, 
                shorttermFRrate = shortTerm + meanNRrate) %>% 
  dplyr::mutate(aveFRrate_se= Average_se + meanNRse,            #adding NR SE and AR/ANR SE for FR SE 
                longtermFRrate_se = longTerm_se + meanNRse, 
                shorttermFRrate_se = shortTerm_se + meanNRse) %>% 
  dplyr::mutate(aveFR_C9yrs = aveFRrate *9*(n*pixelSize2) /1000000000,   #Gt
                aveFR_C29yrs= aveFRrate *29*(n*pixelSize2) /1000000000,
                aveFR_C30yrs= aveFRrate *30*(n*pixelSize2) /1000000000,
                stFR_C9yrs= shorttermFRrate *9*(n*pixelSize2) /1000000000,
                ltFR_C29yrs= longtermFRrate *29*(n*pixelSize2) /1000000000,
                ltFR_C30yrs= longtermFRrate *30*(n*pixelSize2) /1000000000) %>% 
  dplyr::mutate(aveFR_C9yrs_se = aveFRrate_se *9*(n*pixelSize2) /1000000000,   #Gt
                aveFR_C29yrs_se= aveFRrate_se *29*(n*pixelSize2) /1000000000,
                aveFR_C30yrs_se= aveFRrate_se *30*(n*pixelSize2) /1000000000,
                stFR_C9yrs_se= shorttermFRrate_se *9*(n*pixelSize2) /1000000000,
                ltFR_C29yrs_se= longtermFRrate_se*29*(n*pixelSize2) /1000000000,
                ltFR_C30yrs_se= longtermFRrate_se *30*(n*pixelSize2) /1000000000) %>% 
  dplyr::select(biome, FR,n,  aveFR_C9yrs, aveFR_C29yrs, aveFR_C30yrs, aveFR_C9yrs_se, aveFR_C29yrs_se, aveFR_C30yrs_se)

colnames(ATT_ave) <- colnames(ATT_nr)

ATT_comb2 <-ATT_ave %>% rbind(ATT_nr) %>% 
  pivot_longer(cols = c(totalNR_C9yrs, totalNR_C29yrs,  totalNR_C30yrs, totalNRse_C9yrs, totalNRse_C29yrs,  totalNRse_C30yrs),
               names_to = "time_horizon",
               values_to = "C_stock") %>% dplyr::distinct() %>% dplyr::arrange(biome) %>% data.frame() %>% 
  dplyr::mutate(value = ifelse(grepl('se',time_horizon, fixed = TRUE), 'C_SE','C_stock')) %>% 
  dplyr::mutate(time_horizon = sub("^[^_]*_", "", time_horizon)) %>% 
  pivot_wider(names_from = value, values_from= C_stock) 

ATT_comb2 %>% data.frame()

#summing up short / long term by FR 
ATT_comb2 %>% dplyr::group_by(time_horizon, FR) %>% dplyr::summarise(sumC =sum(C_stock), SE= sum(C_SE)) %>% print()   #total by FR x biome

#ATT_comb2 %>% dplyr::group_by(time_horizon, biome) %>% dplyr::summarise(maxC =max(C_stock))%>% print()  #picking out the best by biome

ATT_comb2 %>% dplyr::group_by(time_horizon, biome) %>% dplyr::top_n(1, C_stock) %>% 
  dplyr::arrange(biome)%>% print()   #picking out the best by biome

ATT_comb2 %>% dplyr::group_by(time_horizon, biome) %>% dplyr::top_n(1, C_stock)%>%   #summing C for two time horizon using the best FR
  dplyr::group_by(time_horizon) %>% dplyr::summarise(sumC =sum(C_stock), seC = sum(C_SE))%>% print()


restoredC_ave <- ATT_comb2 %>% dplyr::filter(time_horizon %notin% c("C30yrs")) %>% 
  dplyr::mutate(time_horizon=factor(time_horizon, levels =c('C9yrs', 'C29yrs')),
                FR =factor(FR, levels=c('NR', 'ANR', 'AR')),
                biome=factor(biome,#levels = c('TSMBF','TSGSS','MGS'),
                             labels = c('Montane shrublands & forests','Savannas & dry forests','Tropical moist forests'))) %>% 
  ggplot(., aes(fill=FR, y=C_stock, x=biome)) + 
  geom_bar(position="dodge", stat="identity", width = 0.8) +
  geom_errorbar( aes(x=biome, ymin=C_stock-C_SE, ymax=C_stock+C_SE), position=position_dodge(width = 0.8),
                 width=0.1, size=0.3, colour="black", alpha=0.9)+
  facet_wrap(vars(time_horizon), labeller = labeller(time_horizon = 
                                                       c("C9yrs" = "C removal capacity by restoring suitable areas by 2030",
                                                         "C29yrs" = "C removal capacity by restoring suitable areas by 2050") ))+
  scale_fill_manual(values = c("#159122", "#663695", "#1071f4" ),name='Forest restortaion stretagies', 
                    labels=c('Natural Regeneration','Assisted Natural Regeneration','Active Restoration'))+
  ylim(0,2.5)+
  theme_bw()+
  theme_ipsum() +
  # theme(strip.text = element_text(size=12), axis.text.x = element_text(angle=25, vjust=1, hjust=1),
  #       plot.margin = margin(0,0,0,4, "cm"))+ 
  xlab("")+ylab("C removal capacity (Gt C)")+#legend.position = "bottom",
  theme( plot.margin = margin(0,0,0,5, "cm"))+
  theme(strip.background.x=element_blank() ,strip.background.y =element_rect(fill="grey", color='white'),
        strip.text = element_text(size=18,family = 'serif' ),
        # strip.text.x = element_blank(), axis.title=element_blank(),
        # text=element_text(size=16, family = 'serif'),
        axis.title.y = element_text( size =18, angle = 90, hjust = .5, vjust = 1,family = 'serif'),
        axis.text.y = element_text(size=16, vjust=1, hjust=1, family = 'serif'),
        axis.text.x = element_text(angle=16,size=16, vjust=1, hjust=1, family = 'serif'))+
  theme(legend.text=element_text(size=18,family = 'serif' ),legend.title=element_text(size=18,family = 'serif'))

restoredC_ave


t2 <- tag_facet2(restoredC_ave)

ggsave('Fig/Fig4_carbon_implictaion_average_SE_recalc5_modified.png',t2,width =9000, height =4200, units = 'px', dpi=500 )

