setwd('/gpfs/data1/duncansongp/amberliang/EA_data')
library(tidyr)
library(ggplot2)
library(ggpubr)

library(raster)
library(rgdal)
# poly <- readOGR('AOI/Kibale/kibale_managed_all6_cleanEcotrust.geojson')
poly <- readOGR('AOI/TISTUGA/TISTUGA_tsgss_t163060.geojson')
poly$year <- poly$timestamp
poly$interventi <- 'enclosure'
poly$manageType <- 'AR'

pattern <-'beam+2sig_no5_Sept_thre_tuned_dcol_nbr'    #'beam+2sig_no5_Sept_thre_tuned_dcol'

ras_temp <- raster(paste('agbd_predictions/kibale_ts_out3/agbd_2021_xgboost_sqrt_nbr_lhs_33_tile+', pattern,'_ls_lhs_medoid_173060_2021_NBR_3by3_dur8_biome1-0000000000-0000005120.tif', sep=''))
fact <-  raster::res(raster('covariates/UGA/d2roads.tif'))[1]/raster::res(ras_temp)[1]


##adding in the covaiarte raster, and ### 
d2road <- raster('covariates/UGA/d2roads.tif')%>% raster::disaggregate(.,fact=fact)
popcnt <- raster('covariates/UGA/pop_cnt_2000.tif') %>% raster::disaggregate(.,fact=fact)
slope <- raster('covariates/UGA/slope.tif')%>% raster::disaggregate(.,fact=fact)
dem <- raster('covariates/UGA/dem.tif')%>% raster::disaggregate(.,fact=fact)
precp <- raster('covariates/UGA/wc_prec_1990-1999.tif')%>% raster::disaggregate(.,fact=fact)
temp <- raster('covariates/UGA/wc_tavg_1990-1999.tif')%>% raster::disaggregate(.,fact=fact)
soil <- raster('covariates/UGA/soilType.tif') %>% raster::disaggregate(.,fact=fact)

start.time <- Sys.time()
tile_agbd_by_manage <- data.frame()
for (pred_year in 2021:1986){   #1986
  print(pred_year)
  # pred_year <- as.character(pred_year)
  if (pred_year < 2019  && pred_year>2015){
    annual_ras1 <- raster(paste('agbd_predictions/kibale_ts_out3/agbd_', pred_year,'_xgboost_sqrt_nbr_lhs_33_tile+', pattern,'_ls_lhs_medoid_173060_', pred_year,'_NBR_3by3_dur8_biome1_val-0000005120-0000002560.tif', sep=''))
    annual_ras2 <- raster(paste('agbd_predictions/kibale_ts_out3/agbd_', pred_year,'_xgboost_sqrt_nbr_lhs_33_tile+', pattern,'_ls_lhs_medoid_173060_', pred_year,'_NBR_3by3_dur8_biome1_val-0000002560-0000005120.tif', sep=''))
    annual_ras3 <- raster(paste('agbd_predictions/kibale_ts_out3/agbd_', pred_year,'_xgboost_sqrt_nbr_lhs_33_tile+', pattern,'_ls_lhs_medoid_173060_', pred_year,'_NBR_3by3_dur8_biome1_val-0000005120-0000005120.tif', sep=''))
    
  } else if (pred_year<=2015){
    annual_ras1 <- raster(paste('agbd_predictions/kibale_ts_out3/agbd_', pred_year,'_xgboost_sqrt_nbr_lhs_33_tile+', pattern,'_ls_lhs_medoid_173060_', pred_year,'_NBR_3by3_dur8_biome1_val_v2-0000005120-0000002560.tif', sep=''))
    annual_ras2 <- raster(paste('agbd_predictions/kibale_ts_out3/agbd_', pred_year,'_xgboost_sqrt_nbr_lhs_33_tile+', pattern,'_ls_lhs_medoid_173060_', pred_year,'_NBR_3by3_dur8_biome1_val_v2-0000002560-0000005120.tif', sep=''))
    annual_ras3 <- raster(paste('agbd_predictions/kibale_ts_out3/agbd_', pred_year,'_xgboost_sqrt_nbr_lhs_33_tile+', pattern,'_ls_lhs_medoid_173060_', pred_year,'_NBR_3by3_dur8_biome1_val_v2-0000005120-0000005120.tif', sep=''))
    
    
  } else if (pred_year %in% c(2021,2020,2019)){
    annual_ras1 <- raster(paste('agbd_predictions/kibale_ts_out3/agbd_', pred_year,'_xgboost_sqrt_nbr_lhs_33_tile+', pattern,'_ls_lhs_medoid_173060_', pred_year,'_NBR_3by3_dur8_biome1-0000005120-0000002560.tif', sep=''))
    annual_ras2 <- raster(paste('agbd_predictions/kibale_ts_out3/agbd_', pred_year,'_xgboost_sqrt_nbr_lhs_33_tile+', pattern,'_ls_lhs_medoid_173060_', pred_year,'_NBR_3by3_dur8_biome1-0000002560-0000005120.tif', sep=''))
    annual_ras3 <- raster(paste('agbd_predictions/kibale_ts_out3/agbd_', pred_year,'_xgboost_sqrt_nbr_lhs_33_tile+', pattern,'_ls_lhs_medoid_173060_', pred_year,'_NBR_3by3_dur8_biome1-0000005120-0000005120.tif', sep=''))
    
  }
  
  names(annual_ras1) <- paste('est_agbd', sep='')
  names(annual_ras2) <- paste('est_agbd',  sep='')
  names(annual_ras3) <- paste('est_agbd',  sep='')
  
  annual_agbd_by_manage <- data.frame()
  for (pl in 1:length(poly)){
    # print('ploy out of 47')
    # print(pl)
    spoly <- poly[pl,]
    sub <- '1'
    if (!class(try(intersect(annual_ras1,spoly),T ))=='try-error'){
      annual1_sub <- raster::crop(annual_ras1, spoly) %>% raster::mask(.,spoly)
      covar1 <- raster::stack(d2road, popcnt, slope, dem, precp, temp) %>% projectRaster(from=., to=annual1_sub, method="bilinear")
      covar2 <- raster::stack(soil) %>% projectRaster(from=., to=annual1_sub, method="bilinear")
      covar1c <- raster::crop(covar1, spoly) %>% raster::mask(.,spoly)
      covar2c <- raster::crop(covar2, spoly) %>% raster::mask(.,spoly)
      annual1_sub_covar <- stack(annual1_sub, covar1c, covar2c)
      annual1_sub2 <- annual1_sub_covar %>% as.data.frame(., xy = TRUE) %>%
        mutate(manage_stratum = spoly$year, interv = spoly$interventi, interv2 = spoly$manageType, estimat_year =pred_year, ras_sub =sub)
      annual1_sub2 <-  annual1_sub2 %>% dplyr::filter(!is.na(est_agbd))
    }else{
      annual1_sub2 <- data.frame()
    }
    
    sub2 <- '2'
    if (!class(try(intersect(annual_ras2,spoly),T ))=='try-error'){
      annual1_sub_v2 <- raster::crop(annual_ras2, spoly) %>% raster::mask(.,spoly)
      covar1b <- raster::stack(d2road, popcnt, slope, dem, precp, temp) %>% projectRaster(from=., to= annual1_sub_v2, method="bilinear")
      covar2b <- raster::stack(soil) %>% projectRaster(from=., to= annual1_sub_v2, method="bilinear")
      covar1c2 <- raster::crop(covar1b, spoly) %>% raster::mask(.,spoly)
      covar2c2 <- raster::crop(covar2b, spoly) %>% raster::mask(.,spoly)
      annual1_sub_covar_v2 <- stack(annual1_sub_v2, covar1c2, covar2c2)
      annual1_sub2_v2 <- annual1_sub_covar_v2 %>% as.data.frame(., xy = TRUE) %>%
        mutate(manage_stratum = spoly$year, interv = spoly$interventi, interv2 = spoly$manageType, estimat_year =pred_year, ras_sub =sub2)
      annual1_sub2_v2 <-  annual1_sub2_v2 %>% dplyr::filter(!is.na(est_agbd))
      # # print('2')
      # annual1_sub_v2 <- raster::crop(annual_ras2, spoly)
      # annual1_sub2_v2 <- raster::mask(annual1_sub_v2, spoly) %>% as.data.frame(., xy = TRUE) %>%
      #   mutate(manage_stratum = spoly$year, interv = spoly$interventi, interv2 = spoly$manageType, estimat_year =pred_year, ras_sub =sub2)
      # annual1_sub2_v2 <-  annual1_sub2_v2 %>% dplyr::filter(!is.na(est_agbd))
    } else{
      annual1_sub2_v2 <- data.frame()
      
    }
    
    sub3 <- '3'
    if (!class(try(intersect(annual_ras3,spoly),T ))=='try-error'){
      annual1_sub_v3 <- raster::crop(annual_ras3, spoly) %>% raster::mask(.,spoly)
      covar1b <- raster::stack(d2road, popcnt, slope, dem, precp, temp) %>% projectRaster(from=., to= annual1_sub_v3, method="bilinear")
      covar2b <- raster::stack(soil) %>% projectRaster(from=., to= annual1_sub_v3, method="bilinear")
      covar1c3 <- raster::crop(covar1b, spoly) %>% raster::mask(.,spoly)
      covar2c3 <- raster::crop(covar2b, spoly) %>% raster::mask(.,spoly)
      annual1_sub_covar_v3 <- stack(annual1_sub_v3, covar1c3, covar2c3)
      annual1_sub2_v3 <- annual1_sub_covar_v3 %>% as.data.frame(., xy = TRUE) %>%
        mutate(manage_stratum = spoly$year, interv = spoly$interventi,  interv2 = spoly$manageType,estimat_year =pred_year, ras_sub =sub3)
      annual1_sub2_v3 <-  annual1_sub2_v3 %>% dplyr::filter(!is.na(est_agbd))
      # annual1_sub_v3 <- raster::crop(annual_ras3, spoly)
      # annual1_sub2_v3 <- raster::mask(annual1_sub_v3, spoly) %>% as.data.frame(., xy = TRUE) %>%
      #   mutate(manage_stratum = spoly$year, interv = spoly$interventi,  interv2 = spoly$manageType,estimat_year =pred_year, ras_sub =sub3)
      # annual1_sub2_v3 <-  annual1_sub2_v3 %>% dplyr::filter(!is.na(est_agbd))
    } else{
      annual1_sub2_v3 <- data.frame()
      
    }
    
    
    
    annual_agbd_by_manage <- rbind(annual_agbd_by_manage, annual1_sub2)
    print(paste('pred year',pred_year,  'poly no.', pl, 'nrow output:',nrow(annual_agbd_by_manage), sep=' '))
    rm(annual1_sub2)
    
    annual_agbd_by_manage <- rbind(annual_agbd_by_manage, annual1_sub2_v2)
    print(paste('pred year',pred_year,  'poly no.', pl, 'nrow output:',nrow(annual_agbd_by_manage), sep=' '))
    rm(annual1_sub2_v2)
    
    annual_agbd_by_manage <- rbind(annual_agbd_by_manage, annual1_sub2_v3)
    print(paste('pred year',pred_year,  'poly no.', pl, 'nrow output:',nrow(annual_agbd_by_manage), sep=' '))
    rm(annual1_sub2_v3)
    
    
  }
  
  
  tile_agbd_by_manage <- rbind(tile_agbd_by_manage,annual_agbd_by_manage)
  
}


# write.csv( tile_agbd_by_manage, paste("agbd_predictions/kibale_ts_out/icc_agbd_by_planting_px",
#                                       pattern,"_ecotrust_covar_ver4.csv", sep=""))
write.csv( tile_agbd_by_manage, paste("agbd_predictions/AMF_sites_df/agbd_by_planting_px",
                                      pattern,"_TISTUGA_AR_covar.csv", sep=""))

end.time <- Sys.time()
time.taken <- end.time - start.time
print(paste('total run time is', time.taken, sep=": "))