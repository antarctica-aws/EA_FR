setwd('/gpfs/data1/duncansongp/amberliang/EA_data')
library(tidyr)
library(ggplot2)
library(ggpubr)
library(foreach)
library(doParallel)
library(raster)
library(rgdal)

# poly <- readOGR('AOI/verra_sites/sodo_sites_comb.shp') #wf_new_interv4_chfor.shp')
poly <- readOGR('AOI/Dodoma/dodoma_fmnr_plotcoords_300mbuff_rprj.geojson')
# poly <- poly[1,]
#spoly$year, interv = spoly$interventi, interv2 = spoly$manageType
poly$year <- poly$timestamp
poly$interventi <- 'FMNR'
poly$manageType <- 'ANR'
poly$year <- poly$FMNR_starting_year


pattern <- 'dodoma2+2sig_sen97_beam+sol_nbr'
tile <- '168064'
tile2 <- '168063'

#just to get the agbd output resolution 
ras_temp <-  raster(paste('agbd_predictions/dodoma1_ts_out/agbd_2021_xgboost_sqrt_nbr_lhs_33_', pattern,'_ls_lhs_medoid_',tile,'_2021_NBR_3by3_dur8_biome1_val_v2-0000002560-0000005120.tif', sep=''))
fact <-  raster::res(raster('covariates/TZA/d2roads.tif'))[1]/raster::res(ras_temp)[1]

##adding in the covaiarte raster, and ### 
d2road <- raster('covariates/TZA/d2roads.tif')%>% raster::disaggregate(.,fact=fact)
popcnt <- raster('covariates/TZA/pop_cnt_2000.tif') %>% raster::disaggregate(.,fact=fact)
slope <- raster('covariates/TZA/slope.tif')%>% raster::disaggregate(.,fact=fact)
dem <- raster('covariates/TZA/dem.tif')%>% raster::disaggregate(.,fact=fact)
precp <- raster('covariates/TZA/wc_prec_1990-1999.tif')%>% raster::disaggregate(.,fact=fact)
temp <- raster('covariates/TZA/wc_tavg_1990-1999.tif')%>% raster::disaggregate(.,fact=fact)
soil <- raster('covariates/TZA/soilType.tif') %>% raster::disaggregate(.,fact=fact)

start.time <- Sys.time()
registerDoParallel(12)
tile_agbd_by_manage <- foreach(pred_year=2021:1986, .combine = rbind, .packages=c('sp','magrittr', 'dplyr','tidyr','raster')) %dopar% {   
  tile_agbd_by_manage <- data.frame()
  print(pred_year)
  
  annual_ras1 <- raster(paste('agbd_predictions/dodoma1_ts_out/agbd_', pred_year,'_xgboost_sqrt_nbr_lhs_33_', pattern,'_ls_lhs_medoid_',tile,'_', pred_year,'_NBR_3by3_dur8_biome1_val_v2-0000000000-0000000000.tif', sep=''))
  annual_ras2 <- raster(paste('agbd_predictions/dodoma1_ts_out/agbd_', pred_year,'_xgboost_sqrt_nbr_lhs_33_', pattern,'_ls_lhs_medoid_',tile,'_', pred_year,'_NBR_3by3_dur8_biome1_val_v2-0000000000-0000002560.tif', sep=''))
  annual_ras3 <- raster(paste('agbd_predictions/dodoma1_ts_out/agbd_', pred_year,'_xgboost_sqrt_nbr_lhs_33_', pattern,'_ls_lhs_medoid_',tile,'_', pred_year,'_NBR_3by3_dur8_biome1_val_v2-0000002560-0000002560.tif', sep=''))
  annual_ras4 <- raster(paste('agbd_predictions/dodoma1_ts_out/agbd_', pred_year,'_xgboost_sqrt_nbr_lhs_33_', pattern,'_ls_lhs_medoid_',tile,'_', pred_year,'_NBR_3by3_dur8_biome1_val_v2-0000002560-0000000000.tif', sep=''))
  annual_ras5 <- raster(paste('agbd_predictions/dodoma1_ts_out/agbd_', pred_year,'_xgboost_sqrt_nbr_lhs_33_', pattern,'_ls_lhs_medoid_',tile,'_', pred_year,'_NBR_3by3_dur8_biome1_val_v2-0000002560-0000005120.tif', sep=''))
  annual_ras6 <- raster(paste('agbd_predictions/dodoma1_ts_out/agbd_', pred_year,'_xgboost_sqrt_nbr_lhs_33_', pattern,'_ls_lhs_medoid_',tile,'_', pred_year,'_NBR_3by3_dur8_biome1_val_v2-0000005120-0000005120.tif', sep=''))
  annual_ras7 <- raster(paste('agbd_predictions/dodoma1_ts_out/agbd_', pred_year,'_xgboost_sqrt_nbr_lhs_33_', pattern,'_ls_lhs_medoid_',tile,'_', pred_year,'_NBR_3by3_dur8_biome1_val_v2-0000005120-0000002560.tif', sep=''))
  annual_ras8 <- raster(paste('agbd_predictions/dodoma1_ts_out/agbd_', pred_year,'_xgboost_sqrt_nbr_lhs_33_', pattern,'_ls_lhs_medoid_',tile,'_', pred_year,'_NBR_3by3_dur8_biome1_val_v2-0000005120-0000000000.tif', sep=''))
  
  annual_ras9 <- raster(paste('agbd_predictions/dodoma2_ts_out/agbd_', pred_year,'_xgboost_sqrt_nbr_lhs_33_', pattern,'_ls_lhs_medoid_',tile2,'_', pred_year,'_NBR_3by3_dur8_biome1_val_v2-0000002560-0000000000.tif', sep=''))
  annual_ras10 <- raster(paste('agbd_predictions/dodoma2_ts_out/agbd_', pred_year,'_xgboost_sqrt_nbr_lhs_33_', pattern,'_ls_lhs_medoid_',tile2,'_', pred_year,'_NBR_3by3_dur8_biome1_val_v2-0000002560-0000002560.tif', sep=''))
  annual_ras11 <- raster(paste('agbd_predictions/dodoma2_ts_out/agbd_', pred_year,'_xgboost_sqrt_nbr_lhs_33_', pattern,'_ls_lhs_medoid_',tile2,'_', pred_year,'_NBR_3by3_dur8_biome1_val_v2-0000005120-0000002560.tif', sep=''))
  annual_ras12 <- raster(paste('agbd_predictions/dodoma2_ts_out/agbd_', pred_year,'_xgboost_sqrt_nbr_lhs_33_', pattern,'_ls_lhs_medoid_',tile2,'_', pred_year,'_NBR_3by3_dur8_biome1_val_v2-0000005120-0000000000.tif', sep=''))
    
  names(annual_ras1) <- paste('est_agbd', sep='')
  names(annual_ras2) <- paste('est_agbd', sep='')
  names(annual_ras3) <- paste('est_agbd', sep='')
  names(annual_ras4) <- paste('est_agbd', sep='')
  names(annual_ras5) <- paste('est_agbd', sep='')
  names(annual_ras6) <- paste('est_agbd', sep='')
  names(annual_ras7) <- paste('est_agbd', sep='')
  names(annual_ras8) <- paste('est_agbd', sep='')
  names(annual_ras9) <- paste('est_agbd', sep='')
  names(annual_ras10) <- paste('est_agbd', sep='')
  names(annual_ras11) <- paste('est_agbd', sep='')
  names(annual_ras12) <- paste('est_agbd', sep='')
  
  registerDoParallel(12)
  annual_agbd_by_manage <- foreach(pl=1:length(poly), .combine = rbind, .packages=c('sp','magrittr', 'dplyr','tidyr','raster')) %dopar% {
    # print(paste(pl, pred_year,sep=" "))
    annual_agbd_by_manage <- data.frame()  
    
    spoly <- poly[pl,]
    sub <- '1'
    if (!class(try(raster::intersect(annual_ras1,spoly),T ))=='try-error'){
      annual1_sub <- raster::crop(annual_ras1, spoly) %>% raster::mask(.,spoly)
      covar1 <- raster::stack(d2road, popcnt, slope, dem, precp, temp) %>% projectRaster(from=., to=annual1_sub, method="bilinear")
      covar2 <- raster::stack(soil) %>% projectRaster(from=., to=annual1_sub, method="bilinear")
      covar1c <- raster::crop(covar1, spoly) %>% raster::mask(.,spoly)
      covar2c <- raster::crop(covar2, spoly) %>% raster::mask(.,spoly)
      annual1_sub_covar <- stack(annual1_sub, covar1c, covar2c)
      annual1_sub2 <- annual1_sub_covar %>% as.data.frame(., xy = TRUE) %>%
        mutate(manage_stratum = spoly$year, interv = spoly$interventi, interv2 = spoly$manageType, 
               estimat_year =pred_year, ras_sub =sub, poly_id=spoly$id)
      annual1_sub2 <-  annual1_sub2 %>% dplyr::filter(!is.na(est_agbd))
    }else{
      annual1_sub2 <- data.frame()
    }
    
    sub2 <- '2'
    if (!class(try(raster::intersect(annual_ras2,spoly),T ))=='try-error'){
      annual1_sub_v2 <- raster::crop(annual_ras2, spoly) %>% raster::mask(.,spoly)
      covar1b <- raster::stack(d2road, popcnt, slope, dem, precp, temp) %>% projectRaster(from=., to= annual1_sub_v2, method="bilinear")
      covar2b <- raster::stack(soil) %>% projectRaster(from=., to= annual1_sub_v2, method="bilinear")
      covar1c2 <- raster::crop(covar1b, spoly) %>% raster::mask(.,spoly)
      covar2c2 <- raster::crop(covar2b, spoly) %>% raster::mask(.,spoly)
      annual1_sub_covar_v2 <- stack(annual1_sub_v2, covar1c2, covar2c2)
      annual1_sub2_v2 <- annual1_sub_covar_v2 %>% as.data.frame(., xy = TRUE) %>%
        mutate(manage_stratum = spoly$year, interv = spoly$interventi, interv2 = spoly$manageType, 
               estimat_year =pred_year, ras_sub =sub2, poly_id=spoly$id)
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
    if (!class(try(raster::intersect(annual_ras3,spoly),T ))=='try-error'){
      annual1_sub_v3 <- raster::crop(annual_ras3, spoly) %>% raster::mask(.,spoly)
      covar1b <- raster::stack(d2road, popcnt, slope, dem, precp, temp) %>% projectRaster(from=., to= annual1_sub_v3, method="bilinear")
      covar2b <- raster::stack(soil) %>% projectRaster(from=., to= annual1_sub_v3, method="bilinear")
      covar1c3 <- raster::crop(covar1b, spoly) %>% raster::mask(.,spoly)
      covar2c3 <- raster::crop(covar2b, spoly) %>% raster::mask(.,spoly)
      annual1_sub_covar_v3 <- stack(annual1_sub_v3, covar1c3, covar2c3)
      annual1_sub2_v3 <- annual1_sub_covar_v3 %>% as.data.frame(., xy = TRUE) %>%
        mutate(manage_stratum = spoly$year, interv = spoly$interventi,  interv2 = spoly$manageType,
               estimat_year =pred_year, ras_sub =sub3, poly_id=spoly$id)
      annual1_sub2_v3 <-  annual1_sub2_v3 %>% dplyr::filter(!is.na(est_agbd))
      # annual1_sub_v3 <- raster::crop(annual_ras3, spoly)
      # annual1_sub2_v3 <- raster::mask(annual1_sub_v3, spoly) %>% as.data.frame(., xy = TRUE) %>%
      #   mutate(manage_stratum = spoly$year, interv = spoly$interventi,  interv2 = spoly$manageType,estimat_year =pred_year, ras_sub =sub3)
      # annual1_sub2_v3 <-  annual1_sub2_v3 %>% dplyr::filter(!is.na(est_agbd))
    } else{
      annual1_sub2_v3 <- data.frame()
    }
    
    sub4 <- '4'
    if (!class(try(raster::intersect(annual_ras4,spoly),T ))=='try-error'){
      annual1_sub_v4 <- raster::crop(annual_ras4, spoly) %>% raster::mask(.,spoly)
      covar1b <- raster::stack(d2road, popcnt, slope, dem, precp, temp) %>% projectRaster(from=., to= annual1_sub_v4, method="bilinear")
      covar2b <- raster::stack(soil) %>% projectRaster(from=., to= annual1_sub_v4, method="bilinear")
      covar1c4 <- raster::crop(covar1b, spoly) %>% raster::mask(.,spoly)
      covar2c4 <- raster::crop(covar2b, spoly) %>% raster::mask(.,spoly)
      annual1_sub_covar_v4 <- stack(annual1_sub_v4, covar1c4, covar2c4)
      annual1_sub2_v4 <- annual1_sub_covar_v4 %>% as.data.frame(., xy = TRUE) %>%
        mutate(manage_stratum = spoly$year, interv = spoly$interventi,  interv2 = spoly$manageType,
               estimat_year =pred_year, ras_sub =sub4, poly_id=spoly$id)
      annual1_sub2_v4 <-  annual1_sub2_v4 %>% dplyr::filter(!is.na(est_agbd))
    } else{
      annual1_sub2_v4 <- data.frame()
    }
    
    sub5 <- '5'
    if (!class(try(raster::intersect(annual_ras5,spoly),T ))=='try-error'){
      annual1_sub_v5 <- raster::crop(annual_ras5, spoly) %>% raster::mask(.,spoly)
      covar1b <- raster::stack(d2road, popcnt, slope, dem, precp, temp) %>% projectRaster(from=., to= annual1_sub_v5, method="bilinear")
      covar2b <- raster::stack(soil) %>% projectRaster(from=., to= annual1_sub_v5, method="bilinear")
      covar1c5 <- raster::crop(covar1b, spoly) %>% raster::mask(.,spoly)
      covar2c5 <- raster::crop(covar2b, spoly) %>% raster::mask(.,spoly)
      annual1_sub_covar_v5 <- stack(annual1_sub_v5, covar1c5, covar2c5)
      annual1_sub2_v5 <- annual1_sub_covar_v5 %>% as.data.frame(., xy = TRUE) %>%
        mutate(manage_stratum = spoly$year, interv = spoly$interventi,  interv2 = spoly$manageType,
               estimat_year =pred_year, ras_sub =sub5, poly_id=spoly$id)
      annual1_sub2_v5 <-  annual1_sub2_v5 %>% dplyr::filter(!is.na(est_agbd))
    } else{
      annual1_sub2_v5 <- data.frame()
    }
    
    sub6 <- '6'
    if (!class(try(raster::intersect(annual_ras6,spoly),T ))=='try-error'){
      annual1_sub_v6 <- raster::crop(annual_ras6, spoly) %>% raster::mask(.,spoly)
      covar1b <- raster::stack(d2road, popcnt, slope, dem, precp, temp) %>% projectRaster(from=., to= annual1_sub_v6, method="bilinear")
      covar2b <- raster::stack(soil) %>% projectRaster(from=., to= annual1_sub_v6, method="bilinear")
      covar1c6 <- raster::crop(covar1b, spoly) %>% raster::mask(.,spoly)
      covar2c6 <- raster::crop(covar2b, spoly) %>% raster::mask(.,spoly)
      annual1_sub_covar_v6 <- stack(annual1_sub_v6, covar1c6, covar2c6)
      annual1_sub2_v6 <- annual1_sub_covar_v6 %>% as.data.frame(., xy = TRUE) %>%
        mutate(manage_stratum = spoly$year, interv = spoly$interventi,  interv2 = spoly$manageType,
               estimat_year =pred_year, ras_sub =sub6, poly_id=spoly$id)
      annual1_sub2_v6 <-  annual1_sub2_v6 %>% dplyr::filter(!is.na(est_agbd))
    } else{
      annual1_sub2_v6 <- data.frame()
    }
    
    sub7 <- '7'
    if (!class(try(raster::intersect(annual_ras7,spoly),T ))=='try-error'){
      annual1_sub_v7 <- raster::crop(annual_ras7, spoly) %>% raster::mask(.,spoly)
      covar1b <- raster::stack(d2road, popcnt, slope, dem, precp, temp) %>% projectRaster(from=., to= annual1_sub_v7, method="bilinear")
      covar2b <- raster::stack(soil) %>% projectRaster(from=., to= annual1_sub_v7, method="bilinear")
      covar1c7 <- raster::crop(covar1b, spoly) %>% raster::mask(.,spoly)
      covar2c7 <- raster::crop(covar2b, spoly) %>% raster::mask(.,spoly)
      annual1_sub_covar_v7 <- stack(annual1_sub_v7, covar1c7, covar2c7)
      annual1_sub2_v7 <- annual1_sub_covar_v7 %>% as.data.frame(., xy = TRUE) %>%
        mutate(manage_stratum = spoly$year, interv = spoly$interventi,  interv2 = spoly$manageType,
               estimat_year =pred_year, ras_sub =sub7, poly_id=spoly$id)
      annual1_sub2_v7 <-  annual1_sub2_v7 %>% dplyr::filter(!is.na(est_agbd))
    } else{
      annual1_sub2_v7 <- data.frame()
    }
    
    sub8 <- '8'
    if (!class(try(raster::intersect(annual_ras8,spoly),T ))=='try-error'){
      annual1_sub_v8 <- raster::crop(annual_ras8, spoly) %>% raster::mask(.,spoly)
      covar1b <- raster::stack(d2road, popcnt, slope, dem, precp, temp) %>% projectRaster(from=., to= annual1_sub_v8, method="bilinear")
      covar2b <- raster::stack(soil) %>% projectRaster(from=., to= annual1_sub_v8, method="bilinear")
      covar1c8 <- raster::crop(covar1b, spoly) %>% raster::mask(.,spoly)
      covar2c8 <- raster::crop(covar2b, spoly) %>% raster::mask(.,spoly)
      annual1_sub_covar_v8 <- stack(annual1_sub_v8, covar1c8, covar2c8)
      annual1_sub2_v8 <- annual1_sub_covar_v8 %>% as.data.frame(., xy = TRUE) %>%
        mutate(manage_stratum = spoly$year, interv = spoly$interventi,  interv2 = spoly$manageType,
               estimat_year =pred_year, ras_sub =sub8, poly_id=spoly$id)
      annual1_sub2_v8 <-  annual1_sub2_v8 %>% dplyr::filter(!is.na(est_agbd))
    } else{
      annual1_sub2_v8 <- data.frame()
    }
    
    sub9 <- '9'
    if (!class(try(raster::intersect(annual_ras9,spoly),T ))=='try-error'){
      annual1_sub_v9 <- raster::crop(annual_ras9, spoly) %>% raster::mask(.,spoly)
      covar1b <- raster::stack(d2road, popcnt, slope, dem, precp, temp) %>% projectRaster(from=., to= annual1_sub_v9, method="bilinear")
      covar2b <- raster::stack(soil) %>% projectRaster(from=., to= annual1_sub_v9, method="bilinear")
      covar1c9 <- raster::crop(covar1b, spoly) %>% raster::mask(.,spoly)
      covar2c9 <- raster::crop(covar2b, spoly) %>% raster::mask(.,spoly)
      annual1_sub_covar_v9 <- stack(annual1_sub_v9, covar1c9, covar2c9)
      annual1_sub2_v9 <- annual1_sub_covar_v9 %>% as.data.frame(., xy = TRUE) %>%
        mutate(manage_stratum = spoly$year, interv = spoly$interventi,  interv2 = spoly$manageType,
               estimat_year =pred_year, ras_sub =sub9, poly_id=spoly$id)
      annual1_sub2_v9 <-  annual1_sub2_v9 %>% dplyr::filter(!is.na(est_agbd))
    } else{
      annual1_sub2_v9 <- data.frame()
    }
    
    sub10 <- '10'
    if (!class(try(raster::intersect(annual_ras10,spoly),T ))=='try-error'){
      annual1_sub_v10 <- raster::crop(annual_ras10, spoly) %>% raster::mask(.,spoly)
      covar1b <- raster::stack(d2road, popcnt, slope, dem, precp, temp) %>% projectRaster(from=., to= annual1_sub_v10, method="bilinear")
      covar2b <- raster::stack(soil) %>% projectRaster(from=., to= annual1_sub_v10, method="bilinear")
      covar1c10 <- raster::crop(covar1b, spoly) %>% raster::mask(.,spoly)
      covar2c10 <- raster::crop(covar2b, spoly) %>% raster::mask(.,spoly)
      annual1_sub_covar_v10 <- stack(annual1_sub_v10, covar1c10, covar2c10)
      annual1_sub2_v10 <- annual1_sub_covar_v10 %>% as.data.frame(., xy = TRUE) %>%
        mutate(manage_stratum = spoly$year, interv = spoly$interventi,  interv2 = spoly$manageType,
               estimat_year =pred_year, ras_sub =sub10, poly_id=spoly$id)
      annual1_sub2_v10 <-  annual1_sub2_v10 %>% dplyr::filter(!is.na(est_agbd))
    } else{
      annual1_sub2_v10 <- data.frame()
    }
    
    sub11 <- '11'
    if (!class(try(raster::intersect(annual_ras11,spoly),T ))=='try-error'){
      annual1_sub_v11 <- raster::crop(annual_ras11, spoly) %>% raster::mask(.,spoly)
      covar1b <- raster::stack(d2road, popcnt, slope, dem, precp, temp) %>% projectRaster(from=., to= annual1_sub_v11, method="bilinear")
      covar2b <- raster::stack(soil) %>% projectRaster(from=., to= annual1_sub_v11, method="bilinear")
      covar1c11 <- raster::crop(covar1b, spoly) %>% raster::mask(.,spoly)
      covar2c11 <- raster::crop(covar2b, spoly) %>% raster::mask(.,spoly)
      annual1_sub_covar_v11 <- stack(annual1_sub_v11, covar1c11, covar2c11)
      annual1_sub2_v11 <- annual1_sub_covar_v11 %>% as.data.frame(., xy = TRUE) %>%
        mutate(manage_stratum = spoly$year, interv = spoly$interventi,  interv2 = spoly$manageType,
               estimat_year =pred_year, ras_sub =sub11, poly_id=spoly$id)
      annual1_sub2_v11 <-  annual1_sub2_v11 %>% dplyr::filter(!is.na(est_agbd))
    } else{
      annual1_sub2_v11 <- data.frame()
    }
    
    sub12 <- '12'
    if (!class(try(raster::intersect(annual_ras12,spoly),T ))=='try-error'){
      annual1_sub_v12 <- raster::crop(annual_ras12, spoly) %>% raster::mask(.,spoly)
      covar1b <- raster::stack(d2road, popcnt, slope, dem, precp, temp) %>% projectRaster(from=., to= annual1_sub_v12, method="bilinear")
      covar2b <- raster::stack(soil) %>% projectRaster(from=., to= annual1_sub_v12, method="bilinear")
      covar1c12 <- raster::crop(covar1b, spoly) %>% raster::mask(.,spoly)
      covar2c12 <- raster::crop(covar2b, spoly) %>% raster::mask(.,spoly)
      annual1_sub_covar_v12 <- stack(annual1_sub_v12, covar1c12, covar2c12)
      annual1_sub2_v12 <- annual1_sub_covar_v12 %>% as.data.frame(., xy = TRUE) %>%
        mutate(manage_stratum = spoly$year, interv = spoly$interventi,  interv2 = spoly$manageType,
               estimat_year =pred_year, ras_sub =sub12,  poly_id=spoly$id)
      annual1_sub2_v12 <-  annual1_sub2_v12 %>% dplyr::filter(!is.na(est_agbd))
    } else{
      annual1_sub2_v12 <- data.frame()
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
    
    annual_agbd_by_manage <- rbind(annual_agbd_by_manage, annual1_sub2_v4)
    print(paste('pred year',pred_year,  'poly no.', pl, 'nrow output:',nrow(annual_agbd_by_manage), sep=' '))
    rm(annual1_sub2_v4)
    
    annual_agbd_by_manage <- rbind(annual_agbd_by_manage, annual1_sub2_v5)
    print(paste('pred year',pred_year,  'poly no.', pl, 'nrow output:',nrow(annual_agbd_by_manage), sep=' '))
    rm(annual1_sub2_v5)
    
    annual_agbd_by_manage <- rbind(annual_agbd_by_manage, annual1_sub2_v6)
    print(paste('pred year',pred_year,  'poly no.', pl, 'nrow output:',nrow(annual_agbd_by_manage), sep=' '))
    rm(annual1_sub2_v6)
    
    annual_agbd_by_manage <- rbind(annual_agbd_by_manage, annual1_sub2_v7)
    print(paste('pred year',pred_year,  'poly no.', pl, 'nrow output:',nrow(annual_agbd_by_manage), sep=' '))
    rm(annual1_sub2_v7)
    
    annual_agbd_by_manage <- rbind(annual_agbd_by_manage, annual1_sub2_v8)
    print(paste('pred year',pred_year,  'poly no.', pl, 'nrow output:',nrow(annual_agbd_by_manage), sep=' '))
    rm(annual1_sub2_v8)
    
    annual_agbd_by_manage <- rbind(annual_agbd_by_manage, annual1_sub2_v9)
    print(paste('pred year',pred_year,  'poly no.', pl, 'nrow output:',nrow(annual_agbd_by_manage), sep=' '))
    rm(annual1_sub2_v9)
    
    annual_agbd_by_manage <- rbind(annual_agbd_by_manage, annual1_sub2_v10)
    print(paste('pred year',pred_year,  'poly no.', pl, 'nrow output:',nrow(annual_agbd_by_manage), sep=' '))
    rm(annual1_sub2_v10)
    
    annual_agbd_by_manage <- rbind(annual_agbd_by_manage, annual1_sub2_v11)
    print(paste('pred year',pred_year,  'poly no.', pl, 'nrow output:',nrow(annual_agbd_by_manage), sep=' '))
    rm(annual1_sub2_v11)
    
    annual_agbd_by_manage <- rbind(annual_agbd_by_manage, annual1_sub2_v12)
    print(paste('pred year',pred_year,  'poly no.', pl, 'nrow output:',nrow(annual_agbd_by_manage), sep=' '))
    rm(annual1_sub2_v12)
    
    
    return(annual_agbd_by_manage)
    
  }
  stopImplicitCluster()  
  
  
  tile_agbd_by_manage <- rbind(tile_agbd_by_manage,annual_agbd_by_manage)
  
  
  return(tile_agbd_by_manage)
}
stopImplicitCluster()

end.time <- Sys.time()
time.taken <- end.time - start.time
print(paste('total run time is', time.taken, sep=": "))


write.csv( tile_agbd_by_manage, paste("agbd_predictions/dodoma1_ts_out/agbd_collrm_by_planting_px",
                                      pattern,"_tsgss_dodomaPlots_ANR_covar.csv", sep=""))


