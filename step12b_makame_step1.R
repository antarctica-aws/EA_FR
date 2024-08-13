setwd('/gpfs/data1/duncansongp/amberliang/EA_data')
library(tidyr)
library(ggplot2)
library(ggpubr)
library(foreach)
library(doParallel)
library(raster)
library(rgdal)

# poly <- readOGR('AOI/verra_sites/sodo_sites_comb.shp') #wf_new_interv4_chfor.shp')
poly <- readOGR('AOI/verra_sites/makame_CT_redd.geojson')
# poly <- poly[3001:3536,]
#spoly$year, interv = spoly$interventi, interv2 = spoly$manageType
poly$year <- poly$timestamp
poly$interventi <- 'enclosure'
poly$manageType <- 'NR'


pattern <- 'dodoma2+2sig_sen97_beam+sol_nbr'
tile <- '168063'

#just to get the agbd output resolution 
ras_temp <-  raster(paste('agbd_predictions/dodoma2_ts_out/agbd_2021_xgboost_sqrt_nbr_lhs_33_', pattern,'_ls_lhs_medoid_',tile,'_2021_NBR_3by3_dur8_biome1_val_v2-0000002560-0000005120.tif', sep=''))
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
  
  annual_ras1 <- raster(paste('agbd_predictions/dodoma2_ts_out/agbd_', pred_year,'_xgboost_sqrt_nbr_lhs_33_', pattern,'_ls_lhs_medoid_',tile,'_', pred_year,'_NBR_3by3_dur8_biome1_val_v2-0000002560-0000000000.tif', sep=''))
  annual_ras2 <- raster(paste('agbd_predictions/dodoma2_ts_out/agbd_', pred_year,'_xgboost_sqrt_nbr_lhs_33_', pattern,'_ls_lhs_medoid_',tile,'_', pred_year,'_NBR_3by3_dur8_biome1_val_v2-0000002560-0000005120.tif', sep=''))
  annual_ras3 <- raster(paste('agbd_predictions/dodoma2_ts_out/agbd_', pred_year,'_xgboost_sqrt_nbr_lhs_33_', pattern,'_ls_lhs_medoid_',tile,'_', pred_year,'_NBR_3by3_dur8_biome1_val_v2-0000002560-0000002560.tif', sep=''))
  annual_ras4 <- raster(paste('agbd_predictions/dodoma2_ts_out/agbd_', pred_year,'_xgboost_sqrt_nbr_lhs_33_', pattern,'_ls_lhs_medoid_',tile,'_', pred_year,'_NBR_3by3_dur8_biome1_val_v2-0000005120-0000000000.tif', sep=''))

  names(annual_ras1) <- paste('est_agbd', sep='')
  names(annual_ras2) <- paste('est_agbd', sep='')
  names(annual_ras3) <- paste('est_agbd', sep='')
  names(annual_ras4) <- paste('est_agbd', sep='')
  
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
        mutate(manage_stratum = spoly$year, interv = spoly$interventi, interv2 = spoly$manageType, estimat_year =pred_year, ras_sub =sub)
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
    if (!class(try(raster::intersect(annual_ras3,spoly),T ))=='try-error'){
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
    
    sub4 <- '4'
    if (!class(try(raster::intersect(annual_ras4,spoly),T ))=='try-error'){
      annual1_sub_v4 <- raster::crop(annual_ras4, spoly) %>% raster::mask(.,spoly)
      covar1b <- raster::stack(d2road, popcnt, slope, dem, precp, temp) %>% projectRaster(from=., to= annual1_sub_v4, method="bilinear")
      covar2b <- raster::stack(soil) %>% projectRaster(from=., to= annual1_sub_v4, method="bilinear")
      covar1c4 <- raster::crop(covar1b, spoly) %>% raster::mask(.,spoly)
      covar2c4 <- raster::crop(covar2b, spoly) %>% raster::mask(.,spoly)
      annual1_sub_covar_v4 <- stack(annual1_sub_v4, covar1c4, covar2c4)
      annual1_sub2_v4 <- annual1_sub_covar_v4 %>% as.data.frame(., xy = TRUE) %>%
        mutate(manage_stratum = spoly$year, interv = spoly$interventi,  interv2 = spoly$manageType,estimat_year =pred_year, ras_sub =sub4)
      annual1_sub2_v4 <-  annual1_sub2_v4 %>% dplyr::filter(!is.na(est_agbd))
    } else{
      annual1_sub2_v4 <- data.frame()
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
                                      pattern,"_tsgss_makame_NR_covar.csv", sep=""))


