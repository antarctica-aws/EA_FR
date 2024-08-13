setwd('/gpfs/data1/duncansongp/amberliang/EA_data')
library(tidyr)
library(ggplot2)
library(ggpubr)

library(raster)
library(rgdal)

# poly <- readOGR('AOI/verra_sites/sodo_sites_comb.shp') #wf_new_interv4_chfor.shp')
poly <- readOGR('AOI/AMF_sites/tigray_combined_NR.geojson')
poly$name <- 'tigray_NR'
poly$year <- '1990'
pattern <- 'tigray+1sig_sen96_sol_no5_nbr'
tile <- '169051'


ras_temp <-  raster(paste('agbd_predictions/tigray_ts_out/agbd_2021_xgboost_sqrt_nbr_lhs_33_', pattern,'_ls_lhs_medoid_',tile,'_2021_NBR_3by3_dur8_biome1_val_v2-0000000000-0000005120.tif', sep=''))
fact <-  raster::res(raster('covariates/ETH/d2roads.tif'))[1]/raster::res(ras_temp)[1]

##adding in the covaiarte raster, and ### 
d2road <- raster('covariates/ETH/d2roads.tif')%>% raster::disaggregate(.,fact=fact)
popcnt <- raster('covariates/ETH/pop_cnt_2000.tif') %>% raster::disaggregate(.,fact=fact)
slope <- raster('covariates/ETH/slope.tif')%>% raster::disaggregate(.,fact=fact)
dem <- raster('covariates/ETH/dem.tif')%>% raster::disaggregate(.,fact=fact)
precp <- raster('covariates/ETH/wc_prec_1990-1999.tif')%>% raster::disaggregate(.,fact=fact)
temp <- raster('covariates/ETH/wc_tavg_1990-1999.tif')%>% raster::disaggregate(.,fact=fact)
soil <- raster('covariates/ETH/soilType.tif') %>% raster::disaggregate(.,fact=fact)

start.time <- Sys.time()

tile_agbd_by_manage <- data.frame()

for (pred_year in 2021:1986){
  print(pred_year)
  # pred_year <- as.character(pred_year)
  
  annual_ras1 <- raster(paste('agbd_predictions/tigray_ts_out/agbd_', pred_year,'_xgboost_sqrt_nbr_lhs_33_', pattern,'_ls_lhs_medoid_',tile,'_', pred_year,'_NBR_3by3_dur8_biome1_val_v2-0000002560-0000005120.tif', sep=''))
  annual_ras2 <- raster(paste('agbd_predictions/tigray_ts_out/agbd_', pred_year,'_xgboost_sqrt_nbr_lhs_33_', pattern,'_ls_lhs_medoid_',tile,'_', pred_year,'_NBR_3by3_dur8_biome1_val_v2-0000000000-0000005120.tif', sep=''))
  
  names(annual_ras1) <- paste('est_agbd', sep='')
  names(annual_ras2) <- paste('est_agbd',  sep='')
  
  annual_agbd_by_manage <- data.frame()
  for (pl in 1:length(poly)){
    # print('ploy out of 47')
    # print(pl)
    spoly <- poly[pl,]
    sub <- '1'
    if (!class(try(intersect(annual_ras1,spoly),T ))=='try-error'){
      # print('1')
      annual1_sub <- raster::crop(annual_ras1, spoly) %>% raster::mask(.,spoly)
      covar1 <- raster::stack(d2road, popcnt, slope, dem, precp, temp) %>% projectRaster(from=., to=annual1_sub, method="bilinear")
      covar2 <- raster::stack(soil) %>% projectRaster(from=., to=annual1_sub, method="bilinear")
      covar1c <- raster::crop(covar1, spoly) %>% raster::mask(.,spoly)
      covar2c <- raster::crop(covar2, spoly) %>% raster::mask(.,spoly)
      annual1_sub_covar <- stack(annual1_sub, covar1c, covar2c)
      annual1_sub2 <- annual1_sub_covar %>% as.data.frame(., xy = TRUE) %>%
        mutate(manage_stratum = spoly$year, interv = spoly$Name, estimat_year =pred_year, ras_sub =sub)
      annual1_sub2 <-  annual1_sub2 %>% dplyr::filter(!is.na(est_agbd))
      # annual1_sub <- raster::crop(annual_ras1, spoly)
      # annual1_sub2 <- raster::mask(annual1_sub,spoly) %>% as.data.frame(., xy = TRUE) %>%
      #   mutate(manage_stratum = spoly$year, interv = spoly$interventi, estimat_year =pred_year, ras_sub =sub)
      # annual1_sub2 <-  annual1_sub2 %>% dplyr::filter(!is.na(est_agbd))
    }else{
      annual1_sub2 <- data.frame()
    }
    
    sub2 <- '2'
    if (!class(try(intersect(annual_ras2,spoly),T ))=='try-error'){
      # print('2')
      annual1_sub_v2 <- raster::crop(annual_ras2, spoly) %>% raster::mask(.,spoly)
      covar1b <- raster::stack(d2road, popcnt, slope, dem, precp, temp) %>% projectRaster(from=., to= annual1_sub_v2, method="bilinear")
      covar2b <- raster::stack(soil) %>% projectRaster(from=., to= annual1_sub_v2, method="bilinear")
      covar1c2 <- raster::crop(covar1b, spoly) %>% raster::mask(.,spoly)
      covar2c2 <- raster::crop(covar2b, spoly) %>% raster::mask(.,spoly)
      annual1_sub_covar_v2 <- stack(annual1_sub_v2, covar1c2, covar2c2)
      annual1_sub2_v2 <- annual1_sub_covar_v2 %>% as.data.frame(., xy = TRUE) %>%
        mutate(manage_stratum = spoly$year, interv = spoly$Name, estimat_year =pred_year, ras_sub =sub2)
      annual1_sub2_v2 <-  annual1_sub2_v2 %>% dplyr::filter(!is.na(est_agbd))
      # annual1_sub_v2 <- raster::crop(annual_ras2, spoly)
      # annual1_sub2_v2 <- raster::mask(annual1_sub_v2, spoly) %>% as.data.frame(., xy = TRUE) %>%
      #   mutate(manage_stratum = spoly$year, interv = spoly$interventi, estimat_year =pred_year, ras_sub =sub2)
      # annual1_sub2_v2 <-  annual1_sub2_v2 %>% dplyr::filter(!is.na(est_agbd))
    } else{
      annual1_sub2_v2 <- data.frame()
      
    }
    
    annual_agbd_by_manage <- rbind(annual_agbd_by_manage, annual1_sub2)
    print(nrow(annual_agbd_by_manage))
    rm(annual1_sub2)
    
    annual_agbd_by_manage <- rbind(annual_agbd_by_manage, annual1_sub2_v2)
    print(nrow(annual_agbd_by_manage))
    rm(annual1_sub2_v2)
    
  }
  
  tile_agbd_by_manage <- rbind(tile_agbd_by_manage,annual_agbd_by_manage)
  
}
# 
# write.csv( tile_agbd_by_manage, paste("agbd_predictions/sodo_ts_out/sodo_agbd_collrm_by_planting_px",
#                                       pattern,"_covar.csv", sep=""))

write.csv( tile_agbd_by_manage, paste("agbd_predictions/AMF_sites_df/tigray_mf_agbd_collrm_by_planting_px",
                                      pattern,"_covar_v2.csv", sep=""))

end.time <- Sys.time()
time.taken <- end.time - start.time
print(paste('total run time is', time.taken, sep=": "))