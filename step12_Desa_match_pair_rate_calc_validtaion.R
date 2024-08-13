
#step12_desa_match_pair_validation

setwd('/gpfs/data1/duncansongp/amberliang/EA_data')
library(tidyr)
library(ggplot2)
library(ggpubr)
library(terra)
library(raster)
library(sp)

pattern <- 'desa2+1sig_sen98_sol_v3_rfr7_thre_tuned_dcol'
icc <- 'icc_'
b_year <- '2021'

agbdOut <- read.csv('agbd_predictions/desa_ts_out/desa2+alg2_1sig_sen98_sol_thre_tuned_dcol_localhd5t.csv')  ##good single year bias on flux
agbdOutSp <-agbdOut %>% SpatialPointsDataFrame(coords =.[, c("coords.x1",  "coords.x2" )], data =.,
                                               proj4string = CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 '))

agbdOutSp$pred_agbd_v2 <- NA

print(b_year)
ras1 <- raster(paste('agbd_predictions/desa_ts_out/out/',icc,'agbd_',b_year,'_xgboost_sqrt_nbr_lhs_33_',pattern,'_ls_lhs_medoid_168051_',b_year,'_NBR_3by3_dur8_biome1_val_v2-0000000000-0000000000.tif', sep=''))
ras2 <- raster(paste('agbd_predictions/desa_ts_out/out/',icc,'agbd_',b_year,'_xgboost_sqrt_nbr_lhs_33_',pattern,'_ls_lhs_medoid_168050_',b_year,'_NBR_3by3_dur8_biome1_val_v2-0000005120-0000000000.tif', sep=''))



#aggregate from 40x40 resolution to 120x120 (factor = 3)
res <-  200
ras1.aggregate <- aggregate(ras1, fact=res/30)
ras2.aggregate <- aggregate(ras2, fact=res/30)


res(ras1.aggregate)

# convert raster to 90m grid
# getGridTopology(as(ras1.aggregate, "SpatialGrid"))

grid1 <-   rasterToPolygons(ras1.aggregate)
grid2 <-   rasterToPolygons(ras2.aggregate)
names(grid1) <- 'AGBD2021_168051_90m'
names(grid2) <- 'AGBD2021_168050_90m'

# extract from ras 1 and ras 2 the plot agbd
myr <- unique(agbdOut$measure_year)
#use the original filed plot data to extract 

alluniPlot <- data.frame()

for (y in myr){
  print(y)
  
  fplot <- read.csv(paste('/gpfs/data1/duncansongp/amberliang/EA_data/validation/wf_desa/desa_plotsplot_level_agb_localhd5T_anci_year_',y,'_v3.csv', sep=''))
  plotSp <- fplot %>% SpatialPointsDataFrame(coords =.[, c("coords.x1",  "coords.x2" )], data =.,
                                             proj4string = CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 '))
  plotSp$pred_agbd_v2 <- NA
  
  sub1 <-  plotSp %>% raster::crop(., ras1.aggregate)
  agbd1 <-  raster::extract(ras1.aggregate, sub1)
  plotSp$pred_agbd_v2[plotSp$Plot_ID %in% sub1$Plot_ID]<-  agbd1
  
  sub2 <- plotSp[is.na(plotSp$pred_agbd_v2),] #%>% raster::crop(., ras2.aggregate)
  agbd2 <-  raster::extract(ras2.aggregate, sub2)
  plotSp$pred_agbd_v2[is.na(plotSp$pred_agbd_v2)] <- agbd2
  
  alluniPlot <-  rbind(alluniPlot, plotSp@data)
  
}

alluniPlot %>% 
  # dplyr::filter(measure_year==2016) %>% 
  ggplot(., aes(x = plot_agbd, y = pred_agbd_v2, color = as.character(measure_year))) +
  geom_point()+xlim(0,400)+ylim(0,400)

# redefine the plot location by using the 90m grid 
alluniPlotSp <-  alluniPlot %>% SpatialPointsDataFrame(coords =.[, c("coords.x1",  "coords.x2" )], data =.,
                                                       proj4string = CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 '))

# obtaining only the grids with plots in them -> reucing grid sizes
inter1 <- ifelse(sf::st_intersects(st_as_sf(grid1), st_as_sf(alluniPlotSp), sparse = F), 
                 "Yes", 
                 "No")
grid1_plot <- apply(as.data.frame(inter1),1,function(x) ifelse(any(x=='Yes'), T, F))
grid1$withPlots <- grid1_plot
g1_plots <-  grid1[grid1$withPlots==T,]
names(g1_plots)[2] <- 'withPlots'
g1_plots$gid <-  seq.int(nrow(g1_plots)) #give the filtered grids unique ids 


inter2 <- ifelse(sf::st_intersects(st_as_sf(grid2), st_as_sf(alluniPlotSp), sparse = F), 
                 "Yes", 
                 "No")
grid2_plot <- apply(as.data.frame(inter2),1,function(x) ifelse(any(x=='Yes'), T, F))
grid2$withPlots <- grid2_plot
g2_plots <-  grid2[grid2$withPlots==T,]
names(g2_plots)[2] <- 'withPlots'
g2_plots$gid <-  seq.int(from =length(g1_plots$gid)+1, to = length(g1_plots$gid)+nrow(g2_plots))  #give the filtered grids unique ids but follwoing the first ones


# for each plot: assign the grid id that they are in 
alluniPlotSp$g1_id <-  over(alluniPlotSp, g1_plots)$gid
alluniPlotSp$g2_id <-  over(alluniPlotSp, g2_plots)$gid


#check the ones plots that fall into the same gird and see if change can be calced for these grids 
table_grid1 <- table(na.omit(alluniPlotSp$g1_id))>1
dupID_grid1 <- names(table_grid1 )[table_grid1]
alluniPlotSps_sub1 <- alluniPlotSp[alluniPlotSp$g1_id %in% as.integer(dupID_grid1),]
rate1 <- alluniPlotSps_sub1 %>% 
  as.data.frame() %>% 
  dplyr::select(g1_id, measure_year,Plot_ID , plot_agbd, landuse) %>% 
  group_by(g1_id) %>% 
  dplyr::mutate(landuse =as.character(landuse), dur= dplyr::first(measure_year)-dplyr::last(measure_year)+1) %>% 
  dplyr::mutate(change= (dplyr::first(plot_agbd)-dplyr::last(plot_agbd)), 
                rate = (dplyr::first(plot_agbd)-dplyr::last(plot_agbd))/dur) %>% 
  dplyr::filter(dur>2) %>%
  dplyr::filter(!duplicated(paste(measure_year, g1_id, sep="_"))) %>% #for this one, it's removing usually not the first and last rows of each group
  dplyr::ungroup() %>% dplyr::rename(g_id= g1_id)

table_grid2 <- table(na.omit(alluniPlotSp$g2_id))>1
dupID_grid2 <- names(table_grid2)[table_grid2]
alluniPlotSps_sub2 <- alluniPlotSp[alluniPlotSp$g2_id %in% as.integer(dupID_grid2),]
rate2 <- alluniPlotSps_sub2 %>% 
  as.data.frame() %>% 
  dplyr::select(g2_id, measure_year,Plot_ID , plot_agbd, landuse ) %>% 
  group_by(g2_id) %>% 
  dplyr::mutate(landuse =as.character(landuse), dur= dplyr::first(measure_year)-dplyr::last(measure_year)+1) %>% 
  dplyr::mutate(change= (dplyr::first(plot_agbd)-dplyr::last(plot_agbd)), 
                rate = (dplyr::first(plot_agbd)-dplyr::last(plot_agbd))/dur) %>% 
  dplyr::filter(dur>2) %>% 
  dplyr::filter(!duplicated(paste(measure_year, g2_id, sep="_"))) %>% #for this one, it's removing usually not the first and last rows of each group
  dplyr::ungroup() %>% dplyr::rename(g_id= g2_id)


#combine the rate 1 and rate 2 and check duplicates 
rates <- rbind(rate1, rate2) %>%   ##[**need to get coordinates**]##
  pivot_wider(id_cols = c(g_id,dur, change, rate),names_from=measure_year,values_from=c(landuse, Plot_ID, plot_agbd)) %>% 
  dplyr::mutate(forest_condition0 =  dplyr::coalesce(landuse_2021, landuse_2020, landuse_2018)) %>% 
  dplyr::mutate(forest_condition = ifelse(landuse_2016 %in% c('Dense forest') | forest_condition0 %in% c('Dense forest '),'Dense forest','Open forest' ))

rates %>% data.frame()

p <- rates %>%
  ggplot( aes(x=rate, fill=forest_condition)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_bw() +
  labs(fill="")
p

#### extract the predicted AGBD data at the gird with change rates  ###
delta_grid1 <- g1_plots[g1_plots$gid %in% rates$g_id,] %>%st_as_sf(.) %>% st_centroid(.) %>% as(.,'Spatial')
delta_grid2 <- g2_plots[g2_plots$gid %in% rates$g_id,] %>% st_as_sf(.) %>% st_centroid(.) %>% as(.,'Spatial')


for (p_year in c('2016', '2018', '2019','2020', '2021')){
  print(p_year)
  ras1 <- raster(paste('agbd_predictions/desa_ts_out/out/',icc,'agbd_',p_year,'_xgboost_sqrt_nbr_lhs_33_',pattern,'_ls_lhs_medoid_168051_',p_year,'_NBR_3by3_dur8_biome1_val_v2-0000000000-0000000000.tif', sep=''))
  ras2 <- raster(paste('agbd_predictions/desa_ts_out/out/',icc,'agbd_',p_year,'_xgboost_sqrt_nbr_lhs_33_',pattern,'_ls_lhs_medoid_168050_',p_year,'_NBR_3by3_dur8_biome1_val_v2-0000005120-0000000000.tif', sep=''))
  names(ras1) <- paste('agbd', p_year, sep='_')
  names(ras2) <- paste('agbd', p_year, sep='_')
  
  #aggregate from 30x30 resolution to 200x200 (factor = 200/3)
  # res <-  100
  ras1.aggregate <- aggregate(ras1, fact=90/30)
  ras2.aggregate <- aggregate(ras2, fact=90/30)
  
  grid1_pred_agbd <- raster::extract(ras1.aggregate, delta_grid1, df=T)
  grid2_pred_agbd <- raster::extract(ras2.aggregate, delta_grid2, df=T) 
  
  delta_grid1$pred_agbd <- grid1_pred_agbd[,2]
  delta_grid2$pred_agbd <- grid2_pred_agbd[,2]
  
  names(delta_grid1)[names(delta_grid1) == 'pred_agbd'] <- paste('agbd', p_year, 'res',res, sep='_')
  names(delta_grid2)[names(delta_grid2) == 'pred_agbd'] <- paste('agbd', p_year, 'res',res, sep='_')
  
}

delta_grids <- rbind(delta_grid1[ , names(delta_grid1) != "AGBD2021_168051_90m"],
                     delta_grid2[ , names(delta_grid2) != "AGBD2021_168050_90m"])

##dynamic  way of calc delta for predicted data, by using 2016  - dur 
pred_rates_dyn <- c()   
for (rr in 1:nrow(delta_grids@data)){
  print(rr)
  rdf <- delta_grids@data[rr,]
  rdf2 <- rdf %>% dplyr::left_join(rates, by=c('gid'='g_id'))
  print(rdf2)
  dura <- rdf2$dur
  print(dura)
  print(names(rdf2[3+dura-2]))
  pred_rate <- (rdf2[, 3+dura-2]-rdf2[, 3])/dura
  pred_rates_dyn <- c(pred_rates_dyn, pred_rate)
}

cor(pred_rates_dyn, rates$rate)

#use a static duration for calc the ave rate
rates %>% dplyr::left_join(delta_grids@data, by =c('g_id'='gid')) %>% data.frame() %>% 
  dplyr::mutate(predicted_delta = agbd_2020_res_200- agbd_2016_res_200, predicted_rate = predicted_delta/ 5)->tr

cor(tr$rate, tr$predicted_rate)



change_validation <- ggscatter(tr, x='rate', y='predicted_rate',
                               add = "reg.line",conf.int = TRUE,  # Add regressin line
                               add.params = list(color = "blue", fill = "lightgray"))+ # Customize reg. line
  # conf.int = TRUE, xlab = "plot agbd", ylab = "pred agbd")+
  stat_regline_equation(label.x=5, label.y=8) +
  stat_cor(aes(label=..rr.label..), label.x=5, label.y=6)+
  stat_cor(method = "pearson", label.x = 5, label.y =4)+
  coord_cartesian(xlim =c(-5, 10), ylim = c(-5,10))+
  labs(y= "Estimated average AGBD change rate (Mg/ha/yr)", x = "Plot average AGBD change rate (Mg/ha/yr)") 
change_validation

hdelta <- tr %>%
  dplyr::select(rate, predicted_rate) %>% 
  tidyr::pivot_longer(everything(),names_to ='type',values_to = 'agbd') %>% 
  ggplot( aes(x=agbd, fill=type)) +
  geom_histogram( color="#e9ecef", size=0.2, alpha=0.6, bins=70, position = 'identity') +
  scale_fill_manual(labels=c('Predicted AGBD change rate', "Plot AGBD change rate"),values=c('blue', 'green'))+
  labs(xlab='AGBD change rate (Mg/ha/year)')+
  theme_bw() +
  theme(legend.position = c(0.7, 0.8)) +
  labs(fill="")

hdelta


dist_scatter_change <- ggarrange(hdelta,change_validation,
                                 labels = c("A", "B"),
                                 ncol = 1, nrow = 2)

dist_scatter_change


ggsave(paste("Fig/desa2+1sig_sen98_sol_collrn_thre_tuned_dcol_icc.png",sep=""), plot= dist_scatter_change, 
       width = 1500, height = 2600, units = "px")

