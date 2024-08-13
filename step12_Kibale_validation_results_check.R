setwd('/gpfs/data1/duncansongp/amberliang/EA_data')
library(tidyr)
library(ggplot2)
library(ggpubr)

#------step 12.0: extract raster predictions' values at plots using a buffer--------
####method 1: just upscale the raster predictions###

test <- read.csv('agbd_predictions/Kibale_Aug2_output_sampling_2020_EF2.csv')

csvname <- 'pred_agbd_icc_all_plots_years_val_EF2_beam+2sig_no5_Sept_bilinear3'

agbdOut <- read.csv(paste('agbd_predictions/kibale_ts_out2/',csvname, '.csv', sep=""))
  # 'agbd_predictions/kibale_ts_out2/pred_agbd_all_plots_years_val_EF2_2sig_no5_ht_Aug_bilinear3.csv')
  # 'agbd_predictions/kibale_ts_out2/previous/pred_agbd_all_plots_years_val_EF2_2sig_no5_Samp2_827_bilinear3.csv')  ##good single year bias on flux
  # 'agbd_predictions/kibale_ts_out/pred_agbd_all_plots_years_val_default_1sig_no5_823_bilinear3.csv')
  # pred_agbd_all_plots_years_val_default_1sig_no5_628.csv')
  # 'agbd_predictions/kibale_ts_out/pred_icc_agbd_all_plots_years_val_EF2_2sig_no5_628_coarse_bilinear.csv') ##the best
# 'agbd_predictions/kibale_ts_out2/pred_icc_agbd_all_plots_years_val_default_1sig_no5_823.csv')
#'agbd_predictions/kibale_ts_out/pred_icc_agbd_all_plots_years_val_EF2_2sig_no5_628_coarse_bilinear.csv')
#pred_icc_agbd_all_plots_years_val_EF2_2sig_no5_628_coarse_bilinear.csv')
#read.csv('agbd_predictions/kibale_ts_out/pred_agbd_all_plots_years_val_default_2sig.csv')
agbdOld <- read.csv(
  'agbd_predictions/kibale_ts_out/pred_icc_agbd_all_plots_years_val_EF2_2sig_no5_628_coarse_bilinear.csv')



plots <- agbdOut$Plot_ID %>% unique()

plot <- plots[12]
print(plot)
agbdOut1 <- agbdOut %>% 
  dplyr::select(Plot_ID, Stratum, measure_year, plot_agbd, pred_agbd) %>% 
  pivot_wider(names_from = measure_year, values_from = c(plot_agbd, pred_agbd))
print(agbdOut1)

agbdOut2 <- agbdOut1 %>% 
  dplyr::group_by(Plot_ID) %>% 
  dplyr::mutate(pDelta1417= (plot_agbd_2017-plot_agbd_2014)/3, pDelta0820= (plot_agbd_2020-plot_agbd_2008)/12, pDelta1720= (plot_agbd_2020-plot_agbd_2017)/3, pDelta1420= (plot_agbd_2020-plot_agbd_2014)/6 ) %>% 
  dplyr::mutate(eDelta1417= (pred_agbd_2017-pred_agbd_2014)/3, eDelta1720= (pred_agbd_2020-pred_agbd_2017)/3, eDelta1420= (pred_agbd_2020-pred_agbd_2014)/6 ) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(Stratum2=ifelse(Stratum %in% c('2000','2002','2003', '2004', '2005', '2006','2007','2008', '2009', '2010'),
                                'Planted in and after 2000', 'Planted prior to 2000'))
# dplyr::filter(Stratum !='2004') %>% 
  # dplyr::mutate(Stratum2=ifelse(Stratum %in% c('2000','2002','2003', '2004', '2005', '2006','2007','2008', '2009', '2010'),
  #                               'Planting stratum in and after 2000', ifelse(Stratum %in% c("NaturalForest_Kanyanchu", "NaturalForest_Sebitole"), 'Protected Areas', 
  #                                                                            ifelse(Stratum =='Regeneration Area', "Passive Regeneration", 'Planting stratum before 2000'))))

cor(agbdOut2$pDelta1720, agbdOut2$eDelta1720, use= 'complete.ob')
cor(agbdOut2$pDelta0820, agbdOut2$eDelta1420, use= 'complete.ob')
cor(agbdOut2$pDelta1420, agbdOut2$eDelta1420, use= 'complete.ob')
cor(agbdOut2$pDelta1417, agbdOut2$eDelta1420, use= 'complete.ob')
cor(agbdOut2$plot_agbd_2020, agbdOut2$pred_agbd_2020, use= 'complete.ob')


p1 <- ggplot(agbdOut2, aes(x=pDelta0820, y=eDelta1420)) + geom_point(shape=1)+
  geom_smooth(method = "lm",  formula = y ~ x)+
  coord_cartesian(xlim =c(-5,30), ylim = c(-5, 30))
  # facet_wrap( ~ Stratum, ncol=3)
p1


change_validation <- ggscatter(agbdOut2,x='pDelta0820', y='eDelta1420', 
          add = "reg.line",conf.int = TRUE,  # Add regressin line
          add.params = list(color = "blue", fill = "lightgray"))+ # Customize reg. line
  # conf.int = TRUE, xlab = "plot agbd", ylab = "pred agbd")+
  stat_regline_equation(label.x=30, label.y=28) +
  stat_cor(aes(label=..rr.label..), label.x=30, label.y=23)+
  stat_cor(method = "pearson", label.x = 30, label.y =16)+
  coord_cartesian(xlim =c(-20, 40), ylim = c(-20,40))+
  labs(y= "Estimated average AGBD change rate (Mg/ha/yr)", x = "Plot average AGBD change rate (Mg/ha/yr)") 
change_validation

change_validation_group <- ggscatter(agbdOut2,x='pDelta0820', y='eDelta1420', facet.by = "Stratum2",
          add = "reg.line", conf.int = TRUE, # Add regressin line
          add.params = list(color = "blue", fill = "lightgray"))+ # Customize reg. line
          # conf.int = TRUE, xlab = "plot agbd", ylab = "pred agbd")+
  stat_regline_equation(label.x=17, label.y=28) +
  stat_cor(aes(label=..rr.label..), label.x=17, label.y=23)+
  stat_cor(method = "pearson", label.x = 17, label.y =16)+
  coord_cartesian(xlim =c(-10, 30), ylim = c(-10,30))+
  labs(y= "Estimated average AGBD change rate (Mg/ha/yr)", x = "Plot average AGBD change rate (Mg/ha/yr)") 
change_validation_group


# p11 <-  p1 + geom_text(data=eq,aes(x = 25, y = 300,label=V1), parse = TRUE, inherit.aes=FALSE) + facet_grid(group~.)
# p11 
library(Metrics)
agbdOut2 %>% dplyr::select(plot_agbd_2020, pred_agbd_2020) %>% drop_na()->tr
mae <- mae(tr$plot_agbd_2020, tr$pred_agbd_2020)
rmse <- rmse(tr$plot_agbd_2020, tr$pred_agbd_2020)
bias <- bias(tr$plot_agbd_2020, tr$pred_agbd_2020)
print(paste(rmse, bias,mae))


sp3 <- ggscatter(agbdOut2, x='plot_agbd_2020', y='pred_agbd_2020', #facet.by = "Stratum2",
                 add = "reg.line",  # Add regressin line
                 add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
                 conf.int = TRUE, xlab = "plot agbd", ylab = "pred agbd")+
  coord_cartesian(xlim =c(0,300), ylim = c(0, 300))+
  stat_regline_equation(label.x=3, label.y=280) +
  stat_cor(aes(label=..rr.label..), label.x=3, label.y=250)+
  stat_cor(method = "pearson", label.x = 3, label.y =220)+
  theme_bw()+
  theme( strip.background = element_blank(),strip.text = element_text(
    size = 12, face = "bold"), axis.text=element_text(size=11), 
    plot.title = element_text(size=12, face = "bold"))
sp3


sp4 <- ggscatter(agbdOut2, x='plot_agbd_2020', y='pred_agbd_2020', facet.by = "Stratum2",
                 add = "reg.line",  # Add regressin line
                 add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
                 conf.int = TRUE, xlab = "plot agbd", ylab = "pred agbd")+
  coord_cartesian(xlim =c(0,300), ylim = c(0, 300))+
  stat_regline_equation(label.x=3, label.y=280) +
  stat_cor(aes(label=..rr.label..), label.x=3, label.y=250)+
  stat_cor(method = "pearson", label.x = 3, label.y =220)+
  theme_bw()+
  theme( strip.background = element_blank(),strip.text = element_text(
    size = 12, face = "bold"), axis.text=element_text(size=11), 
    plot.title = element_text(size=12, face = "bold"))
sp4

# ggsave("Fig/kibale_agbd_delta_validation_EF2_2sig_icc.png", plot= change_validation, width = 12, height = 12, units = "cm")
# ggsave("Fig/kibale_agbd_delta_validation_coarse_stratum_EF2_2sig_icc.png", plot= change_validation_group, width = 24, height = 18, units = "cm")
# ggsave("Fig/kibale_agbd_2000_validation_coarse_stratum_EF2_2sig_icc.png", plot= sp3, width = 24, height = 18, units = "cm")
# 


####replot with overlapping historgam ########
offset_factor <- 0   #this is the bias from 2020 validtaion, use 0 

pdelta <- agbdOut2 %>%
  dplyr::select(pDelta0820, eDelta1420) %>% dplyr::mutate(eDelta1420=eDelta1420+offset_factor) %>% 
  tidyr::pivot_longer(everything(),names_to ='type',values_to = 'agbd') %>% 
  ggplot( aes(x=agbd, fill=type)) +
  geom_histogram( color="#e9ecef", size=0.2, alpha=0.6, bins=70, position = 'identity') +
  scale_fill_manual(labels=c('Predicted AGBD change rate', "Plot AGBD change rate"),values=c('blue', 'green'))+
  labs(xlab='AGBD change rate (Mg/ha/year)')+
  theme_bw() +
  theme(legend.position = c(0.2, 0.8)) +
  labs(fill="")

pdelta

dist_scatter_change <- ggarrange(pdelta,change_validation,
          labels = c("A", "B"),
          ncol = 1, nrow = 2)


pdelta_group <- agbdOut2 %>%
  dplyr::select(pDelta0820, eDelta1420, Stratum2) %>% 
  tidyr::pivot_longer(cols=1:2,names_to ='type',values_to = 'agbd') %>% 
  ggplot( aes(x=agbd, fill=type)) +
  geom_histogram( color="#e9ecef",  size=0.1,alpha=0.5, bins=50, position = 'identity') +
  scale_fill_manual(labels=c('Predicted AGBD change rate', "Plot AGBD change rate"),values=c('blue', 'green'))+
  theme_bw() +
  theme(legend.position = c(0.2, 0.8)) +
  labs(fill="")+facet_wrap(vars(Stratum2))

pdelta_group


dist_scatter_change_group <- ggarrange(pdelta_group,change_validation_group,
                                 labels = c("A", "B"),
                                 ncol = 1, nrow = 2)

# 
# #inspecting the outliers
# agbdOut2[agbdOut2$pDelta0820 >=15 ,] %>% 
#   dplyr::select("pDelta1417", "pDelta0820" ,"pDelta1720" , "pDelta1420" ,"eDelta1417","eDelta1720",
#                 "eDelta1420", "Stratum", 'Plot_ID', "pred_agbd_2020", "pred_agbd_2014", "plot_agbd_2008", "plot_agbd_2020" ) %>% 
#   dplyr::filter(!is.na(eDelta1420)) %>% data.frame()


p2020 <- agbdOut2 %>%
  dplyr::select(plot_agbd_2020, pred_agbd_2020) %>% 
  tidyr::pivot_longer(everything(),names_to ='type',values_to = 'agbd') %>% 
  ggplot( aes(x=agbd, fill=type)) +
  geom_histogram( color="#e9ecef", size=0.1, alpha=0.6, bins=100, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080"), labels=c("Plot AGBD for 2020",'Predicted AGBD for 2020')) +
  theme_bw() +theme(legend.position = c(0.7, 0.8))+
  labs(fill="")

p2020

dist_scatter_p2020 <- ggarrange(p2020,sp3,
                                      labels = c("A", "B"),
                                      ncol = 1, nrow = 2)

p2020_group <- agbdOut2 %>%
  dplyr::select(plot_agbd_2020, pred_agbd_2020, Stratum2) %>% 
  tidyr::pivot_longer(cols=1:2,names_to ='type',values_to = 'agbd') %>% 
  ggplot( aes(x=agbd, fill=type)) +
  geom_histogram( color="#e9ecef", size=0.1, alpha=0.6, bins=50, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080"), labels=c("Plot AGBD for 2020",'Predicted AGBD for 2020')) +
  theme_bw() + theme(legend.position = c(0.84, 0.8)) +
  labs(fill="")+facet_wrap(vars(Stratum2))

p2020_group

dist_scatter_p2020_group <- ggarrange(p2020_group,sp4,
                                       labels = c("A", "B"),
                                       ncol = 1, nrow = 2)


ggsave(paste("Fig/kibale_", csvname, "_p2020_group.png",sep=""), plot= dist_scatter_p2020_group, 
       width = 1800, height = 1800, units = "px")
ggsave(paste("Fig/kibale_", csvname, "_p2020.png",sep=""), plot= dist_scatter_p2020, 
       width = 1000, height = 1800, units = "px")

ggsave(paste("Fig/kibale_", csvname, "_delta_group.png",sep=""), plot= dist_scatter_change_group, 
       width = 1800, height = 1800, units = "px")
ggsave(paste("Fig/kibale_", csvname, "_delta.png",sep=""), plot= dist_scatter_change, 
       width = 2400, height = 3000, units = "px")



ggsave("Fig/kibale_agbd_delta_hist_EF2_2sig_icc_bilinear.png", plot= pdelta, width = 12, height = 8, units = "cm")
ggsave("Fig/kibale_agbd_2000_hist_EF2_2sig_icc_bilinear.png", plot= p2000, width = 12, height = 8, units = "cm")
ggsave("Fig/kibale_agbd_delta_hist_coarse_stratum_EF2_2sig_icc_bilinear.png", plot=pdelta_group, width = 12, height = 6, units = "cm")
ggsave("Fig/kibale_agbd_2000_hist_coarse_stratum_EF2_2sig_icc_bilinear.png", plot= p2000_group, width = 24, height = 6, units = "cm")



#---------extracting agbd values from predicted raster using the plantation compartment polygons---------
library(raster)   
library(rgdal)
poly <- readOGR('AOI/Kibale/kibale_managed_all3.shp')

tile_agbd_by_manage <- data.frame()
for (pred_year in 2021:1986){   ##[slow]
  print(pred_year)
  # pred_year <- as.character(pred_year)
  if (pred_year < 2019  && pred_year>2015){
    annual_ras1 <- raster(paste('agbd_predictions/kibale_ts_out/icc_agbd_', pred_year,'_xgboost_sqrt_nbr_lhs_33_tile_no5+beam+2sig_thre_tuned_dcol_ls_lhs_medoid_173060_', pred_year,'_NBR_3by3_dur8_biome1_val-0000000000-0000005120.tif', sep=''))
    annual_ras2 <- raster(paste('agbd_predictions/kibale_ts_out/icc_agbd_', pred_year,'_xgboost_sqrt_nbr_lhs_33_tile_no5+beam+2sig_thre_tuned_dcol_ls_lhs_medoid_173060_', pred_year,'_NBR_3by3_dur8_biome1_val-0000002560-0000005120.tif', sep=''))
    
  } else if (pred_year<=2015){
    annual_ras1 <- raster(paste('agbd_predictions/kibale_ts_out/icc_agbd_', pred_year,'_xgboost_sqrt_nbr_lhs_33_tile_no5+beam+2sig_thre_tuned_dcol_ls_lhs_medoid_173060_', pred_year,'_NBR_3by3_dur8_biome1_val_v2-0000000000-0000005120.tif', sep=''))
    annual_ras2 <- raster(paste('agbd_predictions/kibale_ts_out/icc_agbd_', pred_year,'_xgboost_sqrt_nbr_lhs_33_tile_no5+beam+2sig_thre_tuned_dcol_ls_lhs_medoid_173060_', pred_year,'_NBR_3by3_dur8_biome1_val_v2-0000002560-0000005120.tif', sep=''))
    
    
  } else if (pred_year %in% c(2021,2020,2019)){
    annual_ras1 <- raster(paste('agbd_predictions/kibale_ts_out/icc_agbd_', pred_year,'_xgboost_sqrt_nbr_lhs_33_tile_no5+beam+2sig_thre_tuned_dcol_ls_lhs_medoid_173060_', pred_year,'_NBR_3by3_dur8_biome1-0000000000-0000005120.tif', sep=''))
    annual_ras2 <- raster(paste('agbd_predictions/kibale_ts_out/icc_agbd_', pred_year,'_xgboost_sqrt_nbr_lhs_33_tile_no5+beam+2sig_thre_tuned_dcol_ls_lhs_medoid_173060_', pred_year,'_NBR_3by3_dur8_biome1-0000002560-0000005120.tif', sep=''))
  }
  
  names(annual_ras1) <- paste('est_agbd', sep='')
  names(annual_ras2) <- paste('est_agbd',  sep='')
  
  annual_agbd_by_manage <- data.frame()
  for (pl in 1:length(poly)){
    print(pl)
    spoly <- poly[pl,]
    sub <- '1'
    if (!class(try(intersect(annual_ras1,spoly),T ))=='try-error'){
      annual1_sub <- raster::crop(annual_ras1, spoly)
      annual1_sub2 <- raster::mask(annual1_sub,spoly) %>% as.data.frame(., xy = TRUE) %>%
        mutate(manage_stratum = spoly$PlantingYe, estimat_year =pred_year, ras_sub =sub)
      annual1_sub2 <-  annual1_sub2 %>% dplyr::filter(!is.na(est_agbd))
    }else{
      annual1_sub2 <- data.frame()
    }

    sub2 <- '2'
    if (!class(try(intersect(annual_ras2,spoly),T ))=='try-error'){
      annual1_sub_v2 <- raster::crop(annual_ras2, spoly)
      annual1_sub2_v2 <- raster::mask(annual1_sub_v2, spoly) %>% as.data.frame(., xy = TRUE) %>%
        mutate(manage_stratum = spoly$PlantingYe, estimat_year =pred_year, ras_sub= sub2)
      annual1_sub2_v2 <-  annual1_sub2_v2 %>% dplyr::filter(!is.na(est_agbd))
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


write.csv( tile_agbd_by_manage, "agbd_predictions/kibale_ts_out/icc_agbd_by_planting_px.csv")


#calcualte the mean and median AAGBD by managemnt year and plot the time series of mean and median change
agbd_ts <- tile_agbd_by_manage %>% 
  dplyr::mutate(manage_stratum =as.character(manage_stratum)) %>% 
  dplyr::mutate(manage_stratum= sub("^","Plant year ", manage_stratum)) %>% 
  dplyr::mutate(manage_stratum =ifelse(manage_stratum =='Plant year 1990', 'Normal PA', manage_stratum)) %>% 
  dplyr::mutate(manage_stratum =ifelse(manage_stratum =='Plant year 1991', 'Non-strict PA', manage_stratum)) %>% 
  dplyr::mutate(manage_stratum =ifelse(manage_stratum =='Plant year 1993', 'Passive regeneration', manage_stratum)) %>% 
  dplyr::group_by(manage_stratum, estimat_year) %>% 
  dplyr::summarise(meanAGBD=mean(est_agbd), medianAGBD=median(est_agbd), sd_agbd=sd(est_agbd)) %>% 
  pivot_longer(names_to='statistics', cols =c(meanAGBD, medianAGBD)) %>% 
  ggplot(data=., aes(x=estimat_year, y=value, group=statistics)) +
  facet_wrap(vars(manage_stratum))+
  geom_line(color='grey',linetype="dashed", size=0.7)+
  geom_point(aes(color=statistics), size=1)+
  scale_color_brewer(palette="Dark2")+ylab('AGBD (Mg/ha)')+ylab('Year of Predictions')

ggsave("Fig/kibale_agbd_ts_icc.png", plot= agbd_ts, width =2800, height = 1600, units = "px")


#calculate change rate before and after implementing managemnt 
###1. simply using the mean of each strata
simp <- tile_agbd_by_manage %>% 
  dplyr::mutate(implement_year =as.numeric(as.character(manage_stratum))) %>% 
  dplyr::mutate(manage_stratum =as.character(manage_stratum)) %>% 
  dplyr::mutate(manage_stratum= sub("^","Plant year ", manage_stratum)) %>% 
  dplyr::mutate(manage_stratum =ifelse(manage_stratum =='Plant year 1990', 'Normal PA', manage_stratum)) %>% 
  dplyr::mutate(manage_stratum =ifelse(manage_stratum =='Plant year 1991', 'Non-strict PA', manage_stratum)) %>% 
  dplyr::mutate(manage_stratum =ifelse(manage_stratum =='Plant year 1993', 'Passive regeneration', manage_stratum)) %>% 
  dplyr::group_by(manage_stratum, estimat_year) %>% 
  dplyr::summarise(meanAGBD=mean(est_agbd), medianAGBD=median(est_agbd), sd_agbd=sd(est_agbd), implement_year=unique(implement_year)) %>% 
  dplyr::ungroup()

rate_simp <- simp %>%  dplyr::mutate(manage_status = ifelse((estimat_year > implement_year)|is.na(implement_year), 'after_imp','before_imp')) %>% 
  dplyr::mutate(est_agbd =medianAGBD) %>% 
  dplyr::group_by(manage_stratum, manage_status) %>% 
  dplyr::mutate(dur= dplyr::last(estimat_year)-dplyr::first(estimat_year)+1) %>% 
  dplyr::mutate(change= (dplyr::last(est_agbd)-dplyr::first(est_agbd)), rate = (dplyr::last(est_agbd)-dplyr::first(est_agbd))/dur)

rate_change_simp <- ggplot(rate_simp, aes(x = manage_stratum, y = rate, color =manage_status)) +
  geom_point(size=3)+
  scale_color_viridis(discrete=T, name="")+theme_bw()+
  xlab('Mangement type')+ylab('Average AGBD change rate (Mg/ha/Yr)')+ 
  theme(axis.text.x = element_text(angle = 30, vjust = 0.9, hjust=1))
rate_change_simp

ggsave("Fig/kibale_agbd_rate_median.png", plot=rate_change_simp, width = 18, height = 14, units = "cm")



###2) agbd change rate before and after implementation by site -- distribution plot
prep <- tile_agbd_by_manage %>%   ##**note NA in gap years have been removed in dataset compilation **##
  # dplyr::group_by(manage_stratum, estimat_year) %>% 
  # dplyr::summarise(meanAGBD=mean(est_agbd), medianAGBD=median(est_agbd), sd_agbd=sd(est_agbd)) %>% 
  dplyr::mutate(implement_year = manage_stratum) %>% 
  dplyr::mutate(implement_year= as.numeric(as.character(implement_year)), manage_stratum=as.numeric(as.character(manage_stratum))) %>% 
  dplyr::mutate(manage_stratum =ifelse(manage_stratum ==1990, 'Normal PA', manage_stratum)) %>%
  dplyr::mutate(manage_stratum =ifelse(manage_stratum ==1991, 'Non-strict PA', manage_stratum)) %>% 
  dplyr::mutate(manage_stratum =ifelse(manage_stratum ==1993, 'Passive regeneration', manage_stratum)) %>%
  # dplyr::mutate(implement_year = ifelse(implement_year%in% c(1990, 1991, 1993), NA, implement_year))%>%
  dplyr::mutate(implement_year = ifelse(implement_year %in% c(1993), 1995, implement_year)) %>% 
  dplyr::mutate(implement_year = ifelse(implement_year %in% c(1991), NA, implement_year)) %>% #non strict PAs
  dplyr::mutate(implement_year = ifelse(implement_year %in% c(1990), NA, implement_year)) %>% #normal PA will not have a start year
  dplyr::mutate(loc= paste(x, y, sep='_'))
  # dplyr::mutate(before_rate = )

# t <- prep %>% dplyr::group_by(loc) 
# joindf <- data.frame(manage_stratum =rep(unique(prep$manage_stratum),each = (2021-1986)+1), estimate_year =rep(c(2021:1986), length(unique(prep$manage_stratum))))
test <- prep %>%
  dplyr::mutate(manage_status = ifelse((estimat_year > implement_year)|is.na(implement_year), 'after_imp','before_imp')) %>% 
  # `dplyr::filter(loc =='30.326629918816_0.464204423429297')%>% 
  dplyr::group_by(loc, manage_status) %>% 
  dplyr::mutate(dur= dplyr::first(estimat_year)-dplyr::last(estimat_year)+1) %>% 
  dplyr::mutate(change= (dplyr::first(est_agbd)-dplyr::last(est_agbd)), rate = (dplyr::first(est_agbd)-dplyr::last(est_agbd))/dur) %>% 
  dplyr::ungroup()

#add additional column on the intervention type#
`%notin%` <- Negate(`%in%`)
test$intervention <- 'none'
test[test$manage_stratum %in% c('Normal PA', 'Non-strict PA'),]$intervention <- 'Conservation'
test[test$manage_stratum %in% c('Passive regeneration'),]$intervention <- 'Natural Regeneration'
test[test$manage_stratum %notin% c('Passive regeneration', 'Normal PA', 'Non-strict PA'),]$intervention <- 'Reforestation'


rate_change <- ggplot(test, aes(x = na.omit(manage_stratum), y = rate, fill=manage_status)) +
  geom_boxplot(position="dodge", alpha=0.5) +
  scale_fill_viridis(discrete=T, name="")+
  theme_bw()  + xlab('Mangement type')+ylab('Average AGBD chaange rate (Mg/ha/Yr)')+
  theme(axis.text.x = element_text(angle = 30, vjust = 0.9, hjust=1))+
  facet_wrap(vars(intervention), scales="free_x")
rate_change

ggsave("Fig/kibale_agbd_rate_dist_icc_pa.png", plot=rate_change, width = 3200, height= 1800, units='px')



#----------------stacking all images for plotting the change rate [see 12b]----------------------------------

poly <- readOGR('AOI/Kibale/kibale_managed_all3.shp')

agbd_stack1 <- stack()
agbd_stack2 <- stack()
for (pred_year in 2021:1986){
  print(pred_year)
  # pred_year <- as.character(pred_year)
  if (pred_year < 2019  && pred_year>2015){
    annual_ras1 <- raster(paste('agbd_predictions/kibale_ts_out/agbd_', pred_year,'_xgboost_sqrt_nbr_lhs_33_tile_no5+beam+2sig_thre_tuned_dcol_ls_lhs_medoid_173060_', pred_year,'_NBR_3by3_dur8_biome1_val-0000000000-0000005120.tif', sep=''))
    annual_ras2 <- raster(paste('agbd_predictions/kibale_ts_out/agbd_', pred_year,'_xgboost_sqrt_nbr_lhs_33_tile_no5+beam+2sig_thre_tuned_dcol_ls_lhs_medoid_173060_', pred_year,'_NBR_3by3_dur8_biome1_val-0000002560-0000005120.tif', sep=''))
    
  } else if (pred_year<=2015){
    annual_ras1 <- raster(paste('agbd_predictions/kibale_ts_out/agbd_', pred_year,'_xgboost_sqrt_nbr_lhs_33_tile_no5+beam+2sig_thre_tuned_dcol_ls_lhs_medoid_173060_', pred_year,'_NBR_3by3_dur8_biome1_val_v2-0000000000-0000005120.tif', sep=''))
    annual_ras2 <- raster(paste('agbd_predictions/kibale_ts_out/agbd_', pred_year,'_xgboost_sqrt_nbr_lhs_33_tile_no5+beam+2sig_thre_tuned_dcol_ls_lhs_medoid_173060_', pred_year,'_NBR_3by3_dur8_biome1_val_v2-0000002560-0000005120.tif', sep=''))
    
    
  } else if (pred_year %in% c(2021,2020,2019)){
    annual_ras1 <- raster(paste('agbd_predictions/kibale_ts_out/agbd_', pred_year,'_xgboost_sqrt_nbr_lhs_33_tile_no5+beam+2sig_thre_tuned_dcol_ls_lhs_medoid_173060_', pred_year,'_NBR_3by3_dur8_biome1-0000000000-0000005120.tif', sep=''))
    annual_ras2 <- raster(paste('agbd_predictions/kibale_ts_out/agbd_', pred_year,'_xgboost_sqrt_nbr_lhs_33_tile_no5+beam+2sig_thre_tuned_dcol_ls_lhs_medoid_173060_', pred_year,'_NBR_3by3_dur8_biome1-0000002560-0000005120.tif', sep=''))
  }
  
  names(annual_ras1) <- paste('est_agbd', as.character(pred_year),sep='')
  names(annual_ras2) <- paste('est_agbd', as.character(pred_year),sep='')
  
  agbd_stack1 <- stack( agbd_stack1 , annual_ras1)
  agbd_stack2 <- stack( agbd_stack2 , annual_ras2)

}

# poly <- readOGR('AOI/Kibale/kibale_managed_all3.shp')  #rasterize the polygons to two different base raster 
r1 <- agbd_stack1$est_agbd2021
r2 <- agbd_stack2$est_agbd2021

# poly1 <- crop(poly, extent(r1))
# poly2 <- crop(poly, extent(r2))
poly$PlantingYe <- as.numeric(as.character(poly$PlantingYe))

r1 <- rasterize(poly, r1, field='PlantingYe', background=1986)
r2 <- rasterize(poly, r2, field='PlantingYe', background=1986)

names(r1) <- 'manage_status'
names(r2) <- 'manage_status'

#stack on to the agbd stack for function overlays 
agbd_stack1 <-  stack(agbd_stack1, r1)
agbd_stack2 <-  stack(agbd_stack2, r2)


startTime <- Sys.time()

#using the vector based method
agbd_stack1_df <- as.data.frame(agbd_stack1, xy=TRUE)

#calc change rate
library(readr)
tt1 <- agbd_stack1_df %>%
  pivot_longer(cols = 3:38,names_to='est_year', values_to='est_agbd') %>%
  dplyr::mutate(est_year=parse_number(est_year), loc =paste(x,y, sep='_'))%>%
  dplyr::rename(manage_year=manage_status) %>% 
  dplyr::mutate(manage_status = ifelse((est_year > manage_year)|is.na(manage_year), 'after_imp','before_imp')) %>% 
  # `dplyr::filter(loc =='30.326629918816_0.464204423429297')%>% 
  dplyr::group_by(loc, manage_status) %>% 
  dplyr::mutate(dur= dplyr::first(est_year)-dplyr::last(est_year)+1) %>% 
  dplyr::mutate(change= (dplyr::first(est_agbd)-dplyr::last(est_agbd)), rate = (dplyr::first(est_agbd)-dplyr::last(est_agbd))/dur) %>% 
  dplyr::ungroup()
tt1

tt1_group <- tt1 %>% group_by(loc) %>% pivot_wider(names_from = manage_status, values_from = rate)

tt1_spdf <- tt1_group %>% SpatialPointsDataFrame(coords =.[,c(1,2)], data =.,
                                  proj4string = CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 '))


#rasterize
ras1_rate <- rasterize(test_spdf, r1, field='PlantingYe', background=1986)

endTime <- Sys.time()
# prints recorded time
print(endTime - startTime)



agbd_stack2_df <- as.data.frame(agbd_stack2, xy=TRUE)

# #apply funtion for calculating change rate based: bnow is 2021, bpast is 1986
# pixel_delta_before <- function(x){#bstat, bnow,b2020, b2019, b2018, b2017, b2016, bpast){
#   # before_rate <-  ifelse(bstat==1999, b1999-bpast/(1999-1986))
#   t= bstat
#   if t
#   s[9+1]-s[30]/t=1986
# 
# 
# }










