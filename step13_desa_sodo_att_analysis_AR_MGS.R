# this script is for estimating the ATT for AR intervenitonin MGS


setwd('/gpfs/data1/duncansongp/amberliang/EA_data')
library(tidyr)
library(ggplot2)
library(ggpubr)
library(raster)
library(rgdal)
`%notin%` <- Negate(`%in%`)
#combine sodo AGBD predictions with Desa'a predictions#
pattern <- 'no5+beam+2sig_thre_tuned_dcol'
tile_agbd_by_manage_ecotr <- read.csv(paste("agbd_predictions/AMF_sites_df/agbd_by_planting_px",
                                           pattern,"_mtrewenzuri_ecotrust_covar.csv", sep="")) %>%
  dplyr::mutate(interv='ecotrust',interv2='ANR') %>%
  dplyr::select(X,x,y, est_agbd, d2roads, pop_cnt_2000, slope,  dem, wc_prec_1990.1999, wc_tavg_1990.1999, soilType,
                manage_stratum, interv, interv2, estimat_year, ras_sub)

# pattern <- 'tile+beam+21halfsig_sol_power_0801samp_thre_tuned_dcol'
# tile_agbd_by_manage_humbosub <- read.csv(paste("agbd_predictions/AMF_sites_df/humbo_mf_agbd_collrm_by_planting_px",
#                                            pattern,"_covar.csv", sep="")) %>%
#   dplyr::mutate(interv='humbo',interv2='ANR') %>%
#   dplyr::select(X,x,y, est_agbd, d2roads, pop_cnt_2000, slope,  dem, wc_prec_1990.1999, wc_tavg_1990.1999, soilType,
#                 manage_stratum, interv, interv2, estimat_year, ras_sub)

pattern <- 'tile+beam+21halfsig_sol_power_0801samp_thre_tuned_dcol'
tile_agbd_by_manage_sodo <- read.csv(paste("agbd_predictions/sodo_ts_out/sodo_agbd_collrm_by_planting_px",
                                           pattern,"_covar.csv", sep="")) %>%
  dplyr::mutate(interv2=ifelse(interv =='humbo','ANR','AR')) %>%
  dplyr::filter(interv2=='AR') %>%
  dplyr::select(X,x,y, est_agbd, d2roads, pop_cnt_2000, slope,  dem, wc_prec_1990.1999, wc_tavg_1990.1999, soilType,
                manage_stratum, interv, interv2, estimat_year, ras_sub)

pattern <- 'tigray+1sig_sen96_sol_no5_nbr'
tile <- '169051'
# tile_agbd_by_manage <- read.csv('agbd_predictions/kibale_ts_out/agbd_by_planting_px.csv')
tile_agbd_by_manage_tigray <- read.csv(paste("agbd_predictions/AMF_sites_df/tigray_mf_agbd_collrm_by_planting_px",
                                             pattern,"_covar_v2.csv", sep="")) %>% 
  # dplyr::filter(interv %in% c('addi lihitsi')) %>% 
  dplyr::mutate(interv='tigray',interv2='NR') %>%
  dplyr::select(X,x,y, est_agbd, d2roads, pop_cnt_2000, slope,  dem, wc_prec_1990.1999, wc_tavg_1990.1999, soilType,
                manage_stratum, interv, interv2, estimat_year, ras_sub)


pattern <- 'desa2+1sig_sen98_sol_v4_rfr7_thre_tuned_dcol'
tile_agbd_by_manage_desa <- read.csv( paste("agbd_predictions/desa_ts_out/agbd_collrm_by_planting_px",
                                       pattern,"_chfor_AR_ANR_NR_covar.csv", sep="")) %>% 
  dplyr::filter(interv2 %notin% c('NR'))


tile_agbd_by_manage <- rbind(tile_agbd_by_manage_desa, tile_agbd_by_manage_tigray, tile_agbd_by_manage_sodo)

# pattern <- 'desa2+1sig_sen98_sol_v4_rfr7_thre_tuned_dcol'
# tile_agbd_by_manage <- read.csv( paste("agbd_predictions/desa_ts_out/agbd_collrm_by_planting_px",
#                                        pattern,"_chfor_AR_ANR_NR.csv", sep=""))

tile_agbd_by_manage$loc <- paste(tile_agbd_by_manage$x, tile_agbd_by_manage$y, sep="_")

set.seed(1994)

stratified <- function(df, group, size, select = NULL, replace = FALSE, bothSets = FALSE) {
  if (is.null(select)) {
    df <- df
  } else {
    if (is.null(names(select))) stop("'select' must be a named list")
    if (!all(names(select) %in% names(df)))
      stop("Please verify your 'select' argument")
    temp <- sapply(names(select),
                   function(x) df[[x]] %in% select[[x]])
    df <- df[rowSums(temp) == length(select), ]
  }
  df.interaction <- interaction(df[group], drop = TRUE)
  df.table <- table(df.interaction)
  df.split <- split(df, df.interaction)
  if (length(size) > 1) {
    if (length(size) != length(df.split))
      stop("Number of groups is ", length(df.split),
           " but number of sizes supplied is ", length(size))
    if (is.null(names(size))) {
      n <- setNames(size, names(df.split))
      message(sQuote("size"), " vector entered as:\n\nsize = structure(c(",
              paste(n, collapse = ", "), "),\n.Names = c(",
              paste(shQuote(names(n)), collapse = ", "), ")) \n\n")
    } else {
      ifelse(all(names(size) %in% names(df.split)),
             n <- size[names(df.split)],
             stop("Named vector supplied with names ",
                  paste(names(size), collapse = ", "),
                  "\n but the names for the group levels are ",
                  paste(names(df.split), collapse = ", ")))
    }
  } else if (size < 1) {
    n <- round(df.table * size, digits = 0)
  } else if (size >= 1) {
    if (all(df.table >= size) || isTRUE(replace)) {
      n <- setNames(rep(size, length.out = length(df.split)),
                    names(df.split))
    } else {
      message(
        "Some groups\n---",
        paste(names(df.table[df.table < size]), collapse = ", "),
        "---\ncontain fewer observations",
        " than desired number of samples.\n",
        "All observations have been returned from those groups.")
      n <- c(sapply(df.table[df.table >= size], function(x) x = size),
             df.table[df.table < size])
    }
  }
  temp <- lapply(
    names(df.split),
    function(x) df.split[[x]][sample(df.table[x],
                                     n[x], replace = replace), ])
  set1 <- do.call("rbind", temp)
  
  if (isTRUE(bothSets)) {
    set2 <- df[!rownames(df) %in% rownames(set1), ]
    list(SET1 = set1, SET2 = set2)
  } else {
    set1
  }
}

tile_agbd_by_manage_all <- stratified(tile_agbd_by_manage, "manage_stratum",0.5)

######calcualte the mean and median AAGBD by managemnt year and plot the time series of mean and median change
tile_agbd_by_manage_all <- tile_agbd_by_manage_all[!(tile_agbd_by_manage_all$manage_stratum%in%c(2021:2022)),]
tile_agbd_by_manage_all$manage_stratum[tile_agbd_by_manage_all$interv2=='NR'] <- 1986
tile_agbd_by_manage_all$interv <- as.character(tile_agbd_by_manage_all$interv)
# tile_agbd_by_manage_all$interv[tile_agbd_by_manage_all$interv2=='NR'] <- 'church forest conserved'
# tile_agbd_by_manage_all$interv2[tile_agbd_by_manage_all$interv=='sodo'] <- 'AR'

agbd_ts <- tile_agbd_by_manage_all %>% 
  dplyr::mutate(manage_stratum =as.character(manage_stratum), 
                interv =as.character(interv)) %>% 
  dplyr::mutate(manage_stratum= sub("^","Plant year ", manage_stratum)) %>% 
  dplyr::mutate(interv_by_year = paste(interv2, manage_stratum, sep="_")) %>% 
  # dplyr::mutate(manage_stratum =ifelse(is.na(manage_stratum), 'PA', manage_stratum))%>% 
  dplyr::group_by(interv_by_year, estimat_year) %>% 
  dplyr::summarise(meanAGBD=mean(est_agbd), medianAGBD=median(est_agbd), sd_agbd=sd(est_agbd)) %>% 
  pivot_longer(names_to='statistics', cols =c(meanAGBD, medianAGBD)) %>% 
  dplyr::ungroup()

agbd_ts_plot <- agbd_ts %>%  
  ggplot(data=., aes(x=estimat_year, y=value, group=statistics)) +
  facet_wrap(vars(interv_by_year))+
  geom_line(color='grey',linetype="dashed", size=0.7)+
  geom_point(aes(color=statistics), size=1)+
  scale_color_brewer(palette="Dark2")+xlab('Year')+ylab('Predicted AGBD (mg/ha)')+
  theme_bw()+theme(legend.position = c(0.9, 0.15)) +ylim(2,200)
agbd_ts_plot

######
tile_agbd_by_manage_all$manage_stratum[tile_agbd_by_manage_all$interv2=='NR'] <- 1986

tile_agbd_by_manage3 <- tile_agbd_by_manage_all %>%   #regroup
  # dplyr::filter(interv %notin% c('humbo')) %>% 
  dplyr::mutate(loc0=loc, loc =as.numeric(factor(loc))) %>% 
  dplyr::mutate(manage_stratum =as.character(manage_stratum), 
                interv2 =as.character(interv2)) %>% 
  # dplyr::mutate( interv2  = ifelse(interv %in% c('sodo'), 'AR', interv2)) %>% 
  dplyr::mutate(manage_stratum= sub("^","Plant year ", manage_stratum)) %>% 
  dplyr::mutate(manage_group = paste(interv2, manage_stratum, sep="_")) %>% 
  # dplyr::mutate(manage_stratum =as.character(manage_stratum), manage_group = manage_stratum) %>% 
  # dplyr::mutate(manage_stratum= sub("^","Plant year ", manage_stratum)) %>% 
  # dplyr::mutate(manage_stratum =ifelse(manage_stratum =='Plant year 1989', 'ANR2006_south', manage_stratum)) %>% 
  # dplyr::mutate(manage_stratum =ifelse(manage_stratum =='Plant year 1994', 'ANR2006', manage_stratum)) %>% 
  # dplyr::mutate(manage_stratum =ifelse(manage_stratum =='Plant year 1990', 'Normal_PA_1986', manage_stratum)) %>% 
  # dplyr::mutate(manage_stratum =ifelse(manage_stratum =='Plant year 1991', 'Non-strict PA', manage_stratum)) %>% 
  # dplyr::mutate(manage_stratum =ifelse(manage_stratum =='Plant year 1993', 'NR1995', manage_stratum)) %>% 
  dplyr::mutate(manage_group= ifelse(manage_group %in% c("ANR_Plant year 2006"),"ANR_Plant year 2006",manage_group)) %>% 
  dplyr::mutate(manage_group= ifelse(manage_group %in% c("ANR_Plant year 2018","ANR_Plant year 2019",
                                                         "ANR_Plant year 2020"),"ANR_Plant year 2020",manage_group)) %>% 
  dplyr::mutate(manage_group= ifelse(manage_group %in% c("AR_Plant year 2006"),"AR_Plant year 2006",manage_group)) %>% 
  dplyr::mutate(manage_group= ifelse(manage_group %in% c("AR_Plant year 2018","AR_Plant year 2019",
                                                         "AR_Plant year 2020"),"AR_Plant year 2020",manage_group)) %>%
  dplyr::mutate(manage_group= ifelse(manage_group %in% c("AR_Plant year 2017"),"AR_Plant year 2017",manage_group)) 
  # dplyr::mutate(manage_group= ifelse(manage_group %in% c(1993),'NR1995_group',manage_group)) 

tile_agbd_by_manage3$manage_group %>% table()

#--------------------below is for the sodo AR program in 2006-------------------
subset1 <- tile_agbd_by_manage3 %>%
  # dplyr::filter(manage_group %in% c("ANR_Plant year 2020")) %>% 
  dplyr::filter(manage_group %in% c("AR_Plant year 2006")) %>%
  # dplyr::filter(manage_group %in% c("ANR_Plant year 2006","ANR_Plant year 2020")) %>%
  # dplyr::mutate(manage_group = ifelse(manage_group!="ANR_Plant year 2006", "ANR_Plant year 2020",manage_group)) %>% #this is for if you want a completely never treated greoup
  dplyr::mutate(first.treat=readr::parse_number(manage_group)) %>%
  dplyr::select(-c(manage_stratum, manage_group))
# subset1 <- tile_agbd_by_manage3 %>% 
#   dplyr::filter(manage_group %in% c("ANR_Plant year 2006")) %>%    
#   dplyr::mutate(manage_group = 'AR_Plant year 2006',first.treat=readr::parse_number(manage_group)) %>% 
#   # dplyr::mutate(first.treat =ifelse(first.treat %in% c(1986), 0, first.treat)) %>% #this is for if you want a completely never treated greoup
#   dplyr::select(-c(manage_stratum, manage_group)) 

subset1$first.treat %>% table()

subset0 <- tile_agbd_by_manage3 %>% #for the old-growth, too many observations so sample them and recombine
  dplyr::ungroup() %>% 
  # dplyr::filter(manage_stratum %in% c("Plant year 2021")) %>% 
  dplyr::filter(manage_group %in% c("NR_Plant year 1986")) %>%
  dplyr::mutate(first.treat=readr::parse_number(manage_group)) %>% 
  dplyr::mutate(first.treat =ifelse(first.treat %in% c(1986), 0, first.treat)) %>% #this is for if you want a completely never treated greoup
  dplyr::select(-c(manage_stratum, manage_group)) 
# subset0 <-  subset0[sample(nrow(subset0), nrow(subset0)*1), ]

subset <- rbind(subset1,subset0)

agbd_rfmt <- subset %>% 
  dplyr::rename( year= estimat_year) %>% 
  dplyr::mutate(treat = ifelse(year >=first.treat, 1, 0)) %>% 
  dplyr::mutate(intervDur = 2021-first.treat, postintervTime= ifelse(treat ==0, 0, year - first.treat)) %>% 
  data.frame()
agbd_rfmt$first.treat %>% table()

library(zoo)
agbd_rfmt30 <-  agbd_rfmt %>% #agbd_rfmt[agbd_rfmt$loc==32227,]%>% 
  group_by(first.treat) %>% 
  mutate(groupCount = length(unique(loc))) %>% dplyr::ungroup() %>% 
  group_by(loc) %>%
  dplyr::arrange(year) %>% #data.frame() %>% dplyr::select(year, est_agbd)#, DiffV2, cma2)
  mutate( DiffT = year - dplyr::lag(year), DiffT= ifelse(is.na(DiffT), 1, DiffT)) %>% 
  # dplyr::do( data.frame(column = rep(.$year, each = .$DiffT), stringsAsFactors = FALSE) )
  uncount(DiffT) %>% 
  dplyr::mutate(year = dplyr::first(year):dplyr::last(year)) %>%  
  dplyr::arrange(desc(year)) %>% 
  dplyr::mutate(est_agbd= ifelse(duplicated(est_agbd), NA, est_agbd)) %>% 
  dplyr::arrange(year) %>% 
  dplyr::mutate(DiffV = dplyr::first(est_agbd)  - est_agbd, #using 1986 as baselien for change 
                annualR =DiffV/(1986-year),
                DiffV2 = est_agbd  - dplyr::lag(est_agbd) ) %>% #data.frame() %>%  #change from previous year 
  # nv = rep(annualR, each = DiffT))  %>% 
  mutate(cma = rollmean(annualR, k = 2, fill = NA), cma2 = rollmean(DiffV2, k =3, fill = NA),
         siteLength=length(unique(year)), siteNAlen = sum(is.na(cma2)) ) %>%
  # mutate(tma = rollmean(est_agbd, k = 3, fill = NA, align = "right"))
  dplyr::ungroup() %>% 
  data.frame()

# # #thre >=2 gives you 0 day ATT
# agbd_rfmt30[(agbd_rfmt30$year<2006)&(!is.na(agbd_rfmt30$cma2)),]$loc %>% unique()->unique_loc
# agbd_rfmt30[(agbd_rfmt30$year>=2006)&(!is.na(agbd_rfmt30$cma2)),]$loc %>% unique()->unique_loc2
# agbd_rfmt3 <- agbd_rfmt30[(agbd_rfmt30$loc %in% unique_loc2)&(agbd_rfmt30$loc %in% unique_loc),]
agbd_rfmt3 <- agbd_rfmt30 %>% dplyr::filter(siteLength-siteNAlen>=6)#the difference represents at least n needs to have value
# agbd_rfmt3 <- agbd_rfmt3 %>% dplyr::filter(loc %notin% unique_treat)
agbd_rfmt3$first.treat %>% table()

agbd_rfmt3$intervDur <- abs(agbd_rfmt3$intervDur)
agbd_rfmt3$log_cma <- log(agbd_rfmt3$cma)
agbd_rfmt3$log_DiffV2 <- log(agbd_rfmt3$DiffV2)
agbd_rfmt3$log_cma2 <- log(agbd_rfmt3$cma2)
agbd_rfmt3$sqrt_cma2 <- sqrt(agbd_rfmt3$cma2)
agbd_rfmt3$sqrt_precp <-sqrt(agbd_rfmt3$wc_prec_1990.1999)
agbd_rfmt3$log_precp <-log(agbd_rfmt3$wc_prec_1990.1999)
agbd_rfmt3$log_popc <-log(agbd_rfmt3$pop_cnt_2000)
agbd_rfmt3$sqrt_d2roads <-sqrt(agbd_rfmt3$d2roads)


##############try staggered ATT method ---------------------
library(did)
# estimate group-time average treatment effects without covariates
mw.attgt2 <- att_gt(yname = "cma2",
                   gname = "first.treat",
                   idname = "loc",
                   tname = "year",
                   xformla = ~1,
                   # weightsname = 'groupCount',
                   data = agbd_rfmt3,
                   allow_unbalanced_panel = TRUE,
                   control_group ='nevertreated',  
                   print_details = TRUE, panel = FALSE,  pl=T, cores=5
)
# summarize the results
summary(mw.attgt2)

# plot the results# set ylim so that all plots have the same scale along y-axis
ggdid(mw.attgt2)

agg.simple2 <- aggte(mw.attgt2, type = "simple", na.rm=T)  #simple aggregation 
summary(agg.simple2)


agg.gs2 <- aggte(mw.attgt2, type = "group", na.rm=T)     #aggregation by group
summary(agg.gs)
ggdid(agg.gs)

mw.dyn2 <- aggte(mw.attgt2, type = "dynamic", na.rm=T)   #aggregation by length of exposure: group 12 will get dropped
summary(mw.dyn2)
ggdid(mw.dyn2)

# save(mw.attgt2,file = 'ATT_results_save/MGS_AR_2006_group_attObj.Rdata')

#--------------------below is for the post 2012 grousp----------------------------------
subset1 <- tile_agbd_by_manage3 %>%
  # dplyr::filter(manage_group %in% c("ANR_Plant year 2020")) %>% 
  dplyr::filter(manage_group %in% c("AR_Plant year 2017","AR_Plant year 2020")) %>%
  # dplyr::filter(manage_group %in% c("ANR_Plant year 2006","ANR_Plant year 2020")) %>%
  # dplyr::mutate(manage_group = ifelse(manage_group!="ANR_Plant year 2006", "ANR_Plant year 2020",manage_group)) %>% #this is for if you want a completely never treated greoup
  dplyr::mutate(first.treat=readr::parse_number(manage_group)) %>%
  dplyr::select(-c(manage_stratum, manage_group))
# subset1 <- tile_agbd_by_manage3 %>% 
#   dplyr::filter(manage_group %in% c("ANR_Plant year 2006")) %>%    
#   dplyr::mutate(manage_group = 'AR_Plant year 2006',first.treat=readr::parse_number(manage_group)) %>% 
#   # dplyr::mutate(first.treat =ifelse(first.treat %in% c(1986), 0, first.treat)) %>% #this is for if you want a completely never treated greoup
#   dplyr::select(-c(manage_stratum, manage_group)) 

subset1$first.treat %>% table()

subset0 <- tile_agbd_by_manage3 %>% #for the old-growth, too many observations so sample them and recombine
  dplyr::ungroup() %>% 
  # dplyr::filter(manage_stratum %in% c("Plant year 2021")) %>% 
  dplyr::filter(manage_group %in% c("NR_Plant year 1986")) %>%
  dplyr::mutate(first.treat=readr::parse_number(manage_group)) %>% 
  dplyr::mutate(first.treat =ifelse(first.treat %in% c(1986), 0, first.treat)) %>% #this is for if you want a completely never treated greoup
  dplyr::select(-c(manage_stratum, manage_group)) 
# subset0 <-  subset0[sample(nrow(subset0), nrow(subset0)*1), ]

subset <- rbind(subset1,subset0)

agbd_rfmt <- subset %>% 
  dplyr::rename( year= estimat_year) %>% 
  dplyr::mutate(treat = ifelse(year >=first.treat, 1, 0)) %>% 
  dplyr::mutate(intervDur = 2021-first.treat, postintervTime= ifelse(treat ==0, 0, year - first.treat)) %>% 
  data.frame()
agbd_rfmt$first.treat %>% table()

library(zoo)
agbd_rfmt30 <-  agbd_rfmt %>% #agbd_rfmt[agbd_rfmt$loc==32227,]%>% 
  group_by(first.treat) %>% 
  mutate(groupCount = length(unique(loc))) %>% dplyr::ungroup() %>% 
  group_by(loc) %>%
  dplyr::arrange(year) %>% #data.frame() %>% dplyr::select(year, est_agbd)#, DiffV2, cma2)
  mutate( DiffT = year - dplyr::lag(year), DiffT= ifelse(is.na(DiffT), 1, DiffT)) %>% 
  # dplyr::do( data.frame(column = rep(.$year, each = .$DiffT), stringsAsFactors = FALSE) )
  uncount(DiffT) %>% 
  dplyr::mutate(year = dplyr::first(year):dplyr::last(year)) %>%  
  dplyr::arrange(desc(year)) %>% 
  dplyr::mutate(est_agbd= ifelse(duplicated(est_agbd), NA, est_agbd)) %>% 
  dplyr::arrange(year) %>% 
  dplyr::mutate(DiffV = dplyr::first(est_agbd)  - est_agbd, #using 1986 as baselien for change 
                annualR =DiffV/(1986-year),
                DiffV2 = est_agbd  - dplyr::lag(est_agbd) ) %>% #data.frame() %>%  #change from previous year 
  # nv = rep(annualR, each = DiffT))  %>% 
  mutate(cma = rollmean(annualR, k = 2, fill = NA), cma2 = rollmean(DiffV2, k = 2, fill = NA),
         siteLength=length(unique(year)), siteNAlen = sum(is.na(cma2)) ) %>%
  # mutate(tma = rollmean(est_agbd, k = 3, fill = NA, align = "right"))
  dplyr::ungroup() %>% 
  data.frame()

# # #thre >=2 gives you 0 day ATT
# agbd_rfmt30[(agbd_rfmt30$year<2006)&(!is.na(agbd_rfmt30$cma2)),]$loc %>% unique()->unique_loc
# agbd_rfmt30[(agbd_rfmt30$year>=2006)&(!is.na(agbd_rfmt30$cma2)),]$loc %>% unique()->unique_loc2
# agbd_rfmt3 <- agbd_rfmt30[(agbd_rfmt30$loc %in% unique_loc2)&(agbd_rfmt30$loc %in% unique_loc),]
agbd_rfmt3 <- agbd_rfmt30 %>% dplyr::filter(siteLength-siteNAlen>=3)#the difference represents at least n needs to have value
# agbd_rfmt3 <- agbd_rfmt3 %>% dplyr::filter(loc %notin% unique_treat)
agbd_rfmt3$first.treat %>% table()

agbd_rfmt3$intervDur <- abs(agbd_rfmt3$intervDur)
agbd_rfmt3$log_cma <- log(agbd_rfmt3$cma)
agbd_rfmt3$log_DiffV2 <- log(agbd_rfmt3$DiffV2)
agbd_rfmt3$log_cma2 <- log(agbd_rfmt3$cma2)
agbd_rfmt3$sqrt_cma2 <- sqrt(agbd_rfmt3$cma2)
agbd_rfmt3$sqrt_precp <-sqrt(agbd_rfmt3$wc_prec_1990.1999)
agbd_rfmt3$log_precp <-log(agbd_rfmt3$wc_prec_1990.1999)
agbd_rfmt3$log_popc <-log(agbd_rfmt3$pop_cnt_2000)
agbd_rfmt3$sqrt_d2roads <-sqrt(agbd_rfmt3$d2roads)

##############try staggered ATT method ---------------------
library(did)
# estimate group-time average treatment effects without covariates
mw.attgt3 <- att_gt(yname = "cma2",
                   gname = "first.treat",
                   idname = "loc",
                   tname = "year",
                   xformla = ~d2roads+slope,
                   # weightsname = 'groupCount',
                   data = agbd_rfmt3,
                   allow_unbalanced_panel = TRUE,
                   control_group ='nevertreated',  
                   print_details = TRUE, panel = FALSE,  pl=T, cores=5
)
# summarize the results
summary(mw.attgt3)

# plot the results# set ylim so that all plots have the same scale along y-axis
ggdid(mw.attgt3)

agg.simple3 <- aggte(mw.attgt3, type = "simple", na.rm=T)  #simple aggregation 
summary(agg.simple3)


agg.gs3 <- aggte(mw.attgt3, type = "group", na.rm=T)     #aggregation by group
summary(agg.gs3)
ggdid(agg.gs3)

mw.dyn3 <- aggte(mw.attgt3, type = "dynamic", na.rm=T)   #aggregation by length of exposure: group 12 will get dropped
summary(mw.dyn3)
ggdid(mw.dyn3)

save(mw.attgt3,file = 'ATT_results_save/MGS_AR_2017~2020_group_attObj.Rdata')
