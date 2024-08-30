###this script analyze the ATT for ANR in comparison to NR for the TSGSS biome

setwd('/gpfs/data1/duncansongp/amberliang/EA_data')
library(tidyr)
library(ggplot2)
library(ggpubr)
library(raster)
library(rgdal)
`%notin%` <- Negate(`%in%`)
#-------combine AGBD predictions wfor NR, ANR, and AR sites-----#

pattern <- 'shinyanga2+1sig_sen96_nbr'
tile <- '171063'
tile_agbd_by_manage_shinwest <- read.csv( paste("agbd_predictions/shinyanga1_ts_out/agbd_collrm_by_planting_px",
                                                pattern,"_HASHI_west_covar_v2.csv", sep="")) %>% 
  dplyr::filter(polyID %in% c('poly_2')) %>% 
  dplyr::select(X,x,y, est_agbd, d2roads, pop_cnt_2000, slope,  dem, wc_prec_1990.1999, wc_tavg_1990.1999, soilType,
                manage_stratum, interv, interv2, estimat_year, ras_sub)

# pattern <- 'dodoma2+2sig_sen97_beam+sol_nbr'  #unless for treated after 2012 or before 1991, not useful
# tile_agbd_by_manage_dodoma0 <- read.csv( paste("agbd_predictions/dodoma1_ts_out/agbd_collrm_by_planting_px",
#                                                pattern,"_tsgss_dodomaPoly4_lite_ANR_covar.csv", sep="")) 
# tile_agbd_by_manage_dodoma <- tile_agbd_by_manage_dodoma0 %>%   
#   dplyr::mutate(manage_stratum=2015) %>%
#   dplyr::select(X,x,y, est_agbd, d2roads, pop_cnt_2000, slope,  dem, wc_prec_1990.1999, wc_tavg_1990.1999, soilType,
#                 manage_stratum, interv, interv2, estimat_year, ras_sub)
# 
# tile_agbd_by_manage_dodoma$loc <- paste(tile_agbd_by_manage_dodoma$x, tile_agbd_by_manage_dodoma$y, sep="_")
# 
# sloc <- sample(unique(tile_agbd_by_manage_dodoma$loc),length(unique(tile_agbd_by_manage_dodoma$loc))*0.02)
# tile_agbd_by_manage_dodoma <-  tile_agbd_by_manage_dodoma[tile_agbd_by_manage_dodoma$loc %in% sloc,]
# tile_agbd_by_manage_dodoma <- tile_agbd_by_manage_dodoma[,!(names(tile_agbd_by_manage_dodoma) %in% c('loc'))]
# # write.csv(tile_agbd_by_manage_dodoma, paste("agbd_predictions/dodoma1_ts_out/agbd_collrm_by_planting_px",
# #                                             pattern,"_tsgss_dodomaPoly4_lite_ANR_covar.csv", sep=""))

pattern <- 'beam+2sig_no5_Sept_thre_tuned_dcol_nbr'  #unless for treated after 2012 or before 1991, not useful
tile_agbd_by_manage_bushenyi <- read.csv( paste("agbd_predictions/AMF_sites_df/agbd_by_planting_px",
                                                pattern,"_bushenyi_tsgss_ANR_covar.csv", sep="")) %>%
  dplyr::mutate(interv='exotrust',interv2='ANR') %>%
  dplyr::select(X,x,y, est_agbd, d2roads, pop_cnt_2000, slope,  dem, wc_prec_1990.1999, wc_tavg_1990.1999, soilType,
                manage_stratum, interv, interv2, estimat_year, ras_sub)


pattern <- 'dodoma2+2sig_sen97_beam+sol_nbr'
tile <- '168064'
tile_agbd_by_manage_dodoma <- read.csv(paste("agbd_predictions/dodoma1_ts_out/agbd_collrm_by_planting_px",
                                             pattern,"_tsgss_dodomaPlots_ANR_covar.csv", sep="")) %>% 
  dplyr::mutate(interv='dodoma_plots',interv2='ANR') %>%
  dplyr::select(X,x,y, est_agbd, d2roads, pop_cnt_2000, slope,  dem, wc_prec_1990.1999, wc_tavg_1990.1999, soilType,
                manage_stratum, interv, interv2, estimat_year, ras_sub)

#combine different projects
tile_agbd_by_manage <- rbind(tile_agbd_by_manage_shinwest, tile_agbd_by_manage_dodoma, tile_agbd_by_manage_bushenyi)

tile_agbd_by_manage$loc <- paste(tile_agbd_by_manage$x, tile_agbd_by_manage$y, sep="_")

print(tile_agbd_by_manage$loc %>% table() %>% unique())
print(tile_agbd_by_manage$interv2 %>% table())

#clean out the dplicates in the time series for each loc
tile_agbd_by_manage_t <- tile_agbd_by_manage %>% dplyr::group_by(loc) %>% 
  dplyr::arrange(estimat_year, -est_agbd) %>%
  dplyr::filter(!duplicated(estimat_year)) %>% dplyr::ungroup()
print(tile_agbd_by_manage_t$loc %>% table() %>% unique())
print(tile_agbd_by_manage_t$interv2 %>% table())

set.seed(1994)

#sample each year equally
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

tile_agbd_by_manage_all <- stratified(tile_agbd_by_manage_t, "manage_stratum",0.5)


######calcualte the mean and median AAGBD by managemnt year and plot the time series of mean and median change
# tile_agbd_by_manage_all <- tile_agbd_by_manage_t
tile_agbd_by_manage_all$manage_stratum[tile_agbd_by_manage_all$interv2=='NR'] <- 1986
tile_agbd_by_manage_all$interv <- as.character(tile_agbd_by_manage_all$interv)

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
  theme_bw()+theme(legend.position = c(0.9, 300)) +ylim(2,300)
agbd_ts_plot


######
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
  dplyr::mutate(manage_group= ifelse(manage_group %in% c("ANR_Plant year 2002","ANR_Plant year 2004","ANR_Plant year 2005"),"ANR_Plant year 2005",manage_group)) %>%
  dplyr::mutate(manage_group= ifelse(manage_group %in% c("ANR_Plant year 2006","ANR_Plant year 2008","ANR_Plant year 2009"),"ANR_Plant year 2009",manage_group)) %>%
  dplyr::mutate(manage_group= ifelse(manage_group %in% c("ANR_Plant year 2010","ANR_Plant year 2011","ANR_Plant year 2012"),"ANR_Plant year 2012",manage_group)) %>%
  dplyr::mutate(manage_group= ifelse(manage_group %in% c("ANR_Plant year 2015"),"ANR_Plant year 2015",manage_group)) %>%
  dplyr::mutate(manage_group= ifelse(manage_group %in% c("ANR_Plant year 2017", "ANR_Plant year 2018","ANR_Plant year 2019"),"ANR_Plant year 2019",manage_group)) %>%
  dplyr::mutate(manage_group= ifelse(manage_group %in% c("AR_Plant year 2010","AR_Plant year 2011","AR_Plant year 2012"),"AR_Plant year 2012",manage_group)) %>%
  dplyr::mutate(manage_group= ifelse(manage_group %in% c("AR_Plant year 2013", "AR_Plant year 2014"),"AR_Plant year 2014",manage_group)) %>% 
  dplyr::mutate(manage_group= ifelse(manage_group %in% c("AR_Plant year 2018","AR_Plant year 2019"),"AR_Plant year 2019",manage_group)) 
# dplyr::mutate(manage_group= ifelse(manage_group %in% c(1993),'NR1995_group',manage_group)) 

tile_agbd_by_manage3$manage_group %>% table()


subset1 <- tile_agbd_by_manage3 %>%
  # dplyr::filter(manage_group %notin% c("NR_Plant year 1986", "AR_Plant year 2014")) %>%
  # dplyr::filter(manage_group %in% c("AR_Plant year 2013")) %>%
  dplyr::filter(manage_group %in% c("ANR_Plant year 2005", "ANR_Plant year 2009", "ANR_Plant year 2012","ANR_Plant year 2015","ANR_Plant year 2019")) %>%
  # dplyr::mutate(manage_group = ifelse(manage_group!="ANR_Plant year 2006", "ANR_Plant year 2020",manage_group)) %>% #this is for if you want a completely never treated greoup
  dplyr::mutate(first.treat=readr::parse_number(manage_group)) %>%
  dplyr::select(-c(manage_stratum, manage_group))
# subset1 <- tile_agbd_by_manage3 %>% 
#   dplyr::filter(manage_group %in% c("ANR_Plant year 2006")) %>%    
#   dplyr::mutate(manage_group = 'AR_Plant year 2006',first.treat=readr::parse_number(manage_group)) %>% 
#   # dplyr::mutate(first.treat =ifelse(first.treat %in% c(1986), 0, first.treat)) %>% #this is for if you want a completely never treated greoup
#   dplyr::select(-c(manage_stratum, manage_group)) 
# #sample the loc column 
sloc <- sample(unique(subset1$loc),length(unique(subset1$loc))*1)
subset1 <-  subset1[subset1$loc %in% sloc,]
subset1$first.treat %>% table()

subset0 <- tile_agbd_by_manage3 %>% #for the old-growth, too many observations so sample them and recombine
  dplyr::ungroup() %>% 
  # dplyr::filter(manage_stratum %in% c("Plant year 2021")) %>% 
  dplyr::filter(manage_group %in% c("NR_Plant year 1986")) %>%
  dplyr::mutate(first.treat=readr::parse_number(manage_group)) %>% 
  dplyr::mutate(first.treat =ifelse(first.treat %in% c(1986, 2012), 0, first.treat)) %>% #this is for if you want a completely never treated greoup
  dplyr::select(-c(manage_stratum, manage_group)) 
# #sample the loc column 
sloc <- sample(unique(subset0$loc),length(unique(subset0$loc))*0.02)
subset00 <-  subset0[subset0$loc %in% sloc,]
# # subset0 <-  subset0[sample(nrow(subset0), nrow(subset0)*0.3), ]
# subset00 <-  subset0
subset <- rbind(subset1,subset00)

agbd_rfmt <- subset %>% 
  dplyr::rename( year= estimat_year) %>% 
  dplyr::mutate(treat = ifelse(year >=first.treat, 1, 0)) %>% 
  dplyr::mutate(intervDur = 2021-first.treat, postintervTime= ifelse(treat ==0, 0, year - first.treat)) %>% 
  data.frame()
agbd_rfmt$first.treat %>% table()



#######calculate change rate in AGBD###################


library(zoo)
agbd_rfmt30 <-  agbd_rfmt%>% 
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
                DiffV2 = est_agbd  - dplyr::lag(est_agbd) ) %>%#data.frame() %>%  #change from previous year 
  # nv = rep(annualR, each = DiffT))  %>% 
  mutate(cma = rollmean(annualR, k = 2, fill = NA), cma2 = rollmean(DiffV2, k = 2, fill = NA),
         siteLength=length(unique(year)), siteNAlen = sum(is.na(cma2)) ) %>%
  # mutate(tma = rollmean(est_agbd, k = 3, fill = NA, align = "right"))
  dplyr::ungroup() %>% 
  data.frame() #%>% dplyr::select(year, est_agbd, DiffV2, cma2)

# # #thre >=2 gives you 0 day ATT
# agbd_rfmt30[(agbd_rfmt30$year<2006)&(!is.na(agbd_rfmt30$cma2)),]$loc %>% unique()->unique_loc
# agbd_rfmt30[(agbd_rfmt30$year>=2006)&(!is.na(agbd_rfmt30$cma2)),]$loc %>% unique()->unique_loc2
# agbd_rfmt3 <- agbd_rfmt30[(agbd_rfmt30$loc %in% unique_loc2)&(agbd_rfmt30$loc %in% unique_loc),]
agbd_rfmt3 <- agbd_rfmt30 %>% dplyr::filter(siteLength-siteNAlen>=4)#the difference represents at least n needs to have value
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
mw.attgt <- att_gt(yname = "cma2",
                   gname = "first.treat",
                   idname = "loc",
                   tname = "year",
                   xformla = ~1,
                   # weightsname = 'intervDur',
                   data = agbd_rfmt3,
                   allow_unbalanced_panel = TRUE,
                   control_group ='nevertreated',  
                   # bstrap = FALSE,
                   print_details = TRUE, panel = FALSE,  pl=T, cores=5
)
# summarize the results
summary(mw.attgt)

# plot the results# set ylim so that all plots have the same scale along y-axis
ggdid(mw.attgt)

agg.simple <- aggte(mw.attgt, type = "simple", na.rm=T)  #simple aggregation 
summary(agg.simple)

agg.gs <- aggte(mw.attgt, type = "group", na.rm=T)     #aggregation by group
summary(agg.gs)
ggdid(agg.gs)

mw.dyn <- aggte(mw.attgt, type = "dynamic", na.rm=T)   #aggregation by length of exposure: group 12 will get dropped
summary(mw.dyn)
ggdid(mw.dyn)

# save(mw.attgt,file = 'ATT_results_save/TSGSS_ANR_2005~2019_group_attObj.Rdata')
