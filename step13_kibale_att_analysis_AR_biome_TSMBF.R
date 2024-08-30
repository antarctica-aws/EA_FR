setwd('/gpfs/data1/duncansongp/amberliang/EA_data')
library(tidyr)
library(ggplot2)
library(ggpubr)
library(raster)
library(rgdal)
pattern <-'no5+beam+2sig_thre_tuned_dcol'
tile_agbd_by_manage <- read.csv(paste("agbd_predictions/kibale_ts_out/agbd_by_planting_px",
                                      pattern,"_ecotrust_covar_ver4.csv", sep=""))

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

#---so far sampling and extraction is done in step13_kibale_att_analysis_envR_extract.R----------------
tile_agbd_by_manage$loc <- paste(tile_agbd_by_manage$x, tile_agbd_by_manage$y, sep="_")
tile_agbd_by_manage_envR <- read.csv(paste("agbd_predictions/kibale_ts_out/icc_agbd_by_planting_px",
                                           pattern,"_ecotrust_ver3_envR2_samp02.csv", sep=""))
tile_agbd_by_manage_envR$loc <- paste(tile_agbd_by_manage_envR$X1, tile_agbd_by_manage_envR$X2, sep="_")

tile_agbd_by_manage_all <-tile_agbd_by_manage_envR %>% dplyr::left_join(tile_agbd_by_manage, by='loc') 
#----------------------------------------------------------------------------------------------------

tile_agbd_by_manage2 <- tile_agbd_by_manage_all %>% #mean time series bymanage startum 
  # dplyr::mutate(manage_stratum =as.character(manage_stratum), 
  #               interv =as.character(interv)) %>% 
  dplyr::mutate(manage_stratum =as.character(manage_stratum)) %>% 
  dplyr::mutate(manage_stratum= sub("^","Plant year ", manage_stratum)) %>% 
  dplyr::mutate(manage_stratum =ifelse(manage_stratum =='Plant year 1989', 'ANR2006_south', manage_stratum)) %>% 
  dplyr::mutate(manage_stratum =ifelse(manage_stratum =='Plant year 1994', 'ANR2006', manage_stratum)) %>% 
  dplyr::mutate(manage_stratum =ifelse(manage_stratum =='Plant year 1990', 'Normal PA', manage_stratum)) %>% 
  dplyr::mutate(manage_stratum =ifelse(manage_stratum =='Plant year 1991', 'Non-strict PA', manage_stratum)) %>% 
  dplyr::mutate(manage_stratum =ifelse(manage_stratum =='Plant year 1993', 'NR1995', manage_stratum)) %>% 
  dplyr::mutate(loc = paste(x,y,sep="_")) %>% 
  dplyr::group_by(manage_stratum, estimat_year) %>% 
  dplyr::summarise(meanAGBD=mean(est_agbd), medianAGBD=median(est_agbd), sd_agbd=sd(est_agbd)) 


tile_agbd_by_manage3 <- tile_agbd_by_manage_all %>%   #regroup
  dplyr::mutate(loc0=loc, loc =as.numeric(factor(loc))) %>% 
  dplyr::mutate(manage_stratum =as.character(manage_stratum), manage_group = manage_stratum) %>% 
  dplyr::mutate(manage_stratum= sub("^","Plant year ", manage_stratum)) %>% 
  dplyr::mutate(manage_stratum =ifelse(manage_stratum =='Plant year 1989', 'ANR2006_south', manage_stratum)) %>% 
  dplyr::mutate(manage_stratum =ifelse(manage_stratum =='Plant year 1994', 'ANR2006', manage_stratum)) %>% 
  dplyr::mutate(manage_stratum =ifelse(manage_stratum =='Plant year 1990', 'Normal_PA_1986', manage_stratum)) %>% 
  dplyr::mutate(manage_stratum =ifelse(manage_stratum =='Plant year 1991', 'Non-strict PA', manage_stratum)) %>% 
  dplyr::mutate(manage_stratum =ifelse(manage_stratum =='Plant year 1993', 'NR1995', manage_stratum)) %>% 
  dplyr::mutate(manage_group= ifelse(manage_group %in% c(1990),'Normal_PA_1986',manage_group)) %>%
  dplyr::mutate(manage_group= ifelse(manage_group %in% c(1994),'ANR2006_group',manage_group)) %>% 
  dplyr::mutate(manage_group= ifelse(manage_group %in% c(1993),'NR1995_group',manage_group)) %>% 
  dplyr::mutate(manage_group= ifelse(manage_group %in% c(1995:1997),'AR1997_group',manage_group)) %>% 
  dplyr::mutate(manage_group= ifelse(manage_group %in% c(1998:2000),'AR2000_group',manage_group)) %>%
  dplyr::mutate(manage_group= ifelse(manage_group %in% c(2001:2003),'AR2003_group',manage_group)) %>%
  dplyr::mutate(manage_group= ifelse(manage_group %in% c(2004:2006),'AR2006_group',manage_group)) %>%
  dplyr::mutate(manage_group= ifelse(manage_group %in% c(2007:2009),'AR2009_group',manage_group)) %>%
  dplyr::mutate(manage_group= ifelse(manage_group %in% c(2010:2012),'AR2012_group',manage_group)) %>% 
  dplyr::mutate(manage_group= ifelse(manage_group %in% c(2013:2015),'AR2015_group',manage_group)) %>%
  dplyr::mutate(manage_group= ifelse(manage_group %in% c(2016:2018),'AR2018_group',manage_group)) %>%
  # dplyr::mutate(manage_group= ifelse(manage_group %in% c(2009:2010),'AR2009_group',manage_group)) %>%
  # dplyr::mutate(manage_group= ifelse(manage_group %in% c(2011:2012),'AR2011_group',manage_group)) %>%
  # dplyr::mutate(manage_group= ifelse(manage_group %in% c(2019:2019),'AR2019_group',manage_group)) %>%
  dplyr::mutate(manage_group= ifelse(manage_group %in% c(2019:2021),'AR2021_group',manage_group))

tile_agbd_by_manage3$manage_group %>% table()  # 19118  


#------------------------------overall analysis-------------------------------------------------

`%notin%` <- Negate(`%in%`)
subset1 <- tile_agbd_by_manage3 %>% 
  dplyr::ungroup() %>% 
  # dplyr::filter(manage_stratum %in% c( 'ANR2006')) %>%
  dplyr::filter(manage_group %in% c('AR1997_group','AR2000_group','AR2003_group','AR2006_group','AR2009_group',
                                    'AR2012_group', 'AR2015_group', 'AR2018_group', 'AR2021_group')) %>%
  # dplyr::filter(manage_stratum %notin% c( "ANR2006_south", "Normal_PA_1986",  "Non-strict PA", "NR1995" , "ANR2006"  )) %>%
  # dplyr::filter(manage_group %in% c('AR1998_group', 'AR2002_group','AR2006_group','AR2010_group','AR2014_group', 'AR2018_group','AR2021_group')) %>%
  dplyr::mutate(first.treat=readr::parse_number(manage_group)) %>% 
  # dplyr::mutate(first.treat =ifelse(first.treat %in% c(1986), 0, first.treat)) %>% #this is for if you want a completely never treated greoup
  dplyr::select(-c(manage_stratum, manage_group)) 

subset0 <- tile_agbd_by_manage3 %>% #for the old-growth, too many observations so sample them and recombine
  dplyr::ungroup() %>% 
  # dplyr::filter(manage_stratum %in% c("Plant year 2021")) %>% 
  dplyr::filter(manage_group %in% c("NR1995_group")) %>%
  dplyr::mutate(first.treat=readr::parse_number(manage_group)) %>% 
  dplyr::mutate(first.treat =ifelse(first.treat %in% c(1995), 0, first.treat)) %>% #this is for if you want a completely never treated greoup
  dplyr::select(-c(manage_stratum, manage_group)) 
subset0 <-  subset0[sample(nrow(subset0), nrow(subset0)*0.5), ]

subset <- rbind(subset1,subset0)

agbd_rfmt <- subset %>% 
  dplyr::rename( year= estimat_year) %>% 
  dplyr::mutate(treat = ifelse(year >=first.treat, 1, 0)) %>% 
  dplyr::mutate(intervDur = 1994-first.treat, postintervTime= ifelse(treat ==0, 0, year - first.treat)) %>% 
  data.frame()
agbd_rfmt$first.treat %>% table()



#######calculate change rate in AGBD###################


library(zoo)
agbd_rfmt30 <-  agbd_rfmt%>% 
  group_by(first.treat) %>% 
  mutate(groupCount = length(unique(loc))) %>% dplyr::ungroup() %>% 
  group_by(loc) %>% dplyr::arrange(year) %>%
  mutate( DiffT = year - dplyr::lag(year), DiffT= ifelse(is.na(DiffT), 1, DiffT)) %>% 
  # dplyr::do( data.frame(column = rep(.$year, each = .$DiffT), stringsAsFactors = FALSE) )
  uncount(DiffT) %>% 
  dplyr::mutate(year = dplyr::first(year):dplyr::last(year),
                est_agbd= ifelse(duplicated(est_agbd), NA, est_agbd),
                DiffV = dplyr::first(est_agbd)  - est_agbd, #using 1986 as baselien for change 
                annualR =DiffV/(1986-year),
                DiffV2 = est_agbd  - dplyr::lag(est_agbd) ) %>% #change from previous year 
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
##***notes: for AR 3 year bins, filter n>=4, groupCount leads to sig overall ATT, no weighting leads to positive but not sig overall ATT; intervDur bias high
#                                      n>=5, groupCount weighting also works but the above is better
# but using the updated loc filter, just use n >=4 and weight by groupCount, sig overall

# estimate group-time average treatment effects without covariates
mw.attgt <- att_gt(yname = "cma2",
                   gname = "first.treat",
                   idname = "loc",
                   tname = "year",
                   xformla = ~1,
                   weightsname = 'groupCount',
                   data = agbd_rfmt3,
                   allow_unbalanced_panel = TRUE,
                   control_group ='nevertreated',  
                   print_details = TRUE, panel = FALSE,  pl=T, cores=5
)
# summarize the results
summary(mw.attgt)

# plot the results# set ylim so that all plots have the same scale along y-axis
ggdid(mw.attgt)

agg.gs <- aggte(mw.attgt, type = "group", na.rm=T)     #aggregation by group
summary(agg.gs)
ggdid(agg.gs)

agg.simple <- aggte(mw.attgt, type = "simple", na.rm=T)  #simple aggregation 
summary(agg.simple)

mw.dyn <- aggte(mw.attgt, type = "dynamic", na.rm=T)   #aggregation by length of exposure: group 12 will get dropped
summary(mw.dyn)
ggdid(mw.dyn)


# save(mw.attgt,file = 'ATT_results_save/TSMBF_AR_1997~2018_group_attObj.Rdata')
