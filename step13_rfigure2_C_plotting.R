#this is for plotting the grouped ATT results 

setwd('/gpfs/data1/duncansongp/amberliang/EA_data')

att <- read.csv('ATT_results_save/ATT_results_group_summary_simp_plotting.csv') %>% dplyr::arrange(desc(group)) %>% 
  dplyr::mutate(att= att*0.47, confidence_lower= confidence_lower*0.47, confidence_higher=confidence_higher*0.47,
    treatment1=2021-group, treatment2= treatment1+2, treatment =paste(treatment1,treatment2,sep="~")) %>% 
  # dplyr::mutate(treatment=paste("restored in", group, sep=" ")) %>% 
  dplyr::mutate(FR =factor(FR, levels=c('NR',  'AR', 'ANR'),
                           labels = c('Natural regeneration (NR)','Active restoration (AR)','Assisted natural regeneration (ANR)')),
                biome=factor(Biome, levels=c('MGS', 'TSGSS', 'TSMBF'),
                             labels = c('Montane shrublands & forests', 
                                        'Savannas & dry forests',
                                        'Tropical moist forests ')))


###########horizontal version###############################
tsmbf_plot <- att %>% dplyr::filter(Biome=='TSMBF') %>% 
  ggplot(., aes(as.factor(treatment), att)) +
  geom_hline(yintercept=0, size=0.4)+
  facet_grid(FR ~ biome, scales = 'free')+
  geom_errorbar(
    aes(ymin = confidence_lower, ymax =confidence_higher, color = before),
    position = position_dodge(0.3), width = 0.2, size=1
  )+
  geom_point(aes(color = before), position = position_dodge(0.3),shape = 18, size=5) +
  scale_color_manual(values = c("#78c679", "#238443"),name='Restortaion time horizons', 
                     labels=c('Sites younger than 9 years','Sites equal or older than 9 years')) +
  coord_flip()+
  theme_bw() + theme(legend.position = "top")+
  theme(strip.background = element_blank(),strip.text.x = element_text(size=14,family = 'serif' ),
        strip.text.y = element_blank(), axis.title=element_blank(),
        axis.text=element_text(size=12, family = 'serif'))+
  theme(legend.text=element_text(size=14,family = 'serif' ),legend.title=element_text(size=14,family = 'serif' ))

tsmbf_plot


mgs_plot <- att %>% dplyr::filter(Biome=='MGS') %>% 
  ggplot(., aes(as.factor(treatment), att)) +
  geom_hline(yintercept=0, size=0.4)+
  facet_grid(FR ~ biome, scales = 'free')+
  geom_errorbar(
    aes(ymin = confidence_lower, ymax =confidence_higher, color = before),
    position = position_dodge(0.3), width = 0.2, size=1
  )+
  geom_point(aes(color = before), position = position_dodge(0.3),shape = 18, size=5) +
  scale_color_manual(values = c("#78c679", "#238443"),name='Restortaion time horizons', 
                     labels=c('Sites younger than 9 years','Sites equal or older than 9 years')) +
  coord_flip()+
  theme_bw() + theme(legend.position = "top")+
  theme(strip.background = element_blank(),strip.text.x = element_text(size=14,family = 'serif' ),
        strip.text.y = element_blank(), axis.title=element_blank(),
        axis.text=element_text(size=12, family = 'serif'))+
  theme(legend.text=element_text(size=14,family = 'serif' ),legend.title=element_text(size=14,family = 'serif'))

mgs_plot


tsgss_plot <- att %>% dplyr::filter(Biome=='TSGSS') %>% 
  ggplot(., aes(as.factor(treatment), att)) +
  geom_hline(yintercept=0, size=0.4)+
  facet_grid(FR ~ biome, scales = 'free')+
  geom_errorbar(
    aes(ymin = confidence_lower, ymax =confidence_higher, color = before),
    position = position_dodge(0.3), width = 0.2, size=1
  )+
  geom_point(aes(color = before), position = position_dodge(0.3),shape = 18, size=5) +
  scale_color_manual(values = c("#78c679", "#238443"),name='Restortaion time horizons', 
                     labels=c('Sites younger than 9 years','Sites equal or older than 9 years')) +
  coord_flip()+
  theme_bw() + theme(legend.position = "top")+
  theme(strip.background.x=element_blank() ,strip.background.y =element_rect(fill="grey", color='white'),strip.text.x = element_text(size=14,family = 'serif' ),
        strip.text.y= element_text(size=16,family = 'serif' ), axis.title=element_blank(),
        axis.text=element_text(size=12, family = 'serif'))+
  theme(legend.text=element_text(size=14,family = 'serif' ),legend.title=element_text(size=14,family = 'serif' ))

tsgss_plot


require(grid)
figure <-ggarrange(tsmbf_plot, mgs_plot, tsgss_plot, 
                   labels = c("A", "B", "C"),
                   ncol = 3, nrow = 1, common.legend = TRUE, legend = "right", 
                   font.label = list(size = 14, color = "black", face = "bold", family = 'serif'))
figure2 <- annotate_figure(figure, left =  text_grob("Restoration start year", rot = 90, vjust = 1, 
                                                     family = 'serif', size = 14),
                           bottom =  text_grob("ATT / Increase in AGB accumulation rate by AR or ANR (Mg/ha/year)",
                                               family = 'serif',  size = 14))

ggsave('Fig/Fig2_group_ATT_v3.png',figure2,width = 6500, height = 2500, units = 'px', dpi=300 )


#-----------vertical panel-----------------

att$treatment <- factor(att$treatment, levels = rev(c("1~3"  , "2~4" ,  "3~5",   "4~6"  , "6~8" ,  "7~9" ,  "8~10",  "9~11" , "12~14" ,"15~17", "16~18", "18~20" ,"21~23", "24~26")))

tsgss_plot <- att %>% dplyr::filter(Biome=='TSGSS') %>% 
  ggplot(., aes(treatment, att)) +
  geom_hline(yintercept=0, size=0.4)+
  facet_grid(biome ~ FR, scales = 'free')+
  geom_errorbar(
    aes(ymin = confidence_lower, ymax =confidence_higher, color = before),
    position = position_dodge(0.3), width = 0.2, size=1
  )+
  geom_point(aes(color = before), position = position_dodge(0.3),shape = 18, size=5) +
  scale_color_manual(values = c("#78c679", "#238443"),name='Restortaion time horizons', 
                     labels=c('Sites younger than 9 years','Sites equal or older than 9 years')) +
  coord_flip()+
  theme_bw() + theme(legend.position = "top")+
  theme(strip.background.x=element_blank() ,strip.background.y =element_rect(fill="grey", color='white'),
        strip.text.y = element_text(size=18,family = 'serif' ),strip.text.x=element_blank(), axis.title=element_blank(),
        axis.text=element_text(size=14, family = 'serif'))+
  theme(legend.text=element_text(size=16,family = 'serif' ),legend.title=element_text(size=16,family = 'serif' ))+
  ylim(-8, 8)

tsgss_plot

tsmbf_plot <- att %>% dplyr::filter(Biome=='TSMBF') %>% 
  ggplot(., aes(treatment, att)) +
  geom_hline(yintercept=0, size=0.4)+
  facet_grid(biome ~ FR, scales = 'free')+
  geom_errorbar(
    aes(ymin = confidence_lower, ymax =confidence_higher, color = before),
    position = position_dodge(0.3), width = 0.2, size=1
  )+
  geom_point(aes(color = before), position = position_dodge(0.3),shape = 18, size=5) +
  scale_color_manual(values = c("#78c679", "#238443"),name='Restortaion time horizons', 
                     labels=c('Sites younger than 9 years','Sites equal or older than 9 years')) +
  coord_flip()+
  theme_bw() + theme(legend.position = "top")+
  theme(strip.background.x=element_blank() ,strip.background.y =element_rect(fill="grey", color='white'),
        strip.text.y = element_text(size=18,family = 'serif' ),
        strip.text.x= element_text(size=18,family = 'serif' ), axis.title=element_blank(),
        axis.text=element_text(size=14, family = 'serif'))+
  theme(legend.text=element_text(size=16,family = 'serif' ),legend.title=element_text(size=16,family = 'serif' ))+
  ylim(-15, 15)

tsmbf_plot


mgs_plot <- att %>% dplyr::filter(Biome=='MGS') %>% 
  ggplot(., aes(treatment, att)) +
  geom_hline(yintercept=0, size=0.4)+
  facet_grid( biome ~ FR, scales = 'free')+
  geom_errorbar(
    aes(ymin = confidence_lower, ymax =confidence_higher, color = before),
    position = position_dodge(0.3), width = 0.2, size=1
  )+
  geom_point(aes(color = before), position = position_dodge(0.3),shape = 18, size=5) +
  scale_color_manual(values = c("#78c679", "#238443"),name='Restortaion time horizons', 
                     labels=c('Sites younger than 9 years','Sites equal or older than 9 years')) +
  coord_flip()+
  theme_bw() + theme(legend.position = "top")+
  theme(strip.background.x=element_blank() ,strip.background.y =element_rect(fill="grey", color='white'),
        strip.text.y = element_text(size=18,family = 'serif' ),
        strip.text.x = element_blank(), axis.title=element_blank(),
        axis.text=element_text(size=14, family = 'serif'))+
  theme(legend.text=element_text(size=16,family = 'serif' ),legend.title=element_text(size=16,family = 'serif'))+
  ylim(-10, 10)

mgs_plot


tag_facet <- function(p, open = "", close = ")", tag_pool = letters, x = -Inf, y = Inf, 
                      hjust = -0.5, vjust = 1.5, fontface = 1, family = "serif", ...) {
  
  gb <- ggplot_build(p)
  lay <- gb$layout$layout
  tags <- cbind(lay, label = paste0(open, tag_pool[lay$PANEL], close), x = x, y = y)
  p + geom_text(data = tags, aes_string(x = "x", y = "y", label = "label"), ..., hjust = hjust, 
                vjust = vjust, fontface = fontface, family = family,size=10, inherit.aes = FALSE) 
}

mgs_plot2 <- tag_facet(mgs_plot, x = Inf, y = -9, 
          hjust = 1,
          tag_pool = c('e','f'))
tsgss_plot2 <- tag_facet(tsgss_plot, x = Inf, y = -7, 
                       hjust = 1,
                       tag_pool = c('c','d'))
tsmbf_plot2 <- tag_facet(tsmbf_plot, x = Inf, y = -13.5, 
                       hjust = 1,
                       tag_pool = c('a','b'))



figureB <-ggpubr::ggarrange(tsmbf_plot2,  tsgss_plot2, mgs_plot2,
                    ncol = 1, nrow = 3, common.legend = TRUE, legend = "bottom", 
                    font.label = list(size = 14, color = "black", face = "bold", family = 'serif'))


figure2B <- annotate_figure(figureB, left =  text_grob("Years since restortaion", rot = 90, vjust = 0.5, 
                                                       family = 'serif', size = 18),
                            bottom =  text_grob(expression(paste("ATT (Increase in AGC accumulation rate by active interventions, Mg C ", ha^{-1}, year^{-1},')')),
                                               family = 'serif',  size = 18))

figure2B


ggsave('Fig/Fig2_group_ATT_vertical_C_v2.png',figure2B,height =4800, width= 2600, units = 'px', dpi=300 )


