#2010-2015 North sampling (TB and SP data)

# plot with light, ice type, thickness, zoops, chl-a

## Clear workspace and console
#rm(list=ls()); cat("\014")

# Load required packages
#library(FSA); library(dplyr);library(magrittr);library(tidyr) # data management
#library(mgcv);library(nlme); library(lme4) # modeling
#library(viridisLite); library(gridExtra); library(ggplot2) # data viz
#library(lubridate) # dealing with dates
#library(ggpubr); library(fuzzyjoin)
########

#20101

TB_driver<- read.csv("data/TBCleanSet07212020.csv", stringsAsFactors = F)
TB_driver$sampledate = ymd(TB_driver$sampledate)
TB_driver$year = year(TB_driver$sampledate)
TB_driver.phyto1$season[is.na(TB_driver.phyto1$season)]=0
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Synechococcus ')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '20101')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Synechococcus 20101")

ggsave("Synechococcus_20101.png", plot = last_plot(), height = 8, width = 12, units = "in")

##############
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Uroglena ')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '20101')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Uroglena 20101")

ggsave("Uroglena_20101.png", plot = last_plot(), height = 8, width = 12, units = "in")

##################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Stichogloea olivacea')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '20101')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Stichogloea olivacea 20101")

ggsave("Stichogloea_olivacea_20101.png", plot = last_plot(), height = 8, width = 12, units = "in")

############
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Stichococcus bacillaris')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '20101')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Stichococcus bacillaris 20101")

ggsave("Stichococcus_bacillaris_20101.png", plot = last_plot(), height = 8, width = 12, units = "in")

##############

TB_driver.phyto2=subset(TB_driver, taxa_name== 'Stephanodiscus parvus')
TB_driver.phyto3=subset(TB_driver.phyto2, species_code== '20101')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Stephanodiscus parvus 20101")

ggsave("Stephanodiscus_parvus_20101.png", plot = last_plot(), height = 8, width = 12, units = "in")

###################

TB_driver.phyto1=subset(TB_driver, taxa_name== 'Stephanodiscus medius')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '20101')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Stephanodiscus medius 20101")

ggsave("Stephanodiscus_medius_20101.png", plot = last_plot(), height = 8, width = 12, units = "in")

####################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Staurastrum hexacerum')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '20101')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Staurastrum hexacerum 20101")

ggsave("Staurastrum_hexacerum_20101.png", plot = last_plot(), height = 8, width = 12, units = "in")

###################

TB_driver.phyto1=subset(TB_driver, taxa_name== 'Staurastrum ')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '20101')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Staurastrum 20101")

ggsave("Staurastrum_20101.png", plot = last_plot(), height = 8, width = 12, units = "in")

####################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Spinocosmarium ')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '20101')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Spinocosmarium 20101")

ggsave("Spinocosmarium_20101.png", plot = last_plot(), height = 8, width = 12, units = "in")

####################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Sphaerocystis schroeteri')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '20101')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Sphaerocystis schroeteri 20101")

ggsave("Sphaerocystis_schroeteri_20101.png", plot = last_plot(), height = 8, width = 12, units = "in")

####################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Schroederia judayi')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '20101')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Schroederia judayi 20101")

ggsave("Schroederia judayi_20101.png", plot = last_plot(), height = 8, width = 12, units = "in")

####################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Scenedesmus bijuga')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '20101')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Scenedesmus bijuga 20101")

ggsave("Scenedesmus_bijuga_20101.png", plot = last_plot(), height = 8, width = 12, units = "in")

####################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Rhodospirillum rubrum')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '20101')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Rhodospirillum rubrum 20101")

ggsave("Rhodospirillum_rubrum_20101.png", plot = last_plot(), height = 8, width = 12, units = "in")

####################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Rhodomonas minuta')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '20101')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Rhodomonas minuta 20101")

ggsave("Rhodomonas_minuta_20101.png", plot = last_plot(), height = 8, width = 12, units = "in")

####################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Quadrigula lacustris')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '20101')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Quadrigula lacustris 20101")

ggsave("Quadrigula_lacustris_20101.png", plot = last_plot(), height = 8, width = 12, units = "in")

####################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Pseudanabaena limnetica')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '20101')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Pseudanabaena limnetica 20101")

ggsave("Pseudanabaena_limnetica_20101.png", plot = last_plot(), height = 8, width = 12, units = "in")

####################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Planktothrix agardhii')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '20101')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Planktothrix agardhii 20101")

ggsave("Planktothrix_agardhii_20101.png", plot = last_plot(), height = 8, width = 12, units = "in")

####################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Phormidium ')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '20101')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Phormidium 20101")

ggsave("Phormidium_20101.png", plot = last_plot(), height = 8, width = 12, units = "in")

####################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Peridinium umbonatum')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '20101')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Peridinium umbonatum 20101")

ggsave("Peridinium_umbonatum_20101.png", plot = last_plot(), height = 8, width = 12, units = "in")

####################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Peridinium inconspicuum')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '20101')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Peridinium inconspicuum 20101")

ggsave("Peridinium_inconspicuum_20101.png", plot = last_plot(), height = 8, width = 12, units = "in")

####################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Peridinium cinctum')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '20101')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Peridinium cinctum 20101")

ggsave("Peridinium_cinctum_20101.png", plot = last_plot(), height = 8, width = 12, units = "in")

####################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Peridinium ')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '20101')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Peridinium 20101")

ggsave("Peridinium_20101.png", plot = last_plot(), height = 8, width = 12, units = "in")

####################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Oocystis pusilla')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '20101')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Oocystis pusilla 20101")

ggsave("Oocystis_pusilla_20101.png", plot = last_plot(), height = 8, width = 12, units = "in")
####################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Oocystis parva')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '20101')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Oocystis parva 20101")

ggsave("Oocystis_parva_20101.png", plot = last_plot(), height = 8, width = 12, units = "in")

####################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Oedogonium ')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '20101')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Oedogonium 20101")

ggsave("Oedogonium_20101.png", plot = last_plot(), height = 8, width = 12, units = "in")

####################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Mucidosphaerium pulchellum')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '20101')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Mucidosphaerium pulchellum 20101")

ggsave("Mucidosphaerium_pulchellum_20101.png", plot = last_plot(), height = 8, width = 12, units = "in")

####################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Mougeotia ')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '20101')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Mougeotia 20101")

ggsave("Mougeotia_20101.png", plot = last_plot(), height = 8, width = 12, units = "in")

####################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Monoraphidium griffithii')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '20101')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Monoraphidium griffithii 20101")

ggsave("Monoraphidium_griffithii_20101.png", plot = last_plot(), height = 8, width = 12, units = "in")

####################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Monoraphidium arcuatum')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '20101')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Monoraphidium arcuatum 20101")

ggsave("Monoraphidium_arcuatum_20101.png", plot = last_plot(), height = 8, width = 12, units = "in")

####################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Monomastix minuta')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '20101')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Monomastix minuta 20101")

ggsave("Monomastix_minuta_20101.png", plot = last_plot(), height = 8, width = 12, units = "in")

####################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Monomastix astigmata')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '20101')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Monomastix astigmata 20101")

ggsave("Monomastix_astigmata_20101.png", plot = last_plot(), height = 8, width = 12, units = "in")

###################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Miscellaneous')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '20101')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Miscellaneous 20101")

ggsave("Miscellaneous_20101.png", plot = last_plot(), height = 8, width = 12, units = "in")

###################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Microspora ')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '20101')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Microspora 20101")

ggsave("Microspora_20101.png", plot = last_plot(), height = 8, width = 12, units = "in")

###################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Mallomonas akrokomas')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '20101')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Mallomonas akrokomas 20101")

ggsave("Mallomonas_akrokomas_20101.png", plot = last_plot(), height = 8, width = 12, units = "in")

###################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Mallomonas ')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '20101')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Mallomonas 20101")

ggsave("Mallomonas_20101.png", plot = last_plot(), height = 8, width = 12, units = "in")

###################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Lindavia intermedia')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '20101')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Lindavia intermedia 20101")

ggsave("Lindavia_intermedia_20101.png", plot = last_plot(), height = 8, width = 12, units = "in")

###################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Lepocinclis ')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '20101')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Lepocinclis 20101")

ggsave("Lepocinclis_20101.png", plot = last_plot(), height = 8, width = 12, units = "in")
###################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Kephyrion planctonicum')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '20101')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Kephyrion planctonicum 20101")

ggsave("Kephyrion_planctonicum_20101.png", plot = last_plot(), height = 8, width = 12, units = "in")

###################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Kephyrion ')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '20101')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Kephyrion 20101")

ggsave("Kephyrion_20101.png", plot = last_plot(), height = 8, width = 12, units = "in")

###################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Gymnodinium sp. 3')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '20101')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Gymnodinium sp. 3 20101")

ggsave("Gymnodinium_sp_3_20101.png", plot = last_plot(), height = 8, width = 12, units = "in")

###################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Gymnodinium sp. 2')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '20101')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Gymnodinium sp. 2 20101")

ggsave("Gymnodinium_sp_2_20101.png", plot = last_plot(), height = 8, width = 12, units = "in")

###################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Gymnodinium sp. 1')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '20101')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Gymnodinium sp. 1 20101")

ggsave("Gymnodinium_sp_1_20101.png", plot = last_plot(), height = 8, width = 12, units = "in")

###################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Gonyostomum semen')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '20101')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Gonyostomum semen 20101")

ggsave("Gonyostomum_semen_20101.png", plot = last_plot(), height = 8, width = 12, units = "in")

###################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Gloeococcus minor')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '20101')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Gloeococcus minor 20101")

ggsave("Gloeococcus_minor_20101.png", plot = last_plot(), height = 8, width = 12, units = "in")

###################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Glenodinium quadridens')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '20101')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Glenodinium quadridens 20101")

ggsave("Glenodinium_quadridens_20101.png", plot = last_plot(), height = 8, width = 12, units = "in")

###################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Fragilaria filiformis')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '20101')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Fragilaria filiformis 20101")

ggsave("Fragilaria_filiformis_20101.png", plot = last_plot(), height = 8, width = 12, units = "in")

###################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Eunotia ')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '20101')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Eunotia 20101")

ggsave("Eunotia_20101.png", plot = last_plot(), height = 8, width = 12, units = "in")

###################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Euglena acus')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '20101')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Euglena acus 20101")

ggsave("Euglena_acus_20101.png", plot = last_plot(), height = 8, width = 12, units = "in")

###################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Euglena ')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '20101')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Euglena 20101")

ggsave("Euglena_20101.png", plot = last_plot(), height = 8, width = 12, units = "in")

###################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Dinobryon sociale')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '20101')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Dinobryon sociale 20101")

ggsave("Dinobryon_sociale_20101.png", plot = last_plot(), height = 8, width = 12, units = "in")

###################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Dinobryon cyst')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '20101')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Dinobryon cyst 20101")

ggsave("Dinobryon_cyst_20101.png", plot = last_plot(), height = 8, width = 12, units = "in")

###################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Dinobryon cylindricum')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '20101')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Dinobryon cylindricum 20101")

ggsave("Dinobryon_cylindricum_20101.png", plot = last_plot(), height = 8, width = 12, units = "in")

###################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Dinobryon crenulatum')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '20101')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Dinobryon crenulatum 20101")

ggsave("Dinobryon_crenulatum_20101.png", plot = last_plot(), height = 8, width = 12, units = "in")

###################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Dinobryon ')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '20101')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Dinobryon 20101")

ggsave("Dinobryon_20101.png", plot = last_plot(), height = 8, width = 12, units = "in")

###################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Cyclotella sp. 1')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '20101')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Cyclotella sp. 1 20101")

ggsave("Cyclotella_sp_1_20101.png", plot = last_plot(), height = 8, width = 12, units = "in")

###################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Cyclotella ocellata')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '20101')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Cyclotella ocellata 20101")

ggsave("Cyclotella_ocellata_20101.png", plot = last_plot(), height = 8, width = 12, units = "in")

###################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Cryptomonas erosa')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '20101')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Cryptomonas erosa 20101")

ggsave("Cryptomonas_erosa_20101.png", plot = last_plot(), height = 8, width = 12, units = "in")

###################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Cosmarium ')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '20101')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Cosmarium 20101")

ggsave("Cosmarium_20101.png", plot = last_plot(), height = 8, width = 12, units = "in")

###################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Colonial ')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '20101')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Colonial 20101")

ggsave("Colonial_20101.png", plot = last_plot(), height = 8, width = 12, units = "in")

###################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Coelastrum ')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '20101')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Coelastrum  20101")

ggsave("Coelastrum_20101.png", plot = last_plot(), height = 8, width = 12, units = "in")

###################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Chrysophyceae')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '20101')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Chrysophyceae 20101")

ggsave("Chrysophyceae_20101.png", plot = last_plot(), height = 8, width = 12, units = "in")

###################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Chrysococcus minutus')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '20101')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Chrysococcus minutus 20101")

ggsave("Chrysococcus_minutus_20101.png", plot = last_plot(), height = 8, width = 12, units = "in")

###################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Chrysochromulina parva')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '20101')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Chrysochromulina parva 20101")

ggsave("Chrysochromulina_parva_20101.png", plot = last_plot(), height = 8, width = 12, units = "in")

###################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Chrysocapsaceae')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '20101')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Chrysocapsaceae 20101")

ggsave("Chrysocapsaceae_20101.png", plot = last_plot(), height = 8, width = 12, units = "in")

###################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Chlorophyta')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '20101')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Chlorophyta 20101")

ggsave("Chlorophyta_20101.png", plot = last_plot(), height = 8, width = 12, units = "in")

###################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Chlorococcaceae')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '20101')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Chlorococcaceae 20101")

ggsave("Chlorococcaceae_20101.png", plot = last_plot(), height = 8, width = 12, units = "in")

###################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Chlamydomonas ')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '20101')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Chlamydomonas 20101")

ggsave("Chlamydomonas_20101.png", plot = last_plot(), height = 8, width = 12, units = "in")

###################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Botryococcus braunii')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '20101')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Botryococcus braunii 20101")

ggsave("Botryococcus_braunii_20101.png", plot = last_plot(), height = 8, width = 12, units = "in")

###################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Asterionella formosa')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '20101')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Asterionella formosa 20101")

ggsave("Asterionella_formosa_20101.png", plot = last_plot(), height = 8, width = 12, units = "in")

###################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Aphanothece saxicola')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '20101')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Aphanothece saxicola 20101")

ggsave("Aphanothece_saxicola_20101.png", plot = last_plot(), height = 8, width = 12, units = "in")

###################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Aphanothece nidulans')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '20101')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Aphanothece nidulans 20101")

ggsave("Aphanothece_nidulans_20101.png", plot = last_plot(), height = 8, width = 12, units = "in")

###################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Aphanothece ')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '20101')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Aphanothece 20101")

ggsave("Aphanothece_20101.png", plot = last_plot(), height = 8, width = 12, units = "in")

###################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Aphanocapsa delicatissima')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '20101')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Aphanocapsa delicatissima 20101")

ggsave("Aphanocapsa_delicatissima_20101.png", plot = last_plot(), height = 8, width = 12, units = "in")

###################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Aphanizomenon flos-aquae')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '20101')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Aphanizomenon flos-aquae 20101")

ggsave("Aphanizomenon_flos-aquae_20101.png", plot = last_plot(), height = 8, width = 12, units = "in")

###################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Aphanizomenon ')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '20101')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Aphanizomenon 20101")

ggsave("Aphanizomenon_20101.png", plot = last_plot(), height = 8, width = 12, units = "in")

###################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Actinella punctata')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '20101')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Actinella punctata 20101")

ggsave("Actinella_punctata_20101.png", plot = last_plot(), height = 8, width = 12, units = "in")

###################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Total')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '20101')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Total 20101")

ggsave("Total_20101.png", plot = last_plot(), height = 8, width = 12, units = "in")

###################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Teilingia granulata')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '20101')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Teilingia granulata 20101")

ggsave("Teilingia_granulata_20101.png", plot = last_plot(), height = 8, width = 12, units = "in")

###################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Total')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '20101')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Total 20101")

ggsave("Total_20101.png", plot = last_plot(), height = 8, width = 12, units = "in")
