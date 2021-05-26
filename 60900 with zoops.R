#2010-2015 North sampling (TB and SP data)

# plot with light, ice type, thickness, zoops, chl-a

## Clear workspace and console
rm(list=ls()); cat("\014")

# Load required packages
library(FSA); library(dplyr);library(magrittr);library(tidyr) # data management
library(mgcv);library(nlme); library(lme4) # modeling
library(viridisLite); library(gridExtra); library(ggplot2) # data viz
library(lubridate) # dealing with dates
library(ggpubr); library(fuzzyjoin)
########

#60900

TB_driver<- read.csv("data/TBCleanSet07212020.csv", stringsAsFactors = F)
TB_driver$sampledate = ymd(TB_driver$sampledate)
TB_driver$year = year(TB_driver$sampledate)
TB_driver.phyto1$season[is.na(TB_driver.phyto1$season)]=0
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Synechococcus ')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '60900')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Synechococcus 60900")

ggsave("Synechococcus_60900.png", plot = last_plot(), height = 8, width = 12, units = "in")

##############
TB_driver<- read.csv("data/TBCleanSet07212020.csv", stringsAsFactors = F)
TB_driver$sampledate = ymd(TB_driver$sampledate)
TB_driver$year = year(TB_driver$sampledate)
TB_driver.phyto1$season[is.na(TB_driver.phyto1$season)]=0
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Uroglena ')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '60900')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Uroglena 60900")

ggsave("Uroglena_60900.png", plot = last_plot(), height = 8, width = 12, units = "in")

##################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Stichogloea olivacea')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '60900')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Stichogloea olivacea 60900")

ggsave("Stichogloea_olivacea_60900.png", plot = last_plot(), height = 8, width = 12, units = "in")

############
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Stichococcus bacillaris')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '60900')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Stichococcus bacillaris 60900")

ggsave("Stichococcus_bacillaris_60900.png", plot = last_plot(), height = 8, width = 12, units = "in")

##############

TB_driver.phyto2=subset(TB_driver, taxa_name== 'Stephanodiscus parvus')
TB_driver.phyto3=subset(TB_driver.phyto2, species_code== '60900')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Stephanodiscus parvus 60900")

ggsave("Stephanodiscus_parvus_60900.png", plot = last_plot(), height = 8, width = 12, units = "in")

###################

TB_driver.phyto1=subset(TB_driver, taxa_name== 'Stephanodiscus medius')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '60900')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Stephanodiscus medius 60900")

ggsave("Stephanodiscus_medius_60900.png", plot = last_plot(), height = 8, width = 12, units = "in")

####################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Staurastrum hexacerum')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '60900')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Staurastrum hexacerum 60900")

ggsave("Staurastrum_hexacerum_60900.png", plot = last_plot(), height = 8, width = 12, units = "in")

###################

TB_driver.phyto1=subset(TB_driver, taxa_name== 'Staurastrum ')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '60900')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Staurastrum 60900")

ggsave("Staurastrum_60900.png", plot = last_plot(), height = 8, width = 12, units = "in")

####################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Spinocosmarium ')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '60900')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Spinocosmarium 60900")

ggsave("Spinocosmarium_60900.png", plot = last_plot(), height = 8, width = 12, units = "in")

####################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Sphaerocystis schroeteri')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '60900')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Sphaerocystis schroeteri 60900")

ggsave("Sphaerocystis_schroeteri_60900.png", plot = last_plot(), height = 8, width = 12, units = "in")

####################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Schroederia judayi')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '60900')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Schroederia judayi 60900")

ggsave("Schroederia judayi_60900.png", plot = last_plot(), height = 8, width = 12, units = "in")

####################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Scenedesmus bijuga')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '60900')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Scenedesmus bijuga 60900")

ggsave("Scenedesmus_bijuga_60900.png", plot = last_plot(), height = 8, width = 12, units = "in")

####################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Rhodospirillum rubrum')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '60900')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Rhodospirillum rubrum 60900")

ggsave("Rhodospirillum_rubrum_60900.png", plot = last_plot(), height = 8, width = 12, units = "in")

####################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Rhodomonas minuta')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '60900')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Rhodomonas minuta 60900")

ggsave("Rhodomonas_minuta_60900.png", plot = last_plot(), height = 8, width = 12, units = "in")

####################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Quadrigula lacustris')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '60900')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Quadrigula lacustris 60900")

ggsave("Quadrigula_lacustris_60900.png", plot = last_plot(), height = 8, width = 12, units = "in")

####################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Pseudanabaena limnetica')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '60900')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Pseudanabaena limnetica 60900")

ggsave("Pseudanabaena_limnetica_60900.png", plot = last_plot(), height = 8, width = 12, units = "in")

####################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Planktothrix agardhii')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '60900')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Planktothrix agardhii 60900")

ggsave("Planktothrix_agardhii_60900.png", plot = last_plot(), height = 8, width = 12, units = "in")

####################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Phormidium ')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '60900')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Phormidium 60900")

ggsave("Phormidium_60900.png", plot = last_plot(), height = 8, width = 12, units = "in")

####################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Peridinium umbonatum')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '60900')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Peridinium umbonatum 60900")

ggsave("Peridinium_umbonatum_60900.png", plot = last_plot(), height = 8, width = 12, units = "in")

####################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Peridinium inconspicuum')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '60900')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Peridinium inconspicuum 60900")

ggsave("Peridinium_inconspicuum_60900.png", plot = last_plot(), height = 8, width = 12, units = "in")

####################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Peridinium cinctum')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '60900')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Peridinium cinctum 60900")

ggsave("Peridinium_cinctum_60900.png", plot = last_plot(), height = 8, width = 12, units = "in")

####################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Peridinium ')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '60900')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Peridinium 60900")

ggsave("Peridinium_60900.png", plot = last_plot(), height = 8, width = 12, units = "in")

####################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Oocystis pusilla')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '60900')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Oocystis pusilla 60900")

ggsave("Oocystis_pusilla_60900.png", plot = last_plot(), height = 8, width = 12, units = "in")
####################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Oocystis parva')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '60900')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Oocystis parva 60900")

ggsave("Oocystis_parva_60900.png", plot = last_plot(), height = 8, width = 12, units = "in")

####################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Oedogonium ')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '60900')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Oedogonium 60900")

ggsave("Oedogonium_60900.png", plot = last_plot(), height = 8, width = 12, units = "in")

####################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Mucidosphaerium pulchellum')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '60900')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Mucidosphaerium pulchellum 60900")

ggsave("Mucidosphaerium_pulchellum_60900.png", plot = last_plot(), height = 8, width = 12, units = "in")

####################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Mougeotia ')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '60900')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Mougeotia 60900")

ggsave("Mougeotia_60900.png", plot = last_plot(), height = 8, width = 12, units = "in")

####################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Monoraphidium griffithii')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '60900')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Monoraphidium griffithii 60900")

ggsave("Monoraphidium_griffithii_60900.png", plot = last_plot(), height = 8, width = 12, units = "in")

####################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Monoraphidium arcuatum')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '60900')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Monoraphidium arcuatum 60900")

ggsave("Monoraphidium_arcuatum_60900.png", plot = last_plot(), height = 8, width = 12, units = "in")

####################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Monomastix minuta')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '60900')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Monomastix minuta 60900")

ggsave("Monomastix_minuta_60900.png", plot = last_plot(), height = 8, width = 12, units = "in")

####################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Monomastix astigmata')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '60900')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Monomastix astigmata 60900")

ggsave("Monomastix_astigmata_60900.png", plot = last_plot(), height = 8, width = 12, units = "in")

###################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Miscellaneous')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '60900')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Miscellaneous 60900")

ggsave("Miscellaneous_60900.png", plot = last_plot(), height = 8, width = 12, units = "in")

###################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Microspora ')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '60900')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Microspora 60900")

ggsave("Microspora_60900.png", plot = last_plot(), height = 8, width = 12, units = "in")

###################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Mallomonas akrokomas')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '60900')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Mallomonas akrokomas 60900")

ggsave("Mallomonas_akrokomas_60900.png", plot = last_plot(), height = 8, width = 12, units = "in")

###################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Mallomonas ')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '60900')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Mallomonas 60900")

ggsave("Mallomonas_60900.png", plot = last_plot(), height = 8, width = 12, units = "in")

###################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Lindavia intermedia')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '60900')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Lindavia intermedia 60900")

ggsave("Lindavia_intermedia_60900.png", plot = last_plot(), height = 8, width = 12, units = "in")

###################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Lepocinclis ')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '60900')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Lepocinclis 60900")

ggsave("Lepocinclis_60900.png", plot = last_plot(), height = 8, width = 12, units = "in")
###################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Kephyrion planctonicum')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '60900')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Kephyrion planctonicum 60900")

ggsave("Kephyrion_planctonicum_60900.png", plot = last_plot(), height = 8, width = 12, units = "in")

###################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Kephyrion ')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '60900')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Kephyrion 60900")

ggsave("Kephyrion_60900.png", plot = last_plot(), height = 8, width = 12, units = "in")

###################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Gymnodinium sp. 3')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '60900')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Gymnodinium sp. 3 60900")

ggsave("Gymnodinium_sp_3_60900.png", plot = last_plot(), height = 8, width = 12, units = "in")

###################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Gymnodinium sp. 2')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '60900')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Gymnodinium sp. 2 60900")

ggsave("Gymnodinium_sp_2_60900.png", plot = last_plot(), height = 8, width = 12, units = "in")

###################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Gymnodinium sp. 1')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '60900')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Gymnodinium sp. 1 60900")

ggsave("Gymnodinium_sp_1_60900.png", plot = last_plot(), height = 8, width = 12, units = "in")

###################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Gonyostomum semen')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '60900')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Gonyostomum semen 60900")

ggsave("Gonyostomum_semen_60900.png", plot = last_plot(), height = 8, width = 12, units = "in")

###################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Gloeococcus minor')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '60900')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Gloeococcus minor 60900")

ggsave("Gloeococcus_minor_60900.png", plot = last_plot(), height = 8, width = 12, units = "in")

###################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Glenodinium quadridens')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '60900')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Glenodinium quadridens 60900")

ggsave("Glenodinium_quadridens_60900.png", plot = last_plot(), height = 8, width = 12, units = "in")

###################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Fragilaria filiformis')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '60900')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Fragilaria filiformis 60900")

ggsave("Fragilaria_filiformis_60900.png", plot = last_plot(), height = 8, width = 12, units = "in")

###################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Eunotia ')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '60900')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Eunotia 60900")

ggsave("Eunotia_60900.png", plot = last_plot(), height = 8, width = 12, units = "in")

###################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Euglena acus')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '60900')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Euglena acus 60900")

ggsave("Euglena_acus_60900.png", plot = last_plot(), height = 8, width = 12, units = "in")

###################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Euglena ')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '60900')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Euglena 60900")

ggsave("Euglena_60900.png", plot = last_plot(), height = 8, width = 12, units = "in")

###################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Dinobryon sociale')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '60900')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Dinobryon sociale 60900")

ggsave("Dinobryon_sociale_60900.png", plot = last_plot(), height = 8, width = 12, units = "in")

###################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Dinobryon cyst')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '60900')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Dinobryon cyst 60900")

ggsave("Dinobryon_cyst_60900.png", plot = last_plot(), height = 8, width = 12, units = "in")

###################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Dinobryon cylindricum')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '60900')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Dinobryon cylindricum 60900")

ggsave("Dinobryon_cylindricum_60900.png", plot = last_plot(), height = 8, width = 12, units = "in")

###################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Dinobryon crenulatum')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '60900')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Dinobryon crenulatum 60900")

ggsave("Dinobryon_crenulatum_60900.png", plot = last_plot(), height = 8, width = 12, units = "in")

###################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Dinobryon ')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '60900')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Dinobryon 60900")

ggsave("Dinobryon_60900.png", plot = last_plot(), height = 8, width = 12, units = "in")

###################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Cyclotella sp. 1')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '60900')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Cyclotella sp. 1 60900")

ggsave("Cyclotella_sp_1_60900.png", plot = last_plot(), height = 8, width = 12, units = "in")

###################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Cyclotella ocellata')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '60900')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Cyclotella ocellata 60900")

ggsave("Cyclotella_ocellata_60900.png", plot = last_plot(), height = 8, width = 12, units = "in")

###################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Cryptomonas erosa')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '60900')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Cryptomonas erosa 60900")

ggsave("Cryptomonas_erosa_60900.png", plot = last_plot(), height = 8, width = 12, units = "in")

###################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Cosmarium ')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '60900')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Cosmarium 60900")

ggsave("Cosmarium_60900.png", plot = last_plot(), height = 8, width = 12, units = "in")

###################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Colonial ')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '60900')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Colonial 60900")

ggsave("Colonial_60900.png", plot = last_plot(), height = 8, width = 12, units = "in")

###################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Coelastrum ')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '60900')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Coelastrum  60900")

ggsave("Coelastrum_60900.png", plot = last_plot(), height = 8, width = 12, units = "in")

###################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Chrysophyceae')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '60900')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Chrysophyceae 60900")

ggsave("Chrysophyceae_60900.png", plot = last_plot(), height = 8, width = 12, units = "in")

###################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Chrysococcus minutus')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '60900')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Chrysococcus minutus 60900")

ggsave("Chrysococcus_minutus_60900.png", plot = last_plot(), height = 8, width = 12, units = "in")

###################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Chrysochromulina parva')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '60900')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Chrysochromulina parva 60900")

ggsave("Chrysochromulina_parva_60900.png", plot = last_plot(), height = 8, width = 12, units = "in")

###################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Chrysocapsaceae')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '60900')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Chrysocapsaceae 60900")

ggsave("Chrysocapsaceae_60900.png", plot = last_plot(), height = 8, width = 12, units = "in")

###################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Chlorophyta')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '60900')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Chlorophyta 60900")

ggsave("Chlorophyta_60900.png", plot = last_plot(), height = 8, width = 12, units = "in")

###################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Chlorococcaceae')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '60900')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Chlorococcaceae 60900")

ggsave("Chlorococcaceae_60900.png", plot = last_plot(), height = 8, width = 12, units = "in")

###################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Chlamydomonas ')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '60900')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Chlamydomonas 60900")

ggsave("Chlamydomonas_60900.png", plot = last_plot(), height = 8, width = 12, units = "in")

###################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Botryococcus braunii')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '60900')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Botryococcus braunii 60900")

ggsave("Botryococcus_braunii_60900.png", plot = last_plot(), height = 8, width = 12, units = "in")

###################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Asterionella formosa')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '60900')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Asterionella formosa 60900")

ggsave("Asterionella_formosa_60900.png", plot = last_plot(), height = 8, width = 12, units = "in")

###################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Aphanothece saxicola')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '60900')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Aphanothece saxicola 60900")

ggsave("Aphanothece_saxicola_60900.png", plot = last_plot(), height = 8, width = 12, units = "in")

###################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Aphanothece nidulans')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '60900')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Aphanothece nidulans 60900")

ggsave("Aphanothece_nidulans_60900.png", plot = last_plot(), height = 8, width = 12, units = "in")

###################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Aphanothece ')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '60900')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Aphanothece 60900")

ggsave("Aphanothece_60900.png", plot = last_plot(), height = 8, width = 12, units = "in")

###################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Aphanocapsa delicatissima')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '60900')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Aphanocapsa delicatissima 60900")

ggsave("Aphanocapsa_delicatissima_60900.png", plot = last_plot(), height = 8, width = 12, units = "in")

###################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Aphanizomenon flos-aquae')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '60900')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Aphanizomenon flos-aquae 60900")

ggsave("Aphanizomenon_flos-aquae_60900.png", plot = last_plot(), height = 8, width = 12, units = "in")

###################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Aphanizomenon ')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '60900')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Aphanizomenon 60900")

ggsave("Aphanizomenon_60900.png", plot = last_plot(), height = 8, width = 12, units = "in")

###################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Actinella punctata')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '60900')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Actinella punctata 60900")

ggsave("Actinella_punctata_60900.png", plot = last_plot(), height = 8, width = 12, units = "in")

###################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Total')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '60900')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Total 60900")

ggsave("Total_60900.png", plot = last_plot(), height = 8, width = 12, units = "in")

###################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Teilingia granulata')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '60900')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Teilingia granulata 60900")

ggsave("Teilingia_granulata_60900.png", plot = last_plot(), height = 8, width = 12, units = "in")

###################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Total')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '60900')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Total 60900")

ggsave("Total_60900.png", plot = last_plot(), height = 8, width = 12, units = "in")

###################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Monomastix astigmata')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '60900')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Monomastix astigmata 60900")

ggsave("Monomastix_astigmata_60900.png", plot = last_plot(), height = 8, width = 12, units = "in")

###################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Monomastix astigmata')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '60900')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Monomastix astigmata 60900")

ggsave("Monomastix_astigmata_60900.png", plot = last_plot(), height = 8, width = 12, units = "in")

###################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Monomastix astigmata')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '60900')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Monomastix astigmata 60900")

ggsave("Monomastix_astigmata_60900.png", plot = last_plot(), height = 8, width = 12, units = "in")

###################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Monomastix astigmata')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '60900')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Monomastix astigmata 60900")

ggsave("Monomastix_astigmata_60900.png", plot = last_plot(), height = 8, width = 12, units = "in")

###################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Monomastix astigmata')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '60900')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Monomastix astigmata 60900")

ggsave("Monomastix_astigmata_60900.png", plot = last_plot(), height = 8, width = 12, units = "in")

###################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Monomastix astigmata')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '60900')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Monomastix astigmata 60900")

ggsave("Monomastix_astigmata_60900.png", plot = last_plot(), height = 8, width = 12, units = "in")

###################
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Monomastix astigmata')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '60900')

totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Monomastix astigmata 60900")

ggsave("Monomastix_astigmata_60900.png", plot = last_plot(), height = 8, width = 12, units = "in")

