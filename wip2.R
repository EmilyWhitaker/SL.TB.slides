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
#species.6=c("60800") #next is 60900
#species.2= c("10000","20000","20302", "20601", "20701","30000","30801",
#              "50700","51000", "51100", "51130","51801","60200","60201",
#             "60800","60900","61300","61402","61502","61701",
#             "61702","61800","61801","61803","61804","61805","61806","61810",
#             "61900","61911","61918","62400","62406","63004","63005",
#            "63400", "63600","63602","63612","63616","88888")
#62406 63004, #61808 20901, 30201,50000,50900, 51101,51103,51105, 51200, 51202, 51800,53200, 60202,
#60203, 60300,60400,60500,61200, 61500, 61501, 61809,61904, 61919,62001, 62005, 62007, 62201, 62407, 62607
#62700, 62902, 63000, 63003, 63603,63700,80000, 80001, 20101-62000

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

ggsave("Synechococcus_60800.png", plot = last_plot(), height = 8, width = 12, units = "in")

TB_driver<- read.csv("data/TBCleanSet07212020.csv", stringsAsFactors = F)
TB_driver$sampledate = ymd(TB_driver$sampledate)
TB_driver$year = year(TB_driver$sampledate)
TB_driver.phyto1$season[is.na(TB_driver.phyto1$season)]=0
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Synechococcus ')
TB_driver.phyto3=subset(TB_driver.phyto1, species_code== '10000')

totals.long = pivot_longer(TB_driver.phyto3, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Synechococcus 10000")

ggsave("Synechococcus_10000.png", plot = last_plot(), height = 8, width = 12, units = "in")


TB_driver<- read.csv("data/TBCleanSet07212020.csv", stringsAsFactors = F)
TB_driver$sampledate = ymd(TB_driver$sampledate)
TB_driver$year = year(TB_driver$sampledate)
TB_driver.phyto1$season[is.na(TB_driver.phyto1$season)]=0
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Synechococcus ')
TB_driver.phyto4=subset(TB_driver.phyto1, species_code== '20000')

totals.long = pivot_longer(TB_driver.phyto4, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Synechococcus 20000")

ggsave("Synechococcus_20000.png", plot = last_plot(), height = 8, width = 12, units = "in")



TB_driver<- read.csv("data/TBCleanSet07212020.csv", stringsAsFactors = F)
TB_driver$sampledate = ymd(TB_driver$sampledate)
TB_driver$year = year(TB_driver$sampledate)
TB_driver.phyto1$season[is.na(TB_driver.phyto1$season)]=0
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Synechococcus ')
TB_driver.phyto5=subset(TB_driver.phyto1, species_code== '20302')

totals.long = pivot_longer(TB_driver.phyto5, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Synechococcus 20000")

ggsave("Synechococcus_20000.png", plot = last_plot(), height = 8, width = 12, units = "in")




"10000","20000","20302", "20601", "20701","30000","30801",
#              "50700","51000", "51100", "51130","51801","60200","60201",
#             "60800","60900","61300","61402","61502","61701",
#             "61702","61800","61801","61803","61804","61805","61806","61810",
#             "61900","61911","61918","62400","62406","63004","63005",
#            "63400", "63600","63602","63612","63616","88888")
#62406 63004, #61808 20901, 30201,50000,50900, 51101,51103,51105, 51200, 51202, 51800,53200, 60202,
#60203, 60300,60400,60500,61200, 61500, 61501, 61809,61904, 61919,62001, 62005, 62007, 62201, 62407, 62607
#62700, 62902, 63000, 63003, 63603,63700,80000, 80001, 20101-62000



TB_driver<- read.csv("data/TBCleanSet07212020.csv", stringsAsFactors = F)
TB_driver$sampledate = ymd(TB_driver$sampledate)
TB_driver$year = year(TB_driver$sampledate)
TB_driver.species=subset(TB_driver, species_code== '61808')

sum(TB_driver.species$density>0)

#taxa_name=c('Uroglena', 'Trachelomonas ', 'Teilingia granulata', 'Tabellaria flocculosa', 'Tabellaria binalis',
#            'Synechocystis ' ,'Synechococcus elongatus', 'Synechococcus ','Stichogloea olivacea','Stichococcus bacillaris',
#           'Stephanodiscus parvus','Stephanodiscus medius','Staurastrum hexacerum','Staurastrum', 'Spinocosmarium',
#            'Sphaerocystis schroeteri','Schroederia judayi','Scenedesmus bijuga','Rhodospirillum rubrum','Rhodomonas minuta',
#            'Quadrigula lacustris','Pseudanabaena limnetica','Planktothrix agardhii','Phormidium ','Peridinium umbonatum',
#            'Peridinium inconspicuum','Peridinium cinctum','Peridinium ','Oocystis pusilla','Oocystis parva','Oedogonium ',
#           'Mucidosphaerium pulchellum','Mougeotia ','Monoraphidium griffithii','Monoraphidium arcuatum','Monomastix minuta',
#            'Monomastix astigmata','Miscellaneous','Microspora ','Mallomonas akrokomas','Mallomonas', 'Lindavia intermedia',
#            'Lepocinclis', 'Kephyrion planctonicum', 'Kephyrion','Gymnodinium sp. 3', 'Gymnodinium sp. 2','Gymnodinium sp. 1',
#            'Gonyostomum semen','Gloeococcus minor','Glenodinium quadridens','Fragilaria filiformis', 'Eunotia','Euglena acus',
#            'Euglena', 'Dinobryon sociale', 'Dinobryon cyst', 'Dinobryon cylindricum', 'Dinobryon crenulatum','Dinobryon',
#            'Cyclotella sp. 1','Cyclotella ocellata', 'Cryptomonas erosa','Cosmarium', 'Colonial', 'Coelastrum', 'Chrysophyceae',
#            'Chrysococcus minutus', 'Chrysochromulina parva', 'Chrysocapsaceae','Chlorophyta','Chlorococcaceae', 'Chlamydomonas',
#            'Botryococcus braunii', 'Asterionella formosa','Aphanothece saxicola','Aphanothece nidulans', 'Aphanothece',
#            'Aphanocapsa delicatissima','Aphanizomenon flos-aquae', 'Aphanizomenon', 'Actinella punctata')
#'Total'
#'
TB_driver.species=subset(TB_driver, taxa_name== 'Total')



TB_driver<- read.csv("data/TBCleanSet07212020.csv", stringsAsFactors = F)
TB_driver$sampledate = ymd(TB_driver$sampledate)
TB_driver$year = year(TB_driver$sampledate)
TB_driver$season[is.na(TB_driver$season)]=0
TB_driver.species=subset(TB_driver, species_code== '20601')
sum(TB_driver.species$density>0)

TB_driver.species %>% distinct(TBZoopDate, .keep_all = TRUE)

ggplot(TB_driver.species, aes(TB_driver.species$TBZoopDate, TB_driver.species$density))+
  geom_point(aes(colour=biovolume_conc))+
  #geom_smooth(method = 'lm')+
  #geom_line()+  
  labs(title="")+
  theme_classic()+
  labs(x='Sample Date', y= 'Density')+
  facet_wrap('taxa_name', scales = 'free')+
  #facet_wrap('season')+
  labs(title="Trout Bog 2008-2018 20601 Zoop Density")

#ggsave("10000_ZoopDensity.png", plot = last_plot(), height = 8, width = 12, units = "in")


TB_driver.species=select(filter(taxa_name == 'Rhodospirillum rubrum'))


TB_driver.species=subset(TB_driver, species_code== '20601')
sum(TB_driver.species$density>0)

ggplot(TB_driver.species, aes(TB_driver.species$TBZoopDate, TB_driver.species$density, color = biovolume_conc))+
  geom_point()+
  #geom_smooth(method = 'lm')+
  #geom_line()+  
  labs(title="")+
  theme_classic()+
  labs(x='Sample Date', y= 'Density')+
  facet_wrap('taxa_name', scales = 'free')+
  #facet_wrap('season')+
  labs(title="Trout Bog 2008-2018 20601 Zoop Density")

TB_driver.species=subset(TB_driver, species_code== '50700')

ggplot(TB_driver.species, aes(TB_driver.species$TBZoopDate, TB_driver.species$density, color = biovolume_conc))+
  geom_point()+
  #geom_smooth(method = 'lm')+
  #geom_line()+  
  labs(title="")+
  theme_classic()+
  labs(x='Sample Date', y= 'Density')+
  facet_wrap('taxa_name', scales = 'free')+
  #facet_wrap('season')+
  labs(title="Trout Bog 2008-2018 50700 Zoop Density")

ggplot(TB_driver.species, aes(TB_driver.species$TBZoopDate, TB_driver.species$density))+
  geom_point()+
  #geom_smooth(method = 'lm')+
  #geom_line()+  
  labs(title="")+
  theme_classic()+
  labs(x='Sample Date', y= 'Density')+
  #facet_wrap('taxa_name', scales = 'free')+
  facet_wrap('season')+
  labs(title="Trout Bog 2008-2018 50700 Zoop Density")



TB_driver_2<- read.csv("data/TBCleanSet07212020.csv", stringsAsFactors = F, na = "empty")
TB_driver_2$sampledate = ymd(TB_driver_2$sampledate)
TB_driver_2$year = year(TB_driver_2$sampledate)
TB_driver_2$season[is.na(TB_driver_2$season)]=0
TB_driver.species2=subset(TB_driver_2, species_code== '20601')

TB_driver.species[TB_driver.species$taxa_name=='NA'] <- NA
#TB_driver.species.1=filter(TB_driver.species, taxa_name=| "Rhodospirillum rubrum", "NA")

TB_driver.species.2=subset(TB_driver.species2, taxa_name== "Rhodospirillum rubrum" | taxa_name== "NA")

ggplot(TB_driver.species.2, aes(TB_driver.species.2$TBZoopDate, TB_driver.species.2$density, color = TB_driver.species.2$biovolume_conc))+
  geom_point()+
  #geom_smooth(method = 'lm')+
  #geom_line(aes('taxa_name'))+  
  labs(title="")+
  theme_classic()+
  labs(x='Sample Date', y= 'Density')+
  #facet_wrap('taxa_name', scales = 'free')+
  #facet_wrap('season')+
  labs(title="Trout Bog 2008-2018 50700 Zoop Density")

