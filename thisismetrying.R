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
#############
library(ggplot2)
library(knitr)
library(printr)
library(plyr)
library(dplyr)
library(lubridate)
library(gridExtra)
library(reshape2)
library(TTR)

#####


#drivers of zooplankton 

TB_driver<- read.csv("data/TBCleanSet07212020.csv", stringsAsFactors = F)
TB_driver$sampledate = ymd(TB_driver$sampledate)
TB_driver$year = year(TB_driver$sampledate)
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Synechococcus ')


#finished: Teilingia granulata, Actinella punctata, Tabellaria flocculosa,Tabellaria binalis, Synechocystis, 
#Synechococcus elongatus

#TB_driver.phyto2=subset(TB_driver, taxa_name %in% taxa_name)

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


species.6=c("60800") #next is 60900
#species.2= c("10000","20000","20302", "20601", "20701","30000","30801",
#              "50700","51000", "51100", "51130","51801","60200","60201",
#             "60800","60900","61300","61402","61502","61701",
#             "61702","61800","61801","61803","61804","61805","61806","61810",
#             "61900","61911","61918","62400","62406","63004","63005",
#            "63400", "63600","63602","63612","63616","88888")
#62406 same as, 63004, just translated forward potentially diff mmagnitude 
#61808

#species.2= c("61702", "63004", "62406", "63612", "61806", "60900")
#species.3= c("30000", "61502","61800", "61808", "61918", "63400")
#species.4= c("10000", "20000","20302","20601","50700", "51000", "51130")
#species.5= c("51810", "60200", "60800", "61701", "61801", "63005", "88888")
#species.6= c("30801") #"61918",30801

species.6


species.sub6 = subset(TB_driver.phyto1, species_code %in% species.6)
species.sub6%<>% select(sampledate, biovolume_conc, density) 
#species.sub6$d2= 100*species.sub6$density
#species.sub6$density <- NULL
Synechococcus <- ts(species.sub6)
a<-plot.ts(Synechococcus)


#no intersection with Teilingia granulata-- "61918",30801

#Trachelomonas: no interaction with 20901, 30201,50000,50900, 51101,51103,51105, 51200, 51202, 51800,53200, 60202,
#60203, 60300,60400,60500,61200, 61500, 61501, 61809,61904, 61919,62001, 62005, 62007, 62201, 62407, 62607
#62700, 62902, 63000, 63003, 63603,63700,80000, 80001, 20101-62000


Actinella.punctata







#######
#snow and light
TB_driver1<- read.csv("data/TBCleanSet07212020.csv", stringsAsFactors = F)
TB_driver1$sampledate = ymd(TB_driver1$sampledate)
TB_driver1$year = year(TB_driver1$sampledate)

species.1=c()
species.2= c("61702", "63004", "62406", "63612", "61806", "60900")

species.3= c("30000", "61502","61800", "61808", "61918", "63400")

species.4= c("10000", "20000","20302","20601","50700", "51000", "51130")

species.5= c("51810", "60200", "60800", "61701", "61801", "63005", "88888")

species.6= c("20000") #"61918",30801


species.sub6 = subset(TB_driver1, species_code %in% species.6)
TB_driver.phyto2=subset(TB_driver1, taxa_name== 'Tabellaria flocculosa')
species.sub6%<>% select(avsnow, whiteice, blueice, density) 
#species.sub6$d2= 100*species.sub6$density
#species.sub6$density <- NULL
ts.species.sub6 <- ts(species.sub6)
a<-plot.ts(ts.species.sub6)
a<-labs(title="30801")




##################


TB_driver.phyto1=subset(TB_driver, taxa_name== 'Tabellaria flocculosa')
TB_driver.phyto1$season[is.na(TB_driver.phyto1$season)]=0
#seasonal trends in this taxa

ggplot(species.sub6, aes(sampledate, density))+
  geom_point()+
  #geom_smooth(method='lm')+
  #scale_color_brewer(palette = 'Paired')+
  facet_wrap(~season, scales='free')
  #try layover points with ice and zoops 








