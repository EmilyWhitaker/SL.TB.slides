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

##################
TB_driver<- read.csv("data/TBCleanSet07212020.csv", stringsAsFactors = F)
TB_driver$sampledate = ymd(TB_driver$sampledate)
TB_driver$year = year(TB_driver$sampledate)
TB_driver.phyto1$season[is.na(TB_driver.phyto1$season)]=0
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Synechococcus ')

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
#62406 same as, 63004, just translated forward potentially diff mmagnitude #61808
#Trachelomonas: no interaction with 20901, 30201,50000,50900, 51101,51103,51105, 51200, 51202, 51800,53200, 60202,
#60203, 60300,60400,60500,61200, 61500, 61501, 61809,61904, 61919,62001, 62005, 62007, 62201, 62407, 62607
#62700, 62902, 63000, 63003, 63603,63700,80000, 80001, 20101-62000

#gen.keep = c("Armored Dinoflagellate","Naked Dinoflagellate","Limnothrix","Microcystis","Lindavia",
#             "Cryptomonad", "Asterionella", "Dinobryon", "Fragilaria", "Total")

#genus.sub = subset(genus, Genus %in% gen.keep)
species.sub6 = subset(TB_driver.phyto1, species_code %in% species.6)
species.sub6$season[is.na(species.sub6$season)]=0
ggplot(species.sub6, aes(density, biovolume_conc))+
  geom_point()+
  #geom_smooth(method='lm',se=F)+
  scale_color_brewer(palette = 'Paired')+
  facet_wrap(~season, scales = "free")



# paired-plot of predictors and total biovolumes, density for co-linearity
species.sub6 = subset(TB_driver.phyto1, species_code %in% species.6)
species.sub6$season[is.na(species.sub6$season)]=0
ggplot(species.sub6, aes(density, biovolume_conc))+
  geom_point()+
  #geom_smooth(method='lm',se=F)+
  scale_color_brewer(palette = 'Paired')+
  facet_wrap(~season, scales = "free")

pairs(species.sub6[,c(7,52,58)])
############################################


#totals %<>% select(-Genus)
#totals$log.chlor = log(totals$chlor)
#totals$log.bv = log(totals$CellBioVol)
#totals.long = pivot_longer(totals, cols=c("chlor","log.chlor","avsnow","totice","whiteice","blueice","perwhiteice","perblueice",
#                                          "light","CellBioVol","log.bv"), names_to="variable", values_to = "value")

TB_driver.phyto1$biovolume_conc<-as.numeric(TB_driver.phyto1$biovolume_conc)

#TB_driver.phyto1%<>% select("10000","20000","20302", "20601", "20701","30000","30801")
totals.long = pivot_longer(TB_driver.phyto2, cols=c("wtemp","biovolume_conc", "density", "avsnow", "frlight"), names_to="variable", values_to = "value")

ggplot(totals.long, aes(sampledate, value, color=variable))+
  geom_point()+
  geom_smooth(aes(group=variable))+
  #geom_line(aes(group=variable))+
  theme_bw()+
  facet_wrap(~variable, scales='free')+
  labs(title="Synechococcus 60900")

ggsave(totals.long,"figures/zoops northern/scatter/Synechococcus_60900.png", plot=a, height = 8, width = 12, units = "in")

ggsave("Synechococcus_60800.png", plot = last_plot(), height = 8, width = 12, units = "in")


TB_driver<- read.csv("data/TBCleanSet07212020.csv", stringsAsFactors = F)
TB_driver$sampledate = ymd(TB_driver$sampledate)
TB_driver$year = year(TB_driver$sampledate)
TB_driver.phyto1$season[is.na(TB_driver.phyto1$season)]=0
TB_driver.phyto1=subset(TB_driver, taxa_name== 'Synechococcus ')
TB_driver.phyto2=subset(TB_driver.phyto1, species_code== '60900')



#species.6=c("60800") #next is 60900
#species.2= c("10000","20000","20302", "20601", "20701","30000","30801",
#              "50700","51000", "51100", "51130","51801","60200","60201",
#             "60800","60900","61300","61402","61502","61701",
#             "61702","61800","61801","61803","61804","61805","61806","61810",
#             "61900","61911","61918","62400","62406","63004","63005",
#            "63400", "63600","63602","63612","63616","88888")
#62406 same as, 63004, just translated forward potentially diff mmagnitude #61808
#Trachelomonas: no interaction with 20901, 30201,50000,50900, 51101,51103,51105, 51200, 51202, 51800,53200, 60202,
#60203, 60300,60400,60500,61200, 61500, 61501, 61809,61904, 61919,62001, 62005, 62007, 62201, 62407, 62607
#62700, 62902, 63000, 63003, 63603,63700,80000, 80001, 20101-62000
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
            
            
#species.6=c("60800") #next is 60900
#species.2= c("10000","20000","20302", "20601", "20701","30000","30801",
#              "50700","51000", "51100", "51130","51801","60200","60201",
#             "60800","60900","61300","61402","61502","61701",
#             "61702","61800","61801","61803","61804","61805","61806","61810",
#             "61900","61911","61918","62400","62406","63004","63005",
#            "63400", "63600","63602","63612","63616","88888")
#62406 same as, 63004, just translated forward potentially diff mmagnitude #61808
 #Trachelomonas: no interaction with 20901, 30201,50000,50900, 51101,51103,51105, 51200, 51202, 51800,53200, 60202,
#60203, 60300,60400,60500,61200, 61500, 61501, 61809,61904, 61919,62001, 62005, 62007, 62201, 62407, 62607
#62700, 62902, 63000, 63003, 63603,63700,80000, 80001, 20101-62000
            
splot <- function(folder, plotname) {
  
  ggsave(filename = paste("Plots/", folder, plotname, ".png", sep = ""), width = 6, height = 4, units = 'in')
  
}
