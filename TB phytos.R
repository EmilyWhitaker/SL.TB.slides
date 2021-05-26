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
##########

TB_dataset <- read.csv("data/TBCleanSet07212020.csv", stringsAsFactors = F)
TB_dataset$sampledate = ymd(TB_dataset$sampledate)
TB_dataset$year = year(TB_dataset$sampledate)
TB_dataset.subset = subset(TB_dataset, year >= 2008)

write.csv(TB_dataset.subset, 'data/TB_dataset.subset.csv', row.names = F)

### starts in 2010 problematic#######
SP_dataset <- read.csv("data/SPCleanSet07212020.csv", stringsAsFactors = F)
SP_dataset$sampledate = ymd(SP_dataset$sampledate)
SP_dataset$year = year(SP_dataset$sampledate)
SP_dataset.subset = subset(SP_dataset, year >= 2008)

write.csv(SP_dataset.subset, 'data/SP_dataset.subset.csv', row.names = F)

##################
TB_dataset1 <- read.csv("data/TB_dataset.subset.season.csv", stringsAsFactors = F)
TB_dataset1$sampledate = mdy(TB_dataset1$sampledate)
TB_dataset.totals.phyto1=subset(TB_dataset1, genus== 'Total')
TB_dataset.totals.phyto.zoop1= subset(TB_dataset.totals.phyto1, density > 0)
#write.csv(TB_dataset.totals.phyto.zoop, 'data/TBZeroPhytosZoops.csv', row.names = F)


TB_dataset.totals.phyto.zoop1 %<>% select(sampledate, biovolume_conc,species_code, species_name, density) 


#TB_snowice.long = pivot_longer(TB_dataset.totals.phyto.zoop, cols=c("biovolume_conc","blueice"), names_to="variable", values_to = "value")

ggplot(TB_dataset.totals.phyto.zoop1, aes(sampledate, biovolume_conc))+
  geom_point()+
  geom_smooth(aes())+
  labs(title="TB Total Phyto Biovolume")+
  facet_wrap('species_code')

ggplot(TB_dataset.totals.phyto.zoop1, aes(biovolume_conc, density))+
  geom_point()+
  geom_smooth(aes())+
  labs(title="TB Total Phyto Biovolume")+
  facet_wrap('species_code')



species.1 = c("")
  
species.2= c("61702", "63004", "62406", "63612", "61806", "60900")

species.3= c("30000", "61502","61800", "61808", "61918", "63400")

species.4= c("10000", "20000","20302","20601","50700", "51000", "51130")

species.5= c("51810", "60200", "60800", "61701", "61801", "63005", "88888")

species.6= c("30801", "61918")

species.sub5 = subset(TB_dataset.totals.phyto.zoop1, species_code %in% species.5)

ggplot(species.sub5, aes(sampledate, density))+
  geom_point()+
  #geom_smooth(aes())+
  labs(title="TB Total Phyto Biovolume")+
  facet_wrap('species_code', scales = 'free')

ggplot(species.sub5, aes(biovolume_conc, density))+
  geom_point()+
  #geom_smooth(aes())+
  labs(title="TB Total Phyto Biovolume")+
  facet_wrap('species_code', scales = 'free')

RMSE = function(m, o){
  sqrt(mean((m - o)^2,na.rm = T))
}


RMSE()

species.sub5$logbvc=log(species.sub5$biovolume_conc)
species.sub5.long2= pivot_longer(species.sub5, cols=c("logbvc","density"), names_to="variable", values_to = "value")
species.sub5.long2 %<>% select(sampledate,species_code, species_name,variable, value) 

#species.sub5$logbvc=log(species.sub5$biovolume_conc)
species.sub5.long3= pivot_longer(species.sub5, cols=c("biovolume_conc","density"), names_to="variable", values_to = "value")
species.sub5.long3 %<>% select(sampledate,species_code, species_name,variable, value) 

ggplot(species.sub5.long2, aes(sampledate, value, color= variable))+
  geom_point()+
  #geom_smooth(aes())+
  labs(title="TB Total Phyto Biovolume and Zooplankton Dynamics", y= "log(biovolume) of phytoplankton and density of zooplankton")+
  facet_wrap('species_code', scales = 'free')


#############

TB_dataset <- read.csv("data/TB_dataset.subset.season.csv", stringsAsFactors = F)
TB_dataset$sampledate = mdy(TB_dataset$sampledate)
TB_dataset.totals.phyto=subset(TB_dataset, genus== 'Total')
TB_dataset.totals.phyto.zoop= subset(TB_dataset.totals.phyto, density > 0)
TB_dataset.totals.phyto.zoop %<>% select(sampledate, biovolume_conc,species_code, species_name, density, biomass) 

ggplot(TB_dataset.totals.phyto.zoop, aes(biomass, density, color= species_name))+
  geom_point()+
  geom_smooth(aes())+
  labs(title="", y= "density")+
  facet_wrap('species_code', scales = 'free')

ggplot(TB_dataset.totals.phyto.zoop, aes(biovolume_conc, density))+
  geom_point()+
  geom_smooth(aes())+
  labs(title="", y= "density")+
  facet_wrap('species_code', scales = 'free')


###################

#change in denisty and avg_length for zoops
#change in biovol for phytos 

TB_dataset$month = month(TB_dataset$sampledate)



#TB_dataset.sub1 =  subset(TB_dataset,sampledate, division,	genus,taxa_name, biovolume_conc,species_code, species_name, density, biomass, year, month, avsnow,	totice,	whiteice,	blueice,	iceduration,	season, frlight,	chlor.surf,	chlor.int ) 

#taxaname2- biovolume_conc2)-(taxaname1- biovolume_conc1)= $entryindelta



