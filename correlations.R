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
library(corrplot)
library(RColorBrewer)

##########

TB_driver_2<- read.csv("data/TBCleanSet07212020.csv", stringsAsFactors = F, na = "empty")
TB_driver_2$sampledate = ymd(TB_driver_2$sampledate)
TB_driver_2$year = year(TB_driver_2$sampledate)
TB_driver_2$season[is.na(TB_driver_2$season)]=0
TB_driver_2=filter(TB_driver_2, year >=2010)

#TB_driver.species %>% distinct(TBZoopDate, .keep_all = TRUE)
#TB_driver_3= TB_driver_2[c(2:9,38:43,46, 48,49, 52, 55:58,63)]

#TB_driver_phyto=TB_driver_2[c(2:9,38:43,46,49, 52, 63)]
TB_driver_phyto=TB_driver_2[c(2:9,38:43,46,49, 52, 63)]

TB_driver_phyto=TB_driver_2[c(2:9,38:43,46,49, 52, 63)]
#TB_driver_phyto =TB_driver_phyto[c(1:14,16,17)]
TB_driver_phyto2<-distinct(TB_driver_phyto, TBphytoDate, taxa_name, .keep_all = T)


#TB_driver_phyto3<-TB_driver_phyto2[c(15:17)]

#major phyto players
TB_driver_phyto2
TB_taxa=subset(TB_driver_phyto2, taxa_name== 'Total')
sum(TB_taxa$biovolume_conc>0)

#TB_driver_phyto3 %>% 
#  pivot_longer(-TBphytoDate, names_to = "taxa_name", values_to = "biovolune_conc")

#data.long2 = pivot_longer(data, cols=c("whiteice","blueice"), names_to="variable", values_to = "value")
#TB_driver_2.long= pivot_longer(TB_driver_phyto2, cols=c("taxa_name", "biovolume_conc"),  names_to="variable", values_to = "value")
TB_driver_2.wide= pivot_wider(TB_driver_phyto2, names_from = taxa_name, values_from = biovolume_conc)

TB_driver_2.wide_Winter=filter(TB_driver_2.wide, season==1)
TB_driver_2.wide_Summer=filter(TB_driver_2.wide, season=='NA')
write.csv(TB_driver_2.wide_Winter, 'data/TB_driver_2.wide_winter.csv', row.names = F)
write.csv(TB_driver_2.wide_Summer, 'data/TB_driver_2.wide_Summer.csv', row.names = F)
write.csv(TB_driver_2.wide, 'data/TB_driver_2.wide.vars.csv', row.names = F)


TB_drivermajorplayers.wide<- read.csv("data/TB_driver_majorplayers.csv", stringsAsFactors = F, na = "empty")



TB_drivermajorplayers.Summer<- read.csv("data/TB_driver_2.wide_Summer.Vars.Main.csv", stringsAsFactors = F, na = "empty")
TB_drivermajorplayers.Winter1<- read.csv("data/TB_driver_2.wide_winter_Vars.csv", stringsAsFactors = F, na = "empty")
TB_drivermajorplayers.Winter2<- read.csv("data/TB_driver_2.wide_winter_Vars_twomore.csv", stringsAsFactors = F, na = "empty")
TB_drivermajorplayers.Winter3<- read.csv("data/TB_driver_2.wide_winter_Vars_threemore.csv", stringsAsFactors = F, na = "empty")
TB_driver.fullset<- read.csv("data/TB_driver_2.wide.vars.clean.csv", stringsAsFactors = F, na = "empty")
TB_driver.major<- read.csv("data/TB_driver_2.wide.vars.clean.major.csv", stringsAsFactors = F, na = "empty")


M<-cor(TB_drivermajorplayers.Winter1)
corrplot(M, type="upper",
         col=brewer.pal(n=8, name="RdYlBu"))

write.csv(M, 'data/chl-a/MTB_drivermajorplayers.Winter1.csv', row.names = F)



###################################

TB_drivers<- read.csv("data/TBCleanSet07212020.csv", stringsAsFactors = F, na = "empty")
TB_drivers$sampledate = ymd(TB_drivers$sampledate)
TB_drivers$year = year(TB_drivers$sampledate)
TB_drivers$month=month(TB_drivers$sampledate)
TB_drivers$season[is.na(TB_drivers$season)]=0
TB_drivers=filter(TB_drivers, year >=2010)

#TB_driver.species %>% distinct(TBZoopDate, .keep_all = TRUE)
TB_driver_zoop= TB_drivers[c(2:9,38:43, 55,56, 58,63,64)]
TB_driver_zoop2<-distinct(TB_driver_zoop, TBZoopDate, species_code, .keep_all = T)
TB_driver_zoop.wide= pivot_wider(TB_driver_zoop2, names_from = c("species_code"), values_from = c("density"))
TB_driver_zoop.wide$season[is.na(TB_driver_zoop.wide$season)]=0
write.csv(TB_driver_zoop.wide, 'data/TB_driver_zoop.wide.csv', row.names = F)

TB_Zoops<- read.csv("data/TB_driver_zoop.wide.edit.csv", stringsAsFactors = F, na = "empty")

TB_Zoops$sampledate = mdy(TB_Zoops$sampledate)
TB_Zoops$TBZoopDate = mdy(TB_Zoops$TBZoopDate)
TB_Zoops$season[is.na(TB_Zoops$season)]=0
library(readr)
TB_ZoopsCorrs <- read.csv("data/zoopcors.csv", stringsAsFactors = F,
                     col_types = cols(`10000` = col_number(), 
                                      `20000` = col_number(), `20302` = col_number(), 
                                      `20601` = col_number(), `30000` = col_number(), 
                                      `30801` = col_number(), `50700` = col_number(), 
                                      `51000` = col_number(), `51130` = col_number(), 
                                      `51801` = col_number(), `60800` = col_number(), 
                                      `60900` = col_number(), `61502` = col_number(), 
                                      `61701` = col_number(), `61702` = col_number(), 
                                      `61800` = col_number(), `61801` = col_number(), 
                                      `61806` = col_number(), `61808` = col_number(), 
                                      `61918` = col_number(), `62406` = col_number(), 
                                      `63004` = col_number(), `63005` = col_number(), 
                                      `63400` = col_number(), `63612` = col_number(), 
                                      `88888` = col_number()))


TB_ZoopsCorrs<- read.csv("data/zoopcors.csv", stringsAsFactors = F)

#TB_ZoopsCorrs<-col_types = cols(`10000` = col_number(), 
  #               `20000` = col_number(), `20302` = col_number(), 
  #               `20601` = col_number(), `30000` = col_number(), 
  #               `30801` = col_number(), `50700` = col_number(), 
  #               `51000` = col_number(), `51130` = col_number(),
  #               `51801` = col_number(), `60800` = col_number(), 
  #               `60900` = col_number(), `61502` = col_number(), 
  #               `61701` = col_number(), `61702` = col_number(), 
  #               `61800` = col_number(), `61801` = col_number(), 
  #               `61806` = col_number(), `61808` = col_number(),
  #               `61918` = col_number(), `62406` = col_number(), 
  #               `63004` = col_number(), `63005` = col_number(), 
  #               `63400` = col_number(), `63612` = col_number(), 
  #               `88888` = col_number())

TB_ZoopsCorrs$X20000=as.numeric(TB_ZoopsCorrs$X20000)
TB_ZoopsCorrs$X10000=as.numeric(TB_ZoopsCorrs$X10000)
TB_ZoopsCorrs$X20302=as.numeric(TB_ZoopsCorrs$X20302)
TB_ZoopsCorrs$X20601=as.numeric(TB_ZoopsCorrs$X20601)
TB_ZoopsCorrs$X30000=as.numeric(TB_ZoopsCorrs$X30000)
TB_ZoopsCorrs$X30801=as.numeric(TB_ZoopsCorrs$X30801)
TB_ZoopsCorrs$X50700=as.numeric(TB_ZoopsCorrs$X50700)
TB_ZoopsCorrs$X51000=as.numeric(TB_ZoopsCorrs$X51000)
TB_ZoopsCorrs$X51130=as.numeric(TB_ZoopsCorrs$X51130)
TB_ZoopsCorrs$X51801=as.numeric(TB_ZoopsCorrs$X51801)
TB_ZoopsCorrs$X60800=as.numeric(TB_ZoopsCorrs$X60800)
TB_ZoopsCorrs$X60800=as.numeric(TB_ZoopsCorrs$X60800)
TB_ZoopsCorrs$X60900=as.numeric(TB_ZoopsCorrs$X60900)
TB_ZoopsCorrs$X61502=as.numeric(TB_ZoopsCorrs$X61502)
TB_ZoopsCorrs$X61701=as.numeric(TB_ZoopsCorrs$X61701)
TB_ZoopsCorrs$X61702=as.numeric(TB_ZoopsCorrs$X61702)
TB_ZoopsCorrs$X61800=as.numeric(TB_ZoopsCorrs$X61800)
TB_ZoopsCorrs$X61801=as.numeric(TB_ZoopsCorrs$X61801)
TB_ZoopsCorrs$X61806=as.numeric(TB_ZoopsCorrs$X61806)
TB_ZoopsCorrs$X61808=as.numeric(TB_ZoopsCorrs$X61808)
TB_ZoopsCorrs$X61918=as.numeric(TB_ZoopsCorrs$X61918)
TB_ZoopsCorrs$X62406=as.numeric(TB_ZoopsCorrs$X62406)
TB_ZoopsCorrs$X63004=as.numeric(TB_ZoopsCorrs$X63004)
TB_ZoopsCorrs$X63005=as.numeric(TB_ZoopsCorrs$X63005)
TB_ZoopsCorrs$X63400=as.numeric(TB_ZoopsCorrs$X63400)
TB_ZoopsCorrs$X63612=as.numeric(TB_ZoopsCorrs$X63612)
TB_ZoopsCorrs$X88888=as.numeric(TB_ZoopsCorrs$X88888)


TB_ZoopsCorrs_Winter=filter(TB_ZoopsCorrs, season==1)
write.csv(TB_ZoopsCorrs_Winter, 'data/TB_ZoopsCorrs_Winter.csv', row.names = F)
TB_ZoopsWinter<- read.csv("data/TB_ZoopsWinter.csv", stringsAsFactors = F)

TB_ZoopsCorrs_Summer=filter(TB_ZoopsCorrs, season==0)
write.csv(TB_ZoopsCorrs_Summer, 'data/TB_ZoopsCorrs_Summer.csv', row.names = F)
TB_ZoopsSummer<- read.csv("data/TB_ZoopsSummer.csv", stringsAsFactors = F)

TB_ZoopsVars<- read.csv("data/zoopcorsmorevar.csv", stringsAsFactors = F)
TB_ZoopsVarsNA<-na.omit(TB_ZoopsVars)

M<-cor(TB_ZoopsSummer)
corrplot(M, type="upper",
         col=brewer.pal(n=8, name="RdYlBu"))

write.csv(M, 'data/chl-a/MTB_ZoopsWinter.csv', row.names = F)


#ggsave("Zoop Corrs1 Winter.png", plot = corrplot, height = 8, width = 12, units = "in") #doesn't have corrplot :( )


TB_ZoopsVars<- read.csv("data/zoopcorsmorevar.csv", stringsAsFactors = F)
TB_ZoopsVarsNA<-na.omit(TB_ZoopsVars)

TB_ZoopsVars_Winter=filter(TB_ZoopsVars, season==1)
write.csv(TB_ZoopsVars_Winter, 'data/TB_ZoopsVars_Winter.csv', row.names = F)
TB_ZoopsVars_Winter2<- read.csv("data/TB_ZoopsVars_Winter.edit.csv", stringsAsFactors = F)

TB_ZoopsVars_Summer=filter(TB_ZoopsVars, season==0)
TB_ZoopsVars_Summer<-na.omit(TB_ZoopsVars_Summer)
write.csv(TB_ZoopsVars_Summer, 'data/TB_ZoopsVars_Summer.csv', row.names = F)
TB_ZoopsVars_Summer<- read.csv("data/TB_ZoopsVars_Summer.csv", stringsAsFactors = F)
TB_ZoopsVars_Summer<-na.omit(TB_ZoopsVars_Summer)

M<-cor(TB_ZoopsVars)
corrplot(M, type="upper",
         col=brewer.pal(n=8, name="RdYlBu"))

write.csv(M, '/Users/emilywhitaker/Desktop/coors/TB_ZoopsSummer.csv', row.names = F)



#write.csv(M, 'data/chl-a/MTB_ZoopsVars2.csv', row.names = F)


######################

#repeat with phyto data- check
#merge phyto with zoops #URGGGGGGGGGG #we can do this!!!!!!

#full, winter, and summer coors,, not this way #ask hil for help 
TB_driver_zoop
TB_driver_phyto2

write.csv(TB_driver_zoop, 'data/tocombinewith.csv', row.names = F)
write.csv(TB_driver_phyto2, 'data/tocombinewithme.csv', row.names = F)

TB_ZoopsA<- read.csv("data/tocombinewith2.csv", stringsAsFactors = F)
TB_PhytosB<- read.csv("data/tocombinewithme2.csv", stringsAsFactors = F)

allofthem<- rbind(TB_ZoopsA, TB_PhytosB)
allofthemAdj<-distinct(allofthem, TBDate, thing, .keep_all = T)
write.csv(allofthemAdj, 'data/allofthemAdj.csv', row.names = F)

allofthem.wide= pivot_wider(allofthemAdj, names_from = c("thing"), values_from = c("amount"))
write.csv(allofthem.wide, 'data/allofthem.wide.csv', row.names = F)

allofthem.wide.nums<- as.numeric(allofthem.wide, c(19:181))

write.csv(M, '/Users/emilywhitaker/Desktop/coors/WinterSomeVarsAlltypes.csv', row.names = F)



M<-cor(allofthem.wide.nums)
corrplot(M, type="upper",
         col=brewer.pal(n=8, name="RdYlBu"))

write.csv(M, 'data/chl-a/Mallofthem.wide.nums2.csv', row.names = F)




TB_driver_phyto2$month <- month(TB_driver_phyto2$sampledate)
allofthem_wide_majorplayers <- read.csv("data/allofthem.wide.majorplayers.csv", 
                                        col_types = cols(`10000` = col_number(), 
                                                         `20000` = col_number(), `30201` = col_number(), 
                                                         `50700` = col_number(), `51000` = col_number(), 
                                                         `51101` = col_number(), `60200` = col_number(), 
                                                         `61806` = col_number(), `61808` = col_number(), 
                                                         `61810` = col_number(), `61904` = col_number(), 
                                                         `61918` = col_number(), `62406` = col_number(), 
                                                         `63003` = col_number(), `63600` = col_number(), 
                                                         `63700` = col_number(), `88888` = col_number(), 
                                                         Aphanothece = col_number(), Chrysocapsaceae = col_number(), 
                                                         `Chrysochromulina parva` = col_number(), 
                                                         `Chrysococcus minutus` = col_number(), 
                                                         `Glenodinium quadridens` = col_number(), 
                                                         `Quadrigula lacustris` = col_number(), 
                                                         `Stephanodiscus medius` = col_number(), 
                                                         TBDate = col_skip(), `Tabellaria flocculosa` = col_number(), 
                                                         sampledate = col_skip(), year = col_skip()))
View(allofthem_wide_majorplayers)


DataFrame<- read.csv("data/allofthem.wide.majorplayers.na.csv", stringsAsFactors = F)

DataFrameWinter=filter(DataFrame, season==1)
write.csv(DataFrameWinter, 'data/chl-a/DataFrameWinter.csv', row.names = F)

DataFrameWinter2<- read.csv("data/chl-a/DataFrameWinteredit.csv", stringsAsFactors = F)
class(DataFrameWinter2$Schroederia.judayi)

DataFrameSummer =filter(DataFrame, season==0) 
write.csv(DataFrameSummer, 'data/chl-a/DataFrameSummer.csv', row.names = F)
DataFrameSummer2<- read.csv("data/chl-a/DataFrameSummeredit.csv", stringsAsFactors = F)


DataFrame.NoVars <- select(DataFrame, 12:57)
DataFrameWinterNV =filter(DataFrame.NoVars, season==1) 

M<-cor(DataFrameWinterNV)
corrplot(M, type="upper", order = "original",
         col=brewer.pal(n=8, name="RdYlBu"))
#write.csv(M, 'data/chl-a/MDataFrame2.csv', row.names = F)

write.csv(M, '/Users/emilywhitaker/Desktop/coors/WinterSomeVarsAlltypes.csv', row.names = F)

#####SAVE THIS CODE FOREVER
png(file = "/Users/emilywhitaker/Desktop/coors/My Plot.png",   # The directory you want to save the file in
    width = 1500, # The width of the plot in inches
    height = 1500) # The height of the plot in inches

# Step 2: Create the plot with R code
corrplot(M, type="upper", order = "original",
         col=brewer.pal(n=8, name="RdYlBu"))
# Additional low-level plotting commands

# Step 3: Run dev.off() to create the file!
dev.off()



