library(tidyverse)
library(lubridate)
library(ggplot2)


Limnothrixatzero2 <- read_csv('Limnothrixatzero2.csv',
                              col_types = cols(sampledate = col_date(format = "%m/%d/%Y")))
lte_lite <- read_csv('lte_lite.csv',
                     col_types = cols(sampledate = col_date(format = "%m/%d/%Y")))

bothchloro<- read_csv('fullchloro.csv')


Limnothrixlte_lite3 <- lte_lite %>%
  right_join(Limnothrixatzero2, by= c('sampledate'))
write.csv(Limnothrixlte_lite3, 'Limnothrixlte_lite3.csv')

Limnothrixlte_lite3_chloro <-Limnothrixlte_lite3 %>%
  right_join(bothchloro, by= c('sampledate'))
write.csv(Limnothrixlte_lite3_chloro, 'Limnothrixlte_lite3_chloro.csv')

Limnothrix_lte_lite3_chloro_datayearonly <- read_csv('Limnothrixlte_lite3_chloro_datayearonly.csv',
                                                     col_types = cols(sampledate = col_date(format = "%m/%d/%Y")))



lll3light <- ggplot(Limnothrixlte_lite3, aes(x= CellBioVol, y=surflite))+geom_point()+
  facet_wrap('group')+
  labs(x = "Biovolume Limnothrix",
       y = "surf light")+
  theme_bw()
lll3light
ggsave(plot=lll3light,filename='lll3light.png',height = 18, width =16, units = 'in')

lll3light <- ggplot(Limnothrix_lte_lite3_chloro_datayearonly, aes(x= CellBioVol, y=surflite))+geom_point(size=2)+
  facet_wrap('group')+
  labs(x = "Limnothrix Biovolume",
       y = "surf light")+
  ggtitle('Seasonal Limnothrix Relationship with Surface Light')+
  theme_bw()
lll3light
ggsave(plot=lll3light,filename='LLsurflight.png',height = 18, width =16, units = 'cm')
lll3lightyear<- lll3light+ xlab('Limnothrix Biovolume (um3)') + ylab('Surface Light (units)')+ ylim(0,1)+
  geom_point(aes(color = factor(year4.x)))
lll3lightyear
ggsave(plot=lll3lightyear,filename='LLsurflightyear.png',height = 18, width =16, units = 'cm')
###needs work##


lll3snow <- ggplot(Limnothrixlte_lite3, aes(x= CellBioVol, y=avsnow))+ geom_point()+
  labs(x = "Biovolume Limnothrix",
       y = "av snow")+
  theme_bw()
lll3snow
ggsave(plot=lll3snow,filename='lll3snow.png',height = 18, width =16, units = 'in')


lll3snow <- ggplot(Limnothrix_lte_lite3_chloro_datayearonly, aes(x= CellBioVol, y=avsnow))+geom_point(size=2)+
  labs(x = "Limnothrix Biovolume",
       y = "av snow")+
  ggtitle('Seasonal Limnothrix Relationship with Average Snow')+
  theme_bw()
lll3snow
ggsave(plot=lll3snow,filename='LLsnow.png',height = 18, width =16, units = 'cm')
lll3snowyear<- lll3snow+ xlab('Limnothrix Biovolume (um3)') + ylab('Average Snow (cm)')+ 
  geom_point(aes(color = factor(year4.x)))
lll3snowyear
ggsave(plot=lll3snowyear,filename='LLsnowyear.png',height = 18, width =16, units = 'cm')


lll3tice <- ggplot(Limnothrixlte_lite3, aes(x= CellBioVol, y=totice))+geom_point()+
  labs(x = "Biovolume Limnothrix",
       y = "total ice")+
  theme_bw()
lll3tice
ggsave(plot=lll3tice,filename='lll3tice.png',height = 18, width =16, units = 'in')


lll3tice <- ggplot(Limnothrix_lte_lite3_chloro_datayearonly, aes(x= CellBioVol, y=totice))+geom_point(size=2)+
  labs(x = "Limnothrix Biovolume",
       y = "total ice")+
  ggtitle('Seasonal Limnothrix Relationship with Ice')+
  theme_bw()
lll3tice
ggsave(plot=lll3tice,filename='lll3tice.png',height = 18, width =16, units = 'cm')
lll3ticeyear<- lll3tice+ xlab('Limnothrix Biovolume (um3)') + ylab('Total Ice (cm)')+ 
  geom_point(aes(color = factor(year4.x)))
lll3ticeyear
ggsave(plot=lll3ticeyear,filename='LLiceyear.png',height = 18, width =16, units = 'cm')


lll3whiteice <- ggplot(Limnothrixlte_lite3, aes(x= CellBioVol, y=whiteice))+geom_point()+
  labs(x = "Biovolume Limnothrix",
       y = "white ice")+
  theme_bw()
lll3whiteice
ggsave(plot=lll3whiteice,filename='lll3whiteice.png',height = 18, width =16, units = 'in')

lll3whiteice <- ggplot(Limnothrix_lte_lite3_chloro_datayearonly, aes(x= CellBioVol, y=whiteice))+geom_point(size=2)+
  labs(x = "Limnothrix Biovolume",
       y = "white ice")+
  ggtitle('Seasonal Limnothrix Relationship with White Ice')+
  theme_bw()
lll3whiteice
ggsave(plot=lll3whiteice,filename='lll3whiteice.png',height = 18, width =16, units = 'cm')
lll3whiteiceyear<- lll3whiteice+ xlab('Limnothrix Biovolume (um3)') + ylab('Total White Ice (cm)')+ 
  geom_point(aes(color = factor(year4.x)))
lll3whiteiceyear
ggsave(plot=lll3whiteiceyear,filename='LLwhiteiceyear.png',height = 18, width =16, units = 'cm')


lll3blueice <- ggplot(Limnothrixlte_lite3, aes(x= CellBioVol, y=blueice))+geom_point()+
  labs(x = "Biovolume Limnothrix",
       y = "blue ice")+
  theme_bw()
lll3blueice
ggsave(plot=lll3blueice,filename='lll3blueice.png',height = 18, width =16, units = 'in')

lll3blueice <- ggplot(Limnothrix_lte_lite3_chloro_datayearonly, aes(x= CellBioVol, y=blueice))+geom_point(size=2)+
  labs(x = "Limnothrix Biovolume",
       y = "Blue ice")+
  ggtitle('Seasonal Limnothrix Relationship with Blue Ice')+
  theme_bw()
lll3blueice
ggsave(plot=lll3blueice,filename='lll3blueice.png',height = 18, width =16, units = 'cm')
lll3blueiceyear<- lll3whiteice+ xlab('Limnothrix Biovolume (um3)') + ylab('Total Blue Ice (cm)')+ 
  geom_point(aes(color = factor(year4.x)))
lll3blueiceyear
ggsave(plot=lll3blueiceyear,filename='LLblueiceyear.png',height = 18, width =16, units = 'cm')


lll3surfchloro <-  ggplot(Limnothrixlte_lite3_chloro, aes(x= CellBioVol, y=surfchlor))+geom_point()+
  facet_wrap('group')+
  labs(x = "Biovolume Limnothrix",
       y = "surface chloro")+
  theme_bw()
lll3surfchloro
ggsave(plot=lll3surfchloro,filename='lll3surfchloro.png',height = 18, width =16, units = 'in')

lll3surfchloro <- ggplot(Limnothrix_lte_lite3_chloro_datayearonly, aes(x= CellBioVol, y=surfchlor))+geom_point(size=2)+
  facet_wrap('group')+
  labs(x = "Limnothrix Biovolume",
       y = "surface chloro")+
  ggtitle('Seasonal Limnothrix Relationship with Surface Chloro')+
  theme_bw()
lll3surfchloro
ggsave(plot=lll3surfchloro,filename='lll3surfchloro.png',height = 18, width =16, units = 'cm')
lll3surfchloroyear<- lll3surfchloro+ xlab('Limnothrix Biovolume (um3)') + ylab('Surface Chloro ()')+ ylim(0,10)+
  geom_point(aes(color = factor(year4.x)))
lll3surfchloroyear
ggsave(plot=lll3surfchloroyear,filename='lll3surfchloroyear.png',height = 18, width =16, units = 'cm')


lll3chloro <-  ggplot(Limnothrixlte_lite3_chloro, aes(x= CellBioVol, y=chlor))+geom_point()+
  facet_wrap('group')+
  labs(x = "Biovolume Limnothrix",
       y = "int chloro")+
  theme_bw()
lll3chloro
ggsave(plot=lll3chloro,filename='lll3chloro.png',height = 18, width =16, units = 'in')

lll3chloro <- ggplot(Limnothrix_lte_lite3_chloro_datayearonly, aes(x= CellBioVol, y=chlor))+geom_point(size=2)+
  facet_wrap('group')+
  labs(x = "Limnothrix Biovolume",
       y = "Int chloro")+
  ggtitle('Seasonal Limnothrix Relationship with Integrated Chloro')+
  theme_bw()
lll3chloro
ggsave(plot=lll3chloro,filename='lll3chloro.png',height = 18, width =16, units = 'cm')
lll3chloroyear<- lll3chloro+ xlab('Limnothrix Biovolume (um3)') + ylab('Integrated Chloro ()')+ ylim(0,10)+
  geom_point(aes(color = factor(year4.x)))
lll3chloroyear
ggsave(plot=lll3chloroyear,filename='lll3chloroyear.png',height = 18, width =16, units = 'cm')


lll3wtemp <-  ggplot(Limnothrixlte_lite3_chloro, aes(x= CellBioVol, y=wtemp))+geom_point()+
  facet_wrap('group')+
  labs(x = "Biovolume Limnothrix",
       y = "int wtemp")+
  theme_bw()
lll3wtemp
ggsave(plot=lll3wtemp,filename='Limnothrixintwtemp.png',height = 18, width =16, units = 'in')

lll3wtemp <- ggplot(Limnothrix_lte_lite3_chloro_datayearonly, aes(x= CellBioVol, y=wtemp))+geom_point(size=2)+
  facet_wrap('group')+
  labs(x = "Limnothrix Biovolume",
       y = "Int Water Temp")+
  ggtitle('Seasonal Limnothrix Relationship with Integrated Water Temperature')+
  theme_bw()
lll3wtemp
ggsave(plot=lll3wtemp,filename='lll3wtemp.png',height = 18, width =16, units = 'cm')
lll3wtempyear<- lll3wtemp+ xlab('Limnothrix Biovolume (um3)') + ylab('Integrated Water Temp (C)')+ 
  geom_point(aes(color = factor(year4.x)))
lll3wtempyear
ggsave(plot=lll3wtempyear,filename='lll3wtempyear.png',height = 18, width =16, units = 'cm')



lll3o2 <-  ggplot(Limnothrixlte_lite3_chloro, aes(x= CellBioVol, y=o2))+geom_point()+
  facet_wrap('group')+
  labs(x = "Biovolume Limnothrix",
       y = "int o2")+
  theme_bw()
lll3o2
ggsave(plot=lll3o2,filename='Limnothrixso2.png',height = 18, width =16, units = 'in')

lll3o2 <- ggplot(Limnothrix_lte_lite3_chloro_datayearonly, aes(x= CellBioVol, y=o2))+geom_point(size=2)+
  facet_wrap('group')+
  labs(x = "Limnothrix Biovolume",
       y = "Int 02")+
  ggtitle('Seasonal Limnothrix Relationship with Integrated Oxygen')+
  theme_bw()
lll3o2
ggsave(plot=lll3o2,filename='lll3o2.png',height = 18, width =16, units = 'cm')
lll3o2year<- lll3o2+ xlab('Limnothrix Biovolume (um3)') + ylab('Integrated Oxygen ()')+ ylim(5,12)+
  geom_point(aes(color = factor(year4.x)))
lll3o2year
ggsave(plot=lll3o2year,filename='lll3o2year.png',height = 18, width =16, units = 'cm')


lll3o2sat <-  ggplot(Limnothrixlte_lite3_chloro, aes(x= CellBioVol, y=o2sat))+geom_point()+
  facet_wrap('group')+
  labs(x = "Biovolume Limnothrix",
       y = "int o2 sat")+
  theme_bw()
lll3o2sat
ggsave(plot=lll3o2sat,filename='Limnothrixinto2sat.png',height = 18, width =16, units = 'in')

lll3o2sat <- ggplot(Limnothrix_lte_lite3_chloro_datayearonly, aes(x= CellBioVol, y=o2sat))+geom_point(size=2)+
  facet_wrap('group')+
  labs(x = "Limnothrix Biovolume",
       y = "Int 02 sat")+
  ggtitle('Seasonal Limnothrix Relationship with Integrated Staturated Oxygen')+
  theme_bw()
lll3o2sat
ggsave(plot=lll3o2sat,filename='lll3o2sat.png',height = 18, width =16, units = 'cm')
lll3o2satyear<- lll3o2sat+ xlab('Limnothrix Biovolume (um3)') + ylab('Integrated Staturated Oxygen ()')+ ylim(60,120)+
  geom_point(aes(color = factor(year4.x)))
lll3o2satyear
ggsave(plot=lll3o2satyear,filename='lll3o2satyear.png',height = 18, width =16, units = 'cm')



lll3cond <-  ggplot(Limnothrixlte_lite3_chloro, aes(x= CellBioVol, y=cond))+geom_point()+
  facet_wrap('group')+
  labs(x = "Biovolume Limnothrix",
       y = "int cond")+
  theme_bw()
lll3cond
ggsave(plot= lll3cond,filename='Limnothrixintcond.png',height = 18, width =16, units = 'in')

lll3cond <- ggplot(Limnothrix_lte_lite3_chloro_datayearonly, aes(x= CellBioVol, y=cond))+geom_point(size=2)+
  facet_wrap('group')+
  labs(x = "Limnothrix Biovolume",
       y = "Int cond")+
  ggtitle('Seasonal Limnothrix Relationship with Integrated Cond')+
  theme_bw()
lll3cond
ggsave(plot=lll3cond,filename='lll3cond.png',height = 18, width =16, units = 'cm')
lll3condyear<- lll3cond+ xlab('Limnothrix Biovolume (um3)') + ylab('Integrated Cond Oxygen ()')+
  geom_point(aes(color = factor(year4.x)))
lll3condyear
ggsave(plot=lll3condyear,filename='lll3condyear.png',height = 18, width =16, units = 'cm')



lll3doc.y <-  ggplot(Limnothrixlte_lite3_chloro, aes(x= CellBioVol, y=doc.y))+geom_point()+
  facet_wrap('group')+
  labs(x = "Biovolume Limnothrix",
       y = "int doc")+
  theme_bw()
lll3doc.y
ggsave(plot=lll3doc.y,filename='Limnothrixintdoc.png',height = 18, width =16, units = 'in')

lll3doc.y <- ggplot(Limnothrix_lte_lite3_chloro_datayearonly, aes(x= CellBioVol, y=doc.y))+geom_point(size=2)+
  facet_wrap('group')+
  labs(x = "Limnothrix Biovolume",
       y = "Int DOC")+
  ggtitle('Seasonal Limnothrix Relationship with Integrated DOC')+
  theme_bw()
lll3doc.y
ggsave(plot=lll3doc.y,filename='lll3doc.png',height = 18, width =16, units = 'cm')
lll3doc.yyear<- lll3doc.y+ xlab('Limnothrix Biovolume (um3)') + ylab('Integrated DOC ()')+
  geom_point(aes(color = factor(year4.x)))
lll3doc.yyear
ggsave(plot=lll3doc.yyear,filename='lll3docyear.png',height = 18, width =16, units = 'cm')


lll3ph.y <-  ggplot(Limnothrixlte_lite3_chloro, aes(x= CellBioVol, y=ph.y))+geom_point()+
  facet_wrap('group')+
  labs(x = "Biovolume Limnothrix",
       y = "int ph")+
  theme_bw()
lll3ph.y
ggsave(plot=lll3ph.y,filename='Limnothrixintph.png',height = 18, width =16, units = 'in')

lll3ph<- ggplot(Limnothrix_lte_lite3_chloro_datayearonly, aes(x= CellBioVol, y=ph.y))+geom_point(size=2)+
  facet_wrap('group')+
  labs(x = "Limnothrix Biovolume",
       y = "Int pH")+
  ggtitle('Seasonal Limnothrix Relationship with Integrated pH')+
  theme_bw()
lll3ph
ggsave(plot=lll3ph,filename='lll3ph',height = 18, width =16, units = 'cm')
lll3phyear<- lll3ph+ xlab('Limnothrix Biovolume (um3)') + ylab('Integrated pH')+ ylim(7, 7.6)+
  geom_point(aes(color = factor(year4.x)))
lll3phyear
ggsave(plot=lll3phyear,filename='lll3phyear.png',height = 18, width =16, units = 'cm')


library("cowplot")
lll3_all <- plot_grid( lll3snow, lll3tice, lll3whiteice, lll3blueice, lll3ph.y, lll3doc.y, lll3light,lll3surfchloro,
                       lll3chloro, lll3wtemp, lll3o2,lll3o2sat,
                       labels = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L"),
                       ncol = 4, nrow = 3)
ggsave(plot=lll3_all,filename='lll3_all.png',height = 40, width =49, units = 'in')
lll3_all

lll3_allyears <- plot_grid (lll3snowyear, lll3ticeyear,lll3whiteiceyear, lll3blueiceyear, lll3lightyear, lll3surfchloroyear, 
                            lll3chloroyear,lll3wtempyear, lll3o2year,lll3o2satyear,
                            labels = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L"),
                            ncol = 3, nrow = 4)
ggsave(plot=lll3_allyears,filename='lll3_allyears.png',height = 40, width =49, units = 'in')
lll3_allyears



library("cowplot")
dbandll<-plot_grid(dbl3lightyear,lll3lightyear, dbl3snow2, lll3snowyear,
                   labels = c("A", "B", "C", "D"),
                   ncol = 2, nrow = 2)
dbandll
ggsave(plot=dbandll,filename='snowandlightdbll.png',height = 40, width =49, units = 'cm')


dbl3_all_years <- plot_grid(dbl3snow2, dbl3tice2, dbl3whiteice2year, dbl3blueice2, dbl3lightyear, dbl3surfchloro2, dbl3chloro2year, dbl3wtemp2,
                             dbl3o2year, dbl3o2sat2year, dbl3cond2year,dbl3ph2year,
                             labels = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L"),
                             ncol = 4, nrow = 3)
ggsave(plot=dbl3_all_years,filename='dbl3_all_years.png',height = 40, width =49, units = 'cm')
dbl3_all_years
