#correlations two

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
library(readr)
###############################

#TB_Zoopsfamily<- read.csv("data/tocombinewith2.vars.csv", stringsAsFactors = F)
FamBam<- read.csv("data/NEWTBCleanSet09122020.csv", stringsAsFactors = F, na = "empty")
FamBam$sampledate = mdy(FamBam$sampledate)
FamBam$year = year(FamBam$sampledate)
FamBam$month=month(FamBam$sampledate)
FamBam$season=is.na(FamBam$season)=0
FamBam.2010=filter(FamBam, year >=2010)
#FamBam.2010$season=is.na(FamBam.2010$season)=0
write.csv(FamBam.2010, 'data/FamBam.2010.csv', row.names = F)
#TB_driver.species %>% distinct(TBZoopDate, .keep_all = TRUE)
TB_driver_family= FamBam.2010[c(2:9,38:43, 55:61,66,67)]
TB_driver_family2<-distinct(TB_driver_family, TBZoopDate, species_code, .keep_all = T)


TB_driver_family_species.wide= pivot_wider(TB_driver_family2, names_from = c("species_code"), values_from = c("density"))
write.csv(TB_driver_family_species.wide, 'data/TB_driver_family_species.wide.csv', row.names = F)


TB_driver_family_Phylum.wide= pivot_wider(TB_driver_family2, names_from = c("Phylum"), values_from = c("density"))
#need something that something says skips when NA
TB_driver_family_Class.wide= pivot_wider(TB_driver_family2, names_from = c("Class"), values_from = c("density"))
#need something that something says skips when NA

write.csv(TB_driver_zoop.wide, 'data/TB_driver_zoop.wide.csv', row.names = F)

TB_Zoops<- read.csv("data/TB_driver_zoop.wide.edit.csv", stringsAsFactors = F, na = "empty")

TB_Zoops$sampledate = mdy(TB_Zoops$sampledate)
TB_Zoops$TBZoopDate = mdy(TB_Zoops$TBZoopDate)
TB_Zoops$season[is.na(TB_Zoops$season)]=0
library(readr)


ggplot(TB_driver_family2, aes(sampledate, as.numeric(density), color=Class))+
  geom_point()+
  #geom_smooth(aes(group=species_code))+
  geom_line(aes(group=species_code), linetype = "dashed")+
  theme_bw()+
  ylab("density")+
  facet_wrap(~species_code, scales='free')

ggsave("Classes of Zoops.png", plot = last_plot(), height = 15, width = 15, units = "in")



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




