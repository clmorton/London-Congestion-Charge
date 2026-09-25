#Preliminary Analysis of car ownership 

#Load packages
library(readr)
library(ggplot2)
library(tidyverse)
library(reshape2)
library(lmtest)
library(plm)
library(sf)
library(sp)
library(spdep)
library(splm)
library(spatialreg)
library(stargazer)
library(fixest) 
library(jtools)

#Load data
Data_2001 <- read_csv("Analysis/Data/2001Census/Integrated_2001_cars_buffs.csv")

#Create categorical variables form buffer dummies
Data_2001$Buff250 <- ifelse(Data_2001$IN250 == '1', 'IN', 
                                         ifelse(Data_2001$OUT250 == '1', 'OUT', 'NA'))

Data_2001$Buff500 <- ifelse(Data_2001$IN500 == '1', 'IN', 
                                         ifelse(Data_2001$OUT500 == '1', 'OUT', 'NA'))

Data_2001$Buff750 <- ifelse(Data_2001$IN750 == '1', 'IN', 
                                         ifelse(Data_2001$OUT750 == '1', 'OUT', 'NA'))

Data_2001$Buff1000 <- ifelse(Data_2001$IN1000 == '1', 'IN', 
                                         ifelse(Data_2001$OUT1000 == '1', 'OUT', 'NA'))

Data_2001$BuffExtR250 <- ifelse(Data_2001$INExtR250 == '1', 'IN', 
                            ifelse(Data_2001$OUTExtR250 == '1', 'OUT', 'NA'))

Data_2001$BuffExtR500 <- ifelse(Data_2001$INExtR500 == '1', 'IN', 
                            ifelse(Data_2001$OUTExtR500 == '1', 'OUT', 'NA'))

Data_2001$BuffExtR750 <- ifelse(Data_2001$INExtR750 == '1', 'IN', 
                            ifelse(Data_2001$OUTExtR750 == '1', 'OUT', 'NA'))

Data_2001$BuffExtR1000 <- ifelse(Data_2001$INExtR1000 == '1', 'IN', 
                             ifelse(Data_2001$OUTExtR1000 == '1', 'OUT', 'NA'))

#Count number of Output Areas assigned to experimental groups
Data_2001 %>% count(Buff250)
Data_2001 %>% count(Buff500)
Data_2001 %>% count(Buff750)
Data_2001 %>% count(Buff1000)

#Disable scientific notation
options(scipen=999)

#Calculate percentage change in car ownership between 2001 and 2005 as well as 2005 and 2009 and show histograms
Data_2001$PCT_CHG_Cars_01_05 <- (((Data_2001$`2005` - Data_2001$`2001`)/
                              Data_2001$`2001`)*100)
hist(Data_2001$PCT_CHG_Cars_01_05, breaks = 50)

trim_q <- function(Data_2001, lb, ub){
  Data_2001[(Data_2001 > quantile(Data_2001, lb)) & (Data_2001 < quantile(Data_2001, ub))]
}
hist(trim_q(Data_2001$PCT_CHG_Cars_01_05, 0.01, 0.99), breaks =50, main = NULL, 
     xlab = 'Percentage Change in Private Car Registrations (2001-2005)', col = 'gray')

write.csv(Data_2001, "C:/Users/cvyya/OneDrive - Loughborough University/Lboro/Research/Papers/Craig_congestion/File_Name.csv", row.names=TRUE)

Data_2001$PCT_CHG_Cars_05_09 <- (((Data_2001$`2009` - Data_2001$`2005`)/
                              Data_2001$`2005`)*100)
hist(Data_2001$PCT_CHG_Cars_05_09, breaks = 50)

trim_q2 <- function(Data_2001, lb, ub){
  Data_2001[(Data_2001 > quantile(Data_2001, lb)) & (Data_2001 < quantile(Data_2001, ub))]
}
hist(trim_q2(Data_2001$PCT_CHG_Cars_05_09, 0.02, 0.98), breaks =50, main = NULL, 
     xlab = 'Percentage Change in Private Car Registrations (2005-2009)', col = 'gray')

#Subset data to only select OAs which are in treatment or control groups
Buff_250 <- subset(Data_2001, Buff250 != 'NA')
Buff_500 <- subset(Data_2001, Buff500 != 'NA')
Buff_750 <- subset(Data_2001, Buff750 != 'NA')
Buff_1000 <- subset(Data_2001, Buff1000 != 'NA')

Buff_250_ExtR <- subset(Data_2001, BuffExtR250 != 'NA')
Buff_500_ExtR <- subset(Data_2001, BuffExtR500 != 'NA')
Buff_750_ExtR <- subset(Data_2001, BuffExtR750 != 'NA')
Buff_1000_ExtR <- subset(Data_2001, BuffExtR1000 != 'NA')

#Boxplots of car ownership in 2003 and 2011 by buffer
CarsBuff250 <- ggplot(Buff_250, aes(x=Buff250, y=`2003`)) + 
  geom_boxplot(fill="gray") +
  labs(x = 'Output Areas Within a 250m Buffer', y = 'Private Car Registrations in 2003 (log)') +
  coord_flip() +
  scale_y_log10() +
  theme_light()
CarsBuff250

CarsBuff500 <- ggplot(Buff_500, aes(x=Buff500, y=`2003`)) + 
  labs(x = 'Output Areas Within a 500m Buffer', y = 'Private Car Registrations in 2003 (log)') +
  theme_light() +
  coord_flip() +
  scale_y_log10() +
  geom_boxplot(fill="gray")
CarsBuff500

CarsBuff750 <- ggplot(Buff_750, aes(x=Buff750, y=`2003`)) + 
  labs(x = 'Output Areas Within a 750m Buffer', y = 'Private Car Registrations in 2003 (log)') +
  theme_light() +
  coord_flip() +
  scale_y_log10() +
  geom_boxplot(fill="gray")
CarsBuff750

CarsBuff1000 <- ggplot(Buff_1000, aes(x=Buff1000, y=`2003`)) + 
  labs(x = 'Output Areas Within a 1000m Buffer', y = 'Private Car Registrations in 2003 (log)') +
  theme_light() +
  coord_flip() +
  scale_y_log10() +
  geom_boxplot(fill="gray")
CarsBuff1000

#Distance decay plot
Dist_mat <- read_csv("Analysis/Data/Dist_mat.csv")
Cars01 <- read_csv("Analysis/Data/VehicleStats/Vehicle_Stats_2001.csv")
Dist_mat_cars <- merge(Dist_mat, Cars01, by.x = "OA_2001", by.y = "OA_2001")
dis <- ggplot(Dist_mat_cars, aes(x = Dist_mat_cars$Distance, y = Dist_mat_cars$`2003`))+
  geom_point(size = 0.5) +
  stat_density_2d(aes(fill = ..level..), geom="polygon")+
  scale_fill_gradient(low="blue", high="red") +
  labs( x = 'Distance from Output Area to to Central Business District', y = ' Private Car Registrations in 2003') +
  lims (y = c(0,350))
dis

#Percentage change across treatment and control groups for different buffers
aggregate(Buff_250$PCT_CHG_Cars_01_05, list(Buff_250$Buff250), FUN=mean)
aggregate(Buff_500$PCT_CHG_Cars_01_05, list(Buff_500$Buff500), FUN=mean)
aggregate(Buff_750$PCT_CHG_Cars_01_05, list(Buff_750$Buff750), FUN=mean)
aggregate(Buff_1000$PCT_CHG_Cars_01_05, list(Buff_1000$Buff1000), FUN=mean)

aggregate(Buff_250_ExtR$PCT_CHG_Cars_05_09, list(Buff_250_ExtR$BuffExtR250), FUN=mean)
aggregate(Buff_500_ExtR$PCT_CHG_Cars_05_09, list(Buff_500_ExtR$BuffExtR500), FUN=mean)
aggregate(Buff_750_ExtR$PCT_CHG_Cars_05_09, list(Buff_750_ExtR$BuffExtR750), FUN=mean)
aggregate(Buff_1000_ExtR$PCT_CHG_Cars_05_09, list(Buff_1000_ExtR$BuffExtR1000), FUN=mean)

#Create car ownership timeline for different buffers
Mean_2000_250 <- aggregate(Data_2001$`2000`, list(Data_2001$Buff250), FUN=mean)
Mean_2001_250 <- aggregate(Data_2001$`2001`, list(Data_2001$Buff250), FUN=mean)
Mean_2002_250 <- aggregate(Data_2001$`2002`, list(Data_2001$Buff250), FUN=mean)
Mean_2003_250 <- aggregate(Data_2001$`2003`, list(Data_2001$Buff250), FUN=mean)
Mean_2004_250 <- aggregate(Data_2001$`2004`, list(Data_2001$Buff250), FUN=mean)
Mean_2005_250 <- aggregate(Data_2001$`2005`, list(Data_2001$Buff250), FUN=mean)
Mean_2006_250 <- aggregate(Data_2001$`2005`, list(Data_2001$Buff250), FUN=mean)

Time_250 <- cbind(Mean_2000_250,Mean_2001_250,Mean_2002_250,Mean_2003_250,
                  Mean_2004_250,Mean_2005_250,Mean_2006_250)
Time_250 <- Time_250[-c(3,5,7,9,11,13)]
names(Time_250)[1] <- '250Buff'
names(Time_250)[2] <- '2000'
names(Time_250)[3] <- '2001'
names(Time_250)[4] <- '2002'
names(Time_250)[5] <- '2003'
names(Time_250)[6] <- '2004'
names(Time_250)[7] <- '2005'
names(Time_250)[8] <- '2006'
Time_250 <-Time_250 %>% arrange(Time_250$'250Buff')

Time_250_long <- t(Time_250)
Time_250_long <- Time_250_long[-c(1), ]
Time_250_long <- cbind(rownames(Time_250_long), data.frame(Time_250_long, row.names=NULL))
Time_250_long <- as_tibble(Time_250_long)
names(Time_250_long)[1] <- 'Year'
names(Time_250_long)[2] <- 'In'
names(Time_250_long)[3] <- 'Rest'
names(Time_250_long)[4] <- 'Out'
Time_250_long <- gather(Time_250_long, Buffer, Cars, In:Out)
Time_250_long$Cars <- as.numeric(Time_250_long$Cars)
Time_250_long$Year <- as.factor(Time_250_long$Year)
Time_250_long$Buffer <- as.factor(Time_250_long$Buffer)

write.csv(Time_250_long, "Analysis/Data/VehicleStats/Time_250_long.csv")

Mean_2000_500 <- aggregate(Data_2001$`2000`, list(Data_2001$Buff500), FUN=mean)
Mean_2001_500 <- aggregate(Data_2001$`2001`, list(Data_2001$Buff500), FUN=mean)
Mean_2002_500 <- aggregate(Data_2001$`2002`, list(Data_2001$Buff500), FUN=mean)
Mean_2003_500 <- aggregate(Data_2001$`2003`, list(Data_2001$Buff500), FUN=mean)
Mean_2004_500 <- aggregate(Data_2001$`2004`, list(Data_2001$Buff500), FUN=mean)
Mean_2005_500 <- aggregate(Data_2001$`2005`, list(Data_2001$Buff500), FUN=mean)
Mean_2006_500 <- aggregate(Data_2001$`2006`, list(Data_2001$Buff500), FUN=mean)

Time_500 <- cbind(Mean_2000_500,Mean_2001_500,Mean_2002_500,Mean_2003_500,
                  Mean_2004_500,Mean_2005_500,Mean_2006_500)
Time_500 <- Time_500[-c(3,5,7,9,11,13)]
names(Time_500)[1] <- '500Buff'
names(Time_500)[2] <- '2000'
names(Time_500)[3] <- '2001'
names(Time_500)[4] <- '2002'
names(Time_500)[5] <- '2003'
names(Time_500)[6] <- '2004'
names(Time_500)[7] <- '2005'
names(Time_500)[8] <- '2006'
Time_500 <-Time_500 %>% arrange(Time_500$'500Buff')

Time_500_long <- t(Time_500)
Time_500_long <- Time_500_long[-c(1), ]
Time_500_long <- cbind(rownames(Time_500_long), data.frame(Time_500_long, row.names=NULL))
Time_500_long <- as_tibble(Time_500_long)
names(Time_500_long)[1] <- 'Year'
names(Time_500_long)[2] <- 'In'
names(Time_500_long)[3] <- 'Rest'
names(Time_500_long)[4] <- 'Out'
Time_500_long <- gather(Time_500_long, Buffer, Cars, In:Out)
Time_500_long$Cars <- as.numeric(Time_500_long$Cars)
Time_500_long$Year <- as.factor(Time_500_long$Year)
Time_500_long$Buffer <- as.factor(Time_500_long$Buffer)

write.csv(Time_500_long, "Analysis/Data/VehicleStats/Time_500_long.csv")

Mean_2000_750 <- aggregate(Data_2001$`2000`, list(Data_2001$Buff750), FUN=mean)
Mean_2001_750 <- aggregate(Data_2001$`2001`, list(Data_2001$Buff750), FUN=mean)
Mean_2002_750 <- aggregate(Data_2001$`2002`, list(Data_2001$Buff750), FUN=mean)
Mean_2003_750 <- aggregate(Data_2001$`2003`, list(Data_2001$Buff750), FUN=mean)
Mean_2004_750 <- aggregate(Data_2001$`2004`, list(Data_2001$Buff750), FUN=mean)
Mean_2005_750 <- aggregate(Data_2001$`2005`, list(Data_2001$Buff750), FUN=mean)
Mean_2006_750 <- aggregate(Data_2001$`2006`, list(Data_2001$Buff750), FUN=mean)

Time_750 <- cbind(Mean_2000_750,Mean_2001_750,Mean_2002_750,Mean_2003_750,
                  Mean_2004_750,Mean_2005_750,Mean_2006_750)
Time_750 <- Time_750[-c(3,5,7,9,11,13)]
names(Time_750)[1] <- '750Buff'
names(Time_750)[2] <- '2000'
names(Time_750)[3] <- '2001'
names(Time_750)[4] <- '2002'
names(Time_750)[5] <- '2003'
names(Time_750)[6] <- '2004'
names(Time_750)[7] <- '2005'
names(Time_750)[8] <- '2006'
Time_750 <-Time_750 %>% arrange(Time_750$'750Buff')

Time_750_long <- t(Time_750)
Time_750_long <- Time_750_long[-c(1), ]
Time_750_long <- cbind(rownames(Time_750_long), data.frame(Time_750_long, row.names=NULL))
Time_750_long <- as_tibble(Time_750_long)
names(Time_750_long)[1] <- 'Year'
names(Time_750_long)[2] <- 'In'
names(Time_750_long)[3] <- 'Rest'
names(Time_750_long)[4] <- 'Out'
Time_750_long <- gather(Time_750_long, Buffer, Cars, In:Out)
Time_750_long$Cars <- as.numeric(Time_750_long$Cars)
Time_750_long$Year <- as.factor(Time_750_long$Year)
Time_750_long$Buffer <- as.factor(Time_750_long$Buffer)

write.csv(Time_750_long, "Analysis/Data/VehicleStats/Time_750_long.csv")

Mean_2000_1000 <- aggregate(Data_2001$`2000`, list(Data_2001$Buff1000), FUN=mean)
Mean_2001_1000 <- aggregate(Data_2001$`2001`, list(Data_2001$Buff1000), FUN=mean)
Mean_2002_1000 <- aggregate(Data_2001$`2002`, list(Data_2001$Buff1000), FUN=mean)
Mean_2003_1000 <- aggregate(Data_2001$`2003`, list(Data_2001$Buff1000), FUN=mean)
Mean_2004_1000 <- aggregate(Data_2001$`2004`, list(Data_2001$Buff1000), FUN=mean)
Mean_2005_1000 <- aggregate(Data_2001$`2005`, list(Data_2001$Buff1000), FUN=mean)
Mean_2006_1000 <- aggregate(Data_2001$`2006`, list(Data_2001$Buff1000), FUN=mean)

Time_1000 <- cbind(Mean_2000_1000,Mean_2001_1000,Mean_2002_1000,Mean_2003_1000,
                   Mean_2004_1000,Mean_2005_1000,Mean_2006_1000)
Time_1000 <- Time_1000[-c(3,5,7,9,11,13)]
names(Time_1000)[1] <- '1000Buff'
names(Time_1000)[2] <- '2000'
names(Time_1000)[3] <- '2001'
names(Time_1000)[4] <- '2002'
names(Time_1000)[5] <- '2003'
names(Time_1000)[6] <- '2004'
names(Time_1000)[7] <- '2005'
names(Time_1000)[8] <- '2006'
Time_1000 <-Time_1000 %>% arrange(Time_1000$'1000Buff')

Time_1000_long <- t(Time_1000)
Time_1000_long <- Time_1000_long[-c(1), ]
Time_1000_long <- cbind(rownames(Time_1000_long), data.frame(Time_1000_long, row.names=NULL))
Time_1000_long <- as_tibble(Time_1000_long)
names(Time_1000_long)[1] <- 'Year'
names(Time_1000_long)[2] <- 'In'
names(Time_1000_long)[3] <- 'Rest'
names(Time_1000_long)[4] <- 'Out'
Time_1000_long <- gather(Time_1000_long, Buffer, Cars, In:Out)
Time_1000_long$Cars <- as.numeric(Time_1000_long$Cars)
Time_1000_long$Year <- as.factor(Time_1000_long$Year)
Time_1000_long$Buffer <- as.factor(Time_1000_long$Buffer)

write.csv(Time_1000_long, "Analysis/Data/VehicleStats/Time_1000_long.csv")

Mean_2001_250 <- aggregate(Data_2001$`2001`, list(Data_2001$BuffExtR250), FUN=mean)
Mean_2002_250 <- aggregate(Data_2001$`2002`, list(Data_2001$BuffExtR250), FUN=mean)
Mean_2003_250 <- aggregate(Data_2001$`2003`, list(Data_2001$BuffExtR250), FUN=mean)
Mean_2004_250 <- aggregate(Data_2001$`2004`, list(Data_2001$BuffExtR250), FUN=mean)
Mean_2005_250 <- aggregate(Data_2001$`2005`, list(Data_2001$BuffExtR250), FUN=mean)
Mean_2006_250 <- aggregate(Data_2001$`2006`, list(Data_2001$BuffExtR250), FUN=mean)
Mean_2007_250 <- aggregate(Data_2001$`2007`, list(Data_2001$BuffExtR250), FUN=mean)
Mean_2008_250 <- aggregate(Data_2001$`2008`, list(Data_2001$BuffExtR250), FUN=mean)
Mean_2009_250 <- aggregate(Data_2001$`2009`, list(Data_2001$BuffExtR250), FUN=mean)
Mean_2010_250 <- aggregate(Data_2001$`2010`, list(Data_2001$BuffExtR250), FUN=mean)
Mean_2011_250 <- aggregate(Data_2001$`2011`, list(Data_2001$BuffExtR250), FUN=mean)
Mean_2012_250 <- aggregate(Data_2001$`2012`, list(Data_2001$BuffExtR250), FUN=mean)
Mean_2013_250 <- aggregate(Data_2001$`2013`, list(Data_2001$BuffExtR250), FUN=mean)

Time_250_ExtR <- cbind(Mean_2001_250,Mean_2002_250,Mean_2003_250,Mean_2004_250,Mean_2005_250,
                       Mean_2006_250,Mean_2007_250,Mean_2008_250,Mean_2009_250,Mean_2010_250,
                       Mean_2011_250,Mean_2012_250,Mean_2013_250)
Time_250_ExtR <- Time_250_ExtR[-c(3,5,7,9,11,13,15,17,19,21,23,25)]
names(Time_250_ExtR)[1] <- '250BuffExtR'
names(Time_250_ExtR)[2] <- '2001'
names(Time_250_ExtR)[3] <- '2002'
names(Time_250_ExtR)[4] <- '2003'
names(Time_250_ExtR)[5] <- '2004'
names(Time_250_ExtR)[6] <- '2005'
names(Time_250_ExtR)[7] <- '2006'
names(Time_250_ExtR)[8] <- '2007'
names(Time_250_ExtR)[9] <- '2008'
names(Time_250_ExtR)[10] <- '2009'
names(Time_250_ExtR)[11] <- '2010'
names(Time_250_ExtR)[12] <- '2011'
names(Time_250_ExtR)[13] <- '2012'
names(Time_250_ExtR)[14] <- '2013'
Time_250_ExtR <-Time_250_ExtR %>% arrange(Time_250_ExtR$'250BuffExtR')

Time_250_ExtR_long <- t(Time_250_ExtR)
Time_250_ExtR_long <- Time_250_ExtR_long[-c(1), ]
Time_250_ExtR_long <- cbind(rownames(Time_250_ExtR_long), data.frame(Time_250_ExtR_long, row.names=NULL))
Time_250_ExtR_long <- as_tibble(Time_250_ExtR_long)
names(Time_250_ExtR_long)[1] <- 'Year'
names(Time_250_ExtR_long)[2] <- 'In'
names(Time_250_ExtR_long)[3] <- 'Rest'
names(Time_250_ExtR_long)[4] <- 'Out'
Time_250_ExtR_long <- gather(Time_250_ExtR_long, BuffExtRer, Cars, In:Out)
Time_250_ExtR_long$Cars <- as.numeric(Time_250_ExtR_long$Cars)
Time_250_ExtR_long$Year <- as.factor(Time_250_ExtR_long$Year)
Time_250_ExtR_long$BuffExtRer <- as.factor(Time_250_ExtR_long$BuffExtRer)

write.csv(Time_250_ExtR_long, "Analysis/Data/VehicleStats/Time_250_ExtR_long.csv")

Mean_2001_500 <- aggregate(Data_2001$`2001`, list(Data_2001$BuffExtR500), FUN=mean)
Mean_2002_500 <- aggregate(Data_2001$`2002`, list(Data_2001$BuffExtR500), FUN=mean)
Mean_2003_500 <- aggregate(Data_2001$`2003`, list(Data_2001$BuffExtR500), FUN=mean)
Mean_2004_500 <- aggregate(Data_2001$`2004`, list(Data_2001$BuffExtR500), FUN=mean)
Mean_2005_500 <- aggregate(Data_2001$`2005`, list(Data_2001$BuffExtR500), FUN=mean)
Mean_2006_500 <- aggregate(Data_2001$`2006`, list(Data_2001$BuffExtR500), FUN=mean)
Mean_2007_500 <- aggregate(Data_2001$`2007`, list(Data_2001$BuffExtR500), FUN=mean)
Mean_2008_500 <- aggregate(Data_2001$`2008`, list(Data_2001$BuffExtR500), FUN=mean)
Mean_2009_500 <- aggregate(Data_2001$`2009`, list(Data_2001$BuffExtR500), FUN=mean)
Mean_2010_500 <- aggregate(Data_2001$`2010`, list(Data_2001$BuffExtR500), FUN=mean)
Mean_2011_500 <- aggregate(Data_2001$`2011`, list(Data_2001$BuffExtR500), FUN=mean)
Mean_2012_500 <- aggregate(Data_2001$`2012`, list(Data_2001$BuffExtR500), FUN=mean)
Mean_2013_500 <- aggregate(Data_2001$`2013`, list(Data_2001$BuffExtR500), FUN=mean)

Time_500_ExtR <- cbind(Mean_2001_500,Mean_2002_500,Mean_2003_500,Mean_2004_500,Mean_2005_500,
                       Mean_2006_500,Mean_2007_500,Mean_2008_500,Mean_2009_500,Mean_2010_500,
                       Mean_2011_500,Mean_2012_500,Mean_2013_500)
Time_500_ExtR <- Time_500_ExtR[-c(3,5,7,9,11,13,15,17,19,21,23,25)]
names(Time_500_ExtR)[1] <- '500BuffExtR'
names(Time_500_ExtR)[2] <- '2001'
names(Time_500_ExtR)[3] <- '2002'
names(Time_500_ExtR)[4] <- '2003'
names(Time_500_ExtR)[5] <- '2004'
names(Time_500_ExtR)[6] <- '2005'
names(Time_500_ExtR)[7] <- '2006'
names(Time_500_ExtR)[8] <- '2007'
names(Time_500_ExtR)[9] <- '2008'
names(Time_500_ExtR)[10] <- '2009'
names(Time_500_ExtR)[11] <- '2010'
names(Time_500_ExtR)[12] <- '2011'
names(Time_500_ExtR)[13] <- '2012'
names(Time_500_ExtR)[14] <- '2013'
Time_500_ExtR <-Time_500_ExtR %>% arrange(Time_500_ExtR$'500BuffExtR')

Time_500_ExtR_long <- t(Time_500_ExtR)
Time_500_ExtR_long <- Time_500_ExtR_long[-c(1), ]
Time_500_ExtR_long <- cbind(rownames(Time_500_ExtR_long), data.frame(Time_500_ExtR_long, row.names=NULL))
Time_500_ExtR_long <- as_tibble(Time_500_ExtR_long)
names(Time_500_ExtR_long)[1] <- 'Year'
names(Time_500_ExtR_long)[2] <- 'In'
names(Time_500_ExtR_long)[3] <- 'Rest'
names(Time_500_ExtR_long)[4] <- 'Out'
Time_500_ExtR_long <- gather(Time_500_ExtR_long, BuffExtRer, Cars, In:Out)
Time_500_ExtR_long$Cars <- as.numeric(Time_500_ExtR_long$Cars)
Time_500_ExtR_long$Year <- as.factor(Time_500_ExtR_long$Year)
Time_500_ExtR_long$BuffExtRer <- as.factor(Time_500_ExtR_long$BuffExtRer)

write.csv(Time_500_ExtR_long, "Analysis/Data/VehicleStats/Time_500_ExtR_long.csv")

Mean_2001_750 <- aggregate(Data_2001$`2001`, list(Data_2001$BuffExtR750), FUN=mean)
Mean_2002_750 <- aggregate(Data_2001$`2002`, list(Data_2001$BuffExtR750), FUN=mean)
Mean_2003_750 <- aggregate(Data_2001$`2003`, list(Data_2001$BuffExtR750), FUN=mean)
Mean_2004_750 <- aggregate(Data_2001$`2004`, list(Data_2001$BuffExtR750), FUN=mean)
Mean_2005_750 <- aggregate(Data_2001$`2005`, list(Data_2001$BuffExtR750), FUN=mean)
Mean_2006_750 <- aggregate(Data_2001$`2006`, list(Data_2001$BuffExtR750), FUN=mean)
Mean_2007_750 <- aggregate(Data_2001$`2007`, list(Data_2001$BuffExtR750), FUN=mean)
Mean_2008_750 <- aggregate(Data_2001$`2008`, list(Data_2001$BuffExtR750), FUN=mean)
Mean_2009_750 <- aggregate(Data_2001$`2009`, list(Data_2001$BuffExtR750), FUN=mean)
Mean_2010_750 <- aggregate(Data_2001$`2010`, list(Data_2001$BuffExtR750), FUN=mean)
Mean_2011_750 <- aggregate(Data_2001$`2011`, list(Data_2001$BuffExtR750), FUN=mean)
Mean_2012_750 <- aggregate(Data_2001$`2012`, list(Data_2001$BuffExtR750), FUN=mean)
Mean_2013_750 <- aggregate(Data_2001$`2013`, list(Data_2001$BuffExtR750), FUN=mean)

Time_750_ExtR <- cbind(Mean_2001_750,Mean_2002_750,Mean_2003_750,Mean_2004_750,Mean_2005_750,
                       Mean_2006_750,Mean_2007_750,Mean_2008_750,Mean_2009_750,Mean_2010_750,
                       Mean_2011_750,Mean_2012_750,Mean_2013_750)
Time_750_ExtR <- Time_750_ExtR[-c(3,5,7,9,11,13,15,17,19,21,23,25)]
names(Time_750_ExtR)[1] <- '750BuffExtR'
names(Time_750_ExtR)[2] <- '2001'
names(Time_750_ExtR)[3] <- '2002'
names(Time_750_ExtR)[4] <- '2003'
names(Time_750_ExtR)[5] <- '2004'
names(Time_750_ExtR)[6] <- '2005'
names(Time_750_ExtR)[7] <- '2006'
names(Time_750_ExtR)[8] <- '2007'
names(Time_750_ExtR)[9] <- '2008'
names(Time_750_ExtR)[10] <- '2009'
names(Time_750_ExtR)[11] <- '2010'
names(Time_750_ExtR)[12] <- '2011'
names(Time_750_ExtR)[13] <- '2012'
names(Time_750_ExtR)[14] <- '2013'
Time_750_ExtR <-Time_750_ExtR %>% arrange(Time_750_ExtR$'750BuffExtR')

Time_750_ExtR_long <- t(Time_750_ExtR)
Time_750_ExtR_long <- Time_750_ExtR_long[-c(1), ]
Time_750_ExtR_long <- cbind(rownames(Time_750_ExtR_long), data.frame(Time_750_ExtR_long, row.names=NULL))
Time_750_ExtR_long <- as_tibble(Time_750_ExtR_long)
names(Time_750_ExtR_long)[1] <- 'Year'
names(Time_750_ExtR_long)[2] <- 'In'
names(Time_750_ExtR_long)[3] <- 'Rest'
names(Time_750_ExtR_long)[4] <- 'Out'
Time_750_ExtR_long <- gather(Time_750_ExtR_long, BuffExtRer, Cars, In:Out)
Time_750_ExtR_long$Cars <- as.numeric(Time_750_ExtR_long$Cars)
Time_750_ExtR_long$Year <- as.factor(Time_750_ExtR_long$Year)
Time_750_ExtR_long$BuffExtRer <- as.factor(Time_750_ExtR_long$BuffExtRer)

write.csv(Time_750_ExtR_long, "Analysis/Data/VehicleStats/Time_750_ExtR_long.csv")

Mean_2001_1000 <- aggregate(Data_2001$`2001`, list(Data_2001$BuffExtR1000), FUN=mean)
Mean_2002_1000 <- aggregate(Data_2001$`2002`, list(Data_2001$BuffExtR1000), FUN=mean)
Mean_2003_1000 <- aggregate(Data_2001$`2003`, list(Data_2001$BuffExtR1000), FUN=mean)
Mean_2004_1000 <- aggregate(Data_2001$`2004`, list(Data_2001$BuffExtR1000), FUN=mean)
Mean_2005_1000 <- aggregate(Data_2001$`2005`, list(Data_2001$BuffExtR1000), FUN=mean)
Mean_2006_1000 <- aggregate(Data_2001$`2006`, list(Data_2001$BuffExtR1000), FUN=mean)
Mean_2007_1000 <- aggregate(Data_2001$`2007`, list(Data_2001$BuffExtR1000), FUN=mean)
Mean_2008_1000 <- aggregate(Data_2001$`2008`, list(Data_2001$BuffExtR1000), FUN=mean)
Mean_2009_1000 <- aggregate(Data_2001$`2009`, list(Data_2001$BuffExtR1000), FUN=mean)
Mean_2010_1000 <- aggregate(Data_2001$`2010`, list(Data_2001$BuffExtR1000), FUN=mean)
Mean_2011_1000 <- aggregate(Data_2001$`2011`, list(Data_2001$BuffExtR1000), FUN=mean)
Mean_2012_1000 <- aggregate(Data_2001$`2012`, list(Data_2001$BuffExtR1000), FUN=mean)
Mean_2013_1000 <- aggregate(Data_2001$`2013`, list(Data_2001$BuffExtR1000), FUN=mean)

Time_1000_ExtR <- cbind(Mean_2001_1000,Mean_2002_1000,Mean_2003_1000,Mean_2004_1000,Mean_2005_1000,
                        Mean_2006_1000,Mean_2007_1000,Mean_2008_1000,Mean_2009_1000,Mean_2010_1000,
                        Mean_2011_1000,Mean_2012_1000,Mean_2013_1000)
Time_1000_ExtR <- Time_1000_ExtR[-c(3,5,7,9,11,13,15,17,19,21,23,25)]
names(Time_1000_ExtR)[1] <- '1000BuffExtR'
names(Time_1000_ExtR)[2] <- '2001'
names(Time_1000_ExtR)[3] <- '2002'
names(Time_1000_ExtR)[4] <- '2003'
names(Time_1000_ExtR)[5] <- '2004'
names(Time_1000_ExtR)[6] <- '2005'
names(Time_1000_ExtR)[7] <- '2006'
names(Time_1000_ExtR)[8] <- '2007'
names(Time_1000_ExtR)[9] <- '2008'
names(Time_1000_ExtR)[10] <- '2009'
names(Time_1000_ExtR)[11] <- '2010'
names(Time_1000_ExtR)[12] <- '2011'
names(Time_1000_ExtR)[13] <- '2012'
names(Time_1000_ExtR)[14] <- '2013'
Time_1000_ExtR <-Time_1000_ExtR %>% arrange(Time_1000_ExtR$'1000BuffExtR')

Time_1000_ExtR_long <- t(Time_1000_ExtR)
Time_1000_ExtR_long <- Time_1000_ExtR_long[-c(1), ]
Time_1000_ExtR_long <- cbind(rownames(Time_1000_ExtR_long), data.frame(Time_1000_ExtR_long, row.names=NULL))
Time_1000_ExtR_long <- as_tibble(Time_1000_ExtR_long)
names(Time_1000_ExtR_long)[1] <- 'Year'
names(Time_1000_ExtR_long)[2] <- 'In'
names(Time_1000_ExtR_long)[3] <- 'Rest'
names(Time_1000_ExtR_long)[4] <- 'Out'
Time_1000_ExtR_long <- gather(Time_1000_ExtR_long, BuffExtRer, Cars, In:Out)
Time_1000_ExtR_long$Cars <- as.numeric(Time_1000_ExtR_long$Cars)
Time_1000_ExtR_long$Year <- as.factor(Time_1000_ExtR_long$Year)
Time_1000_ExtR_long$BuffExtRer <- as.factor(Time_1000_ExtR_long$BuffExtRer)

write.csv(Time_1000_ExtR_long, "Analysis/Data/VehicleStats/Time_1000_ExtR_long.csv")

#Time series charts for car registrations by different buffers
Time_250_long <- read_csv("Analysis/Data/VehicleStats/Time_250_long.csv")

Time_250_long_NA = subset(Time_250_long, Time_250_long$Buffer != 'Rest')

p1 <- ggplot() + geom_line(aes(y = Cars, x = Year, colour = Buffer), size=1.5,
                           data = Time_250_long_NA, stat="identity") +
  ylab("Private Car Registrations") +
  xlab("Year") +
  scale_x_continuous(breaks = seq(2000, 2006, 1)) +
  geom_vline(xintercept = 2003) +
  geom_vline(xintercept = 2001, linetype = 'dotted') +
  geom_vline(xintercept = 2005, linetype = 'dotted') +
  theme(axis.title.y=element_text(color = "black", face="bold")) +
  theme(axis.title.x=element_text(color = "black", face="bold")) +
  theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank())
p1

Time_500_long <- read_csv("Analysis/Data/VehicleStats/Time_500_long.csv")

Time_500_long_NA = subset(Time_500_long, Time_500_long$Buffer != 'Rest')

p2 <- ggplot() + geom_line(aes(y = Cars, x = Year, colour = Buffer), size=1.5,
                           data = Time_500_long_NA, stat="identity") +
  ylab("Private Car Registrations") +
  xlab("Year") +
  scale_x_continuous(breaks = seq(2000, 2006, 1)) +
  geom_vline(xintercept = 2003) +
  geom_vline(xintercept = 2001, linetype = 'dotted') +
  geom_vline(xintercept = 2005, linetype = 'dotted') +
  theme(axis.title.y=element_text(color = "black", face="bold")) +
  theme(axis.title.x=element_text(color = "black", face="bold")) +
  theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank())
p2

Time_750_long <- read_csv("Analysis/Data/VehicleStats/Time_750_long.csv")

Time_750_long_NA = subset(Time_750_long, Time_750_long$Buffer != 'Rest')

p3 <- ggplot() + geom_line(aes(y = Cars, x = Year, colour = Buffer), size=1.5,
                           data = Time_750_long_NA, stat="identity") +
  ylab("Private Car Registrations") +
  xlab("Year") +
  scale_x_continuous(breaks = seq(2000, 2006, 1)) +
  geom_vline(xintercept = 2003) +
  geom_vline(xintercept = 2001, linetype = 'dotted') +
  geom_vline(xintercept = 2005, linetype = 'dotted') +
  theme(axis.title.y=element_text(color = "black", face="bold")) +
  theme(axis.title.x=element_text(color = "black", face="bold")) +
  theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank())
p3

Time_1000_long <- read_csv("Analysis/Data/VehicleStats/Time_1000_long.csv")

Time_1000_long_NA = subset(Time_1000_long, Time_1000_long$Buffer != 'Rest')

p4 <- ggplot() + geom_line(aes(y = Cars, x = Year, colour = Buffer), size=1.5,
                           data = Time_1000_long_NA, stat="identity") +
  ylab("Private Car Registrations") +
  xlab("Year") +
  scale_x_continuous(breaks = seq(2000, 2006, 1)) +
  geom_vline(xintercept = 2003) +
  geom_vline(xintercept = 2001, linetype = 'dotted') +
  geom_vline(xintercept = 2005, linetype = 'dotted') +
  theme(axis.title.y=element_text(color = "black", face="bold")) +
  theme(axis.title.x=element_text(color = "black", face="bold")) +
  theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank())
p4

Time_250_ExtR_long <- read_csv("Analysis/Data/VehicleStats/Time_250_ExtR_long.csv")

Time_250_ExtR_long_NA = subset(Time_250_ExtR_long, Time_250_ExtR_long$BuffExtRer != 'Rest')

p5 <- ggplot() + geom_line(aes(y = Cars, x = Year, colour = BuffExtRer), size=1.5,
                           data = Time_250_ExtR_long_NA, stat="identity") +
  ylab("Private Car Registrations") +
  xlab("Year") +
  scale_x_continuous(breaks = seq(2001, 2013, 1)) +
  geom_vline(xintercept = 2007) +
  geom_vline(xintercept = 2005, linetype = 'dotted') +
  geom_vline(xintercept = 2009, linetype = 'dotted') +
  theme(axis.title.y=element_text(color = "black", face="bold")) +
  theme(axis.title.x=element_text(color = "black", face="bold")) +
  theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank())
p5

Time_500_ExtR_long <- read_csv("Analysis/Data/VehicleStats/Time_500_ExtR_long.csv")

Time_500_ExtR_long_NA = subset(Time_500_ExtR_long, Time_500_ExtR_long$BuffExtRer != 'Rest')

p6 <- ggplot() + geom_line(aes(y = Cars, x = Year, colour = BuffExtRer), size=1.5,
                           data = Time_500_ExtR_long_NA, stat="identity") +
  ylab("Private Car Registrations") +
  xlab("Year") +
  scale_x_continuous(breaks = seq(2001, 2013, 1)) +
  geom_vline(xintercept = 2007) +
  geom_vline(xintercept = 2005, linetype = 'dotted') +
  geom_vline(xintercept = 2009, linetype = 'dotted') +
  theme(axis.title.y=element_text(color = "black", face="bold")) +
  theme(axis.title.x=element_text(color = "black", face="bold")) +
  theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank())
p6

Time_750_ExtR_long <- read_csv("Analysis/Data/VehicleStats/Time_750_ExtR_long.csv")

Time_750_ExtR_long_NA = subset(Time_750_ExtR_long, Time_750_ExtR_long$BuffExtRer != 'Rest')

p7 <- ggplot() + geom_line(aes(y = Cars, x = Year, colour = BuffExtRer), size=1.5,
                           data = Time_750_ExtR_long_NA, stat="identity") +
  ylab("Private Car Registrations") +
  xlab("Year") +
  scale_x_continuous(breaks = seq(2001, 2013, 1)) +
  geom_vline(xintercept = 2007) +
  geom_vline(xintercept = 2005, linetype = 'dotted') +
  geom_vline(xintercept = 2009, linetype = 'dotted') +
  theme(axis.title.y=element_text(color = "black", face="bold")) +
  theme(axis.title.x=element_text(color = "black", face="bold")) +
  theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank())
p7

Time_1000_ExtR_long <- read_csv("Analysis/Data/VehicleStats/Time_1000_ExtR_long.csv")

Time_1000_ExtR_long_NA = subset(Time_1000_ExtR_long, Time_1000_ExtR_long$BuffExtRer != 'Rest')

p8 <- ggplot() + geom_line(aes(y = Cars, x = Year, colour = BuffExtRer), size=1.5,
                           data = Time_1000_ExtR_long_NA, stat="identity") +
  ylab("Private Car Registrations") +
  xlab("Year") +
  scale_x_continuous(breaks = seq(2001, 2013, 1)) +
  geom_vline(xintercept = 2007) +
  geom_vline(xintercept = 2005, linetype = 'dotted') +
  geom_vline(xintercept = 2009, linetype = 'dotted') +
  theme(axis.title.y=element_text(color = "black", face="bold")) +
  theme(axis.title.x=element_text(color = "black", face="bold")) +
  theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank())
p8

Time_2500_long <- read_csv("Analysis/Data/VehicleStats/Time_2500_long.csv")

Time_2500_long_NA = subset(Time_2500_long, Time_2500_long$Buffer != 'Rest')

p9 <- ggplot() + geom_line(aes(y = Cars, x = Year, colour = Buffer), size=1.5,
                           data = Time_2500_long_NA, stat="identity") +
  ylab("Private Car Registrations") +
  xlab("Year") +
  scale_x_continuous(breaks = seq(2000, 2006, 1)) +
  geom_vline(xintercept = 2003) +
  geom_vline(xintercept = 2001, linetype = 'dotted') +
  geom_vline(xintercept = 2005, linetype = 'dotted') +
  theme(axis.title.y=element_text(color = "black", face="bold")) +
  theme(axis.title.x=element_text(color = "black", face="bold")) +
  theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank())
p9

#Turn data from wide to long for DiD

Data_250_Long <- Buff_250[c(2,22,26,60)]
Data_250_Long <- gather(Data_250_Long , Buff250, cars,`2001`:`2005`)
Data_250_Long <- merge(x = Data_250_Long, y = Buff_250[ , c("OA_2001", "Buff250", 
                        "EcoActivePCT2001", "Drive_PCT2001", "Flats_PCT2001", "PopD2001")], by = "OA_2001", all.x=TRUE)

names(Data_250_Long)[2] <- 'Year'
names(Data_250_Long)[4] <- 'Buffer'

Data_250_Long$TimeDummy <- ifelse(Data_250_Long$Year == '2005', '1', '0')
Data_250_Long$BuffDummy <- ifelse(Data_250_Long$Buffer == 'OUT', '1', '0')
Data_250_Long$TimeBuffDummy <- ifelse(Data_250_Long$Buffer == 'OUT' & Data_250_Long$Year == '2005' , '1', '0')

Data_500_Long <- Buff_500[c(2,22,26,61)]
Data_500_Long <- gather(Data_500_Long , Buff500, cars,`2001`:`2005`)
Data_500_Long <- merge(x = Data_500_Long, y = Buff_500[ , c("OA_2001", "Buff500",
                                                            "EcoActivePCT2001", "Drive_PCT2001", "Flats_PCT2001", "PopD2001")], by = "OA_2001", all.x=TRUE)

names(Data_500_Long)[2] <- 'Year'
names(Data_500_Long)[4] <- 'Buffer'

Data_500_Long$TimeDummy <- ifelse(Data_500_Long$Year == '2005', '1', '0')
Data_500_Long$BuffDummy <- ifelse(Data_500_Long$Buffer == 'OUT', '1', '0')
Data_500_Long$TimeBuffDummy <- ifelse(Data_500_Long$Buffer == 'OUT' & Data_500_Long$Year == '2005' , '1', '0')

Data_750_Long <- Buff_750[c(2,22,26,62)]
Data_750_Long <- gather(Data_750_Long , Buff750, cars,`2001`:`2005`)
Data_750_Long <- merge(x = Data_750_Long, y = Buff_750[ , c("OA_2001", "Buff750",
                                                            "EcoActivePCT2001", "Drive_PCT2001", "Flats_PCT2001", "PopD2001")], by = "OA_2001", all.x=TRUE)

names(Data_750_Long)[2] <- 'Year'
names(Data_750_Long)[4] <- 'Buffer'

Data_750_Long$TimeDummy <- ifelse(Data_750_Long$Year == '2005', '1', '0')
Data_750_Long$BuffDummy <- ifelse(Data_750_Long$Buffer == 'OUT', '1', '0')
Data_750_Long$TimeBuffDummy <- ifelse(Data_750_Long$Buffer == 'OUT' & Data_750_Long$Year == '2005' , '1', '0')

Data_1000_Long <- Buff_1000[c(2,22,26,63)]
Data_1000_Long <- gather(Data_1000_Long , Buff1000, cars,`2001`:`2005`)
Data_1000_Long <- merge(x = Data_1000_Long, y = Buff_1000[ , c("OA_2001", "Buff1000",
                                                               "EcoActivePCT2001", "Drive_PCT2001", "Flats_PCT2001", "PopD2001")], by = "OA_2001", all.x=TRUE)

names(Data_1000_Long)[2] <- 'Year'
names(Data_1000_Long)[4] <- 'Buffer'

Data_1000_Long$TimeDummy <- ifelse(Data_1000_Long$Year == '2005', '1', '0')
Data_1000_Long$BuffDummy <- ifelse(Data_1000_Long$Buffer == 'OUT', '1', '0')
Data_1000_Long$TimeBuffDummy <- ifelse(Data_1000_Long$Buffer == 'OUT' & Data_1000_Long$Year == '2005' , '1', '0')

#OLS-DiD Models

mod1 <- log(cars) ~ BuffDummy + TimeDummy + TimeBuffDummy
mod2 <- log(cars) ~ EcoActivePCT2001 + Drive_PCT2001 + Flats_PCT2001 + PopD2001
mod3 <- log(cars) ~ EcoActivePCT2001 + Drive_PCT2001 + Flats_PCT2001 + PopD2001 + BuffDummy + TimeDummy + TimeBuffDummy

OLSDiD_250 <- lm(formula = mod3, data = Data_250_Long)
summary(OLSDiD_250)
par(mfrow=c(2,2))
plot(OLSDiD_250, main = "Model with 250m Buffers")
bptest(OLSDiD_250)

OLSDiD_500 <- lm(formula = mod3, data = Data_500_Long)
summary(OLSDiD_500)
par(mfrow=c(2,2))
plot(OLSDiD_500, main = "Model with 500m Buffers")
bptest(OLSDiD_500)

OLSDiD_750 <- lm(formula = mod3, data = Data_750_Long)
summary(OLSDiD_750)
par(mfrow=c(2,2))
plot(OLSDiD_750, main = "Model with 750m Buffers")
bptest(OLSDiD_750)

OLSDiD_1000 <- lm(formula = mod3, data = Data_1000_Long)
summary(OLSDiD_1000)
par(mfrow=c(2,2))
plot(OLSDiD_1000, main = "Model with 1000m Buffers")
bptest(OLSDiD_1000)

#Panel DID

FEDiD_250 <- plm(formula = mod3, data = Data_250_Long, index = c("OA_2001", "Year"), 
                 model = "within", effect = 'twoways')
summary(FEDiD_250)

REDiD_250 <- plm(formula = mod3, data = Data_250_Long, index = c("OA_2001", "Year"), 
                 model = "random", random.method = 'walhus')
summary(REDiD_250)

phtest(FEDiD_250, REDiD_250)

FEDiD_500 <- plm(formula = mod3, data = Data_500_Long, index = c("OA_2001", "Year"), 
                 model = "within", effect = 'twoways')
summary(FEDiD_500)

REDiD_500 <- plm(formula = mod3, data = Data_500_Long, index = c("OA_2001", "Year"), 
                 model = "random", random.method = 'walhus')
summary(REDiD_500)

phtest(FEDiD_500, REDiD_500)

FEDiD_750 <- plm(formula = mod3, data = Data_750_Long, index = c("OA_2001", "Year"), 
                 model = "within", effect = 'twoways')
summary(FEDiD_750)

REDiD_750 <- plm(formula = mod3, data = Data_750_Long, index = c("OA_2001", "Year"), 
                 model = "random", random.method = 'walhus')
summary(REDiD_750)

phtest(FEDiD_750, REDiD_750)

FEDiD_1000 <- plm(formula = mod3, data = Data_1000_Long, index = c("OA_2001", "Year"), 
                  model = "within", effect = 'twoways')
summary(FEDiD_1000)

REDiD_1000 <- plm(formula = mod3, data = Data_1000_Long, index = c("OA_2001", "Year"), 
                 model = "random", random.method = 'walhus')
summary(REDiD_1000)
sqrt(mean((Data_1000_Long$cars - fitted.values(REDiD_1000))^2))

phtest(FEDiD_1000, REDiD_1000)

stargazer(REDiD_250, REDiD_500, REDiD_750, REDiD_1000, type = 'text')
stargazer(FEDiD_250, FEDiD_500, FEDiD_750, FEDiD_1000, type = 'text')

# Load Shapefile and create weights matrix

OA_2001_250 <- st_read("Analysis/Shapefiles/OA2001", "OA_2001_250") 
OA_2001_250 <- OA_2001_250[order(OA_2001_250$OA_2001),]
summary(OA_2001_250)

OA_2001_250_nb <- poly2nb(OA_2001_250, queen = TRUE)
OA_2001_250_nb_listw <- nb2listw(OA_2001_250_nb)

OA_2001_500 <- st_read("Analysis/Shapefiles/OA2001", "OA_2001_500") 
OA_2001_500 <- OA_2001_500[order(OA_2001_500$OA_2001),]
summary(OA_2001_500)

OA_2001_500_nb <- poly2nb(OA_2001_500, queen = TRUE)
OA_2001_500_nb_listw <- nb2listw(OA_2001_500_nb)

OA_2001_750 <- st_read("Analysis/Shapefiles/OA2001", "OA_2001_750") 
OA_2001_750 <- OA_2001_750[order(OA_2001_750$OA_2001),]
summary(OA_2001_750)

OA_2001_750_nb <- poly2nb(OA_2001_750, queen = TRUE)
OA_2001_750_nb_listw <- nb2listw(OA_2001_750_nb)

OA_2001_1000 <- st_read("Analysis/Shapefiles/OA2001", "OA_2001_1000") 
OA_2001_1000 <- OA_2001_1000[order(OA_2001_1000$OA_2001),]
summary(OA_2001_1000)

OA_2001_1000_nb <- poly2nb(OA_2001_1000, queen = TRUE)
OA_2001_1000_nb_listw <- nb2listw(OA_2001_1000_nb)

# spatial Weights for K-nearest and distance based

OA_2001_250_coords <- coordinates(as(OA_2001_250,"Spatial"))
OA_2001_250_k4 <- knn2nb(knearneigh(OA_2001_250_coords, k=4))
OA_2001_250_k4_listw <- nb2listw(OA_2001_250_k4)

OA_2001_250_dsts <- unlist(nbdists(OA_2001_250_k4, OA_2001_250_coords))
summary(OA_2001_250_dsts)

OA_2001_250_dist <- dnearneigh(OA_2001_250_coords, d1=0, d2=556) 
OA_2001_250_dist_listw <- nb2listw(OA_2001_250_dist)

OA_2001_500_coords <- coordinates(as(OA_2001_500,"Spatial"))
OA_2001_500_k4 <- knn2nb(knearneigh(OA_2001_500_coords, k=4))
OA_2001_500_k4_listw <- nb2listw(OA_2001_500_k4)

OA_2001_500_dsts <- unlist(nbdists(OA_2001_500_k4, OA_2001_500_coords))
summary(OA_2001_500_dsts)

OA_2001_500_dist <- dnearneigh(OA_2001_500_coords, d1=0, d2=556) 
OA_2001_500_dist_listw <- nb2listw(OA_2001_500_dist)

OA_2001_750_coords <- coordinates(as(OA_2001_750,"Spatial"))
OA_2001_750_k4 <- knn2nb(knearneigh(OA_2001_750_coords, k=4))
OA_2001_750_k4_listw <- nb2listw(OA_2001_750_k4)

OA_2001_750_dsts <- unlist(nbdists(OA_2001_750_k4, OA_2001_750_coords))
summary(OA_2001_750_dsts)

OA_2001_750_dist <- dnearneigh(OA_2001_750_coords, d1=0, d2=556) 
OA_2001_750_dist_listw <- nb2listw(OA_2001_750_dist)

OA_2001_1000_coords <- coordinates(as(OA_2001_1000,"Spatial"))
OA_2001_1000_k4 <- knn2nb(knearneigh(OA_2001_1000_coords, k=4))
OA_2001_1000_k4_listw <- nb2listw(OA_2001_1000_k4)

OA_2001_1000_dsts <- unlist(nbdists(OA_2001_1000_k4, OA_2001_1000_coords))
summary(OA_2001_1000_dsts)

OA_2001_1000_dist <- dnearneigh(OA_2001_1000_coords, d1=0, d2=592)
OA_2001_1000_dist_listw <- nb2listw(OA_2001_1000_dist)

# Remove nieghbourless spatial units and zero car spatial units from data

Data_250_Long_R <- filter(Data_250_Long, Data_250_Long$OA_2001 != 'E00023812' & Data_250_Long$OA_2001 != 'E00022974')
Data_500_Long_R <- filter(Data_500_Long, Data_500_Long$OA_2001 != 'E00023812' & Data_500_Long$OA_2001 != 'E00004186')
Data_750_Long_R <- filter(Data_750_Long, Data_750_Long$OA_2001 != 'E00023812' & Data_750_Long$OA_2001 != 'E00004186')                          
Data_1000_Long_R <- filter(Data_1000_Long, Data_1000_Long$OA_2001 != 'E00023812' & Data_1000_Long$OA_2001 != 'E00004186')  

# Spatial Diagnostics

slmtest(mod3, data = Data_250_Long_R, listw = OA_2001_250_nb_listw, test="rlme")
slmtest(mod3, data = Data_250_Long_R, listw = OA_2001_250_nb_listw, test="rlml")
bsktest(x = mod3, data = Data_250_Long_R, listw = OA_2001_250_nb_listw, test = 'CLMlambda')
bsktest(x = mod3, data = Data_250_Long_R, listw = OA_2001_250_nb_listw, test = 'CLMmu')

slmtest(mod3, data = Data_500_Long_R, listw = OA_2001_500_nb_listw, test="rlme")
slmtest(mod3, data = Data_500_Long_R, listw = OA_2001_500_nb_listw, test="rlml")
bsktest(x = mod3, data = Data_500_Long_R, listw = OA_2001_500_nb_listw, test = 'CLMlambda')
bsktest(x = mod3, data = Data_500_Long_R, listw = OA_2001_500_nb_listw, test = 'CLMmu')

slmtest(mod3, data = Data_750_Long_R, listw = OA_2001_750_nb_listw, test="rlme")
slmtest(mod3, data = Data_750_Long_R, listw = OA_2001_750_nb_listw, test="rlml")
bsktest(x = mod3, data = Data_750_Long_R, listw = OA_2001_750_nb_listw, test = 'CLMlambda')
bsktest(x = mod3, data = Data_750_Long_R, listw = OA_2001_750_nb_listw, test = 'CLMmu')

slmtest(mod3, data = Data_1000_Long_R, listw = OA_2001_1000_nb_listw, test="rlme")
slmtest(mod3, data = Data_1000_Long_R, listw = OA_2001_1000_nb_listw, test="rlml")
bsktest(x = mod3, data = Data_1000_Long_R, listw = OA_2001_1000_nb_listw, test = 'CLMlambda')
bsktest(x = mod3, data = Data_1000_Long_R, listw = OA_2001_1000_nb_listw, test = 'CLMmu')

#Spatial Hausman Tests

sphtest(x = mod3, data = Data_250_Long_R, listw = OA_2001_250_nb_listw, spatial.model = 'lag', method = 'GM')
sphtest(x = mod3, data = Data_500_Long_R, listw = OA_2001_500_nb_listw, spatial.model = 'lag', method = 'GM')
sphtest(x = mod3, data = Data_750_Long_R, listw = OA_2001_750_nb_listw, spatial.model = 'lag', method = 'GM')
sphtest(x = mod3, data = Data_1000_Long_R, listw = OA_2001_1000_nb_listw, spatial.model = 'lag', method = 'GM')

#Spatial autocorrelation analysis

Data_250_Long_R_2001 <- subset(Data_250_Long_R, Year == '2001')
moran.test(Data_250_Long_R_2001$cars, OA_2001_250_nb_listw)
moran.plot(Data_250_Long_R_2001$cars, OA_2001_250_nb_listw)

Data_500_Long_R_2001 <- subset(Data_500_Long_R, Year == '2001')
moran.test(Data_500_Long_R_2001$cars, OA_2001_500_nb_listw)
moran.plot(Data_500_Long_R_2001$cars, OA_2001_500_nb_listw)

Data_750_Long_R_2001 <- subset(Data_750_Long_R, Year == '2001')
moran.test(Data_750_Long_R_2001$cars, OA_2001_750_nb_listw)
moran.plot(Data_750_Long_R_2001$cars, OA_2001_750_nb_listw)

Data_1000_Long_R_2001 <- subset(Data_1000_Long_R, Year == '2001')
moran.test(Data_1000_Long_R_2001$cars, OA_2001_1000_nb_listw)
moran.plot(Data_1000_Long_R_2001$cars, OA_2001_1000_nb_listw)

# Spatial Panel DIDs using ML estimation

sararremod_250 <- spml(mod3, data = Data_250_Long_R, listw = OA_2001_250_dist_listw,
                  model="random", spatial.error="none", lag=TRUE, effect = 'individual')
summary(sararremod_250)
residuals_250 <- residuals(sararremod_250)
impacts_250 <- impacts(sararremod_250, listw = OA_2001_250_nb_listw, time = 3)
summary(impac1, zstats=TRUE, short=TRUE)
sararremod_250$logLik

sararremod_500 <- spml(mod3, data = Data_500_Long_R, listw = OA_2001_500_nb_listw,
                       model="random", spatial.error="none", lag=TRUE, effect = 'individual')
residuals_500 <- residuals(sararremod_500)
summary(sararremod_500)
sararremod_500$logLik

sararremod_750 <- spml(mod3, data = Data_750_Long_R, listw = OA_2001_750_nb_listw,
                       model="random", spatial.error="none", lag=TRUE, effect = 'individual')
residuals_750 <- residuals(sararremod_750)
summary(sararremod_750)
sararremod_750$logLik

sararremod_1000 <- spml(mod3, data = Data_1000_Long_R, listw = OA_2001_1000_nb_listw,
                       model="random", spatial.error="none", lag=TRUE, effect = 'individual')
residuals_1000 <- residuals(sararremod_1000)
summary(sararremod_1000)
sqrt(mean((Data_1000_Long_R$cars - fitted.values(sararremod_1000))^2))
sararremod_1000$logLik

# Spatial Panel DiDs using GM estimator

GM_lag_250 <- spgm(formula = mod3, data = Data_250_Long_R, listw = OA_2001_250_nb_listw, lag = TRUE,
                   moments = "fullweights", model = "random", spatial.error = TRUE)
summary(GM_lag_250)

GM_lag_500 <- spgm(formula = mod3, data = Data_500_Long_R, listw = OA_2001_500_nb_listw, lag = TRUE,
                    moments = "fullweights", model = "random", spatial.error = TRUE)
residuals_500 <- residuals(GM_lag_500)
summary(GM_lag_500)

GM_lag_750 <- spgm(formula = mod3, data = Data_750_Long_R, listw = OA_2001_750_nb_listw, lag = TRUE,
                    moments = "fullweight", model = "random", spatial.error = TRUE)
summary(GM_lag_750)

GM_lag_1000 <- spgm(formula = mod3, data = Data_1000_Long_R, listw = OA_2001_1000_nb_listw, lag = TRUE,
                   moments = "fullweights", model = "random", spatial.error = TRUE)
summary(GM_lag_1000)

# Spatial Panel DiDs with K-nearest and Distance based spatial weights

sararremod_250_k4 <- spml(mod3, data = Data_250_Long_R, listw = OA_2001_250_k4_listw,
                       model="random", spatial.error="none", lag=TRUE, effect = 'individual')
summary(sararremod_250_k4)
residuals_250_k4 <- residuals(sararremod_250_k4)
sararremod_250_k4$logLik

sararremod_500_k4 <- spml(mod3, data = Data_500_Long_R, listw = OA_2001_500_k4_listw,
                       model="random", spatial.error="none", lag=TRUE, effect = 'individual')
residuals_500_k4 <- residuals(sararremod_500_k4)
summary(sararremod_500_k4)

sararremod_750_k4 <- spml(mod3, data = Data_750_Long_R, listw = OA_2001_750_k4_listw,
                       model="random", spatial.error="none", lag=TRUE, effect = 'individual')
residuals_750_k4 <- residuals(sararremod_750_k4)
summary(sararremod_750_k4)

sararremod_1000_k4 <- spml(mod3, data = Data_1000_Long_R, listw = OA_2001_1000_k4_listw,
                        model="random", spatial.error="none", lag=TRUE, effect = 'individual')
residuals_1000_k4 <- residuals(sararremod_1000_k4)
summary(sararremod_1000_k4)

sararremod_250_dist <- spml(mod3, data = Data_250_Long_R, listw = OA_2001_250_dist_listw,
                            model="random", spatial.error="none", lag=TRUE, effect = 'individual')
summary(sararremod_250_dist)
residuals_250_dist <- residuals(sararremod_250_dist)
sararremod_250_dist$logLik

sararremod_500_dist <- spml(mod3, data = Data_500_Long_R, listw = OA_2001_500_dist_listw,
                            model="random", spatial.error="none", lag=TRUE, effect = 'individual')
residuals_500_dist <- residuals(sararremod_500_dist)
summary(sararremod_500_dist)

sararremod_750_dist <- spml(mod3, data = Data_750_Long_R, listw = OA_2001_750_dist_listw,
                            model="random", spatial.error="none", lag=TRUE, effect = 'individual')
residuals_750_dist <- residuals(sararremod_750_dist)
summary(sararremod_750_dist)

sararremod_1000_dist <- spml(mod3, data = Data_1000_Long_R, listw = OA_2001_1000_dist_listw,
                             model="random", spatial.error="none", lag=TRUE, effect = 'individual')
residuals_1000_dist <- residuals(sararremod_1000_dist)
summary(sararremod_1000_dist)

# Spatial dependence tests on SARRAR model residuals 

Data_250_Long_R_residuals <- cbind(Data_250_Long_R, residuals_250, residuals_250_k4, residuals_250_dist)
Data_250_Long_R_residuals <- subset(Data_250_Long_R_residuals, Year == '2005')
moran.test(Data_250_Long_R_residuals$residuals_250, OA_2001_250_nb_listw)
moran.test(Data_250_Long_R_residuals$residuals_250_k4, OA_2001_250_k4_listw)
moran.test(Data_250_Long_R_residuals$residuals_250_dist, OA_2001_250_dist_listw)
moran.mc(Data_250_Long_R_residuals$residuals_250, OA_2001_250_nb_listw, nsim=999, alternative="greater")
moran.mc(Data_250_Long_R_residuals$residuals_250_k4, OA_2001_250_k4_listw, nsim=999, alternative="greater")
moran.mc(Data_250_Long_R_residuals$residuals_250_dist, OA_2001_250_dist_listw, nsim=999, alternative="greater")
moran.plot(Data_250_Long_R_residuals$residuals_250, OA_2001_250_nb_listw)
moran.plot(Data_250_Long_R_residuals$residuals_250_k4, OA_2001_250_k4_listw)
moran.plot(Data_250_Long_R_residuals$residuals_250_dist, OA_2001_250_dist_listw)

Data_500_Long_R_residuals <- cbind(Data_500_Long_R, residuals_500, residuals_500_k4, residuals_500_dist)
Data_500_Long_R_residuals <- subset(Data_500_Long_R_residuals, Year == '2005')
moran.test(Data_500_Long_R_residuals$residuals_500, OA_2001_500_nb_listw)
moran.test(Data_500_Long_R_residuals$residuals_500_k4, OA_2001_500_k4_listw)
moran.test(Data_500_Long_R_residuals$residuals_500_dist, OA_2001_500_dist_listw)
moran.mc(Data_500_Long_R_residuals$residuals_500, OA_2001_500_nb_listw, nsim=999, alternative="greater")
moran.mc(Data_500_Long_R_residuals$residuals_500_k4, OA_2001_500_k4_listw, nsim=999, alternative="greater")
moran.mc(Data_500_Long_R_residuals$residuals_500_dist, OA_2001_500_dist_listw, nsim=999, alternative="greater")
moran.plot(Data_500_Long_R_residuals$residuals_500, OA_2001_500_nb_listw)
moran.plot(Data_500_Long_R_residuals$residuals_500_k4, OA_2001_500_k4_listw)
moran.plot(Data_500_Long_R_residuals$residuals_500_dist, OA_2001_500_dist_listw)

Data_750_Long_R_residuals <- cbind(Data_750_Long_R, residuals_750, residuals_750_k4, residuals_750_dist)
Data_750_Long_R_residuals <- subset(Data_750_Long_R_residuals, Year == '2005')
moran.test(Data_750_Long_R_residuals$residuals_750, OA_2001_750_nb_listw)
moran.test(Data_750_Long_R_residuals$residuals_750_k4, OA_2001_750_k4_listw)
moran.test(Data_750_Long_R_residuals$residuals_750_dist, OA_2001_750_dist_listw)
moran.mc(Data_750_Long_R_residuals$residuals_750, OA_2001_750_nb_listw, nsim=999, alternative="greater")
moran.mc(Data_750_Long_R_residuals$residuals_750_k4, OA_2001_750_k4_listw, nsim=999, alternative="greater")
moran.mc(Data_750_Long_R_residuals$residuals_750_dist, OA_2001_750_dist_listw, nsim=999, alternative="greater")
moran.plot(Data_750_Long_R_residuals$residuals_750, OA_2001_750_nb_listw)
moran.plot(Data_750_Long_R_residuals$residuals_750_k4, OA_2001_750_k4_listw)
moran.plot(Data_750_Long_R_residuals$residuals_750_dist, OA_2001_750_dist_listw)

Data_1000_Long_R_residuals <- cbind(Data_1000_Long_R, residuals_1000, residuals_1000_k4, residuals_1000_dist)
Data_1000_Long_R_residuals <- subset(Data_1000_Long_R_residuals, Year == '2005')
moran.test(Data_1000_Long_R_residuals$residuals_1000, OA_2001_1000_nb_listw)
moran.test(Data_1000_Long_R_residuals$residuals_1000_k4, OA_2001_1000_k4_listw)
moran.test(Data_1000_Long_R_residuals$residuals_1000_dist, OA_2001_1000_dist_listw)
moran.mc(Data_1000_Long_R_residuals$residuals_1000, OA_2001_1000_nb_listw, nsim=999, alternative="greater")
moran.mc(Data_1000_Long_R_residuals$residuals_1000_k4, OA_2001_1000_k4_listw, nsim=999, alternative="greater")
moran.mc(Data_1000_Long_R_residuals$residuals_1000_dist, OA_2001_1000_dist_listw, nsim=999, alternative="greater")
moran.plot(Data_1000_Long_R_residuals$residuals_1000, OA_2001_1000_nb_listw)
moran.plot(Data_1000_Long_R_residuals$residuals_1000_k4, OA_2001_1000_k4_listw)
moran.plot(Data_1000_Long_R_residuals$residuals_1000_dist, OA_2001_1000_dist_listw)

## Event Study Analysis

# Turn data into long format with all years of vehicle ownership
ESA_250_Long <- Buff_250[c(2,21:27,44)]
ESA_250_Long <- gather(ESA_250_Long , IN250, cars,`2000`:`2006`)
ESA_250_Long$LCC <- '2003'
ESA_250_Long <- merge(x = ESA_250_Long, y = Buff_250[ , c("OA_2001", "IN250")], by = "OA_2001", all.x=TRUE)
names(ESA_250_Long)[2] <- 'Year'
names(ESA_250_Long)[5] <- 'Buffer'
ESA_250_Long$Year <- as.numeric(ESA_250_Long$Year)
ESA_250_Long$LCC <- as.numeric(ESA_250_Long$LCC)
ESA_250_Long$Time_to_treat <- ifelse(ESA_250_Long$Buffer =='1', (ESA_250_Long$LCC - ESA_250_Long$Year), '0')

ESA_500_Long <- Buff_500[c(2,21:27,46)]
ESA_500_Long <- gather(ESA_500_Long , IN500, cars,`2000`:`2006`)
ESA_500_Long$LCC <- '2003'
ESA_500_Long <- merge(x = ESA_500_Long, y = Buff_500[ , c("OA_2001", "IN500")], by = "OA_2001", all.x=TRUE)
names(ESA_500_Long)[2] <- 'Year'
names(ESA_500_Long)[5] <- 'Buffer'
ESA_500_Long$Year <- as.numeric(ESA_500_Long$Year)
ESA_500_Long$LCC <- as.numeric(ESA_500_Long$LCC)
ESA_500_Long$Time_to_treat <- ifelse(ESA_500_Long$Buffer =='1', (ESA_500_Long$LCC - ESA_500_Long$Year), '0')

ESA_750_Long <- Buff_750[c(2,21:27,48)]
ESA_750_Long <- gather(ESA_750_Long , IN750, cars,`2000`:`2006`)
ESA_750_Long$LCC <- '2003'
ESA_750_Long <- merge(x = ESA_750_Long, y = Buff_750[ , c("OA_2001", "IN750")], by = "OA_2001", all.x=TRUE)
names(ESA_750_Long)[2] <- 'Year'
names(ESA_750_Long)[5] <- 'Buffer'
ESA_750_Long$Year <- as.numeric(ESA_750_Long$Year)
ESA_750_Long$LCC <- as.numeric(ESA_750_Long$LCC)
ESA_750_Long$Time_to_treat <- ifelse(ESA_750_Long$Buffer =='1', (ESA_750_Long$LCC - ESA_750_Long$Year), '0')

ESA_1000_Long <- Buff_1000[c(2,21:27,50)]
ESA_1000_Long <- gather(ESA_1000_Long , IN1000, cars,`2000`:`2006`)
ESA_1000_Long$LCC <- '2003'
ESA_1000_Long <- merge(x = ESA_1000_Long, y = Buff_1000[ , c("OA_2001", "IN1000")], by = "OA_2001", all.x=TRUE)
names(ESA_1000_Long)[2] <- 'Year'
names(ESA_1000_Long)[5] <- 'Buffer'
ESA_1000_Long$Year <- as.numeric(ESA_1000_Long$Year)
ESA_1000_Long$LCC <- as.numeric(ESA_1000_Long$LCC)
ESA_1000_Long$Time_to_treat <- ifelse(ESA_1000_Long$Buffer =='1', (ESA_1000_Long$LCC - ESA_1000_Long$Year), '0')

# Conduct Event Study Analysis 

ESA_250_2WFE = feols(log(cars) ~ i(Time_to_treat, Buffer, ref = 0) | OA_2001 + Year, cluster = ~OA_2001, data = ESA_250_Long)
summary(ESA_250_2WFE)
plot_summs(ESA_250_2WFE)

ESA_500_2WFE = feols(log(cars) ~ i(Time_to_treat, Buffer, ref = 0) | OA_2001 + Year, cluster = ~OA_2001, data = ESA_500_Long)
summary(ESA_500_2WFE)
plot_summs(ESA_500_2WFE)

ESA_750_2WFE = feols(log(cars) ~ i(Time_to_treat, Buffer, ref = 0) | OA_2001 + Year, cluster = ~OA_2001, data = ESA_750_Long)
summary(ESA_750_2WFE)
plot_summs(ESA_750_2WFE)

ESA_1000_2WFE = feols(log(cars) ~ i(Time_to_treat, Buffer, ref = 0) | OA_2001 + Year, cluster = ~OA_2001, data = ESA_1000_Long)
summary(ESA_1000_2WFE)
plot_summs(ESA_1000_2WFE)

plot_summs(ESA_250_2WFE, ESA_500_2WFE, ESA_750_2WFE, ESA_1000_2WFE, model.names = c('250m', '500m', '750m', '1000m'))

## Sensitivity analysis on before-and-after year selection for the DiD

# Create new long version of data using 2002 and 2004 as the before-and-after periods 

Data_250_Long_v2 <- Buff_250[c(2,23,25,60)]
Data_250_Long_v2 <- gather(Data_250_Long_v2 , Buff250, cars,`2002`:`2004`)
Data_250_Long_v2 <- merge(x = Data_250_Long_v2, y = Buff_250[ , c("OA_2001", "Buff250", 
                                                            "EcoActivePCT2001", "Drive_PCT2001", "Flats_PCT2001", "PopD2001")], by = "OA_2001", all.x=TRUE)
names(Data_250_Long_v2)[2] <- 'Year'
names(Data_250_Long_v2)[4] <- 'Buffer'

Data_250_Long_v2$TimeDummy <- ifelse(Data_250_Long_v2$Year == '2004', '1', '0')
Data_250_Long_v2$BuffDummy <- ifelse(Data_250_Long_v2$Buffer == 'OUT', '1', '0')
Data_250_Long_v2$TimeBuffDummy <- ifelse(Data_250_Long_v2$Buffer == 'OUT' & Data_250_Long_v2$Year == '2004' , '1', '0')

Data_500_Long_v2 <- Buff_500[c(2,23,25,61)]
Data_500_Long_v2 <- gather(Data_500_Long_v2 , Buff500, cars,`2002`:`2004`)
Data_500_Long_v2 <- merge(x = Data_500_Long_v2, y = Buff_500[ , c("OA_2001", "Buff500", 
                                                                  "EcoActivePCT2001", "Drive_PCT2001", "Flats_PCT2001", "PopD2001")], by = "OA_2001", all.x=TRUE)
names(Data_500_Long_v2)[2] <- 'Year'
names(Data_500_Long_v2)[4] <- 'Buffer'

Data_500_Long_v2$TimeDummy <- ifelse(Data_500_Long_v2$Year == '2004', '1', '0')
Data_500_Long_v2$BuffDummy <- ifelse(Data_500_Long_v2$Buffer == 'OUT', '1', '0')
Data_500_Long_v2$TimeBuffDummy <- ifelse(Data_500_Long_v2$Buffer == 'OUT' & Data_500_Long_v2$Year == '2004' , '1', '0')

Data_750_Long_v2 <- Buff_750[c(2,23,25,62)]
Data_750_Long_v2 <- gather(Data_750_Long_v2 , Buff750, cars,`2002`:`2004`)
Data_750_Long_v2 <- merge(x = Data_750_Long_v2, y = Buff_750[ , c("OA_2001", "Buff750", 
                                                                  "EcoActivePCT2001", "Drive_PCT2001", "Flats_PCT2001", "PopD2001")], by = "OA_2001", all.x=TRUE)
names(Data_750_Long_v2)[2] <- 'Year'
names(Data_750_Long_v2)[4] <- 'Buffer'

Data_750_Long_v2$TimeDummy <- ifelse(Data_750_Long_v2$Year == '2004', '1', '0')
Data_750_Long_v2$BuffDummy <- ifelse(Data_750_Long_v2$Buffer == 'OUT', '1', '0')
Data_750_Long_v2$TimeBuffDummy <- ifelse(Data_750_Long_v2$Buffer == 'OUT' & Data_750_Long_v2$Year == '2004' , '1', '0')

Data_1000_Long_v2 <- Buff_1000[c(2,23,25,63)]
Data_1000_Long_v2 <- gather(Data_1000_Long_v2 , Buff1000, cars,`2002`:`2004`)
Data_1000_Long_v2 <- merge(x = Data_1000_Long_v2, y = Buff_1000[ , c("OA_2001", "Buff1000", 
                                                                     "EcoActivePCT2001", "Drive_PCT2001", "Flats_PCT2001", "PopD2001")], by = "OA_2001", all.x=TRUE)
names(Data_1000_Long_v2)[2] <- 'Year'
names(Data_1000_Long_v2)[4] <- 'Buffer'

Data_1000_Long_v2$TimeDummy <- ifelse(Data_1000_Long_v2$Year == '2004', '1', '0')
Data_1000_Long_v2$BuffDummy <- ifelse(Data_1000_Long_v2$Buffer == 'OUT', '1', '0')
Data_1000_Long_v2$TimeBuffDummy <- ifelse(Data_1000_Long_v2$Buffer == 'OUT' & Data_1000_Long_v2$Year == '2004' , '1', '0')

# Trim the new before-and-after data 

Data_250_Long_v2_R <- filter(Data_250_Long_v2, Data_250_Long_v2$OA_2001 != 'E00023812' & Data_250_Long_v2$OA_2001 != 'E00022974')
Data_500_Long_v2_R <- filter(Data_500_Long_v2, Data_500_Long_v2$OA_2001 != 'E00023812' & Data_500_Long_v2$OA_2001 != 'E00004186')
Data_750_Long_v2_R <- filter(Data_750_Long_v2, Data_750_Long_v2$OA_2001 != 'E00023812' & Data_750_Long_v2$OA_2001 != 'E00004186')                          
Data_1000_Long_v2_R <- filter(Data_1000_Long_v2, Data_1000_Long_v2$OA_2001 != 'E00023812' & Data_1000_Long_v2$OA_2001 != 'E00004186') 

# Conduct two way fixed effects DiD models with the new before-and-after periods

FEDiD_250_v2 <- plm(formula = mod3, data = Data_250_Long_v2, index = c("OA_2001", "Year"), 
                 model = "within", effect = 'twoways')
summary(FEDiD_250_v2)

FEDiD_500_v2 <- plm(formula = mod3, data = Data_500_Long_v2, index = c("OA_2001", "Year"), 
                    model = "within", effect = 'twoways')
summary(FEDiD_500_v2)

FEDiD_750_v2 <- plm(formula = mod3, data = Data_750_Long_v2, index = c("OA_2001", "Year"), 
                    model = "within", effect = 'twoways')
summary(FEDiD_750_v2)

FEDiD_1000_v2 <- plm(formula = mod3, data = Data_1000_Long_v2, index = c("OA_2001", "Year"), 
                    model = "within", effect = 'twoways')
summary(FEDiD_1000_v2)

stargazer(FEDiD_250_v2, FEDiD_500_v2, FEDiD_750_v2, FEDiD_1000_v2, type = 'text')

#Conduct SpDIDs on the new before-and-after periods

sararremod_250_v2 <- spml(mod3, data = Data_250_Long_v2_R, listw = OA_2001_250_dist_listw,
                       model="random", spatial.error="none", lag=TRUE, effect = 'individual')
summary(sararremod_250_v2)
sararremod_250_v2$logLik

sararremod_500_v2 <- spml(mod3, data = Data_500_Long_v2_R, listw = OA_2001_500_nb_listw,
                       model="random", spatial.error="none", lag=TRUE, effect = 'individual')
summary(sararremod_500_v2)
sararremod_500_v2$logLik

sararremod_750_v2 <- spml(mod3, data = Data_750_Long_v2_R, listw = OA_2001_750_nb_listw,
                       model="random", spatial.error="none", lag=TRUE, effect = 'individual')
summary(sararremod_750_v2)
sararremod_750_v2$logLik

sararremod_1000_v2 <- spml(mod3, data = Data_1000_Long_v2_R, listw = OA_2001_1000_nb_listw,
                        model="random", spatial.error="none", lag=TRUE, effect = 'individual')
summary(sararremod_1000_v2)
sararremod_1000_v2$logLik
