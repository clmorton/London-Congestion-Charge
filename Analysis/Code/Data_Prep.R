#Data Integration and Preparation

#Load packages
library(readr)

#Load datasets
CarsVans_2001 <- read_csv("Analysis/Data/2001Census/CarsVans_2.csv")
EcoStat_2001 <- read_csv("Analysis/Data/2001Census/EcoStat_2.csv")
NS_Sec_2001 <- read_csv("Analysis/Data/2001Census/NS_Sec_2.csv")
PopDens_2001 <- read_csv("Analysis/Data/2001Census/PopDens_2.csv")
Qual_2001 <- read_csv("Analysis/Data/2001Census/Qual_2.csv")
WorkMode_2001 <- read_csv("Analysis/Data/2001Census/WorkMode_2.csv")
Dwelling_2001 <- read_csv("Analysis/Data/2001Census/Dwelling_2.csv")

CarsVans_2011 <- read_csv("Analysis/Data/2011Census/CarsVans.csv")
EcoStat_2011 <- read_csv("Analysis/Data/2011Census/EcoStatus_2.csv")
NS_Sec_2011 <- read_csv("Analysis/Data/2011Census/NS_Sec_2.csv")
PopDens_2011 <- read_csv("Analysis/Data/2011Census/PopDens.csv")
Qual_2011 <- read_csv("Analysis/Data/2011Census/Qual.csv")
WorkMode_2011 <- read_csv("Analysis/Data/2011Census/WorkMode.csv")
Dwelling_2011 <- read_csv("Analysis/Data/2011Census/Dwelling.csv")

OA_Check <- read_csv("Analysis/Shapefiles/OA_Check.csv")

#Check order of observations to ensure Output Area reference is being used as the index
head(CarsVans_2001)
head(EcoStat_2001)
head(NS_Sec_2001)
head(PopDens_2001)
head(Qual_2001)
head(WorkMode_2001)
head(Dwelling_2001)

head(CarsVans_2011)
head(EcoStat_2011)
head(NS_Sec_2011)
head(PopDens_2011)
head(Qual_2011)
head(WorkMode_2011)
head(Dwelling_2011)

#Create required variables for 2001
OA_2001 <- (CarsVans_2001$`2001 output area`)
OA_2001_old <- (CarsVans_2001$ons_label)
Cars2001 <- CarsVans_2001$`Total cars or van`
CarsPerHouse2001 <- CarsVans_2001$`Total cars or van`/CarsVans_2001$`All categories: Cars or Vans`
NoCarPCT2001 <- ((CarsVans_2001$`No car or van`/CarsVans_2001$`All categories: Cars or Vans`)*100)
EcoActivePCT2001 <- (EcoStat_2001$`Economically active: Total`/EcoStat_2001$`All categories: Economic activity`)*100
NS_Sec1_PCT2001 <- (NS_Sec_2001$`1. Higher managerial, administrative and professional occupations`/
                  NS_Sec_2001$`All categories: NS-SeC`)*100
NS_Sec2_PCT2001 <- (NS_Sec_2001$`2. Lower managerial, administrative and professional occupations`/
                  NS_Sec_2001$`All categories: NS-SeC`)*100
NS_Sec1_and_2_PCT2001 <- NS_Sec1_PCT2001 + NS_Sec2_PCT2001
PopD2001 <- PopDens_2001$`Density (number of persons per hectare)`
No_Qual_PCT2001 <- (Qual_2001$`No qualifications`/Qual_2001$`All categories: Highest level of qualification`)*100
Level4_Qual_PCT2001 <- (Qual_2001$`Level 4/5 qualifications`/Qual_2001$`All categories: Highest level of qualification`)*100
WFH_PCT2001 <- (WorkMode_2001$`Work mainly at or from home`/WorkMode_2001$`All categories: Method of travel to work`)*100
Drive_PCT2001 <- ((WorkMode_2001$`Driving a car or van`+ WorkMode_2001$`Passenger in a car or van`)/
         WorkMode_2001$`All categories: Method of travel to work`)*100
PT_PCT2001 <- ((WorkMode_2001$`Underground, metro, light rail or tram` + WorkMode_2001$Train +
             WorkMode_2001$`Bus, minibus or coach`)/WorkMode_2001$`All categories: Method of travel to work`)*100
AT_PCT2001 <- ((WorkMode_2001$Bicycle + WorkMode_2001$`On foot`)/WorkMode_2001$`All categories: Method of travel to work`)*100
Flats_PCT2001 <- ((Dwelling_2001$`Flat, maisonette or apartment - in a purpose built block of flats or tenement` +
                     Dwelling_2001$`Flat, maisonette or apartment - part of a converted or shared house (includes bed-sit)` +
                     Dwelling_2001$`Flat, maisonette or apartment - in a commercial building`)/Dwelling_2001$`All household spaces`)*100

#Merge created variables into an integrated dataset for 2001
Integrated_2001 <- cbind(OA_2001, OA_2001_old, Cars2001, CarsPerHouse2001, NoCarPCT2001, EcoActivePCT2001, NS_Sec1_PCT2001,
                         NS_Sec2_PCT2001, NS_Sec1_and_2_PCT2001, PopD2001, No_Qual_PCT2001, Level4_Qual_PCT2001,
                         WFH_PCT2001, Drive_PCT2001, PT_PCT2001, AT_PCT2001, Flats_PCT2001)
write.csv(Integrated_2001, "Analysis/Data/2001Census/Integrated_2001.csv")

#Create required variables for 2011
OA_2011 <- (CarsVans_2011$`geography code`)
Cars2011 <- CarsVans_2011$`Cars: sum of All cars or vans in the area; measures: Value`
CarsPerHouse2011 <-(CarsVans_2011$`Cars: sum of All cars or vans in the area; measures: Value`/
                  CarsVans_2011$`Cars: All categories: Car or van availability; measures: Value`)
NoCarPCT2011 <- (CarsVans_2011$`Cars: No cars or vans in household; measures: Value`/
               CarsVans_2011$`Cars: All categories: Car or van availability; measures: Value`)*100
EcoActivePCT2011 <-(EcoStat_2011$`Economically active: Total`/EcoStat_2011$`All categories: Economic activity`)*100
NS_Sec1_PCT2011 <- (NS_Sec_2011$`1. Higher managerial, administrative and professional occupations`/
                  NS_Sec_2011$`All categories: NS-SeC`)*100
NS_Sec2_PCT2011 <- (NS_Sec_2011$`2. Lower managerial, administrative and professional occupations`/
                  NS_Sec_2011$`All categories: NS-SeC`)*100
NS_Sec1_and_2_PCT2011 <- NS_Sec1_PCT2011 + NS_Sec2_PCT2011
PopD2011 <- PopDens_2011$`Area/Population Density: Density (number of persons per hectare); measures: Value`
No_Qual_PCT2011 <- (Qual_2011$`Qualification: No qualifications; measures: Value`/
                  Qual_2011$`Qualification: All categories: Highest level of qualification; measures: Value`)*100
Level4_Qual_PCT2011 <- (Qual_2011$`Qualification: Level 4 qualifications and above; measures: Value`/
                       Qual_2011$`Qualification: All categories: Highest level of qualification; measures: Value`)*100
WFH_PCT2011 <- (WorkMode_2011$`Method of Travel to Work: Work mainly at or from home; measures: Value`/
              WorkMode_2011$`Method of Travel to Work: All categories: Method of travel to work; measures: Value`)*100
Drive_PCT2011 <- ((WorkMode_2011$`Method of Travel to Work: Driving a car or van; measures: Value`+
                WorkMode_2011$`Method of Travel to Work: Passenger in a car or van; measures: Value`)/
                WorkMode_2011$`Method of Travel to Work: All categories: Method of travel to work; measures: Value`)*100
PT_PCT2011 <- ((WorkMode_2011$`Method of Travel to Work: Underground, metro, light rail, tram; measures: Value`+
            WorkMode_2011$`Method of Travel to Work: Train; measures: Value` + WorkMode_2011$`Method of Travel to Work: Bus, minibus or coach; measures: Value`)/
             WorkMode_2011$`Method of Travel to Work: All categories: Method of travel to work; measures: Value`)*100
AT_PCT2011 <- ((WorkMode_2011$`Method of Travel to Work: Bicycle; measures: Value`+
              WorkMode_2011$`Method of Travel to Work: On foot; measures: Value`)/
              WorkMode_2011$`Method of Travel to Work: All categories: Method of travel to work; measures: Value`)*100
Flats_PCT2011 <- (Dwelling_2011$`Dwelling Type: Unshared dwelling: Flat, maisonette or apartment: Total; measures: Value`/
                    Dwelling_2011$`Dwelling Type: Unshared dwelling: Total; measures: Value`)*100

#Merge created variables into an integrated dataset for 2011
Integrated_2011 <- cbind(OA_2011, Cars2011, CarsPerHouse2011, NoCarPCT2011, EcoActivePCT2011, NS_Sec1_PCT2011,
                         NS_Sec2_PCT2011, NS_Sec1_and_2_PCT2011, PopD2011, No_Qual_PCT2011, Level4_Qual_PCT2011,
                         WFH_PCT2011, Drive_PCT2011, PT_PCT2011, AT_PCT2011, Flats_PCT2011)
write.csv(Integrated_2011, "Analysis/Data/2011Census/Integrated_2011.csv")

#Merge 2001 and 2001 spreadsheets using a common lookup table
names(OA_Check)[1] <- "OA_2001"
Integrated_01 <- merge(Integrated_2001, OA_Check, by = 'OA_2001')
names(Integrated_01)[17] <- "OA_2011"
Integrated_spreadsheet <- merge(Integrated_2011, Integrated_01, by = 'OA_2011')
write.csv(Integrated_spreadsheet, "Analysis/Data/Integrated_census.csv")
