#Vehicle Statistics data preparation and analysis

library(tidyr)
library(readr)

# Load vehicle stats data recording number of private car registrations and seperate out by census year of OAs
Vehicle_Stats <- read_csv("Analysis/Data/VehicleStats/221111-craig-morton/221111-craig-morton.txt")

Vehicle_Stats_2001 <- subset(Vehicle_Stats, Vehicle_Stats$CensusYear == '2001')
Vehicle_Stats_2011 <- subset(Vehicle_Stats, Vehicle_Stats$CensusYear == '2011')

# Turn the vehicle stats data from long to wide format 
VehicleStats_2011_Wide <- spread(Vehicle_Stats_2011, Year, N)
VehicleStats_2001_Wide <- spread(Vehicle_Stats_2001, Year, N)

# Change OAs coded as 'NA' (which mean the number of cars is between 0-5) to '3'
VehicleStats_2011_Wide[is.na(VehicleStats_2011_Wide)] <- 3
VehicleStats_2001_Wide[is.na(VehicleStats_2001_Wide)] <- 3

# Rename spatial ID in vehicle stats data so that it corresponds to census
names(VehicleStats_2011_Wide)[2] <- "OA_2011"
names(VehicleStats_2001_Wide)[2] <- "OA_2001_old"

#Save vehicle stats as a .csv
write.csv(VehicleStats_2001_Wide, "Analysis/Data/VehicleStats/Vehicle_Stats_2001.csv")
write.csv(VehicleStats_2011_Wide, "Analysis/Data/VehicleStats/Vehicle_Stats_2011.csv")

#Load in Census data for 2001 and 2011
Integrated_2001 <- read_csv("Analysis/Data/2001Census/Integrated_2001.csv")
Integrated_2011 <- read_csv("Analysis/Data/2011Census/Integrated_2011.csv")

#Merge vehicle stats data and census 
Integrated_2001_cars <- merge(Integrated_2001, VehicleStats_2001_Wide, by = 'OA_2001_old')
Integrated_2011_cars <- merge(Integrated_2011, VehicleStats_2011_Wide, by = 'OA_2011')

#Save census and car fleet stats as a .csv
write.csv(Integrated_2001_cars, "Analysis/Data/2001Census/Integrated_2001_cars.csv")
write.csv(Integrated_2011_cars, "Analysis/Data/2011Census/Integrated_2011_cars.csv")

#Load dummy variables for treatment and control groups
Buffs_2001 <- read_csv("Analysis/Data/BuffDummies/2001_Buffers_v3.csv")
Buffs_2011 <- read_csv("Analysis/Data/BuffDummies/2011_Buffers.csv")

#Merge vehicle stats + census data with buffers
Integrated_2001_cars_buffs <- merge(Integrated_2001_cars, Buffs_2001, by = 'OA_2001')
Integrated_2011_cars_buffs <- merge(Integrated_2011_cars, Buffs_2011, by = 'OA_2011')

#Save census and car fleet stats as a .csv
write.csv(Integrated_2001_cars_buffs, "Analysis/Data/2001Census/Integrated_2001_cars_buffs.csv")
write.csv(Integrated_2011_cars_buffs, "Analysis/Data/2011Census/Integrated_2011_cars_buffs.csv")

