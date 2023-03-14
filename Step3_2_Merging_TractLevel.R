rm(list=ls())
library(readr)
library(tidyverse)
library(stringr)

# Community Indicators
Comm_ACS <- read_csv("Data/Community_Indicators/Community_ACSIndicators_tract.csv")
Comm_ACS$County <- NA

for (i in 1:nrow(Comm_ACS)) {
  Comm_ACS$County[i] <- 
    paste(str_split(str_split(Comm_ACS$Name, ",")[[i]][2]," ")[[1]][2],
          "County,",str_replace_all(str_split(Comm_ACS$Name, ",")[[1]][3]," ",""))
}

Comm_CHS <- read_csv("Data/Community_Indicators/Community_CHSIndicators_county.csv")
Comm_CHS <- Comm_CHS[2:nrow(Comm_CHS),]
Comm_CHS$County <- paste(Comm_CHS$County, "County,",str_replace_all(str_split(Comm_ACS$Name[1], ",")[[1]][3]," ",""))
Comm <- left_join(Comm_ACS, Comm_CHS, by="County")
rm(Comm_CHS, Comm_ACS)

GEOID_County <- Comm %>% 
  select(GEOID, County)
GEOID_Name <- Comm %>% 
  select(GEOID, Name)

# Economy Indicators

# str_replace_all(str_split(Comm$Name, ",")[[1]][3]," ","")

Econ_ACS <- read_csv("Data/Economy_Indicators/Economy_ACSIndicators_tract.csv")
Econ <- left_join(Econ_ACS, GEOID_County, by="GEOID")

Econ_CHS <- read_csv("Data/Economy_Indicators/Economy_CHSIndicators_county.csv")
Econ_CHS <- Econ_CHS[2:nrow(Econ_CHS),]
Econ_CHS$County <- paste(Econ_CHS$County, paste("County,",str_replace_all(str_split(Comm$Name, ",")[[1]][3]," ","") ))
Econ <- left_join(Econ, Econ_CHS, by="County")

Econ_EQI <- read_csv("Data/Economy_Indicators/Economy_EQIIndicators_county.csv")
Econ_EQI$County <- paste(str_replace_all(paste(Econ_EQI$County, ","), " ,", ","),str_replace_all(str_split(Comm$Name, ",")[[1]][3]," ",""))
Econ <- left_join(Econ, Econ_EQI, by="County")
Econ <- left_join(Econ, GEOID_Name, by="GEOID")
rm(Econ_ACS, Econ_CHS, Econ_EQI)

# Housing Indicators
H_ACS <- read_csv("Data/Housing_Indicators/Housing_ACSIndicators_tract.csv")
Hous <- left_join(H_ACS, GEOID_County, by="GEOID")

H_CHS <- read_csv("Data/Housing_Indicators/Housing_CHSIndicators_county.csv")
H_CHS <- H_CHS[2:nrow(H_CHS),]
H_CHS$County <- paste(H_CHS$County, paste("County,",str_replace_all(str_split(Comm$Name, ",")[[1]][3]," ","") ))
Hous <- left_join(Hous, H_CHS, by="County")

H_EQI <- read_csv("Data/Housing_Indicators/Housing_EQIIndicators_county.csv")
H_EQI$County <- paste(str_replace(paste(H_EQI$County, ",")," ,",","), paste(str_replace_all(str_split(Comm$Name, ",")[[1]][3]," ","") ))

Hous <- left_join(Hous, H_EQI, by="County")
Hous <- left_join(Hous, GEOID_Name, by="GEOID")
rm(H_ACS, H_CHS, H_EQI)

# Infrastructure Indicators
Infra_ACS <- read_csv("Data/Infrastructure_Indicators/Infrastructure_ACSIndicators_tract.csv")
Infra <- left_join(Infra_ACS, GEOID_County, by="GEOID")

Infra_CHS <- read_csv("Data/Infrastructure_Indicators/Infrastructure_CHSIndicators_county.csv")
Infra_CHS <- Infra_CHS[2:nrow(Infra_CHS),]
Infra_CHS$County <- paste(Infra_CHS$County, paste("County,",str_replace_all(str_split(Comm$Name, ",")[[1]][3]," ","") ))
Infra <- left_join(Infra, Infra_CHS, by="County")
rm(Infra_CHS, Infra_ACS)
Infra <- left_join(Infra, GEOID_Name, by="GEOID")

# Environmental Indicators
Envi_EQI <- read_csv("Data/Environment_Indicators/Environment_EQIIndicators_county.csv")
Envi_EQI$County <- paste(str_replace(paste(Envi_EQI$County, ",")," ,",","), paste(str_replace_all(str_split(Comm$Name, ",")[[1]][3]," ","") ))

Envi <- Infra %>% select(GEOID, Name, County)
Envi <- left_join(Envi,Envi_EQI, by="County")
rm(Envi_EQI)

# Combing and Saving
dir.create("Data/Indicators_TractLevel")

write_csv(Comm, "Data/Indicators_TractLevel/Community_Indicators_Tract.csv")
write_csv(Econ, "Data/Indicators_TractLevel/Economy_Indicators_Tract.csv")
write_csv(Hous, "Data/Indicators_TractLevel/Housing_Indicators_Tract.csv")
write_csv(Infra, "Data/Indicators_TractLevel/Infrastructure_Indicators_Tract.csv")
write_csv(Envi, "Data/Indicators_TractLevel/Environment_Indicators_Tract.csv")

Econ$Name <- NULL
Econ$County <- NULL
All_Combined <- left_join(Comm, Econ, by="GEOID")
Hous$Name <- NULL
Hous$County <- NULL
All_Combined <- left_join(All_Combined, Hous, by="GEOID")
Infra$Name <- NULL
Infra$County <- NULL
All_Combined<- left_join(All_Combined, Infra, by="GEOID")
Envi$Name <- NULL
Envi$County <- NULL
All_Combined <- left_join(All_Combined, Envi, by="GEOID")

write_csv(All_Combined, "Data/Indicators_TractLevel/All_Indicators_Tract.csv")

