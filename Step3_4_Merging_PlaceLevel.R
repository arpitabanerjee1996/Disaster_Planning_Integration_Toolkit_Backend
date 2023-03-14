rm(list=ls())
library(readr)
library(tidyverse)
library(stringr)
library(tidycensus)
library(sf)

# From place to County
x <- read_csv("Data/Community_Indicators/Community_ACSIndicators_place.csv")
State <- str_replace_all(str_split(x$Name[1], ",")[[1]][2]," ","")

#B08121_001 is a random filler variable used to run tidycensus to extract geographies. 
#All its values will be removed later
county_bounds <- get_acs(geography = "county", state = State, variables ="B08121_001", 
                         year=2019, survey="acs5", geometry=TRUE)
place_bounds <- get_acs(geography = "place", state = State, variables ="B08121_001", 
                        year=2019, survey="acs5", geometry=TRUE)

county_place <- st_join(place_bounds, county_bounds, 
                        join=st_within)
county_place <- st_drop_geometry(county_place)
rm(county_bounds, place_bounds)

county_place <- county_place %>% 
  select(GEOID_Place=GEOID.x,
         GEOID_County=GEOID.y,
         Name_Place=NAME.x,
         Name_County=NAME.y)
dir.create("Data/Indicators_PlaceLevel")
write_csv(county_place, "Data/Indicators_PlaceLevel/NameMatching_Places_to_County.csv")


# Community Indicators
Comm_ACS <- read_csv("Data/Community_Indicators/Community_ACSIndicators_place.csv")
Comm_ACS$Name <- NULL
Comm_ACS$GEOID_Place <- Comm_ACS$GEOID
Comm_ACS$GEOID <- NULL
Comm_ACS$GEOID_Place <- as.character(Comm_ACS$GEOID_Place)
x <- left_join(Comm_ACS,county_place, by="GEOID_Place")
Comm_ACS <- x


# Comm_ACS$County <- NA
# 
# for (i in 1:nrow(Comm_ACS)) {
#   Comm_ACS$County[i] <- 
#     paste(str_split(str_split(Comm_ACS$Name, ",")[[i]][2]," ")[[1]][2],
#           "County,",str_replace_all(str_split(Comm_ACS$Name, ",")[[1]][3]," ",""))
# }

Comm_CHS <- read_csv("Data/Community_Indicators/Community_CHSIndicators_county.csv")
Comm_CHS <- Comm_CHS[2:nrow(Comm_CHS),]
Comm_CHS$Name_County <- paste(Comm_CHS$County, "County,",State)
Comm <- left_join(Comm_ACS, Comm_CHS, by="Name_County")
rm(Comm_CHS, Comm_ACS)

# GEOID_County <- Comm %>% 
#   select(GEOID, County)
# GEOID_Name <- Comm %>% 
#   select(GEOID, Name)

# Economy Indicators

# str_replace_all(str_split(Comm$Name, ",")[[1]][3]," ","")

Econ_ACS <- read_csv("Data/Economy_Indicators/Economy_ACSIndicators_Place.csv")
Econ_ACS$GEOID_Place <- Econ_ACS$GEOID
Econ_ACS$GEOID <- NULL
Econ_ACS$GEOID_Place <- as.character(Econ_ACS$GEOID_Place)
x <- left_join(Econ_ACS,county_place, by="GEOID_Place")
Econ_ACS <- x
Econ <- Econ_ACS

Econ_CHS <- read_csv("Data/Economy_Indicators/Economy_CHSIndicators_county.csv")
Econ_CHS <- Econ_CHS[2:nrow(Econ_CHS),]
Econ_CHS$Name_County <- paste(Econ_CHS$County, paste("County,",State ))
Econ <- left_join(Econ, Econ_CHS, by="Name_County")

Econ_EQI <- read_csv("Data/Economy_Indicators/Economy_EQIIndicators_county.csv")
Econ_EQI$Name_County <- paste(str_replace_all(paste(Econ_EQI$County, ","), " ,", ","),State)
Econ <- left_join(Econ, Econ_EQI, by="Name_County")
rm(Econ_ACS, Econ_CHS, Econ_EQI)
Econ$County.x <- NULL
Econ$County.y <- NULL


# Housing Indicators
H_ACS <- read_csv("Data/Housing_Indicators/Housing_ACSIndicators_Place.csv")
H_ACS$GEOID_Place <- H_ACS$GEOID
H_ACS$GEOID <- NULL
H_ACS$GEOID_Place <- as.character(H_ACS$GEOID_Place)
x <- left_join(H_ACS,county_place, by="GEOID_Place")
H_ACS <- x
Hous <- H_ACS

H_CHS <- read_csv("Data/Housing_Indicators/Housing_CHSIndicators_county.csv")
H_CHS <- H_CHS[2:nrow(H_CHS),]
H_CHS$Name_County <- paste(H_CHS$County, paste("County,",State ))
Hous <- left_join(Hous, H_CHS, by="Name_County")

H_EQI <- read_csv("Data/Housing_Indicators/Housing_EQIIndicators_county.csv")
H_EQI$Name_County <- paste(str_replace(paste(H_EQI$County, ",")," ,",","), paste(State ))

Hous <- left_join(Hous, H_EQI, by="Name_County")
Hous$County.x <- NULL
Hous$County.y <- NULL
rm(H_ACS, H_CHS, H_EQI)

# Infrastructure Indicators
Infra_ACS <- read_csv("Data/Infrastructure_Indicators/Infrastructure_ACSIndicators_Place.csv")
Infra_ACS$GEOID_Place <- Infra_ACS$GEOID
Infra_ACS$GEOID <- NULL
Infra_ACS$GEOID_Place <- as.character(Infra_ACS$GEOID_Place)
x <- left_join(Infra_ACS,county_place, by="GEOID_Place")
Infra_ACS <- x
Infra <- Infra_ACS

Infra_CHS <- read_csv("Data/Infrastructure_Indicators/Infrastructure_CHSIndicators_county.csv")
Infra_CHS <- Infra_CHS[2:nrow(Infra_CHS),]
Infra_CHS$Name_County <- paste(Infra_CHS$County, paste("County,",State ))
Infra <- left_join(Infra, Infra_CHS, by="Name_County")
rm(Infra_CHS, Infra_ACS)
Infra$County <- NULL

# Environmental Indicators
Envi_EQI <- read_csv("Data/Environment_Indicators/Environment_EQIIndicators_county.csv")
Envi_EQI$Name_County <- paste(str_replace(paste(Envi_EQI$County, ",")," ,",","), State )

x <- Infra %>% select(Name_County, Name_Place, GEOID_Place, GEOID_County)
Envi <- left_join(x, Envi_EQI, by="Name_County")
Envi$County <- NULL
rm(Envi_EQI)

# Combing and Saving

Comm <- Comm %>% 
  relocate(GEOID_Place, Name_Place, GEOID_County, Name_County)
Econ <- Econ %>% 
  relocate(GEOID_Place, Name_Place, GEOID_County, Name_County)
Hous <- Hous %>% 
  relocate(GEOID_Place, Name_Place, GEOID_County, Name_County)
Infra <- Infra %>% 
  relocate(GEOID_Place, Name_Place, GEOID_County, Name_County)
Envi<- Envi %>% 
  relocate(GEOID_Place, Name_Place, GEOID_County, Name_County)

write_csv(Comm, "Data/Indicators_PlaceLevel/Community_Indicators_Place.csv")
write_csv(Econ, "Data/Indicators_PlaceLevel/Economy_Indicators_Place.csv")
write_csv(Hous, "Data/Indicators_PlaceLevel/Housing_Indicators_Place.csv")
write_csv(Infra, "Data/Indicators_PlaceLevel/Infrastructure_Indicators_Place.csv")
write_csv(Envi, "Data/Indicators_PlaceLevel/Environment_Indicators_Place.csv")

Econ$Name_County <- NULL
Econ$GEOID_County <- NULL
Econ$Name_Place <- NULL
All_Combined <- left_join(Comm, Econ, by="GEOID_Place")

Hous$Name_County <- NULL
Hous$GEOID_County <- NULL
Hous$Name_Place <- NULL
All_Combined <- left_join(All_Combined, Hous, by="GEOID_Place")

Infra$Name_County <- NULL
Infra$GEOID_County <- NULL
Infra$Name_Place <- NULL
All_Combined<- left_join(All_Combined, Infra, by="GEOID_Place")

Envi$Name_County <- NULL
Envi$GEOID_County <- NULL
Envi$Name_Place <- NULL
All_Combined <- left_join(All_Combined, Envi, by="GEOID_Place")
All_Combined$County <-NULL 

write_csv(All_Combined, "Data/Indicators_PlaceLevel/All_Indicators_Place.csv")

