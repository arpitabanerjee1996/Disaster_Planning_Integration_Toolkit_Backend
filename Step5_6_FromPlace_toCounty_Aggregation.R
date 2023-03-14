rm(list = ls())
library(readr)
library(tidyverse)

Indicators <- read_csv("Data/Normalized_and_Weighed/Weighed_Indicators_PlaceLevel/All_Indicators_Place.csv")
Indicators$GEOID_Place <- NULL
Indicators$GEOID_County <- NULL
Indicators$Name_Place <- NULL
Indicators <- Indicators %>% 
  filter(!is.na(Name_County))
y <- Indicators %>% 
  group_by(Name_County) %>%
  summarise_if(is.numeric, median, na.rm = TRUE)

x <- y %>% select(Name_County)
Resilience <- rowSums(y[ , c(2:ncol(y))])
x$Resilience <- Resilience
x <- x %>% mutate(Resilience_Quantile = ntile(Resilience, 5)) %>% 
  mutate(Resilience_Category = case_when(Resilience_Quantile==5~"Very High",
                                         Resilience_Quantile==4~"High",
                                         Resilience_Quantile==3~"Moderate",
                                         Resilience_Quantile==2~"Low",
                                         Resilience_Quantile==1~"Very Low"))
# Community
Indicators <- read_csv("Data/Normalized_and_Weighed/Weighed_Indicators_PlaceLevel/Community_Indicators_Place.csv")
Indicators$GEOID_Place <- NULL
Indicators$GEOID_County <- NULL
Indicators$Name_Place <- NULL
Indicators <- Indicators %>% 
  filter(!is.na(Name_County))
y <- Indicators %>% 
  group_by(Name_County) %>%
  summarise_if(is.numeric, median, na.rm = TRUE)
Comm_Resilience <- rowSums(y[ , c(2:ncol(Indicators))])
x$Comm_Resilience <- Comm_Resilience
x <- x %>% mutate(Comm_Resilience_Quantile = ntile(Comm_Resilience, 5)) %>% 
  mutate(Comm_Resilience_Category = case_when(Comm_Resilience_Quantile==5~"Very High",
                                              Comm_Resilience_Quantile==4~"High",
                                              Comm_Resilience_Quantile==3~"Moderate",
                                              Comm_Resilience_Quantile==2~"Low",
                                              Comm_Resilience_Quantile==1~"Very Low"))

Indicators_Ranked <- y
for (i in 2:ncol(y)) {
  Indicators_Ranked[,i] <- ntile(y[,i],10)
}
Start <- colnames(Indicators_Ranked[,2])
End <- colnames(Indicators_Ranked[,ncol(Indicators_Ranked)])
Indicators_Ranked <- Indicators_Ranked %>% pivot_longer(all_of(Start):all_of(End),names_to = "Indicators", values_to = "Ranks")

x$Comm_HighRisk <- ""
x$Comm_LowRisk <- ""

for (i in 1:nrow(x)) {
  County_name <- y$Name_County[i]
  
  dummy <- Indicators_Ranked %>% 
    filter(Ranks == 10) %>% 
    filter(Name_County == County_name) %>% 
    select(Indicators)
  dummy <- toString(unlist(as.list(dummy)))

  x$Comm_HighRisk[i] <- dummy

  dummy <- Indicators_Ranked %>%
    filter(Ranks == 5) %>%
    filter(Name_County == County_name) %>%
    select(Indicators)
  dummy <- toString(unlist(as.list(dummy)))
  x$Comm_LowRisk[i] <- dummy
}


# Comm_RiskAreas <- as.data.frame(matrix(nrow = nrow(All_indicators_Ranked), ncol = 3))
# 
# colnames(Comm_RiskAreas) <- c("Name", "Comm_High_Risk", "Comm_Low_Risk")
# Comm_RiskAreas$Name <- Comm_quant$Name
# Comm_RiskAreas_Longer <- pivot_longer(Comm_quant, Population:Percent_rural, names_to = "Type", values_to = "Value")
# Comm_HighRisk <- Comm_RiskAreas_Longer %>% filter(Value==1)
# Comm_LowRisk <- Comm_RiskAreas_Longer %>% filter(Value==5)
# 
# for (i in 1:nrow(Comm_HighRisk)) {
#   # High Risk Areas
#   County <- Comm_HighRisk$Name[i]
#   y <- Comm_HighRisk %>% filter(Name == County)
#   y <- y$Type
#   library(stringr)
#   y <- toString(y)
#   #print(y)
#   Comm_RiskAreas$Comm_High_Risk[which(Comm_RiskAreas$Name==County)] <- y
#   
#   # Low Risk Areas
#   County <- Comm_LowRisk$Name[i]
#   y <- Comm_LowRisk %>% filter(Name == County)
#   y <- y$Type
#   y <- toString(y)
#   #print(y)
#   Comm_RiskAreas$Comm_Low_Risk[which(Comm_RiskAreas$Name==County)] <- y
# }

# Economy
Indicators <- read_csv("Data/Normalized_and_Weighed/Weighed_Indicators_PlaceLevel/Economy_Indicators_Place.csv")
Indicators$GEOID_Place <- NULL
Indicators$GEOID_County <- NULL
Indicators$Name_Place <- NULL
Indicators <- Indicators %>% 
  filter(!is.na(Name_County))
y <- Indicators %>% 
  group_by(Name_County) %>%
  summarise_if(is.numeric, median, na.rm = TRUE)

Econ_Resilience <- rowSums(y[ , c(2:ncol(Indicators))])
x$Econ_Resilience <- Econ_Resilience
x <- x %>% mutate(Econ_Resilience_Quantile = ntile(Econ_Resilience, 5)) %>% 
  mutate(Econ_Resilience_Category = case_when(Econ_Resilience_Quantile==5~"Very High",
                                              Econ_Resilience_Quantile==4~"High",
                                              Econ_Resilience_Quantile==3~"Moderate",
                                              Econ_Resilience_Quantile==2~"Low",
                                              Econ_Resilience_Quantile==1~"Very Low"))

Indicators_Ranked <- y
for (i in 2:ncol(y)) {
  Indicators_Ranked[,i] <- ntile(y[,i],10)
}
Start <- colnames(Indicators_Ranked[,2])
End <- colnames(Indicators_Ranked[,ncol(Indicators_Ranked)])
Indicators_Ranked <- Indicators_Ranked %>% pivot_longer(all_of(Start):all_of(End),names_to = "Indicators", values_to = "Ranks")

x$Econ_HighRisk <- ""
x$Econ_LowRisk <- ""

for (i in 1:nrow(x)) {
  County_name <- y$Name_County[i]
  
  dummy <- Indicators_Ranked %>% 
    filter(Ranks == 10) %>% 
    filter(Name_County == County_name) %>% 
    select(Indicators)
  dummy <- toString(unlist(as.list(dummy)))
  
  x$Econ_HighRisk[i] <- dummy
  
  dummy <- Indicators_Ranked %>%
    filter(Ranks == 5) %>%
    filter(Name_County == County_name) %>%
    select(Indicators)
  dummy <- toString(unlist(as.list(dummy)))
  x$Econ_LowRisk[i] <- dummy
}

# Housing
Indicators <- read_csv("Data/Normalized_and_Weighed/Weighed_Indicators_PlaceLevel/Housing_Indicators_Place.csv")
Indicators$GEOID_Place <- NULL
Indicators$GEOID_County <- NULL
Indicators$Name_Place <- NULL
Indicators <- Indicators %>% 
  filter(!is.na(Name_County))
y <- Indicators %>% 
  group_by(Name_County) %>%
  summarise_if(is.numeric, median, na.rm = TRUE)

Hous_Resilience <- rowSums(y[ , c(2:ncol(Indicators))])
x$Hous_Resilience <- Hous_Resilience
x <- x %>% mutate(Hous_Resilience_Quantile = ntile(Hous_Resilience, 5)) %>% 
  mutate(Hous_Resilience_Category = case_when(Hous_Resilience_Quantile==5~"Very High",
                                              Hous_Resilience_Quantile==4~"High",
                                              Hous_Resilience_Quantile==3~"Moderate",
                                              Hous_Resilience_Quantile==2~"Low",
                                              Hous_Resilience_Quantile==1~"Very Low"))
Indicators_Ranked <- y
for (i in 2:ncol(y)) {
  Indicators_Ranked[,i] <- ntile(y[,i],10)
}
Start <- colnames(Indicators_Ranked[,2])
End <- colnames(Indicators_Ranked[,ncol(Indicators_Ranked)])
Indicators_Ranked <- Indicators_Ranked %>% pivot_longer(all_of(Start):all_of(End),names_to = "Indicators", values_to = "Ranks")

x$Hous_HighRisk <- ""
x$Hous_LowRisk <- ""

for (i in 1:nrow(x)) {
  County_name <- y$Name_County[i]
  
  dummy <- Indicators_Ranked %>% 
    filter(Ranks == 10) %>% 
    filter(Name_County == County_name) %>% 
    select(Indicators)
  dummy <- toString(unlist(as.list(dummy)))
  
  x$Hous_HighRisk[i] <- dummy
  
  dummy <- Indicators_Ranked %>%
    filter(Ranks == 5) %>%
    filter(Name_County == County_name) %>%
    select(Indicators)
  dummy <- toString(unlist(as.list(dummy)))
  x$Hous_LowRisk[i] <- dummy
}

#Infrastructure
Indicators <- read_csv("Data/Normalized_and_Weighed/Weighed_Indicators_PlaceLevel/Infrastructure_Indicators_Place.csv")
Indicators$GEOID_Place <- NULL
Indicators$GEOID_County <- NULL
Indicators$Name_Place <- NULL
Indicators <- Indicators %>% 
  filter(!is.na(Name_County))
y <- Indicators %>% 
  group_by(Name_County) %>%
  summarise_if(is.numeric, median, na.rm = TRUE)

Infra_Resilience <- rowSums(y[ , c(2:ncol(Indicators))])
x$Infra_Resilience <- Infra_Resilience
x <- x %>% mutate(Infra_Resilience_Quantile = ntile(Infra_Resilience, 5)) %>% 
  mutate(Infra_Resilience_Category = case_when(Infra_Resilience_Quantile==5~"Very High",
                                               Infra_Resilience_Quantile==4~"High",
                                               Infra_Resilience_Quantile==3~"Moderate",
                                               Infra_Resilience_Quantile==2~"Low",
                                               Infra_Resilience_Quantile==1~"Very Low"))
Indicators_Ranked <- y
for (i in 2:ncol(y)) {
  Indicators_Ranked[,i] <- ntile(y[,i],10)
}
Start <- colnames(Indicators_Ranked[,2])
End <- colnames(Indicators_Ranked[,ncol(Indicators_Ranked)])
Indicators_Ranked <- Indicators_Ranked %>% pivot_longer(all_of(Start):all_of(End),names_to = "Indicators", values_to = "Ranks")

x$Infra_HighRisk <- ""
x$Infra_LowRisk <- ""

for (i in 1:nrow(x)) {
  County_name <- y$Name_County[i]
  
  dummy <- Indicators_Ranked %>% 
    filter(Ranks == 10) %>% 
    filter(Name_County == County_name) %>% 
    select(Indicators)
  dummy <- toString(unlist(as.list(dummy)))
  
  x$Infra_HighRisk[i] <- dummy
  
  dummy <- Indicators_Ranked %>%
    filter(Ranks == 5) %>%
    filter(Name_County == County_name) %>%
    select(Indicators)
  dummy <- toString(unlist(as.list(dummy)))
  x$Infra_LowRisk[i] <- dummy
}

# Environment
Indicators <- read_csv("Data/Normalized_and_Weighed/Weighed_Indicators_PlaceLevel/Environment_Indicators_Place.csv")
Indicators$GEOID_Place <- NULL
Indicators$GEOID_County <- NULL
Indicators$Name_Place <- NULL
Indicators <- Indicators %>% 
  filter(!is.na(Name_County))
y <- Indicators %>% 
  group_by(Name_County) %>%
  summarise_if(is.numeric, median, na.rm = TRUE)

Envi_Resilience <- rowSums(y[ , c(2:ncol(Indicators))])
x$Envi_Resilience <- Envi_Resilience
x <- x %>% mutate(Envi_Resilience_Quantile = ntile(Envi_Resilience, 5)) %>% 
  mutate(Envi_Resilience_Category = case_when(Envi_Resilience_Quantile==5~"Very High",
                                              Envi_Resilience_Quantile==4~"High",
                                              Envi_Resilience_Quantile==3~"Moderate",
                                              Envi_Resilience_Quantile==2~"Low",
                                              Envi_Resilience_Quantile==1~"Very Low"))

Indicators_Ranked <- y
for (i in 2:ncol(y)) {
  Indicators_Ranked[,i] <- ntile(y[,i],10)
}
Start <- colnames(Indicators_Ranked[,2])
End <- colnames(Indicators_Ranked[,ncol(Indicators_Ranked)])
Indicators_Ranked <- Indicators_Ranked %>% pivot_longer(all_of(Start):all_of(End),names_to = "Indicators", values_to = "Ranks")

x$Envi_HighRisk <- ""
x$Envi_LowRisk <- ""

for (i in 1:nrow(x)) {
  County_name <- y$Name_County[i]
  
  dummy <- Indicators_Ranked %>% 
    filter(Ranks == 10) %>% 
    filter(Name_County == County_name) %>% 
    select(Indicators)
  dummy <- toString(unlist(as.list(dummy)))
  
  x$Envi_HighRisk[i] <- dummy
  
  dummy <- Indicators_Ranked %>%
    filter(Ranks == 5) %>%
    filter(Name_County == County_name) %>%
    select(Indicators)
  dummy <- toString(unlist(as.list(dummy)))
  x$Envi_LowRisk[i] <- dummy
}


# Places by Resilience
All_Place <- read_csv("Data/Resilience/Resilience_Categories_Place.csv")
Place_per_County <- All_Place %>% 
  group_by(Name_County) %>% 
  summarize(Total_Number_in_County_Place = n())
y <- All_Place %>% 
  select(Name_Place, Name_County, Resilience_Category) %>% 
  group_by(Name_County, Resilience_Category) %>% 
  summarise(count=n()) %>% 
  pivot_wider(names_from = Resilience_Category, values_from = count)
y <- left_join(y, Place_per_County, by="Name_County")
colnames(y) <- str_replace_all(colnames(y)," ","")
y[is.na(y)] <- 0
y <- y %>% 
  mutate(Low_Percent = round((Low*100)/Total_Number_in_County_Place,0),
         VeryLow_Percent = round((VeryLow*100)/Total_Number_in_County_Place,0),
         High_Percent = round((High*100)/Total_Number_in_County_Place,0),
         VeryHigh_Percent = round((VeryHigh*100)/Total_Number_in_County_Place,0),
         Moderate_Percent = round((Moderate*100)/Total_Number_in_County_Place,0))

Resilience_Categories_Places_by_County <- left_join(x,y, by="Name_County")

write_csv(Resilience_Categories_Places_by_County,
          "Data/Resilience/Resilience_Categories_Places_by_County.csv")
  
