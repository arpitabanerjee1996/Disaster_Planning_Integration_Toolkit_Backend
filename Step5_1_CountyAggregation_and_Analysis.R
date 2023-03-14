rm(list = ls())
library(readr)
library(tidyverse)

dir.create("Data/Resilience")
#County Level
#Calculating Resilience
Indicators <- read_csv("Data/Normalized_and_Weighed/Weighed_Indicators_CountyLevel/All_Indicators_County.csv")
Resilience <- rowSums(Indicators[ , c(3:ncol(Indicators))])
x <- Indicators %>% 
  select(GEOID, County_Name=Name)
x$Resilience <- Resilience
x <- x %>% mutate(Resilience_Quantile = ntile(Resilience, 5)) %>% 
  mutate(Resilience_Category = case_when(Resilience_Quantile==5~"Very High",
                                         Resilience_Quantile==4~"High",
                                         Resilience_Quantile==3~"Moderate",
                                         Resilience_Quantile==2~"Low",
                                         Resilience_Quantile==1~"Very Low"))


Indicators <- read_csv("Data/Normalized_and_Weighed/Weighed_Indicators_CountyLevel/Community_Indicators_County.csv")
Comm_Resilience <- rowSums(Indicators[ , c(3:ncol(Indicators))])
x$Comm_Resilience <- Comm_Resilience
x <- x %>% mutate(Comm_Resilience_Quantile = ntile(Comm_Resilience, 5)) %>% 
  mutate(Comm_Resilience_Category = case_when(Comm_Resilience_Quantile==5~"Very High",
                                         Comm_Resilience_Quantile==4~"High",
                                         Comm_Resilience_Quantile==3~"Moderate",
                                         Comm_Resilience_Quantile==2~"Low",
                                         Comm_Resilience_Quantile==1~"Very Low"))
y <- Indicators
Indicators_Ranked <- y
for (i in 3:ncol(y)) {
  Indicators_Ranked[,i] <- ntile(y[,i],10)
}


Start <- colnames(Indicators_Ranked[,3])
End <- colnames(Indicators_Ranked[,ncol(Indicators_Ranked)])
Indicators_Ranked <- Indicators_Ranked %>% pivot_longer(all_of(Start):all_of(End),names_to = "Indicators", values_to = "Ranks")

x$Comm_HighRisk <- ""
x$Comm_LowRisk <- ""

for (i in 1:nrow(x)) {
  County_name <- y$Name[i]
  
  dummy <- Indicators_Ranked %>% 
    filter(Ranks == 10) %>% 
    filter(Name == County_name) %>% 
    select(Indicators)
  dummy <- toString(unlist(as.list(dummy)))
  
  x$Comm_HighRisk[i] <- dummy
  
  dummy <- Indicators_Ranked %>%
    filter(Ranks == 5) %>%
    filter(Name == County_name) %>%
    select(Indicators)
  dummy <- toString(unlist(as.list(dummy)))
  x$Comm_LowRisk[i] <- dummy
}

Indicators <- read_csv("Data/Normalized_and_Weighed/Weighed_Indicators_CountyLevel/Economy_Indicators_County.csv")
Econ_Resilience <- rowSums(Indicators[ , c(3:ncol(Indicators))])
x$Econ_Resilience <- Econ_Resilience
x <- x %>% mutate(Econ_Resilience_Quantile = ntile(Econ_Resilience, 5)) %>% 
  mutate(Econ_Resilience_Category = case_when(Econ_Resilience_Quantile==5~"Very High",
                                              Econ_Resilience_Quantile==4~"High",
                                              Econ_Resilience_Quantile==3~"Moderate",
                                              Econ_Resilience_Quantile==2~"Low",
                                              Econ_Resilience_Quantile==1~"Very Low"))
y <- Indicators
Indicators_Ranked <- y
for (i in 3:ncol(y)) {
  Indicators_Ranked[,i] <- ntile(y[,i],10)
}
Start <- colnames(Indicators_Ranked[,3])
End <- colnames(Indicators_Ranked[,ncol(Indicators_Ranked)])
Indicators_Ranked <- Indicators_Ranked %>% pivot_longer(all_of(Start):all_of(End),names_to = "Indicators", values_to = "Ranks")

x$Econ_HighRisk <- ""
x$Econ_LowRisk <- ""

for (i in 1:nrow(x)) {
  County_name <- y$Name[i]
  
  dummy <- Indicators_Ranked %>% 
    filter(Ranks == 10) %>% 
    filter(Name == County_name) %>% 
    select(Indicators)
  dummy <- toString(unlist(as.list(dummy)))
  
  x$Econ_HighRisk[i] <- dummy
  
  dummy <- Indicators_Ranked %>%
    filter(Ranks == 5) %>%
    filter(Name == County_name) %>%
    select(Indicators)
  dummy <- toString(unlist(as.list(dummy)))
  x$Econ_LowRisk[i] <- dummy
}


Indicators <- read_csv("Data/Normalized_and_Weighed/Weighed_Indicators_CountyLevel/Housing_Indicators_County.csv")
Hous_Resilience <- rowSums(Indicators[ , c(3:ncol(Indicators))])
x$Hous_Resilience <- Hous_Resilience
x <- x %>% mutate(Hous_Resilience_Quantile = ntile(Hous_Resilience, 5)) %>% 
  mutate(Hous_Resilience_Category = case_when(Hous_Resilience_Quantile==5~"Very High",
                                              Hous_Resilience_Quantile==4~"High",
                                              Hous_Resilience_Quantile==3~"Moderate",
                                              Hous_Resilience_Quantile==2~"Low",
                                              Hous_Resilience_Quantile==1~"Very Low"))
y <- Indicators
Indicators_Ranked <- y
for (i in 3:ncol(y)) {
  Indicators_Ranked[,i] <- ntile(y[,i],10)
}
Start <- colnames(Indicators_Ranked[,3])
End <- colnames(Indicators_Ranked[,ncol(Indicators_Ranked)])
Indicators_Ranked <- Indicators_Ranked %>% pivot_longer(all_of(Start):all_of(End),names_to = "Indicators", values_to = "Ranks")

x$Hous_HighRisk <- ""
x$Hous_LowRisk <- ""

for (i in 1:nrow(x)) {
  County_name <- y$Name[i]
  
  dummy <- Indicators_Ranked %>% 
    filter(Ranks == 10) %>% 
    filter(Name == County_name) %>% 
    select(Indicators)
  dummy <- toString(unlist(as.list(dummy)))
  
  x$Hous_HighRisk[i] <- dummy
  
  dummy <- Indicators_Ranked %>%
    filter(Ranks == 5) %>%
    filter(Name == County_name) %>%
    select(Indicators)
  dummy <- toString(unlist(as.list(dummy)))
  x$Hous_LowRisk[i] <- dummy
}

Indicators <- read_csv("Data/Normalized_and_Weighed/Weighed_Indicators_CountyLevel/Infrastructure_Indicators_County.csv")
Infra_Resilience <- rowSums(Indicators[ , c(3:ncol(Indicators))])
x$Infra_Resilience <- Infra_Resilience
x <- x %>% mutate(Infra_Resilience_Quantile = ntile(Infra_Resilience, 5)) %>% 
  mutate(Infra_Resilience_Category = case_when(Infra_Resilience_Quantile==5~"Very High",
                                               Infra_Resilience_Quantile==4~"High",
                                               Infra_Resilience_Quantile==3~"Moderate",
                                               Infra_Resilience_Quantile==2~"Low",
                                               Infra_Resilience_Quantile==1~"Very Low"))
y <- Indicators
Indicators_Ranked <- y
for (i in 3:ncol(y)) {
  Indicators_Ranked[,i] <- ntile(y[,i],10)
}
Start <- colnames(Indicators_Ranked[,3])
End <- colnames(Indicators_Ranked[,ncol(Indicators_Ranked)])
Indicators_Ranked <- Indicators_Ranked %>% pivot_longer(all_of(Start):all_of(End),names_to = "Indicators", values_to = "Ranks")

x$Infra_HighRisk <- ""
x$Infra_LowRisk <- ""

for (i in 1:nrow(x)) {
  County_name <- y$Name[i]
  
  dummy <- Indicators_Ranked %>% 
    filter(Ranks == 10) %>% 
    filter(Name == County_name) %>% 
    select(Indicators)
  dummy <- toString(unlist(as.list(dummy)))
  
  x$Infra_HighRisk[i] <- dummy
  
  dummy <- Indicators_Ranked %>%
    filter(Ranks == 5) %>%
    filter(Name == County_name) %>%
    select(Indicators)
  dummy <- toString(unlist(as.list(dummy)))
  x$Infra_LowRisk[i] <- dummy
}

Indicators <- read_csv("Data/Normalized_and_Weighed/Weighed_Indicators_CountyLevel/Environment_Indicators_County.csv")
Envi_Resilience <- rowSums(Indicators[ , c(3:ncol(Indicators))])
x$Envi_Resilience <- Envi_Resilience
x <- x %>% mutate(Envi_Resilience_Quantile = ntile(Envi_Resilience, 5)) %>% 
  mutate(Envi_Resilience_Category = case_when(Envi_Resilience_Quantile==5~"Very High",
                                              Envi_Resilience_Quantile==4~"High",
                                              Envi_Resilience_Quantile==3~"Moderate",
                                              Envi_Resilience_Quantile==2~"Low",
                                              Envi_Resilience_Quantile==1~"Very Low"))
y <- Indicators
Indicators_Ranked <- y
for (i in 3:ncol(y)) {
  Indicators_Ranked[,i] <- ntile(y[,i],10)
}
Start <- colnames(Indicators_Ranked[,3])
End <- colnames(Indicators_Ranked[,ncol(Indicators_Ranked)])
Indicators_Ranked <- Indicators_Ranked %>% pivot_longer(all_of(Start):all_of(End),names_to = "Indicators", values_to = "Ranks")

x$Envi_HighRisk <- ""
x$Envi_LowRisk <- ""

for (i in 1:nrow(x)) {
  County_name <- y$Name[i]
  
  dummy <- Indicators_Ranked %>% 
    filter(Ranks == 10) %>% 
    filter(Name == County_name) %>% 
    select(Indicators)
  dummy <- toString(unlist(as.list(dummy)))
  
  x$Envi_HighRisk[i] <- dummy
  
  dummy <- Indicators_Ranked %>%
    filter(Ranks == 5) %>%
    filter(Name == County_name) %>%
    select(Indicators)
  dummy <- toString(unlist(as.list(dummy)))
  x$Envi_LowRisk[i] <- dummy
}

# Tracts by Resilience
All_Tract <- read_csv("Data/Resilience/Resilience_Categories_Tract.csv")
Tract_per_County <- All_Tract %>% 
  group_by(County) %>% 
  summarize(Total_Number_in_County_Tract = n())
y <- All_Tract %>% 
  select(Tract_Name, County, Resilience_Category) %>% 
  group_by(County, Resilience_Category) %>% 
  summarise(count=n()) %>% 
  pivot_wider(names_from = Resilience_Category, values_from = count)
y <- left_join(y, Tract_per_County, by="County")
colnames(y) <- str_replace_all(colnames(y)," ","")
y[is.na(y)] <- 0
y <- y %>% 
  mutate(Low_Percent = round((Low*100)/Total_Number_in_County_Tract,0),
         VeryLow_Percent = round((VeryLow*100)/Total_Number_in_County_Tract,0),
         High_Percent = round((High*100)/Total_Number_in_County_Tract,0),
         VeryHigh_Percent = round((VeryHigh*100)/Total_Number_in_County_Tract,0),
         Moderate_Percent = round((Moderate*100)/Total_Number_in_County_Tract,0))

Resilience_Categories_Tracts_by_County <- left_join(x,y, c("County_Name"="County"))

write.csv(Resilience_Categories_Tracts_by_County, "Data/Resilience/Resilience_Categories_County.csv")
