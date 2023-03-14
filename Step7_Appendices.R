rm(list=ls())
library(readr)
library(tidyverse)
library(readxl)
library(stringr)
dir.create("Data/Appendices")

Descriptions <- read_excel("Step1_Final Listing of Indicators.xlsx")
Descriptions <-Descriptions %>% 
  select(Definition = 
           `Indicator Definition`,
         Indicator = `Variable Name`,
         "Low Risk Interpretation" = `Interpretation as Low Risk`,
         "High Risk Interpretation" = `Interpretation as High Risk`)

# County Level
Indicators <- read_csv("Data/Normalized_and_Weighed/Weighed_Indicators_CountyLevel/All_Indicators_County.csv")
y <- Indicators
Indicators_Ranked <- y
for (i in 3:ncol(y)) {
  Indicators_Ranked[,i] <- ntile(y[,i],5)
}

Start <- colnames(Indicators_Ranked[,3])
End <- colnames(Indicators_Ranked[,ncol(Indicators_Ranked)])

Indicators_Ranked <- Indicators_Ranked %>% 
  pivot_longer(all_of(Start):all_of(End), names_to = "Indicator",
               values_to = "Values") %>% 
  mutate(Risk_Level=case_when(Values == 1 ~ "Very High",
                              Values == 2 ~ "High",
                              Values == 3 ~ "Moderate",
                              Values == 4 ~ "Low",
                              Values == 5 ~ "Very Low"))
Appendix1 <- left_join(Indicators_Ranked,
          Descriptions,
          by="Indicator")
Appendix1 <- Appendix1 %>% 
  filter(!is.na(Definition)) %>% 
  select(County=Name, County_GEOID=GEOID,
           Indicator, Definition, `Low Risk Interpretation`,
           `High Risk Interpretation`, Risk_Level)

write_csv(Appendix1,
          "Data/Appendices/Appendix1.csv")


# County from Rural
Indicators <- read_csv("Data/Normalized_and_Weighed/Weighed_Indicators_RuralLevel/All_Indicators_Rural.csv")
Indicators$GEOID <- NULL
Indicators$Name <- NULL
y <- Indicators %>% 
  group_by(County) %>%
  summarise_if(is.numeric, median, na.rm = TRUE)

Indicators_Ranked <- y
for (i in 2:ncol(y)) {
  Indicators_Ranked[,i] <- ntile(y[,i],5)
}

Start <- colnames(Indicators_Ranked[,2])
End <- colnames(Indicators_Ranked[,ncol(Indicators_Ranked)])

Indicators_Ranked <- Indicators_Ranked %>% 
  pivot_longer(all_of(Start):all_of(End), names_to = "Indicator",
               values_to = "Values") %>% 
  mutate(Risk_Level=case_when(Values == 1 ~ "Very High",
                              Values == 2 ~ "High",
                              Values == 3 ~ "Moderate",
                              Values == 4 ~ "Low",
                              Values == 5 ~ "Very Low"))
Indicators_Ranked_Rural <- Indicators_Ranked %>% 
  select(County, Indicator, Risk_Level_Rural=Risk_Level) %>% 
  mutate(key=paste(County, Indicator))
  
# County from Urban
Indicators <- read_csv("Data/Normalized_and_Weighed/Weighed_Indicators_UrbanLevel/All_Indicators_Urban.csv")
Indicators$GEOID <- NULL
Indicators$Name <- NULL
y <- Indicators %>% 
  group_by(County) %>%
  summarise_if(is.numeric, median, na.rm = TRUE)

Indicators_Ranked <- y
for (i in 2:ncol(y)) {
  Indicators_Ranked[,i] <- ntile(y[,i],5)
}

Start <- colnames(Indicators_Ranked[,2])
End <- colnames(Indicators_Ranked[,ncol(Indicators_Ranked)])

Indicators_Ranked <- Indicators_Ranked %>% 
  pivot_longer(all_of(Start):all_of(End), names_to = "Indicator",
               values_to = "Values") %>% 
  mutate(Risk_Level=case_when(Values == 1 ~ "Very High",
                              Values == 2 ~ "High",
                              Values == 3 ~ "Moderate",
                              Values == 4 ~ "Low",
                              Values == 5 ~ "Very Low"))
Indicators_Ranked_Urban <- Indicators_Ranked %>% 
  select(County, Indicator, Risk_Level) %>% 
  mutate(key=paste(County, Indicator)) %>% 
  select(key,Risk_Level_Urban=Risk_Level)

# County from Places
Indicators <- read_csv("Data/Normalized_and_Weighed/Weighed_Indicators_PlaceLevel/All_Indicators_Place.csv")
Indicators$GEOID_Place <- NULL
Indicators$GEOID_County <- NULL
Indicators$Name_Place <- NULL
Indicators <- Indicators %>% 
  filter(!is.na(Name_County))
y <- Indicators %>% 
  group_by(Name_County) %>%
  summarise_if(is.numeric, median, na.rm = TRUE)

Indicators_Ranked <- y
for (i in 2:ncol(y)) {
  Indicators_Ranked[,i] <- ntile(y[,i],5)
}

Start <- colnames(Indicators_Ranked[,2])
End <- colnames(Indicators_Ranked[,ncol(Indicators_Ranked)])

Indicators_Ranked <- Indicators_Ranked %>% 
  pivot_longer(all_of(Start):all_of(End), names_to = "Indicator",
               values_to = "Values") %>% 
  mutate(Risk_Level=case_when(Values == 1 ~ "Very High",
                              Values == 2 ~ "High",
                              Values == 3 ~ "Moderate",
                              Values == 4 ~ "Low",
                              Values == 5 ~ "Very Low"))
Indicators_Ranked_Place <- Indicators_Ranked %>% 
  select(County=Name_County, Indicator, Risk_Level) %>% 
  mutate(key=paste(County, Indicator)) %>% 
  select(key,Risk_Level_Place=Risk_Level)

Indicators_Ranked_Place$key <- 
  str_replace_all(Indicators_Ranked_Place$key,".x", "")
Indicators_Ranked_Urban$key <- 
  str_replace_all(Indicators_Ranked_Urban$key,".x", "")
Indicators_Ranked_Rural$key <- 
  str_replace_all(Indicators_Ranked_Rural$key,".x", "")


x <- left_join(Indicators_Ranked_Rural,Indicators_Ranked_Urban, by="key")
x <- left_join(x, Indicators_Ranked_Place, by="key")
x <- x %>% 
  select(County, Indicator, Risk_Level_Rural, Risk_Level_Urban, Risk_Level_Place)

write_csv(x,
          "Data/Appendices/Appendix2.csv")


