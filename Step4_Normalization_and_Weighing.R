rm(list = ls())

library(tidyverse)
directory <- getwd()
dir.create("Data/Normalized_and_Weighed")
dir.create("Data/Normalized_and_Weighed/Weighed_Indicators_CountyLevel")
dir.create("Data/Normalized_and_Weighed/Weighed_Indicators_TractLevel")
dir.create("Data/Normalized_and_Weighed/Weighed_Indicators_PlaceLevel")
dir.create("Data/Normalized_and_Weighed/Weighed_Indicators_RuralLevel")
dir.create("Data/Normalized_and_Weighed/Weighed_Indicators_UrbanLevel")

# Indicators at the County Level

library(readr)

# All_Indicators
All_Indicators <- read_csv("Data/Indicators_CountyLevel/All_Indicators_County.csv")
x <- All_Indicators

Negatively_weighed_columns <- c(# Community
  "Population", "Young_percent", "Old_percent","NonWhite_Percent","NotMarried_Percent","Disabled_Percent","Recent_Immigrants_Percent",
  "LargeFam_Percent","SingleParent_Percent","Elderly_growth_percent",
  "Avg_HHsize","Percent_LanguageBarrier","Percent_AssistanceNeed",                        
  "Percent_Commuters","Percent_NoHealthIns","Physically_Unhealthy_Days","Violent_Crime_Rate",                            
  "Inadequate_Facilities","COVID-19_death_rate" ,"Percent_Food_Insecure",
  "Percent_Limited_Access_to_Healthy_Foods","Percent_Disconnected_Youth","Segregation_index",                             
  "Percent_rural",
  # Economy
  "Agro_Percent","UnstableEmployment_Percent","LowIncome_Percent",                               
  "PT","PT_LQ","Agri","Agri_LQ","Manufacturing","Manufacturing_LQ","Wholesale",
  "Wholesale_LQ","Arts","Arts_LQ","GINI_Index",
  "Percent_Children_in_Poverty","Gender_Pay_Gap",
  "Percent_income_required_for_childcare_expenses","CommuteTime",
  "Vice_related_business_rate","Recreation_related_business_rate",
  # Housing
  "Total",
  "OldHomes","Rented","Vacancy","Units_SingleFamily","MobileHomes","Overcrowding",
  "Rental_Overcrowding","Renters_Cost_IncomeRatio","Rental_CostBurden_below20000",
  "NoVehicle_Pop.x","GroupQuarters","Percent_HousingProblems","Percent_Section8",
  # Infrastructure
  "Percent_PoorCondition_Bridges","Percent_commuters_by_transit",
  "Other_county_workers","Workers_to_OtherCounties","Commuters_withinCounty", 
  # Environment
  "Avg_temp","Max_temp","Min_temp","Precipitation","CO8",
  "Pb3","NO2AM","O3","PM10","PM2.5","PM2Point5","pct_manure_acres_ln","pct_disease_acres_ln",
  "pct_harvested_acres_ln","pct_au_ln","fungicide_ln",
  "herbicide_ln","insecticide_ln","std_coal_prim_pop_ln",
  "std_metal_prim_pop_ln","std_nonmetal_prim_pop_ln",
  "std_sandandgravel_prim_pop_ln","std_stone_prim_pop_ln","Radon.x",
  "Per_TotPopSS.x", "D303_Percent_ln", "ALLNPDESperKM_ln","CaAve_ln",
  "Kave_ln","NO3Ave.x","ClAve_ln" ,"SO4Ave_ln","HgAve.x",
  "AvgOfD3_ave.x","W_As_ln","W_Ba_ln","W_Cd_ln","W_Cr_ln",
  "W_CN_ln","W_FL_ln","W_HG_ln","W_NO3_ln","W_NO2_ln")

x[,3:ncol(x)] <- lapply(x[,3:ncol(x)],as.numeric)
x[,which(colnames(x) %in% Negatively_weighed_columns)] <- 
  (x[,which(colnames(x) %in% Negatively_weighed_columns)]*-1)

x[x==Inf] <- 0
x[x==-Inf] <- 0
x[is.na(x)] <- 0

y <- x
for (i in 3:ncol(x)) {
  y[,i] <-
    (x[,i]-min(x[,i]))/(max(x[,i])-min(x[,i]))
}

write_csv(y, "Data/Normalized_and_Weighed/Weighed_Indicators_CountyLevel/All_Indicators_County.csv")

# Community Indicators
Index_Start <- which(colnames(x)=="Population")
Index_End <- which(colnames(x)=="Percent_rural")
y <- x[,c(1,2,Index_Start:Index_End)]
write_csv(y, "Data/Normalized_and_Weighed/Weighed_Indicators_CountyLevel/Community_Indicators_County.csv")

# Economy Indicators
Index_Start <- which(colnames(x)=="Income")
Index_End <- which(colnames(x)=="Recreation_related_business_rate")
y <- x[,c(1,2,Index_Start:Index_End)]
write_csv(y, "Data/Normalized_and_Weighed/Weighed_Indicators_CountyLevel/Economy_Indicators_County.csv")

# Housing Indicators
Index_Start <- which(colnames(x)=="Total")
Index_End <- which(colnames(x)=="Percent_Section8")
y <- x[,c(1,2,Index_Start:Index_End)]
write_csv(y, "Data/Normalized_and_Weighed/Weighed_Indicators_CountyLevel/Housing_Indicators_County.csv")

# Infrastructure Indicators
Index_Start <- which(colnames(x)=="Pubtrans_Dependent")
Index_End <- which(colnames(x)=="Percent_BroadbandAccess")
y <- x[,c(1,2,Index_Start:Index_End)]
write_csv(y, "Data/Normalized_and_Weighed/Weighed_Indicators_CountyLevel/Infrastructure_Indicators_County.csv")

# Environment
Index_Start <- which(colnames(x)=="Percent_BroadbandAccess")+1
Index_End <- ncol(x)
y <- x[,c(1,2,Index_Start:Index_End)]
write_csv(y, "Data/Normalized_and_Weighed/Weighed_Indicators_CountyLevel/Environment_Indicators_County.csv")


# Indicators at the Tract Level


rm(list=ls())

# All_Indicators
All_Indicators <- read_csv("Data/Indicators_TractLevel/All_Indicators_Tract.csv")
x <- All_Indicators

Negatively_weighed_columns <- c(# Community
  "Population", "Young_percent", "Old_percent","NonWhite_Percent","NotMarried_Percent","Disabled_Percent","Recent_Immigrants_Percent",
  "LargeFam_Percent","SingleParent_Percent","Elderly_growth_percent",
  "Avg_HHsize","Percent_LanguageBarrier","Percent_AssistanceNeed",                        
  "Percent_Commuters","Percent_NoHealthIns","Physically_Unhealthy_Days","Violent_Crime_Rate",                            
  "Inadequate_Facilities","COVID-19_death_rate" ,"Percent_Food_Insecure",
  "Percent_Limited_Access_to_Healthy_Foods","Percent_Disconnected_Youth","Segregation_index",                             
  "Percent_rural",
  # Economy
  "Agro_Percent","UnstableEmployment_Percent","LowIncome_Percent",                               
  "PT","PT_LQ","Agri","Agri_LQ","Manufacturing","Manufacturing_LQ","Wholesale",
  "Wholesale_LQ","Arts","Arts_LQ","GINI_Index",
  "Percent_Children_in_Poverty","Gender_Pay_Gap",
  "Percent_income_required_for_childcare_expenses","CommuteTime",
  "Vice_related_business_rate","Recreation_related_business_rate",
  # Housing
  "Total",
  "OldHomes","Rented","Vacancy","Units_SingleFamily","MobileHomes","Overcrowding",
  "Rental_Overcrowding","Renters_Cost_IncomeRatio","Rental_CostBurden_below20000",
  "NoVehicle_Pop.x","GroupQuarters","Percent_HousingProblems","Percent_Section8",
  # Infrastructure
  "Percent_PoorCondition_Bridges","Percent_commuters_by_transit",
  "Other_Tract_workers","Workers_to_OtherCounties","Commuters_withinTract", 
  # Environment
  "Avg_temp","Max_temp","Min_temp","Precipitation","CO8",
  "Pb3","NO2AM","O3","PM10","PM2.5","PM2Point5","pct_manure_acres_ln","pct_disease_acres_ln",
  "pct_harvested_acres_ln","pct_au_ln","fungicide_ln",
  "herbicide_ln","insecticide_ln","std_coal_prim_pop_ln",
  "std_metal_prim_pop_ln","std_nonmetal_prim_pop_ln",
  "std_sandandgravel_prim_pop_ln","std_stone_prim_pop_ln","Radon.x",
  "Per_TotPopSS.x", "D303_Percent_ln", "ALLNPDESperKM_ln","CaAve_ln",
  "Kave_ln","NO3Ave.x","ClAve_ln" ,"SO4Ave_ln","HgAve.x",
  "AvgOfD3_ave.x","W_As_ln","W_Ba_ln","W_Cd_ln","W_Cr_ln",
  "W_CN_ln","W_FL_ln","W_HG_ln","W_NO3_ln","W_NO2_ln")

x <- x %>% relocate(GEOID, Name, County)

x[,4:ncol(x)] <- lapply(x[,4:ncol(x)],as.numeric)

x[,which(colnames(x) %in% Negatively_weighed_columns)] <- 
  (x[,which(colnames(x) %in% Negatively_weighed_columns)]*-1)

x[x==Inf] <- 0
x[x==-Inf] <- 0
x[is.na(x)] <- 0

y <- x
for (i in 4:ncol(x)) {
  if(max(x[,i], na.rm = TRUE)-min(x[,i], na.rm = TRUE) == 0) {y[,i] = 0.5}
  else{y[,i] <-
    (x[,i]-min(x[,i], na.rm = TRUE))/(max(x[,i], na.rm = TRUE)-min(x[,i], na.rm=TRUE))}
}

write_csv(y, "Data/Normalized_and_Weighed/Weighed_Indicators_TractLevel/All_Indicators_Tract.csv")
x <- y

# Community Indicators
Index_Start <- which(colnames(x)=="Population")
Index_End <- which(colnames(x)=="Percent_rural")
y <- x[,c(1,2,3,Index_Start:Index_End)]
write_csv(y, "Data/Normalized_and_Weighed/Weighed_Indicators_TractLevel/Community_Indicators_Tract.csv")

# Economy Indicators
Index_Start <- which(colnames(x)=="Income")
Index_End <- which(colnames(x)=="Recreation_related_business_rate")
y <- x[,c(1,2,3,Index_Start:Index_End)]
write_csv(y, "Data/Normalized_and_Weighed/Weighed_Indicators_TractLevel/Economy_Indicators_Tract.csv")

# Housing Indicators
Index_Start <- which(colnames(x)=="Total")
Index_End <- which(colnames(x)=="Percent_Section8")
y <- x[,c(1,2,3,Index_Start:Index_End)]
write_csv(y, "Data/Normalized_and_Weighed/Weighed_Indicators_TractLevel/Housing_Indicators_Tract.csv")

# Infrastructure Indicators
Index_Start <- which(colnames(x)=="Pubtrans_Dependent")
Index_End <- which(colnames(x)=="Percent_BroadbandAccess")
y <- x[,c(1,2,3,Index_Start:Index_End)]
write_csv(y, "Data/Normalized_and_Weighed/Weighed_Indicators_TractLevel/Infrastructure_Indicators_Tract.csv")

# Environment
Index_Start <- which(colnames(x)=="Percent_BroadbandAccess")+1
Index_End <- ncol(x)
y <- x[,c(1,2,3,Index_Start:Index_End)]
write_csv(y, "Data/Normalized_and_Weighed/Weighed_Indicators_TractLevel/Environment_Indicators_Tract.csv")

# Place Level Indicators
# All_Indicators
All_Indicators <- read_csv("Data/Indicators_PlaceLevel/All_Indicators_Place.csv")
x <- All_Indicators

Negatively_weighed_columns <- c(# Community
  "Population", "Young_percent", "Old_percent","NonWhite_Percent","NotMarried_Percent","Disabled_Percent","Recent_Immigrants_Percent",
  "LargeFam_Percent","SingleParent_Percent","Elderly_growth_percent",
  "Avg_HHsize","Percent_LanguageBarrier","Percent_AssistanceNeed",                        
  "Percent_Commuters","Percent_NoHealthIns","Physically_Unhealthy_Days","Violent_Crime_Rate",                            
  "Inadequate_Facilities","COVID-19_death_rate" ,"Percent_Food_Insecure",
  "Percent_Limited_Access_to_Healthy_Foods","Percent_Disconnected_Youth","Segregation_index",                             
  "Percent_rural",
  # Economy
  "Agro_Percent","UnstableEmployment_Percent","LowIncome_Percent",                               
  "PT","PT_LQ","Agri","Agri_LQ","Manufacturing","Manufacturing_LQ","Wholesale",
  "Wholesale_LQ","Arts","Arts_LQ","GINI_Index",
  "Percent_Children_in_Poverty","Gender_Pay_Gap",
  "Percent_income_required_for_childcare_expenses","CommuteTime",
  "Vice_related_business_rate","Recreation_related_business_rate",
  # Housing
  "Total",
  "OldHomes","Rented","Vacancy","Units_SingleFamily","MobileHomes","Overcrowding",
  "Rental_Overcrowding","Renters_Cost_IncomeRatio","Rental_CostBurden_below20000",
  "NoVehicle_Pop.x","GroupQuarters","Percent_HousingProblems","Percent_Section8",
  # Infrastructure
  "Percent_PoorCondition_Bridges","Percent_commuters_by_transit",
  "Other_Place_workers","Workers_to_OtherCounties","Commuters_withinPlace", 
  # Environment
  "Avg_temp","Max_temp","Min_temp","Precipitation","CO8",
  "Pb3","NO2AM","O3","PM10","PM2.5","PM2Point5","pct_manure_acres_ln","pct_disease_acres_ln",
  "pct_harvested_acres_ln","pct_au_ln","fungicide_ln",
  "herbicide_ln","insecticide_ln","std_coal_prim_pop_ln",
  "std_metal_prim_pop_ln","std_nonmetal_prim_pop_ln",
  "std_sandandgravel_prim_pop_ln","std_stone_prim_pop_ln","Radon.x",
  "Per_TotPopSS.x", "D303_Percent_ln", "ALLNPDESperKM_ln","CaAve_ln",
  "Kave_ln","NO3Ave.x","ClAve_ln" ,"SO4Ave_ln","HgAve.x",
  "AvgOfD3_ave.x","W_As_ln","W_Ba_ln","W_Cd_ln","W_Cr_ln",
  "W_CN_ln","W_FL_ln","W_HG_ln","W_NO3_ln","W_NO2_ln")

x <- x %>% filter(!is.na(Name_County))
x[,5:ncol(x)] <- lapply(x[,5:ncol(x)],as.numeric)

x[,which(colnames(x) %in% Negatively_weighed_columns)] <- x[,which(colnames(x) %in% Negatively_weighed_columns)]*-1

x[is.na(x)] <- 0
x[x==-Inf] <- 0
x[x==Inf] <- 0
y <- x

for (i in 5:ncol(x)) {
  if(max(x[,i], na.rm = TRUE)-min(x[,i], na.rm = TRUE) == 0) {y[,i] = 0.5}
  else{y[,i] <-(x[,i]-min(x[,i], na.rm = TRUE))/(max(x[,i], na.rm = TRUE)-min(x[,i], na.rm=TRUE))}
}

write_csv(y, "Data/Normalized_and_Weighed/Weighed_Indicators_PlaceLevel/All_Indicators_Place.csv")
x <- y



# Community Indicators
Index_Start <- which(colnames(x)=="Population")
Index_End <- which(colnames(x)=="Percent_rural")
y <- x[,c(1,2,3,4,Index_Start:Index_End)]
write_csv(y, "Data/Normalized_and_Weighed/Weighed_Indicators_PlaceLevel/Community_Indicators_Place.csv")

# Economy Indicators
Index_Start <- which(colnames(x)=="Income")
Index_End <- which(colnames(x)=="Recreation_related_business_rate")
y <- x[,c(1,2,3,4,Index_Start:Index_End)]
write_csv(y, "Data/Normalized_and_Weighed/Weighed_Indicators_PlaceLevel/Economy_Indicators_Place.csv")

# Housing Indicators
Index_Start <- which(colnames(x)=="Total")
Index_End <- which(colnames(x)=="Percent_Section8")
y <- x[,c(1,2,3,4,Index_Start:Index_End)]
write_csv(y, "Data/Normalized_and_Weighed/Weighed_Indicators_PlaceLevel/Housing_Indicators_Place.csv")

# Infrastructure Indicators
Index_Start <- which(colnames(x)=="Pubtrans_Dependent")
Index_End <- which(colnames(x)=="Percent_BroadbandAccess")
y <- x[,c(1,2,3,4,Index_Start:Index_End)]
write_csv(y, "Data/Normalized_and_Weighed/Weighed_Indicators_PlaceLevel/Infrastructure_Indicators_Place.csv")

# Environment
Index_Start <- which(colnames(x)=="Percent_BroadbandAccess")+1
Index_End <- ncol(x)
y <- x[,c(1,2,3,4,Index_Start:Index_End)]
write_csv(y, "Data/Normalized_and_Weighed/Weighed_Indicators_PlaceLevel/Environment_Indicators_Place.csv")



# Rural Level Indicators
# All_Indicators
All_Indicators <- read_csv("Data/Indicators_RuralLevel/All_Indicators_Rural.csv")
x <- All_Indicators

Negatively_weighed_columns <- c(# Community
  "Population", "Young_percent", "Old_percent","NonWhite_Percent","NotMarried_Percent","Disabled_Percent","Recent_Immigrants_Percent",
  "LargeFam_Percent","SingleParent_Percent","Elderly_growth_percent",
  "Avg_HHsize","Percent_LanguageBarrier","Percent_AssistanceNeed",                        
  "Percent_Commuters","Percent_NoHealthIns","Physically_Unhealthy_Days","Violent_Crime_Rate",                            
  "Inadequate_Facilities","COVID-19_death_rate" ,"Percent_Food_Insecure",
  "Percent_Limited_Access_to_Healthy_Foods","Percent_Disconnected_Youth","Segregation_index",                             
  "Percent_rural",
  # Economy
  "Agro_Percent","UnstableEmployment_Percent","LowIncome_Percent",                               
  "PT","PT_LQ","Agri","Agri_LQ","Manufacturing","Manufacturing_LQ","Wholesale",
  "Wholesale_LQ","Arts","Arts_LQ","GINI_Index",
  "Percent_Children_in_Poverty","Gender_Pay_Gap",
  "Percent_income_required_for_childcare_expenses","CommuteTime",
  "Vice_related_business_rate","Recreation_related_business_rate",
  # Housing
  "Total",
  "OldHomes","Rented","Vacancy","Units_SingleFamily","MobileHomes","Overcrowding",
  "Rental_Overcrowding","Renters_Cost_IncomeRatio","Rental_CostBurden_below20000",
  "NoVehicle_Pop.x","GroupQuarters","Percent_HousingProblems","Percent_Section8",
  # Infrastructure
  "Percent_PoorCondition_Bridges","Percent_commuters_by_transit",
  "Other_Rural_workers","Workers_to_OtherCounties","Commuters_withinRural", 
  # Environment
  "Avg_temp","Max_temp","Min_temp","Precipitation","CO8",
  "Pb3","NO2AM","O3","PM10","PM2.5","PM2Point5","pct_manure_acres_ln","pct_disease_acres_ln",
  "pct_harvested_acres_ln","pct_au_ln","fungicide_ln",
  "herbicide_ln","insecticide_ln","std_coal_prim_pop_ln",
  "std_metal_prim_pop_ln","std_nonmetal_prim_pop_ln",
  "std_sandandgravel_prim_pop_ln","std_stone_prim_pop_ln","Radon.x",
  "Per_TotPopSS.x", "D303_Percent_ln", "ALLNPDESperKM_ln","CaAve_ln",
  "Kave_ln","NO3Ave.x","ClAve_ln" ,"SO4Ave_ln","HgAve.x",
  "AvgOfD3_ave.x","W_As_ln","W_Ba_ln","W_Cd_ln","W_Cr_ln",
  "W_CN_ln","W_FL_ln","W_HG_ln","W_NO3_ln","W_NO2_ln")

x$Population <- x$Population.x
x$Population.x <- NULL
x$Population.y <- NULL
x$Tract_FIPS <- NULL
x <- x %>% 
  relocate(GEOID, Name, County, Population)

x[,4:ncol(x)] <- lapply(x[,4:ncol(x)],as.numeric)
x[is.na(x)] <- 0
x[x==-Inf] <- 0
x[x==Inf] <- 0
x[,which(colnames(x) %in% Negatively_weighed_columns)] <- x[,which(colnames(x) %in% Negatively_weighed_columns)]*-1

y <- x

for (i in 4:ncol(x)) {
  if(max(x[,i], na.rm = TRUE)-min(x[,i], na.rm = TRUE) == 0) {y[,i] = 0.5}
  else{y[,i] <-(x[,i]-min(x[,i], na.rm = TRUE))/(max(x[,i], na.rm = TRUE)-min(x[,i], na.rm=TRUE))}
}

write_csv(y, "Data/Normalized_and_Weighed/Weighed_Indicators_RuralLevel/All_Indicators_Rural.csv")
x <- y

# Community Indicators
Index_Start <- which(colnames(x)=="Population")
Index_End <- which(colnames(x)=="Percent_rural")
y <- x[,c(1,2,3,Index_Start:Index_End)]
write_csv(y, "Data/Normalized_and_Weighed/Weighed_Indicators_RuralLevel/Community_Indicators_Rural.csv")

# Economy Indicators
Index_Start <- which(colnames(x)=="Income")
Index_End <- which(colnames(x)=="Recreation_related_business_rate")
y <- x[,c(1,2,3,Index_Start:Index_End)]
write_csv(y, "Data/Normalized_and_Weighed/Weighed_Indicators_RuralLevel/Economy_Indicators_Rural.csv")

# Housing Indicators
Index_Start <- which(colnames(x)=="Total")
Index_End <- which(colnames(x)=="Percent_Section8")
y <- x[,c(1,2,3,Index_Start:Index_End)]
write_csv(y, "Data/Normalized_and_Weighed/Weighed_Indicators_RuralLevel/Housing_Indicators_Rural.csv")

# Infrastructure Indicators
Index_Start <- which(colnames(x)=="Pubtrans_Dependent")
Index_End <- which(colnames(x)=="Percent_BroadbandAccess")
y <- x[,c(1,2,3,Index_Start:Index_End)]
write_csv(y, "Data/Normalized_and_Weighed/Weighed_Indicators_RuralLevel/Infrastructure_Indicators_Rural.csv")

# Environment
Index_Start <- which(colnames(x)=="Percent_BroadbandAccess")+1
Index_End <- ncol(x)
y <- x[,c(1,2,3,Index_Start:Index_End)]
write_csv(y, "Data/Normalized_and_Weighed/Weighed_Indicators_RuralLevel/Environment_Indicators_Rural.csv")

# Urban Level Indicators
# All_Indicators
All_Indicators <- read_csv("Data/Indicators_UrbanLevel/All_Indicators_Urban.csv")
x <- All_Indicators

Negatively_weighed_columns <- c(# Community
  "Population", "Young_percent", "Old_percent","NonWhite_Percent","NotMarried_Percent","Disabled_Percent","Recent_Immigrants_Percent",
  "LargeFam_Percent","SingleParent_Percent","Elderly_growth_percent",
  "Avg_HHsize","Percent_LanguageBarrier","Percent_AssistanceNeed",                        
  "Percent_Commuters","Percent_NoHealthIns","Physically_Unhealthy_Days","Violent_Crime_Rate",                            
  "Inadequate_Facilities","COVID-19_death_rate" ,"Percent_Food_Insecure",
  "Percent_Limited_Access_to_Healthy_Foods","Percent_Disconnected_Youth","Segregation_index",                             
  "Percent_rural",
  # Economy
  "Agro_Percent","UnstableEmployment_Percent","LowIncome_Percent",                               
  "PT","PT_LQ","Agri","Agri_LQ","Manufacturing","Manufacturing_LQ","Wholesale",
  "Wholesale_LQ","Arts","Arts_LQ","GINI_Index",
  "Percent_Children_in_Poverty","Gender_Pay_Gap",
  "Percent_income_required_for_childcare_expenses","CommuteTime",
  "Vice_related_business_rate","Recreation_related_business_rate",
  # Housing
  "Total",
  "OldHomes","Rented","Vacancy","Units_SingleFamily","MobileHomes","Overcrowding",
  "Rental_Overcrowding","Renters_Cost_IncomeRatio","Rental_CostBurden_below20000",
  "NoVehicle_Pop.x","GroupQuarters","Percent_HousingProblems","Percent_Section8",
  # Infrastructure
  "Percent_PoorCondition_Bridges","Percent_commuters_by_transit",
  "Other_Rural_workers","Workers_to_OtherCounties","Commuters_withinRural", 
  # Environment
  "Avg_temp","Max_temp","Min_temp","Precipitation","CO8",
  "Pb3","NO2AM","O3","PM10","PM2.5","PM2Point5","pct_manure_acres_ln","pct_disease_acres_ln",
  "pct_harvested_acres_ln","pct_au_ln","fungicide_ln",
  "herbicide_ln","insecticide_ln","std_coal_prim_pop_ln",
  "std_metal_prim_pop_ln","std_nonmetal_prim_pop_ln",
  "std_sandandgravel_prim_pop_ln","std_stone_prim_pop_ln","Radon.x",
  "Per_TotPopSS.x", "D303_Percent_ln", "ALLNPDESperKM_ln","CaAve_ln",
  "Kave_ln","NO3Ave.x","ClAve_ln" ,"SO4Ave_ln","HgAve.x",
  "AvgOfD3_ave.x","W_As_ln","W_Ba_ln","W_Cd_ln","W_Cr_ln",
  "W_CN_ln","W_FL_ln","W_HG_ln","W_NO3_ln","W_NO2_ln")

x$Population <- x$Population.x
x$Population.x <- NULL
x$Population.y <- NULL
x$Tract_FIPS <- NULL
x <- x %>% 
  relocate(GEOID, Name, County, Population)
x$State <- NULL

x[,4:ncol(x)] <- lapply(x[,4:ncol(x)],as.numeric)
x[is.na(x)] <- 0
x[x==-Inf] <- 0
x[x==Inf] <- 0
x[,which(colnames(x) %in% Negatively_weighed_columns)] <- x[,which(colnames(x) %in% Negatively_weighed_columns)]*-1

y <- x

for (i in 4:ncol(x)) {
  if(max(x[,i], na.rm = TRUE)-min(x[,i], na.rm = TRUE) == 0) {y[,i] = 0.5}
  else{y[,i] <-(x[,i]-min(x[,i], na.rm = TRUE))/(max(x[,i], na.rm = TRUE)-min(x[,i], na.rm=TRUE))}
}

write_csv(y, "Data/Normalized_and_Weighed/Weighed_Indicators_UrbanLevel/All_Indicators_Urban.csv")
x <- y

# Community Indicators
Index_Start <- which(colnames(x)=="Population")
Index_End <- which(colnames(x)=="Percent_rural")
y <- x[,c(1,2,3,Index_Start:Index_End)]
write_csv(y, "Data/Normalized_and_Weighed/Weighed_Indicators_UrbanLevel/Community_Indicators_Urban.csv")

# Economy Indicators
Index_Start <- which(colnames(x)=="Income")
Index_End <- which(colnames(x)=="Recreation_related_business_rate")
y <- x[,c(1,2,3,Index_Start:Index_End)]
write_csv(y, "Data/Normalized_and_Weighed/Weighed_Indicators_UrbanLevel/Economy_Indicators_Urban.csv")

# Housing Indicators
Index_Start <- which(colnames(x)=="Total")
Index_End <- which(colnames(x)=="Percent_Section8")
y <- x[,c(1,2,3,Index_Start:Index_End)]
write_csv(y, "Data/Normalized_and_Weighed/Weighed_Indicators_UrbanLevel/Housing_Indicators_Urban.csv")

# Infrastructure Indicators
Index_Start <- which(colnames(x)=="Pubtrans_Dependent")
Index_End <- which(colnames(x)=="Percent_BroadbandAccess")
y <- x[,c(1,2,3,Index_Start:Index_End)]
write_csv(y, "Data/Normalized_and_Weighed/Weighed_Indicators_UrbanLevel/Infrastructure_Indicators_Urban.csv")

# Environment
Index_Start <- which(colnames(x)=="Percent_BroadbandAccess")+1
Index_End <- ncol(x)
y <- x[,c(1,2,3,Index_Start:Index_End)]
write_csv(y, "Data/Normalized_and_Weighed/Weighed_Indicators_UrbanLevel/Environment_Indicators_Urban.csv")

