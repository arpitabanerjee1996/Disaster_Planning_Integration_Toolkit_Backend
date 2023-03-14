# Install all required packages if not already present
# Remove the "#" from the lines below beginning with "#install.packages" and press enter to execute each line
# If already installed in the past, please ignore

#install.packages("stringi")
#install.packages("stringr")
#install.packages("tidyverse")
#install.packages("tidycensus")

# Adding libraries to the code and cleaning environment
library(stringr)
library(tidyverse)
library(readr)
library(tidycensus)
rm(list=ls())

# specifying State
# In this case, we specify state as Illinois, ID as "IL"
# This can be replaced with any state that the developer wants to generate reports for
stateID <- "IL"

# specifying Geography
# For our Assessment Tool, ACS extractions are needed only at two geographic levels - county and tract
# In this case, we specify geography as Geo
# This can be replaced with "county" or "place" when required
Geo <- "tract"

# specifying Year
# In this case, we specify year as "YearID" because it corresponds to the latest version of ACS 5 year dataset
# This can be replaced with any year that the developer wants to generate reports for
# This is a useful step to specify during updates to the Assessment Tool
YearID <- 2020

# Once the State, Geography and Year are specified
# Select the entire code below the double # and press enter
# To generate all ACS related indicators for the Specified State, Geography and Year

#
#
# Start Selection Here

# Loading all ACS Variables
v20 <- load_variables(YearID, "acs5", cache = TRUE)
v20 <- v20 %>% filter(!(str_detect(label, "Rico")))

# Indicator Group 1 - Community
# Age - Under 18 and above 60 and their percentages
age_labs <- c("Estimate!!Total:",
              "Estimate!!Total:!!Male:!!Under 5 years",
              "Estimate!!Total:!!Male:!!5 to 9 years",
              "Estimate!!Total:!!Male:!!10 to 14 years",
              "Estimate!!Total:!!Male:!!15 to 17 years",
              "Estimate!!Total:!!Male:!!60 and 61 years",
              "Estimate!!Total:!!Male:!!62 to 64 years",
              "Estimate!!Total:!!Male:!!65 and 66 years",
              "Estimate!!Total:!!Male:!!67 to 69 years",
              "Estimate!!Total:!!Male:!!70 to 74 years",
              "Estimate!!Total:!!Male:!!75 to 79 years",
              "Estimate!!Total:!!Male:!!80 to 84 years",
              "Estimate!!Total:!!Male:!!85 years and over",
              "Estimate!!Total:!!Female:!!Under 5 years",
              "Estimate!!Total:!!Female:!!5 to 9 years",
              "Estimate!!Total:!!Female:!!10 to 14 years",
              "Estimate!!Total:!!Female:!!15 to 17 years",
              "Estimate!!Total:!!Female:!!60 and 61 years",
              "Estimate!!Total:!!Female:!!62 to 64 years",
              "Estimate!!Total:!!Female:!!65 and 66 years",
              "Estimate!!Total:!!Female:!!67 to 69 years",
              "Estimate!!Total:!!Female:!!70 to 74 years",
              "Estimate!!Total:!!Female:!!75 to 79 years",
              "Estimate!!Total:!!Female:!!80 to 84 years",
              "Estimate!!Total:!!Female:!!85 years and over")
age_vars <- v20$name[which(v20$label %in% age_labs &
                             v20$concept == "SEX BY AGE")]
age_geos <- get_acs(geography = Geo,
                      state = stateID,
                      year = YearID,
                      var= age_vars,
                      survey = "acs5",
                      output="wide")
colnames(age_geos)[2] <- "Name"

# Removing error margins and cleaning up generated dataset
age_geos[which(str_detect(colnames(age_geos),"M"))] <- NULL

# Changing column names for ease during subsequent operations
colnames(age_geos) <- c("GEOID", "Name",
                          "Total",
                          "Male_Under5",
                          "Male_5to9",
                          "Male_10to14",
                          "Male_15to17",
                          "Male_60to61",
                          "Male_62to64",
                          "Male_65to66",
                          "Male_67to69",
                          "Male_70to74",
                          "Male_75to79",
                          "Male_80to84",
                          "Male_85andAbove",
                          "Female_Under5",
                          "Female_5to9",
                          "Female_10to14",
                          "Female_15to17",
                          "Female_60to61",
                          "Female_62to64",
                          "Female_65to66",
                          "Female_67to69",
                          "Female_70to74",
                          "Female_75to79",
                          "Female_80to84",
                          "Female_85andAbove")
# Creating column for Youths
age_geos$Young <- age_geos$Male_Under5+
  age_geos$Male_5to9+
  age_geos$Male_10to14+
  age_geos$Male_15to17+age_geos$Female_Under5+
  age_geos$Female_5to9+
  age_geos$Female_10to14+
  age_geos$Female_15to17

# Creating column for Elders
age_geos$Old <-  age_geos$Male_60to61+
  age_geos$Male_62to64+
  age_geos$Male_65to66+
  age_geos$Male_67to69+
  age_geos$Male_70to74+
  age_geos$Male_75to79+
  age_geos$Male_80to84+
  age_geos$Male_85andAbove+
  age_geos$Female_60to61+
  age_geos$Female_62to64+
  age_geos$Female_65to66+
  age_geos$Female_67to69+
  age_geos$Female_70to74+
  age_geos$Female_75to79+
  age_geos$Female_80to84+
  age_geos$Female_85andAbove

# Calculating percentages of Youths and Elders
age_geos$Young_percent <- (age_geos$Young*100)/age_geos$Total
age_geos$Old_percent <- (age_geos$Old*100)/age_geos$Total

# All indicators erswhile generated will be stored in the variable "final_indicators"
final_indicators <- age_geos %>% select(GEOID,
                                          Name,
                                          Population=Total,
                                          Young_percent,
                                          Old_percent)
# Removing all extraneous variables after adding them to variable "final_indicators"
rm(age_geos, age_labs, age_vars)

#
#
# Race- Aggregate Percentage of minority populations
# Extracting total white population
race_lab <- "Estimate!!Total:!!White alone"
race_var <- v20$name[which(v20$label == race_lab)]
race_geos <- get_acs(geography = Geo,
                       state = stateID,
                       year = YearID,
                       var= race_var,
                       survey = "acs5",
                       output="wide")
colnames(race_geos)[3] <- "Whites"
race_geos <- race_geos %>% select(Whites)

# Adding white population to final indicators
final_indicators <- bind_cols(final_indicators, race_geos)

# Removing all extraneous variables
rm(race_lab, race_var, race_geos)

# Calculating NonWhite Population Percentage from total population and White population
final_indicators$NonWhite_Percent <- 
  (final_indicators$Population-final_indicators$Whites)*100/final_indicators$Population

# Removing White population from final indicators
final_indicators$Whites <- NULL  

#
#
# Sex- Percentage of females
# Arrangement of script is very similar to Race
# The upcoming sections also contain very similar arrangements
sex_lab <- "Estimate!!Total:!!Female:"
sex_var <- v20$name[which(v20$label == sex_lab &
                            v20$concept=="SEX BY AGE")]
sex_geos <- get_acs(geography = Geo,
                      state = stateID,
                      year = YearID,
                      var= sex_var,
                      survey = "acs5",
                      output="wide")
colnames(sex_geos)[3] <- "Females"
sex_geos <- sex_geos %>% select(Females)
final_indicators <- bind_cols(final_indicators, sex_geos)
rm(sex_lab, sex_var, sex_geos)
final_indicators$Females_Percent <- 
  (final_indicators$Females*100)/final_indicators$Population
final_indicators$Females <- NULL  

#
#
# Marital Status - Percentage of Unmarried Individuals
mar_lab <- "Estimate!!Total:!!Now married, except separated"
mar_var <- v20$name[which(v20$label==mar_lab)][3]
mar_geos <- get_acs(geography = Geo,
                      state = stateID,
                      year = YearID,
                      var= mar_var,
                      survey = "acs5",
                      output="wide")
colnames(mar_geos)[3] <- "Married"
mar_geos <- mar_geos %>% select(Married)
final_indicators <- bind_cols(final_indicators, mar_geos)
rm(mar_lab, mar_var, mar_geos)
final_indicators$NotMarried_Percent <- 
  ((final_indicators$Population-final_indicators$Married)*100)/final_indicators$Population
final_indicators$Married <- NULL

#
#
# Health - Percentage of Disabled Individuals
health_lab <- "Estimate!!Total:!!With any disability:"
health_var <- v20$name[which(v20$label==health_lab)]
health_geos <- get_acs(geography = Geo,
                         state = stateID,
                         year = YearID,
                         var= health_var,
                         survey = "acs5",
                         output="wide")
colnames(health_geos)[3] <- "Disabled"
health_geos <- health_geos %>% select(Disabled)
final_indicators <- bind_cols(final_indicators, health_geos)
rm(health_lab, health_var, health_geos)
final_indicators$Disabled_Percent <- 
  ((final_indicators$Disabled)*100)/final_indicators$Population
final_indicators$Disabled <- NULL

#
#
# Female headed-households - As a percentage of total population
# Although this might seem like an unusual calculation, since it is weighed across every tract/county, it will 
# yeild homogeneous measures throughout the state
fhh_lab <- "Estimate!!Total:!!Family households:!!Other family:!!Female householder, no spouse present"
fhh_concept <- "HOUSEHOLD TYPE (INCLUDING LIVING ALONE)"
fhh_var <- v20$name[which(v20$label==fhh_lab &
                            v20$concept==fhh_concept)]
fhh_counties <- get_acs(geography = Geo,
                        state = stateID,
                        year = YearID,
                        var= fhh_var,
                        survey = "acs5",
                        output="wide")
colnames(fhh_counties)[3] <- "FemaleHeaded_HHs"
fhh_counties <- fhh_counties %>% select(FemaleHeaded_HHs)
final_indicators <- bind_cols(final_indicators, fhh_counties)
rm(fhh_lab, fhh_var, fhh_counties, fhh_concept)
final_indicators$FemaleHeaded_HHs_Percent <- 
  ((final_indicators$FemaleHeaded_HHs)*100)/final_indicators$Population
final_indicators$FemaleHeaded_HHs <- NULL

#
#
# Compiling remaining indicators from the Indicator Group "Community"
#
# 

# Names of all required variables from acs data
Community_labs <- c("Estimate!!Total:!!4-or-more-person household:",
                    "Estimate!!Total:!!Under 6 years:!!Living with one parent:",
                    "Estimate!!Total:!!6 to 17 years:!!Living with one parent:",
                    "Estimate!!Total:!!Less than high school graduate",
                    "Estimate!!Total:!!Graduate or professional degree",
                    "Estimate!!Total:!!Male:!!60 and 61 years",
                    "Estimate!!Total:!!Male:!!62 to 64 years",
                    "Estimate!!Total:!!Male:!!65 and 66 years",
                    "Estimate!!Total:!!Male:!!67 to 69 years",
                    "Estimate!!Total:!!Male:!!70 to 74 years",
                    "Estimate!!Total:!!Male:!!75 to 79 years",
                    "Estimate!!Total:!!Male:!!80 to 84 years",
                    "Estimate!!Total:!!Male:!!85 years and over",
                    "Estimate!!Total:!!Female:!!60 and 61 years",
                    "Estimate!!Total:!!Female:!!62 to 64 years",
                    "Estimate!!Total:!!Female:!!65 and 66 years",
                    "Estimate!!Total:!!Female:!!67 to 69 years",
                    "Estimate!!Total:!!Female:!!70 to 74 years",
                    "Estimate!!Total:!!Female:!!75 to 79 years",
                    "Estimate!!Total:!!Female:!!80 to 84 years",
                    "Estimate!!Total:!!Female:!!85 years and over",
                    "Estimate!!Total:!!Unpaid family workers",
                    "Estimate!!Total:!!Worked full-time, year-round:",
                    "Estimate!!Total:!!Unpaid family workers",
                    "Estimate!!Total:!!Female:!!Worked in the past 12 months:",
                    "Estimate!!Total:!!Female:!!16 to 64 years:!!In labor force:",
                    "Estimate!!Total:!!Male:!!16 to 64 years:",
                    "Estimate!!Total:!!Male:!!16 to 64 years:",
                    "Estimate!!Average household size --!!Total:",
                    "Estimate!!Total:!!High school graduate (includes equivalency)",
                    "Estimate!!Total:!!Bachelor's degree or higher:",
                    'Estimate!!Total:!!Speak other languages:!!Speak English less than "very well"',
                    "Estimate!!Total:!!Emergency medicine physicians",
                    "Estimate!!Total:!!Family households:",
                    "Estimate!!Total:!!Household did not receive Food Stamps/SNAP in the past 12 months",
                    "Estimate!!Total:!!Public transportation (excluding taxicab):!!No vehicle available",
                    "Estimate!!Total:!!Personal care aides",
                    "Estimate!!Total:!!Under 19 years:!!No health insurance coverage",
                    "Estimate!!Total:!!19 to 64 years:!!No health insurance coverage",
                    "Estimate!!Total:!!65 years and over:!!No health insurance coverage")
Community_var <- v20 %>% filter(label %in% Community_labs)
Community_var <- Community_var[!duplicated(Community_var$label),]
Community_var_labels <- c("Male_60to61","Male_62to64","Male_65to66","Male_67to69",
                          "Male_70to74","Male_75to79","Male_80to84","Male_85andOver",
                          "Female_60to61","Female_62to64","Female_65to66","Female_67to69",
                          "Female_70to74","Female_75to79","Female_80to84","Female_85andOver",
                          "Oneparent_Under6","Oneparent_6to17","LanguageBarrier_English","Edu_LessThanHS",
                          "Edu_HS","Edu_Graduate","Unpaid_FamWorkers","Commuters","LargeHH",
                          "Tot_FamHH","Edu_Bachelor","HH_notFoodstamp","Female_Worker",
                          "Emergency_Physicians","Personal_aides","HH_size","No_HealthIns_under19",
                          "No_HealthIns_over65","Male_16to64","Female_16to64_Labour","No_HealthIns_19to64","Total_Labour")
Community_var_labels <- as.data.frame(Community_var_labels)
Community_var <- bind_cols(Community_var, Community_var_labels)
rm(Community_var_labels, Community_labs)

library(readr)
Community_var_def <- Community_var
Community_var <- Community_var_def$name
Community_geos <- get_acs(geography = Geo,
                            state = stateID,
                            year = YearID,
                            var= Community_var,
                            survey = "acs5",
                            output="wide")
Community_geos[which(str_detect(colnames(Community_geos),"M"))] <- NULL
x <- Community_var_def$Community_var_labels
x <- c("GEOID",x)
colnames(Community_geos) <-x 

# Some columns do not have values
Community_geos$Emergency_Physicians <- NULL
Community_geos$Personal_aides <- NULL
#
#
# Large families
Community_geos$LargeFam <- (Community_geos$LargeHH*100)/Community_geos$Tot_FamHH
final_indicators$LargeFam_Percent <- Community_geos$LargeFam
#
#
# Living with one parent
final_indicators$SingleParent_Percent <- ((Community_geos$Oneparent_Under6+
Community_geos$Oneparent_6to17)*100)/final_indicators$Population
#
#
# Education
final_indicators$Educated_percent <- 
  ((final_indicators$Population-Community_geos$Edu_LessThanHS)*100)/final_indicators$Population
#
#
# Skills
final_indicators$Skilled_Percent <-
  ((Community_geos$Edu_Graduate)*100)/
  final_indicators$Population
#
#
# Growth rate of Elderly Population
var <- Community_var_def$name[1:16]

x1 <- get_acs(geography = Geo,
              state = stateID,
              year = 2019,
              var= var,
              survey = "acs5",
              output="wide")
x1[which(str_detect(colnames(x1),"M"))] <- NULL
x1<- pivot_longer(x1, B01001_018E:B01001_049E,
                  names_to = "Var", values_to = "Value")
x1 <- x1 %>% group_by(GEOID) %>% 
  summarise(Value = sum(Value))
x2 <- get_acs(geography = Geo,state = stateID,year = YearID,
              var= var,survey = "acs5",output="wide")
x2[which(str_detect(colnames(x2),"M"))] <- NULL
x2<- pivot_longer(x2, B01001_018E:B01001_049E,
                  names_to = "Var", values_to = "Value")
x2 <- x2 %>% group_by(GEOID) %>% 
  summarise(Value = sum(Value))
final_indicators$Elderly_growth_percent <- ((x2$Value-x1$Value)*100)/x1$Value
#
#
# Ratio of working adults to dependents
final_indicators$Working_Nonworking_ratio <- 
  (Community_geos$Unpaid_FamWorkers)/
  Community_geos$Total_Labour
#
#
# Persons to devote time in need of disasters
final_indicators$HelperPool <- Community_geos$Unpaid_FamWorkers
#
#
# Female workers
final_indicators$FemaleLaborforce_percent <- 
  (Community_geos$Female_Worker*100)/Community_geos$Total_Labour
#
#
# Persons of Prime Working Age
final_indicators$PrimeWorkingAge_percent <- 
  ((Community_geos$Male_16to64+
      Community_geos$Female_16to64_Labour)*100)/final_indicators$Population
#
#
# Average HH Size
final_indicators$Avg_HHsize <- 
  Community_geos$HH_size
#
#
# Bachelor's or higher
final_indicators$Percent_BachelorsDegree <- 
  (Community_geos$Edu_Bachelor*100)/final_indicators$Population
#
#
# speaks english less than well
final_indicators$Percent_LanguageBarrier <- 
  (Community_geos$LanguageBarrier_English*100)/final_indicators$Population
#
#
# Families with Assistance Needs
final_indicators$Percent_AssistanceNeed <- 
  ((Community_geos$Tot_FamHH-Community_geos$HH_notFoodstamp)*100)/Community_geos$Tot_FamHH
#
#
# Commuters
final_indicators$Percent_Commuters <- 
  ((Community_geos$Commuters)*100)/final_indicators$Population
#
#
# Availability of Healthcare Support
final_indicators$Percent_NoHealthIns <-
  ((Community_geos$No_HealthIns_19to64+
      Community_geos$No_HealthIns_under19+
      Community_geos$No_HealthIns_over65)*100)/
  final_indicators$Population
community_indicators <- final_indicators

# Removing all other variables
rm(Community_labs, Community_geos, Community_var,Community_var_def, x1, x2, var, x)

#
#
# Indicator Group 2 - Economic Development
# Follows the same process as Community Indicators
econ_labs <- c("Estimate!!Median household income in the past 12 months (in 2020 inflation-adjusted dollars)",
               "Estimate!!Total:!!Agriculture, forestry, fishing and hunting, and mining",
               "Estimate!!Total:!!Self-employed in own not incorporated business workers",
               "Estimate!!Total:!!Self-employed in own incorporated business workers:",
               "Estimate!!Total:!!Private for-profit wage and salary workers:",
               "Estimate!!Total:!!Private not-for-profit wage and salary workers",
               "Estimate!!Total:!!With income:!!$1 to $9,999 or loss",
               "Estimate!!Total:!!With income:!!$10,000 to $14,999",
               "Estimate!!Total:!!With income:!!$15,000 to $24,999",
               "Estimate!!Total:!!Below 100 percent of the poverty level",
               "Estimate!!Total:!!100 to 149 percent of the poverty level",
               "Estimate!!Total:!!Never married:!!Male:!!In labor force:!!Unemployed",
               "Estimate!!Total:!!Never married:!!Female:!!In labor force:!!Unemployed",
               "Estimate!!Total:!!Female:!!Worked in state of residence:",
               "Estimate!!Total:!!Male:!!Worked in state of residence:",
               "Estimate!!Total:!!Management, business, science, and arts occupations",
               "Estimate!!Total:!!Service occupations",
               "Estimate!!Total:!!Sales and office occupations",
               "Estimate!!Total:!!Natural resources, construction, and maintenance occupations",
               "Estimate!!Total:!!Production, transportation, and material moving occupations",
               "Estimate!!Total:!!Agriculture, forestry, fishing and hunting, and mining",
               "Estimate!!Total:!!Manufacturing",
               "Estimate!!Total:!!Wholesale trade",
               "Estimate!!Total:!!Retail trade",
               "Estimate!!Total:!!Transportation and warehousing, and utilities",
               "Estimate!!Total:!!Information",
               "Estimate!!Total:!!Finance and insurance, and real estate and rental and leasing",
               "Estimate!!Total:!!Professional, scientific, and management, and administrative, and waste management services:",
               "Estimate!!Total:!!Educational services, and health care and social assistance",
               "Estimate!!Total:!!Arts, entertainment, and recreation, and accommodation and food services",
               "Estimate!!Total:!!Other services, except public administration",
               "Estimate!!Total:!!Public administration",
               "Estimate!!Gini Index")
econ_varlabs <- c("Income",
                  "Employed_Total",
                  "Income_1_9999",
                  "Income_10000_14999",
                  "Income_15000_24999",
                  "Poverty_Below100",
                  "Poverty_100to149",
                  "Male_WorkedNearby",
                  "Female_WorkedNearby",
                  "Occ_MBS",
                  "Occ_Service",
                  "Occ_Sales",
                  "Occ_Construction",
                  "Occ_PT",
                  "Occ_Agri",
                  "Occ_Manufacturing",
                  "Occ_Wholesale",
                  "Occ_Retail",
                  "Occ_Transport",
                  "Occ_Info",
                  "Occ_Finance",
                  "Occ_Edu",
                  "Occ_Arts",
                  "Occ_PublicAdmin",
                  "Occ_PrivateforProfit",
                  "Occ_PrivateNonProfit",
                  "Occ_Selfemployed_NotInc",
                  "Unemployed",
                  "GiniIndex",
                  "Occ_Management",
                  "Occ_Others",
                  "Occ_Selfemployed_Inc",
                  "GiniIndex")
econ_var <- v20 %>% filter(label %in% econ_labs)
econ_var <- econ_var[!duplicated(econ_var$label), ]
econ_labs <- econ_labs[!duplicated(econ_labs)]
econ_varlabs <- econ_varlabs[!duplicated(econ_varlabs)]

econ_var <- econ_var$name
econ_geos <- get_acs(geography = Geo,
                       state = stateID,
                       year = YearID,
                       var= econ_var,
                       survey = "acs5",
                       output="wide")

econ_geos[which(str_detect(colnames(econ_geos),"M"))] <- NULL
econ_varlabs <- c("GEOID", econ_varlabs)
colnames(econ_geos) <- econ_varlabs
econ_geos$Occ_Others[which(is.na(econ_geos$Occ_Others))] <- 0
econ_geos$Occ_Management[which(is.na(econ_geos$Occ_Management))] <- 0
#rm(econ_var, econ_varlabs)
#
#
#income
final_indicators$MHHI <- econ_geos$Income
econ_indicators <- econ_geos[,1:2]
#
#
#livelihood ties to tourism, fishing, etc. 
final_indicators$Agro_Percent <-(econ_geos$Occ_Agri*100)/econ_geos$
  Employed_Total
econ_indicators$Agro_Percent <- final_indicators$Agro_Percent
#
#
#unstable employment - self employed/private sector
final_indicators$UnstableEmployment_Percent <- 
  ((econ_geos$Occ_PrivateforProfit+econ_geos$Occ_PrivateNonProfit+
      econ_geos$Occ_Selfemployed_NotInc+econ_geos$Occ_Selfemployed_Inc)*100)/
  econ_geos$Employed_Total
econ_indicators$UnstableEmployment_percent<- final_indicators$UnstableEmployment_Percent
#
#
#low-income persons
final_indicators$LowIncome_Percent <- 
  ((econ_geos$Income_1_9999+econ_geos$Income_10000_14999+econ_geos$Income_15000_24999)*100)/
  final_indicators$Population
econ_indicators$LowIncome_Percent <- final_indicators$LowIncome_Percent
#
#
#poverty
final_indicators$poverty <- 
  ((econ_geos$Poverty_Below100+econ_geos$Poverty_100to149)*100)/
  final_indicators$Population
econ_indicators$poverty <- final_indicators$poverty
#
#
#unemployment
final_indicators$Unemployment_percent <- 
  econ_geos$Unemployed/econ_geos$Employed_Total
econ_indicators$Unemployment_percent <- final_indicators$Unemployment_percent 
#
#
#percent of residents working within the community
final_indicators$WorkNearby_percent <-
  (econ_geos$Male_WorkedNearby+econ_geos$Female_WorkedNearby)*100/econ_geos$Employed_Total
econ_indicators$WorkNearby_percent <- final_indicators$WorkNearby_percent
#
#
#Individual LQs as well as percentage employment
econ_states <- get_acs(geography = "state",
                       state = stateID,
                       year = YearID,
                       var= econ_var,
                       survey = "acs5",
                       output="wide")
econ_states[which(str_detect(colnames(econ_states),"M"))] <- NULL
colnames(econ_states) <- econ_varlabs
econ_states$Occ_Others[which(is.na(econ_states$Occ_Others))] <- 0
econ_states$Occ_Management[which(is.na(econ_states$Occ_Management))] <- 0
#
#
#Management, business, science, and arts occupations
final_indicators$MBS <- econ_geos$Occ_MBS
final_indicators$MBS_LQ<- (econ_geos$Occ_MBS/econ_geos$Employed_Total)/
  (econ_states$Occ_MBS/econ_states$Employed_Total)
econ_indicators$MBS <- final_indicators$MBS
econ_indicators$MBS_LQ<- final_indicators$MBS_LQ
#
#
#Service occupations
final_indicators$Service <- econ_geos$Occ_Service
final_indicators$Service_LQ<- (econ_geos$Occ_Service/econ_geos$Employed_Total)/
  (econ_states$Occ_Service/econ_states$Employed_Total)
econ_indicators$Service <- final_indicators$Service
econ_indicators$Service_LQ<- final_indicators$Service_LQ
#
#
#Sales and office occupations
final_indicators$Sales <- econ_geos$Occ_Sales
final_indicators$Sales_LQ<- (econ_geos$Occ_Sales/econ_geos$Employed_Total)/
  (econ_states$Occ_Sales/econ_states$Employed_Total)
econ_indicators$Sales <- final_indicators$Sales
econ_indicators$Sales_LQ<- final_indicators$Sales_LQ
#
#
#Natural resources, construction, and maintenance occupations
final_indicators$construction <- econ_geos$Occ_Construction
final_indicators$construction_LQ<- (econ_geos$Occ_Construction/econ_geos$Employed_Total)/
  (econ_states$Occ_Construction/econ_states$Employed_Total)
econ_indicators$construction <- final_indicators$construction
econ_indicators$construction_LQ<- final_indicators$construction_LQ
#
#
#Production, transportation, and material moving occupations
final_indicators$PT <- econ_geos$Occ_PT
final_indicators$PT_LQ<- (econ_geos$Occ_PT/econ_geos$Employed_Total)/
  (econ_states$Occ_PT/econ_states$Employed_Total)
econ_indicators$PT <- final_indicators$PT
econ_indicators$PT_LQ<- final_indicators$PT_LQ
#
#
#Agriculture, forestry, fishing and hunting, mining
final_indicators$Agri <- econ_geos$Occ_Agri
final_indicators$Agri_LQ<- (econ_geos$Occ_Agri/econ_geos$Employed_Total)/
  (econ_states$Occ_Agri/econ_states$Employed_Total)
econ_indicators$Agri <- final_indicators$Agri
econ_indicators$Agri_LQ<- final_indicators$Agri_LQ
#
#
#Manufacturing
final_indicators$Manufacturing <- econ_geos$Occ_Manufacturing
final_indicators$Manufacturing_LQ<- (econ_geos$Occ_Manufacturing/econ_geos$Employed_Total)/
  (econ_states$Occ_Manufacturing/econ_states$Employed_Total)
econ_indicators$Manufacturing <- final_indicators$Manufacturing
econ_indicators$Manufacturing_LQ<- final_indicators$Manufacturing_LQ
#
#
#Wholesale trade
final_indicators$Wholesale <- econ_geos$Occ_Wholesale
final_indicators$Wholesale_LQ<- (econ_geos$Occ_Wholesale/econ_geos$Employed_Total)/
  (econ_states$Occ_Wholesale/econ_states$Employed_Total)
econ_indicators$Wholesale <- final_indicators$Wholesale
econ_indicators$Wholesale_LQ<- final_indicators$Wholesale_LQ
#
#
#Retail trade
final_indicators$Retail <- econ_geos$Occ_Retail
final_indicators$Retail_LQ<- (econ_geos$Occ_Retail/econ_geos$Employed_Total)/
  (econ_states$Occ_Retail/econ_states$Employed_Total)
econ_indicators$Retail <- final_indicators$Retail
econ_indicators$Retail_LQ<- final_indicators$Retail_LQ
#
#
#Transportation and warehousing, and utilities
final_indicators$Transport <- econ_geos$Occ_Transport
final_indicators$Transport_LQ<- (econ_geos$Occ_Transport/econ_geos$Employed_Total)/
  (econ_states$Occ_Transport/econ_states$Employed_Total)
econ_indicators$Transport <- final_indicators$Transport
econ_indicators$Transport_LQ<- final_indicators$Transport_LQ
#
#
#Information
final_indicators$Info <- econ_geos$Occ_Info
final_indicators$Info_LQ<- (econ_geos$Occ_Info/econ_geos$Employed_Total)/
  (econ_states$Occ_Info/econ_states$Employed_Total)
econ_indicators$Info <- final_indicators$Info
econ_indicators$Info_LQ<- final_indicators$Info_LQ
#
#
#Finance and insurance, and real estate and rental and leasing
final_indicators$Finance <- econ_geos$Occ_Finance
final_indicators$Finance_LQ<- (econ_geos$Occ_Finance/econ_geos$Employed_Total)/
  (econ_states$Occ_Finance/econ_states$Employed_Total)
econ_indicators$Finance <- final_indicators$Finance
econ_indicators$Finance_LQ<- final_indicators$Finance_LQ
#
#
#Professional, scientific, and management, and administrative and waste management services
final_indicators$Management <- econ_geos$Occ_Management
final_indicators$Management_LQ<- (econ_geos$Occ_Management/econ_geos$Employed_Total)/
  (econ_states$Occ_Management/econ_states$Employed_Total)
econ_indicators$Management <- final_indicators$Management
econ_indicators$Management_LQ<- final_indicators$Management_LQ
#
#
#Educational services, and health care and social assist
final_indicators$Edu <- econ_geos$Occ_Edu
final_indicators$Edu_LQ<- (econ_geos$Occ_Edu/econ_geos$Employed_Total)/
  (econ_states$Occ_Edu/econ_states$Employed_Total)
econ_indicators$Edu <- final_indicators$Edu
econ_indicators$Edu_LQ<- final_indicators$Edu_LQ
#
#
#Arts, entertainment, and recreation, and accommodation
final_indicators$Arts <- econ_geos$Occ_Arts
final_indicators$Arts_LQ<- (econ_geos$Occ_Arts/econ_geos$Employed_Total)/
  (econ_states$Occ_Arts/econ_states$Employed_Total)
econ_indicators$Arts <- final_indicators$Arts
econ_indicators$Arts_LQ<- final_indicators$Arts_LQ
#
#
#Other services, except public administration
final_indicators$Others <- econ_geos$Occ_Others
final_indicators$Others_LQ<- (econ_geos$Occ_Others/econ_geos$Employed_Total)/
  (econ_states$Occ_Others/econ_states$Employed_Total)
econ_indicators$Others <- final_indicators$Others
econ_indicators$Others_LQ<- final_indicators$Others_LQ
#
#
#Public administration
final_indicators$Others <- econ_geos$Occ_Others
final_indicators$Others_LQ<- (econ_geos$Occ_Others/econ_geos$Employed_Total)/
  (econ_states$Occ_Others/econ_states$Employed_Total)
econ_indicators$Others <- final_indicators$Others
econ_indicators$Others_LQ<- final_indicators$Others_LQ
#
#
# Gini index
final_indicators$GINI_Index <- econ_geos$GiniIndex

rm(econ_states,
   econ_geos,
   econ_var,
   econ_labs,
   econ_varlabs)

#
#
# Indicator Group 3 - Housing
housing_var <-c("B08014_002","B09001_010","B11011_004","B11011_006","B11011_009","B11011_011","B11011_013","B11011_015","B11011_017",
                 "B11011_019","B19013_001","B25002_003","B25012_002","B25012_010","B25014A_002","B25014A_003","B25015_017","B25015_018",
                 "B25015_019","B25015_021","B25015_022","B25015_023","B25015_025","B25015_026","B25015_027","B25034_002","B25034_003",
                 "B25034_004","B25034_005","B25034_006","B25034_007","B25034_008","B25034_009","B25034_010","B25034_011","B25077_001",
                 "B25105_001","B25112_001","B25119_003","B25122_006","B25122_007","B25122_008","B25122_009","B25122_010","B25122_011",
                 "B25122_012","B25122_013","B25122_014","B25122_015","B25122_016","B25122_017","B25122_045","B25122_046","B25122_047",
                 "B25122_048","B25122_049","B25122_050","B25122_051")
housing_labs <- c("Pop_NoVehicle",
                  "Pop_GroupQuarters",
                  "HH_SingleFamily",
                  "HH_MobileHomes",
                  "HH_MaleHouseholder_SingleFamily",
                  "HH_MaleHouseholder_MobileHomes",
                  "HH_FemaleHouseholder_SingleFamily",
                  "HH_FemaleHouseholder_MobileHomes",
                  "HH_Nonfamily_SingleUnit",
                  "HH_Nonfamily_MobileHomes",
                  "MHHI",
                  "Units_Vacant",
                  "Units_Owned",
                  "Units_Rented",
                  "Units_1orlessOccupant",
                  "Units_Morethan1Occupant",
                  "Units_Rented_15to34_1orlessOccupant",
                  "Units_Rented_15to34_1andHalfOccupants",
                  "Units_Rented_15to34_Morethan_1andHalfOccupants",
                  "Units_Rented_35to64_1orlessOccupant",
                  "Units_Rented_35to64_1andHalfOccupants",
                  "Units_Rented_35to64_Morethan_1andHalfOccupants",
                  "Units_Rented_65ormore_1orlessOccupant",
                  "Units_Rented_65ormore_1andHalfOccupants",
                  "Units_Rented_65ormore_1andHalfOccupants",
                  "Units_Builtafter2013",
                  "Units_2010to2013",
                  "Units_2009to2009",
                  "Units_1990to1999",
                  "Units_1980to1989",
                  "Units_1970to1979",
                  "Units_1960to1969",
                  "Units_1950to1959",
                  "Units_1940to1949",
                  "Units_1939orearlier",
                  "MedianHomeValue",
                  "Median_MonthlyHousingCosts",
                  "Gross_rent",
                  "MHHI_renter",
                  "MHHI_lessthan10000_Rent_200to299",
                  "MHHI_lessthan10000_Rent_300to399",
                  "MHHI_lessthan10000_Rent_400to499",
                  "MHHI_lessthan10000_Rent_500to599",
                  "MHHI_lessthan10000_Rent_600to699",
                  "MHHI_lessthan10000_Rent_700to799",
                  "MHHI_lessthan10000_Rent_800to899",
                  "MHHI_lessthan10000_Rent_900to999",
                  "MHHI_lessthan10000_Rent_1000to1249",
                  "MHHI_lessthan10000_Rent_1250to1499",
                  "MHHI_lessthan10000_Rent_1500to1999",
                  "MHHI_lessthan10000_Rent_2000ormore",
                  "MHHI_20000to34999_Rent_700to799",
                  "MHHI_20000to34999_Rent_800to899",
                  "MHHI_20000to34999_Rent_900to999",
                  "MHHI_20000to34999_Rent_1000to1249",
                  "MHHI_20000to34999_Rent_1250to1499",
                  "MHHI_20000to34999_Rent_1500to1999",
                  "MHHI_20000to34999_Rent_2000ormore")
housing_geos <- get_acs(geography = Geo,
                          state = stateID,
                          year = YearID,
                          var= housing_var,
                          survey = "acs5",
                          output="wide")
housing_geos[which(str_detect(colnames(housing_geos),"M"))] <- NULL
housing_labs <- c("GEOID", housing_labs)
colnames(housing_geos) <- housing_labs
housing_geos$Gross_rent <- NULL
#
#
#home-ownership
housing_indicators <- housing_geos[,1]
housing_indicators$Total <- housing_geos$Units_Owned+
  housing_geos$Units_Rented+housing_geos$Units_Vacant
housing_indicators$Homewownership <- 
  (housing_geos$Units_Owned*100)/housing_indicators$Total
#
#
#Probability for occupancy in areas of structural damage during disasters
housing_indicators$OldHomes <- ((housing_geos$Units_1939orearlier+
                                   housing_geos$Units_1940to1949+
                                   housing_geos$Units_1950to1959+
                                   housing_geos$Units_1960to1969+
                                   housing_geos$Units_1970to1979+
                                   housing_geos$Units_1980to1989)*100)/housing_indicators$Total

housing_indicators$NewHomes <- ((housing_geos$Units_1990to1999+
                                   housing_geos$Units_2009to2009+
                                   housing_geos$Units_2010to2013+
                                   housing_geos$Units_Builtafter2013)*100)/housing_indicators$Total
#
#
#Renters
housing_indicators$Rented <- 
  (housing_geos$Units_Rented*100)/
  housing_indicators$Total
#
#
#Vacancy rate
housing_indicators$Vacancy <- 
  (housing_geos$Units_Vacant*100)/housing_indicators$Total
#
#
#Number of units in single family homes
housing_indicators$Units_SingleFamily <- 
  ((housing_geos$HH_FemaleHouseholder_SingleFamily+
      housing_geos$HH_MaleHouseholder_SingleFamily+
      housing_geos$HH_Nonfamily_SingleUnit+
      housing_geos$HH_SingleFamily)*100)/housing_indicators$Total
#
#
#Percent of units in mobile homes
housing_indicators$MobileHomes <- ((housing_geos$HH_MobileHomes+
                                      housing_geos$HH_FemaleHouseholder_MobileHomes+
                                      housing_geos$HH_MaleHouseholder_MobileHomes+
                                      housing_geos$HH_Nonfamily_MobileHomes)*100)/housing_indicators$Total
#
#
#Percent of all households in overcrowded housing
housing_indicators$Overcrowding <- ((housing_geos$Units_Morethan1Occupant)*100)/
  housing_indicators$Total
#
#
#Percent of renters in overcrowded housing
housing_indicators$Rental_Overcrowding <- ((housing_geos$Units_Rented_15to34_1andHalfOccupants+
                                              housing_geos$Units_Rented_15to34_Morethan_1andHalfOccupants+
                                              housing_geos$Units_Rented_35to64_1andHalfOccupants+
                                              housing_geos$Units_Rented_35to64_Morethan_1andHalfOccupants+
                                              housing_geos$Units_Rented_65ormore_1andHalfOccupants)*100)/housing_indicators$Total
#
#
#Cost to Income Ratio
housing_indicators$Cost_IncomeRatio <- ((housing_geos$Median_MonthlyHousingCosts*12)/housing_geos$MHHI*100)/
  housing_indicators$Total
#
#
#Cost to Income Ratio for renters
housing_indicators$Renters_Cost_IncomeRatio <- ((housing_geos$Median_MonthlyHousingCosts*12)/housing_geos$MHHI_renter*100)/
  housing_indicators$Total
#
#
#Percent of households with less than $20,000 annual income who are housing cost burdened
housing_indicators$Rental_CostBurden_below20000 <- ((housing_geos$MHHI_lessthan10000_Rent_200to299+
                                                       housing_geos$MHHI_lessthan10000_Rent_300to399+
                                                       housing_geos$MHHI_lessthan10000_Rent_400to499+
                                                       housing_geos$MHHI_lessthan10000_Rent_500to599+
                                                       housing_geos$MHHI_lessthan10000_Rent_600to699+
                                                       housing_geos$MHHI_lessthan10000_Rent_700to799+
                                                       housing_geos$MHHI_lessthan10000_Rent_800to899+
                                                       housing_geos$MHHI_lessthan10000_Rent_900to999+
                                                       housing_geos$MHHI_lessthan10000_Rent_1000to1249+
                                                       housing_geos$MHHI_lessthan10000_Rent_1250to1499+
                                                       housing_geos$MHHI_lessthan10000_Rent_1500to1999+
                                                       housing_geos$MHHI_lessthan10000_Rent_2000ormore+
                                                       housing_geos$MHHI_20000to34999_Rent_700to799+
                                                       housing_geos$MHHI_20000to34999_Rent_800to899+
                                                       housing_geos$MHHI_20000to34999_Rent_900to999+
                                                       housing_geos$MHHI_20000to34999_Rent_1000to1249+
                                                       housing_geos$MHHI_20000to34999_Rent_1250to1499+
                                                       housing_geos$MHHI_20000to34999_Rent_1500to1999+
                                                       housing_geos$MHHI_20000to34999_Rent_2000ormore)*100)/housing_indicators$Total
#
#
#Median renter household income
housing_indicators$Renter_MHHI <- housing_geos$MHHI_renter
#
#
#Median home value
housing_indicators$MedianHomeValue <- housing_geos$MedianHomeValue
#
#
#No Vehicle
housing_indicators$NoVehicle_Pop <- housing_geos$Pop_NoVehicle
#
#
#Housing vulnerability includes - multi-unit structures, mobile homes, crowding, no vehicle, group quarters
housing_indicators$GroupQuarters <-housing_geos$Pop_GroupQuarters

x <- housing_indicators[, 2:18]
final_indicators <- bind_cols(final_indicators,x)
rm(housing_geos,
   housing_var,
   housing_vars,
   housing_labs)

#
#
# Indicator Group 4 - Infrastructure
infra_vars <-c("B08006_008","B08134_091","B08141_017","B24011_021","B24114_232","B24114_270","B24134_209","B24134_212","B24134_221",
               "B25043_003","B25043_012","B28002_002")
infra_geos <- get_acs(geography = Geo,
                        state = stateID,
                        year = YearID,
                        var= infra_vars,
                        survey = "acs5",
                        output="wide")
infra_geos[which(str_detect(colnames(infra_geos),"M"))] <- NULL
infra_labs <-  c("GEOID", "Pop_pubtransport",
                 "Pop_poptrans_LongCommute",
                 "Pop_pubtrans_NoVehicle",
                 "ProtectiveServices",
                 "Emergency_Medical",
                 "Police",
                 "Schools_Emp",
                 "OtherSchools_Emp",
                 "Hospitals_Emp",
                 "WithTelephone_Owner",
                 "WithTelephone_Renter",
                 "WithInternet")
colnames(infra_geos) <- infra_labs
infra_geos$Emergency_Medical <- NULL
infra_geos$Police <- NULL
infra_geos$Schools_Emp <- NULL
infra_geos$OtherSchools_Emp <- NULL
infra_geos$Hospitals_Emp <- NULL
infra_geos$ProtectiveServices[which(is.na(infra_geos$ProtectiveServices))] <- 0
#
#
#Lack of access to public or private transportation
infra_indicators <- infra_geos[,1]
infra_indicators$Pubtrans_Dependent <- 
  (infra_geos$Pop_pubtransport*100)/final_indicators$Population

infra_indicators$Pubtrans_heavilyDependent <- 
  (infra_geos$Pop_pubtrans_NoVehicle+
     infra_geos$Pop_poptrans_LongCommute*100)/final_indicators$Population

infra_indicators$No_Pubtrans<- 
  100-infra_indicators$Pubtrans_Dependent
#
#
#Communication sources
infra_indicators$No_CommunicationSources <- (final_indicators$Population-(infra_geos$WithTelephone_Owner+
                                                                            infra_geos$WithTelephone_Renter+
                                                                            infra_geos$WithInternet)*100)/final_indicators$Population


#
#
#Critical response facilities and support systems - hospitals, police and fire stations
infra_indicators$Protective_Services<- 
  (infra_geos$ProtectiveServices*100)/final_indicators$Population

infra_indicators <- infra_indicators[,1:6]

rm(infra_geos,infra_vars,infra_labs)

#
#
# WRITING INDICATORS
# Creating names of Files
name_All <- str_replace_all(paste("All_ACSIndicators_", Geo,".csv")," ", "")
name_C <- str_replace_all(paste("Community_ACSIndicators_", Geo,".csv")," ", "")
name_E <- str_replace_all(paste("Economy_ACSIndicators_", Geo,".csv")," ", "")
name_H <- str_replace_all(paste("Housing_ACSIndicators_", Geo,".csv")," ", "")
name_I <- str_replace_all(paste("Infrastructure_ACSIndicators_", Geo,".csv")," ", "")

# Creating new Folders
directory <- getwd()
directory <- str_replace_all(paste(directory, "/Data")," ","")
setwd(directory)
dir.create("AllIndicators")
dir.create("Community_Indicators")
dir.create("Economy_Indicators")
dir.create("Housing_Indicators")
dir.create("Infrastructure_Indicators")
dir.create("Environment_Indicators")

# Writing Files in new Folders
path <-  str_replace_all(paste("AllIndicators/",name_All)," ", "")
write_csv(final_indicators, path)
path <- str_replace_all(paste("Community_Indicators/",name_C)," ", "")
write_csv(community_indicators, path)
path <- str_replace_all(paste("Economy_Indicators/",name_E)," ", "")
write_csv(econ_indicators, path)
path <- str_replace_all(paste("Housing_Indicators/",name_H)," ", "")
write_csv(housing_indicators, path)
path <- str_replace_all(paste("Infrastructure_Indicators/",name_I)," ", "")
write_csv(infra_indicators, path)
#
#
# End Selection Here
# Press Enter
# All required indicator data will be generated
# Considering the structure of ACS data remains the same, and the tidycensus package is functional
#
# 
