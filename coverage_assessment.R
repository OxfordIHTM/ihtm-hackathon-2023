################################################################################
#
# MSc IHTM Hackathon 2023
#
################################################################################

# Load libraries ----

## Load R packages ----
library(dplyr)

## Load functions in R directory ----
for (f in list.files("R", full.names = TRUE)) source (f)


# Read data ----
nut <- get_data()


# Problem 1: Describe the data ----










# Problem 2: Screening coverage ----










# Problem 3: Prevalence of acute undernutrition/malnutrition ----
View(nut)



##How many children 6-59 months old in the sample are moderate acute malnourished (MAM) cases?
with(nut, sum(muac <12.5 & muac >= 11.5 & oedema == 2 & age >= 6 & age <=59, na.rm = TRUE) )

##How many children 6-59 months old in the sample are moderate acute malnourished (MAM) cases by sex and by location?
Mam_Male_GB <- with(nut, sum(muac <12.5 & muac >= 11.5 & oedema == 2 & age >= 6 & age <=59 & sex == 1 & county == 'Grand Bassa', na.rm = TRUE) )

Mam_Male_UM <- with(nut, sum(muac <12.5 & muac >= 11.5 & oedema == 2 & age >= 6 & age <=59 & sex == 1 & county == 'Urban Montserrado', na.rm = TRUE) )

Mam_Female_GB <- with(nut, sum(muac <12.5 & muac >= 11.5 & oedema == 2 & age >= 6 & age <=59 & sex == 2 & county == 'Grand Bassa', na.rm = TRUE) )

Mam_Female_UM <- with(nut, sum(muac <12.5 & muac >= 11.5 & oedema == 2 & age >= 6 & age <=59 & sex == 2 & county == 'Urban Montserrado', na.rm = TRUE) )



##What is the prevalence of moderate acute malnutrition (MAM) among children 6-59 months old in the sample by sex and by location?
Total_Male_GB <- with(nut, sum(age >= 6 & age <=59 & sex == 1 & county == 'Grand Bassa', na.rm = TRUE) )

Total_Male_UM <- with(nut, sum(age >= 6 & age <=59 & sex == 1 & county == 'Urban Montserrado', na.rm = TRUE) )

Total_Female_GB <- with(nut, sum(age >= 6 & age <=59 & sex == 2 & county == 'Grand Bassa', na.rm = TRUE) )

Total_Female_UM <- with(nut, sum(age >= 6 & age <=59 & sex == 2 & county == 'Urban Montserrado', na.rm = TRUE) )

###Prevalence on MAM in Males in Grand Bassa
Mam_Male_GB / Total_Male_GB * 100

with(nut, sum(muac <12.5 & muac >= 11.5 & oedema == 2 & age >= 6 & age <=59 & sex == 1 & county == 'Grand Bassa', na.rm = TRUE) ) / with(nut, sum(age >= 6 & age <=59 & sex == 1 & county == 'Grand Bassa', na.rm = TRUE) ) * 100

Mam_Male_UM / Total_Male_UM * 100

Mam_Female_GB / Total_Female_GB * 100

Mam_Female_UM / Total_Female_UM * 100

####table version 
table(nut$age < 60 & nut$age >= 6, nut$sex, useNA = "ifany")

total_sex_country <- table(nut$age < 60 & nut$age >= 6, nut$sex, nut$county, useNA = "ifany")

data.frame(total_sex_country) [2,4]

##How many children 6-59 months old in the sample are severe acute malnourished (SAM) cases?
##How many children 6-59 months old in the sample are severe acute malnourished (SAM) cases by sex and by location?
##What is the prevalence of severe acute malnutrition (SAM) among children 6-59 months old in the sample by sex and by location?
##How many children 6-59 months old in the sample are acute malnourished cases (either MAM or SAM)?
##How many children 6-59 months old in the sample are acute malnourished cases (either MAM or SAM) by sex and by location?
##What is the prevalence of acute malnutrition (either MAM or SAM) among children 6-59 months old in the sample by sex and location?







# Problem 4: Coverage of severe acute malnutrition treatment ----










