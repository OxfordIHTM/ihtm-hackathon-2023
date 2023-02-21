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

## moderate acute malnutrition (MAM) case definition: MUAC < 12.5 and MUAC >= 11.5 cms and negative for nutritional oedema for children 6-59 months old

### Question 1: How many children 6-59 months old in the sample are moderate acute malnourished (MAM) cases? ----

#### to answer the question, get the summary the script below

with(nut, sum(muac >=11.5 & muac < 12.5 & age >=6 & age<60 & oedema == 2, na.rm = TRUE))

#### answer is 235 children 


### Question 2: How many children 6-59 months old in the sample are moderate acute malnourished (MAM) cases by sex and by location?

#### number of male MAM children is 86
with(nut, sum(muac >=11.5 & muac < 12.5 & age >=6 & age<60 & oedema == 2 & sex <=1, na.rm = TRUE))

#### number of female MAM children is 149
with(nut, sum(muac >=11.5 & muac < 12.5 & age >=6 & age<60 & oedema == 2 & sex == 2, na.rm = TRUE))

#### number of MAM children in Grand Bassa is 125
with(nut, sum(muac >=11.5 & muac < 12.5 & age >=6 & age<60 & oedema == 2 & county == 'Grand Bassa', na.rm = TRUE))

##### number of male MAM children in Grand Bassa is 45
with(nut, sum(muac >=11.5 & muac < 12.5 & age >=6 & age<60 & oedema == 2 & sex == 1 & county == 'Grand Bassa', na.rm = TRUE))

##### number of female MAM children in Grand Bassa is 80
with(nut, sum(muac >=11.5 & muac < 12.5 & age >=6 & age<60 & oedema == 2 & sex == 2 & county == 'Grand Bassa', na.rm = TRUE))

#### number of MAM children in Urban Montserrado is 110
with(nut, sum(muac >=11.5 & muac < 12.5 & age >=6 & age<60 & oedema == 2 & county == 'Urban Montserrado', na.rm = TRUE))

##### number of male MAM children in Urban Montserrado is 41
with(nut, sum(muac >=11.5 & muac < 12.5 & age >=6 & age<60 & sex == 1 & oedema == 2 & county == 'Urban Montserrado', na.rm = TRUE))

##### number of female MAM children in Urban Montserrado is 69
with(nut, sum(muac >=11.5 & muac < 12.5 & age >=6 & age<60 & sex == 2 & oedema == 2 & county == 'Urban Montserrado', na.rm = TRUE))


### Question 3: What is the prevalence of moderate acute malnutrition (MAM) among children 6-59 months old in the sample by sex and by location?

#### using table and data.frame to answer questions
##### table only gives a 2x2 table, so if you add a 3rd variable, it will give you multiple tables
table(nut$age < 60 & nut$age >= 6, nut$sex, nut$county, useNA = "ifany")

##### assign object to table code to simplify
t_all_age_sex_county <- table(nut$age < 60 & nut$age >= 6, nut$sex, nut$county, useNA = "ifany")

##### this gives a summary of the table above in one table
data.frame(t_all_age_sex_county)

##### assign object to dataframe code to simplify
df_all_age_sex_county <- data.frame(t_all_age_sex_county)

##### index to get value of male children in Grand Bassa is 2707
df_all_age_sex_county[2,4]

##### Prevalence of MAM



# Problem 4: Coverage of severe acute malnutrition treatment ----










