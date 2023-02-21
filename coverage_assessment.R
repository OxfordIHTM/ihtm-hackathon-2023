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










# Problem 4: Coverage of severe acute malnutrition treatment ----
## number of moderate acute malnourished (MAM) cases children 6-59
with(nut, sum(muac >=11.5 & muac <12.5 & age >=6 & age <60 & oedema ==2, na.rm =TRUE))

##number of moderate acute malnutrition (MAM) cases in MALE children 6-59
with(nut, sum(muac >=11.5 & muac <12.5 & age >=6 & age <60 & sex ==1, na.rm =TRUE))

with(nut, sum(muac >=11.5 & muac <12.5 & age >=6 & age <60 & oedema ==2 & sex ==1, na.rm =TRUE))

##number of moderate acute malnourished (MAM) cases in FEMALE children 6-59
with(nut, sum(muac >=11.5 & muac <12.5 & age >=6 & age <60 & sex ==2, na.rm = TRUE))

with(nut, sum(muac >=11.5 & muac <12.5 & age >=6 & age <60 & oedema ==2 & sex ==2, na.rm = TRUE))

##number of moderate acute malnutrition (MAM) cases by location children 6-59
with(nut, sum(muac >=11.5 & muac <12.5 & age >=6 & age <60 & county =="Urban Montserrado", na.rm = TRUE))

##MAM sex and location for MALE
with(nut, sum(muac >=11.5 & muac <12.5 & age >=6 & age <60 & sex ==1 & county =="Urban Montserrado", na.rm = TRUE))

##MAM sex and location for FEMALE
with(nut, sum(muac >=11.5 & muac <12.5 & age >=6 & age <60 & sex ==2 & county =="Urban Montserrado", na.rm = TRUE))

##number of moderate acute malnutrition (MAM) cases children 6-59 montha old other than urban montserrado to GB_MAM
with(nut, sum(muac >=11.5 & muac <12.5 & age >=6 & age <60 & county =="Grand Bassa", na.rm = TRUE))

##Prevalence of children with MAM cases children 6-59 months old by sex and location
with(nut, sum(age >=6 & sex ==1, na.rm =TRUE))
##total number of male 6447







