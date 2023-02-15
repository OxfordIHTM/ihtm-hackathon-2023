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

library(dplyr)

## Load functions in R directory ----
for (f in list.files("R", full.names = TRUE)) source (f)


# Read data ----
nut <- get_data()


with(nut, sum(muac >= 11.5 & muac <12.5 & oedema == 2, na.rm= TRUE))

##number of moderate acute malnourished (MAM) cases children 6-59 months old 


with(nut, sum(muac >= 11.5 & muac <12.5 & oedema == 2 & sex == 1 , na.rm= TRUE ))

##number of moderate acute malnourished (MAM) male 6-59 months old 


with(nut, sum(muac >= 11.5 & muac <12.5 & oedema == 2 & sex == 2 , na.rm= TRUE ))

##number of moderate acute malnourished (MAM) female 6-59 months old 


with(nut, sum(muac >= 11.5 & muac <12.5 & oedema == 2 & county == "Urban Montserrado" , na.rm= TRUE))

##number of moderate acute malnourished (MAM) cases children 6-59 months old  in Urban Montserrado


with(nut, sum(muac >= 11.5 & muac <12.5 & oedema == 2 & county != "Urban Montserrado" , na.rm= TRUE))

##number of moderate acute malnourished (MAM) cases children 6-59 months old other than Urban Montserrado





# Problem 4: Coverage of severe acute malnutrition treatment ----










