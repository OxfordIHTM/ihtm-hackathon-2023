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

####MAM

with(nut, sum(muac >= 11.5 & muac <12.5  & age >=6 & age <=59 & oedema == 2, na.rm= TRUE))

##number of moderate acute malnourished (MAM) cases children 6-59 months old 

Male_MAM <- with(nut, sum(muac >= 11.5 & muac <12.5 & age >=6 & age <=59 & oedema == 2 & sex == 1 , na.rm= TRUE ))

##assigning moderate acute malnourished (MAM) male 6-59 months old to Male_MAM

Female_MAM <- with(nut, sum(muac >= 11.5 & muac <12.5 & age >=6 & age <=59 & oedema == 2 & sex == 2 , na.rm= TRUE ))

##assigning  moderate acute malnourished (MAM) female 6-59 months old to Female_MAM

UM_MAM <- with(nut, sum(muac >= 11.5 & muac <12.5 & age >=6 & age <=59 & oedema == 2 & county == "Urban Montserrado" , na.rm= TRUE))

##assigning moderate acute malnourished (MAM) cases children 6-59 months old  in Urban Montserrado to UM_MAM

GB_MAM <- with(nut, sum(muac >= 11.5 & muac <12.5 &age >=6 & age <=59 & oedema == 2 & county != "Urban Montserrado" , na.rm= TRUE))

##assigning  moderate acute malnourished (MAM) cases children 6-59 months old other than Urban Montserrado to GB_MAM

MAM <- data.frame(Male_MAM, Female_MAM, UM_MAM, GB_MAM)

## creating a data frame for MAM

if(interactive()) View(MAM)

##Viewing MAM data

den1 <- with(nut, sum(age >=5 & age < 60, na.rm = TRUE ))

##assigning total number of children aged 5-60 months old to den1


###Prevalence of MAM:

Male_MAM/den1

##Prevalence of moderate acute malnourished (MAM) male 6-59 months old 

Female_MAM/den1

##Prevalence of moderate acute malnourished (MAM) female 6-59 months old 

UM_MAM/den1

##Prevalence of moderate acute malnourished (MAM) cases children 6-59 months old  in Urban Montserrado

GB_MAM/den1

##Prevalence of moderate acute malnourished (MAM) cases children 6-59 months old other than Urban Montserrado


####SAM

with(nut, sum(muac < 11.5  & age >=6 & age <=59 , oedema == 1, na.rm= TRUE))

##number of severe acute malnourished (SAM) cases children 6-59 months old 

Male_SAM <- with(nut, sum(muac < 11.5 & age >=6 & age <=59 & sex == 1, oedema == 1  , na.rm= TRUE ))

##assigning severe acute malnourished (MAM) female 6-59 months old to Male_SAM

Female_SAM <- with(nut, sum(muac < 11.5 & age >=6 & age <=59 & sex == 2, oedema == 1  , na.rm= TRUE ))

##assigning severe acute malnourished (MAM) female 6-59 months old to Female_SAM

UM_SAM <- with(nut, sum(muac < 11.5  & age >=6 & age <=59 & county == "Urban Montserrado" ,oedema == 1, na.rm= TRUE))

##assigning severe acute malnourished (MAM) cases children 6-59 months old  in Urban Montserrado to UM_SAM

GB_SAM <- with(nut, sum(muac < 11.5  & age >=6 & age <=59 & county != "Urban Montserrado" ,oedema == 1, na.rm= TRUE))

##assigning moderate acute malnourished (MAM) cases children 6-59 months old  other than Urban Montserrado to GB_MA

SAM <- data.frame(Male_SAM, Female_SAM, UM_SAM, GB_SAM)

## creating a data frame for SAM

if(interactive()) View(SAM)

##Viewing SAM data


###Prevalence of SAM:

Male_SAM/den1

##Prevalence of severe acute malnourished (MAM) male 6-59 months old 

Female_SAM/den1

##Prevalence of severe acute malnourished (MAM) female 6-59 months old 

UM_SAM/den1

##Prevalence of severe acute malnourished (MAM) cases children 6-59 months old in Urban Montserrado

GB_SAM/den1

##Prevalence of severe acute malnourished (MAM) cases children 6-59 months old other than Urban Montserrado


"git status"





# Problem 4: Coverage of severe acute malnutrition treatment ----










