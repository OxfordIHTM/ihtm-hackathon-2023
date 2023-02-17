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

with(nut, sum(muac >= 11.5 & muac <12.5  & age >=6 & age <60 & oedema == 2, na.rm= TRUE))

##number of moderate acute malnourished (MAM) cases children 6-59 months old 

Male_MAM <- with(nut, sum(muac >= 11.5 & muac <12.5 & age >=6 & age <60 & oedema == 2 & sex == 1 , na.rm= TRUE ))

##assigning moderate acute malnourished (MAM) male 6-59 months old to Male_MAM

Female_MAM <- with(nut, sum(muac >= 11.5 & muac <12.5 & age >=6 & age <60 & oedema == 2 & sex == 2 , na.rm= TRUE ))

##assigning  moderate acute malnourished (MAM) female 6-59 months old to Female_MAM

UM_MAM <- with(nut, sum(muac >= 11.5 & muac <12.5 & age >=6 & age <60 & oedema == 2 & county == "Urban Montserrado" , na.rm= TRUE))

##assigning moderate acute malnourished (MAM) cases children 6-59 months old  in Urban Montserrado to UM_MAM

GB_MAM <- with(nut, sum(muac >= 11.5 & muac <12.5 &age >=6 & age <60 & oedema == 2 & county == "Grand Bassa" , na.rm= TRUE))

##assigning  moderate acute malnourished (MAM) cases children 6-59 months old other than Urban Montserrado to GB_MAM

MAM <- data.frame(Male_MAM, Female_MAM, UM_MAM, GB_MAM)

## creating a data frame for MAM

if(interactive()) View(MAM)

##Viewing MAM data



####Alternate table method for number of moderate,severe and normal malnutrition by sex and by location:

num_data <- with(nut, data.frame(sex,county,muac,oedema))

##assigning the data frame to num_data

SEX <- ifelse(num_data$sex == "1", "Male", "Female")
OEDEMA <- ifelse(num_data$oedema == "1", "Yes", "No")
MUAC <- with( num_data, ifelse (muac <11.5, "Severe",
                               ifelse(oedema == 1, "Severe",
                                      ifelse(muac >=11.5 & muac<12.5, "Moderate",
                                             ifelse (muac >=12.5, "Normal", "No")))))

list <- data.frame(SEX,MUAC, OEDEMA, num_data$county)

##creating data frame by sex,muac,oedema and location


tab <-table(SEX,num_data$county, MUAC )

##creating table showing different numbers of children according to moderate,severe degree of Malnutrition and normal children

tab


##viewing table 


###Prevalence of MAM:

den1 <- with(nut, sum(age >=6 & age < 60, na.rm = TRUE ))

##assigning total number of children aged 6-59 months old to den1

Male_MAM/den1

##Prevalence of moderate acute malnourished (MAM) male 6-59 months old 

Female_MAM/den1

##Prevalence of moderate acute malnourished (MAM) female 6-59 months old 

UM_MAM/den1

##Prevalence of moderate acute malnourished (MAM) cases children 6-59 months old  in Urban Montserrado

GB_MAM/den1

##Prevalence of moderate acute malnourished (MAM) cases children 6-59 months old other than Urban Montserrado


####SAM

with(nut, sum(muac < 11.5  & age >=6 & age <60 , oedema == 1, na.rm= TRUE))

##number of severe acute malnourished (SAM) cases children 6-59 months old 

Male_SAM <- with(nut, sum(muac < 11.5 & age >=6 & age <60 & sex == 1, oedema == 1  , na.rm= TRUE ))

##assigning severe acute malnourished (MAM) female 6-59 months old to Male_SAM

Female_SAM <- with(nut, sum(muac < 11.5 & age >=6 & age <60 & sex == 2, oedema == 1  , na.rm= TRUE ))

##assigning severe acute malnourished (MAM) female 6-59 months old to Female_SAM

with(Male_SAM, data.frame())

UM_SAM <- with(nut, sum(muac < 11.5  & age >=6 & age <60 & county == "Urban Montserrado" ,oedema == 1, na.rm= TRUE))

##assigning severe acute malnourished (MAM) cases children 6-59 months old  in Urban Montserrado to UM_SAM

GB_SAM <- with(nut, sum(muac < 11.5  & age >=6 & age <60 & county == "Grand Bassa" ,oedema == 1, na.rm= TRUE))

##assigning moderate acute malnourished (MAM) cases children 6-59 months old  other than Urban Montserrado to GB_MA


###Prevalence of SAM:

Prev1 <- Male_SAM/den1

##Prevalence of severe acute malnourished (MAM) male 6-59 months old 

Prev2 <- Female_SAM/den1

##Prevalence of severe acute malnourished (MAM) female 6-59 months old 

Prev3 <- UM_SAM/den1

##Prevalence of severe acute malnourished (MAM) cases children 6-59 months old in Urban Montserrado

Prev4<- GB_SAM/den1

##Prevalence of severe acute malnourished (MAM) cases children 6-59 months old other than Urban Montserrado






# Problem 4: Coverage of severe acute malnutrition treatment ----










