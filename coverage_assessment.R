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






 |  



# Problem 2: Screening coverage ----










# Problem 3: Prevalence of acute undernutrition/malnutrition ----










# Problem 4: Coverage of severe acute malnutrition treatment ----

> nut$sam[nut$age>=6 & nut$age<60 &(nut$muac<11.5 | nut$oedema==1)]<-'yes'
> nut$sam[nut$age>=6 & nut$age<60 &(nut$muac>=11.5 | nut$oedema==2)]<-'no'








