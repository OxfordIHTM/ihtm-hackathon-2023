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

#How many children in the baseline 
sum(nut$survey_round == "Baseline")
#Number of children in endline
sum(nut$survey_round == "Endline")
nrow ("survey_round")

A=sum(nut$survey_round == "Baseline")
B= sum(nut$survey_round == "Endline")

sum (A+B)
if(nut$survey_round!= "Baseline" && "Endline")
  






# Problem 2: Screening coverage ----










# Problem 3: Prevalence of acute undernutrition/malnutrition ----










# Problem 4: Coverage of severe acute malnutrition treatment ----










