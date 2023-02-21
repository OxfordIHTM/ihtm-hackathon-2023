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

#1st question
total_children <- nrow(nut)
count(nrow)
A <- sum(nut$survey_round=="Baseline")
B <- sum(nut$survey_round=="Endline")
A+B
if (nut$survey_round != "Baseline" || "Endline")
    print("Its the same")
else 
    print("False")










