################################################################################
#
# MSc IHTM Hackathon 2023
#
################################################################################
#Install Dplyr
install.packages("dplyr")
# Load libraries ----

## Load R packages ----
library(dplyr)

## Load functions in R directory ----
for (f in list.files("R", full.names = TRUE)) source (f)


# Read data ----
nut <- get_data()


# Problem 1: Describe the data ----
##Number of children in Baseline
A <- sum(nut$survey_round == "Baseline")
##Number of children in Endline
B <- sum(nut$survey_round == "Endline")
nrow("survey_round")
sum(A+B)
##Checking the total number of data points in survey round
length(nut$survey_round)
#So there are no entries other than baseline and endline in survey_round column


# Problem 2: Screening coverage ----










# Problem 3: Prevalence of acute undernutrition/malnutrition ----










# Problem 4: Coverage of severe acute malnutrition treatment ----










