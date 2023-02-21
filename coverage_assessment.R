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
#Number of children in endline 6552
sum(nut$survey_round == "Endline")
nrow (survey_round)
ncol (survey_round)

A <- sum(nut$survey_round == "Baseline")
B <- sum(nut$survey_round == "Endline")
sum("baseline" + "endline" )

sum (A+B)
if(nut$survey_round != "Baseline" && "Endline")
 #total number of children in baseline and endline 12811 
#total number of children in the data set
sum(nut$sex %in% c(1))
sum(nut$sex %in% c(2))
sum(nut$sex %in% c(NA))
A <- sum(nut$sex %in% c(NA))
B<- sum(nut$sex %in% c(2))
C <- sum(nut$sex %in% c(1))
B+C-A
sum(nut$sex, na.rm=TRUE)
table(nut$sex,useNA="ifany")
#There are 6471 males and 6239 males and 101 NAs  in the data set
#3 How many children are in the counties
table(nut$county)
#there are 5355 males and 7456 females in the counties
table(nut$county, nut$sex,useNA="ifany")
#there are 2709 males in GB and 3762 in UM and 2615 females in GB and 3264 in UM
#What is the distributtion of age
table(nut$age)


# Problem 2: Screening coverage ----










# Problem 3: Prevalence of acute undernutrition/malnutrition ----










# Problem 4: Coverage of severe acute malnutrition treatment ----










