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
## number of children in Baseline
sum(nut$survey_round == "Baseline")

## number of children in Endline
sum(nut$survey_round == "Endline")

nrow(survey_round)
## Allocate Baseline to A
A <- sum(nut$survey_round == "Baseline")

## Allocate Endline to B
B <- sum(nut$survey_round == "Endline")

## Total
sum(A+B)

## Total without NA
sum(1218+1187+1491+1428+1968+1861+1794+1763)

12710

## This is the SUM way of doing it
sum(nut$sex %in% c(1))
sum(nut$sex %in% c(2))
sum(nut$sex %in% c(NA))
A <- sum(nut$sex %in% c(NA))
B<- sum(nut$sex %in% c(2))
C <- sum(nut$sex %in% c(1))
B+C-A

## Or you can do it like this for question 1. How many children are in the overall sample?
sum(is.na(nut$sex))

sum(nut$sex, na.rm=TRUE)

## 2.How many children are males and how many are females?
table(nut$sex, useNA="ifany")

## 3. How many children are from Urban Montserrado and how many are from Grand Bassa?
table(nut$county, useNA="ifany")

## 4. How many children are males from Urban Montserrado and males from Grand Bassa? and 5. How many children are females from Urban Montserrado and females from Grand Bassa?
table(nut$county, nut$sex, useNA="ifany")

## 6. What is the distribution of ages of children in the overall sample?
table(nut$age)




# Problem 2: Screening coverage ----










# Problem 3: Prevalence of acute undernutrition/malnutrition ----










# Problem 4: Coverage of severe acute malnutrition treatment ----










