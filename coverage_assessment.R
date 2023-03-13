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

total_children <- nrow(nut)
count(nrow)
A <- sum(nut$survey_round=="Baseline")
B <- sum(nut$survey_round=="Endline")
A+B
nrow("survey_round")
if (nut$survey_round != "Baseline" || "Endline")
{"Its the same"
} else {"False"
}









# Problem 2

B<- sum(nut$sex %in% c(2))
C <- sum(nut$sex %in% c(1))
B +C

sum(nut$sex, na.rm=TRUE)
table(nut$sex, useNA="ifany")

#3 How many children are from Grand Bassa and Urban Montserrado

table(nut$county,useNA = "ifany")

#How many children are males from Urban Montserrado and males from Grand Bassa?

table(nut$county, nut$sex, useNA="ifany")

#How many children are females from Urban Montserrado and females from Grand Bassa?
table(nut$county, nut$sex, useNA="ifany")
#What is the distribution of ages of children in the overall sample?
table(nut$age)
hist(nut$age)
#What is the distribution of ages of children by the sex of the child?
table(nut$sex)
hist(nut$sex)
#What is the distribution of ages of children by sex?
table(nut$age,nut$sex)
scatter.smooth(table(nut$age,nut$sex))

        
#What is the distribution of ages of children by location?
table(nut$county,nut$age)
barplot(table(nut$county,nut$age))

#What is the distribution of ages of children by sex and by location?
table(nut$sex, nut$county)
barplot(table(nut$sex,nut$county))
































