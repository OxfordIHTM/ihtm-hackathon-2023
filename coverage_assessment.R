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
table(nut$survey_round, nut$sex, nut$county, useNA = "ifany")
prop.table(nut$survey_round, nut$sex, nut$county, useNA = "ifany")

# Problem 2: Screening coverage ----
##Or you can do it like this for question 1
#How  many children are there in the overall sample
sum(is.na(nut$sex))


#Total number 
sum(nut$sex, na.rm = TRUE)

#2. Number of children that are male and female
table(nut$sex, useNA = "ifany")

#3 Number of children from urban Montserrado and Grand Bassa
table(nut$county, useNA = "ifany")

# 4and 5 male and female children from Urban Montserrado and males from Grand Bassa
table(nut$county, nut$sex, useNA = "ifany")

#6 Distribution of ages of children in the overall sample
table(nut$age)
summary(nut$age)
prop.table(table(nut$age, useNA = "ifany"))

#7 Distribution of ages of children by the sex of the child
table(nut$sex, nut$age)
scatter.smooth(table(nut$age, nut$sex))
hist(table(nut$age, nut$sex))

#8 Distribution of ages of children by location
table(nut$county, nut$age)
hist(table(nut$age, nut$county))
scatter.smooth(table(nut$age, nut$county))

#9 Distribution of ages of children by sex and by location
install.packages("ggplot2")
library(ggplot2)
ggplot(data = nut, aes(x = age, fill = sex)) +
  geom_histogram(alpha = 0.5, position = "identity", bins = 20) +
  facet_wrap(~county+sex) +
  labs(title = "Distribution of Ages of Children by Sex and Location", x = "Age", y = "Count")




# Problem 3: Prevalence of acute undernutrition/malnutrition ----










# Problem 4: Coverage of severe acute malnutrition treatment ----










