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



table(nut$sam)

# What is the coverage of severe acute malnutrition (SAM) treatment in the sample?
  
  # 1. How many severe acute malnourished children 6-59 months old are currently in the treatment programme?

nut$sam[nut$age>=6 & nut$age<60 &(nut$muac>=11.5 | nut$oedema==2)]<-'no'
nut$sam[nut$age>=6 & nut$age<60 &(nut$muac<11.5 | nut$oedema==1)]<-'yes'

table(nut$sam)

  # 2. How many severe acute malnourished children 6-59 months old are currently in the treatment programme by location?

table(nut$sam,nut$county)
  
  # 3. How many children 6-59 months old are currently in the treatment programme and are recovering from severe acute malnutrition?
  
table(nut$sam, nut$survey_round)

  # 4. How many children 6-59 months old are currently in the treatment programme and are recovering from severe acute malnutrition by location?

table(nut$sam, nut$survey_round, nut$county)

  # 5. What is the case-finding effectiveness of the SAM treatment programme?
       ### sam covered divided by all sam
        #### cov_status:	Is the child currently receiving treatment with peanut butter medicine/Plumpynut? 1 = Yes; 2 = No

table(nut$sam=='yes', nut$cov_status)

samcov <- table(nut$sam, nut$cov_status)
prop.table(samcov)*100


  # 6. What is the case-finding effectiveness of the SAM treatment programme by location?

samcovloc <- table(nut$sam, nut$cov_status, nut$county)
prop.table(samcovloc)*100

  # 7. What is the treatment coverage of the SAM treatment programme?

table(nut$cov_status)

##### numerator sam or not sam + covered.... denominator sam + (covstatus== 1 & not sam)

table(nut$sam, nut$cov_status==1)
table(nut$cov_status==1 & nut$sam=="no")


  # 8. What is the treatment coverage of the SAM treatment programme by location?


