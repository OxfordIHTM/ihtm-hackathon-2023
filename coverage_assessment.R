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


SEX <- ifelse(nut$sex == 1, "Male", "Female") ##Assigning male and female to sex

OEDEMA <- ifelse(nut$oedema == 1, "Yes", "No") ## Assigning yes and No to oedema

COUNTY <- nut$county ##Assigning nut$county to COUNTY

Den <- sum(nut$age >=6 & nut$age<60, na.rm = TRUE) ## Assigning the total number of children between 6-60 to Den


####NUMBER AND PREVALENCE OF MAM CASES


Mod_MUAC <- with (nut, muac>= 11.5 & muac <12.5 & age >=6 & age<60) ## Assigning the moderate MUAC criteria for children between 6-60 for Mod_MUAC

MAM <- table (SEX, OEDEMA, Mod_MUAC, COUNTY, useNA = "ifany") ## Assigning MAM to table 

data.frame(MAM) ##CREATING A DATA FRAME FOR TOTAL MAM CASES


MAM_num_F1 <- MAM ["Female", "No", "TRUE","Urban Montserrado"] ## TOTAL MAM FEMALES IN URBAN MONTSERRADO

MAM_num_F2 <- MAM ["Female", "No", "TRUE","Grand Bassa"] ## TOTAL MAM FEMALES IN GRAND BASSA

MAM_num_M1 <- MAM ["Male", "No", "TRUE","Urban Montserrado"] ## TOTAL MAM MALES IN URBAN MONTSERRADO

MAM_num_M2 <- MAM ["Male", "No", "TRUE","Grand Bassa"] ## TOTAL MAM MALES IN GRAND BASSA

MAM_number <- data.frame(Sex = c("Female", "Female", "Male", "Male"),County = c("Urban Montserrado", "Grand Bassa", "Urban Montserrado", "Grand Bassa"), Number_of_MAM = c(MAM_num_F1, MAM_num_F2, MAM_num_M1, MAM_num_M2)) ## CREATING DATA FRAME FOR MAM CASES BY SEX AND COUNTY

MAM_number_table <- xtabs(Number_of_MAM ~ Sex + County, data = MAM_number) ##CREATING A TABLE BY SEX AND COUNTY

MAM_number_table ##VIEW MAM CASES BY SEX AND COUNTY

Prev1_MAM <- MAM ["Female", "No", "TRUE","Urban Montserrado"]/ Den ##PREVALENCE OF FEMALE MAM IN URBAN MONTSERRADO

Prev2_MAM <- MAM ["Female", "No", "TRUE","Grand Bassa"]/ Den #PREVALENCE OF FEMALE MAM IN GRAND BASSA

Prev3_MAM <- MAM ["Male", "No", "TRUE","Urban Montserrado"]/ Den #PREVALENCE OF MALE MAM IN URBAN MONTSERRADO

Prev4_MAM <- MAM ["Male", "No", "TRUE","Grand Bassa"]/ Den #PREVALENCE OF FEMALE MAM IN GRAND BASSA

MAM_prevalences <- data.frame(Sex = c("Female", "Female", "Male", "Male"),County = c("Urban Montserrado", "Grand Bassa", "Urban Montserrado", "Grand Bassa"),Prevalence_of_MAM = c(Prev1_MAM,Prev2_MAM,Prev3_MAM,Prev4_MAM)) ## PREVALENCE OF MAM CASES BY SEX AND COUNTY

MAM_prevalences_table <- xtabs(Prevalence_of_MAM ~ Sex + County, data = MAM_prevalences) ##CREATING TABLE FOR MAM PREVALENCE BY SEX AND COUNTY
prevalences

MAM_prevalences_table ##VIEW MAM PREVALENCE CASES BY SEX AND COUNTY


####NUMBER AND PREVALENCE OF SAM


Sev_MUAC <- with (nut, age >=6 & age<60 & muac <11.5) ## Assigning the severe MUAC criteria for children between 6-60 for Sev_MUAC

SAM <- table (SEX, OEDEMA, Sev_MUAC, COUNTY, useNA = "ifany") ## Assigning SAM

data.frame(SAM)

SAM_num_F1 <- SAM ["Female", "No", "TRUE","Urban Montserrado"] ## TOTAL SAM FEMALES IN URBAN MONTSERRADO

SAM_num_F2 <- SAM ["Female", "No", "TRUE","Grand Bassa"] ## TOTAL SAM FEMALES IN GRAND BASSA

SAM_num_M1 <- SAM ["Male", "No", "TRUE","Urban Montserrado"] ## TOTAL MAM MALES IN URBAN MONTSERRADO

SAM_num_M2 <- SAM ["Male", "No", "TRUE","Grand Bassa"] ## TOTAL MAM MALES IN GRAND BASSA

SAM_number <- data.frame(Sex = c("Female", "Female", "Male", "Male"),County = c("Urban Montserrado", "Grand Bassa", "Urban Montserrado", "Grand Bassa"), Number_of_SAM = c(SAM_num_F1, SAM_num_F2, SAM_num_M1, SAM_num_M2)) ## CREATING DATA FRAME FOR MAM CASES BY SEX AND COUNTY

SAM_number_table <- xtabs(Number_of_SAM ~ Sex + County, data = SAM_number) ## CREATING TABLE FOR SAM CASES BY SEX AND COUNTY

SAM_number_table ##VIEW SAM CASES BY SEX AND COUNTY


Prev1_SAM <- SAM ["Female", "Yes", "TRUE","Urban Montserrado"]/Den ##PREVALENCE OF FEMALE SAM IN URBAN MONTSERRADO

Prev2_SAM <- SAM ["Female", "No", "TRUE","Grand Bassa"]/Den ##PREVALENCE OF FEMALE SAM IN GRAND BASSA

Prev3_SAM <- SAM ["Male", "No", "TRUE","Urban Montserrado"]/Den ##PREVALENCE OF MALE SAM IN URBAN MONTSERRADO

Prev4_SAM <- SAM ["Male", "No", "TRUE","Grand Bassa"]/Den ##PREVALENCE OF MALE SAM IN GRAND BASSA

SAM_prevalences <- data.frame(Sex = c("Female", "Female", "Male", "Male"),County = c("Urban Montserrado", "Grand Bassa", "Urban Montserrado", "Grand Bassa"),Prevalence_of_SAM = c(Prev1_SAM,Prev2_SAM,Prev3_SAM,Prev4_SAM)) ## PREVALENCE OF SAM CASES BY SEX AND COUNTY

SAM_prevalences_table <- xtabs(Prevalence_of_SAM ~ Sex + County, data = SAM_prevalences) ## CREATING TABLE FOR SAM PREVALENCE BY SEX AND COUNTY

SAM_prevalences_table ##VIEW SAM PREVALENCE BY SEX AND COUNTY


###SAM OR MAM CASES:

MALNUTRITION <- ifelse(nut$muac <11.5 | nut$oedema == 1, "SAM","No SAM")

MALNUTRITION <- ifelse(nut$muac >=11.5 & nut$muac < 12.5 & nut$oedema == 2, "MAM", AM)

MALNUTRITION <- ifelse(AM == "No SAM", "not SAM or MAM", AM)

MALNUTRITION <- ifelse(nut$age >=6 & nut$age <60, AM, NA )

final<- table(SEX,OEDEMA,MALNUTRITION,COUNTY, useNA = "ifany") ##CREATING A TABLE FOR EITHER MAM OR SAM CASES

data.frame(final) ## CREATING A DATA FRAME FOR TOTAL SAM OR MAM CASES


##NUMBER OF SAM OR MAM CASES

SAMorMAM_F1 <- final["Female",, c("MAM","SAM"), "Urban Montserrado"] ##TOTAL SAM OR MAM FEMALES IN URBAN MONTSERRADO

SAMorMAM_F2 <- final["Female",,c("MAM","SAM"),"Grand Bassa"] ##TOTAL SAM OR MAM FEMALES IN GRAND BASSA

SAMorMAM_M1 <- final["Male",,c("MAM","SAM"),"Urban Montserrado"] ##TOTAL SAM OR MAM MALES IN URBAN MONTSERRADO

SAMorMAM_M2 <- final["Male",,c("MAM","SAM"),"Grand Bassa"] ##TOTAL SAM OR MAM FEMALES IN GRAND BASSA

SAMorMAM_number <- data.frame(Sex = c("Female", "Female", "Male", "Male"),County = c("Urban Montserrado", "Grand Bassa", "Urban Montserrado", "Grand Bassa"), Number_of_SAMorMAM = c(SAMorMAM_F1, SAMorMAM_F2, SAMorMAM_M1, SAMorMAM_M2)) ##CREATING A DATA FRAME FOR TOTAL SAM OR MAM CASES BY SEX AND COUNTY

SAMorMAM_number_table <- xtabs(Number_of_SAMorMAM ~ Sex + County, data = SAMorMAM_number) ##CREATING A TABLE FOR SAM OR MAM CASES BY SEX AND COUNTY

SAMorMAM_number_table ##VIEW SAM OR MAM CASES BY SEX AND COUNTY

Prev1_SAMorMAM <- final["Female",, c("MAM","SAM"), "Urban Montserrado"]/Den ##PREVALENCE OF FEMALE MAM OR SAM CASES IN URBAN MONTSERRADO

Prev2_SAMorMAM <- final["Female",, c("MAM","SAM"), "Grand Bassa"]/Den ##PREVALENCE OF FEMALE MAM OR SAM CASES IN GRAND BASSA

Prev3_SAMorMAM <- final["Female",, c("MAM","SAM"), "Urban Montserrado"]/Den ##PREVALENCE OF MALE MAM OR SAM CASES IN URBAN MONTSERRADO

Prev4_SAMorMAM <- final["Female",, c("MAM","SAM"), "Grand Bassa"]/Den ##PREVALENCE OF FEMALE MAM OR SAM CASES IN GRAND BASSA

SAMorMAM_prevalences <- data.frame(Sex = c("Female", "Female", "Male", "Male"),County = c("Urban Montserrado", "Grand Bassa", "Urban Montserrado", "Grand Bassa"),Prevalence_of_SAM = c(Prev1_SAMorMAM,Prev2_SAMorMAM,Prev3_SAMorMAM,Prev4_SAMorMAM)) ##CREATING A DATA FRAME FOR PREVALENCE OF SAM OR MAM CASES BY SEX AND COUNTY

SAMorMAM_prevalences_table <- xtabs(Prevalence_of_SAM ~ Sex + County, data = SAMorMAM_prevalences) ##CREATING A TABLE FOR PREVALENCE OF SAM OR MAM BY SEX AND COUNTY

SAMorMAM_prevalences_table ##VIEW SAM OR MAM PREVALENCE BY SEX AND COUNTY










# Problem 4: Coverage of severe acute malnutrition treatment ----










