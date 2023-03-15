################################################################################
#
# MSc IHTM Hackathon 2023
#
################################################################################

# Load libraries ----

## Load R packages ----
library(dplyr)
library(gtsummary)  ## Used by Team Charmander/@quyen for their tables
library(ggplot2)    ## Used by Team Eevee for their plots

## Load functions in R directory ----
for (f in list.files("R", full.names = TRUE)) source (f)


# Read data ----
nut <- get_data()


# Problem 1: Describe the data ----

## number of children in Baseline
sum(nut$survey_round == "Baseline")

## number of children in Endline
sum(nut$survey_round == "Endline")

#nrow(survey_round)

## Allocate Baseline to A
A <- sum(nut$survey_round == "Baseline")

## Allocate Endline to B
B <- sum(nut$survey_round == "Endline")

## Total
sum(A+B)

## Total without NA
#sum(1218+1187+1491+1428+1968+1861+1794+1763)
#12710

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

sum(nut$sex, na.rm = TRUE)

## 2.How many children are males and how many are females?
table(nut$sex, useNA = "ifany")

## 3. How many children are from Urban Montserrado and how many are from Grand Bassa?
table(nut$county, useNA = "ifany")

## 4. How many children are males from Urban Montserrado and males from Grand Bassa? and 5. How many children are females from Urban Montserrado and females from Grand Bassa?
table(nut$county, nut$sex, useNA = "ifany")

## 6. What is the distribution of ages of children in the overall sample?
table(nut$age)
hist(nut$age)

## 7. What is the distribution of ages of children by the sex of the child?
table(nut$age, nut$sex)
scatter.smooth(table(nut$age, nut$sex))

## 8. What is the distribution of ages of children by location?
table(nut$county, nut$age)
barplot(
  height = table(nut$county, nut$age),
  beside = FALSE,
  legend.text = TRUE,
  main = "Distribution of Ages of Children by Location",
  xlab = "Age (months)",
  ylab = "n"
)

## 9. What is the distribution of ages of children by sex and by location?

ggplot(
  data = nut, 
  aes(
    x = age, 
    fill = factor(sex, labels = c("Male", "Female")),
    group = factor(sex, labels = c("Male", "Female"))
  )
) +
  geom_histogram(alpha = 0.5, bins = 60) +
  scale_x_continuous(n.breaks = 24) +
  facet_wrap( ~ county, ncol = 2) +
  labs(
    title = "Distribution of Ages of Children by Sex and Location", 
    x = "Age (months()", y = "n",
    fill = NULL
  ) +
  theme_bw() +
  theme(legend.position = "top")


# Problem 2: Screening coverage ----
## Following code based on R script in hackathon2.R from @quyen. Modifications
## done to ensure continuity with code written by other groups.
## Note that this code has created a dependency to the {gtsummary} package.
## Dependency has been declared in the early part of this script and renv.lock
## updated accordingly

## Subset data: Transform variables of interest into table nut1 
nut1 <- nut %>% select(survey_round,county,sex,muac_screen,oedema_screen)

## Clean data in subset: Change 99 to NA 
### Clean data only after having made subset nut1. 
### Code don't work if data is cleaned in nut and then subset. ???
nut1$muac_screen <- ifelse(nut1$muac_screen == 99, NA, nut1$muac_screen)
nut1$muac_screen[nut$muac_screen == 1] <- "Y"
nut1$muac_screen[nut$muac_screen == 2] <- "N"

nut1$oedema_screen <- ifelse(nut1$oedema_screen == 99, NA, nut1$oedema_screen)
nut1$oedema_screen[nut$oedema_screen == 1] <- "Y"
nut1$oedema_screen[nut$oedema_screen == 2] <- "N"

nut1$sex <- ifelse(nut1$sex == 1, "Male", "Female")

## 1. What is the coverage of mid-upper arm circumference (MUAC) screening among 
## the children in the sample?
## Use tbl_summary to compare muac_screen by survey round, endline vs. baseline
nut1 %>% 
  tbl_summary(
    by = survey_round, 
    include = muac_screen,
    statistic = list(all_continuous() ~ "{mean} ({sd})")
  )

## The endline data shows LITTLE if NO improvement in MUAC screening coverage 
## among the sample children vs. baseline. Percentage of children had MUAC 
## screened (answered 1 to survey) stayed relatively the same from baseline 
## to endline. The percentage of children without MUAC screening went up from 
## 73% to 93%, mainly due to children from the NA population at baseline 
## moving over. This may suggest a better awareness in this population related 
## to not having gotten MUAC screening?

## 2. What is the coverage of mid-upper arm circumference (MUAC) screening 
## among the children in the sample by sex?
nut1 %>% 
  tbl_strata(
    strata = sex, 
    .tbl_fun = ~ .x %>% 
      tbl_summary(
        by = survey_round,
        include = muac_screen,
        statistic = list(all_continuous() ~ "{mean} ({sd})")
      )
  )

## There is no apparent difference in MUAC screening coverage between girls 
## and boys in the sample. Or, gender doesn't seem to influence MUAC screening 
## in children.

## 3. What is the coverage of mid-upper arm circumference (MUAC) screening 
## among the children in the sample by location?
nut1 %>% 
  tbl_strata(
    strata=county,
    .tbl_fun = ~ .x %>% 
      tbl_summary(
        by = survey_round,
        include = muac_screen,
        statistic = list(all_continuous() ~ "{mean} ({sd})")
      )
   )

## The data suggests an increase in children receiving MUAC screening at 
## endline in Grand Bassa vs. in Urban Montserrado. In fact, it seems that 
## the coverage of MUAC screening in Urban Montserrado decreases 
## (from 4.5% at baseline to 3% at endline). This suggests Grand Bassa to have 
## done a better job in MUAC screening than Montserrado. However, neither 
## location has a significant improvement in MUAC screening coverage. 
## The overall trend of increasing in no screening is observed here but this 
## is again due to population who answered NA at baseline moving to answered 
## no to screening at endline.

## 4. What is the coverage of nutritional oedema screening among children in 
## the sample?
nut1 %>% 
  tbl_summary(
    by = survey_round, 
    include = oedema_screen,
    statistic = list(all_continuous() ~ "{mean} ({sd})")
  )

## Similar to MUAC screening, the endline data shows LITTLE if NO improvement 
## in OEDEMA screening coverage among the sample children vs. baseline. The 
## same observation applies for movement of populations who answered NA for 
## Oedema screening at baseline to answering no to Oedema screening at endline. 
## This may suggest again a better sense of awareness and may serve as a 
## reflection of public engagement working?

## 5. What is the coverage of nutritional oedema screening among children in 
## the sample by sex?
nut1 %>% 
  tbl_strata(
    strata = sex,
    .tbl_fun = ~ .x %>% 
      tbl_summary(
        by = survey_round,
        include = oedema_screen, 
        statistic = list(all_continuous() ~ "{mean} ({sd})")
      )
  )

## The data suggests oedema screening improves SLIGHTLY in females vs. in males. 
## However, this difference does not seem big enough to call for gender as a 
## determinant in improvement of oedema screening. p-value may be needed to 
## determine if the difference is significant.  

## 6. What is the coverage of nutritional oedema screening among children in 
## the sample by location?
nut1 %>% 
  tbl_strata(
    strata = county,
    .tbl_fun = ~ .x %>% 
      tbl_summary(
        by = survey_round,
        include = oedema_screen,
        statistic = list(all_continuous() ~ "{mean} ({sd})")
      )
  )

## The same trend is observed here for oedema screening. The data suggests an 
## increase in oedema screening coverage in Grand Bassa but a decrease in 
## Urban Montserrado, suggesting Grand Bassa doing a better job. 

## 7. Summary all selected variables in a table stratified by survey_round and 
## include p values
nut1 %>% 
  tbl_summary(
    by = survey_round, 
    statistic = list(all_continuous() ~ "{mean} ({sd})")
  ) %>% 
  add_p() 

## Pearson's Chi-squared test at the end help to show that difference between 
## endline and baseline is indeed statistically significant. But this may be 
## driven mainly by the number answering NO to screening increasing between 
## baseline and endline, NOT a reflection of how well the screening has 
## increased between the 2 timepoints.

# Problem 3: Prevalence of acute undernutrition/malnutrition ----

## Create vectors of variables

### Assigning male and female to sex
SEX <- ifelse(nut$sex == 1, "Male", "Female") 

### Assigning yes and No to oedema
OEDEMA <- ifelse(nut$oedema == 1, "Yes", "No") 

### Assigning nut$county to COUNTY
COUNTY <- nut$county 

### Assigning the total number of children between 6-59 to Den
Den <- sum(nut$age >= 6 & nut$age < 60, na.rm = TRUE) 

## NUMBER AND PREVALENCE OF MAM CASES

### Assigning the moderate MUAC criteria for children between 6-60 for Mod_MUAC
Mod_MUAC <- with(
  nut, 
  muac >= 11.5 & muac < 12.5 & age >= 6 & age < 60
) 

### Assigning MAM to table
MAM <- table(SEX, OEDEMA, Mod_MUAC, COUNTY, useNA = "ifany")  

### CREATING A DATA FRAME FOR TOTAL MAM CASES
data.frame(MAM) 

### TOTAL MAM FEMALES IN URBAN MONTSERRADO
MAM_num_F1 <- MAM["Female", "No", "TRUE","Urban Montserrado"] 

### TOTAL MAM FEMALES IN GRAND BASSA
MAM_num_F2 <- MAM["Female", "No", "TRUE","Grand Bassa"] 

### TOTAL MAM MALES IN URBAN MONTSERRADO
MAM_num_M1 <- MAM["Male", "No", "TRUE","Urban Montserrado"]

### TOTAL MAM MALES IN GRAND BASSA
MAM_num_M2 <- MAM ["Male", "No", "TRUE","Grand Bassa"] 

MAM_number <- data.frame(
  Sex = c("Female", "Female", "Male", "Male"),
  County = c("Urban Montserrado", "Grand Bassa", "Urban Montserrado", "Grand Bassa"), 
  Number_of_MAM = c(MAM_num_F1, MAM_num_F2, MAM_num_M1, MAM_num_M2)
) 

## CREATING DATA FRAME FOR MAM CASES BY SEX AND COUNTY

### CREATING A TABLE BY SEX AND COUNTY
MAM_number_table <- xtabs(
  Number_of_MAM ~ Sex + County, data = MAM_number
) 

### VIEW MAM CASES BY SEX AND COUNTY
MAM_number_table 

### PREVALENCE OF FEMALE MAM IN URBAN MONTSERRADO
Prev1_MAM <- MAM["Female", "No", "TRUE","Urban Montserrado"] / Den 

### PREVALENCE OF FEMALE MAM IN GRAND BASSA
Prev2_MAM <- MAM["Female", "No", "TRUE","Grand Bassa"] / Den 

### PREVALENCE OF MALE MAM IN URBAN MONTSERRADO
Prev3_MAM <- MAM["Male", "No", "TRUE","Urban Montserrado"] / Den 

### PREVALENCE OF FEMALE MAM IN GRAND BASSA
Prev4_MAM <- MAM["Male", "No", "TRUE","Grand Bassa"] / Den 

### PREVALENCE OF MAM CASES BY SEX AND COUNTY
MAM_prevalences <- data.frame(
  Sex = c("Female", "Female", "Male", "Male"),
  County = c("Urban Montserrado", "Grand Bassa", "Urban Montserrado", "Grand Bassa"),
  Prevalence_of_MAM = c(Prev1_MAM, Prev2_MAM, Prev3_MAM, Prev4_MAM)
) 

## CREATING TABLE FOR MAM PREVALENCE BY SEX AND COUNTY

MAM_prevalences_table <- xtabs(
  Prevalence_of_MAM ~ Sex + County, data = MAM_prevalences
) 

### VIEW MAM PREVALENCE CASES BY SEX AND COUNTY
MAM_prevalences_table 


## NUMBER AND PREVALENCE OF SAM

### Assigning the severe MUAC criteria for children between 6-59 for Sev_MUAC
Sev_MUAC <- with(nut, age >= 6 & age < 60 & muac < 11.5) 

### Assigning SAM
SAM <- table (SEX, OEDEMA, Sev_MUAC, COUNTY, useNA = "ifany") 

data.frame(SAM)

### TOTAL SAM FEMALES IN URBAN MONTSERRADO
SAM_num_F1 <- SAM["Female", "No", "TRUE","Urban Montserrado"] 

### TOTAL SAM FEMALES IN GRAND BASSA
SAM_num_F2 <- SAM["Female", "No", "TRUE","Grand Bassa"] 

### TOTAL SAM MALES IN URBAN MONTSERRADO
SAM_num_M1 <- SAM["Male", "No", "TRUE","Urban Montserrado"] 

### TOTAL SAM MALES IN GRAND BASSA
SAM_num_M2 <- SAM["Male", "No", "TRUE","Grand Bassa"] 

### CREATING DATA FRAME FOR SAM CASES BY SEX AND COUNTY
SAM_number <- data.frame(
  Sex = c("Female", "Female", "Male", "Male"),
  County = c("Urban Montserrado", "Grand Bassa", "Urban Montserrado", "Grand Bassa"), 
  Number_of_SAM = c(SAM_num_F1, SAM_num_F2, SAM_num_M1, SAM_num_M2)
)

## CREATING TABLE FOR SAM CASES BY SEX AND COUNTY
SAM_number_table <- xtabs(
  Number_of_SAM ~ Sex + County, data = SAM_number
) 

### VIEW SAM CASES BY SEX AND COUNTY
SAM_number_table 

### PREVALENCE OF FEMALE SAM IN URBAN MONTSERRADO
Prev1_SAM <- SAM["Female", "Yes", "TRUE","Urban Montserrado"] / Den 

### PREVALENCE OF FEMALE SAM IN GRAND BASSA
Prev2_SAM <- SAM["Female", "No", "TRUE","Grand Bassa"] / Den 

### PREVALENCE OF MALE SAM IN URBAN MONTSERRADO
Prev3_SAM <- SAM["Male", "No", "TRUE","Urban Montserrado"] / Den 

### PREVALENCE OF MALE SAM IN GRAND BASSA
Prev4_SAM <- SAM["Male", "No", "TRUE","Grand Bassa"] / Den 

### PREVALENCE OF SAM CASES BY SEX AND COUNTY
SAM_prevalences <- data.frame(
  Sex = c("Female", "Female", "Male", "Male"),
  County = c("Urban Montserrado", "Grand Bassa", "Urban Montserrado", "Grand Bassa"),
  Prevalence_of_SAM = c(Prev1_SAM, Prev2_SAM, Prev3_SAM, Prev4_SAM)
) 

## CREATING TABLE FOR SAM PREVALENCE BY SEX AND COUNTY
SAM_prevalences_table <- xtabs(
  Prevalence_of_SAM ~ Sex + County, data = SAM_prevalences
) 

### VIEW SAM PREVALENCE BY SEX AND COUNTY
SAM_prevalences_table 


## SAM OR MAM CASES:
MALNUTRITION <- ifelse(nut$muac < 11.5 | nut$oedema == 1, "SAM", "No SAM")

MALNUTRITION <- ifelse(
  nut$muac >= 11.5 & nut$muac < 12.5 & nut$oedema == 2, "MAM", MALNUTRITION
)

MALNUTRITION <- ifelse(MALNUTRITION == "No SAM", "not SAM or MAM", MALNUTRITION)

MALNUTRITION <- ifelse(nut$age >= 6 & nut$age < 60, MALNUTRITION, NA)

### CREATING A TABLE FOR EITHER MAM OR SAM CASES
final <- table(
  SEX, OEDEMA, MALNUTRITION, COUNTY, useNA = "ifany"
) 

### CREATING A DATA FRAME FOR TOTAL SAM OR MAM CASES
data.frame(final) 

## NUMBER OF SAM OR MAM CASES

### TOTAL SAM OR MAM FEMALES IN URBAN MONTSERRADO
SAMorMAM_F1 <- final["Female", , c("MAM", "SAM"), "Urban Montserrado"] 

### TOTAL SAM OR MAM FEMALES IN GRAND BASSA
SAMorMAM_F2 <- final["Female", , c("MAM", "SAM"), "Grand Bassa"] 

### TOTAL SAM OR MAM MALES IN URBAN MONTSERRADO
SAMorMAM_M1 <- final["Male", , c("MAM", "SAM"), "Urban Montserrado"] 

### TOTAL SAM OR MAM FEMALES IN GRAND BASSA
SAMorMAM_M2 <- final["Male", , c("MAM", "SAM"), "Grand Bassa"] 

### CREATING A DATA FRAME FOR TOTAL SAM OR MAM CASES BY SEX AND COUNTY
SAMorMAM_number <- data.frame(
  Sex = c("Female", "Female", "Male", "Male"), 
  County = c("Urban Montserrado", "Grand Bassa", "Urban Montserrado", "Grand Bassa"), 
  Number_of_SAMorMAM = c(SAMorMAM_F1, SAMorMAM_F2, SAMorMAM_M1, SAMorMAM_M2)
) 

### CREATING A TABLE FOR SAM OR MAM CASES BY SEX AND COUNTY
SAMorMAM_number_table <- xtabs(
  Number_of_SAMorMAM ~ Sex + County, data = SAMorMAM_number
) 

### VIEW SAM OR MAM CASES BY SEX AND COUNTY
SAMorMAM_number_table 

### PREVALENCE OF FEMALE MAM OR SAM CASES IN URBAN MONTSERRADO
Prev1_SAMorMAM <- final["Female", , c("MAM", "SAM"), "Urban Montserrado"] / Den 

### PREVALENCE OF FEMALE MAM OR SAM CASES IN GRAND BASSA
Prev2_SAMorMAM <- final["Female", , c("MAM", "SAM"), "Grand Bassa"] / Den 

### PREVALENCE OF MALE MAM OR SAM CASES IN URBAN MONTSERRADO
Prev3_SAMorMAM <- final["Female", , c("MAM", "SAM"), "Urban Montserrado"] / Den 

### PREVALENCE OF FEMALE MAM OR SAM CASES IN GRAND BASSA
Prev4_SAMorMAM <- final["Female", , c("MAM", "SAM"), "Grand Bassa"] / Den 

### CREATING A DATA FRAME FOR PREVALENCE OF SAM OR MAM CASES BY SEX AND COUNTY
SAMorMAM_prevalences <- data.frame(
  Sex = c("Female", "Female", "Male", "Male"),
  County = c("Urban Montserrado", "Grand Bassa", "Urban Montserrado", "Grand Bassa"),
  Prevalence_of_SAM = c(Prev1_SAMorMAM, Prev2_SAMorMAM, Prev3_SAMorMAM, Prev4_SAMorMAM)
) 

### CREATING A TABLE FOR PREVALENCE OF SAM OR MAM BY SEX AND COUNTY
SAMorMAM_prevalences_table <- xtabs(
  Prevalence_of_SAM ~ Sex + County, 
  data = SAMorMAM_prevalences
) 

### VIEW SAM OR MAM PREVALENCE BY SEX AND COUNTY
SAMorMAM_prevalences_table 


# Problem 4: Coverage of severe acute malnutrition treatment ----
## What is the coverage of severe acute malnutrition (SAM) treatment in the 
## sample?

table(nut$sam)

## 1. How many severe acute malnourished children 6-59 months old are currently 
## in the treatment programme?

nut$sam[nut$age >= 6 & nut$age < 60 & (nut$muac >= 11.5 | nut$oedema == 2)] <- 'no'
nut$sam[nut$age >= 6 & nut$age < 60 & (nut$muac < 11.5 | nut$oedema == 1)] <-'yes'

table(nut$sam)

## 2. How many severe acute malnourished children 6-59 months old are currently 
## in the treatment programme by location?

table(nut$sam, nut$county)
  
## 3. How many children 6-59 months old are currently in the treatment 
## programme and are recovering from severe acute malnutrition?
  
table(nut$sam, nut$survey_round)

## 4. How many children 6-59 months old are currently in the treatment programme 
## and are recovering from severe acute malnutrition by location?

table(nut$sam, nut$survey_round, nut$county)

## 5. What is the case-finding effectiveness of the SAM treatment programme?
       
### sam covered divided by all sam
### cov_status:	Is the child currently receiving treatment with peanut 
### butter medicine/Plumpynut? 1 = Yes; 2 = No

table(nut$sam == 'yes', nut$cov_status)

samcov <- table(nut$sam, nut$cov_status)
prop.table(samcov) * 100


## 6. What is the case-finding effectiveness of the SAM treatment programme by 
## location?

samcovloc <- table(nut$sam, nut$cov_status, nut$county)
prop.table(samcovloc)*100

## 7. What is the treatment coverage of the SAM treatment programme?

table(nut$cov_status)

### numerator sam or not sam + covered.... 
### denominator sam + (covstatus== 1 & not sam)

table(nut$sam, nut$cov_status==1)
table(nut$cov_status==1 & nut$sam=="no")


## 8. What is the treatment coverage of the SAM treatment programme by location?
