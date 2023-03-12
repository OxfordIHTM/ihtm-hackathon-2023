# GOAL of this research: ARE WE IMPROVING on SCREENING of MUAC and OEDEMA? 
##Are wee expanding our screening coverage? From baseline vs. endline? By sex? By location?

## Install & Load R packages 
install.packages("gtsummary")
install.packages("dplyr")

library(dplyr)
library("gtsummary")

## Load functions in R directory 
for (f in list.files("R", full.names = TRUE)) source (f)

# Read data 
nut<-read.csv(file="https://raw.githubusercontent.com/OxfordIHTM/ihtm-hackathon-2023/main/data/nutrition_survey_data.csv")  
nut

##Subset data: Transform variables of interest into table nut1 
nut1 <- nut %>% select(survey_round,county,sex,muac_screen,oedema_screen)

View(nut1)

##Clean data in subset: Change 99 to NA 
###Clean data only after having made subset nut1. Code don't work if data is cleaned in nut and then subset. 
nut1$muac_screen <- ifelse(nut1$muac_screen == 99, "NA", nut1$muac_screen)
nut1$oedema_screen <- ifelse(nut1$oedema_screen == 99, "NA", nut1$muac_screen)

##1. What is the coverage of mid-upper arm circumference (MUAC) screening among the children in the sample?
## Use tbl_summary to compare muac_screen by survey round, endline vs. baseline
nut1 %>% tbl_summary(by = survey_round, 
                      include=c(muac_screen),
                      statistic = list(all_continuous() ~ "{mean} ({sd})"))
                                     
nut1

### The endline data shows LITTLE if NO improvement in MUAC screening coverage among the sample children vs. baseline. Percentage of children had MUAC screened (answered 1 to survey) stayed relatively the same from baseline to endline. The percentage of children without MUAC screening went up from 73% to 93%, mainly due to children from the NA population at baseline moving over. This may suggest a better awareness in this population related to not having gotten MUAC screening?

##2. What is the coverage of mid-upper arm circumference (MUAC) screening among the children in the sample by sex?

##Present data: Replace value 1 and 2 in sex to Male and Female for better presentation.
nut1$sex <- ifelse(nut1$sex == 1, "Male", nut1$sex)
nut1$sex <- ifelse(nut1$sex == 2, "Female", nut1$sex)
nut1

nut1 %>% tbl_strata(strata=sex,.tbl_fun = ~ .x %>% 
                       tbl_summary(by = survey_round,
                                   include=c(muac_screen),
                                   statistic = list(all_continuous() ~ "{mean} ({sd})")))

### There is no apparent difference in MUAC screening coverage between girls and boys in the sample. Or, gender doesn't seem to influence MUAC screening in children.

##3. What is the coverage of mid-upper arm circumference (MUAC) screening among the children in the sample by location?
nut1 %>% tbl_strata(strata=county,.tbl_fun = ~ .x %>% 
                       tbl_summary(by = survey_round,
                                   include=c(muac_screen),
                                   statistic = list(all_continuous() ~ "{mean} ({sd})")))

### The data suggests an increase in children receiving MUAC screening at endline in Grand Bassa vs. in Urban Montserrado. In fact, it seems that the coverage of MUAC screening in Urban Montserrado decreases (from 4.5% at baseline to 3% at endline). This suggests Grand Bassa to have done a better job in MUAC screening than Montserrado. However, neither location has a significant improvement in MUAC screening coverage. The overall trend of increasing in no screening is observed here but this is again due to population who answered NA at baseline moving to answered no to screening at endline.

##4. What is the coverage of nutritional oedema screening among children in the sample?
nut1 %>% tbl_summary(by = survey_round, 
                      include=c(oedema_screen),
                      statistic = list(all_continuous() ~ "{mean} ({sd})"))

### Similar to MUAC screening, the endline data shows LITTLE if NO improvement in OEDEMA screening coverage among the sample children vs. baseline. The same observation applies for movement of populations who answered NA for Oedema screening at baseline to answering no to Oedema screening at endline. This may suggest again a better sense of awareness and may serve as a reflection of public engagement working?

##5. What is the coverage of nutritional oedema screening among children in the sample by sex?
nut1 %>% tbl_strata(strata=sex,.tbl_fun = ~ .x %>% 
                       tbl_summary(by = survey_round,
                                   include=c(oedema_screen), 
                                   statistic = list(all_continuous() ~ "{mean} ({sd})")))
### The data suggests oedema screening improves SLIGHTLY in females vs. in males. However, this difference does not seem big enough to call for gender as a determinant in improvement of oedema screening. p-value may be needed to determine if the difference is significant.  

##6. What is the coverage of nutritional oedema screening among children in the sample by location?
nut1 %>% tbl_strata(strata=county,.tbl_fun = ~ .x %>% 
                       tbl_summary(by = survey_round,
                                   include=c(oedema_screen),
                                   statistic = list(all_continuous() ~ "{mean} ({sd})")))

### The same trend is observed here for oedema screening. The data suggests an increase in oedema screening coverage in Grand Bassa but a decrease in Urban Montserrado, suggesting Grand Bassa doing a better job. 

##7. Summary all selected variables in a table stratified by survey_round and include p values
nut1 %>% tbl_summary(by = survey_round, 
                      statistic = list(all_continuous() ~ "{mean} ({sd})")) %>% add_p() 

### Pearson's Chi-squared test at the end help to show that difference between endline and baseline is indeed statistically significant. But this may be driven mainly by the number answering NO to screening increasing between baseline and endline, NOT a reflection of how well the screening has increased between the 2 timepoints.