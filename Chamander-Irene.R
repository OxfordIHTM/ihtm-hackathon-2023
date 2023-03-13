##Hackathon

##read imported database

nut <-read.csv(file = "https://raw.githubusercontent.com/OxfordIHTM/ihtm-hackathon-2023/main/data/nutrition_survey_data.csv")

nut

#clean data, remove 99 from column "MUAC_SCREEN" AND "OEDEMA_SCREEN"

nut$muac_screen <- ifelse(nut$muac_screen == 99, NA, nut$muac_screen)
nut$muac_screen

nut$oedema_screen <- ifelse(nut$oedema_screen == 99, NA, nut$oedema_screen)
nut$oedema_screen

nrow(nut)

##Group work
##What is the coverage of mid-upper arm circumference (MUAC) screening among the children in the sample?
##What is the coverage of mid-upper arm circumference (MUAC) screening among the children in the sample by sex?
##What is the coverage of mid-upper arm circumference (MUAC) screening among the children in the sample by location?#
##What is the coverage of nutritional oedema screening among children in the sample?
##What is the coverage of nutritional oedema screening among children in the sample by sex?
##What is the coverage of nutritional oedema screening among children in the sample by location?

nut$oedema_screen[nut$oedema_screen == 99] <-NA
table(nut$oedema_screen, useNA = "ifany")
table(nut$oedema_screen, nut$sex, useNA = "ifany")
table(nut$oedema_screen, nut$county, useNA = "ifany")
table(nut$oedema_screen, nut$county, useNA = "ifany")

nut$muac_screen[nut$muac_screen == 99] <-NA
table(nut$county, nut$survey_round, nut$sex, nut$muac_screen, useNA = "ifany")

table(nut$survey_round, nut$sex, nut$muac_screen, nut$county, useNA = "ifany")

x<-table(nut$muac_screen[nut$county == "Urban Montserrado"], nut$sex[nut$county == "Urban Montserrado"],  nut$survey_round[nut$county == "Urban Montserrado"],useNA = "ifany")
prop.table(x)
y<-table(nut$muac_screen[nut$county == "Grand Bassa"], nut$sex[nut$county == "Grand Bassa"], nut$survey_round[nut$county == "Grand Bassa"], useNA = "ifany")
prop.table(y)

z<-table(nut$oedema_screen[nut$county == "Urban Montserrado"], nut$sex[nut$county == "Urban Montserrado"],  nut$survey_round[nut$county == "Urban Montserrado"], useNA = "ifany")
prop.table(z)
w<-table(nut$oedema_screen[nut$county == "Grand Bassa"], nut$sex[nut$county == "Grand Bassa"],  nut$survey_round[nut$county == "Grand Bassa"], useNA = "ifany")
prop.table(w)

