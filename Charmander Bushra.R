## Team Charmander 

nut<-read.csv(file = "https://raw.githubusercontent.com/OxfordIHTM/ihtm-hackathon-2023/main/data/nutrition_survey_data.csv")
nut <- read.csv(file = "https://raw.githubusercontent.com/OxfordIHTM/ihtm-hackathon-2023/main/data/nutrition_survey_data.csv", header = TRUE)
names(nut)
ncol(nut)
length(names(nut))
nrow(nut)
names(nut) [6]
nut[6]
nut$muac
nut$muac_screen
nut$muac[nut$muac == 99] <- NA
nut$muac_screen[nut$muac_screen == 99] <-NA
sum(nut$muac_screen, header = TRUE)
sum(nut$muac_screen, na.rm = TRUE)
sum(nut$muac, na.rm = TRUE)
sum(nut$muac_screen == 1, na.rm = TRUE)
sum(nut$muac_screen == 2, na.rm = TRUE)
###dont have to write 1 or 2 in table function)
table(nut$muac_screen, useNA = "ifany")
table(nut$muac_screen, nut$sex, useNA = "ifany")
table(nut$muac_screen, nut$county, useNA = "ifany")
nut$oedema_screen[nut$oedema_screen == 99] <-NA
table(nut$oedema_screen, useNA = "ifany")
table(nut$oedema_screen, nut$sex, useNA = "ifany")
table(nut$oedema_screen, nut$county, useNA = "ifany")
