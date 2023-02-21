## Team Charmander 

nut<-read.csv(file = "https://raw.githubusercontent.com/OxfordIHTM/ihtm-hackathon-2023/main/data/nutrition_survey_data.csv")
data
names(nut)
ncol(nut)
nrow(nut)
names(nut) [6]
nut[6]
nut$muac[nut$muac == -99] <- NA