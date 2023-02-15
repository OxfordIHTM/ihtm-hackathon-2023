################################################################################
#
#'
#' Retrieve and process data for Hackathon 2023
#'
#
################################################################################

get_data <- function(file = "data/nutrition_survey_data.csv") {
  round1 <- read.csv(file = "https://github.com/validmeasures/liberiaData/raw/master/data-raw/dataRound1/coverageData.csv")
  
  round1 <- round1 |>
    dplyr::select(
      sdate, county, sex, age, muac1, oedema1, 
      muac_screen, oedema_screen, cov_status
    ) |>
    dplyr::rename(
      survey_date = sdate,
      muac = muac1,
      oedema = oedema1
    ) |>
    dplyr::mutate(
      survey_round = "Baseline", .before = survey_date,
      survey_date = as.Date(survey_date),
      sex = ifelse(sex == "n/a", NA, sex) |> as.integer(),
      age = ifelse(age == "n/a", NA, age) |>
        as.integer(),
      age = ifelse(age < 0, NA, age),
      muac = ifelse(muac == "n/a", NA, muac) |>
        as.numeric(),
      muac = ifelse(muac == 0, NA, muac),
      oedema = ifelse(oedema == "n/a", NA, oedema) |>
        as.integer(),
      muac_screen = ifelse(muac_screen == "n/a", NA, muac_screen) |>
        as.integer(),
      oedema_screen = ifelse(oedema_screen == "n/a", NA, oedema_screen) |>
        as.integer(),
      cov_status = ifelse(cov_status == "n/a", NA, cov_status) |>
        as.integer()
    )
  
  round2_file <- tempfile()
  
  download.file(
    url = "https://github.com/validmeasures/liberiaData/raw/master/data/coverageData.r2.rda",
    destfile = round2_file
  )
  
  load(file = round2_file)
  
  round2 <- coverageData.r2 |>
    dplyr::select(
      sdate, cid, sex, age, muac1, oedema1, 
      muac_screen, oedema_screen, cov_status
    ) |>
    dplyr::rename(
      survey_date = sdate,
      county = cid,
      muac = muac1,
      oedema = oedema1
    ) |>
    dplyr::mutate(
      survey_round = "Endline", .before = survey_date,
      survey_date = as.Date(survey_date, format = "%b %d, %Y"),
      county = ifelse(county == 1, "Urban Montserrado", "Grand Bassa"),
      age = ifelse(age < 0, NA, age),
      
    )
  
  nut_data <- rbind(round1, round2)
  
  write.csv(
    x = nut_data, 
    file = file, 
    row.names = FALSE
  )
  
  nut_data
}





