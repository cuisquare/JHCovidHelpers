#generate minimal augmented data for testing purposes
CountryList <- c("United Kingdom","France","Italy","Germany","Belgium","Greece","Canada","United States")
nbdays <- 10

master_data <- getDataFromWeb() %>%
  addPopulationData()

smaller_master_data <- master_data %>%
  getLatestData(CountryList,nbdays)

smaller_aug_data <- smaller_master_data %>%
  addCalculatedVariables()

usethis::use_data(smaller_master_data, smaller_aug_data, internal = TRUE, overwrite = TRUE)
