test_that("getLatestData runs without errors", {
  CountryList <- c("United Kingdom","France","Italy","Germany","Belgium","Greece")

  nbdays_test <- 5

  Latest_Data <- smaller_aug_data %>%
    getLatestData(CountryList,nbdays_test )

  test_exists_output <- exists("Latest_Data")
  expect_equal(test_exists_output, TRUE)
  test_rightnumberofdays <- (Latest_Data %>% filter(is.na(Province_State)) %>% nrow() == nbdays_test *length(CountryList))
  expect_equal(test_rightnumberofdays, TRUE)
})
