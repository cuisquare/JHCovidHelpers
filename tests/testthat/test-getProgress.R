test_that("getProgress runs without errors", {
  CountryList <- c("United Kingdom","France","Italy","Germany","Belgium","Greece")

  Progress_Increase_Deaths <- smaller_aug_data %>%
    JHGetProgress(CountryList,"Increase_Deaths_Avg")

  test_exists_output <- exists("Progress_Increase_Deaths")
  expect_equal(test_exists_output, TRUE)
})
