test_that("adding population data works", {
  master_data <- getDataFromWeb()
  master_data_withpopulation <- master_data %>%
    addPopulationData()
  expect_equal(exists("master_data_withpopulation"), TRUE)
  expect_equal("Population" %in% names(master_data_withpopulation), TRUE)
})
