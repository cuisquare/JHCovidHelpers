
test_that("package datasets load appropriately", {
  #exported data set should be in ./data/countries_population.rda
  test_exists_countries_population <- exists("countries_population")
  expect_equal(test_exists_countries_population, TRUE)

  #internal data set should be in R./sysdata.rda
  test_exists_smaller_aug_data <- exists("smaller_aug_data")
  expect_equal(test_exists_smaller_aug_data, TRUE)
})
