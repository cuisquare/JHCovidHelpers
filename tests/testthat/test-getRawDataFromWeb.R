test_that("getRawDataFromWeb works", {
  master_data <- getRawDataFromWeb()
  test_exists_data <- exists("master_data")
  expect_equal(test_exists_data, TRUE)
})
