test_that("some data gets loaded from web", {
  master_data <- getDataFromWeb()
  test_exists_data <- exists("master_data")
  expect_equal(test_exists_data, TRUE)
})
