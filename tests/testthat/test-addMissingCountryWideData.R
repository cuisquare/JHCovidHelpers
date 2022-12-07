raw_master_data <- getRawDataFromWeb()

test_that("addMissingCountryWideData works", {

  china_raw_data <- raw_master_data %>%
    filter(Country_Region == "China")

  china_raw_data_summary <- china_raw_data %>%
    group_by(Country_Region,Province_State) %>%
    summarise(nb_obs = n())

  china_corrected_data <- addMissingCountryWideData(china_raw_data)

  expect_equal(china_corrected_data %>% filter(is.na(Province_State)) %>% nrow(), length(unique(china_raw_data$Date)))
})
