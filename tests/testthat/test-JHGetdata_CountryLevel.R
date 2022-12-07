master_data <- getDataFromWeb()
raw_master_data <- getRawDataFromWeb()

test_that("JHGetdata_CountryLevel removes duplicates", {
  uk_country_deaths <- JHGetdata_CountryLevel(
    master_data,
    CountryList = c("United Kingdom"),
    VarName = "Deaths")

  fr_country_deaths <- JHGetdata_CountryLevel(
    master_data,
    CountryList = c("France"),
    VarName = "Deaths")

  expect_equal(sum(duplicated(uk_country_deaths)), 0)
})


test_that("JHGetdata_CountryLevel works for countries with regional info only", {

  china_raw_data <- raw_master_data %>%
    filter(Country_Region == "China")

  china_raw_data_summary <- china_raw_data %>%
    group_by(Country_Region,Province_State) %>%
    summarise(nb_obs = n())

  china_country_deaths <- JHGetdata_CountryLevel(
    master_data,
    CountryList = c("China"),
    VarName = "Deaths")

  canada_country_deaths <- JHGetdata_CountryLevel(
    master_data,
    CountryList = c("Canada"),
    VarName = "Deaths")

  expect_equal(nrow(china_country_deaths), length(unique(raw_master_data$Date)))
  expect_equal(nrow(canada_country_deaths), length(unique(raw_master_data$Date)))
})

