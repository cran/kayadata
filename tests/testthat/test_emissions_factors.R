context("Emissions factors")

test_that("Emissions factors with hydro", {
  ef <- emissions_factors(collapse_renewables = FALSE)
  expect_setequal(ef$fuel, c("Coal", "Oil", "Natural Gas", "Nuclear",
                             "Hydro", "Renewables"))
})

test_that("Emissions factors without hydro", {
  ef <- emissions_factors(collapse_renewables = TRUE)
  expect_setequal(ef$fuel, c("Coal", "Oil", "Natural Gas", "Nuclear",
                             "Renewables"))
})

