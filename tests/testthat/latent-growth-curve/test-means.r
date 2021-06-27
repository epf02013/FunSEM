library(here)
source(here("R/latent-growth-curve/means.r"))

test_that("define_means",{
  result <- define_means("variable_of_interest_name")
  expect_equal(result, 'variable_of_interest_name_level ~ 0*1')
})


test_that("define_means - with include_slopes true",{
  result <- define_means("variable_of_interest_name", TRUE)
  expect_equal(result, 'variable_of_interest_name_level ~ 0*1\nvariable_of_interest_name_slope ~ 1')
})
