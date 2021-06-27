library(here)
source(here("R/latent-growth-curve/slopes.r"))

test_that("define_slope_factor",{
  dat_new <- data.frame(
    factor1=c("var1","var2"),
    factor2=c("2var1","2var2")
  )
  slope_weights = c(0,3)
  result <- define_slope_factor(dat_new, 'bilbo', slope_weights)
  expect_equal(result, 'bilbo_slope =~ 0*factor1 + 3*factor2')
})

test_that("define_slope_factor - given NAs",{
  dat_new <- data.frame(
    factor1=c("var1","var2"),
    factor2=c("2var1","2var2"),
    factor3=c("2var1","2var2"),
    factor4=c("2var1","2var2")
  )
  slope_weights = c(0,NA,NA,5)
  result <- define_slope_factor(dat_new, 'bilbo', slope_weights)
  expect_equal(result, 'bilbo_slope =~ 0*factor1 + NA*factor2 + NA*factor3 + 5*factor4')
})
