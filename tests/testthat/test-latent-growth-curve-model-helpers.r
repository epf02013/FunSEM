test_that("set_time_invariant_covariate when regress_slope_on_covariate is FALSE",{
  result <- set_time_invariant_covariate("a_var","blah", FALSE)
  expect_equal(result, 'a_var_level ~ blah')
})

test_that("set_time_invariant_covariate when regress_slope_on_covariate is TRUE",{
  result <- set_time_invariant_covariate("a_var","blah", TRUE)
  expect_equal(result, 'a_var_level ~ blah\na_var_slope ~ blah')
})
