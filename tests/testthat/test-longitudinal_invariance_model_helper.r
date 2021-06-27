library(here)
source(here("R/longitudinal_invariance_model_helper.r"))

test_that("define_factor_line",{
  df = data.frame(factor1=c("var1", "var2", "var3"))
  result = define_factor_line(df)("factor1")
  expect_equal(result, "factor1 =~ NA*var1 + lambda1*var1 + var2 + var3")
})

test_that("define_factor_line weak",{
  df = data.frame(factor1=c("var1", "var2", "var3"))
  result = define_factor_line(df, TRUE)("factor1")
  expect_equal(result, "factor1 =~ NA*var1 + lambda1*var1 + lambda2*var2 + lambda3*var3")
})

test_that("define_factors", {
  dat_new <- data.frame(
    factor1=c("var1","var2"),
    factor2=c("2var1","2var2")
  )
  result <- define_factors(dat_new)
  expect_equal(result, 'factor1 =~ NA*var1 + lambda1*var1 + var2\nfactor2 =~ NA*2var1 + lambda1*2var1 + 2var2')
})


test_that("define_intercepts",{
  dat_new <- data.frame(
    factor1=c("var1","var2"),
    factor2=c("2var1","2var2")
  )
  result <- define_intercepts(dat_new)
  expect_equal(result, 'var1 ~ i1*1\nvar2 ~ 1\n\n2var1 ~ i1*1\n2var2 ~ 1')
})


test_that("define_intercepts strong",{
  dat_new <- data.frame(
    factor1=c("var1","var2"),
    factor2=c("2var1","2var2")
  )
  result <- define_intercepts(dat_new, TRUE)
  expect_equal(result, 'var1 ~ i1*1\nvar2 ~ i2*1\n\n2var1 ~ i1*1\n2var2 ~ i2*1')
})


test_that("define_intercepts partial strong - when constraining less than 50% of parcels",{
  dat_new <- data.frame(
    factor1=c("var1","var2", "var3"),
    factor2=c("2var1","2var2", "2var3")
  )
  expect_error(define_intercepts(dat_new, TRUE, unconstrained=c(2, 3)))
})

test_that("define_intercepts partial strong - when constraining at least 50% of parcels",{
  dat_new <- data.frame(
    factor1=c("var1","var2", "var3", "var4"),
    factor2=c("2var1","2var2", "2var3", "2var4")
  )
  result = define_intercepts(dat_new, TRUE, unconstrained=c(2, 3))
  expect_equal(result, 'var1 ~ i1*1\nvar2 ~ 1\nvar3 ~ 1\nvar4 ~ i4*1\n\n2var1 ~ i1*1\n2var2 ~ 1\n2var3 ~ 1\n2var4 ~ i4*1')
})


test_that("define_variances",{
  dat_new <- data.frame(
    factor1=c("var1","var2"),
    factor2=c("2var1","2var2")
  )
  result <- define_variances(dat_new)
  expect_equal(result, 'var1 ~~ var1\nvar2 ~~ var2\n\n2var1 ~~ 2var1\n2var2 ~~ 2var2')
})

test_that("define_variances unique",{
  dat_new <- data.frame(
    factor1=c("var1","var2"),
    factor2=c("2var1","2var2")
  )
  result <- define_variances(dat_new, TRUE)
  expect_equal(result, 'var1 ~~ u1*var1\nvar2 ~~ u2*var2\n\n2var1 ~~ u1*2var1\n2var2 ~~ u2*2var2')
})


test_that("define_covariances",{
  dat_new <- data.frame(
    factor1=c("1var1","1var2"),
    factor3=c("3var1","3var2"),
    factor5=c("5var1","5var2")
  )
  result <- define_covariances(dat_new)
  expect_equal(result, '1var1 ~~ 3var1\n1var2 ~~ 3var2\n\n1var1 ~~ 5var1\n1var2 ~~ 5var2\n\n3var1 ~~ 5var1\n3var2 ~~ 5var2')
})


test_that("latent variable means",{
  dat_new <- data.frame(
    factor1=c("1var1","1var2"),
    factor3=c("3var1","3var2"),
    factor5=c("5var1","5var2")
  )
  result <- define_latent_variable_means(dat_new)
  expect_equal(result, 'factor1 ~ 0*1\nfactor3 ~ 1\nfactor5 ~ 1')
})


test_that("latent variable variances",{
  dat_new <- data.frame(
    factor1=c("1var1","1var2"),
    factor3=c("3var1","3var2"),
    factor5=c("5var1","5var2")
  )
  result <- define_latent_variable_variances(dat_new)
  expect_equal(result, 'factor1 ~~ 1*factor1\nfactor3 ~~ factor3\nfactor5 ~~ factor5')
})

test_that("latent variable covariances",{
  dat_new <- data.frame(
    factor1=c("1var1","1var2"),
    factor3=c("3var1","3var2"),
    factor5=c("5var1","5var2")
  )
  result <- define_latent_variable_covariances(dat_new)
  expect_equal(result, 'factor1 ~~ factor3\nfactor1 ~~ factor5\nfactor3 ~~ factor5')
})
