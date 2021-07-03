test_that("define_level_factor",{
    dat_new <- data.frame(
      factor1=c("var1","var2"),
      factor2=c("2var1","2var2")
    )
    result <- define_level_factor(dat_new, "pop")
    expect_equal(result, 'pop_level =~ 1*factor1 + 1*factor2')
})
