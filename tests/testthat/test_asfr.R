context("Age-specific fertility rates and TFR")

library(hhsurveydata)

data(zzir62fl)

test_that("fertility calculations match DHS tables", {
  expect_equal(round(as.numeric(calc_tfr(zzir62fl)$tfr), 1), 4.7)
  expect_equal(round(as.numeric(calc_tfr(zzir62fl, ~v025)$tfr), 1), c(3.5, 5.7))
  expect_equal(round(1000*as.numeric(calc_asfr(zzir62fl)$asfr)),
               c(119, 207, 216, 188, 125, 60, 28))
})
 
