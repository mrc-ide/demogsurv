context("Demographic rate outputs")

library(hhsurveydata)

data(zzir62fl)
data(zzbr62fl)

test_that("fertility calculations match DHS tables", {
  expect_equal(round(as.numeric(calc_tfr(zzir62fl)$tfr), 1), 4.7)
  expect_equal(round(as.numeric(calc_tfr(zzir62fl, ~v025)$tfr), 1), c(3.5, 5.7))
  expect_equal(round(1000*as.numeric(calc_asfr(zzir62fl)$asfr)),
               c(119, 207, 216, 188, 125, 60, 28))
})
 

test_that("child mortality calculations work", {
  zzbr <- zzbr62fl
  zzbr$death <- zzbr$b5 == "no"  # b5: child still alive ("yes"/"no")
  zzbr$dod <- zzbr$b3 + zzbr$b7 + 0.5
  u5mr <- calc_nqx(zzbr)
  expect_equal(round(c(u5mr$nqx), 3), c(0.221, 0.194, 0.141))
  expect_equal(round(u5mr$se, 4), c(0.0117, 0.0083, 0.0064))
  expect_equal(round(c(calc_nqx(zzbr, by=~v102, tips=c(0, 5))$nqx), 3),
               c(0.153, 0.135))
})
 
