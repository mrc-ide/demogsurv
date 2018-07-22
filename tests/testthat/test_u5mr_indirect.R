context("Indirect child mortality calculations")

library(demogsurv)

data(zzir)

test_that("indirect child mortality calculations", {
  zzir$ceb <- zzir$v201
  zzir$csurv <- zzir$v201 - zzir$v206 - zzir$v207
  u5mr_pw <- calc_nqx_indirect(zzir, family = "Princeton-West")
  
  ## Calculate 5q0 for males only based on the maternal age variant, using DHS
  zzir$ceb_m <- zzir$v202 + zzir$v204 + zzir$v206
  zzir$csurv_m <- zzir$ceb_m - zzir$v206
  u5mr_pw_m <- calc_nqx_indirect(zzir, ceb = "ceb_m", csurv = "csurv_m",
                                 family = "Princeton-West", sex = "males")

  ## 5q0 by sociodemographic characteristics
  u5mr_res <- calc_nqx_indirect(zzir, by=~v102) # by urban/rural residence

  expect_equal(round(as.numeric(u5mr_pw$u5mr), 3),
               c(0.192, 0.165, 0.175, 0.192, 0.201, 0.194, 0.201))
  expect_equal(round(as.numeric(u5mr_pw$se.u5mr), 3),
               c(0.021, 0.014, 0.009, 0.009, 0.011, 0.015, 0.009))
  expect_equal(round(as.numeric(u5mr_pw_m$u5mr), 3),
               c(0.225, 0.184, 0.175, 0.205, 0.201, .209, 0.216))
  expect_equal(round(as.numeric(u5mr_res$u5mr), 3),
               c(0.249, 0.172, 0.167, 0.162, 0.168, 0.176, 0.187,
                 0.191, 0.16, 0.218, 0.166, 0.205, 0.184, 0.206))
})
