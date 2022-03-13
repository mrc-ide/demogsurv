context("Demographic rate outputs")

library(demogsurv)

data(zzir)
data(zzbr)

test_that("fertility calculations match DHS tables", {
  expect_equal(round(as.numeric(calc_tfr(zzir)$tfr), 1), 4.7)
  expect_equal(round(as.numeric(calc_tfr(zzir, ~v025)$tfr), 1), c(3.5, 5.7))
  expect_equal(round(1000*as.numeric(calc_asfr(zzir)$asfr)),
               c(119, 207, 216, 188, 125, 60, 28))
})

test_that("fertility varmethod produce different results", {

  tfr_default <- calc_tfr(zzir)
  tfr_lin <- calc_tfr(zzir, varmethod = "lin")
  tfr_jk1 <- calc_tfr(zzir, varmethod = "jk1")
  tfr_jkn <- calc_tfr(zzir, varmethod = "jkn")
  tfr_none <- calc_tfr(zzir, varmethod = "none")

  expect_equal(tfr_default, tfr_lin)
  expect_equal(tfr_lin$tfr, tfr_jk1$tfr)
  expect_equal(tfr_jk1$tfr, tfr_jkn$tfr)
  expect_equal(tfr_default$tfr, tfr_none$tfr)
  
  expect_false(isTRUE(all.equal(tfr_lin$se_tfr, tfr_jk1$se_tfr)))
  expect_false(isTRUE(all.equal(tfr_jk1$se_tfr, tfr_jkn$se_tfr)))
  expect_null(tfr_none$se_tfr)

  expect_error(calc_tfr(zzir, varmethod = "jibberish"),
               'varmethod = "jibberish" is not recognized.')
})
 

test_that("child mortality calculations work", {
  zzbr$death <- zzbr$b5 == "no"  # b5: child still alive ("yes"/"no")
  zzbr$dod <- zzbr$b3 + zzbr$b7 + 0.5
  u5mr <- calc_nqx(zzbr)
  expect_equal(round(c(u5mr$est), 3), c(0.221, 0.194, 0.141))
  expect_equal(round(u5mr$se, 4), c(0.0117, 0.0083, 0.0064))
  expect_equal(round(c(calc_nqx(zzbr, by=~v102, tips=c(0, 5))$est), 3),
               c(0.153, 0.135))
})

test_that("user can specify arbitrary variable names for event args", {

  ## Rename death variable "d"
  zzbr$death <- NULL
  zzbr$d <- zzbr$b5 == "no"

  zzbr$dod <- NULL
  zzbr$dod_approx <- zzbr$b3 + zzbr$b7 + 0.5

  zzbr$DOB <- zzbr$b3
  zzbr$b3 <- NULL

  zzbr$intv_cmc <- zzbr$v008
  zzbr$v008 <- NULL

  u5mr <- calc_nqx(zzbr, dob = "DOB", dod = "dod_approx", death = "d", intv = "intv_cmc")
  expect_equal(round(u5mr$est, 3), c(0.221, 0.194, 0.141))
})

test_that("adult mortality calculation reproduce DHS tables", {
  zzsib <- reshape_sib_data(zzir)
  zzsib$death <- factor(zzsib$mm2, c("dead", "alive")) == "dead"
  zzsib$sex <- factor(zzsib$mm1, c("female", "male"))  # drop mm2 = 3: "missing"
  q3515 <- calc_nqx(zzsib, by=~sex, agegr=seq(15, 50, 5), tips=c(0, 7),
                    dob="mm4", dod="mm8")
  expect_equal(round(1000*c(q3515$est)), c(179, 177))
})


test_that("rate calculations work for single age group", {
  zzbr$death <- zzbr$b5 == "no"  # b5: child still alive ("yes"/"no")
  zzbr$dod <- zzbr$b3 + zzbr$b7 + 0.5
  expect_equal(round(c(calc_asfr(zzir, agegr=c(15, 20))$asfr), 4), 0.1190)
  expect_equal(round(c(calc_nqx(zzbr, agegr=c(0, 1), tips=c(0,5))$est), 4), 0.0884)
  expect_equal(round(calc_nqx(zzbr, agegr=c(0, 1), tips=c(0,5), varmethod="jk1")$se, 4), 0.0054)
})

test_that("variance calculation options work", {
    zzbr$death <- zzbr$b5 == "no"  # b5: child still alive ("yes"/"no")
    zzbr$dod <- zzbr$b3 + zzbr$b7 + 0.5
    expect_equal(round(calc_nqx(zzbr)$se, 4), c(0.0117, 0.0083, 0.0064))
    expect_equal(round(calc_nqx(zzbr, varmethod = "lin")$se, 4), c(0.0117, 0.0083, 0.0064))
    expect_equal(round(calc_nqx(zzbr, varmethod = "jk1")$se, 4), c(0.0121, 0.0084, 0.0065))
    expect_equal(round(calc_nqx(zzbr, varmethod = "jkn")$se, 4), c(0.0119, 0.0083, 0.0064))
    expect_error(calc_nqx(zzbr, varmethod = "jibberish"))
})
    
test_that("calc_asfr() throws error if bvars not found", {
  zzir_nobvars <- zzir[-grep("^b3_", names(zzir))]
  expect_error(calc_asfr(zzir_nobvars),
               "`bvars' must specified as variable or list of variables containing child DOB.")
})
