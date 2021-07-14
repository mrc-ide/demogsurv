context("Demographic rate outputs")

library(demogsurv)
library(rdhs)

data(zzir)
data(zzbr)

test_that("fertility calculations match DHS tables", {
  expect_equal(round(as.numeric(calc_tfr(zzir)$tfr), 1), 4.7)
  expect_equal(round(as.numeric(calc_tfr(zzir, ~v025)$tfr), 1), c(3.5, 5.7))
  expect_equal(round(1000*as.numeric(calc_asfr(zzir)$asfr)),
               c(119, 207, 216, 188, 125, 60, 28))
})

test_that("fertility calculations match DHS tables when only ever married women surveyed", {

  # load data from 1989-1990 Sudan DHS
  surveys <- rdhs::dhs_surveys(countryIds = "SD", surveyYear = 1990, surveyType = "DHS")
  testthat::skip_if_not(
    nrow(surveys) == 1,
    message = "Unable to test fertility calculations with `awfact` argument due to not having access to 1990 SDN DHS"
  )

  ird <- rdhs::dhs_datasets(fileType = "IR", fileFormat = "flat")
  ird <- ird[ird$SurveyId %in% surveys$SurveyId,]
  brd <- rdhs::dhs_datasets(fileType = "BR", fileFormat = "flat")
  brd <- brd[brd$SurveyId %in% surveys$SurveyId,]

  rdhs::get_datasets(ird$FileName)
  rdhs::get_datasets(brd$FileName)

  ird_vars <- c("caseid", "v005", "v008", "v011", "v001", "v101", "v102", "awfactt", "awfactu")
  ird_questions <- rdhs::search_variables(ird$FileName, variables = ird_vars)
  brd_vars <- c("caseid", "bidx", "b3")
  brd_questions <- rdhs::search_variables(brd$FileName, variables = brd_vars)

  ir <- rdhs::extract_dhs(ird_questions, add_geo = FALSE)
  br <- rdhs::extract_dhs(brd_questions, add_geo = FALSE)

  ## Convert to factors (a bit inefficient)
  ir <- lapply(ir, haven::as_factor)
  br <- lapply(br, haven::as_factor)

  # expected values from final report
  # https://dhsprogram.com/publications/publication-FR36-DHS-Final-Reports.cfm
  calc_args <- list(
    data = ir$SDIR02FL, bhdata = br$SDBR02FL,
    clusters = ~v001, strata = ~v101 + v102, bvars = "b3",
    tips = c(0, 5),
    awfact = "awfactt"
  )
  # table 3.1 TFR 0-4 years prior to survey for total
  expect_equal(round(as.numeric(do.call(calc_tfr, calc_args)$tfr), 1), 5.0)
  # table 3.2/3.3 ASFR 0-4 years prior to survey
  expect_equal(round(1000*as.numeric(do.call(calc_asfr, calc_args)$asfr)),
               c(69, 183, 240, 236, 157, 82, 25))

  # table 3.1 TFR 0-4 years prior to survey for urban/rural residences
  calc_args[["by"]] <- ~v102
  calc_args[["awfact"]] <- "awfactu"
  expect_equal(round(as.numeric(do.call(calc_tfr, calc_args)$tfr), 1), c(4.1, 5.6))
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
    
