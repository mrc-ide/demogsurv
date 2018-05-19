context("Age-specific fertility rates and TFR")

library(hhsurveydata)
library(rdhs)

irfl_zip <- tempfile()
download.file("https://dhsprogram.com/customcf/legacy/data/sample_download_dataset.cfm?Filename=ZZIR62FL.ZIP&Tp=1&Ctry_Code=zz&survey_id=0&doctype=dhs", irfl_zip)
ir <- rdhs::read_dhs_flat(irfl_zip)

test_that("fertility calculations match DHS tables", {
  expect_equal(round(as.numeric(calc_tfr(ir)$tfr), 1), 4.7)
  expect_equal(round(as.numeric(calc_tfr(ir, ~v025)$tfr), 1), c(3.5, 5.7))
  expect_equal(round(1000*as.numeric(calc_asfr(ir)$asfr)), c(119, 207, 216, 188, 125, 60, 28))
})
 
