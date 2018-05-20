#' Calculate age-specific fertility rate (ASFR) and total fertility rate (TFR)
#'
#' @aliases calc_tfr
#'
#' @param bvars variables giving child dates of birth.
#' @param agegr break points for age groups in completed years of age.
#' @param period break points for calendar year periods (possibly non-integer).
#' @param tips break points for TIme Preceding Survey.
#'
#' @return A `data.frame` consisting of estimates and standard errors. The column
#' consisting of point estimates (`asfr` or `tfr`) is an object of class
#' [survey::svystat], and thus the full covariance matrix of the estimates
#' can be retreived by `vcov(val$asfr)`.
#'
#' @details
#' Events and person-years are calculated using normalized weights. Unweighted
#' aggregations may be output by specifying `weights=NULL` (default) or
#' `weights=~1`.
#' 
#' The assumption is that all dates in the data are specified in the same
#' format, typically century month code (CMC). The `period` argument is
#' specified in calendar years (possibly non-integer).
#'
#' Default values for `agegr`, `period`, and `tips` parameters returns 
#' age-specific fertility rates over the three-years preceding the survey,
#' the standard fertility indicator produced in DHS reports.
#'
#' The return is a `data.frame`, but 
#'
#' @seealso [demog_pyears()]
#' 
#' @examples
#' data(zzir)
#' 
#' ## Replicate DHS Table 5.1
#' ## ASFR and TFR in 3 years preceding survey by residence
#' calc_asfr(zzir, ~1, tips=c(0, 3)) 
#' reshape2::dcast(calc_asfr(zzir, ~v025, tips=c(0, 3)), agegr ~ v025, value.var = "asfr")
#' calc_tfr(zzir, ~v025)
#' calc_tfr(zzir, ~1)
#' 
#' ## Replicate DHS Table 5.2
#' ## TFR by resdience, region, education, and wealth quintile
#' calc_tfr(zzir, ~v102)  # residence
#' calc_tfr(zzir, ~v101)  # region
#' calc_tfr(zzir, ~v106)  # education
#' calc_tfr(zzir, ~v190)  # wealth
#' calc_tfr(zzir)         # total
#' 
#' ## Calculate annual TFR estimates for 10 years preceding survey
#' tfr_ann <- calc_tfr(zzir, tips=0:9)
#' 
#' ## Sample covariance of annual TFR estimates arising from complex survey design
#' cov2cor(vcov(tfr_ann$tfr)) 
#' 
#' ## Alternately, calculate TFR estimates by calendar year
#' tfr_cal <- calc_tfr(zzir, period = 2004:2015, tips=NULL)
#' tfr_cal
#' 
#' ## sample covariance of annual TFR estimates arising from complex survey design
#' cov2cor(vcov(tfr_cal$tfr))
#' 
#' ## Generate estimates split by period and TIPS 
#' calc_tfr(zzir, period = c(2010, 2013, 2015), tips=0:5)
#' 
#' ## ASFR estimates by birth cohort
#' asfr_coh <- calc_asfr(zzir, cohort=c(1980, 1985, 1990, 1995), tips=NULL)
#' reshape2::dcast(asfr_coh, agegr ~ cohort, value.var = "asfr")
#' 
#' @importFrom survival tmerge
#' @import stats
#' @export
#' @md
calc_asfr <- function(data,
                      by = NULL,
                      agegr = 3:10*5,
                      period = NULL,
                      cohort = NULL,
                      tips = c(0, 3),
                      clusters=~v021,
                      strata=~v024+v025,
                      id="caseid",
                      dob="v011",
                      intv = "v008",
                      weight= "v005",
                      bvars = grep("^b3\\_[0-9]*", names(data), value=TRUE),
                      birth_displace = 1e-6,
                      origin=1900,
                      scale=12){
  
  data$id <- data[[id]]
  data$dob <- data[[dob]]
  data$intv <- data[[intv]]
  data$weights <- data[[weight]] / mean(data[[weight]])

  if(is.null(by))
    by <- ~1
  
  vars <- unique(unlist(lapply(c(by, strata, clusters), all.vars)))
  f <- formula(paste("~", paste(vars, collapse = "+")))
  mf <- model.frame(formula = f, data = data, na.action = na.pass,
                    id = id, weights = weights, dob = dob, intv = intv)

  births <- reshape(model.frame(paste("~", paste(bvars, collapse="+")),
                                data, na.action=na.pass, id=id),
                    idvar="(id)", timevar="bidx",
                    varying=bvars, v.names="bcmc", direction="long")
  births <- births[!is.na(births$bcmc), ]
  births$bcmc <- births$bcmc + births$bidx * birth_displace
  
  epis <- tmerge(mf, mf, id=`(id)`, tstart=`(dob)`, tstop=`(intv)`)
  epis <- tmerge(epis, births, id=`(id)`, birth = event(bcmc))
  
  aggr <- demog_pyears(f, epis, period=period, agegr=agegr, cohort=cohort, tips=tips,
                       event="birth", weights="(weights)", origin=origin, scale=scale)
  
  des <- survey::svydesign(ids=clusters, strata=strata, data=aggr$data, weights=~1)
  class(des) <- c("svypyears", class(des))

  ## construct interaction of all factor levels that appear
  byvar <- intersect(c(all.vars(by), "agegr", "period", "cohort", "tips"),
                     names(des$variables))
  byf <- do.call("interaction", c(des$variables[byvar], drop=TRUE))
  des <- update(des, byf = byf)

  ## fit model
  mod <- survey::svyglm(event ~ -1 + byf + offset(log(pyears)),
                        des, family=quasipoisson)
  
  ## prediction for all factor levels that appear
  val <- data.frame(des$variables[c(byvar, "byf")])[!duplicated(byf),]
  val <- val[order(val$byf), ]
  val$pyears <- 1
  
  val$asfr <- predict(mod, val, type="response", vcov=TRUE)
  val$se_asfr <- sqrt(diag(vcov(val$asfr)))
  val[c("byf", "pyears")] <- NULL

  rownames(val) <- NULL
  
  return(val)
}

#' @export
calc_tfr <- function(data,
                     by = NULL,
                     agegr = 3:10*5,
                     period = NULL,
                     cohort = NULL,
                     tips = c(0, 3),
                     clusters=~v021,
                     strata=~v024+v025,
                     id="caseid",
                     dob="v011",
                     intv = "v008",
                     weight= "v005",
                     bvars = grep("^b3\\_[0-9]*", names(data), value=TRUE),
                     birth_displace = 1e-6,
                     origin=1900,
                     scale=12){
  
  g <- match.call()
  g[[1]] <- quote(calc_asfr)
  asfr <- eval(g)

  mf <- asfr[setdiff(names(asfr), c("asfr", "se_asfr"))]
  dfmm <- .mm_aggr(mf, agegr)
  mm <- dfmm$mm

  mu <- drop(asfr$asfr %*% mm)
  v <- t(mm) %*% vcov(asfr$asfr) %*% mm
  class(mu) <- "svystat"
  attr(mu, "var") <-  v
  attr(mu, "statistic") <- "tfr"

  val <- dfmm$df
  val$tfr <- mu
  val$se_tfr <- sqrt(diag(v))

  rownames(val) <- NULL
  
  val
}
