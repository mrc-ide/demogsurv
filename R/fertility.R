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
#' data(zzir62fl)
#' ir <- zzir62fl
#' 
#' ## Replicate DHS Table 5.1
#' ## ASFR and TFR in 3 years preceding survey by residence
#' calc_asfr(ir, ~1, tips=c(0, 3)) 
#' dcast(calc_asfr(ir, ~v025, tips=c(0, 3)), agegr ~ v025, value.var = "asfr")
#' calc_tfr(ir, ~v025)
#' calc_tfr(ir, ~1)
#' 
#' ## Replicate DHS Table 5.2
#' ## TFR by resdience, region, education, and wealth quintile
#' calc_tfr(ir, ~v102)  # residence
#' calc_tfr(ir, ~v101)  # region
#' calc_tfr(ir, ~v106)  # education
#' calc_tfr(ir, ~v190)  # wealth
#' calc_tfr(ir)         # total
#' 
#' ## Calculate annual TFR estimates for 10 years preceding survey
#' tfr_ann <- calc_tfr(ir, tips=0:9)
#' 
#' ## Sample covariance of annual TFR estimates arising from complex survey design
#' cov2cor(vcov(tfr_ann$tfr)) 
#' 
#' ## Alternately, calculate TFR estimates by calendar year
#' tfr_cal <- calc_tfr(ir, period = 2004:2015, tips=NULL)
#' tfr_cal
#' 
#' ## sample covariance of annual TFR estimates arising from complex survey design
#' cov2cor(vcov(tfr_cal$tfr))
#' 
#' ## Generate estimates split by period and TIPS 
#' calc_tfr(ir, period = c(2010, 2013, 2015), tips=0:5)
#' 
#' ## ASFR estiamtes by birth cohort
#' asfr_coh <- calc_asfr(ir, cohort=c(1980, 1985, 1990, 1995), tips=NULL)
#' reshape2::dcast(asfr_coh, agegr ~ cohort, value.var = "asfr")
#' 
#' @importFrom survival tmerge
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
  f <- survey::make.formula(vars)
  mf <- model.frame(f, data=data, na.action=na.pass,
                    id=id, weights=weights, dob=dob, intv=intv)
  
  births <- reshape(model.frame(survey::make.formula(bvars), data, na.action=na.pass, id=id),
                    idvar="(id)", timevar="bidx",
                    varying=bvars, v.names="bcmc", direction="long")
  births <- subset(births, !is.na(bcmc))
  births$bcmc <- births$bcmc + births$bidx * birth_displace
  
  epis <- tmerge(mf, mf, id=`(id)`, tstart=`(dob)`, tstop=`(intv)`)
  epis <- tmerge(epis, births, id=`(id)`, birth = event(bcmc))
  
  aggr <- demog_pyears(f, epis, period=period, agegr=agegr, cohort=cohort, tips=tips,
                       event="birth", weights="(weights)", origin=origin, scale=scale)
  
  des <- survey::svydesign(ids=clusters, strata=strata, data=aggr$data, weights=~1)
  class(des) <- c("svypyears", class(des))

  f <- by
  byvars <- all.vars(by)

  if(exists("agegr", des$variables)) {
    byvars <- c(byvars, "agegr")
    if(length(levels(des$variables$agegr)) > 1)
      if(f[[2]] == 1)
        f <- update(f, ~agegr)
      else
        f <- update(f, ~agegr:.)
  }

  if(exists("period", des$variables)) {
    byvars <- c(byvars, "period")
    if(length(levels(des$variables$period)) > 1)
      if(f[[2]] == 1)
        f <- update(f, ~period)
      else
        f <- update(f, ~period:.)
  }

  if(exists("cohort", des$variables)) {
    byvars <- c(byvars, "cohort")
    if(length(levels(des$variables$cohort)) > 1)
      if(f[[2]] == 1)
        f <- update(f, ~cohort)
      else
        f <- update(f, ~cohort:.)
  }

  if(exists("tips", des$variables)) {
    byvars <- c(byvars, "tips")
    if(length(levels(des$variables$tips)) > 1)
      if(f[[2]] == 1)
        f <- update(f, ~tips)
      else
        f <- update(f, ~tips:.)
  }
  
  f <- update(f, event~-1+.+offset(log(pyears)))
  
  mod <- survey::svyglm(f, des, family=quasipoisson)

  ## All values of factor combinations that appear
  pred <- unique(model.frame(des)[byvars])
  pred$pyears <- 1
  
  pred$asfr <- predict(mod, pred, type="response", vcov=TRUE)
  pred$se_asfr <- sqrt(diag(vcov(pred$asfr)))
  pred$pyears <- NULL
  
  return(pred)
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

  vars <- setdiff(names(asfr), c("agegr", "asfr", "se_asfr"))

  mf <- asfr[vars]
  mf <- mf[vapply(lapply(mf, unique), length, numeric(1)) > 1]

  if(length(mf))
    f <- paste("~ -1", paste(names(mf), collapse=":"), sep="+")
  else
    f <- "~1"

  ## Drop factors with single level
  nlevels <- vapply(lapply(mf, levels), length, numeric(1))
  
  mm <- model.matrix(formula(f), mf[nlevels > 1])

  ## Drop columns with no predictions
  mm <- mm[ , colSums(mm) > 0]
  
  mu <- 5 * t(mm) %*% asfr$asfr
  v <- 25 * t(mm) %*% vcov(asfr$asfr) %*% mm
  class(mu) <- "svystat"
  attr(mu, "var") <-  v
  attr(mu, "statistic") <- "tfr"

  tfr <- if(length(vars))
           ## use asfr because we might have dropped vars from mf above
           unique(asfr[vars])
         else
           data.frame(total = "total")
  
  tfr$tfr <- mu
  tfr$se_tfr <- sqrt(diag(v))

  tfr
}


