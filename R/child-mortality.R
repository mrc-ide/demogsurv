#' Calculate the probability of dying between age x and x+n (nqx)
#'
#' Default arguments are configured to calculate under 5 mortality
#' from a DHS Births Recode file.
#'
#' @param data A dataset (data.frame), for example a DHS births recode (BR) dataset.
#' @param by A formula specifying factor variables by which to stratify analysis.
#' @param agegr Numeric vector defining ages *in years* for splits.
#' @param period Numeric vector defining calendar periods to stratify analysis, use `NULL` for no periods.
#' @param cohort Numeric vector defining birth cohorts to stratify analysis, use `NULL` for no cohort stratification.
#' @param tips Break points for TIme Preceding Survey.
#' @param clusters Formula or data frame specifying cluster ids from largest level to smallest level, ‘~0’ or ‘~1’ is a formula for no clusters.
#' @param strata Formula or vector specifying strata, use ‘NULL’ for no strata.
#' @param weight Formula or vector specifying sampling weights.
#' @param dob Variable name for date of birth (character string).
#' @param dod Variable name for date of death (character string).
#' @param death Variable name for event variable (character string).
#' @param intv Variable name for interview date (character string).
#' @param varmethod Method for variance calculation. Currently "lin" for Taylor
#'   linearisation or "jk1" for unstratified jackknife, or "jkn", for stratified
#'   jackknife.
#' @param origin Origin year for date arguments. 1900 for CMC inputs.
#' @param scale Scale for dates inputs to calendar years. 12 for CMC inputs.
#'
#' @examples
#'
#' data(zzbr)
#' zzbr$death <- zzbr$b5 == "no"  # b5: child still alive ("yes"/"no")
#' zzbr$dod <- zzbr$b3 + zzbr$b7 + 0.5
#'
#' ## Calculate 5q0 from birth history dataset.
#' ## Note this does NOT exactly match DHS calculation.
#' ## See calc_dhs_u5mr().
#' u5mr <- calc_nqx(zzbr)
#' u5mr
#'
#' ## Retrieve sample covariance and correlation
#' vcov(u5mr)  # sample covariance
#' cov2cor(vcov(u5mr))  # sample correlation
#'
#' ## 5q0 by sociodemographic characteristics
#' calc_nqx(zzbr, by=~v102) # by urban/rural residence
#' calc_nqx(zzbr, by=~v190, tips=c(0, 10)) # by wealth quintile, 0-9 years before
#' calc_nqx(zzbr, by=~v101+v102, tips=c(0, 10)) # by region and residence
#'
#' ## Compare unstratified standard error estiamtes for linearization and jackknife
#' calc_nqx(zzbr, varmethod = "lin")  # unstratified design
#' calc_nqx(zzbr, strata=NULL, varmethod = "lin")  # unstratified design
#' calc_nqx(zzbr, strata=NULL, varmethod = "jk1")  # unstratififed jackknife
#' calc_nqx(zzbr, varmethod = "jkn")  # stratififed jackknife
#'
#' ## Calculate various child mortality indicators (neonatal, infant, etc.)
#' calc_nqx(zzbr, agegr=c(0, 1)/12)  # neonatal
#' calc_nqx(zzbr, agegr=c(1, 3, 5, 12)/12) # postneonatal
#' calc_nqx(zzbr, agegr=c(0, 1, 3, 5, 12)/12) # infant (1q0)
#' calc_nqx(zzbr, agegr=c(12, 24, 36, 48, 60)/12) # child (4q1)
#' calc_nqx(zzbr, agegr=c(0, 1, 3, 5, 12, 24, 36, 48, 60)/12) # u5mr (5q0)
#'
#' ## Calculate annaul 5q0 by calendar year
#' calc_nqx(zzbr, period=2005:2015, tips=NULL)
#'
#' @export
#' @md
calc_nqx <- function(data,
                     by = NULL,
                     agegr = c(0, 1, 3, 5, 12, 24, 36, 48, 60)/12,
                     period = NULL,
                     cohort = NULL,
                     tips = c(0, 5, 10, 15),
                     clusters=~v021,
                     strata=~v024+v025,
                     weight= "v005",
                     dob="b3",
                     dod="dod",
                     death="death",
                     intv = "v008",
                     varmethod = "lin",
                     origin=1900,
                     scale=12){
  g <- match.call()
  g[[1]] <- quote(calc_nmx)
  if(varmethod %in% c("jk1", "jkn"))
    g$clustcounts <- TRUE
  mx <- eval(g, envir=parent.frame())

  mf <- mx[setdiff(names(mx), c("mx", "se_mx"))]
  dfmm <- .mm_aggr(mf, agegr)

  val <- dfmm$df

  if(varmethod == "lin") {
    mm <- dfmm$mm

    lest <- drop(mx$mx %*% mm)
    lv <- t(mm) %*% attr(mx, "var") %*% mm

    dF <- diag(exp(-lest), length(lest))
    v <- dF %*% lv %*% dF

    val$qx <- 1 - exp(-lest)
    val$se_qx <- sqrt(diag(v))
    val$ci_l <- 1 - exp(-(lest - qnorm(0.975)*sqrt(diag(lv))))
    val$ci_u <- 1 - exp(-(lest + qnorm(0.975)*sqrt(diag(lv))))
    attr(val, "var") <- v

  } else if(varmethod %in% c("jkn", "jk1")) {

    estdf <- jackknife(attr(mx, "events_clust"),
                       attr(mx, "pyears_clust"),
                       attr(mx, "strataid"),
                       L = t(dfmm$mm),
                       fn = function(x) 1 - exp(-x))

    val$qx <- estdf$est
    val$se_qx <- estdf$se
    attr(val, "var") <- vcov(estdf)
  } else
    stop(paste0("varmethod = \"", varmethod, "\" is not recognized."))

  # to keep existing column names
  colnames(val)[colnames(val) == 'qx'] <- 'est'
  colnames(val)[colnames(val) == 'se_qx'] <- 'se'


  rownames(val) <- NULL

  return(val)
}

#' Calculate the mortality rate between age x and x+n (nmx)
#'
#' Default arguments are configured to calculate under 5 mortality rate (nmx)
#' from a DHS Births Recode file.
#'
#' @inheritParams calc_nqx
#' @param counts Whether to include counts of deaths & person-years ('pys')
#'   in the returned `data.frame`. Default is 'FALSE'.
#' @param clustcounts Whether to return additional attributes storing cluster
#'   specific counts of deaths `attr(val, 'events_clust')`, person-years
#'   `attr(val, 'pyears_clust')` & number of clusters in each strata
#'   `attr(val, 'strataid')`. Only applicable when using jacknife `varmethod`
#'   'jk1' or 'jkn'. 'strataid' is only included for 'jkn' `varmethod`. Default
#'   is 'FALSE'.
#'
#' @examples
#'
#' # Calculate age specific nmx from birth history dataset.
#' data(zzbr)
#' zzbr$death <- zzbr$b5 == "no"  # b5: child still alive ("yes"/"no")
#' zzbr$dod <- zzbr$b3 + zzbr$b7 + 0.5
#' child_mx <- calc_nmx(zzbr)
#'
#' # Calculate age specific nmx from individual recode.
#' data(zzir)
#' zzsib <- reshape_sib_data(zzir)
#' zzsib$death <- factor(zzsib$mm2, c("dead", "alive")) == "dead"
#' zzsib$sex <- factor(zzsib$mm1, c("female", "male"))  # drop mm2 = 3: "missing"
#' adult_mx <- calc_nmx(zzsib, by=~sex, agegr=seq(15, 50, 5), tips=c(0, 7), dob="mm4", dod="mm8")
#'
#' @export
calc_nmx <- function(data,
                     by = NULL,
                     agegr = c(0, 1, 3, 5, 12, 24, 36, 48, 60)/12,
                     period = NULL,
                     cohort = NULL,
                     tips = c(0, 5, 10, 15),
                     clusters=~v021,
                     strata=~v024+v025,
                     weight= "v005",
                     dob="b3",
                     dod="dod",
                     death="death",
                     intv = "v008",
                     varmethod = "lin",
                     origin=1900,
                     scale=12,
                     counts=FALSE,
                     clustcounts = FALSE){

  data$tstop <- ifelse(data[[death]], data[[dod]], data[[intv]])

  data$dob <- data[[dob]]
  data$death <- data[[death]]
  data$intv <- data[[intv]]
  data$weights <- data[[weight]] / mean(data[[weight]])

  if(is.null(by))
    by <- ~1

  vars <- unique(unlist(lapply(c(by, strata, clusters), all.vars)))
  f <- formula(paste("~", paste(vars, collapse = "+")))
  mf <- model.frame(f, data=data, na.action=na.pass, death=death,
                    weights=weights, dob=dob, intv=intv, tstop=tstop)

  aggr <- demog_pyears(f, mf, period=period, agegr=agegr, tips=tips, event="(death)",
                       tstart="(dob)", tstop="(tstop)", weights="(weights)",
                       origin=origin, scale=scale)$data

  pred <- demog_pred_rate(aggr, by=by, clusters=clusters, strata=strata,
                          varmethod=varmethod, counts=counts, clustcounts=clustcounts)
  colnames(pred)[colnames(pred) == 'rate'] <- 'mx'
  colnames(pred)[colnames(pred) == 'se_rate'] <- 'se_mx'

  return(pred)
}
