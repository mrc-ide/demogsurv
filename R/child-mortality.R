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
#' @param weights Formula or vector specifying sampling weights.
#' @param dob Variable name for date of birth (character string).
#' @param dod Variable name for date of death (character string).
#' @param death Variable name for event variable (character string).
#' @param intv Variable name for interview date (character string).
#' @param varmethod Method for variance calculation. Currently "lin" for Taylor
#'   linearisation or "jk1" for (unstratified) jackknife.
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

  data$tstop <- ifelse(data[[death]], data[[dod]], data[[intv]])

  data$dob <- data[[dob]]
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

  ## All values of factor combinations that appear
  byvar <- intersect(c(all.vars(by), "agegr", "period", "cohort", "tips"),
                     names(aggr))
  aggr$byf <- interaction(aggr[byvar], drop=TRUE)
  
  ## prediction for all factor levels that appear
  pred <- data.frame(aggr[c(byvar, "byf")])[!duplicated(aggr$byf),]
  pred <- pred[order(pred$byf), ]
  pred$pyears <- 1
  
  ## Matrix to aggregate piecewise-constant rates to cumulative hazards
  dfmm <- .mm_aggr(pred[byvar], agegr)
  mm <- dfmm$mm

  if(varmethod == "lin") {
    des <- survey::svydesign(ids=clusters, strata=strata, data=aggr, weights=~1)

    ## fit model
    f <- if(length(levels(aggr$byf)) == 1)
           event ~ offset(log(pyears))
         else
           event ~ -1 + byf + offset(log(pyears))

    mod <- survey::svyglm(f, des, family=quasipoisson)

    mx <- predict(mod, pred, type="response", vcov=TRUE)

    lest <- drop(mx %*% mm)
    lv <- t(mm) %*% vcov(mx) %*% mm
    est <- 
    dF <- diag(exp(-lest), length(lest))
    v <- dF %*% lv %*% dF

    estdf <- data.frame(est  = 1 - exp(-lest),
                        se   = sqrt(diag(v)),
                        ci_l = 1 - exp(-(lest - qnorm(0.975)*diag(lv))),
                        ci_u = 1 - exp(-(lest + qnorm(0.975)*diag(lv))))
    attr(estdf, "var") <- v
    
  } else if(varmethod %in% c("jkn", "jk1")) {

    ## Convert to array with events and PYs for each cluster
    ## reshape2::acast is MUCH faster than stats::reshape
    events_clust <- reshape2::acast(aggr, update(clusters, byf ~ .), value.var="event")
    pyears_clust <- reshape2::acast(aggr, update(clusters, byf ~ .), value.var="pyears")

    if(varmethod == "jkn"){
      aggr$strataid <- as.integer(interaction(aggr[all.vars(strata)], drop=TRUE))
      strataid <- drop(reshape2::acast(unique(aggr[c(all.vars(clusters), "strataid")]),
                                       update(clusters,  1 ~ .), value.var="strataid"))
    } else
      strataid <- NULL

    estdf <- jackknife(events_clust, pyears_clust, strataid, t(dfmm$mm), function(x) 1 - exp(-x))
  } else
    stop(paste0("varmethod = \"", varmethod, "\" is not recognized."))

  val <- data.frame(dfmm$df, estdf)
  attr(val, "var") <- vcov(estdf)

  rownames(val) <- NULL
  
  val
}
