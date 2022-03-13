#' Calculate age-specific fertility rate (ASFR) and total fertility rate (TFR)
#'
#' @aliases calc_tfr
#'
#' @inheritParams calc_nqx
#' @param data A dataset (data.frame), for example a DHS individual recode (IR) dataset.
#' @param id Variable name for identifying each individual respondent (character string).
#' @param dob Variable name for date of birth of each individual (character string).
#' @param bvars Names of variables giving child dates of birth. If `bhdata` is
#'   provided, then length(bvars) must equal 1.
#' @param bhdata A birth history dataset (`data.frame`) with child dates of birth
#'   in long format, for example a DHS births recode (BR) dataset.
#' @param birth_displace Numeric value to displace multiple births date of birth
#'   by. Default is '1e-6'.
#' @param counts Whether to include counts of births & person-years ('pys')
#'   in the returned `data.frame`. Default is 'FALSE'.
#' @param clustcounts Whether to return additional attributes storing cluster
#'   specific counts of births `attr(val, 'events_clust')`, person-years
#'   `attr(val, 'pyears_clust')` & number of clusters in each strata
#'   `attr(val, 'strataid')`. Only applicable when using jacknife `varmethod`
#'   'jk1' or 'jkn'. 'strataid' is only included for 'jkn' `varmethod`. Default
#'   is 'FALSE'.
#'
#' @return A `data.frame` consisting of estimates and standard errors. The full
#' covariance matrix of the estimates can be retrieved by `vcov(val)`.
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
#' cov2cor(vcov(tfr_ann))
#'
#' ## Alternately, calculate TFR estimates by calendar year
#' tfr_cal <- calc_tfr(zzir, period = 2004:2015, tips=NULL)
#' tfr_cal
#'
#' ## sample covariance of annual TFR estimates arising from complex survey design
#' ## Generate estimates split by period and TIPS
#' cov2cor(vcov(tfr_cal))
#'
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
                      varmethod = "lin",
                      bvars = grep("^b3\\_[0-9]*", names(data), value=TRUE),
                      birth_displace = 1e-6,
                      origin=1900,
                      scale=12,
                      bhdata = NULL,
                      counts=FALSE,
                      clustcounts = FALSE){
  
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

  if(is.null(bhdata)) {
    births <- model.frame(paste("~", paste(bvars, collapse="+")),
                          data, na.action=na.pass, id=id)
    births <- reshape(births,
                      idvar="(id)", timevar="bidx",
                      varying=bvars, v.names="bcmc", direction="long")
  } else {
    if(length(bvars) > 1)
      stop("If `bhdata' is provided, bvars must provide a single variable name (length(bvars) = 1)")
    
    bhdata$id <- bhdata[[id]]
    bhdata$bcmc <- bhdata[[bvars]]
    births <- model.frame(~bcmc, data = bhdata, id = id)    
    births$bidx <- ave(births$bcmc, births[["(id)"]], FUN = seq_along)
  }
  births <- births[!is.na(births$bcmc), ]
  births$bcmc <- births$bcmc + births$bidx * birth_displace

  ## Rename for tmerge()
  names(mf)[names(mf) == "(id)"] <- "id_"
  names(births)[names(births) == "(id)"] <- "id_"

  epis <- tmerge(mf, mf, id=id_, tstart=`(dob)`, tstop=`(intv)`)
  epis <- tmerge(epis, births, id=id_, birth = event(bcmc))
  
  aggr <- demog_pyears(f, epis, period=period, agegr=agegr, cohort=cohort, tips=tips,
                       event="birth", weights="(weights)", origin=origin, scale=scale)$data
  
  ## construct interaction of all factor levels that appear
  byvar <- intersect(c(all.vars(by), "agegr", "period", "cohort", "tips"),
                     names(aggr))
  aggr$byf <- interaction(aggr[byvar], drop=TRUE)
  
  ## prediction for all factor levels that appear
  pred <- data.frame(aggr[c(byvar, "byf")])[!duplicated(aggr$byf),]
  pred <- pred[order(pred$byf), ]
  
  if(counts || varmethod == "none"){
    mc <- model.matrix(~-1+byf, aggr)
    clong <- aggr[c("event", "pyears")]
    pred[c("births", "pys")] <- t(mc) %*% as.matrix(clong)
  }

  if(varmethod == "none") {

    pred$asfr <- pred$births / pred$pys
    pred$byf <- NULL
    if(!counts)
      pred[c("births", "pys")] <- NULL

  } else if(varmethod == "lin") {

    des <- survey::svydesign(ids=clusters, strata=strata, data=aggr, weights=~1)
    class(des) <- c("svypyears", class(des))
    
    ## fit model
    f <- if(length(levels(aggr$byf)) == 1)
           event ~ offset(log(pyears))
         else
           event ~ -1 + byf + offset(log(pyears))

    mod <- survey::svyglm(f, des, family=quasipoisson)
    
    ## prediction for all factor levels that appear
    pred$pyears <- 1
  
    asfr <- predict(mod, pred, type="response", vcov=TRUE)
    v <- vcov(asfr)
    dimnames(v) <- list(pred$byf, pred$byf)

    pred$asfr <- as.numeric(asfr)
    pred$se_asfr <- sqrt(diag(v))
    pred[c("byf", "pyears")] <- NULL
    attr(pred, "var") <- v
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
    
    estdf <- jackknife(events_clust, pyears_clust, strataid)

    pred$asfr <- estdf$est
    pred$se_asfr <- estdf$se
    attr(pred, "var") <- vcov(estdf)
    pred$byf <- NULL
    if(clustcounts){
      attr(pred, "events_clust") <- events_clust
      attr(pred, "pyears_clust") <- pyears_clust
      attr(pred, "strataid") <- strataid
    }
  } else
    stop(paste0("varmethod = \"", varmethod, "\" is not recognized."))


  rownames(pred) <- NULL
  
  return(pred)
}

#' @export
calc_tfr <- function(data,
                     by = NULL,
                     agegr = 3:10*5,
                     period = NULL,
                     cohort = NULL,
                     tips = c(0, 3),
                     clusters = ~v021,
                     strata = ~v024+v025,
                     id = "caseid",
                     dob = "v011",
                     intv = "v008",
                     weight = "v005",
                     varmethod = "lin",
                     bvars = grep("^b3\\_[0-9]*", names(data), value=TRUE),
                     birth_displace = 1e-6,
                     origin = 1900,
                     scale = 12,
                     bhdata = NULL){
  
  g <- match.call()
  g[[1]] <- quote(calc_asfr)
  g$data <- data
  g$bhdata <- bhdata
  if(varmethod %in% c("jk1", "jkn"))
    g$clustcounts <- TRUE
  asfr <- eval(g, envir=parent.frame())

  mf <- asfr[setdiff(names(asfr), c("asfr", "se_asfr"))]
  dfmm <- .mm_aggr(mf, agegr)
      
  val <- dfmm$df

  if(varmethod == "lin") {
    mm <- dfmm$mm
    
    mu <- drop(asfr$asfr %*% mm)
    v <- t(mm) %*% attr(asfr, "var") %*% mm

    val$tfr <- mu
    val$se_tfr <- sqrt(diag(v))
    attr(val, "var") <- v
  } else if(varmethod %in% c("jkn", "jk1")) {
    estdf <- jackknife(attr(asfr, "events_clust"),
                       attr(asfr, "pyears_clust"),
                       attr(asfr, "strataid"),
                       t(dfmm$mm))

    val$tfr <- estdf$est
    val$se_tfr <- estdf$se
    attr(val, "var") <- vcov(estdf)
  } else if(varmethod  == "none") {
    val$tfr <- drop(asfr$asfr %*% dfmm$mm)
  } else {
    stop(paste0("varmethod = \"", varmethod, "\" is not recognized."))
  }

  rownames(val) <- NULL
  
  val
}
