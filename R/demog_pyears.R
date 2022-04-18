.epis_labels <- function(x){
  if("labels" %in% attributes(x))
    return(labels(x)[-length(x)])
  lower <- x[-length(x)]
  upper <- x[-1]-1
  val <- ifelse(lower==upper, lower, paste0(lower, "-", upper))
  gsub("-Inf", "+", val)
}

#' Events and person-years from episode data for demographic analysis
#'
#' This is a wrapper for the \code{\link[survival]{pyears}} function
#' in the \code{survival} package with convenient stratifications for
#' demographic analyses.
#'
#' Note that \code{event} must be a binary variable per the internals
#' of the \code{\link[survival]{pyears}()} function. The function could
#' be updated to work around this stipulation.
#'
#' @inheritParams survival::pyears
#' @inheritParams calc_nmx
#' @param tstart Variable name for the start of follow up time, example is date
#'   of birth. Default is 'tstart'.
#' @param tstop Variable name for the end of follow up time, examples include
#'   interview date or date of death. Default is 'tend'.
#' @param event Variable name for the event indicator, example is
#'   birth or death. Default is 'event'.
#'
#' @seealso \code{\link[survival]{pyears}}, \code{\link[survival]{tcut}}
#'
#' @importFrom survival Surv
#' @importFrom survival tcut
#' @importFrom survival pyears
#' @export
demog_pyears <- function(formula, data, period=NULL, agegr=NULL, cohort=NULL, tips=NULL, origin=1900, scale=12,
                         dob="(dob)", intv="(intv)", tstart="tstart", tstop="tstop", event="event", weights=NULL){

  if(!is.null(period)){
    data$period <- tcut(data[[tstart]], (period-origin)*scale, .epis_labels(period))
    formula <- update(formula, ~. + period)
  }

  if(!is.null(agegr)){
    data$agegr <- tcut(data[[tstart]] - data[[dob]], agegr*scale, .epis_labels(agegr))
    formula <- update(formula, ~. + agegr)
  }

  if(!is.null(cohort)){
    ## cohort is not time updating, so use cut rather than tcut
    data$cohort <- cut(data[[dob]], (cohort-origin)*scale, .epis_labels(cohort),
                       include.lowest=TRUE, right=FALSE)
    formula <- update(formula, ~. + cohort)
  }
  
  if(!is.null(tips)){
    data$tips <- tcut(data[[tstart]] - data[[intv]], -rev(tips)*scale, rev(.epis_labels(tips)))
    formula <- update(formula, ~. + tips)
  }

  formula <- update(formula, bquote(Surv(.(as.name(tstop)) - .(as.name(tstart)), .(as.name(event))) ~ .))

  ## TODO: this is a workaround, figure out a better way with bquote() or something.
  if(!is.null(weights))
    data$weights <- data[[weights]]
  else
    data$weights <- 1
  
  pyears(formula, data, scale=scale, data.frame=TRUE, weights=weights)
}

#' Internal function to predict an estimated rate and standard error
#'
#' @param aggr output from `demogsurv::demog_pyears`
#' @inheritParams calc_nmx
demog_pred_rate <- function(aggr,
                            by,
                            clusters,
                            strata,
                            varmethod,
                            counts,
                            clustcounts) {

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
    pred[c("event", "pys")] <- t(mc) %*% as.matrix(clong)
  }

  if(varmethod == "none") {

    pred$asfr <- pred$event / pred$pys
    pred$byf <- NULL
    if(!counts)
      pred[c("event", "pys")] <- NULL

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

    rate <- predict(mod, pred, type="response", vcov=TRUE)
    v <- vcov(rate)
    dimnames(v) <- list(pred$byf, pred$byf)

    pred$rate <- as.numeric(rate)
    pred$se_rate <- sqrt(diag(v))
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

    pred$rate <- estdf$est
    pred$se_rate <- estdf$se
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
