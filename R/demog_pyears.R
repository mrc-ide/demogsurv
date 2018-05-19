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
