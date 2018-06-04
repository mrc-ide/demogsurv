vcov.default <- function(x) attr(x, "var")

#' Construct a model matrix for aggregating over age groups
#' @param mf Model frame for predicted rates
#' @inheritParams calc_asfr
.mm_aggr <- function(mf, agegr){

  if(any(!vapply(mf, is.factor, logical(1)))){
    v <- !vapply(mf, is.factor, logical(1))
    stop(paste("Not all 'by' variables are factors:",
               paste(names(v)[v], collapse = ", ")))
  }

  if(!"agegr" %in% names(mf))
    stop("'agegr' variable not in mf")
               
  ## Calculate duration of each age group
  dur <- setNames(diff(agegr), levels(mf$agegr))
  dur <- dur[mf$agegr]

  mf$agegr <- NULL

  if(length(mf))
    mf$byf <- do.call(interaction, c(mf, drop=TRUE))

  if(!length(mf) || length(levels(mf$byf)) == 1)
    mf$byf <- 1

  mm <- model.matrix(~-1+byf, mf) * dur

  df <- mf[!duplicated(mf$byf), , drop=FALSE]
  df <- df[order(df$byf), , drop=FALSE]
  df["byf"] <- NULL
  rownames(df) <- NULL

  list(df = df, mm = mm)
}


#' Jackknife covariance calculation
#'
#' Calculate the covariance matrix for a vector of estimates of the form \code{fn(L * x/n)}
#' using unstratified (JK1) or stratified (JKn) jackknife calculation removing a
#' single cluster at a time. The calculation assumes infinite population sampling.
#'
#' If \code{strataid} is provided, then the stratified (JKn) covariance is calculated, while
#' if \code{strataid = NULL} then the unstratified (JK1) covariance is calculated. The
#' latter corresponds to the unstratified jackknife covariance reported in DHS survey
#' reports. The calculations are equivalent for \code{strataid = rep(1, ncol(x))}.
#'
#' @param x \code{v x k} matrix specifying weighted numerator for each of
#'   \code{k} PSUs (across columns)
#' @param n \code{v x k} matrix specifying weighted denominator for
#'   each PSU (across columns)
#' @param strataid integer or factor vector consisting of id for each strata. Optional,
#'   length should be number of columns of x if supplied.
#' @param L \code{q x v} matrix defining a linear transform 
#' @param fn function to transorm ratio x/n.
#'
#' @return a data frame with \code{q} rows consisting of estimates calculated as
#'   \code{fn(L * rowSums(x) / rowSums(n) )}, standard error, and 95\% CIs calculated
#'   on the untransformed scale and then transformed.  The covariance matrix is
#'   returned as the \code{"var"} attribute and can be accessed by \code{vcov(val)}.
#'
#' @references Pedersen J, Liu J (2012) Child Mortality Estimation: Appropriate
#'   Time Periods for Child Mortality Estimates from Full Birth Histories.
#'   PLoS Med 9(8): e1001289. \url{https://doi.org/10.1371/journal.pmed.1001289}.
#'
#' @export
jackknife <- function(x, n, strataid = NULL, L = diag(nrow(x)), fn = function(x) x){

  x[is.na(x)] <- 0
  n[is.na(n)] <- 0
  
  xall <- rowSums(x)
  nall <- rowSums(n)

  ## matrix of numerator and denominator w/ single cluster removed
  xjack <- xall - x
  njack <- nall - n

  if(is.null(strataid))
    strataid <- rep(1, ncol(x))
  else
    strataid <- as.integer(factor(strataid))

  ## number of clusters per stratum
  h_n <- c(table(strataid))[strataid]

  ## pre-transformed estimator ( fn^-1(est) )
  lest <- drop(L %*% (xall / nall))
               
  ## array of rates with single cluster removed
  lest_jack <- L %*% (xjack / njack)

  lresid <- lest_jack - lest
  lv <- lresid %*% ((h_n - 1) / h_n * t(lresid))

  resid <- fn(lest) - fn(lest_jack)
  v <- resid %*% ((h_n - 1) / h_n * t(resid))

  val <- data.frame(est  = fn(lest),
                    se   = sqrt(diag(v)),
                    ci_l = fn(lest - qnorm(0.975)*sqrt(diag(lv))),
                    ci_u = fn(lest + qnorm(0.975)*sqrt(diag(lv))))

  attr(val, "var") <- v
  val
}
