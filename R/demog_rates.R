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

  list(df = df, mm = mm)
}
