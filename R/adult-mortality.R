
#' Convert respondent-level sibling history data to one row per sibling
#'
#' @param data A dataset as `data.frame`.
#' @param widevars Character vector of respondent-level variable names to include.
#' @param longvars Character vector of variables corresponding to each sibling.
#' @param idvar Vector of variable names uniquely identifying each respondent.
#' @param sib_vars Vector of same length as longvars giving variable names in long dataset.
#' @param sib_idvar Variable name uniquely identifying each sibling record. Should appear amongst `sib_vars`.
#' @param sibvar_regex Optionally, a regular expression to identify variable names for `longvars` from names of `data`.
#'
#' @examples
#' data(zzir62fl)
#'
#' zzsib <- reshape_sib_data(zzir62fl)
#' zzsib$death <- factor(zzsib$mm2, c("dead", "alive")) == "dead"
#' zzsib$sex <- factor(zzsib$mm1, c("female", "male"))  # drop mm2 = 3: "missing"
#' calc_nqx(zzsib, by=~sex, agegr=seq(15, 50, 5), tips=c(0, 8), dob="mm4", dod="mm8")
#'
#' @export
#' @md

reshape_sib_data <- function(data,
                             widevars = grep("^v", names(data), value=TRUE),
                             longvars = grep(sibvar_regex, names(data), value=TRUE),
                             idvar = "caseid",
                             sib_vars = sub("(.*)_.*", "\\1", longvars),
                             sib_idvar = "mmidx",
                             sibvar_regex="^mm[idx0-9]"){

  if(length(intersect(widevars, longvars)))
    stop(paste("variables cannot be wide and long:", intersect(widevars, longvars)))
  
  varying <- tapply(longvars, sib_vars, c)
  
  ## Reshape wide to long
  val <- reshape(data[c(idvar, longvars)],
                 varying = varying,
                 v.names = names(varying),
                 idvar = idvar,
                 timevar = sib_idvar,
                 direction = "long")

  ## Drop episodes with no sibling reported
  val <- val[!is.na(val[[sib_idvar]]), ]
  
  ## Merge widevars
  val <- merge(data[union(idvar, widevars)], val)

  val
}
