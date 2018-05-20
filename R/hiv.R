create_dhs_hiv_data <- function(ir, mr, ar, country, survyear, strata=NULL){

  if(is.null(strata))
    strata <- "v022"
  
  ir$stratum <- do.call(paste, ir[strata])
  if(is.null(mr))
    ir$sex <- factor(as.integer(ir$aidsex), 1:2, c("male", "female"))
  else
    ir$sex <- "female"

  if(ir$v000[1] == "CI5"){
    ## For Cote d'Ivoire 2005 AIS, individuals are uniquely identified by
    ## four variables {cluster, structure, household, line}.
    ir$v002 <- 100*ir$sstruct + ir$v002
    ar$hivnumb <- 100*ar$hivstruct + ar$hivnumb
  }
  
  dat <- with(ir, data.frame(country    = country,
                             survyear   = survyear,
                             DHSCC     = substr(v000, 1, 2),
                             DHSYEAR   = as.integer(substr(survyear, 1, 4)),
                             stratum    = stratum,
                             ea         = v021,
                             cluster    = v001,
                             household  = v002,
                             line       = v003,
                             indweight  = v005/1e6,
                             sex        = sex,
                             age        = v012,
                             restype    = tolower(v025),
                             region     = v024))
  
  if(!is.null(mr)){
    names(mr) <- sub("^mv", "v", names(mr))
    names(mr) <- sub("^sm", "s", names(mr))
    mr$stratum <- do.call(paste, mr[strata])
    mr$sex <- "male"

    dat <- rbind(dat,
                 with(mr, data.frame(country    = country,
                                     survyear   = survyear,
                                     DHSCC     = substr(v000, 1, 2),
                                     DHSYEAR   = as.integer(substr(survyear, 1, 4)),
                                     stratum    = stratum,
                                     ea         = v021,
                                     cluster    = v001,
                                     household  = v002,
                                     line       = v003,
                                     indweight  = v005/1e6,
                                     sex        = sex,
                                     age        = v012,
                                     restype    = tolower(v025),
                                     region     = v024)))
  }


  ## Recode HIV status outcome
  hivneg_codes <- c("hiv negative", "HIV negative", "0")
  hivpos_codes <- c("hiv  positive", "hiv1  positive", "hiv2 positive", "hiv1 & hiv2 positive", "1", "2", "3", "HIV  positive", "HIV2 positive")
  na_codes <- c("indeterminant", "7", "8", "9", "Indeterminant", "inconclusive", "indeterminate", NA)

  if(any(!ar$hiv03 %in% c(hivpos_codes, hivneg_codes, na_codes)))
    warning(paste0(country, " ", survyear, ": HIV result codes [",
                   paste(unique(ar$hiv03[!ar$hiv03 %in% c(hivpos_codes, hivneg_codes, na_codes)]), collapse=", "),
                   "] not recognized (hiv03)"))

  ar$hivres <- NA
  ar$hivres[ar$hiv03 %in% hivneg_codes] <- FALSE
  ar$hivres[ar$hiv03 %in% hivpos_codes] <- TRUE
  ar$hivweight <- ar$hiv05/1e6
  dat <- merge(dat, ar[c("hivclust", "hivnumb", "hivline", "hivres", "hivweight")],
               by.x=c("cluster", "household", "line"), by.y=c("hivclust", "hivnumb", "hivline"))

  ## TODO:
  ## handle Zimbabwe 2015 survey: "The HIV Test data contain results for children 0-5 years,
  ## minors aged 6-14 years, and adult men and women. For the children and minors, the
  ## household weight (HV005) is used for analysis, and for adult men and women, the HIV
  ## weight (HIV05) is used." (http://www.dhsprogram.com/data/available-datasets.cfm#footNotesLink)
  ## if(is.null(dat$hivage))
  ##   dat$hivage <- with(dat, ifelse(!is.na(ha1), ha1, ifelse(!is.na(hb1), hb1, NA)))
  ## dat$hivweight <- with(dat, ifelse(hivage < 15, hv005, hiv05))/1e6

  return(dat)
}
