
## Converts DHS Individual Recode (ir) file into sibling dataset with one row
## for each sibling reported.
create_sib_data <- function(ir, country, survyear, strata="v022"){

  ## caseid   "case identification"
  ## v000     "country code and phase"
  ## v001     "cluster number"
  ## v002     "household number"
  ## v003     "respondent's line number"
  ## v004     "ultimate area unit"
  ## v005     "women's individual sample weight (6 decimals)"
  ## v006     "month of interview"
  ## v007     "year of interview"
  ## v008     "date of interview (cmc)"
  ## v008a    "date of interview century day code (cdc)"
  ## v009     "respondent's month of birth"
  ## v010     "respondent's year of birth"
  ## v011     "date of birth (cmc)"
  ## v012     "respondent's current age"
  ## v013     "age in 5-year groups"
  ## v014     "completeness of age information"
  ## v015     "result of individual interview"
  ## v016     "day of interview"
  ## v017     "cmc start of calendar"
  ## v018     "row of month of interview"
  ## v019     "length of calendar"
  ## v019a    "number of calendar columns"
  ## v020     "ever-married sample"
  ## v021     "primary sampling unit"
  ## v022     "sample strata for sampling errors"
  ## v023     "stratification used in sample design"
  ## v024     "region"
  ## v025     "type of place of residence"
  ## v026     "na - de facto place of residence"
  ## v027     "number of visits"
  ## v028     "interviewer identification"
  ## v029     "na - keyer identification"
  ## v030     "field supervisor"

  ## mmidx_01 "index to maternal mortality"
  ## mm1_01   "sex of sibling"
  ## mm2_01   "survival status of sibling"
  ## mm3_01   "sibling's current age"
  ## mm4_01   "sibling's date of birth (cmc)"
  ## mm5_01   "na - sibling's marital status"
  ## mm6_01   "years since sibling died"
  ## mm7_01   "sibling's age at death"
  ## mm8_01   "date of death of sibling (cmc)"
  ## mm9_01   "sibling's death and pregnancy"
  ## mm10_01  "na - death and a pregnancy are related"
  ## mm11_01  "na - cause of death sibling"
  ## mm12_01  "na - amount of time between sibling's delivery and death"
  ## mm13_01  "na - place of sibling's death"
  ## mm14_01  "number of sibling's children"
  ## mm15_01  "na - sibling's year of death"


  ir$country <- country
  ir$survyear <- survyear
  ir$stratum <- do.call(paste, ir[strata])

  widevars <- c("caseid", "country", "survyear", "stratum",
                "v001", "v002", "v003", "v005", "v007", "v008", "v011", "v012", "v013", "v021", "v022", "v023", "v024", "v025",
                grep("mmc", names(ir), value=TRUE))
  longvars <- grep("^mm[idx0-9]", names(ir), value=TRUE)
  varying <- tapply(longvars, sub("(.*)_.*", "\\1", longvars), c)

  ir <- ir[c(widevars, longvars)]

  ## Reshape wide to long
  sib <- reshape(ir[c(widevars, unlist(longvars))],
                 varying = varying,
                 v.names = names(varying),
                 idvar = "caseid",
                 timevar = "mmidx",
                 direction = "long")

  ## Drop empty entries
  sib <- subset(sib, !is.na(mmidx))


  ## Recode variables

  if(is.null(sib$mm15))
    sib$mm15 <- rep(NA, nrow(sib))

  if(is.factor(sib$v007)) sib$v007 <- as.integer(as.character(sib$v007))
  sib$v007 <- with(sib, ifelse(v007 < 100, 1900+v007, v007))
  sib$v007 <- with(sib, ifelse(is.na(v007), 1900+floor(v008/12), v007))

  sib$v023 <- factor(sib$v023)

  if(class(sib$mmc5) == "integer")
    sib$mmc5 <- factor(sib$mmc5, c(10, 12, 13, 15), c("10 years", "12 years", "13 years", "15 years"))

  sib$intvy <- sib$v007
  sib$intvcmc <- sib$v008

  mm1 <- sib$mm1
  mm1 <- replace(mm1, mm1==1, "male")
  mm1 <- replace(mm1, mm1==2, "female")
  sib$sex <- factor(tolower(mm1), c("male", "female"))

  mm2 <- sib$mm2
  mm2 <- replace(mm2, mm2==0, "dead")
  mm2 <- replace(mm2, mm2==1, "alive")
  sib$deaths <- factor(tolower(mm2), c("dead", "alive")) == "dead"

  sib$weight <- sib$v005/1e6

  sib$sibdob <- sib$mm4
  sib$sibdod <- sib$mm8

  return(sib)
}


create_mics_sib_data <- function(mm, country, survyear, strata=c("hh7", "hh6")){

  ## HH1      "Cluster number"
  ## HH2      "Household number"
  ## LN       "Line number"
  ## MMLN     "Sibling's line number"
  ## MM5      "Sibling's gender"
  ## MM6      "Sibling still alive"
  ## MM7      "Age of sibling"
  ## MM8      "Years since death"
  ## MM9      "Age at death of sibling"
  ## MM10     "Pregnant when died"
  ## MM11     "Died during the childbirth"
  ## MM12     "Died within two months after the end of a pregnancy or childbirth"
  ## MM13     "Number of live born children during lifetime"
  ## HH6      "Area"
  ## HH7      "Province"
  ## WDOI     "Date of interview women (CMC)"
  ## WDOB     "Date of birth of woman (CMC)"
  ## MM7C     "Imputed date of birth"
  ## MM8C     "Imputed date of death"
  ## welevel  "Education"
  ## wmweight "Women's sample weight"
  ## wscore   "Combined wealth score"
  ## windex5  "Wealth index quintile"
  ## wscoreu  "Urban wealth score"
  ## windex5u "Urban wealth index quintile"
  ## wscorer  "Rural wealth score"
  ## windex5r "Rural wealth index quintile"

  sib <- do.call(data.frame, mm)
  names(sib) <- tolower(names(sib))  # some inconsistency in capitalisation of variable names

  sib$country <- country
  sib$survyear <- survyear
  sib$stratum <- do.call(paste, sib[strata])

  sib$intvcmc <- sib$wdoi
  sib$intvy <- floor(sib$wdoi/12 + 1900)
  sib$sex <- factor(as.integer(sib$mm5), 1:2, c("male", "female"))
  sib$sibdob <- sib$mm7c
  sib$sibdod <- sib$mm8c

  sib$deaths <- as.integer(factor(as.integer(sib$mm6), 1:2) == 2)

  sib$weight <- sib$wmweight

  return(sib)
}



#' Calculate age-specific mortality rates in period preceding survey.
#' Interval 'period' defined in the months before the survey.
#' Should replicate mortality rates reported in DHS reports.
#'
#' @importFrom survival Surv
#' @importFrom survival survSplit
calc_dhs_mx <- function(sib, period=c(0, 84)){

  ## drop if missing survival status or sex
  sib <- subset(sib, sex %in% c("male", "female") & !is.na(deaths))

  ## define episode
  sib$deaths <- with(sib, ifelse(deaths & v008-period[1] < mm8+1, FALSE, deaths))
  sib$agecens <- with(sib, pmin(v008-period[1], mm8+1, na.rm=TRUE) - mm4)  # add 1 to DOD to go to month end
  sib$agestart <- sib$v008 - period[2] - sib$mm4

  sibspl <- survSplit(Surv(agestart, agecens, deaths)~.,
                      subset(sib, agecens > agestart),
                      cut=3:10*5*12, episode="agegr")

  sibspl$agegr <- factor(sibspl$agegr, 2:8, 3:9*5)
  sibspl <- subset(sibspl, !is.na(agegr))
  sibspl$pys <- (sibspl$agecens - sibspl$agestart)/12

  mx <- aggregate(weight*cbind(deaths, pys) ~ agegr + sex + survyear + country, sibspl, sum)
  mx$mx <- mx$deaths / mx$pys

  return(mx)
}

## Calculate 35q15 from output of calc_dhs_mx().
## Shoudl replicate 35q15 reported in DHS reports.
calc_dhs_35q15 <- function(mx){
  qx <- aggregate(cbind(px=exp(-5*mx)) ~ survyear+sex+country, mx, prod)
  qx$qx <- 1-qx$px
  return(qx)

}


#' Create episode dataset split by period, age group, and time preceding survey indicator (TIPS)
#'
#' @importFrom survival Surv
#' @importFrom survival survSplit
create_tips_data <- function(dat, period=do.call(seq.int, as.list(range(dat$intvy)+c(-16, 1))), agegr = 3:12*5, tips = 0:15, dobvar="sibdob", dodvar="sibdod"){

  ## drop if missing survival status or sex
  dat <- subset(dat, sex %in% c("male", "female") & !is.na(deaths))

  ## split into annual TIPS episodes
  ## (Note: splitting TIPS first to eliminate as much person time as posdatle for each person for efficiency)
  dat$tstart <- dat[[dobvar]] # episode starts at child dob
  dat$tcens <- pmin(dat[[dodvar]]+1, dat$intvcmc, na.rm=TRUE) # episode ends at either date of death or interview
  dat$tipsstart <- with(dat, tstart - intvcmc)
  dat$tipscens <- with(dat, tcens - intvcmc)
  dat <- survSplit(Surv(tipsstart, tipscens, deaths)~., subset(dat, tipscens > tipsstart), cut=-tips*12, episode="tips", start="tipsstart", end="tipscens")
  dat$tips <- c(NA, rev(tips[-length(tips)]), NA)[dat$tips]
  dat <- subset(dat, !is.na(tips))

  dat$tstart <- with(dat, tipsstart + intvcmc)
  dat$tcens <- with(dat, tipscens + intvcmc)

  ## define time episode and split
  dat <- survSplit(Surv(tstart, tcens, deaths)~., subset(dat, tcens > tstart), cut=(period-1900)*12, episode="period", start="tstart", end="tcens")
  dat$period <- c(NA, period[-length(period)], NA)[dat$period]
  dat <- subset(dat, !is.na(period))

  ## split into age groups
  dat$agestart <- dat$tstart - dat[[dobvar]]
  dat$agecens <- dat$tcens - dat[[dobvar]]
  dat <- survSplit(Surv(agestart, agecens, deaths)~., dat, cut=agegr*12, episode="agegr", start="agestart", end="agecens")
  dat$agegr <- c(NA, agegr[-length(agegr)], NA)[dat$agegr]
  dat <- subset(dat, !is.na(agegr))

  ## calculate PYs
  dat$pys <- with(dat, (agecens - agestart)/12)

  return(dat)
}
