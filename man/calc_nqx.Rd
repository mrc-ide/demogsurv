% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/child-mortality.R
\name{calc_nqx}
\alias{calc_nqx}
\title{Calculate the probability of dying between age x and x+n (nqx)}
\usage{
calc_nqx(
  data,
  by = NULL,
  agegr = c(0, 1, 3, 5, 12, 24, 36, 48, 60)/12,
  period = NULL,
  cohort = NULL,
  tips = c(0, 5, 10, 15),
  clusters = ~v021,
  strata = ~v024 + v025,
  weight = "v005",
  dob = "b3",
  dod = "dod",
  death = "death",
  intv = "v008",
  varmethod = "lin",
  origin = 1900,
  scale = 12
)
}
\arguments{
\item{data}{A dataset (data.frame), for example a DHS births recode (BR) dataset.}

\item{by}{A formula specifying factor variables by which to stratify analysis.}

\item{agegr}{Numeric vector defining ages \emph{in years} for splits.}

\item{period}{Numeric vector defining calendar periods to stratify analysis, use \code{NULL} for no periods.}

\item{cohort}{Numeric vector defining birth cohorts to stratify analysis, use \code{NULL} for no cohort stratification.}

\item{tips}{Break points for TIme Preceding Survey.}

\item{clusters}{Formula or data frame specifying cluster ids from largest level to smallest level, ‘~0’ or ‘~1’ is a formula for no clusters.}

\item{strata}{Formula or vector specifying strata, use ‘NULL’ for no strata.}

\item{weight}{Formula or vector specifying sampling weights.}

\item{dob}{Variable name for date of birth (character string).}

\item{dod}{Variable name for date of death (character string).}

\item{death}{Variable name for event variable (character string).}

\item{intv}{Variable name for interview date (character string).}

\item{varmethod}{Method for variance calculation. Currently "lin" for Taylor
linearisation or "jk1" for unstratified jackknife, or "jkn", for stratified
jackknife.}

\item{origin}{Origin year for date arguments. 1900 for CMC inputs.}

\item{scale}{Scale for dates inputs to calendar years. 12 for CMC inputs.}
}
\description{
Default arguments are configured to calculate under 5 mortality
from a DHS Births Recode file.
}
\examples{

data(zzbr)
zzbr$death <- zzbr$b5 == "no"  # b5: child still alive ("yes"/"no")
zzbr$dod <- zzbr$b3 + zzbr$b7 + 0.5

## Calculate 5q0 from birth history dataset.
## Note this does NOT exactly match DHS calculation.
## See calc_dhs_u5mr().
u5mr <- calc_nqx(zzbr)
u5mr

## Retrieve sample covariance and correlation
vcov(u5mr)  # sample covariance
cov2cor(vcov(u5mr))  # sample correlation

## 5q0 by sociodemographic characteristics
calc_nqx(zzbr, by=~v102) # by urban/rural residence
calc_nqx(zzbr, by=~v190, tips=c(0, 10)) # by wealth quintile, 0-9 years before
calc_nqx(zzbr, by=~v101+v102, tips=c(0, 10)) # by region and residence

## Compare unstratified standard error estiamtes for linearization and jackknife
calc_nqx(zzbr, varmethod = "lin")  # unstratified design
calc_nqx(zzbr, strata=NULL, varmethod = "lin")  # unstratified design
calc_nqx(zzbr, strata=NULL, varmethod = "jk1")  # unstratififed jackknife
calc_nqx(zzbr, varmethod = "jkn")  # stratififed jackknife

## Calculate various child mortality indicators (neonatal, infant, etc.)
calc_nqx(zzbr, agegr=c(0, 1)/12)  # neonatal
calc_nqx(zzbr, agegr=c(1, 3, 5, 12)/12) # postneonatal
calc_nqx(zzbr, agegr=c(0, 1, 3, 5, 12)/12) # infant (1q0)
calc_nqx(zzbr, agegr=c(12, 24, 36, 48, 60)/12) # child (4q1)
calc_nqx(zzbr, agegr=c(0, 1, 3, 5, 12, 24, 36, 48, 60)/12) # u5mr (5q0)

## Calculate annaul 5q0 by calendar year
calc_nqx(zzbr, period=2005:2015, tips=NULL)

}
