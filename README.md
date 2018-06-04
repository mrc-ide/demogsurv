
<!-- README.md is generated from README.Rmd. Please edit that file -->

[![Travis build
status](https://travis-ci.org/mrc-ide/demogsurv.svg?branch=master)](https://travis-ci.org/mrc-ide/demogsurv)
[![Coverage
status](https://codecov.io/gh/mrc-ide/demogsurv/branch/master/graph/badge.svg)](https://codecov.io/github/mrc-ide/demogsurv?branch=master)

# demogsurv

The goal of demogsurv is to:

  - Calculate common demographic indicators from household survey data,
    including child mortality, adult mortality, and fertility.
  - Stratify indicators according to arbitrary age groups, calendar
    periods, birth cohorts, time before survey, or other survey
    variables (region, residence, wealth category, education, etc.).
  - Efficiently prepare event / person-year datasets from survey data
    for other model-based analysis.
  - Provide standard error estimates for all indicators via Taylor
    linearization or jackknife.
  - Calculate sample covariance for indicators estimated from a single
    complex survey (e.g. time-series of child mortality estimates).
  - Default parameters are specified for analysis of [DHS recode
    datasets](https://dhsprogram.com/data/) without fuss, but funciton
    implementation is flexible for use with other survey data.

For analysis of DHS data, the package interacts well with
[`rdhs`](https://ojwatson.github.io/rdhs/). See the
[vignette](https://github.com/mrc-ide/demogsurv/blob/master/vignettes/rdhs-integration.pdf)
for an example.

## Installation

You can install the development version from
[GitHub](https://github.com/mrc-ide/demogsurv) with:

``` r
# install.packages("devtools")
devtools::install_github("mrc-ide/demogsurv")
```

The package will be released on [CRAN](https://CRAN.R-project.org) in
due course.

## Example

Load the package and example datasets created from [DHS Model
Datasets](https://dhsprogram.com/data/model-datasets.cfm).

``` r
library(demogsurv)

data(zzbr) # Births recode (child mortality)
data(zzir) # Individuals recode (fertility, adult mortality)
```

### Child mortality

By default, the function `calc_nqx` calculates U5MR by periods 0-4, 5-9,
and 10-14 years before the survey. Before calculating mortality rates,
create a binary variable indicator whether a death occurred and a
variable giving the date of death, placed 0.5 months in the month the
death occurred.

``` r
zzbr$death <- zzbr$b5 == "no"      # b5: child is alive ("yes" or "no")
zzbr$dod <- zzbr$b3 + zzbr$b7 + 0.5
u5mr <- calc_nqx(zzbr)
u5mr
#>    tips       est          se      ci_l      ci_u
#> 1 10-14 0.2212304 0.011730185 0.2208840 0.2215766
#> 2   5-9 0.1937855 0.008292561 0.1936183 0.1939527
#> 3   0-4 0.1408711 0.006432798 0.1407767 0.1409655
```

Note that `calc_nqx()` does **not** reproduce child mortality estimates
produced in DHS reports. `calc_nqx()` conducts a standard demographic
rate calculation based on observed events and person years within each
age group and then converts the cumulative hazard to survival
probabilities. The standard DHS indicator uses a rule-based approach to
allocate child deaths and person years across age groups and proceeds by
calculating direct probabilities of death in each age group (see
[Rutstein and
Rojas 2006](http://dhsprogram.com/pubs/pdf/DHSG1/Guide_to_DHS_Statistics_29Oct2012_DHSG1.pdf)).
A function `calc_dhs_u5mr()` will reproduce the DHS calculation, but is
not yet fully implemented.

Use the argument `by=` to specify factor variables by which to stratify
the rate calculation.

``` r
calc_nqx(zzbr, by=~v102) # by urban/rural residence
#>    v102  tips       est          se      ci_l      ci_u
#> 1 urban 10-14 0.1750790 0.018595935 0.1742570 0.1759002
#> 2 rural 10-14 0.2453074 0.013657991 0.2448228 0.2457917
#> 3 urban   5-9 0.1818306 0.016617688 0.1811688 0.1824918
#> 4 rural   5-9 0.1994068 0.009424996 0.1991893 0.1996242
#> 5 urban   0-4 0.1532941 0.014183345 0.1528283 0.1537597
#> 6 rural   0-4 0.1345284 0.006453178 0.1344341 0.1346227
calc_nqx(zzbr, by=~v190, tips=c(0, 10)) # by wealth quintile, 0-9 years before
#>      v190 tips       est          se      ci_l      ci_u
#> 1 poorest  0-9 0.1768813 0.009976478 0.1766442 0.1771182
#> 2  poorer  0-9 0.1823974 0.009763729 0.1821689 0.1826259
#> 3  middle  0-9 0.1657497 0.010603135 0.1654855 0.1660137
#> 4  richer  0-9 0.1557011 0.012229238 0.1553538 0.1560482
#> 5 richest  0-9 0.1456974 0.015916788 0.1451160 0.1462785
calc_nqx(zzbr, by=~v101+v102, tips=c(0, 10)) # by region and residence
#>       v101  v102 tips       est         se      ci_l      ci_u
#> 1 region 1 urban  0-9 0.1440943 0.01331266 0.1436883 0.1445000
#> 2 region 2 urban  0-9 0.1648417 0.02741923 0.1630754 0.1666042
#> 3 region 3 urban  0-9 0.1618804 0.01696421 0.1612071 0.1625531
#> 4 region 4 urban  0-9 0.1998386 0.03390294 0.1970182 0.2026491
#> 5 region 1 rural  0-9 0.1559257 0.01004546 0.1556914 0.1561600
#> 6 region 2 rural  0-9 0.1755639 0.01088753 0.1752821 0.1758457
#> 7 region 3 rural  0-9 0.2021511 0.03301796 0.1994685 0.2048247
#> 8 region 4 rural  0-9 0.1764668 0.01299817 0.1760646 0.1768688
```

The sample covariance or correlation matrix of the estimates can be
obtained via `vcov()`.

``` r
vcov(u5mr)  # sample covariance
#>              [,1]         [,2]         [,3]
#> [1,] 1.375972e-04 4.450180e-05 8.842864e-06
#> [2,] 4.450180e-05 6.876656e-05 1.451310e-05
#> [3,] 8.842864e-06 1.451310e-05 4.138089e-05
cov2cor(vcov(u5mr))  # sample correlation
#>           [,1]      [,2]      [,3]
#> [1,] 1.0000000 0.4574926 0.1171894
#> [2,] 0.4574926 1.0000000 0.2720644
#> [3,] 0.1171894 0.2720644 1.0000000
```

Standard error estimation can be done via Taylor linearisation,
unstratified jackknife, or stratified jackknife. Results are very
similar.

``` r
calc_nqx(zzbr, varmethod = "lin") # default is linearization
#>    tips       est          se      ci_l      ci_u
#> 1 10-14 0.2212304 0.011730185 0.2208840 0.2215766
#> 2   5-9 0.1937855 0.008292561 0.1936183 0.1939527
#> 3   0-4 0.1408711 0.006432798 0.1407767 0.1409655
calc_nqx(zzbr, varmethod = "jkn") # stratified jackknife (varmethod = "jkn")
#>    tips       est          se      ci_l      ci_u
#> 1 10-14 0.2212304 0.011931233 0.1974762 0.2442815
#> 2   5-9 0.1937855 0.008280170 0.1773984 0.2098463
#> 3   0-4 0.1408711 0.006434272 0.1281716 0.1533856

## Compare unstratified standard error estimates for linearization and jackknife
calc_nqx(zzbr, strata=NULL, varmethod = "lin")  # unstratified design
#>    tips       est          se      ci_l      ci_u
#> 1 10-14 0.2212304 0.011976980 0.2208693 0.2215913
#> 2   5-9 0.1937855 0.008367844 0.1936153 0.1939558
#> 3   0-4 0.1408711 0.006507844 0.1407745 0.1409677
calc_nqx(zzbr, strata=NULL, varmethod = "jk1")  # unstratififed jackknife
#>    tips       est          se      ci_l      ci_u
#> 1 10-14 0.2212304 0.012088926 0.1971571 0.2445818
#> 2   5-9 0.1937855 0.008408382 0.1771422 0.2100923
#> 3   0-4 0.1408711 0.006539720 0.1279620 0.1535891
```

To calculate different child mortality indicators (neonatal, infant,
etc.), specify different age groups over which to aggregate.

``` r
calc_nqx(zzbr, agegr=c(0, 1)/12)  # neonatal
#>    tips        est          se       ci_l       ci_u
#> 1 10-14 0.04358764 0.004262693 0.04355040 0.04362487
#> 2   5-9 0.04590724 0.003799869 0.04587757 0.04593690
#> 3   0-4 0.03792144 0.003320563 0.03789898 0.03794390
calc_nqx(zzbr, agegr=c(1, 3, 5, 12)/12) # postneonatal
#>    tips        est          se       ci_l       ci_u
#> 1 10-14 0.10613868 0.007294180 0.10602201 0.10625533
#> 2   5-9 0.08366400 0.006097552 0.08358447 0.08374352
#> 3   0-4 0.05107115 0.003999245 0.05103811 0.05110418
calc_nqx(zzbr, agegr=c(0, 1, 3, 5, 12)/12) # infant (1q0)
#>    tips       est          se       ci_l       ci_u
#> 1 10-14 0.1451000 0.008870650 0.14491956 0.14528037
#> 2   5-9 0.1257305 0.006823068 0.12562608 0.12583481
#> 3   0-4 0.0870559 0.005070203 0.08700071 0.08711109
calc_nqx(zzbr, agegr=c(12, 24, 36, 48, 60)/12) # child (4q1)
#>    tips        est          se       ci_l       ci_u
#> 1 10-14 0.08905182 0.007189004 0.08894062 0.08916301
#> 2   5-9 0.07784223 0.005665474 0.07777401 0.07791045
#> 3   0-4 0.05894689 0.004529373 0.05890416 0.05898961
calc_nqx(zzbr, agegr=c(0, 1, 3, 5, 12, 24, 36, 48, 60)/12) # u5mr (5q0)
#>    tips       est          se      ci_l      ci_u
#> 1 10-14 0.2212304 0.011730185 0.2208840 0.2215766
#> 2   5-9 0.1937855 0.008292561 0.1936183 0.1939527
#> 3   0-4 0.1408711 0.006432798 0.1407767 0.1409655
```

Calculate annual <sub>5</sub>q<sub>0</sub> by calendar year (rather than
years preceding survey).

``` r
calc_nqx(zzbr, period=2005:2015, tips=NULL)
#>    period       est         se      ci_l      ci_u
#> 1    2005 0.1937695 0.01611970 0.1931375 0.1944009
#> 2    2006 0.1890020 0.01485224 0.1884687 0.1895349
#> 3    2007 0.1983320 0.01469757 0.1978037 0.1988600
#> 4    2008 0.1906183 0.01306713 0.1902047 0.1910317
#> 5    2009 0.1979731 0.01559065 0.1973789 0.1985669
#> 6    2010 0.1874172 0.01366295 0.1869668 0.1878673
#> 7    2011 0.1768661 0.01378438 0.1764136 0.1773184
#> 8    2012 0.1390004 0.01488779 0.1384957 0.1395048
#> 9    2013 0.1224668 0.01183607 0.1221539 0.1227796
#> 10   2014 0.1225079 0.01208800 0.1221814 0.1228342
```

### Adult mortality

The function `calc_nqx()` can also used to calculate adult mortality
indicators such as <sub>35</sub>q<sub>15</sub>. First, the convenience
function `reshape_sib_data()` transforms respondent-level data to a
dataset with one row for each sibling reported. Then define a binary
variable for whether the sibling is alive or dead.

``` r
zzsib <- reshape_sib_data(zzir)
zzsib$death <- factor(zzsib$mm2, c("dead", "alive")) == "dead"
```

Calculate <sub>35</sub>q<sub>15</sub> for the seven year period before
the
survey.

``` r
calc_nqx(zzsib, agegr=seq(15, 50, 5), tips=c(0, 7), dob="mm4", dod="mm8")
#>   tips       est          se      ci_l      ci_u
#> 1  0-6 0.1778199 0.009395366 0.1776095 0.1780303
```

Calculate <sub>35</sub>q<sub>15</sub> by sex, replicating Table
MM2.2.

``` r
zzsib$sex <- factor(zzsib$mm1, c("female", "male"))  # drop mm2 = 3: "missing"
calc_nqx(zzsib, by=~sex, agegr=seq(15, 50, 5), tips=c(0, 7), dob="mm4", dod="mm8")
#>      sex tips       est         se      ci_l      ci_u
#> 1 female  0-6 0.1790557 0.01296694 0.1786542 0.1794571
#> 2   male  0-6 0.1766238 0.01332997 0.1762008 0.1770467
```

This calculation exactly reproduces the <sub>35</sub>q<sub>15</sub>
estiamtes produced for Table MM2 for DHS reports. Additional
functionality will be added in future for producing ASMRs (Table MM1),
MMR, and PM (Table MM3) will be added in future.

### Fertility

The functions `calc_asfr()` and `calc_tfr()` calculate age-specific
fertility rates and total fertility rate, respectively. The default
calculation is by five-year age groups for three years before the
survey, exactly reproducing the estimates produced in DHS reports.

``` r
## Replicate DHS Table 5.1.
## Total ASFR and TFR in 3 years preceding survey
calc_asfr(zzir, tips=c(0, 3))
#>   agegr tips       asfr     se_asfr
#> 1 15-19  0-2 0.11901592 0.008154144
#> 2 20-24  0-2 0.20736603 0.012782416
#> 3 25-29  0-2 0.21553394 0.008245023
#> 4 30-34  0-2 0.18803561 0.010426419
#> 5 35-39  0-2 0.12494212 0.008135705
#> 6 40-44  0-2 0.06044451 0.007502008
#> 7 45-49  0-2 0.02828233 0.006066635
calc_tfr(zzir)
#>   tips      tfr    se_tfr
#> 1  0-2 4.718102 0.1949224

## ASFR and TFR by urban/rural residence
reshape2::dcast(calc_asfr(zzir, ~v025, tips=c(0, 3)), agegr ~ v025, value.var = "asfr")
#>   agegr      urban      rural
#> 1 15-19 0.07718083 0.16269262
#> 2 20-24 0.15883881 0.25754199
#> 3 25-29 0.16862119 0.24797353
#> 4 30-34 0.14294124 0.21875542
#> 5 35-39 0.08182797 0.15262614
#> 6 40-44 0.04817726 0.07005696
#> 7 45-49 0.02404944 0.03087701
calc_tfr(zzir, by=~v025)
#>    v025 tips      tfr    se_tfr
#> 1 urban  0-2 3.508184 0.2813637
#> 2 rural  0-2 5.702618 0.1425967
calc_tfr(zzir, by=~v025, varmethod="jkn")
#>    v025 tips      tfr    se_tfr
#> 1 urban  0-2 3.508184 0.3004737
#> 2 rural  0-2 5.702618 0.1427030
```

Replicate fertility estimates stratified by various sociodemographic
characteristics.

``` r
## Replicate DHS Table 5.2
calc_tfr(zzir, ~v102)  # residence
#>    v102 tips      tfr    se_tfr
#> 1 urban  0-2 3.508184 0.2813637
#> 2 rural  0-2 5.702618 0.1425967
calc_tfr(zzir, ~v101)  # region
#>       v101 tips      tfr    se_tfr
#> 1 region 1  0-2 5.334781 0.1781935
#> 2 region 2  0-2 5.255445 0.2842471
#> 3 region 3  0-2 3.079052 0.3674651
#> 4 region 4  0-2 5.500077 0.2452335
calc_tfr(zzir, ~v106)  # education
#>           v106 tips      tfr    se_tfr
#> 1 no education  0-2 5.585398 0.1406657
#> 2      primary  0-2 5.041455 0.3599601
#> 3    secondary  0-2 3.245211 0.2956154
#> 4       higher  0-2 1.388428 0.3198573
calc_tfr(zzir, ~v190)  # wealth quintile
#>      v190 tips      tfr    se_tfr
#> 1 poorest  0-2 5.951668 0.2183868
#> 2  poorer  0-2 5.693275 0.1867203
#> 3  middle  0-2 5.270892 0.2283221
#> 4  richer  0-2 4.406202 0.2813975
#> 5 richest  0-2 2.907027 0.2738504
calc_tfr(zzir)  # total
#>   tips      tfr    se_tfr
#> 1  0-2 4.718102 0.1949224
```

Generate estimates stratified by both calendar period and time preceding
survey.

``` r
calc_tfr(zzir, period = c(2010, 2013, 2015), tips=0:5)
#>      period tips      tfr    se_tfr
#> 1 2010-2012    4 5.260048 0.3166023
#> 2 2010-2012    3 5.207062 0.2697222
#> 3 2010-2012    2 4.084194 0.3176187
#> 4 2013-2014    2 5.020537 0.2999262
#> 5 2013-2014    1 4.555710 0.2448427
#> 6 2013-2014    0 4.720417 0.3525991
```

Calculate ASFR by birth cohort.

``` r
asfr_coh <- calc_asfr(zzir, cohort=c(1980, 1985, 1990, 1995), tips=NULL)
reshape2::dcast(asfr_coh, agegr ~ cohort, value.var = "asfr")
#>   agegr 1980-1984 1985-1989 1990-1994
#> 1 15-19 0.1636472 0.1641461 0.1470502
#> 2 20-24 0.2610719 0.2402068 0.2140443
#> 3 25-29 0.2519899 0.2321897 0.2957371
#> 4 30-34 0.2012103 0.1674921        NA
#> 5 35-39 0.1473996        NA        NA
```

## To Do

  - Refactor for single `calc_rate()` function used by mortality and
    fertility calculations.

  - Add indirect indicators

  - Implement DHS child mortality calculation (Rutstein and Rojas 2006).

  - Handle married women only datasets.

  - Calculate multiple aggregations of nqx and sample correlation of
    these (e.g. NN, PN, 1q0, 4q1, 5q0).

  - Document everything…

  - Develop, test, and create examples for non-DHS household surveys,
    e.g. MICS, WFS.

  - Extend interface to allow specification of formula, variable name,
    or vectors.

  - Add some basic data checking before rate calculation, good warnings,
    potentially automatic handling. 
	- [ ] Variable names all exist. 
	- [ ] No missing data in date variables. 
	- [ ] Episode start date is before episode end date. 
	- [ ] All birth history ids appear in respondent dataset.
