
<!-- README.md is generated from README.Rmd. Please edit that file -->

# hhsurveydata

The goal of hhsurveydata is to:

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
[`rdhs`](https://ojwatson.github.io/rdhs/). See the vignette *TO BE
COMPLETED* for an example.

## Installation

You can install the development version from
[GitHub](https://github.com/mrc-ide/hhsurveydata) with:

``` r
# install.packages("devtools")
devtools::install_github("mrc-ide/hhsurveydata")
```

The package will be released on [CRAN](https://CRAN.R-project.org) in
due course.

## Example

Load example datasets created from [DHS Model
Datasets](https://dhsprogram.com/data/model-datasets.cfm).

``` r
data(zzbr) # Births recode (child mortality)
data(zzir) # Individuals recode (fertility, adult mortality)
```

### Child mortality

By default, the function `calc_nqx` calculates U5MR by periods 0-4, 5-9,
and 10-14 years before the survey. Before calculating mortality rates,
create a binary variable indicator whether a death occurred and a
variable givine the date of death, placed 0.5 months in the month the
death occurred.

``` r
zzbr$death <- zzbr$b5 == "no"      # b5: child is alive ("yes" or "no")
zzbr$dod <- zzbr$b3 + zzbr$b7 + 0.5
u5mr <- calc_nqx(zzbr)
u5mr
#>    tips       nqx          se      ci_l      ci_u
#> 1 10-14 0.2212304 0.011730185 0.1978969 0.2438851
#> 2   5-9 0.1937855 0.008292561 0.1773675 0.2098759
#> 3   0-4 0.1408711 0.006432798 0.1281701 0.1533871
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
#>    v102  tips       nqx          se      ci_l      ci_u
#> 1 urban 10-14 0.1750790 0.018595935 0.1378145 0.2107329
#> 2 rural 10-14 0.2453074 0.013657991 0.2180578 0.2716074
#> 3 urban   5-9 0.1818306 0.016617688 0.1486035 0.2137609
#> 4 rural   5-9 0.1994068 0.009424996 0.1807194 0.2176679
#> 5 urban   0-4 0.1532941 0.014183345 0.1250339 0.1806416
#> 6 rural   0-4 0.1345284 0.006453178 0.1217876 0.1470845
calc_nqx(zzbr, by=~v190, tips=c(0, 10)) # by wealth quintile, 0-9 years before
#>      v190 tips       nqx          se      ci_l      ci_u
#> 1 poorest  0-9 0.1768813 0.009976478 0.1570936 0.1962044
#> 2  poorer  0-9 0.1823974 0.009763729 0.1630352 0.2013118
#> 3  middle  0-9 0.1657497 0.010603135 0.1447069 0.1862747
#> 4  richer  0-9 0.1557011 0.012229238 0.1313887 0.1793329
#> 5 richest  0-9 0.1456974 0.015916788 0.1139245 0.1763310
calc_nqx(zzbr, by=~v101+v102, tips=c(0, 10)) # by region and residence
#>       v101  v102 tips       nqx         se      ci_l      ci_u
#> 1 region 1 urban  0-9 0.1440943 0.01331266 0.1176001 0.1697929
#> 2 region 2 urban  0-9 0.1648417 0.02741923 0.1093342 0.2168898
#> 3 region 3 urban  0-9 0.1618804 0.01696421 0.1279628 0.1944787
#> 4 region 4 urban  0-9 0.1998386 0.03390294 0.1305530 0.2636029
#> 5 region 1 rural  0-9 0.1559257 0.01004546 0.1360056 0.1753866
#> 6 region 2 rural  0-9 0.1755639 0.01088753 0.1539462 0.1966293
#> 7 region 3 rural  0-9 0.2021511 0.03301796 0.1347402 0.2643101
#> 8 region 4 rural  0-9 0.1764668 0.01299817 0.1505927 0.2015527
```

The column `nqx` is stored as class `svystat` from the `survey` package.
The sample covariance or correlation matrix of the estimates can be
obtained using `vcov()`.

``` r
vcov(u5mr$nqx)  # sample covariance
#>              [,1]         [,2]         [,3]
#> [1,] 1.375972e-04 4.450180e-05 8.842864e-06
#> [2,] 4.450180e-05 6.876656e-05 1.451310e-05
#> [3,] 8.842864e-06 1.451310e-05 4.138089e-05
cov2cor(vcov(u5mr$nqx))  # sample correlation
#>           [,1]      [,2]      [,3]
#> [1,] 1.0000000 0.4574926 0.1171894
#> [2,] 0.4574926 1.0000000 0.2720644
#> [3,] 0.1171894 0.2720644 1.0000000
```

Standard error estimation can be done via Taylor linearisation or
unstratified jackknife. Results are very similar if unstratified
design-based estimates are used.

``` r
calc_nqx(zzbr, varmethod = "lin") # default is linearization
#>    tips       nqx          se      ci_l      ci_u
#> 1 10-14 0.2212304 0.011730185 0.1978969 0.2438851
#> 2   5-9 0.1937855 0.008292561 0.1773675 0.2098759
#> 3   0-4 0.1408711 0.006432798 0.1281701 0.1533871

## Compare unstratified standard error estiamtes for linearization and jackknife
calc_nqx(zzbr, strata=NULL, varmethod = "lin")  # unstratified design
#>    tips       nqx          se      ci_l      ci_u
#> 1 10-14 0.2212304 0.011976980 0.1973986 0.2443546
#> 2   5-9 0.1937855 0.008367844 0.1772169 0.2100205
#> 3   0-4 0.1408711 0.006507844 0.1280208 0.1535320
calc_nqx(zzbr, strata=NULL, varmethod = "jk1")  # unstratififed jackknife
#>    tips       nqx          se      ci_l      ci_u
#> 1 10-14 0.2212304 0.012088926 0.1971571 0.2445818
#> 2   5-9 0.1937855 0.008408382 0.1771422 0.2100923
#> 3   0-4 0.1408711 0.006539720 0.1279620 0.1535891
```

To calculate different child mortality indicators (neonatal, infant,
etc.), specify different age groups over which to aggregate.

``` r
calc_nqx(zzbr, agegr=c(0, 1)/12)  # neonatal
#>    tips        nqx          se       ci_l       ci_u
#> 1 10-14 0.04358764 0.004262693 0.03519631 0.05190598
#> 2   5-9 0.04590724 0.003799869 0.03843049 0.05332585
#> 3   0-4 0.03792144 0.003320563 0.03139119 0.04440766
calc_nqx(zzbr, agegr=c(1, 3, 5, 12)/12) # postneonatal
#>    tips        nqx          se       ci_l       ci_u
#> 1 10-14 0.10613868 0.007294180 0.09172741 0.12032129
#> 2   5-9 0.08366400 0.006097552 0.07163475 0.09553739
#> 3   0-4 0.05107115 0.003999245 0.04320031 0.05887724
calc_nqx(zzbr, agegr=c(0, 1, 3, 5, 12)/12) # infant (1q0)
#>    tips       nqx          se      ci_l       ci_u
#> 1 10-14 0.1451000 0.008870650 0.1275358 0.16231054
#> 2   5-9 0.1257305 0.006823068 0.1122547 0.13900166
#> 3   0-4 0.0870559 0.005070203 0.0770642 0.09693942
calc_nqx(zzbr, agegr=c(12, 24, 36, 48, 60)/12) # child (4q1)
#>    tips        nqx          se       ci_l       ci_u
#> 1 10-14 0.08905182 0.007189004 0.07485210 0.10303360
#> 2   5-9 0.07784223 0.005665474 0.06667098 0.08887977
#> 3   0-4 0.05894689 0.004529373 0.05002747 0.06778255
calc_nqx(zzbr, agegr=c(0, 1, 3, 5, 12, 24, 36, 48, 60)/12) # u5mr (5q0)
#>    tips       nqx          se      ci_l      ci_u
#> 1 10-14 0.2212304 0.011730185 0.1978969 0.2438851
#> 2   5-9 0.1937855 0.008292561 0.1773675 0.2098759
#> 3   0-4 0.1408711 0.006432798 0.1281701 0.1533871
```

Calculate annual <sub>5</sub>q<sub>0</sub> by calendar year (rather than
years preceding survey).

``` r
calc_nqx(zzbr, period=2005:2015, tips=NULL)
#>    period       nqx         se       ci_l      ci_u
#> 1    2005 0.1937695 0.01611970 0.16154821 0.2247525
#> 2    2006 0.1890020 0.01485224 0.15936341 0.2175956
#> 3    2007 0.1983320 0.01469757 0.16900148 0.2266273
#> 4    2008 0.1906183 0.01306713 0.16459769 0.2158285
#> 5    2009 0.1979731 0.01559065 0.16682639 0.2279554
#> 6    2010 0.1874172 0.01366295 0.16019216 0.2137596
#> 7    2011 0.1768661 0.01378438 0.14940096 0.2034444
#> 8    2012 0.1390004 0.01488779 0.10932074 0.1676910
#> 9    2013 0.1224668 0.01183607 0.09895918 0.1453611
#> 10   2014 0.1225079 0.01208800 0.09849310 0.1458829
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
calc_nqx(zzsib, agegr=seq(15, 50, 5), tips=c(0, 8), dob="mm4", dod="mm8")
#>   tips       nqx          se      ci_l      ci_u
#> 1  0-7 0.1766998 0.008637983 0.1595944 0.1934571
```

Calculate <sub>35</sub>q<sub>15</sub> by sex, replicating Table
MM2.2.

``` r
zzsib$sex <- factor(zzsib$mm1, c("female", "male"))  # drop mm2 = 3: "missing"
calc_nqx(zzsib, by=~sex, agegr=seq(15, 50, 5), tips=c(0, 8), dob="mm4", dod="mm8")
#>      sex tips       nqx         se      ci_l      ci_u
#> 1 female  0-7 0.1758983 0.01336771 0.1492771 0.2016864
#> 2   male  0-7 0.1772981 0.01264042 0.1521465 0.2017035
```

The above calculation does not exactly match the example tables. Female
<sub>35</sub>q<sub>15</sub> should be 175 rather than 176. This will be
reviewed as they should match, and additional functionality including
functions for producing ASMRs, MMR, and PM will be added in future.

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

  - Refactor jackknife code and implement for ASFR and TFR calculation.
  - Implement stratified jackknife.
  - Implement DHS child mortality calculation with rule-based allocation
    across age intervals.
  - Calculate multiple aggregations of nqx and sample correlation of
    these (e.g. NN, PN, 1q0, 4q1, 5q0).
  - Extend fertility calculation to accomodate birth list (long) data.
  - Document everything…
  - Settle on a better package name. I kind of like ‘demogr’
    (demographic rates / demography R), but this is very close to
    demogR. ‘demogrates’ is an option.
  - Develop, test, and create examples for non-DHS household surveys,
    e.g. MICS, WFS.
  - Extend interface to allow specification of formula, variable name,
    or vectors.
