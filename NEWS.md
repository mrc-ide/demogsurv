# demogsurve 0.2.6

* Add option `varmethod = "none"` for `calc_tfr()`.

# demogsurve 0.2.5

* Lots of cleanup on package elements and documentation to pass R CMD CHECK (thanks @chacalle).
* Remove `data.table` from vignettes and Suggests.

# demogsurv 0.2.4

* Bugfix in `calc_nqx()` to allow user-specified column name for death outcome variable (issue #11; thanks @JoseAntonioOrtega).

# demogsurv 0.2.3

* Internal fix `calc_asfr()` to address `tmerge()` error: `idname option must be a valid variable name`.


# demogsurv 0.2.2

* Patch to `jacknife()` function for case where person-years appear in only one
  cluster, resulting in zero denominator for some jacknife replicates and NaN 
  variance estimate.

# demogsurv 0.2.1

* Implement separate jackknife() function including stratified jackknife.
* Add option `varmethod = "none"` to calculate point estimates but no variance 
  calculations. Especially useful if you're just using 
  `calc_asfr(..., counts = TRUE)` to get weighted births and PYs.

# demogsurv 0.2.0

* Added a `NEWS.md` file to track changes to the package.

