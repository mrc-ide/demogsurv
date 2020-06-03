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

