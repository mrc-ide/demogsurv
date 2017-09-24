# '@...  additional arguments to readfn()
read_zipdata <- function(zfile, pattern=".dta$", readfn=foreign::read.dta, ignore.case=TRUE, ...){
  tmp <- tempfile()
  on.exit(unlink(tmp))
  return(readfn(unzip(zfile, grep(pattern, unzip(zfile, list=TRUE)$Name, ignore.case, value=TRUE), exdir=tmp), ...))
}

read_zipdta <- function(zfile, ...){
  read_zipdata(zfile, ".dta$", foreign::read.dta, TRUE, ...)
}

find_dhsvar <- function(zfile, str="hdpidx", pattern=".MAP$", ignore.case=TRUE){
  map <- read_zipdata(zfile, pattern, readLines, TRUE)
  as.logical(length(grep(str, map, ignore.case)))
}
