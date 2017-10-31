# '@...  additional arguments to readfn()
read_zipdata <- function(zfile, pattern=".dta$", readfn=foreign::read.dta, ignore.case=TRUE, ...){
  tmp <- tempfile()
  on.exit(unlink(tmp))
  file <- grep(pattern, unzip(zfile, list=TRUE)$Name, ignore.case, value=TRUE)
  if(!length(file)){
    warning(paste0("File name matching pattern '", pattern, "' not found in zip file '", basename(zfile), "'."))
    return(invisible(NULL))
  }
  if(length(file) > 1)
    warning(paste0("Multiple file names match pattern '", pattern, "' in zip file '", basename(zfile), "'. Returning file '", file[1], "'."))
  return(readfn(unzip(zfile, file[1], exdir=tmp), ...))
}

read_zipdta <- function(zfile, ...){
  read_zipdata(zfile, ".dta$", foreign::read.dta, TRUE, ...)
}

find_dhsvar <- function(zfile, str="hdpidx", pattern=".MAP$", ignore.case=TRUE){
  map <- read_zipdata(zfile, pattern, readLines, TRUE)
  as.logical(length(grep(str, map, ignore.case)))
}
