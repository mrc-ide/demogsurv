
#' Retrieve data from DHS API
#'
#' @references https://api.dhsprogram.com/rest/dhs/info/citation
fetch_dhsapi <- function(url){
  npages <- jsonlite::fromJSON(url)$TotalPages
  dat <- lapply(seq_len(npages), function(i) jsonlite::fromJSON(paste0(url, "&page=", i))$Data)
  dat <- do.call(rbind, datasets)
  return(dat)
}

#' Get the DHS country code
#'
#' @references \url{https://dhsprogram.com/data/File-Types-and-Names.cfm}
get_dhs_cc <- function(filename){
  substr(basename(filename), 1, 2)
}


#' Get the DHS phase
#'
#' @references \url{https://dhsprogram.com/data/File-Types-and-Names.cfm}
get_dhs_phase <- function(filename){
  as.integer(substr(basename(filename), 5, 5))
}

#' Get the DHS survey number in each phase
#'
#' @references \url{https://dhsprogram.com/data/File-Types-and-Names.cfm}
get_dhs_survnum <- function(filename){
  code <- substr(basename(filename), 6, 6)
  ifelse(code %in% 0:9, 1,
  ifelse(code %in% LETTERS[1:8], 2, # A-H
  ifelse(code %in% LETTERS[9:17], 3, # I-Q
  ifelse(code %in% LETTERS[18:26], 4, NA)))) # R-Z
}

survey_info <- function(filename){
  info <- data.frame(filename = filename,
                     CC = get_dhs_cc(filename),
                     phase = get_dhs_phase(filename),
                     survnum = get_dhs_survnum(filename),
                     stringsAsFactors=FALSE)
  return(info)
}
