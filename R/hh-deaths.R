## HV249                  Member of the HH died last 12 months             219    1    N    I    1    0   No   No
##                                0  No
##                                1  Yes
##                                8  DK
##                                9  Missing
##                           (na)    Not applicable
## HV250                  Number of members who died last 12 months        220    2    N    I    1    0   No   No
##                                1:7  
##                                8    DK
##                            (m) 99   Missing
##                           (na)      Not applicable

## HDPIDX                 Column number                                   4162    1    N    I    4    0   No   No
##                                1:7  
##                           (na)      Not applicable
## HD100                  Sex                                             4166    1    N    I    4    0   No   No
##                                1  Male
##                                2  Female
##                            (m) 9  Missing
##                           (na)    Not applicable
## HD101                  Age                                             4170    2    N    I    4    0   No   No
##                                0:94  
##                                95    95+
##                                98    Don't know
##                            (m) 99    Missing
##                           (na)       NotAppl
## HD102                  Sick at least 3 of 12 months before death       4178    1    N    I    4    0   No   No
##                                0  No
##                                1  Yes
##                                8  DK
##                            (m) 9  Missing
##                           (na)    Not applicable


## Person recode variables ##

## HV101                  Relationship to head                             271    2    N    I    1    0   No   No
##                                1   Head
##                                2   Wife or husband
##                                3   Son/daughter
##                                4   Son/daughter-in-law
##                                5   Grandchild
##                                6   Parent
##                                7   Parent-in-law
##                                8   Brother/sister
##                                9   Co-spouse
##                                10  Other relative
##                                11  Adopted/foster child
##                                12  Not related
##                                13  Niece/nephew by blood
##                                14  Niece/nephew by marriage
##                                98  DK
##                            (m) 99  Missing
## HV102                  Usual resident                                   273    1    N    I    1    0   No   No
##                                0  No
##                                1  Yes
##                            (m) 9  Missing
## HV103                  Slept last night                                 274    1    N    I    1    0   No   No
##                                0  No
##                                1  Yes
##                            (m) 9  Missing
## HV104                  Sex of household member                          275    1    N    I    1    0   No   No
##                                1  Male
##                                2  Female
##                            (m) 9  Missing
## HV105                  Age of household members                         276    2    N    I    1    0   No   No
##                                0:95  
##                                96    96+
##                                98    DK
##                            (m) 99    Missing


##  Issues:
##  - Adjust for half year ageing?
##  - How to handle non-regular HH members? (currently excluded)
##  - Unknown age / sex

create_hhdeaths_data <- function(hr, pr, country, survyear, strata="hv022"){

  hr$country <- country
  hr$survyear <- survyear
  hr$stratum <- do.call(paste, hr[strata])

  pr$country <- country
  pr$survyear <- survyear
  pr$stratum <- do.call(paste, pr[strata])
  
  widevars <- c("hhid", "country", "survyear", "stratum",
                "hv001", "hv002", "hv003", "hv005", "hv007", "hv008", "hv011", "hv012", "hv013", "hv021", "hv022", "hv023", "hv024", "hv025",
                "hv249", "hv250")
  longvars <- grep("^hd", names(hr), value=TRUE)
  varying <- tapply(longvars, sub("(.*)_.*", "\\1", longvars), c)

  hr <- hr[c(widevars, longvars)]

  ## Reshape wide to long
  hhdeaths <- reshape(hr[c(widevars, unlist(longvars))],
                      varying = varying,
                      v.names = names(varying),
                      idvar = "hhid",
                      timevar = "hdpidx",
                      direction = "long")

  ## Drop empty entries
  hhdeaths <- subset(hhdeaths, !is.na(hdpidx))

  hhdeaths$sex <- hhdeaths$hd100
  hhdeaths$sex <- replace(hhdeaths$sex, hhdeaths$sex==1, "male")
  hhdeaths$sex <- replace(hhdeaths$sex, hhdeaths$sex==2, "female")
  hhdeaths$sex <- factor(tolower(hhdeaths$sex), c("male", "female"))

  hhdeaths$age <- replace(hhdeaths$hd101, !hhdeaths$hd101 %in% 0:95, NA)
  
  hhdeaths$death <- TRUE


  ## Household members listing
  pr_vars <- c("hvidx", "hv101", "hv102", "hv103", "hv104", "hv105")
  pr <- pr[c(widevars, pr_vars)]

  pr <- subset(pr, hv102 %in% c(1, "yes")) # regular household members only
  pr$sex <- pr$hv104
  pr$sex <- replace(pr$sex, pr$sex==1, "male")
  pr$sex <- replace(pr$sex, pr$sex==2, "female")
  pr$sex <- factor(tolower(pr$sex), c("male", "female"))
  pr$age <- replace(pr$hv105, !pr$hv105 %in% 0:96, NA)
  pr$death <- FALSE

  dat <- rbind(pr[c(widevars, c("sex", "age", "death"))],
               hhdeaths[c(widevars, c("sex", "age", "death"))])
  dat$weight <- dat$hv005 / 1e6
  dat$pys <- 1-0.5*dat$death  # deceased persons count 0.5 PYs

  return(dat)
}
