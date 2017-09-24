create_birthhist_data <- function(br, country, survyear, strata="v022"){

  ## CASEID            (id) Case Identification                                1   15   AN    I    1    0   No   No"
  ## BIDX              (id) Birth column number                               16    2    N    I    1    0   No   No
  ## BORD                   Birth order number                               852    2    N    I    1    0   No   No
  ## B0                     Child is twin                                    854    1    N    I    1    0   No   No
  ## B1                     Month of birth                                   855    2    N    I    1    0   No   No
  ## B2                     Year of birth                                    857    4    N    I    1    0   No   No
  ## B3                     Date of birth (CMC)                              861    4    N    I    1    0   No   No
  ## B4                     Sex of child                                     865    1    N    I    1    0   No   No
  ## B5                     Child is alive                                   866    1    N    I    1    0   No   No
  ## B6                     Age at death                                     867    3    N    I    1    0   No   No
  ## B7                     Age at death (months, imputed)                   870    3    N    I    1    0   No   No
  ## B8                     Current age of child                             873    2

  br$country <- country
  br$survyear <- survyear
  br$stratum <- do.call(paste, br[strata])

  vars <- c("caseid", "country", "survyear", "stratum",
            "v000", "v001", "v002", "v003", "v005", "v007", "v008", "v011", "v012", "v013", "v021", "v022", "v023", "v024", "v025",
            "bidx", "bord", paste0("b", 0:8))

  br <- br[vars]

  br$intvy <- ifelse(br$v007 < 100, 1900+br$v007, br$v007)
  br$intvcmc <- br$v008

  br$sex <- br$b4
  br$sex <- replace(br$sex, br$sex==1, "male")
  br$sex <- replace(br$sex, br$sex==2, "female")
  br$sex <- factor(tolower(br$sex), c("male", "female"))

  br$deaths <- br$b5 == "no"

  br$weight <- br$v005/1e6

  br$dob <- br$b3
  br$dod <- br$b3+br$b7

  return(br)
}
