library(rdhs)
library(haven)

zfile <- tempfile()

download.file("https://dhsprogram.com/customcf/legacy/data/sample_download_dataset.cfm?Filename=ZZBR62FL.ZIP&Tp=1&Ctry_Code=zz&survey_id=0&doctype=dhs", zfile)
zzbr62fl <- rdhs::read_dhs_flat(zfile)
zzbr62fl <- haven::as_factor(zzbr62fl)

## download.file("https://dhsprogram.com/customcf/legacy/data/sample_download_dataset.cfm?Filename=ZZCR61FL.ZIP&Tp=1&Ctry_Code=zz&survey_id=0&doctype=dhs", zfile)
## zzcr61fl <- rdhs::read_dhs_flat(zfile)
## zzcr61fl <- haven::as_factor(zzcr61fl)

## download.file("https://dhsprogram.com/customcf/legacy/data/sample_download_dataset.cfm?Filename=ZZHR62FL.ZIP&Tp=1&Ctry_Code=zz&survey_id=0&doctype=dhs", zfile)
## zzhr62fl <- rdhs::read_dhs_flat(zfile)
## zzhr62fl <- haven::as_factor(zzhr62fl)

download.file("https://dhsprogram.com/customcf/legacy/data/sample_download_dataset.cfm?Filename=ZZIR62FL.ZIP&Tp=1&Ctry_Code=zz&survey_id=0&doctype=dhs", zfile)
zzir62fl <- rdhs::read_dhs_flat(zfile)
zzir62fl <- haven::as_factor(zzir62fl)

## download.file("https://dhsprogram.com/customcf/legacy/data/sample_download_dataset.cfm?Filename=ZZKR62FL.ZIP&Tp=1&Ctry_Code=zz&survey_id=0&doctype=dhs", zfile)
## zzkr62fl <- rdhs::read_dhs_flat(zfile)
## zzkr62fl <- haven::as_factor(zzkr62fl)

## download.file("https://dhsprogram.com/customcf/legacy/data/sample_download_dataset.cfm?Filename=ZZMR61FL.ZIP&Tp=1&Ctry_Code=zz&survey_id=0&doctype=dhs", zfile)
## zzmr61fl <- rdhs::read_dhs_flat(zfile)
## zzmr61fl <- haven::as_factor(zzmr61fl)

## download.file("https://dhsprogram.com/customcf/legacy/data/sample_download_dataset.cfm?Filename=ZZPR62FL.ZIP&Tp=1&Ctry_Code=zz&survey_id=0&doctype=dhs", zfile)
## zzpr62fl <- rdhs::read_dhs_flat(zfile)
## zzpr62fl <- haven::as_factor(zzpr62fl)

## download.file("https://dhsprogram.com/customcf/legacy/data/sample_download_dataset.cfm?Filename=ZZAR61FL.ZIP&Tp=1&Ctry_Code=zz&survey_id=0&doctype=hiv", zfile)
## zzar61fl <- rdhs::read_dhs_flat(zfile)
## zzar61fl <- haven::as_factor(zzar61fl)

## usethis::use_data(zzir62fl, zzbr62fl, zzmr61fl, zzar61fl,
##                   zzcr61fl, zzhr62fl, zzkr62fl, zzpr62fl)


## Reduce the variables saved to facilitate faster package loading
zzir <- zzir62fl[grep("caseid|^v0|^v1|^v2|^b|^mm", names(zzir62fl), value=TRUE)]
zzbr <- zzbr62fl[grep("caseid|^v0|^v1|^b", names(zzbr62fl), value=TRUE)]

usethis::use_data(zzir, zzbr, overwrite=TRUE)
