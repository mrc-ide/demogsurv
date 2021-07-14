# this is needed for non-standard evaluation Multiple links suggest using
# `utils::globalVariables` to remove notes when checking the package.
# https://www.r-bloggers.com/no-visible-binding-for-global-variable/
# https://stackoverflow.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when
# https://community.rstudio.com/t/how-to-solve-no-visible-binding-for-global-variable-note/28887
utils::globalVariables(c(
  # calc_asfr
  "id_", "(dob)", "(intv)", "event", "bcmc",

  # calc_nqx
  "tstop",

  # create_hhdeaths_data
  "hdpidx", "hv102",

  # calc_dhs_mx
  "sex", "deaths", "agecens", "agestart", "agegr",

  # create_tips_data
  "sex", "deaths", "tipscens", "tipsstart", "tcens", "tstart",

  # create_sib_data
  "mmidx"
  ))
