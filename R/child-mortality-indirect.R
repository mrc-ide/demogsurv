#' Calculate under-five mortality rates based on indirect demographic techniques
#'
#' This function estimates under-five mortality rates from information on the mean number of children ever born and children still alive reported by women classified by age group, or classified by time since first birth. Default arguments are configured to calculate under 5 mortality
#' from a DHS Individual Recode file. Unstratified jackknife is used for variance calculation.v 
#'
#' @param data A dataset (data.frame), for example a DHS Individual recode (IR) dataset.
#' @param by A formula specifying factor variables by which to stratify analysis.
#' @param clusters Formula or data frame specifying cluster ids from largest level to smallest level, '~0' or '~1' is a formula for no clusters.
#' @param strata Formula or vector specifying strata, use 'NULL' for no strata.
#' @param weights Formula or vector specifying sampling weights.
#' @param intv Variable name for interview date (character string).
#' @param dob Variable name for date of birth of the mother (character string). 
#' @param dfb Variable name for the date of first birth, only needed for using data classified by the time since first birth (character string). 
#' @param ceb Variable name for the number of children ever born (character string). 
#' @param csurv Variable name for the number of children surviving (character string).
#' @param method Character string specifying the variant of the method to be used: currently, "ma" for the classical maternal age variant, and "tsfb" for using data classified by the time since first birth. 
#' @param family Model life table family to use to (1) translate proportion dead into nq0 values and (2) translate age-specific nq0 into U5MR: currently, "West", "North", "South", "East" for Princeton regional model life tables. 
#' @param origin Origin year for date arguments. 1900 for CMC inputs.
#' @param sex Character string specifying if sex-specific estimates should be produced ("combined", "males", "females") 
#'
#' @references
#' Hill, K. Indirect estimation of child mortality. In: Moultrie, T., et al. (eds.). \emph{Tools for demographic estimation}, 2013, Paris: IUSSP, 148-164, available at \url{http://demographicestimation.iussp.org/content/indirect-estimation-child-mortality}
#' 
#' Verhulst, A. Child mortality estimation: An assessment of summary birth history methods using microsimulation, \emph{Demographic Research}, 2016, 34, 1075-1128
#' 
#' @examples
#'
#' data(zzir)
#'
#' ## Calculate 5q0 for both sexes based on the maternal age variant, using DHS
#' zzir$ceb <- zzir$v201
#' zzir$csurv <- zzir$v201 - zzir$v206 - zzir$v207
#' calc_u5mr_indirect(zzir, family = "Princeton-West")
#' 
#' ## Calculate 5q0 for males only based on the maternal age variant, using DHS
#' zzir$ceb_m <- zzir$v202 + zzir$v204 + zzir$v206
#' zzir$csurv_m <- zzir$ceb_m - zzir$v206
#' calc_u5mr_indirect(zzir, ceb = "ceb_m", csurv = "csurv_m",
#'                   family = "Princeton-West", sex = "males")
#' 
#' ## Calculate 5q0 for females only based on the maternal age variant, using DHS
#' zzir$ceb_f <- zzir$v203 + zzir$v205 + zzir$v207
#' zzir$csurv_f <- zzir$ceb_f - zzir$v207
#' calc_u5mr_indirect(zzir, ceb = "ceb_f", csurv = "csurv_f",
#'                   family = "Princeton-West", sex = "females")
#' 
#' ## 5q0 by sociodemographic characteristics
#' calc_u5mr_indirect(zzir, by=~v102) # by urban/rural residence
#'
#' ## indirect 5q0 using time since first birth variant
#' calc_u5mr_indirect(zzir, method = "tsfb") 
#'
#' @export
#' @md
calc_u5mr_indirect <- function(data,
                              by = NULL,
                              clusters=~v021,
                              strata=~v024+v025,
                              weight= "v005",
                              intv = "v008",
                              dob = "v011",
                              dfb = "v211",
                              ceb = "ceb",
                              csurv = "csurv",
                              origin=1900,
                              method = "ma",
                              family = "Princeton-West",
                              sex = "combined"){
  
  data$weights <- data[[weight]] / mean(data[[weight]])
  data$intv <- data[[intv]]
  data$ceb <- data[[ceb]]
  data$csurv <- data[[csurv]]
  data$dob <- data[[dob]]
  if(method == "tsfb") { 
    data$dfb <- data[[dfb]]
  }
  
  ## First classification variable: 5 year age groups (needed for both methods)   
  data <- data[data$intv - data$dob >= 180, ]
  data$classvar <- NA
  data$classvar <- factor(trunc(c(data$intv-data$dob)/(12*5))*5,
                          levels = seq(15, 45, 5), labels = paste('ma', seq(15, 45, 5), seq(15, 45, 5)+4, sep = "-"))
  
  ## Cloning agegr to use in demog_rates
  data$agegr <- as.factor(0)
  agegr <- c(0,1)
  
  ## Adding factor variables by which to stratify analysis.
  if(is.null(by))
    by <- ~classvar
  else
    by <- update(as.formula(by), ~ . + classvar)
   
  vars <- unique(unlist(lapply(c(by, strata, clusters), all.vars)))
   
  byvar <- intersect(c(all.vars(by), "classvar", "agegr"),
                     names(data))
  data$byf <- do.call("interaction", c(data[byvar], drop=TRUE))

  ## prediction for all factor levels that appear
  pred <- data.frame(data[c(byvar, "byf")])[!duplicated(data$byf),]
  pred <- pred[order(pred$byf), ]
  
  ## Matrix to keep the structure of calc_nqx()
  dfmm <- .mm_aggr(pred[byvar], agegr)
  mm <- dfmm$mm
  
  ## Using weights
  data$cebw <- data$ceb * data$weights
  data$csurvw <- data$csurv*data$weights
  data$countw <- data$weights # Weighted count to compute average numbers of children ever born/surviving 
       
  ceb_clust <- reshape2::acast(data, update(clusters, byf ~ .), fun.aggregate = sum, value.var="cebw")
  csurv_clust <- reshape2::acast(data, update(clusters, byf ~ .),  fun.aggregate = sum,  value.var="csurvw")
  count_clust <- reshape2::acast(data, update(clusters, byf ~ .),  fun.aggregate = sum,  value.var="countw")
       
  ceb_clust[is.na(ceb_clust)] <- 0
  csurv_clust[is.na(csurv_clust)] <- 0
  count_clust[is.na(count_clust)] <- 0
  
  ceb_all <- rowSums(ceb_clust)
  csurv_all <- rowSums(csurv_clust)
  count_all <- rowSums(count_clust)
  
  ## Parity ratios
  p1p2_all <- (ceb_all[grep("ma-15-19", names(ceb_all))]/count_all[grep("ma-15-19", names(count_all))])/(ceb_all[grep("ma-20-24", names(ceb_all))]/count_all[grep("ma-20-24", names(count_all))])
  p2p3_all <- (ceb_all[grep("ma-20-24", names(ceb_all))]/count_all[grep("ma-20-24", names(count_all))])/(ceb_all[grep("ma-25-29", names(ceb_all))]/count_all[grep("ma-25-29", names(count_all))])
       
  ## now remove one cluster
  ceb_jack <- ceb_all - ceb_clust
  dimnames(ceb_jack) <- dimnames(ceb_clust)
  csurv_jack <- csurv_all - csurv_clust
  dimnames(csurv_jack) <- dimnames(csurv_clust)
  count_jack <- count_all - count_clust
  dimnames(count_jack) <- dimnames(count_clust)
         
  ## array of parity ratios 
  p1p2_jack <- (ceb_jack[grep("ma-15-19", dimnames(ceb_jack)[[1]]),]/count_jack[grep("ma-15-19", dimnames(count_jack)[[1]]),])/(ceb_jack[grep("ma-20-24", dimnames(ceb_jack)[[1]]),]/count_jack[grep("ma-20-24", dimnames(count_jack)[[1]]),])
  p2p3_jack <- (ceb_jack[grep("ma-20-24", dimnames(ceb_jack)[[1]]),]/count_jack[grep("ma-20-24", dimnames(count_jack)[[1]]),])/(ceb_jack[grep("ma-25-29", dimnames(ceb_jack)[[1]]),]/count_jack[grep("ma-25-29", dimnames(count_jack)[[1]]),])
  
  if(method == "ma") { # Maternal age -> compute U5MR and produce output 
    ## Multipliers ki and timing ti
    ki_all <- ti_all <- ceb_all
    ki_all[1:length(ki_all)] <- NA
    ti_all[1:length(ti_all)] <- NA
    for(i in c("ma-15-19", "ma-20-24", "ma-25-29", "ma-30-34", "ma-35-39", "ma-40-44", "ma-45-49") ){
      ki_all[grep(i, names(ki_all))] <- multipliers.princeton['a(x,j)',i,family]+multipliers.princeton['b(x,j)',i,family]*p1p2_all + multipliers.princeton['c(x,j)',i,family]*p2p3_all
      ti_all[grep(i, names(ki_all))] <- multipliers.princeton['e(x,j)',i,family]+multipliers.princeton['f(x,j)',i,family]*p1p2_all + multipliers.princeton['g(x,j)',i,family]*p2p3_all
    } 
    ## Match age group with nq0
    corr <- as.data.frame(cbind(c("ma-15-19","ma-20-24","ma-25-29","ma-30-34","ma-35-39","ma-40-44", "ma-45-49"),
                                c("q(1)", "q(2)", "q(3)", "q(5)", "q(10)", "q(15)", "q(20)")))
    names(corr) <- c("ma", "nq0")
    ## qi, alpha, U5MR for all sample
    qi_all = ((ceb_all - csurv_all) / ceb_all)*ki_all
    alpha_all <- u5mr_all <- qi_all
    alpha_all[1:length(alpha_all)] <- NA
    u5mr_all[1:length(u5mr_all)] <- NA
    for(i in c("ma-15-19", "ma-20-24", "ma-25-29", "ma-30-34", "ma-35-39", "ma-40-44", "ma-45-49") ){
      n <- as.character(corr[corr$ma == i, "nq0"])
      alpha_all[grep(i, names(qi_all))] <- 0.5*(log(qi_all[grep(i, names(qi_all))]/(1-qi_all[grep(i, names(qi_all))])))-lts.model[sex,n,family]
      u5mr_all[grep(i, names(u5mr_all))] <- exp(2*(alpha_all[grep(i, names(alpha_all))]+lts.model[sex,"q(5)",family]))/(1+exp(2*(alpha_all[grep(i, names(alpha_all))]+lts.model[sex,"q(5)",family])))
    }
    
    ## array of pi (proportion dead of children born to women) with single cluster removed
    pi_jack <- (ceb_jack/count_jack - csurv_jack/count_jack) / (ceb_jack/count_jack)
    nc <- ncol(pi_jack)
    ## Multipliers ki and timing ti
    ki_jack <- ti_jack <- ceb_jack
    ki_jack[1:nrow(ki_jack), 1:ncol(ki_jack)] <- NA
    ti_jack[1:nrow(ti_jack), 1:ncol(ti_jack)] <- NA
    for(i in c("ma-15-19", "ma-20-24", "ma-25-29", "ma-30-34", "ma-35-39", "ma-40-44", "ma-45-49") ){
      ki_jack[grep(i, rownames(ki_jack)),] <- multipliers.princeton['a(x,j)',i,family]+multipliers.princeton['b(x,j)',i,family]*p1p2_jack + multipliers.princeton['c(x,j)',i,family]*p2p3_jack
      ti_jack[grep(i, rownames(ki_jack)),] <- multipliers.princeton['e(x,j)',i,family]+multipliers.princeton['f(x,j)',i,family]*p1p2_jack + multipliers.princeton['g(x,j)',i,family]*p2p3_jack
    } 
    ## qi, alpha, U5MR  
    qi_jack <- ((ceb_jack - csurv_jack) / ceb_jack)*ki_jack
    alpha_jack <- u5mr_jack  <- qi_jack
    alpha_jack[1:nrow(alpha_jack),1:ncol(alpha_jack)] <- NA
    u5mr_jack[1:nrow(u5mr_jack),1:ncol(u5mr_jack)] <- NA
    for(i in c("ma-15-19", "ma-20-24", "ma-25-29", "ma-30-34", "ma-35-39", "ma-40-44", "ma-45-49") ){
      n <- as.character(corr[corr$ma == i, "nq0"])
      alpha_jack[grep(i, rownames(qi_jack)),] <- 0.5*(log(qi_jack[grep(i, rownames(qi_jack)),]/(1-qi_jack[grep(i, rownames(qi_jack)),])))-lts.model[sex,n,family]
      u5mr_jack[grep(i, rownames(u5mr_jack)),] <- exp(2*(alpha_jack[grep(i, rownames(alpha_jack)),]+lts.model[sex,"q(5)",family]))/(1+exp(2*(alpha_jack[grep(i, rownames(alpha_jack)),]+lts.model[sex,"q(5)",family])))
    }
    u5mr <- drop(u5mr_all %*% mm)
    u5mr_jack <- t(mm) %*% u5mr_jack
    lv.u5mr <- (nc-1)/nc * (u5mr_jack - u5mr) %*% t((u5mr_jack - u5mr))
    attr(u5mr, "var") <- lv.u5mr
    attr(u5mr, "statistic") <- "u5mr"
    class(u5mr) <- "svystat"
    
    val <- dfmm$df
    val$classvar <- gsub('ma-', '', val$classvar)
    names(val) <- sub('classvar', 'ma', names(val))
    val$ref <- as.vector((weighted.mean(data$intv, data$weight)-1)/12+origin)-drop(ti_all %*% mm)
    val$u5mr <- u5mr
    val$se.u5mr <- sqrt(diag(lv.u5mr))
    val$ci_l.u5mr <- u5mr - qnorm(0.975)*sqrt(diag(lv.u5mr))
    val$ci_u.u5mr <- u5mr + qnorm(0.975)*sqrt(diag(lv.u5mr))
    
    rownames(val) <- NULL
    
    val
    
  } else if(method == "tsfb"){ # Time since first birth - keep only P1/P2 and P2/P3 ratios and start again

    data$classvar <- NA
    data$classvar <- trunc(c(data$intv-data$dfb)/(12*5))*5
    data$classvar[is.na(data$classvar) | data$classvar >= 35] <- 30 # not used for computation
    data$classvar <- factor(data$classvar,
                            levels = seq(0, 30, 5),
                            labels = paste('tsfb', seq(0, 30, 5), seq(0, 30, 5)+4, sep = "-"))
    
    ## Cloning agegr to use in demog_rates
    data$agegr <- as.factor(0)
    agegr <- c(0,1)
    
    ## Adding factor variables by which to stratify analysis.
    if(is.null(by))
      by <- ~classvar
    else
      by <- update(as.formula(by), ~ . + classvar)
    
    vars <- unique(unlist(lapply(c(by, strata, clusters), all.vars)))
    
    byvar <- intersect(c(all.vars(by), "classvar", "agegr"),
                       names(data))
    data$byf <- do.call("interaction", c(data[byvar], drop=TRUE))

    ## prediction for all factor levels that appear
    pred <- data.frame(data[c(byvar, "byf")])[!duplicated(data$byf),]
    pred <- pred[order(pred$byf), ]
    
    ## Matrix to keep the structure of calc_nqx()
    dfmm <- .mm_aggr(pred[byvar], agegr)
    mm <- dfmm$mm
    
    ## Using weights
    data$cebw <- data$ceb*data$weights
    data$csurvw <- data$csurv*data$weights
    data$countw <- data$weights # Weighted count to compute average numbers of children ever born/surviving 
    
    ceb_clust <- reshape2::acast(data, update(clusters, byf ~ .), fun.aggregate = sum, value.var="cebw")
    csurv_clust <- reshape2::acast(data, update(clusters, byf ~ .),  fun.aggregate = sum,  value.var="csurvw")
    count_clust <- reshape2::acast(data, update(clusters, byf ~ .),  fun.aggregate = sum,  value.var="countw")
    
    ceb_clust[is.na(ceb_clust)] <- 0
    csurv_clust[is.na(csurv_clust)] <- 0
    count_clust[is.na(count_clust)] <- 0
    
    ceb_all <- rowSums(ceb_clust)
    csurv_all <- rowSums(csurv_clust)
    count_all <- rowSums(count_clust)
    
    ## now remove one cluster
    ceb_jack <- ceb_all - ceb_clust
    dimnames(ceb_jack) <- dimnames(ceb_clust)
    csurv_jack <- csurv_all - csurv_clust
    dimnames(csurv_jack) <- dimnames(csurv_clust)
    count_jack <- count_all - count_clust
    dimnames(count_jack) <- dimnames(count_clust)
    
    ## Multipliers ki and timing ti
    ki_all <- ti_all <- ceb_all
    ki_all[1:length(ki_all)] <- NA
    ti_all[1:length(ti_all)] <- NA
    for(i in c("tsfb-0-4","tsfb-5-9","tsfb-10-14","tsfb-15-19","tsfb-20-24") ){
      ki_all[grep(i, names(ki_all))] <- multipliers.tsfb['a(x,j)',i,family]+multipliers.tsfb['b(x,j)',i,family]*p1p2_all + multipliers.tsfb['c(x,j)',i,family]*p2p3_all
      ti_all[grep(i, names(ki_all))] <- multipliers.tsfb['e(x,j)',i,family]+multipliers.tsfb['f(x,j)',i,family]*p1p2_all + multipliers.tsfb['g(x,j)',i,family]*p2p3_all
    } 
    ## Match age group with nq0
    corr <- as.data.frame(cbind(c("tsfb-0-4","tsfb-5-9","tsfb-10-14","tsfb-15-19","tsfb-20-24"),
                                c("q(2)", "q(5)", "q(5)", "q(5)", "q(10)")))
    names(corr) <- c("tsfb", "nq0")
    ## qi, alpha, U5MR for all sample
    qi_all <- ((ceb_all - csurv_all) / ceb_all)*ki_all
    alpha_all <- u5mr_all <- qi_all
    alpha_all[1:length(alpha_all)] <- NA
    u5mr_all[1:length(u5mr_all)] <- NA
    for(i in c("tsfb-0-4", "tsfb-5-9", "tsfb-10-14", "tsfb-15-19", "tsfb-20-24") ){
      n <- as.character(corr[corr$tsfb == i, "nq0"])
      alpha_all[grep(i, names(qi_all))] <- 0.5*(log(qi_all[grep(i, names(qi_all))]/(1-qi_all[grep(i, names(qi_all))])))-lts.model[sex,n,family]
      u5mr_all[grep(i, names(u5mr_all))] <- exp(2*(alpha_all[grep(i, names(alpha_all))]+lts.model[sex,"q(5)",family]))/(1+exp(2*(alpha_all[grep(i, names(alpha_all))]+lts.model[sex,"q(5)",family])))
    }
         
    ## array of pi (proportion dead of children born to women) with single cluster removed
    pi_jack <- (ceb_jack/count_jack - csurv_jack/count_jack) / (ceb_jack/count_jack)
    nc <- ncol(pi_jack)

    ## Multipliers ki and timing ti
    ki_jack <- ti_jack <- ceb_jack
    ki_jack[1:nrow(ki_jack), 1:ncol(ki_jack)] <- NA
    ti_jack[1:nrow(ti_jack), 1:ncol(ti_jack)] <- NA
    for(i in c("tsfb-0-4", "tsfb-5-9", "tsfb-10-14", "tsfb-15-19", "tsfb-20-24") ){
      ki_jack[grep(i, rownames(ki_jack)),] <- multipliers.tsfb['a(x,j)',i,family]+multipliers.tsfb['b(x,j)',i,family]*p1p2_jack + multipliers.tsfb['c(x,j)',i,family]*p2p3_jack
      ti_jack[grep(i, rownames(ki_jack)),] <- multipliers.tsfb['e(x,j)',i,family]+multipliers.tsfb['f(x,j)',i,family]*p1p2_jack + multipliers.tsfb['g(x,j)',i,family]*p2p3_jack
    } 

    ## qi, alpha, U5MR  
    qi_jack <- ((ceb_jack - csurv_jack) / ceb_jack)*ki_jack
    alpha_jack <- u5mr_jack  <- qi_jack
    alpha_jack[1:nrow(alpha_jack),1:ncol(alpha_jack)] <- NA
    u5mr_jack[1:nrow(u5mr_jack),1:ncol(u5mr_jack)] <- NA
    for(i in c("tsfb-0-4","tsfb-5-9","tsfb-10-14","tsfb-15-19","tsfb-20-24") ){
      n <- as.character(corr[corr$tsfb == i, "nq0"])
      alpha_jack[grep(i, rownames(qi_jack)),] <- 0.5*(log(qi_jack[grep(i, rownames(qi_jack)),]/(1-qi_jack[grep(i, rownames(qi_jack)),])))-lts.model[sex,n,family]
      u5mr_jack[grep(i, rownames(u5mr_jack)),] <- exp(2*(alpha_jack[grep(i, rownames(alpha_jack)),]+lts.model[sex,"q(5)",family]))/(1+exp(2*(alpha_jack[grep(i, rownames(alpha_jack)),]+lts.model[sex,"q(5)",family])))
    }
    
    u5mr_all[is.na(u5mr_all)]<-0
    u5mr <- drop(u5mr_all %*% mm)
    u5mr_jack[is.na(u5mr_jack)]<-0
    u5mr_jack <- t(mm) %*% u5mr_jack
    lv.u5mr <- (nc-1)/nc * (u5mr_jack - u5mr) %*% t((u5mr_jack - u5mr))
    attr(u5mr, "var") <- lv.u5mr
    attr(u5mr, "statistic") <- "u5mr"
    class(u5mr) <- "svystat"
    
    val <- dfmm$df
    val$classvar <- gsub('tsfb-', '', val$classvar)
    names(val) <- sub('classvar', 'tsfb', names(val))
    ti_all[is.na(ti_all)] <- 0
    val$ref <- as.vector((weighted.mean(data$intv, data$weight)-1)/12+origin)-drop(ti_all %*% mm)
    val$u5mr <- u5mr
    val$se.u5mr <- sqrt(diag(lv.u5mr))
    val$ci_l.u5mr <- u5mr - qnorm(0.975)*sqrt(diag(lv.u5mr))
    val$ci_u.u5mr <- u5mr + qnorm(0.975)*sqrt(diag(lv.u5mr))
    
    rownames(val) <- NULL
    
    val[1:5,]
    
  } else
    stop(paste0("method = \"", method, "\" is not recognized."))
}


#' Princeton multipliers 
#'
#' A dataset containing coefficients developed for the UN Manual X (1983) for estimating probabilities of dying by exact ages nq0 from proportions dead of children ever born classified by age of mother
#'
#' @format An array with 6 rows and 7 columns (for 7 age groups of mothers) and 4 different families
#' \describe{
#'   \item{a(x,j), b(x,j), c(x,j)}{Coefficients used to estimate nq0 from proprtions dead of children ever born}
#'   \item{e(x,j), f(x,j), g(x,j)}{Coefficients used for estimating the reference periods}
#' }
#' @references
#' Verhulst, A. Child mortality estimation: An assessment of summary birth history methods using microsimulation, \emph{Demographic Research}, 2016, 34, 1075-1128
#'
#' Hill, K. Indirect estimation of child mortality. In: Moultrie, T., et al. (eds.). \emph{Tools for demographic estimation}, 2013, Paris: IUSSP, 148-164, available at \url{http://demographicestimation.iussp.org/content/indirect-estimation-child-mortality}
#'
"multipliers.princeton"


#' Logit of lx values of  model life tables used to convert nq0 into U5MR estimates 
#'
#' A dataset containing logits of the lx values of standard life tables for exact ages 1 to 20 to convert age-specific nq0 into U5MR
#'
#' @format An array with 3 rows and 7 columns (for 7 age groups of mothers) and 9 different families of model life tables
#' \describe{
#'   \item{q(1), q(2), q(3), q(5), q(10), q(15), q(20)}{Logits of the lx, the number of survivors at age x, where x is 1,2,3...}
#' }
#' @references
#' Moultrie, T., et al. (eds.). \emph{Tools for demographic estimation}, 2013, Paris: IUSSP, 148-164, available at \url{http://demographicestimation.iussp.org/content/indirect-estimation-child-mortality}
#' 
#' Verhulst, A. Child mortality estimation: An assessment of summary birth history methods using microsimulation, \emph{Demographic Research}, 2016, 34, 1075-1128
#'
"lts.model"
