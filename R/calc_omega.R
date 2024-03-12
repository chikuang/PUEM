#' Calculate the omegaVec under SAR/SCAR assumption
#'
#' @param dat a data matrix that contains all the observations
#' @param param a parameter vector that contains alp1, alp2, beta1, beta2 and values of pi
#' @param ind_SAR an indicator to see if it is under the SAR assumption or SCAR assumption
#' @return a vector that consists the values of omega
#' @export

calc_omega <- function(dat, param, ind_SAR = TRUE){
  if(ind_SAR){
    alp1 <- param$alp1; beta1 <- param$beta1
  } else {
    alp1 <- 0; beta1 <- rep(0, length(param$beta1))
  }

  alp2 <- param$alp2; beta2 <- param$beta2
  piVal <- param$piVal
  dat_unlabel <- extract_dat(dat, ind_label = 0)
  temp <- exp(alp2 - alp1 + dat_unlabel %*% (beta2 - beta1))
  omegaVec <- piVal/(piVal + (1-piVal) * temp)
  return(omegaVec)
}
