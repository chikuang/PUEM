#' Calculate the empirical likelihood under SAR assumption
#'
#' @param dat a data matrix that contains all the observations
#' @param param a parameter vector that contains alp1, alp2, beta1, beta2 and values of pi
#' @param omegaVec the vector that determines weighting of the unlabeled positive
#' @return llk the log-likelihood value
#' @export
calc_EL3 <- function(dat, param, omegaVec){
  alp1 <- param$alp1; beta1 <- param$beta1
  alp2 <- param$alp2; beta2 <- param$beta2
  piVal <- param$piVal
  dat_all <- extract_dat(dat, ind_label = 2)
  dat_unlabel <- extract_dat(dat, ind_label = 0)
  n <- nrow(dat_all) - nrow(dat_unlabel)
  alp1_star <- alp1 + log(sum(omegaVec)/n)
  alp2_star <- alp2 + log(sum(1 - omegaVec)/n)

  qi <- 1/(1 + exp(alp1_star + dat_all %*% beta1) + exp(alp2_star + dat_all %*% beta2))
  qi <- qi/sum(qi) # normalise

  llk_part1 <- sum(log(qi))
  llk_part2 <- sum(log(piVal * exp(alp1 + dat_unlabel %*% beta1 )
                       + (1-piVal) * exp(alp2 + dat_unlabel %*% beta2)))
  llk <- llk_part1 + llk_part2
  return(llk)
}
