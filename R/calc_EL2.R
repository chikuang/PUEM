#' Calculate the empirical likelihood under SCAR assumption
#'
#' @param dat a data matrix that contains all the observations
#' @param param a parameter vector that contains alp1, alp2, beta1, beta2 and values of pi
#' @param omegaVec the vector that determines weighting of the unlabeled positive
#' @return llk the log-likelihood value
#' @export
#'
calc_EL2 <- function(dat, param, omegaVec){
  alp <- param$alp2; beta <- param$beta2
  piVal <- param$piVal
  dat_all <- extract_dat(dat, ind_label = 2)
  dat_unlabel <- extract_dat(dat, ind_label = 0)
  n <- nrow(dat_all) - nrow(dat_unlabel)

  alp_star <- alp + log(sum(1-omegaVec)/(n + sum(omegaVec)))

  qi <- 1/(1 + exp(alp_star + dat_all %*% beta))
  qi <- qi/sum(qi) # normalise

  llk_part1 <- sum(log(qi))
  llk_part2 <- sum(log(piVal
                       + (1-piVal) * exp(alp + dat_unlabel %*% beta)))
  llk <- llk_part1 + llk_part2
  return(llk)
}


