#' Calculate the empirical likelihood under the assumption of DET-SAR model
#'
#' @param dat a data matrix that contains all the observations
#' @param param a parameter vector that contains alp1, alp2, beta1, beta2 and piVal
#' @param maxiter the maximum iteration number
#' @param tol the tolerance of the changing in likelihood
#' @param updatePiVal whether or not to update piVal
#' @param flip whether to flip the pi (proportion) or not
#' @return a list of values: piVal, alp1, alp2, beta1, beta2, llk and iteration number
#' @export

elpu3 <- function(dat, param, maxiter = 500, tol = 1e-4, updatePiVal = TRUE, flip = TRUE){
  alp1 <- param$alp1; beta1 <- param$beta1
  alp2 <- param$alp2; beta2 <- param$beta2
  piVal <- param$piVal
  iter <- 0
  err <- 1
  llk <- -1E5
  dat_all <- extract_dat(dat, ind_label = 2)
  dat_unlabel <- extract_dat(dat, ind_label = 0)
  dat_label <- extract_dat(dat, ind_label = 1)
  n <- nrow(dat_label)
  m <- nrow(dat_unlabel)

  while(err > tol & iter < maxiter){
    omegaVec <- calc_omega(dat, param, ind_SAR = TRUE)
    if(updatePiVal){
      piVal <- mean(omegaVec) # update pi
      param$piVal <- piVal
    }
    param <- calc_alp_beta3(dat, param, omegaVec)
    llk_new <- calc_EL3(dat, param, omegaVec)
    err <- abs(llk_new - llk)
    llk <- llk_new
    iter <- iter + 1
  }
  param$iter <- iter
  param$llk <- llk
  param
}
