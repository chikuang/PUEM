#' Calculate the empirical likelihood under the assumption of DET-SCAR model
#'
#' @param dat a data matrix that contains all the observations
#' @param param a parameter vector that contains alp1, alp2, beta1, beta2 and piVal
#' @param maxiter the maximum iteration number
#' @param tol the tolerance of the changing in likelihood
#' @param updatePiVal whether or not to update piVal
#' @param flip whether to flip the pi (proportion) or not
#' @return a list of values: piVal, alp1, alp2, beta1, beta2, llk and iteration number
#' @export

elpu2 <- function(dat, param, maxiter = 500, tol = 1e-4, updatePiVal = TRUE, flip = TRUE){
  alp <- param$alp2; beta <- param$beta2
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
    temp <- exp(alp + dat_unlabel %*% beta )
    omegaVec <- piVal/(piVal + (1-piVal) * temp)
    rm(temp)
    if(updatePiVal){
      piVal <- mean(omegaVec) # update pi
      param$piVal <- piVal
    }
    # update alpha and beta
    param <- calc_alp_beta2(dat, param, omegaVec)
    llk_new <- calc_EL2(dat, param, omegaVec)
    ## calculate EL

    err <- abs(llk_new - llk)
    llk <- llk_new
    iter <- iter + 1
  }
  param$iter <- iter
  param$llk <- llk
  param
}
