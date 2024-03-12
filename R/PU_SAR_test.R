#' Calculate the p-value for the test that $beta_1=0$, which means the the PU data us from the SAR model
#'
#' @param dat a data matrix that contains all the observations
#' @param param a parameter vector that contains alp1, alp2, beta1, beta2 and piVal
#' @return a list of values: piVal, alp1, alp2, beta1, beta2, llk and iteration number
#' importFrom("stats", "pchisq")
#' @export

PU_SAR_test <- function(dat, param){
  p <- length(param$beta1)
  res3 <- elpu3(dat, param)
  res2 <- elpu2(dat, param)
  chi_val <- 2*(res3$llk - res2$llk)
  return(list(llk2 = res2$llk, llk3 = res3$llk,
              pval = pchisq(chi_val, p, lower.tail = FALSE)))
}
