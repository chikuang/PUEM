#--- unexported utility functions ----------------------------------------------
#' importFrom("stats", "coef", "filter", "rbinom")

# check whether or not to flip the pi
#' @param piVal_true a vector that contains the true pi values
#' @param piVal_hat a vector that contains the estimates pi values
#' @return an indicator to show whether or not to flip the pi
.flip_ind <- function(piVal_true, piVal_hat){
  cond_A <- (piVal_true < 0.5  & piVal_hat > 0.5)
  cond_B <- (piVal_true >= 0.5 & piVal_hat < 0.5)
  if(cond_A | cond_B){
    TRUE
  } else{
    FALSE
  }
}

# to sim some data for demonstration purpose
.sim_data <- function(n, m, piVal, mu1, mu2, mu3){
  r_ind <- stats::rbinom(m, 1, piVal)
  m1 <- sum(r_ind) # unobserved while it is positive, i.e. y = 1|r=0
  m0 <- m - m1
  sigma1 <- sigma2 <- sigma3 <- diag(rep(1, length(mu1)))
  x1 <- MASS::mvrnorm(n = n, mu = mu1, Sigma = sigma1) # labelled
  x2 <- MASS::mvrnorm(n = m1, mu = mu2, Sigma = sigma2) # unlabeled positive
  x3 <- MASS::mvrnorm(n = m0, mu = mu3, Sigma = sigma3) # unlabeled negative
  data.frame(rbind(x1, x2, x3),
             R = as.factor(c(rep(1, n), rep(0, m))),
             Y = as.factor(c(rep(1, n + m1), rep(0, m0))))
}
