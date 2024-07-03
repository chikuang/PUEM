#--- unexported utility functions ----------------------------------------------
#' importFrom("stats", "coef", "filter", "rbinom")

# check whether or not to flip the pi
#' @param piVal_true a vector that contains the true pi values
#' @param piVal_hat a vector that contains the estimates pi values
#' @return an indicator to show whether or not to flip the pi
.flip_ind <- function(piVal_true, piVal_hat) {
  cond_A <- (piVal_true < 0.5  & piVal_hat > 0.5)
  cond_B <- (piVal_true >= 0.5 & piVal_hat < 0.5)
  if(cond_A | cond_B){
    TRUE
  } else{
    FALSE
  }
}
