#' Calculate the alpha and beta values for SCAR model
#'
#' @param dat a data matrix that contains all the observations
#' @param param a parameter vector that contains alp1, alp2, beta1, beta2 and values of pi
#' @param omegaVec the value of the expected....
#' @return a vector that consists the updated parameter values of alp1, alp2, beta1 and beta2
#' @export

calc_alp_beta2 <- function(dat, param, omegaVec) {
  alp <- param$alp2; beta <- param$beta2

  dat_label <- extract_dat(dat, 1)
  dat_unlabel <- extract_dat(dat, 0)
  n <- nrow(dat_label)
  m <- nrow(dat_unlabel)

  fake_dat_x <- rbind(dat_label, dat_unlabel, dat_unlabel)
  # fake_dat_z <- as.factor(c(rep(1, n), rep(c(2, 3), each = m)))
  fake_dat_z <- c(rep(1, n), rep(1, m), rep(2, m))
  my_weight <- c(rep(1, n), omegaVec, 1 - omegaVec)
  ind_break <- FALSE
  ind_break <- tryCatch(
    expr = {
      fit_model <- glmnet::glmnet(x = fake_dat_x,
                                  y = fake_dat_z, family = "binomial",
                                  weights = my_weight,
                                  lambda = 0,
                                  lower.limits = -15, upper.limits=15)
    },
    error = function(e){
      print("ERROR GEEZ")
      return(TRUE)
    },
    warning = function(w){
      print(" Warning, OMG")
      return(TRUE)
    })

  if(isTRUE(ind_break)){
    piVal <- iter <- llk <- alp <- beta <- NA
    print("MY EXIT")
    return(list(piVal = NA,
                alp = NA, beta = NA,
                llk = NA, iter = NA))
  }
  theta <- stats::coef(fit_model)

  alp_star <- theta[1]
  beta <- theta[-1]

  alp <- alp_star - log(sum(1-omegaVec)/(n + sum(omegaVec)))

  return(list(alp1 = 0, alp2 = alp, beta1 = rep(0, length(beta)),
              beta2 = beta, piVal = param$piVal))
}
