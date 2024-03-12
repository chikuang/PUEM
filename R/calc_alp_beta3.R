#' Calculate the omegaVec
#'
#' @param dat a data matrix that contains all the observations
#' @param param a parameter vector that contains alp1, alp2, beta1, beta2 and values of pi
#' @param omegaVec the value of the expected....
#' @return a vector that consists the updated parameter values of alp1, alp2, beta1 and beta2
#' @export

calc_alp_beta3 <- function(dat, param, omegaVec){
  alp1 <- param$alp1; beta1 <- param$beta1
  alp2 <- param$alp2; beta2 <- param$beta2

  dat_label <- extract_dat(dat, 1)
  dat_unlabel <- extract_dat(dat, 0)
  n <- nrow(dat_label)
  m <- nrow(dat_unlabel)
  fake_dat_x <- rbind(dat_label, dat_unlabel, dat_unlabel)
  # fake_dat_z <- as.factor(c(rep(1, n), rep(c(2, 3), each = m)))
  fake_dat_z <- c(rep(1, n), rep(2, m), rep(3, m))
  my_weight <- c(rep(1, n), omegaVec, 1 - omegaVec)


  ind_break <- FALSE
  ind_break <- tryCatch(
    expr = {
      fit_model <- glmnet::glmnet(x = fake_dat_x,
                                  y = fake_dat_z, family = "multinomial",
                                  weights = my_weight,
                                  lambda = 0,
                                  lower.limits = -15, upper.limits=15)
    },
    error = function(e){
      # (Optional)
      # print("ERROR GEEZ")
      return(TRUE)
    },
    warning = function(w){
      # break
      # print(" Warning, OMG")
      return(TRUE)
    })

  if(isTRUE(ind_break)){
    piVal <- iter <- llk <- alp2 <- alp1 <- beta2 <- beta1 <- NA
    # print("MY EXIT")
    return(list(piVal = NA,
                alp1 = NA, beta1 = NA,
                alp2 = NA, beta2 = NA,
                llk = NA, iter = NA))
  }
  coefMat <- stats::coef(fit_model)
  # rm(fit_model)
  theta <- rbind(as.numeric(coefMat$`2`-coefMat$`1`),
                 as.numeric(coefMat$`3`-coefMat$`1`) )
  beta1 <- theta[1, -1]
  beta2 <- theta[2, -1]
  alp1_star <- theta[1, 1]
  alp2_star <- theta[2, 1]

  alp1 <- alp1_star - log(sum(omegaVec)/n)
  alp2 <- alp2_star - log(sum(1 - omegaVec)/n)
  return(list(alp1 = alp1, alp2 = alp2, beta1 = beta1, beta2 = beta2, piVal = param$piVal))
}
