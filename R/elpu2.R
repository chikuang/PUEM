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

elpu2 <- function(dat, param, maxiter = 500, tol = 1e-8, updatePiVal = TRUE, flip = TRUE) {
  alp <- param$alp; beta <- param$beta
  piVal <- param$piVal

  iter <- 0
  err <- 1
  llk <- -1E5
  dat_all <- dat %>% dplyr::select(starts_with("X")) %>% as.matrix()
  dat_unlabel <- dat %>% filter(R == 0) %>% dplyr::select(starts_with("X")) %>%
    as.matrix()
  dat_label <- dat %>% filter(R == 1) %>% dplyr::select(starts_with("X")) %>%
    as.matrix()
  n <- nrow(dat_label)
  m <- nrow(dat_unlabel)
  while(err > tol & iter < maxiter){
    temp <- exp(alp + dat_unlabel %*% beta )
    omegaVec <- piVal/(piVal + (1-piVal) * temp)
    rm(temp)
    if(updatePiVal){
      piVal <- mean(omegaVec) # update pi
    }
    # update alpha and beta
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
    theta <- coef(fit_model)
    # rm(fit_model)

    alp_star <- theta[1]
    beta <- theta[-1]

    alp <- alp_star - log(sum(1-omegaVec)/(n + sum(omegaVec)))

    ## calculate EL
    qi <- 1/(1 + exp(alp_star + dat_all %*% beta))
    qi <- qi/sum(qi) # normalise

    llk_part1 <- sum(log(qi))
    llk_part2 <- sum(log(piVal
                         + (1-piVal) * exp(alp + dat_unlabel %*% beta)))
    llk_new <- llk_part1 + llk_part2
    err <- abs(llk_new - llk)
    llk <- llk_new
    iter <- iter + 1
  }
  list(piVal = piVal,
       alp = alp, beta = beta,
       llk = llk, iter = iter)
}
