#' Calculate the Bayes classifer accurary for the PU data
#'
#' @param res_model the resulting model from the elpu2 or elpu3 functions.
#' @param dat_new the testing data set
#' @param pu_components this is an indicator about how many components do we assume: either 2 (SCAR) or 3 (SAR)
#'
#' @return The resulting classification rate for the entire data and the unlabelled data only
#' @export

pred_pu <- function(res_model, dat_test, pu_components){
  dat_test_X <- dat_new %>%
    dplyr::select(starts_with("X")) %>%
    as.matrix()
  dat_test_Y <- dat_new %>% filter(R == 0) %>%
    mutate(Y = as.numeric(as.character(Y))) %>% pull(Y)

  piVal_hat <- res_model$piVal
  if(pu_components == 2){
    Lambda1_R0 <- 1
    Lambda2_R0 <- exp(res_model$alp + dat_test_X %*% res_model$beta)
  } else if (pu_components == 3){
    Lambda1_R0 <- exp(res_model$alp1 + dat_test_X %*% res_model$beta1)
    Lambda2_R0 <- exp(res_model$alp2 + dat_test_X %*% res_model$beta2)
  } else {
      stop("error input for pu_components")
  }
  phi <- piVal_hat*Lambda1_R0/(piVal_hat*Lambda1_R0+(1-piVal_hat)* Lambda2_R0)

  pred_label <- ifelse(phi > 0.5, 1, 0)
  pred_accuracy <- mean(pred_label == dat_test_Y)
  return(c(pred_accuracy = pred_accuracy))
}
