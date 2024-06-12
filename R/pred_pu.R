#' Calculate the Bayes classifer accurary for the PU data
#'
#' @param res_model the resulting model from the elpu2 or elpu3 functions.
#' @param dat_new the testing data set
#' @param pu_components this is an indicator about how many components do we assume: either 2 (SCAR) or 3 (SAR)
#' 
#' @return The resulting classification rate for the entire data and the unlabelled data only
#' @export

pred_pu <- function(res_model, dat_new, pu_components){
  dat_all <- dat_new %>% dplyr::select(starts_with("X")) %>% as.matrix()
  dat_unlabel <- dat_new %>% filter(R == 0) %>% dplyr::select(starts_with("X")) %>% 
    as.matrix()
  dat_label <- dat_new %>% filter(R == 1) %>% dplyr::select(starts_with("X")) %>% 
    as.matrix()
  dat_all_Y <- dat_new %>% mutate(Y = as.numeric(as.character(Y))) %>% pull(Y)
  dat_unlabel_Y <- dat_new %>% filter(R == 0) %>% 
    mutate(Y = as.numeric(as.character(Y))) %>% pull(Y)
  
  m <- nrow(dat_unlabel); n <- nrow(dat_label)
  gamma <- n/(n+m)
  piVal_hat <- res_model$piVal
  if(pu_components == 2){
    Lambda1_all <- 1
    Lambda2_all <- exp(res_model$alp + dat_all %*% res_model$beta)
  } else if(pu_components == 3){
    Lambda1_all <- exp(res_model$alp1 + dat_all %*% res_model$beta1)
    Lambda2_all <- exp(res_model$alp2 + dat_all %*% res_model$beta2)
  } else{
    stop("error message")
  }
  phi1_numerator <- gamma + (1-gamma)*piVal_hat*Lambda1_all
  phi1_deno <- gamma+(1-gamma)*piVal_hat*Lambda1_all + gamma+(1-gamma)*(1-piVal_hat)*Lambda2_all
  phi1 <- phi1_numerator/phi1_deno
  
  if(pu_components == 2){
    Lambda1_R0 <- 1
    Lambda2_R0 <- exp(res_model$alp + dat_unlabel %*% res_model$beta)
  } else if (pu_components == 3){
    Lambda1_R0 <- exp(res_model$alp1 + dat_unlabel %*% res_model$beta1)
    Lambda2_R0 <- exp(res_model$alp2 + dat_unlabel %*% res_model$beta2)
  }
  phi2 <- piVal_hat*Lambda1_R0/(piVal_hat*Lambda1_R0+(1-piVal_hat)* Lambda2_R0)
  
  C1 <- ifelse(phi1 > 0.5, 1, 0)
  C1_pred <- mean(C1 == dat_all_Y)
  C2 <- ifelse(phi2 > 0.5, 1, 0)
  C2_pred <- mean(C2 == dat_unlabel_Y)
  c(C1 = C1_pred, C2 = C2_pred)
}