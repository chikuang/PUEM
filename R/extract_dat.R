#' Extract the data of whether they are labeled or unlabeled
#'
#' @param dat a data matrix that contains all the observations
#' @param ind_label an indicator to determine whether to extract the label or unlabeled data
#' @return a matrix with all data that are either labeled or unlabeled
#' @export

extract_dat <- function(dat, ind_label = 2){
  if(ind_label == 2){
    dat_temp <- dat
  } else if(ind_label == 0) {
    ind <- 0
    dat_temp <- dat %>% dplyr::filter(dat$R == ind)
  } else if(ind_label == 1) {
    ind <- 1
    dat_temp <- dat %>% dplyr::filter(dat$R == ind)
  } else{
    stop("This is an error message")
  }
  dat_temp %>%
    dplyr::select(dplyr::starts_with("X")) %>%
    as.matrix()
}
