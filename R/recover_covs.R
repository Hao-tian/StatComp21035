#' This is some descriptio of this function.
#' @title recover_covs
#' 
#' @description compute the size and ATE of each iteration
#' 
#' @details Compute the size and ATE of each iteration.
#' 
#' @param d d is a dataframe that are sorted by function 'get_CATE_bit'
#' @param covs covs is the name of covs that are used in this iteration.
#' @param covs_max_list A a numric vector which shows the base system of cov.
#' @return A dataframe including size and ATE value.
#' @export


recover_covs <- function(d,covs,covs_max_list){
  
  n <- nrow(d)/2
  df <- data.frame(size=numeric(n),effect=numeric(n))
  
  for (i in 1:n) {
    df[i,1] <- d$size[2*i]+d$size[2*i-1]
    df[i,2] <- d$mean[2*i]-d$mean[2*i-1]
  }
  
  return(df)
}