#' This is some descriptio of this function.
#' @title flame
#' 
#' @description match units according to the algrithm of FLAME
#' 
#' @details First calculate the bit-vector value. Then count the number of times they appear. Judge whether they are equal.
#' 
#' @param df df is a dataframe of dataset including covs, outcome and treated.
#' @param holdout holdout is a dataframe including data with the same distribution as df.
#' @param covs A subset of colnames of df, which shows the covs this match will use.
#' @param covs_max_list A a numric vector which shows the base system of cov.
#' @param tradeoff_param The parameter that trade off BF and PE.
#' @param printcov If TRUE, the name of used covs in each iteration will be printed.
#' @return the final estimate ATE value
#' @export

flame <- function(df, holdout, covs, covs_max_list, tradeoff_param = 0.1,printcov = FALSE){
  res <- run_bit(df,holdout,covs,covs_max_list,tradeoff_param,printcov = FALSE)
  cleanup <- cleanup_result(res$matching_res)
  ate <- get_estimate_vectors(cleanup)
  return(ate[length(ate)])
}