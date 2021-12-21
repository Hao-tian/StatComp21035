#' This is some descriptio of this function.
#' @title match
#' 
#' @description match units according to the algrithm of FLAME
#' 
#' @details First calculate the bit-vector value. Then count the number of times they appear. Judge whether they are equal.
#' 
#' @param df df is a dataframe of dataset including covs, outcome and treated.
#' @param covs A subset of colnames of df, which shows the covs this match will use.
#' @param covs_max_list A a numric vector which shows the base system of cov.
#' @return A list with 2 units. One is the rowindex of df, which shows the units that are matched. Another is the bit-vector value of these units.
#' @export

match <- function(df,covs,covs_max_list){
  
  # this function takes a dataframe, a set of covariates to match on, 
  # the treatment indicator column and the matched indicator column.
  # it returns the array indicating whether each unit is matched (the first return value), 
  # and a list of indices for the matched units (the second return value)
  
  arr_slice_wo_t <- df[,covs]
  arr_slice_w_t <- df[,c(covs,'treated')]
  
  m <- length(covs)
  n <- nrow(df)
  
  names(covs_max_list) <- 1:length(covs_max_list)
  covs_max_list_sorted <- sort(covs_max_list)
  power <- numeric(m)
  for (i in 1:m) {
    power[i] <- which(names(covs_max_list_sorted)==as.character(i))
  }
  
  wo <- matrix(numeric(length = m),ncol = 1)
  for (i in 1:m) {
    wo[i,1] <- covs_max_list[i]^(power[i]-1)
  }
  lidx_wo_t <- as.matrix(arr_slice_wo_t) %*% wo    
  w <- matrix(numeric(length = m+1),ncol = 1)
  w[m+1,1] <- 1
  for (i in 1:m) {
    w[i,1] <- covs_max_list[i]^power[i]
  }
  lidx_w_t <- as.matrix(arr_slice_w_t) %*% w
  
  
  t_wo <- table(lidx_wo_t) 
  c <- numeric(n)
  for (i in 1:n) {
    c[i] <- t_wo[which(as.character(lidx_wo_t[i])==names(t_wo))]
  }
  t_w <- table(lidx_w_t) 
  c_plus <- numeric(n)
  for (i in 1:n) {
    c_plus[i] <- t_w[which(as.character(lidx_w_t[i])==names(t_w))]
  }
  
  match_indicator <- c()
  for (i in 1:n) {
    if(c[i]!=c_plus[i])match_indicator <- c(match_indicator,i)
  }
  
  
  
  return(list(match_indicator = match_indicator, index = lidx_wo_t[match_indicator]))
  
}

