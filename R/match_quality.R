#' This is some descriptio of this function.
#' @title match_quality
#' 
#' @description compute the match quality score of one match
#' 
#' @details Compute the PE and BF, then return the score.
#' 
#' @param df df is a dataframe of dataset including covs, outcome and treated.
#' @param holdout holdout is a dataframe including data the same distribution with df.
#' @param covs_subset The names of covs that will be used in this match.
#' @param match_indicator the index of units that are matched
#' @param tradeoff The parameter that trade off BF and PE.
#' @return A list with 3 units. First is the score. Second is the balance factor. Third is the prediction error.
#' @export

match_quality <- function(df,holdout, covs_subset, match_indicator, tradeoff = 0.1){
  if(is.null(match_indicator))return(NA)
  
  num_control = sum(as.integer(df[,'treated']==0)) # how many control units that are unmatched (recall matched units are removed from the data frame)
  num_treated = sum(as.integer(df[,'treated']==1)) # how many treated units that are unmatched (recall matched units are removed from the data frame)
  
  
  num_control_matched = sum(as.integer(df[match_indicator,'treated']==0) ) # how many control units that are matched on this level
  num_treated_matched = sum(as.integer(df[match_indicator,'treated']==0) ) # how many treated units that are matched on this level
  
  
  # -- below is the regression part for PE
  
  holdout <- holdout[,c(covs_subset,'treated','outcome')]
  holdout_t <- holdout[holdout[,'treated']==1,]
  formu_t <- paste(colnames(holdout_t)[1:length(covs_subset)],collapse = "+")
  formu_t <- paste('outcome ~',formu_t)
  fit_t <- stats::lm(formula = formu_t, data = holdout_t)
  n_mse_t <- mean((fit_t$residuals)^2)#计算t的mse
  
  
  holdout_c <- holdout[holdout[,'treated']==0,]
  formu_c <- paste(colnames(holdout_c)[1:length(covs_subset)],collapse = "+")
  formu_c <- paste('outcome ~',formu_c)
  fit_c <- stats::lm(formula = formu_c, data = holdout_c)
  n_mse_c <- mean((fit_c$residuals)^2)#计算c的mse
  
  PE <- n_mse_t + n_mse_c
  BF <- num_control_matched/num_control + num_treated_matched/num_treated
  
  return(  list(score = tradeoff * BF -  PE, BF = BF, PE = PE )  )
  
}