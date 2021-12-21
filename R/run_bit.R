#' This is some descriptio of this function.
#' @title run_bit
#' 
#' @description run the FLAME algrithm
#' 
#' @details Drop one cov in each iteration until the prediction error is too large. Use the remaining covs to get the estiamte of ATE.
#' 
#' @param df df is a dataframe of dataset including covs, outcome and treated.
#' @param holdout holdout is a dataframe including data with the same distribution as df.
#' @param covs A subset of colnames of df, which shows the covs this match will use.
#' @param covs_max_list A a numric vector which shows the base system of cov.
#' @param tradeoff_param The parameter that trade off BF and PE.
#' @param printcov If TRUE, the name of used covs in each iteration will be printed.
#' 
#' @return A list with 2 units. One is the matching result in each iteration. Another is match quality score in each iteration.
#' @export

run_bit <- function(df, holdout, covs, covs_max_list, tradeoff_param = 0.1,printcov = TRUE){
  
  cur_covs <- covs
  cur_covs_max_list <- covs_max_list
  
  
  level = 1
  
  match_re <- match(df, covs, covs_max_list)# match without dropping anything
  match_indicator <- match_re$match_indicator
  index <- match_re$index
  
  res = get_CATE_bit(df, match_indicator, index) # get the CATEs without dropping anything
  
  matching_res <- list(list(cur_covs=cur_covs,
                            cur_covs_max_list=cur_covs_max_list,
                            score=NA,
                            match_indicator=match_indicator,
                            index=index,
                            res=res))
  
  df <- df[-match_indicator,]
  
  level_scores <- NA
  
  while (length(cur_covs)>1) {
    
    if(printcov)print(cur_covs)
    
    level = level + 1
    
    if(sum(as.integer(df[,'treated']==0))==0||sum(as.integer(df[,'treated']==1))==0){
      print ('no more matches');
      break;
    }
    
    score <- numeric(length = length(cur_covs))
    for (i in 1:length(cur_covs)) {
      
      cur_covs_no_c <- cur_covs[-i]
      cur_covs_max_list_no_c <- cur_covs_max_list[-i]
      
      match_indicator <- match(df, cur_covs_no_c, cur_covs_max_list_no_c)$match_indicator
      
      score[i] <-  match_quality(df, holdout, cur_covs_no_c, match_indicator, tradeoff=tradeoff_param)$score
      
    }#for end
    
    if(sum(as.integer(!is.na(score)))==0)break; # no more matches
    i <- which.max(score)# use the one with largest MQ(score) as the one to drop
    
    cur_covs_no_c <- cur_covs[-i]
    cur_covs_max_list_no_c <- cur_covs_max_list[-i]
    
    match_re <- match(df, cur_covs_no_c, cur_covs_max_list_no_c)
    match_indicator <- match_re$match_indicator
    index <- match_re$index
    
    match_quality_re <-  match_quality(df, holdout, cur_covs_no_c, match_indicator, tradeoff=tradeoff_param)
    score <- match_quality_re$score
    PE <- match_quality_re$PE
    if(abs(PE)>1.0)break; #set break PE value
    
    best_res <- list(cur_covs=cur_covs_no_c, 
                     cur_covs_max_list=cur_covs_max_list_no_c, 
                     score=score, 
                     match_indicator=match_indicator, 
                     index=index)
    
    level_scores <- c(level_scores,score)
    
    new_matching_res <-  get_CATE_bit(df, match_indicator, index)
    
    
    
    cur_covs = cur_covs_no_c # 更新用于循环
    cur_covs_max_list = cur_covs_max_list_no_c
    
    best_res[[6]] <- new_matching_res
    names(best_res)[6] <- "res"
    matching_res[[level]] <- best_res
    
    
    df = df[-match_indicator,]
    
  }#while end
  
  return(list(matching_res=matching_res, level_scores))
  
}