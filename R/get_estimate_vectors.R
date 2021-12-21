#' This is some descriptio of this function.
#' @title get_estimate_vectors
#' 
#' @description print the ATE
#' 
#' @details Input the result from function 'cleanup_result', then print the ATE.
#' 
#' @param result the result of funciont 'cleanup_result'
#' @return Nothing will be returned.
#' @export

get_estimate_vectors <- function(result){
  
  for (i in 1:length(result)) {
    
    res <- result[[i]]
    #print(res$match_indicator)
    re_df <- res$re_df
    print(mean(re_df[,'effect']))
    
  }
}