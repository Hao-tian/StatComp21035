#' This is some descriptio of this function.
#' @title cleanup_result
#' 
#' @description clean up the results of function 'run_bit'
#' 
#' @details Use the result from 'run_bit'. Then use function 'recover_covs' to get ATE.
#' 
#' @param res_all a list from function 'run_bit'
#' @return A list with 2 units. One is the rowindex of df, which shows the units that are matched. Another is a dataframe contains the size and the ate from each iteration of FLAME.
#' @export




cleanup_result <- function(res_all){
  res <- list()
  
  for (i in 1:length(res_all)) {
    
    r <- res_all[[i]]#r为一个列表 含有6个带名字的元素
    if(!is.na(r$res))res[[i]] <- list(match_indicator=r$match_indicator,
                                      re_df=recover_covs( r$res, r$cur_covs, r$cur_covs_max_list ))
    
  }
  
  return(res)
}