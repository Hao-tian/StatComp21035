#' This is some descriptio of this function.
#' @title get_CATE_bit
#' 
#' @description group units by bit-vector value and 'treated' 
#' 
#' @details group units by bit-vector value and 'treated' so that ATE will be easier to get
#' 
#' @param df df is a dataframe of dataset including covs, outcome and treated.
#' @param match_indicator the index of units that are matched
#' @param index the bit-vector value
#' @return a dataframe including size and mean-value
#' @export



get_CATE_bit <- function(df, match_indicator, index){
  if(is.na(index))return(NA)
  
  d <- cbind(df[match_indicator,c("treated","outcome")],index=index,size=rep(1,length(index)))
  
  res <- dplyr::summarise(group_by(.data = d,index,treated), size=sum(size),mean=mean(outcome))
  #res <- dplyr::summarise(.data = d, size=sum('size'),mean=mean('outcome'),.groups = c('index','treated'))
  
  res <- res[,c('size','mean')]
  return(res)
}