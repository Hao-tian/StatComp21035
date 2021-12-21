## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----message=FALSE, warning=FALSE---------------------------------------------
library(StatComp21035)
library(dplyr)
data(df_for_test)
data(holdout_for_test)

covs <- colnames(df)[1:(ncol(df)-2)]
covs_max_list <- c(3,2,2,2,3,rep(2,length(covs)-5))

## ----message=FALSE, warning=FALSE---------------------------------------------
res <- run_bit(df,holdout,covs,covs_max_list)
cleanup <- cleanup_result(res$matching_res)
get_estimate_vectors(cleanup)

## ----message=FALSE, warning=FALSE---------------------------------------------
flame(df,holdout,covs,covs_max_list)

