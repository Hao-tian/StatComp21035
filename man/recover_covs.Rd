% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/recover_covs.R
\name{recover_covs}
\alias{recover_covs}
\title{recover_covs}
\usage{
recover_covs(d, covs, covs_max_list)
}
\arguments{
\item{d}{d is a dataframe that are sorted by function 'get_CATE_bit'}

\item{covs}{covs is the name of covs that are used in this iteration.}

\item{covs_max_list}{A a numric vector which shows the base system of cov.}
}
\value{
A dataframe including size and ATE value.
}
\description{
compute the size and ATE of each iteration
}
\details{
This is some descriptio of this function.

Compute the size and ATE of each iteration.
}
