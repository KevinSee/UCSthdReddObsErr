% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/estimateRedds.R
\name{estimateRedds}
\alias{estimateRedds}
\title{Estimate Redds}
\usage{
estimateRedds(
  redd_df = NULL,
  group_vars = c(River, Reach, Index, SurveyType),
  new_redd_nm = "NewRedds",
  vis_redd_nm = "VisibleRedds",
  net_err_nm = "NetError",
  net_se_nm = "NetErrorSE",
  min_non0_wks = 3,
  min_redds = 2,
  add_zeros = F,
  ...
)
}
\arguments{
\item{redd_df}{dataframe containing redd data}

\item{new_redd_nm}{quoted name of column in {redd_df} listing number of new redds found during that survey}

\item{vis_redd_nm}{quoted name of column in {redd_df} listing number of visible redds present during that survey}

\item{net_err_nm}{quoted name of column in {redd_df} listing estimate of net error for that survey}

\item{net_se_nm}{quoted name of column in {redd_df} listing standard error of net error estimate for that survey}

\item{min_non0_wks}{minimum number of weeks with at least one new redd observed}

\item{min_redds}{minimum number of total redds observed}

\item{add_zeros}{should leading and trailing zero counts be added if the first and last counts aren't already zero? Default is {FALSE}.}

\item{group_var}{vector of column names from {redd_df} to group results by}
}
\value{
dataframe
}
\description{
Estimates true number of redds across a series of reaches, using a Gaussian area-under-the-curve model where possible and incorporating estimates of observer error if possible.
}
\author{
Kevin See
}
