\name{bootstrap_r_squared_change}
\alias{bootstrap_r_squared_change}
\title{Bootstrap estimate of popualtion r-square of difference between facets and factors}
\usage{
  bootstrap_r_squared_change(data, dv, ivs1, ivs2,
    iterations = 1000, ci = 0.95, method = "olkinpratt")
}
\arguments{
  \item{data}{data.frame}

  \item{dv}{character scalar with name of dependent
  variable}

  \item{ivs1}{character vector of variable names in
  \code{data} typically corresponding to personality
  factors}

  \item{ivs2}{character vector of variable names in
  \code{data} typically corresponding to personality
  facets}

  \item{iterations}{positive number indicating number of
  bootstrap iterations to run}

  \item{ci}{number between 0 and 1 indicating size of
  bootstrap confidence interval. Default value is .95
  representing a 95\% confidence interval.}

  \item{method}{See \code{method} argument for
  \link{adjusted_r_squared} function. Default value is
  \code{olkinpratt} which is designed where inference is
  desired to the situation where the predictor variables
  are assumed to be random.}
}
\value{
  an object of class boot_mean_cor_diff

  \item{variables, data, iterations, ci, method}{copy of
  corresponding arguments}
}
\description{
  The function provides a bootstrapped estimate with
  confidence intervals of of population r-squared change.
}
\details{
  Population r-square change is defined as
  \deqn{\Delta\rho^2 = \rho^2_{ivs2} - \rho^2_{ivs1}}
  Bootstrapping involves performing applying the formula
  for adjusted r-square twice, once to remove the bootstrap
  bias, and a second time to remove the bias inherent to
  the r-square formula. There are several methods available
  (See \code{method} argument for \link{adjusted_r_squared}
  function). However, in general if you assume that the
  predictors are random then \code{olkinpratt} is a good
  option. If you assume that the predictors are fixed, then
  \code{ezekiel} is a good option. A rough guide to number
  of iterations: 100 for basic error checking; 1,000 for
  exploratory analyses; 10,000 minimum recommended for
  publication; 100,000+ preferable for publication where
  computing power allows.
}
\examples{
data(facets_data); data(facets_meta)
# using 100 iterations is too few, but is used here to make example run quickly
bootstrap_r_squared_change(facets_data, facets_meta$swb[1], facets_meta$ipip_factors, facets_meta$ipip_facets, iterations=100)
}

