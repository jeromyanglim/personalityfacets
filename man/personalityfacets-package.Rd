\docType{package}
\name{personalityfacets-package}
\alias{personalityfacets}
\alias{personalityfacets-package}
\title{The personalityfacets Package}
\description{
  This package provides various functions for examining the
  relationship between personality facets, factors, and
  criteria.
}
\details{
  Package performs double adjuste rsquare bootstrap on
  r-square change. The two main functions are
  \link{bootstrap_r_squared_change} and
  \link{facets_semi_partial_r_table}.

  \link{bootstrap_r_squared_change} can be used to obtain
  bootstrap confidence intervals on the population r-square
  change. There are also various supporting functions.
  \link{regression} is a convient way of calling a
  regression with many predictors.
  \link{adjusted_r_squared} and
  \link{lm_adjusted_r_squared} provides several options for
  obtaining adjusted r-squared.

  \link{facets_semi_partial_r_table} decomposes the
  relationship between a set of facets and a criterion. It
  does this by presenting a range of semi-partial
  correlations.

  Additional functionality to explore facets and factors
  may be added in the future.
}
\author{
  Jeromy Anglim
}
\references{
  Anglim, J., Grant, S. Estimating Incremental Criterion
  Prediction of Personality Facets over Factors.
  \url{http://dx.doi.org/10.6084/m9.figshare.917189}
}

