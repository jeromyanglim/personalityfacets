\name{semi_partial_r}
\alias{semi_partial_r}
\title{Semi partial correlation}
\usage{
  semi_partial_r(y, x, controls, data)
}
\arguments{
  \item{y}{character string of name of outcome variable in
  \code{data}}

  \item{x}{character string of name of predictor variable
  in \code{data}}

  \item{controls}{character vector of one or more names of
  control variables in \code{data}}

  \item{data}{data.frame}
}
\description{
  Semi-partial correlation between variables x and y, where
  overlap of x with \code{controls} is controlled for.
}
\examples{
data(facets_data); data(facets_meta)
semi_partial_r('swl', facets_meta$ipip_facets[1], facets_meta$ipip_factors, facets_data)
}

