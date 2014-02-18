\name{unique_variance}
\alias{unique_variance}
\title{Unique variance}
\usage{
  unique_variance(y, x, data)
}
\arguments{
  \item{y}{character string of dependent variable of
  interest in \code{data}}

  \item{x}{character string of predictor variables in
  \code{data}}

  \item{data}{data.frame}
}
\description{
  Proportion of Variance in a variable not explained by a
  linear regression of a set of predictors (i.e., one minus
  r-squared).
}
\examples{
data(facets_data); data(facets_meta)
unique_variance(y='swl', x=facets_meta$ipip_factors, data=facets_data)
}

