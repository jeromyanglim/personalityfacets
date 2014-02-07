\name{adjusted_r_square}
\alias{adjusted_r_square}
\title{Calculate adjusted r square}
\usage{
  adjusted_r_square(rsquared, n, p, method = "ezekiel")
}
\arguments{
  \item{rsquared}{}

  \item{n}{sample size}

  \item{p}{number of predictors}

  \item{method}{for calculating r-squared (currently takes
  'ezekiel' or 'olkinpratt')}
}
\value{
  adjusted r-square
}
\description{
  Calculates adjusted r square
}
\examples{
rs <- summary(regression('swl', ivs= facets_meta$ipip_factors, data=facets_data))$r.squared
n <- nrow(facets_data)
p <- length(facets_meta$ipip_factors)
adjusted_r_square(rs, n, p, method='ezekiel')
adjusted_r_square(rs, n, p, method='olkinpratt')
}

