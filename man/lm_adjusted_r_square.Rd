\name{lm_adjusted_r_square}
\alias{lm_adjusted_r_square}
\title{Calculate adjusted r square}
\usage{
  lm_adjusted_r_square(fit, method)
}
\arguments{
  \item{fit}{object of class lm}

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
fit <- regression('swl', ivs= facets_meta$ipip_factors, data=facets_data)
lm_adjusted_r_square(fit, method='olkinpratt')
}

