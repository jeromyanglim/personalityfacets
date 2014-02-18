\name{adjusted_r_squared}
\alias{adjusted_r_squared}
\title{Calculate adjusted r square}
\usage{
  adjusted_r_squared(rsquared, n, p,
    method = c("ezekiel", "smith", "olkinpratt", "pratt", "wherry1", "wherry2"))
}
\arguments{
  \item{rsquared}{value of r-squared from a regression
  model}

  \item{n}{sample size}

  \item{p}{number of predictors}

  \item{method}{character string indicating the method for
  calculating r-squared. One of "\code{ezekiel}" (default)
  "\code{olkinpratt}", "\code{pratt}", "\code{wherry1}",
  and "\code{wherry2}"}
}
\description{
  Calculates adjusted r square using various methods.
  \code{ezekiel} is the formula typically used in
  statistics packages (e.g., adj.r.squared in summary.lm).
  One recommendation is to use \code{olkinpratt} when the
  predictors are assumed to be random and \code{ezekiel}
  when predictors are assumed to be fixed. All included
  formulas are designed to estimate population \eqn{\rho^2}
  r-squared rather
}
\examples{
data(facets_data); data(facets_meta)
rs <- summary(regression('swl', ivs=facets_meta$ipip_factors, data=facets_data))$r.squared
n <- nrow(facets_data)
p <- length(facets_meta$ipip_factors)

adjusted_r_squared(rs, n, p, method='ezekiel')
adjusted_r_squared(rs, n, p, method='olkinpratt')
adjusted_r_squared(rs, n, p, method='pratt')
adjusted_r_squared(rs, n, p, method='wherry1')
adjusted_r_squared(rs, n, p, method='wherry2')

adjusted_r_squared(.2, 100, 1)
adjusted_r_squared(.2, 100, 5)
adjusted_r_squared(.2, 100, 10)
}
\references{
  Raju, N. S., Bilgic, R., Edwards, J. E., & Fleer, P. F.
  (1997). Methodology review: Estimation of population
  validity and cross-validity, and the use of equal weights
  in prediction. Applied Psychological Measurement, 21(4),
  291-305.

  Yin, P., & Fan, X. (2001). Estimating R2 shrinkage in
  multiple regression: A comparison of different analytical
  methods. The Journal of Experimental Education, 69(2),
  203-224.

  See also:
  \url{http://stats.stackexchange.com/a/63766/183}
}

