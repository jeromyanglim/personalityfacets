\name{k_fold_r_squared}
\alias{k_fold_r_squared}
\title{Perform k-fold cross validation}
\usage{
  k_fold_r_squared(lmfit, ngroup = 10)
}
\arguments{
  \item{lmfit}{object of class lm}

  \item{ngroup}{integer. number of cross validation groups}
}
\value{
  vector including raw r-squared and cross-validated
  r-squared
}
\description{
  Perform k-fold cross validation
}
\examples{
data(facets_data); data(facets_meta)
fit <- regression('swl', ivs=facets_meta$ipip_factors, data=facets_data)
k_fold_r_squared(fit)
}

