\name{bootstrap_r_square_change}
\alias{bootstrap_r_square_change}
\title{Bootstrap estimate of popualtion r-square of difference between facets and factors}
\usage{
  bootstrap_r_square_change(data, dv, ivs1, ivs2,
    iterations = 1000, ci = 0.95, method = "ezekiel")
}
\arguments{
  \item{data}{data.frame}

  \item{dv}{character scalar with name of dependent
  variable}

  \item{ivs1}{character vector of names in data.frame
  typically corresponding to personality factors}

  \item{ivs2}{character vector of names in data.frame
  typically corresponding to personality facets}

  \item{iterations}{positive number indicating number of
  bootstrap iterations to run}

  \item{ci}{number between 0 and 1 indicating size of
  bootstrap confidence interval.}

  \item{method}{One of the 'ezekiel' or 'olkinpratt'
  Default value is .95 representing a 95\% confidence
  interval.}
}
\value{
  an object of class boot_mean_cor_diff
  \item{boot_results}{object of class boot resulting from
  bootstrapping analysis} \item{cor_matrix1}{numeric
  matrix: correlation matrix between set1 variables}
  \item{cor_matrix2}{numeric matrix: correlation matrix
  between set2 variables} \item{mean_cor1}{numeric scalar:
  average correlation between set1 variables}
  \item{mean_cor2}{numeric scalar: average correlation
  between set2 variables} \item{set1, set2, data,
  iterations, ci}{copy of corresponding arguments}
}
\description{
  The function bootstraps the difference by applying a
  double-adjusted-rsquare technique
}
\examples{
bootstrap_r_square_change(facets_data, facets_meta$swb[1], facets_meta$ipip_factors, facets_meta$ipip_facets)
}

