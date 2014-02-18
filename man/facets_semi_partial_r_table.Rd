\name{facets_semi_partial_r_table}
\alias{facets_semi_partial_r_table}
\title{Semi-partial correlations between facets and criterion}
\usage{
  facets_semi_partial_r_table(dv, facets, factors, data)
}
\arguments{
  \item{dv}{character string of the criterion variable name
  in \code{data}}

  \item{facets}{character string of the facet variable
  names in \code{data}}

  \item{factors}{character string of the factor variable
  names in \code{data}. This string should be the same
  length as the facets list. Thus, factor names should
  repeat and correspond to the factor for the facet in the
  corresponding position in facets.}

  \item{data}{data.frame}
}
\value{
  Data frame of correlations \enumerate{ \item
  \code{r_zero_order}: zero order corelation \item
  \code{sr_focal_factor}: semi-partial correlation
  controlling only for focal factor \item
  \code{sr_all_factors}: semi-partial correlation
  controlling for Big 5 \item \code{sr_other_facets}:
  semi-partial correlation controlling for all other facets
  }
}
\description{
  Produce a data.frame of correlations between facets and
  criterion. Rows represent each facet and the four columns
  represent the zero-order correlation between facet and
  criterion followed by various semi-partial correlations
  controlling for assorted variables as described under the
  "Value" section.
}
\details{
  The table is useful for examining the degree to which
  facets provide a unique contribution to predicting a
  criterion. In particular, facets controlling for factors
  is particularly relevant to discussion regarding the
  incrmental value of facets.
}
\examples{
data(facets_data); data(facets_meta)
facets_semi_partial_r_table('swl', facets_meta$ipip_facets,
                           rep(facets_meta$ipip_factors, each=6), facets_data)
}

