#' Unique variance
#' 
#' Proportion of Variance in a variable not explained by a linear regression of a set of predictors
#' (i.e., one minus r-squared).
#' 
#' @param y character string of dependent variable of interest in \code{data}
#' @param x character string of predictor variables in \code{data}
#' @param data data.frame
#' @export
#' @examples
#' data(facets_data); data(facets_meta)
#' unique_variance(y='swl', x=facets_meta$ipip_factors, data=facets_data)
unique_variance <- function(y, x, data) {
    # calculate 1 minus r-square for y predicted from x
    Formula <- formula(paste(y, '~',  paste(x, collapse=" + ")))
    fit <- lm(Formula, data=data)
    s_fit <- summary(fit)
    1 - s_fit$r.squared
}