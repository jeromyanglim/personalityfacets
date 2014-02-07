#' @title Calculate adjusted r square
#' 
#' @description Calculates adjusted r square 
#' 
#' @param rsquared
#' @param n sample size
#' @param p number of predictors
#' @param method for calculating r-squared (currently takes 'ezekiel' or 'olkinpratt')
#' @return adjusted r-square
#' @export
#' @examples
#' rs <- summary(regression('swl', ivs= facets_meta$ipip_factors, data=facets_data))$r.squared
#' n <- nrow(facets_data)
#' p <- length(facets_meta$ipip_factors)
#' adjusted_r_square(rs, n, p, method='ezekiel')
#' adjusted_r_square(rs, n, p, method='olkinpratt')
adjusted_r_square <- function(rsquared, n, p, method='ezekiel') {
    if (method=='ezekiel') {
        return( 
            1 - (1-rsquared)  * ((n-1)/(n-p-1))
        )
    }
    if (method=='olkinpratt') {
        return(
        1 - ((n-3)/(n-p-1)) * (1-rsquared) * phyper(1, 1, (n-p + 1)/2, 1-rsquared)
        )
    }
    
    if (method=='pratt') {
        part1 <- ((n - 3) * (1- rsq)) / (n - p - 1)
        part2 <- 1 + (2 * (1 - rsq))/ (n - p - 2.3)
        1 - part1 * part2
    }
}

lm_adjusted_r_square <- function(fit, method) {
    fit <- regression(dv='swl', ivs=facets_meta$ipip_facets, data=facets_data)
    rsquared <- summary(fit)$r.squared
    n <- length(fit$residuals)
    # minus 1 to remove the intercept
    p <- length(fit$coefficients) - 1 
    adjusted_r_square(rsquared=rsquared, n=n, p=p, method=method)
}   


