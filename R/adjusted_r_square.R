#' @title Calculate adjusted r square
#' 
#' @description Calculates adjusted r square using various methods. 
#' ezekiel is the formula typically used in statistics packages (e.g., adj.r.squared in summary.lm).
#' olkinpratt is useful if you are drawing inference to a situation where the predictors are assumed to be random, 
#' whereas ezekiel makes sense where 
#' 
#' @param rsquared value of r-squared from a regression model
#' @param n sample size
#' @param p number of predictors
#' @param method for calculating r-squared (currently takes 'ezekiel' or 'olkinpratt')
#' @references Raju, N. S., Bilgic, R., Edwards, J. E., & Fleer, P. F. (1997). 
#' Methodology review: Estimation of population validity and cross-validity, and the 
#' use of equal weights in prediction. Applied Psychological Measurement, 21(4), 291-305.
#' See also: \url{http://stats.stackexchange.com/a/63766/183}
#' @export
#' @examples
#' data(facets_data); data(facets_meta)
#' rs <- summary(regression('swl', ivs=facets_meta$ipip_factors, data=facets_data))$r.squared
#' n <- nrow(facets_data)
#' p <- length(facets_meta$ipip_factors)
#' adjusted_r_squared(rs, n, p, method='ezekiel')
#' adjusted_r_squared(rs, n, p, method='olkinpratt')
adjusted_r_squared <- function(rsquared, n, p, method='ezekiel') {
    if (method %in% c('ezekiel', 'smith')) {
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
        part1 <- ((n - 3) * (1- rsquared)) / (n - p - 1)
        part2 <- 1 + (2 * (1 - rsquared))/ (n - p - 2.3)
        return(
            1 - part1 * part2
        )
    }
}

#' @title Calculate adjusted r squared
#' 
#' @description Calculates adjusted r squared
#' 
#' @param fit object of class lm
#' @param method for calculating r-squared (currently takes 'ezekiel' or 'olkinpratt')
#' @return adjusted r-squared
#' @export
#' @examples
#' data(facets_data); data(facets_meta)
#' fit <- regression('swl', ivs= facets_meta$ipip_factors, data=facets_data)
#' lm_adjusted_r_squared(fit, method='olkinpratt')
lm_adjusted_r_squared <- function(fit, method) {
    rsquared <- summary(fit)$r.squared
    n <- length(fit$residuals)
    # minus 1 to remove the intercept
    p <- length(fit$coefficients) - 1 
    adjusted_r_squared(rsquared=rsquared, n=n, p=p, method=method)
}   


