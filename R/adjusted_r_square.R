#' @title Calculate adjusted r square
#' 
#' @description Calculates adjusted r square using various methods. 
#' \code{ezekiel} is the formula typically used in statistics packages (e.g., adj.r.squared in summary.lm).
#' One recommendation is to use \code{olkinpratt} when the predictors are assumed to be random and \code{ezekiel} when predictors are assumed to be fixed.
#' All included formulas are designed to estimate population \eqn{\rho^2} r-squared rather
#' 
#' @param rsquared value of r-squared from a regression model
#' @param n sample size
#' @param p number of predictors
#' @param method character string indicating the method for calculating r-squared. One of "\code{ezekiel}" (default) 
#' "\code{olkinpratt}", "\code{pratt}",  "\code{wherry1}", and "\code{wherry2}"
#'  
#' @references Raju, N. S., Bilgic, R., Edwards, J. E., & Fleer, P. F. (1997). 
#' Methodology review: Estimation of population validity and cross-validity, and the 
#' use of equal weights in prediction. Applied Psychological Measurement, 21(4), 291-305.
#' 
#' Yin, P., & Fan, X. (2001). Estimating R2 shrinkage in multiple regression: 
#' A comparison of different analytical methods. The Journal of Experimental Education, 69(2), 203-224.
#' 
#' See also: \url{http://stats.stackexchange.com/a/63766/183}
#' @export
#' @examples
#' data(facets_data); data(facets_meta)
#' rs <- summary(regression('swl', ivs=facets_meta$ipip_factors, data=facets_data))$r.squared
#' n <- nrow(facets_data)
#' p <- length(facets_meta$ipip_factors)
#' 
#' adjusted_r_squared(rs, n, p, method='ezekiel')
#' adjusted_r_squared(rs, n, p, method='olkinpratt')
#' adjusted_r_squared(rs, n, p, method='pratt')
#' adjusted_r_squared(rs, n, p, method='wherry1')
#' adjusted_r_squared(rs, n, p, method='wherry2')
#' 
#' adjusted_r_squared(.2, 100, 1)
#' adjusted_r_squared(.2, 100, 5)
#' adjusted_r_squared(.2, 100, 10)
adjusted_r_squared <- function(rsquared, n, p, 
    method=c("ezekiel", "smith", "olkinpratt",  
             "pratt", "wherry1", "wherry2")) {
    method <- match.arg(method)
    
    if (method %in% c('ezekiel', 'smith')) {
        result <- 1 - (1-rsquared)  * ((n-1)/(n-p-1))
    }
    if (method=='olkinpratt') {
        result <- 1 - ((n-3)/(n-p-1)) * (1-rsquared) * phyper(1, 1, (n-p + 1)/2, 1-rsquared)
    }
    
    if (method=='pratt') {
        part1 <- ((n - 3) * (1- rsquared)) / (n - p - 1)
        part2 <- 1 + (2 * (1 - rsquared))/ (n - p - 2.3)
        result <- 1 - part1 * part2
    }
    if (method=='wherry1') {
        result <- 1 - (n-1)/(n-p-1)*(1-rsquared)
    }
    if (method=='wherry2') {
        result <- 1 - (n-1)/(n-p)*(1-rsquared)
    }
    result
    
}

#' @title Calculate adjusted r squared
#' 
#' @description This function is an alternative way of calling \link{adjusted_r_squared}. It derives
#' \code{n}, \code{p}, and \code{rsquared} from an \code{lm} object. 
#' 
#' @param fit object of class lm
#' @param method for calculating r-squared (see \link{adjusted_r_squared})
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


