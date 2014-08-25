#' @title Calculate adjusted r-squared
#' 
#' @description Calculates adjusted r-squared using various methods.  
#' 
#' @details
#' The \code{"ezekiel"}  formula is typically used in statistics packages 
#' (e.g., \code{adj.r.squared} in \code{\link{summary.lm}})
#' One recommendation is to use \code{"olkinpratt"} when the predictors are assumed to be random 
#' and \code{"ezekiel"} when predictors are assumed to be fixed.  
#' However, in practice differences between \code{"olkinpratt"} and \code{"ezekiel"} can be trivial.
#' All included formulas are designed to estimate population variance explained, \eqn{\rho^2}, rather than 
#' sample variance explained, \eqn{R^2}.
#' 
#' @param rsquared the value of r-squared [0 to 1.0]
#' @param n sample size
#' @param p number of predictors
#' @param method character string indicating the method for calculating r-squared.  
#'        One of \code{"ezekiel"} (default) 
#' \code{"olkinpratt"}, \code{"pratt"},  \code{"wherry1"}, and \code{"wherry2"}
#'  
#' @references Raju, N. S., Bilgic, R., Edwards, J. E., & Fleer, P. F. (1997). 
#' Methodology review: Estimation of population validity and cross-validity, and the 
#' use of equal weights in prediction. \emph{Applied Psychological Measurement, 21}(4), 291-305.
#' 
#' Yin, P., & Fan, X. (2001). Estimating R2 shrinkage in multiple regression: 
#' A comparison of different analytical methods. \emph{Journal of Experimental Education, 69}(2), 203-224.
#' 
#' See also: \url{http://stats.stackexchange.com/a/63766/183}
#' @export
#' @examples
#' # This first example obtains argument values from sample data.
#' data(facets_data); data(facets_meta)
#' rs <- summary(regression('swl', ivs=facets_meta$ipip_factors, 
#'                          data=facets_data))$r.squared
#' n <- nrow(facets_data)
#' p <- length(facets_meta$ipip_factors)
#' 
#' # These are then passed to the functions using different 
#' #     methods.
#' adjusted_r_squared(rs, n, p, method='ezekiel')
#' adjusted_r_squared(rs, n, p, method='olkinpratt')
#' adjusted_r_squared(rs, n, p, method='pratt')
#' adjusted_r_squared(rs, n, p, method='wherry1')
#' adjusted_r_squared(rs, n, p, method='wherry2')
#' 
#' # Alternatively, the values of rsquared, p, and n
#' #     can be supplied directly.
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
        # hypergeo from hypergeo package
        # computes hypergeometric function 
        multiple <- as.numeric(hypergeo(1,1, round((n-p+1)/2), 1-rsquared)) # note: round seems to be required as
                                                                            # hypergeo gives strange results with numbers ending in .5
        result <- 1 - ((n-3)/(n-p-1)) * (1-rsquared) * multiple
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

#' @title Calculate adjusted r-squared using lm object
#' 
#' @description This function is an alternative way of calling \link{adjusted_r_squared}. It derives
#' \code{n}, \code{p}, and \code{rsquared} from an \code{lm} object. 
#' 
#' @param fit object of class lm
#' @param method method used calculating r-squared (see \link{adjusted_r_squared})
#' @return adjusted r-squared
#' @export
#' @examples
#' # Get data and meta data
#' data(facets_data); data(facets_meta)
#' facets_meta$ipip_factors
#' 
#' # Example showing the use of the regression function
#' fit1 <- regression('swl', ivs= facets_meta$ipip_factors, data=facets_data)
#' lm_adjusted_r_squared(fit1, method='olkinpratt')
#' 
#' # Example showing use of lm formula and how ezekiel values are equivalent
#' fit2 <- lm(swl ~ ipip_extraversion + ipip_openness, facets_data)
#' lm_adjusted_r_squared(fit2, method='ezekiel')
#' summary(fit2)$adj.r.squared
lm_adjusted_r_squared <- function(fit, 
    method=c("ezekiel", "smith", "olkinpratt",  
             "pratt", "wherry1", "wherry2")){
    method <- match.arg(method)
    rsquared <- summary(fit)$r.squared
    n <- length(fit$residuals)
    # minus 1 to remove the intercept
    p <- length(fit$coefficients) - 1 
    adjusted_r_squared(rsquared=rsquared, n=n, p=p, method=method)
}   


