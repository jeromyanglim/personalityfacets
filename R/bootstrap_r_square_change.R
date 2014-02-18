#' @title Bootstrap estimate of popualtion r-square of difference between facets and factors
#' 
#' @description The function provides a bootstrapped estimate with confidence intervals of of population r-squared change. 
#' 
#' @param data data.frame
#' @param dv character scalar with name of dependent variable
#' @param ivs1 character vector of variable names in \code{data} typically corresponding to personality factors
#' @param ivs2 character vector of variable names in \code{data} typically corresponding to personality facets
#' @param iterations positive number indicating number of bootstrap iterations to run
#' @param ci number between 0 and 1 indicating size of bootstrap confidence interval. 
#' Default value is .95 representing a 95\% confidence interval. 
#' @param method See \code{method} argument for \link{adjusted_r_squared} function. 
#' Default value is \code{olkinpratt} which is designed where inference is desired to the situation
#' where the predictor variables are assumed to be random.
#' 
#' @return 
#' an object of class boot_mean_cor_diff
#' 
#' 
#' \item{variables, data, iterations, ci, method}{copy of corresponding arguments}
#' @export
#' @details
#' Population r-square change is defined as
#' \deqn{\Delta\rho^2 = \rho^2_{ivs2} - \rho^2_{ivs1}}
#' Bootstrapping involves performing applying the formula for adjusted r-square twice, 
#' once to remove the bootstrap bias, and a second time to remove the bias inherent to the r-square formula.
#' There are several methods available (See \code{method} argument for \link{adjusted_r_squared} function). 
#' However, in general if you assume that the predictors are random then \code{olkinpratt} is a good option.
#' If you assume that the predictors are fixed, then \code{ezekiel} is a good option.
#' A rough guide to number of iterations: 100 for basic error checking; 1,000 for exploratory analyses; 
#' 10,000 minimum recommended for publication; 100,000+ preferable for publication where computing power allows.
#' 
#' @examples
#' data(facets_data); data(facets_meta)
#' # using 100 iterations is too few, but is used here to make example run quickly
#' bootstrap_r_squared_change(facets_data, facets_meta$swb[1], facets_meta$ipip_factors, facets_meta$ipip_facets, iterations=100)
bootstrap_r_squared_change <- function(data, dv, ivs1, ivs2, iterations=1000, ci=.95, method='olkinpratt') {
    results <- list()
    data <- data[,c(ivs1, ivs2, dv)]
    results$data <- data
    results$theta_hats <- sapply_pb(seq(iterations), function(X) 
        double_adjusted_r_squared_change(
            data[ sample(seq(nrow(results$data)), size=nrow(data), replace=TRUE), ], 
            dv, ivs1, ivs2, method=method))
    results$bootstrap_standard_error <- sd(results$theta_hats)/sqrt(iterations)
    results$sample_theta_hat <- adjusted_r_squared_change(data, dv, ivs1, ivs2)
    results$ci_level <- ci
    results$ci_values <-  quantile(results$theta_hats, c((1-ci) / 2, 1 - (1-ci)/2))
    results$se <- sd(results$theta_hats)
    results$mean <- mean(results$theta_hats)
    results$adjusted_rsquared <- list(
        ivs1=adjusted_r_squared(summary(regression(dv, ivs=ivs1, data=data))$r.squared, 
                                nrow(data), length(ivs1), method=method),
        ivs2=adjusted_r_squared(summary(regression(dv, ivs=ivs2, data=data))$r.squared, 
                                nrow(data), length(ivs2), method=method)
    )
    results$variables <- list(ivs1=ivs1, ivs2=ivs2)
    results$method <- method
    results$iterations <- iterations
    class(results) <- "bootstrap_r_squared_change"
    results
}

# Progress bar from:
# http://ryouready.wordpress.com/2010/01/11/progress-bars-in-r-part-ii-a-wrapper-for-apply-functions/
sapply_pb <- function(X, FUN, ...)
{
    env <- environment()
    pb_Total <- length(X)
    counter <- 0
    pb <- txtProgressBar(min = 0, max = pb_Total, style = 3)
    
    wrapper <- function(...){
        curVal <- get("counter", envir = env)
        assign("counter", curVal +1 ,envir=env)
        setTxtProgressBar(get("pb", envir=env), curVal +1)
        FUN(...)
    }
    res <- sapply(X, wrapper, ...)
    close(pb)
    res
}



#' @title Print output for bootstrap_r_squared_change object
#' 
#' @description Print output including descriptive statistics and bootstrap results
#' 
#' @param x object of class boot_mean_cor_diff
#' @param digits positive intenger: number of decimal points to display. Numbers are passed to round(x, digits)
#' @param ... further arguments passed to or from other methods (not currently used)
#' @method print bootstrap_r_squared_change
#' @export 
#' @examples
#' data(facets_data); data(facets_meta)
#' # using 100 iterations is too few, but is used here to make example run quickly
#' fit <- bootstrap_r_squared_change(facets_data, facets_meta$swb[1], facets_meta$ipip_factors, facets_meta$ipip_facets, iterations=100)
#' print(fit, verbose=TRUE)
print.bootstrap_r_squared_change <- function(x, digits=3, ...) {
    cat('\nBOOTSTRAP ESTIMATE OF POPULATION R-SQUARED CHANGE\n')
    
    cat('\nDESCRIPTION')
    cat('\nVariables - ivs1:', x$variables$ivs1,'\n')
    cat('\nVariables - ivs2:', x$variables$ivs2, '\n')
    
    cat('\nn =', nrow(x$data))
    cat('\nAdjusted r-squared method:', x$method)
    cat('\nBootstrap iterations:', x$iterations)
    cat('\nBootstrap standard error:', x$bootstrap_standard_error)
    
    cat('\nEstimated Population R-squared:\n')
    cat('ivs1:', round(x$adjusted_rsquared$ivs1, digits),'\n')
    cat('ivs2:', round(x$adjusted_rsquared$ivs2, digits),'\n')
    
    cat('\nEstimated Population R-squared Change(ivs2 - ivs1):\n')
    cat('Sample estimate:', round(x$sample_theta_hat, digits), '\n')
    cat('Mean of bootstrapped estimates :', round(x$mean, digits), '\n')
    
    cat('\nBootstrap Confidence Interval:', x$ci_level, '\n')
    cat("Confidence interval:", "(",
        paste(round( x$ci_values, digits=digits), collapse=" - "),
        ")", '\n')
    cat("Standard error:", round( x$se, digits=digits), '\n')
    
}
