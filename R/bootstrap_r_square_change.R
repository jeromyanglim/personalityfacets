#' @title Bootstrap estimate of popualtion r-square of difference between facets and factors
#' 
#' @description The function provides a bootstrapped estimate with confidence intervals of population r-squared. 
#' A rough guide to number of iterations: 100 for basic error checking; 1,000 for exploratory analyses; 
#' 10,000 minimum recommended for publication; 100,000+ preferable for publication where computing power allows.
#' 
#' @param data data.frame
#' @param dv character scalar with name of dependent variable
#' @param ivs1 character vector of names in data.frame typically corresponding to personality factors
#' @param ivs2 character vector of names in data.frame typically corresponding to personality facets
#' @param iterations positive number indicating number of bootstrap iterations to run
#' @param ci number between 0 and 1 indicating size of bootstrap confidence interval. 
#' @param method See \code{method} argument for \link{adjusted_r_squared} function. 
#' Default value is .95 representing a 95\% confidence interval. 
#' @return 
#' an object of class boot_mean_cor_diff
#' \item{boot_results}{object of class boot resulting from bootstrapping analysis}
#' \item{cor_matrix1}{numeric matrix: correlation matrix between set1 variables}
#' \item{cor_matrix2}{numeric matrix: correlation matrix between set2 variables}
#' \item{mean_cor1}{numeric scalar: average correlation between set1 variables}
#' \item{mean_cor2}{numeric scalar: average correlation between set2 variables}
#' \item{set1, set2, data, iterations, ci, method}{copy of corresponding arguments}
#' @export
#' @examples
#' data(facets_data); data(facets_meta)
#' bootstrap_r_squared_change(facets_data, facets_meta$swb[1], facets_meta$ipip_factors, facets_meta$ipip_facets)
bootstrap_r_squared_change <- function(data, dv, ivs1, ivs2, iterations=1000, ci=.95, method='ezekiel') {
    results <- list()
    bootstrapped_data <- lapply(seq(iterations), 
                                function(X) data[sample(seq(nrow(data)), size=nrow(data), replace=TRUE), ])
    
    # sapply_pb is a wrapper for sapply that provides a progress bar
    results$theta_hats <- sapply_pb(bootstrapped_data, function(X) 
        double_adjusted_r_squared_change(X, dv, ivs1, ivs2, method=method))
    results$bootstrapped_standard_error <- 1/ sd(results$theta_hats)
    results$sample_theta_hat <- adjusted_r_squared_change(data, dv, ivs1, ivs2)
    results$ci_level <- ci
    results$ci_values <-  quantile(results$theta_hats, c((1-ci) / 2, 1 - (1-ci)/2))
    results$se <- sd(results$theta_hats)
    results$mean <- mean(results$theta_hats)
    results$adjusted_rsquare <- list(
        ivs1=adjusted_r_squared(summary(regression(dv, ivs=ivs1, data=data))$r.squared, 
                                nrow(data), length(ivs1), method=method),
        ivs2=adjusted_r_squared(summary(regression(dv, ivs=ivs2, data=data))$r.squared, 
                                nrow(data), length(ivs2), method=method)
    )
    results$variables <- list(ivs1=ivs1, ivs2=ivs2)
    results$method <- method
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
