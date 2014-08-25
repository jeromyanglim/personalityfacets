#' @title Bootstrap estimate of popualtion r-squared change 
#' 
#' @description The function provides a bootstrapped estimate with confidence intervals 
#' of population r-squared change. 
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
#' an object of class \code{bootstrap_r_squared_change}.
#' 
#' 
#' \item{variables, data, iterations, ci, method}{copy of corresponding arguments}
#' @export
#' @details
#' Population r-squared change is defined as
#' \deqn{\Delta\rho^2 = \rho^2_{ivs2} - \rho^2_{ivs1}}
#' The Bootstrapping procedure applies the formula for adjusted r-squared twice, 
#' once to remove the bootstrap bias, and a second time to remove the bias inherent to 
#' the r-squared formula.
#' There are several methods available (See the \code{method} argument for the 
#' \code{\link{adjusted_r_squared}} function). 
#' However, in general if you assume that the predictors are random 
#' then \code{"olkinpratt"} is a good option.
#' If you assume that the predictors are fixed, then \code{"ezekiel"} is a good option. 
#' However, generally the results are fairly similar for the two methods
#' We provide the following rough guide for choosing the number of iterations: 
#' 100 for basic error checking; 
#' 1,000 for exploratory analyses; 
#' 10,000 or more is recommended for publication.
#' 
#' Confidence intervals are based on sample quantiles. 
#' For example, .95 ci corresponds to .025 and .975 quantiles of the bootstrap
#' sample estimates.
#' @seealso \code{link{print.bootstrap_r_squared_change}}
#' 
#' @examples
#' ## Load data and meta data:
#' data(facets_data); data(facets_meta)
#' 
#' ## Using 50 iterations is too few, but is used here to 
#' ##     make example run quickly.
#' 
#' ## This version explicitly states the variables:
#' bootstrap_r_squared_change(facets_data, "swl", 
#'     c("ipip_neuroticism", "ipip_extraversion", 
#'       "ipip_openness", "ipip_agreeableness", "ipip_conscientiousness"),
#'     c("ipip_n_anxiety", "ipip_n_anger", 
#'        "ipip_n_depression", "ipip_n_self_consciousness", 
#'       "ipip_n_immoderation", "ipip_n_vulnerability", 
#'       "ipip_e_friendliness", "ipip_e_gregariousness", 
#'       "ipip_e_assertiveness", "ipip_e_activity_level", 
#'       "ipip_e_excitement_seeking", "ipip_e_cheerfulness", 
#'       "ipip_o_imagination", "ipip_o_artistic_interests", 
#'       "ipip_o_emotionality", "ipip_o_adventurousness", 
#'       "ipip_o_intellect", "ipip_o_liberalism", 
#'       "ipip_a_trust", "ipip_a_morality", 
#'       "ipip_a_altruism", "ipip_a_cooperation", 
#'       "ipip_a_modesty", "ipip_a_sympathy", 
#'       "ipip_c_self_efficacy", "ipip_c_orderliness", 
#'       "ipip_c_dutifulness", "ipip_c_achievement_striving", 
#'       "ipip_c_self_discipline", "ipip_c_cautiousness"),
#'      iterations=50)
#'     
#' ## Alternatively, it is often clearer to store the variables 
#' ##      in character vectors.
#' ## For example:
#' facets_meta
#' 
#' ## These can be applied as follows:
#' bootstrap_r_squared_change(facets_data, facets_meta$swb[1], 
#'                            facets_meta$ipip_factors, 
#'                            facets_meta$ipip_facets, 
#'                            iterations=50)
#' 
bootstrap_r_squared_change <- function(data, dv, ivs1, ivs2, iterations=1000, 
                                       ci=.95, method='olkinpratt') {
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
#' @description Print output including descriptive statistics and bootstrap results.
#' 
#' @param x object of class bootstrap_r_squared_change
#' @param digits positive intenger: number of decimal points to display. 
#'        Numbers are passed to \code{round(x, digits)}
#' @param ... further arguments passed to or from other methods (not currently used)
#' @method print bootstrap_r_squared_change
#' @export 
#' @examples
#' ## Load data and meta data:
#' data(facets_data); data(facets_meta)
#' 
#' ## Using 50 iterations is too few, but is used here to 
#' ## make example run quickly.
#' 
#' ##  Save object
#' fit <- bootstrap_r_squared_change(facets_data, 
#'              facets_meta$swb[1], 
#'              facets_meta$ipip_factors, 
#'              facets_meta$ipip_facets, 
#'              iterations=50)
#' ## print object
#' print(fit)
#' 
#' ## Alternatively, the object fit object can be explored directly:
#' 
#' ## Show density plot of bootstrapped estimates of rho-squared change.
#' plot(density(fit$theta_hats), 
#'      xlab="rho-squared change",
#'      main="Density of bootstrap estimates")
#' 
#' ## Extract the confidence interval values
#' fit$ci_values
#' 
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
