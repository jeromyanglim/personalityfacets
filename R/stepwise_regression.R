#' Forward entry stepwise regression
#' 
#' @description This function performs forward entry stepwise regression.
#' 
#' @param dv character string representing name of dependent variable in \code{data}
#' @param ivs character vector representing predictor variables in \code{data}
#' @param data data.frame
#' @param alpha_in maximum p-value permitted of r-square change of non-included predictor to 
#' allow inclusion of predictor
#' @return The model fit of class \code{lm} returned from the final model. 
#' The order of predictors corresponds to the order that they entered the model.
#' 
#' @details The model starts with no predictors and determines which predictor results 
#' in the largest increase to r-squared. If this predictor has a p-value less than \code{alpha_in}
#' then the predictor is retained. If not then the predictor is excluded and the process stops.
#' 
#' Then all remaining predictors are examined. The predictor with the largest squared semi-partial correlation 
#' adjusting for predictors already in the model is further examined.
#' If this predictor has a p-value less than \code{alpha_in} (e.g., .05) then the predictor is retained. 
#' If it is not significant, then the stepwise procedure ends and the model is returned. 
#' If it is significant, thent the procedure is iterated again.
#' 
#' Note that this function is included mostly for comparion purposes. 
#' We do not advocate the use of stepwise regression for the purpose
#' of evaluating the incremental prediction of facets over factors.
#' 
#' @export
#' 
#' @examples
#' data(facets_data); data(facets_meta)
#' 
#' ## Example with the Big 5 as predictors:
#' stepwise_regression('swl', c("ipip_neuroticism", "ipip_extraversion", 
#'      "ipip_openness", "ipip_agreeableness", "ipip_conscientiousness"), 
#'      facets_data, alpha_in=.05)
#'      
#'## Example using 30 facets as predictors 
#'## (illustrating a more stringent alpha_in):
#' stepwise_regression('swl', facets_meta$ipip_facets, 
#'      facets_data, alpha_in=.001)
stepwise_regression <- function(dv, ivs, data, alpha_in=.05) {
    f <- paste(dv, '~', 1)
    included_ivs <- NULL
    excluded_ivs <- ivs
    fit <- lm(formula(f), data)
    
    for ( i in seq(ivs) ) {
        semi_r_sq <- cor(resid(fit), data[,excluded_ivs])^2
        semi_r_sq <- as.vector(semi_r_sq)
        test_iv <- excluded_ivs[which.max(semi_r_sq)]
        f_test <- paste(f, '+', test_iv)
        fit_test <- lm(formula(f_test), data)
        test_p <- summary(fit_test)$coefficients[test_iv, 'Pr(>|t|)']
        is_sig <- test_p < alpha_in
        if (is_sig) {
            f <- f_test
            fit <- fit_test
            included_ivs <- c(included_ivs, test_iv)
            excluded_ivs <- setdiff(excluded_ivs, test_iv)
        } else {
            break
        }
    }
    fit$included_ivs <- included_ivs
    
    return(fit)
}



