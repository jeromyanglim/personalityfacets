#' @title Perform k-fold cross validation
#' 
#' @param lmfit object of class lm
#' @param ngroup integer. number of cross validation groups
#' @return vector including raw r-squared and cross-validated r-squared
#' 
#' @export
#' @examples
#' data(facets_data); data(facets_meta)
#' fit <- regression('swl', ivs=facets_meta$ipip_factors, data=facets_data)
#' k_fold_r_squared(fit)
k_fold_r_squared <- function(lmfit, ngroup=10) {
    # assumes library(bootstrap)
    # adapted from http://www.statmethods.net/stats/regression.html
    mydata <- lmfit$model
    outcome <- names(lmfit$model)[1]
    predictors <- names(lmfit$model)[-1]
    
    theta.fit <- function(x,y){lsfit(x,y)}
    theta.predict <- function(fit,x){cbind(1,x)%*%fit$coef} 
    X <- as.matrix(mydata[predictors])
    y <- as.matrix(mydata[outcome]) 
    
    results <- crossval(X,y,theta.fit,theta.predict,ngroup=ngroup)
    raw_rsq <- cor(y, lmfit$fitted.values)**2 # raw R2 
    cv_rsq <- cor(y,results$cv.fit)**2 # cross-validated R2
    
    c(raw_r_squared=raw_rsq, cross_validated_r_squared=cv_rsq)
}