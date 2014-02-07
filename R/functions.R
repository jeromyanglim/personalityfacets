pratt_adjusted_rsquare <- function(lmfit) {
    # Pratt adjusted Rsquared
    # http://stats.stackexchange.com/a/55932/183
    summary_lmfit <- summary(lmfit)
    rsq <- summary_lmfit$r.squared
    n <-  length(summary_lmfit$residuals)
    p <- length(coef(lmfit)) - 1
    part1 <- ((n - 3) * (1- rsq)) / (n - p - 1)
    part2 <- 1 + (2 * (1 - rsq))/ (n - p - 2.3)
    1 - part1 * part2
}


specify_decimal <- function(x, k) format(round(x, k), nsmall=k)

k_fold_rsq <- function(lmfit, ngroup=10) {
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
    
    c(raw_rsq=raw_rsq, cv_rsq=cv_rsq)
}


regression <- function(dv, ivs, data) {
    # run a linear model with text arguments for dv and ivs
    iv_string <- paste(ivs, collapse=" + ")
    regression_formula <- as.formula(paste(dv, iv_string, sep=" ~ "))
    lm(regression_formula, data)
}

round_df <- function(x, digits) {
    # round all numeric variables
    # x: data frame 
    # digits: number of digits to round
    numeric_columns <- sapply(x, mode) == 'numeric'
    x[numeric_columns] <-  round(x[numeric_columns], digits)
    x
}


# Modified by Jeromy Anglim
# Functions that print steps have been removed
# Code modified so that each step is saved
# The object returned is the fit of the final linear model
# with each step saved in an element of the fit object called "steps"
#
# http://orinanobworld.blogspot.com.au/2011/02/stepwise-regression-in-r.html
#
# This is an R function to perform stepwise regression based on a "nested model" F test for inclusion/exclusion
# of a predictor.  To keep it simple, I made no provision for forcing certain variables to be included in
# all models, did not allow for specification of a data frame, and skipped some consistency checks (such as whether
# the initial model is a subset of the full model).
#
# One other note: since the code uses R's drop1 and add1 functions, it respects hierarchy in models. That is,
# regardless of p values, it will not attempt to drop a term while retaining a higher order interaction
# involving that term, nor will it add an interaction term if the lower order components are not all present.
# (You can of course defeat this by putting interactions into new variables and feeding it what looks like
# a first-order model.)
#
# Consider this to be "beta" code (and feel free to improve it).  I've done very limited testing on it.
#
# Author: Paul A. Rubin (rubin@msu.edu)
#
stepwise <- function(full.model, initial.model, alpha.to.enter, alpha.to.leave) {
    # full.model is the model containing all possible terms
    # initial.model is the first model to consider
    # alpha.to.enter is the significance level above which a variable may enter the model
    # alpha.to.leave is the significance level below which a variable may be deleted from the model
    # (Useful things for someone to add: specification of a data frame; a list of variables that must be included)
    full <- lm(full.model);  # fit the full model
    msef <- (summary(full)$sigma)^2;  # MSE of full model
    n <- length(full$residuals);  # sample size
    allvars <- attr(full$terms, "predvars");  # this gets a list of all predictor variables
    models <- list()
    i <- 1
    current <- lm(initial.model);  # this is the current model
    models[[i]] <- current
    i <- i + 1
    while (TRUE) {  # process each model until we break out of the loop
        temp <- summary(current);  # summary output for the current model
        rnames <- rownames(temp$coefficients);  # list of terms in the current model
        p <- dim(temp$coefficients)[1];  # current model's size
        mse <- (temp$sigma)^2;  # MSE for current model
        cp <- (n-p)*mse/msef - (n-2*p);  # Mallow's cp
        if (p > 1) {  # don't try to drop a term if only one is left
            d <- drop1(current, test="F");  # looks for significance of terms based on F tests
            pmax <- max(d[-1,6]);  # maximum p-value of any term (have to skip the intercept to avoid an NA value)
            if (pmax > alpha.to.leave) {
                # we have a candidate for deletion
                var <- rownames(d)[d[,6] == pmax];  # name of variable to delete
                if (length(var) > 1) {
                    # if an intercept is present, it will be the first name in the list
                    # there also could be ties for worst p-value
                    # taking the second entry if there is more than one is a safe solution to both issues
                    var <- var[2];
                }
                f <- formula(current);  # current formula
                f <- as.formula(paste(f[2], "~", paste(f[3], var, sep=" - ")));  # modify the formula to drop the chosen variable (by subtracting it)
                current <- lm(f);  # fit the modified model
                models[[i]] <- current
                i <- i + 1
                next;  # return to the top of the loop
            }
        }
        # if we get here, we failed to drop a term; try adding one
        a <- tryCatch(add1(current, scope=full, test="F"), error=function(e) NULL);  # looks for significance of possible additions based on F tests
        if (is.null(a)) {
            break;  # there are no unused variables (or something went splat), so we bail out
        }
        pmin <- min(a[-1,6]);  # minimum p-value of any term (skipping the intercept again)
        if (pmin < alpha.to.enter) {
            var <- rownames(a)[a[,6] == pmin];  # name of variable to add
            if (length(var) > 1) {
                # same issue with ties, intercept as above
                var <- var[2];
            }
            f <- formula(current);  # current formula
            f <- as.formula(paste(f[2], "~", paste(f[3], var, sep=" + ")));  # modify the formula to add the chosen variable
            current <- lm(f);  # fit the modified model
            models[[i]] <- current
            i <- i + 1
            next;  # return to the top of the loop
        }
        break;
    } 
    result <- models[[length(models)]]
    result$steps <- models
    class(result) <- class(models[[1]])
    result
}

stepwise_formula <- function(dv, ivs, data, alpha.to.enter=.05, alpha.to.leave=.10) {
    data <- data[,c(ivs, dv)]
    lf <- paste(dv, '~1')
    iv_string <- paste(ivs, collapse=" + ")
    ff <- as.formula(paste(dv, iv_string, sep=" ~ "))
    attach(data, warn=FALSE)
    fit <- stepwise(lm(ff),  lm(lf), 
                    alpha.to.enter=alpha.to.enter, 
                    alpha.to.leave=alpha.to.leave)
    detach(data)
    fit
    
}



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
