#' Semi partial correlation
#' 
#' Semi-partial correlation between variables x and y, 
#' where overlap of x with \code{controls} is controlled for.
#' 
#' @param y character string of name of outcome variable in \code{data}
#' @param x character string of name of predictor variable in \code{data}
#' @param controls character vector of one or more names of control variables in \code{data}
#' @param data data.frame 
#' @param return_pvalue logical indicating whether to return two tailed p-value of semi-partial correlation
#' @export
#' @examples
#' data(facets_data); data(facets_meta)
#' semi_partial_r('swl', facets_meta$ipip_facets[1], facets_meta$ipip_factors, facets_data)
semi_partial_r <- function(y, x, controls, data, return_pvalue=FALSE) {
    # If there are no controls, then simply use semi-partial correlation
    
        Formula <- formula(paste(x, '~',  paste(controls, collapse=" + ")))
        lm_x <- lm(Formula, data=data)
        resid_x <- resid(lm_x)
        semir <-  cor(resid_x, as.vector(data[,y]))
    
    # Calculate p-value
    if (return_pvalue) {
        Formula2 <- formula(paste(y, '~',  paste(c(x, controls), collapse=" + ")))
        lm_y <- lm(Formula2, data=data)
        slm_y <- summary(lm_y)
        p <- slm_y$coefficients[x, "Pr(>|t|)"]
        return(list(semir=semir, p=p))
    }
    return(semir)
}

