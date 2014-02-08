#' Semi partial correlation
#' 
#' Semi-partial correlation between variables x and y, 
#' where overlap of x with \code{controls} is controlled for.
#' 
#' @param y character string of name of outcome variable in \code{data}
#' @param x character string of name of predictor variable in \code{data}
#' @param controls character vector of one or more names of control variables in \code{data}
#' @param data data.frame 
#' @export
#' @examples
#' data(facets_data); data(facets_meta)
#' semi_partial_r('swl', facets_meta$ipip_facets[1], facets_meta$ipip_factors, facets_data)
semi_partial_r <- function(y, x, controls, data) {
    Formula <- formula(paste(x, '~',  paste(controls, collapse=" + ")))
    lm_x <- lm(Formula, data=data)
    resid_x <- resid(lm_x)
    cor(resid_x, data[,y])
}