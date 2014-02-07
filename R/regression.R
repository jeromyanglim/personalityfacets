#' @title lm regression using character strings
#' 
#' @description Call lm function by supply dependent variable and predictors as character strings
#' 
#' @param dv character scale of variable name in data
#' @param ivs character vector of variable names in data
#' @param data data.frame
#' @return same object as return if called using lm
#' @export
#' @examples
#' regression(facets_meta$swb[1], facets_meta$ipip_factors, facets_data)
regression <- function(dv, ivs, data) {
    # run a linear model with text arguments for dv and ivs
    iv_string <- paste(ivs, collapse=" + ")
    regression_formula <- as.formula(paste(dv, iv_string, sep=" ~ "))
    lm(regression_formula, data)
}
