#' Semi-partial correlations between facets and criterion
#' 
#' Produce a data.frame of correlations between facets and criterion. 
#' Rows represent each facet and the four columns represent the zero-order correlation between 
#' facet and criterion followed by various semi-partial correlations where the facet is adjusted
#' controlling for the focal factor, all factors, and all other facets respectively
#'  as described under the "Value" section.
#'  
#' @param dv character string of the criterion variable name in \code{data}
#' @param facets character vector of the facet variable names in \code{data}
#' @param factors character string of the factor variable names in \code{data}. 
#' This string should be the same length as the facets list. 
#' Thus, factor names should repeat and correspond to the factor for the facet 
#' in the corresponding position in facets.
#' @param data data.frame
#' @param return_pvalue logical indicating whether to return p-values associated with each correlation
#' @return By default a data frame of correlations is return. If \code{return_pvalue} is true then a list
#' of two data frames is returned one with the correlations and the other with the pvalues.
#' \enumerate{
#' \item \code{r_zero_order}: zero order corelation
#' \item \code{sr_focal_factor}: semi-partial correlation controlling only for focal factor
#' \item \code{sr_all_factors}: semi-partial correlation controlling for all factors
#' \item \code{sr_other_facets}: semi-partial correlation controlling for all other facets
#' }
#' @export
#' @details The table is useful for examining the degree to which facets provide a unique 
#' contribution to predicting a criterion. In particular, facets controlling for factors is particularly
#' relevant to discussion regarding the incrmental value of facets.
#' @examples
#' ## Load data and meta data:
#' data(facets_data); data(facets_meta)
#' 
#' ## Here we see how the facets and factors should be organised:
#' cbind(facets=facets_meta$ipip_facets, 
#'        factors=rep(facets_meta$ipip_factors, each=6))
#' 
#' facets_semi_partial_r_table('swl', 
#'     facets_meta$ipip_facets, 
#'     rep(facets_meta$ipip_factors, each=6), 
#'     facets_data)
#'     
#' ## Return p-values
#' x <- facets_semi_partial_r_table('swl', 
#'                                  facets_meta$ipip_facets, 
#'                                 rep(facets_meta$ipip_factors, each=6), 
#'                                facets_data, return_pvalue=TRUE)
#'
# Indicate significant correlations based on alpha
#'alpha <- .001
#'cbind(x$p[,1:2], ifelse(x$p[,3:6]<alpha, "*", ""))
#'
facets_semi_partial_r_table <- function(dv, facets, factors, data, return_pvalue=FALSE ) {
    if ( length(facets) != length(factors)) {
        stop("The length of facet variables should be the same as the length of factors")
    }
    
    all_correlations <- function(facet, focal_factor) {
        results <- list()
        results$r_zero_order <- cor(as.vector(data[,dv]), as.vector(data[,facet]))
        results$sr_focal_factor <- semi_partial_r(dv, facet, focal_factor, data)
        results$sr_all_factors <- semi_partial_r(dv, facet, factors, data)
        results$sr_other_facets <- semi_partial_r(dv, facet, setdiff(facets, facet),data)
        unlist(results)
    }

    all_p  <- function(facet, focal_factor) {
        results <- list()
        results$r_zero_order <- cor.test(as.vector(data[,dv]), as.vector(data[,facet]))$p.value
        results$sr_focal_factor <- semi_partial_r(dv, facet, focal_factor, data, return_pvalue = TRUE)$p
        results$sr_all_factors <- semi_partial_r(dv, facet, factors, data, return_pvalue = TRUE)$p
        results$sr_other_facets <- semi_partial_r(dv, facet, setdiff(facets, facet),data, return_pvalue = TRUE)$p
        unlist(results)
    }
    
    
    allsemi <- t(sapply(seq(facets), function(i)  all_correlations(facets[i], factors[i])))
    allsemi <- data.frame(allsemi)
    allsemi <- cbind(factor=factors, facet=facets, allsemi)
    
    allp <- t(sapply(seq(facets), function(i)  all_p(facets[i], factors[i])))
    allp <- data.frame(allp)
    allp <- cbind(factor=factors, facet=facets, allp)
    
    
    if (return_pvalue) {
        return( list(semir=allsemi, p=allp) )    
    }
    allsemi
}
