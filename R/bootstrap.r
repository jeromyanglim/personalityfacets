adjusted_r_square_change <- function(data, dv, ivs1, ivs2) {
    fit_facets <- regression(dv, ivs2, data)
    ars_facets <- summary(fit_facets)$adj.r.squared
    fit_factors <- regression(dv, ivs1, data)
    ars_factors <- summary(fit_factors)$adj.r.squared
    ars_facets - ars_factors
    # c(ars_facets, ars_factors, ars_facets - ars_factors)
}

calculate_adjusted_r_square <- function(rsquare, n, p) {
    1 - (1-rsquare)  * ((n-1)/(n-p-1))
}

double_adjusted_r_square_change <- function(data, dv, ivs1, ivs2) {
    fit1 <- regression(dv, ivs1, data)
    adj_r_square_boot_correction1 <- summary(fit1)$adj.r.squared
    adj_r_square_sample_correction1 <- calculate_adjusted_r_square(adj_r_square_boot_correction1, 
                                                                   nrow(data), length(ivs1))
    
    fit2 <- regression(dv, ivs2, data)
    adj_r_square_boot_correction2 <- summary(fit2)$adj.r.squared
    adj_r_square_sample_correction2 <- calculate_adjusted_r_square(adj_r_square_boot_correction2, 
                                                                   nrow(data), length(ivs2))
    
    adj_r_square_sample_correction2 - adj_r_square_sample_correction1
}


post_adjusted_r_square_change <- function(data, dv) {
    fit_facets <- regression(dv, v$ipip_facets, data)
    ars_facets <- summary(fit_facets)$r.squared
    fit_factors <- regression(dv, v$ipip_factors, data)
    ars_factors <- summary(fit_factors)$r.squared
    r_square_diff <- ars_facets - ars_factors
    p_diff <-  length(coef(fit_facets)) - length(coef(fit_factors))
    n <- nrow(data)
    calculate_adjusted_r_square(r_square_diff, n, p_diff)
    # c(ars_facets, ars_factors, ars_facets - ars_factors)
}

simple_r_square_change <- function(data, dv) {
    fit_facets <- regression(dv, v$ipip_facets, data)
    ars_facets <- summary(fit_facets)$r.squared
    fit_factors <- regression(dv, v$ipip_factors, data)
    ars_factors <- summary(fit_factors)$r.squared
    ars_facets - ars_factors
    # c(ars_facets, ars_factors, ars_facets - ars_factors)
}


