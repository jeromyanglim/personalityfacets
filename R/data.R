#' Data file with personality facets, personality factors, and well being outcomes
#' 
#' The dataset includes 337 participants. The personality scales are based on the IPIP
#' scales modelled on the NEO-PI. Thus, there are scales for the Big 5 along with six facets for each 
#' of the factors. There are also several well-being variablesli
#'
#' @name facets_data 
#' @usage data(facets_data)
#' @docType data
#' @keywords data
#' @references Anglim, J., & Grant, S. (2014). 
#' Predicting Psychological and Subjective Well-Being from Personality: 
#' Incremental Prediction from 30 Facets over the Big 5. 
#' Manuscript Submitted for Publication. 
#' @format 337 participants and 45 variables. Facets appear below with the naming
#' convention of "ipip_" then a letter corresponding to the factor that the facet belongs to
#'  (n=neuroticism, e=extraversion, o=openness, a=agreeableness, c=conscientiousness), 
#'  then the facet name
#' \itemize{
#' \item  idhash. Identifier
#' \item  ipip_n_anxiety
#' \item  ipip_n_anger. 
#' \item  ipip_n_depression
#' \item  ipip_n_self_consciousness
#' \item  ipip_n_immoderation
#' \item  ipip_n_vulnerability
#' \item  ipip_e_friendliness
#' \item  ipip_e_gregariousness
#' \item  ipip_e_assertiveness
#' \item  ipip_e_activity_level
#' \item  ipip_e_excitement_seeking
#' \item  ipip_e_cheerfulness
#' \item  ipip_o_imagination
#' \item  ipip_o_artistic_interests
#' \item  ipip_o_emotionality
#' \item  ipip_o_adventurousness
#' \item  ipip_o_intellect
#' \item  ipip_o_liberalism
#' \item  ipip_a_trust
#' \item  ipip_a_morality
#' \item  ipip_a_altruism
#' \item  ipip_a_cooperation
#' \item  ipip_a_modesty
#' \item  ipip_a_sympathy
#' \item  ipip_c_self_efficacy
#' \item  ipip_c_orderliness
#' \item  ipip_c_dutifulness
#' \item  ipip_c_achievement_striving
#' \item  ipip_c_self_discipline
#' \item  ipip_c_cautiousness
#' \item  ipip_neuroticism. Neuroticism factor
#' \item  ipip_extraversion. Extraversion factor
#' \item  ipip_openness. Openness factor
#' \item  ipip_agreeableness. Agreeablesness factor
#' \item  ipip_conscientiousness. Conscientiousness factor
#' \item  swl. Diener's Satisfaction with life scale
#' \item  panas_pa. PANAS positive affect
#' \item  panas_na. PANAS negative affect
#' \item  pwb_prelwo. Ryff's PWB - Positive relations with others
#' \item  pwb_autonomy. Ryff's PWB - Autonomy
#' \item  pwb_emastery. Ryff's PWB - Environemental mastery
#' \item  pwb_pgrowth. Ryff's PWB - Personal growth
#' \item  pwb_plife. Ryff's PWB - Purpose in life
#' \item  pwb_selfaccept. Ryff's PWB - Self-acceptance
#' }
NULL

#' Meta data for the facets_data data frame
#'
#' Writing out the full list of facet variable names can be tedious. 
#' Thus, it is often useful to store frequently used sets of variables as elements in a named list.
#' This object stores these lists of names as elements in a list.
#' 
#' @name facets_meta
#' @docType data
#' @keywords data
#' @usage data(facets_meta)
#' @examples
#' data(facets_meta)
#' facets_meta$ipip_facets
#' facets_meta$ipip_factors
#' 
#' @format A list of character vectors corresponding to names in the facets_data data frame.
#' \itemize{
#' \item ipip_facets. A characeter vector of the 30 personality facet variable names.
#' \item ipip_factors. A character vector of the 5 personality factor variable names.
#' \item swb. A character vector the 3 subjective well-being variable names.
#' \item pwb. A character vector of the 6 psychological well-being variable names.
#' \item allscales. A character vector of all facet, factor, swb, and pwb scale variable names.
#' }
NULL

#cat(paste("#\' \\item ", names(facets_data)), sep='\n')
