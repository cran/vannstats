#' Uniform Crime Reports, 2015 (County-Level)
#'
#' This subset of data comes from one iteration of the \emph{Uniform Crime Reporting Program}, administered in 2015. These data were collected by the Federal Bureau of Investigation under the United States Department of Justice. While the original data cover every \emph{reported} crime event that took place in 2015, these data are aggregated to the county level. Additionally, these data are combined with (a subset of) county-level demographic data from the 2005-2009 (5-year estimates) iteration of the \emph{American Community Survey}. Information about the data set can be found in the UCR2015 Codebook at: \url{https://burrelvannjr.com/docs/UCR2015_Codebook.pdf}.
#'
#' @format A data frame with 3108 observations and 102 variables.
#' \tabular{ll}{ \cr
#' id \tab State and County Identifier \cr
#' statefips \tab FIPS Code for State \cr
#' countyfips \tab FIPS Code for County \cr
#' state \tab State Name \cr
#' county \tab County Name \cr
#' totalpop \tab Total County Population \cr
#' pct_unemp \tab Percent of Total County Population who are Unemployed \cr
#' pct_homeowners \tab Percent of Total County Population who are Homeowners \cr
#' pct_college \tab Percent of Total County Population who are over 25 years old and hold a Bachelor's Degree \cr
#' med_fam_inc \tab Median Family Income (in Thousands of Dollars) \cr
#' pop_density \tab Population Density (Population over Land Area in County) \cr
#' pct_poverty \tab Percent of Total County Population who are below the Poverty Line \cr
#' pct_white \tab Percent of Total County Population who are White \cr
#' pct_black \tab Percent of Total County Population who are Black \cr
#' pct_latino \tab Percent of Total County Population who are Latinx/e/a/o \cr
#' income_inequality \tab Gini Coefficient of Income Inequality -- The distribution of income across the county population. High scores indicate greater inequality, with high-income individuals receiving much larger percentages of the total income made in the county. \cr
#' rape \tab Forcible rape (Count) \cr
#' robbery \tab Robbery (Count) \cr
#' agg_assault \tab Aggravated assault (Count) \cr
#' burglary \tab Burglary-breaking or entering (Count) \cr
#' larceny \tab Larceny-theft (not motor vehicles) (Count) \cr
#' mv_theft \tab Motor vehicle theft (Count) \cr
#' other_assault \tab Other assaults (Count) \cr
#' arson \tab Arson (Count) \cr
#' forgery \tab Forgery and counterfeiting (Count) \cr
#' fraud \tab Fraud (Count) \cr
#' embezzlement \tab Embezzlement (Count) \cr
#' stolen_property \tab Stolen property-buy, receive, poss. (Count) \cr
#' vandalism \tab Vandalism (Count) \cr
#' weapons \tab Weapons-carry, posses, etc. (Count) \cr
#' sex_offense \tab Sex offenses (not rape or prostitution) (Count) \cr
#' drug_abuse \tab Total drug abuse violations (Count) \cr
#' drug_sale \tab Sale/manufacture (subtotal) (Count) \cr
#' drug_possession \tab Possession (subtotal) (Count) \cr
#' drug_sale_coke \tab Sale/mfg-Opium, coke, and their derivatives (Count) \cr
#' drug_sale_mj \tab Sale/mfg-Marijuana (Count) \cr
#' drug_possession_coke \tab Possession-Opium, coke, and their derivatives (Count) \cr
#' drug_possession_mj \tab Possession-Marijuana (Count) \cr
#' drug_possession_narc \tab Possession-Truly addicting synthetic narcotics (Count) \cr
#' drug_possession_other \tab Possession-Other dangerous non-narc drugs (Count) \cr
#' domestic_offenses \tab Offenses against family and children (Count) \cr
#' dui \tab Driving under the influence (Count) \cr
#' liquor_violation \tab Liquor laws (Count) \cr
#' disorderly_conduct \tab Disorderly conduct (Count) \cr
#' other_nontraffic_violation \tab All other non-traffic offenses (Count) \cr
#' murder \tab Murder and non-negligent manslaughter (Count) \cr
#' drug_sale_other \tab Sale/mfg-Other dangerous non-narc drugs (Count) \cr
#' prostitution \tab Prostitution and commercialized vice (Count) \cr
#' drug_sale_narc \tab Sale/mfg-Truly addicting synthetic narcotics (Count) \cr
#' vagrancy \tab Vagrancy (Count) \cr
#' drunkenness \tab Drunkenness (Count) \cr
#' curfew_loitering \tab Curfew and loitering violations (Count) \cr
#' runaway \tab Runaways (Count) \cr
#' manslaughter_negligence \tab Manslaughter by negligence (Count) \cr
#' gambling_all \tab Gambling (total) (Count) \cr
#' suspicion \tab Suspicion (Count) \cr
#' gambling_bookmaking \tab Bookmaking (horse and sports) (Count) \cr
#' gambling_other \tab All other gambling (Count) \cr
#' gambling_lottery \tab Number and lottery (Count) \cr
#' rape_pct \tab Forcible rape (as percent of total county population) \cr
#' robbery_pct \tab Robbery (as percent of total county population) \cr
#' agg_assault_pct \tab Aggravated assault (as percent of total county population) \cr
#' burglary_pct \tab Burglary-breaking or entering (as percent of total county population) \cr
#' larceny_pct \tab Larceny-theft (not motor vehicles) (as percent of total county population) \cr
#' mv_theft_pct \tab Motor vehicle theft (as percent of total county population) \cr
#' other_assault_pct \tab Other assaults (as percent of total county population) \cr
#' arson_pct \tab Arson (as percent of total county population) \cr
#' forgery_pct \tab Forgery and counterfeiting (as percent of total county population) \cr
#' fraud_pct \tab Fraud (as percent of total county population) \cr
#' embezzlement_pct \tab Embezzlement (as percent of total county population) \cr
#' stolen_property_pct \tab Stolen property-buy, receive, poss. (as percent of total county population) \cr
#' vandalism_pct \tab Vandalism (as percent of total county population) \cr
#' weapons_pct \tab Weapons-carry, posses, etc. (as percent of total county population) \cr
#' sex_offense_pct \tab Sex offenses (not rape or prostitution) (as percent of total county population) \cr
#' drug_abuse_pct \tab Total drug abuse violations (as percent of total county population) \cr
#' drug_sale_pct \tab Sale/manufacture (subtotal) (as percent of total county population) \cr
#' drug_possession_pct \tab Possession (subtotal) (as percent of total county population) \cr
#' drug_sale_coke_pct \tab Sale/mfg-Opium, coke, and their derivatives (as percent of total county population) \cr
#' drug_sale_mj_pct \tab Sale/mfg-Marijuana (as percent of total county population) \cr
#' drug_possession_coke_pct \tab Possession-Opium, coke, and their derivatives (as percent of total county population) \cr
#' drug_possession_mj_pct \tab Possession-Marijuana (as percent of total county population) \cr
#' drug_possession_narc_pct \tab Possession-Truly addicting synthetic narcotics (as percent of total county population) \cr
#' drug_possession_other_pct \tab Possession-Other dangerous non-narc drugs (as percent of total county population) \cr
#' domestic_offenses_pct \tab Offenses against family and children (as percent of total county population) \cr
#' dui_pct \tab Driving under the influence (as percent of total county population) \cr
#' liquor_violation_pct \tab Liquor laws (as percent of total county population) \cr
#' disorderly_conduct_pct \tab Disorderly conduct (as percent of total county population) \cr
#' other_nontraffic_violation_pct \tab All other non-traffic offenses (as percent of total county population) \cr
#' murder_pct \tab Murder and non-negligent manslaughter (as percent of total county population) \cr
#' drug_sale_other_pct \tab Sale/mfg-Other dangerous non-narc drugs (as percent of total county population) \cr
#' prostitution_pct \tab Prostitution and commercialized vice (as percent of total county population) \cr
#' drug_sale_narc_pct \tab Sale/mfg-Truly addicting synthetic narcotics (as percent of total county population) \cr
#' vagrancy_pct \tab Vagrancy (as percent of total county population) \cr
#' drunkenness_pct \tab Drunkenness (as percent of total county population) \cr
#' curfew_loitering_pct \tab Curfew and loitering violations (as percent of total county population) \cr
#' runaway_pct \tab Runaways (as percent of total county population) \cr
#' manslaughter_negligence_pct \tab Manslaughter by negligence (as percent of total county population) \cr
#' gambling_all_pct \tab Gambling (total) (as percent of total county population) \cr
#' suspicion_pct \tab Suspicion (as percent of total county population) \cr
#' gambling_bookmaking_pct \tab Bookmaking (horse and sports) (as percent of total county population) \cr
#' gambling_other_pct \tab All other gambling (as percent of total county population) \cr
#' gambling_lottery_pct \tab Number and lottery (as percent of total county population) \cr
#' }
#' @source Data: \url{https://www.icpsr.umich.edu/web/NACJD/studies/36794} and \url{https://data.census.gov/app/mdat/ACSPUMS1Y2023}
#' @source Codebook: \url{https://burrelvannjr.com/docs/UCR2015_Codebook.pdf}

#'
"UCR2015"
