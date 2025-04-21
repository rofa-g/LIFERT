


#### INPUTS ####


# Share of women who ever cohabit by educational attainment
share_cohabited_edu <- union_shares$share_cohabited_edu

#####



#### UNION FORMATION AND DISSOLUTION ####

# Probabilities of first cohabitation by education (isced 0-2, 3-4, 5-7)
# Multiplied by the share of women who ever cohabit, so the maximum value of the
# CDF is the share of women who ever cohabit. Filled out vectors with NAs so they
# are as long as the data (481 months).


cohabitation1 <-
  c(behavioural_inputs$cohabitation1_cdf, rep(NA_real_, NA_25y))

cohabitation2 <- 
  c(behavioural_inputs$cohabitation2_cdf, rep(NA_real_, NA_25y))

cohabitation3 <- 
  c(behavioural_inputs$cohabitation3_cdf, rep(NA_real_, NA_25y))


# Probabilities of separation by duration of cohabitation
# Filled out vectors with NAs so they are as long as the data (481 months).


separation <- 
  c(behavioural_inputs$separation_cdf, rep(NA_real_, NA_20y)) 


# Probabilities of transition from cohabitation to marriage by duration of
# cohabitation and education (isced 0-2, 3-4, 5-7). Filled out vectors with 
# NAs so they are as long as the data (481 months).


marriage1 <- 
  c(behavioural_inputs$cohab_mar_cdf1, rep(NA_real_, NA_20y))

marriage2 <- 
  c(behavioural_inputs$cohab_mar_cdf2, rep(NA_real_, NA_20y))

marriage3 <- 
  c(behavioural_inputs$cohab_mar_cdf3, rep(NA_real_, NA_20y))




# Probability of divorce by duration of marriage (months) and education
# Multiplied by the share of women who divorce, so the maximum value of the
# CDF is the share of women who divorce. Filled out vectors with NAs so they
# are as long as the data (481 months).

share_divorced_edu <- union_shares$share_divorced_edu


divorce1 <- 
  c(behavioural_inputs$divorce_cdf, rep(NA_real_, NA_35y))

divorce2 <- 
  c(behavioural_inputs$divorce_cdf, rep(NA_real_, NA_35y))

divorce3 <- 
  c(behavioural_inputs$divorce_cdf, rep(NA_real_, NA_35y))


# Probability of re-partnering by duration of singlehood
# Multiplied by the share of women who re-partner, so the maximum value of the
# CDF is the share of women who re-partner. Filled out vectors with NA's so they
# are as long as the data (481 months).

share_repartner <- union_shares$share_repartner

repartner <-
  c(behavioural_inputs$repartner_cdf*share_repartner, rep(NA_real_, NA_10y))

#####