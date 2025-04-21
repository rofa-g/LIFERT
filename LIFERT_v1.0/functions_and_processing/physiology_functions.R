## Reproductive physiology functions ##


#### INPUTS ####

## Age at sterility distribution

# Probability distribution using Leridons data

prob_ster <- physiology_inputs$prob_ster


## Intrauterine mortality distribution

# Coefficients for fitted third degree polynomial to the intrauterine mortality
# data

ium_poly3_coef <- physiology_inputs$ium_poly_3_coef

miscarriage_month <- physiology_inputs$miscarriage_month

miscarriage_prob <- physiology_inputs$miscarriage_prob


#####


#### MAXIMUM FECUNDABILITY ####

## Maximum fecundability function 

# Beta distribution (random variable) function (Leridon 1977,2017)

fecundability <- function(n = 1, a = 3, b = 9){
  a = a
  b = b
  prob <- rbeta(n, a, b, ncp = 0)
  return(prob)
}


#####


#### AGE AT STERILITY ####


## Age at sterility function

age_ster <- function(n = 1){
  
  # Age at sterility draw from sterility distribution
  age_ster <-  sample(age_m, size = n,
                      prob =  prob_ster, replace = TRUE) 
  return(age_ster)
}

#####


#### INCORPORATING THE LINEAR DECLINE IN FECUNDABILITY BEFORE STERILITY ####

## Function to create linear interpolation, returns y for x

lin_int <- function(x1, x2, y1, y2, x) {
  y1 + ((x - x1) * ((y2 - y1) / (x2 - x1)))
}


## Linear interpolation used to model the gradual decline in fecundability from
# age at onset of sterility to permanent sterility (Leridon 1977, pp. 115-120,
# Leridon 2004) 

fec <- function(n = 1) {
  
  # Fecundability draw from beta distribution
  fecundability = fecundability(n)
  
  # Age at sterility draw
  age_ster = age_ster(n)
  
  # Age in months
  age_m = age_m
  
  # Month when decline in fecundability starts
  ster_start <- (age_ster - 12.5*12)
  
  # Month when permanent sterility is reached
  ster_stop <- age_ster
  
  # Fecundability  declines linearly 12.5 years prior to sterility down to 0
  fec = fcase(
    age_m >= (ster_stop) , 0,
    age_m <  (ster_start) , fecundability,
    age_m >= (ster_start) & age_m < (ster_stop) ,
    lin_int(ster_start, ster_stop, fecundability, 0, age_m)
  )
  return(fec)
}


## WITHOUT AGE-DECLINE IN FECUNDABILITY FOR COUNTERFACTUAL SCENARIO

# fec <- function(n = 1) {
# 
#   # Fecundability draw from beta distribution
#   fecundability = fecundability(n)
# 
#   # Age at sterility draw
#   age_ster = age_ster(n)
# 
#   # Age in months
#   age_m = age_m
# 
#   # Month when permanent sterility is reached
#   ster_stop <- age_ster
# 
#   # No decline in fecundability with age
#   fec = fifelse(age_m >= (ster_stop) , 0, fecundability)
# 
#   return(fec)
# }

#####



#### INTRAUTERINE MORTALITY ####


## Function for third degree polynomial with the estimated coefficients,
# x = age range

iu_mort <- function(x = age_m){
  ium_poly3_coef[1] + ium_poly3_coef[2]*x + ium_poly3_coef[3]*x^2 + ium_poly3_coef[4]*x^3
}

# Store function output as vector

ium <- iu_mort()

## INTRAUTERINE MORTALITY FIXED AT THE LEVEL OF A 20-YEAR-OLD FOR COUNTERFACTUAL
# SCENARIO

# ium <- rep(0.12, 481)


## Function for the month in which the foetal death occurs 

miscarriage_time <- function(n = 1){
  sample(miscarriage_month, size = n,
         prob = miscarriage_prob, replace = TRUE)
}




#####