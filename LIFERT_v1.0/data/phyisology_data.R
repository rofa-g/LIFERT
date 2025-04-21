## Calculations for physiological inputs in the model

############################### IMPORTANT !!! ##################################

# RUN THIS SCRIPT  BEFORE YOU RUN THE SIMULATION SCRIPT

################################################################################

library(dplyr)
library(zoo)

## AGE AT STERILITY 


# Age in years
age_y <- 15:55 # age range in years

# Age in months
age_m <- 180:660


# Probability of sterility by age (years) from Leridon's (2017) code

sterility_best<- c(0,   10,   20,   30,   40,   50,   60,   70,   80,   90,
                   100,  115,  130,  145,  170,  200,  240,  290,  350,  420,
                   500,  580,  700,  850, 1000, 1200, 1450, 1900, 2400, 3100,
                   4000, 6000, 7500, 8350, 8900, 9190, 9370, 9520, 9650, 9760, 10000)/10000



# Since I want monthly probabilities I can add NA's between the years and 
# fit a cubic spline to interpolate these values. 

# Modified na.spline function, since the original function does not work properly 
# (na.spline both extrapolates and interpolates)

na_spline <- function (x, gap) na.spline(x, maxgap = gap) + 
  0*na.approx(x, na.rm = FALSE, maxgap = gap) 


# Setting up the data

join_ster_data <- data.frame(age = age_y,
                             sterility_best = sterility_best) %>% 
  mutate(prob_best = c(sterility_best[1], diff(sterility_best)),
         age_m = age_y*12)

ster_data <- data.frame(age_m = age_m) %>% 
  left_join(join_ster_data, by = c("age_m")) 


# Interpolating the probabilities from yearly to monthly

intp_ster_data <- ster_data %>% 
  mutate(across(c(3:4), \(x) na_spline(x, gap = Inf))) %>% 
  mutate(prob_best = c(sterility_best[1], diff(sterility_best)))

prob_ster <- intp_ster_data$prob_best



## INTRAUTERINE MORTALITY

# Age range in years
iu_age <- 15:49

# Age range in months
iu_age_m <- iu_age*12

# Values for intrauterine mortality from Leridon's (2017) code

miscarriage = c(115,  115,  116,  117,  118,  119,  120,  122,  123,  125,
                127,  130,  134,  137,  141,  145,  149,  154,  160,  166,
                173,  182,  192,  203,  215,  228,  242,  259,  280,  300, 
                320,  340,  360,  380,  400)/1000

# Data frame with estimates
iu_mort_data <- data.frame(age = iu_age_m,
                           miscarriage = miscarriage)


# Fitting a third degree polynomial function to the values

test_3_poly <- lm(miscarriage ~ poly(age, 3, raw = TRUE), data = iu_mort_data)

# Extracting the coefficients (intercept, x, x^2, x^3)

ium_poly_3_coef <- coef(test_3_poly)

## MISCARRIAGE PROBABILITY

# Roughly 80% of miscarriages occur during first trimester

# Cannot say much about the distribution, except that the probability declines
# with month of pregnancy


# Month in which miscarriage occurs

miscarriage_month <- 1:8

# Probabilities 

miscarriage_prob <- c(0.50, 0.20, 0.10, 0.08, 0.06, 0.03, 0.02, 0.01)





## BREASTFEEDING AND POSTPARTUM AMENORRHEA

# Share of women who breastfeed 

# Share of women who breastfeed in the interval from Theurich et al. 2019

breastfeed <- c(0.2, 0.16, 0.08, 0.07, 0.49)

# Breastfeeding intervals 
# 0-1, 1-2, 3-4, 5-6, 6+

# Days of postpartum amenorrhea from Leridon (1977: 83)
pp_ameno <- c(52, 68, 95, 126, 175)

# Mean duration of amenorrhea, roughly 4 months
sum(breastfeed*pp_ameno)



# List with the data

physiology_inputs <- list(prob_ster, ium_poly_3_coef, miscarriage_month, miscarriage_prob)

names(physiology_inputs) <- c("prob_ster", "ium_poly_3_coef", "miscarriage_month", "miscarriage_prob")

# Exporting the data as .RDS

saveRDS(physiology_inputs, "./simulation/physiology_inputs.RDS")





## Mean time to conception 

## Calculating the time to conception based on a mathematical formula

## For conception we need to remove the duration
## of pregnancy and the time to pregnancy. Following Leridon's formula for calculating
## the mean time to conception using the mean fecundability (Leridon 1977, pp. 27,34):

## m = integral(0,1) 1/p 

## In this case p(mean fecundability) = 0.25, m(mean time to conception) = 4 months

## Not all conceptions end in live births, the mean probability of intrauterine
## mortality is 0.236, so around 1/4 of all conceptions can be assumed to result
## in intrauterine mortality, so we multiply p by 3/4

## m(mean time to conception)  = 16/3 (5.333). We therefore use the estimate of
## 5 months

#####

