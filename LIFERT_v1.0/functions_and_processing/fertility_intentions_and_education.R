## Intended family size and educational attainment ##



#### INTENDED FAMILY SIZE ####

# Intended family size function

intended_children <- function(n = 1){
  
  # Number of children
  children <- c(0,1,2,3,4,5)
  
  # Proportion of women for each intended number of children
  fert_int = behavioural_inputs$fert_int_both
  
  sample(children, size = n, prob = fert_int, replace = TRUE)
  
}

# Mean intended family size (for calculating the fertility gap)

mean_intend_fam <- sum(behavioural_inputs$fert_int_both*c(0,1,2,3,4,5))


#####



#### EDUCATIONAL ATTAINMENT ####


# Function for the share of women in each educational category
# (assignment of educational attainment to each woman)

# 1 = ISCED 0-2
# 2 = ISCED 3-4
# 3 = ISCED 5-8


educational_attainment <- function(n = 1){
  
  education <- behavioural_inputs$edu_share
  edu_levels = c(1,2,3)
  
  sample(edu_levels, size = n, prob = education, replace = TRUE)
  
}

## FUTURE SCENARIO

# Changing educational structure to women ages 25-34 in 2022 
# (1988-1997 birth cohort)

# 
# edu_share_real_2534 <-
#   c(0.083, 0.308, 0.609)
# 
# educational_attainment <- function(n = 1){
# 
#   education <- edu_share_real_2534
#   edu_levels = c(1,2,3)
# 
#   sample(edu_levels, size = n, prob = education, replace = TRUE)
# 
# }

## ALL WOMEN EITHER HIGH OR LOW EDUCATED

# edu_share_high <- c(0,0,1)
# 
# edu_share_low <- c(1,0,0)
# 
# educational_attainment <- function(n = 1){
# 
#   education <- edu_share_high
#   edu_levels = c(1,2,3)
# 
#   sample(edu_levels, size = n, prob = education, replace = TRUE)
# 
# }



#####