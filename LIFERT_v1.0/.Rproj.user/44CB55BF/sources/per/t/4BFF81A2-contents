## Contraception, non-susceptible period, and spacing ##
 


#### SPACING AND STOPPING EFFICACIES AND ABORTION ####

# Efficacy of spacing (before first birth and between births) for ages 15-29
# Multiplied by fecundability during spacing.

spacing_efficacy  <- (1-0.982) 

# Efficacy of spacing (before first birth and between births) for ages 30-55
# Multiplied by fecundability during spacing.

spacing_efficacy_30  <- (1-0.962) 

# Efficacy of stopping (after indended number of children reached). 
# Multiplied by fecundability  during stopping.

stopping_efficacy <- (1-0.991) 

# Probability of abortion that results in roughly 154 abortions per 1,000 live
# births. 

prob_abortion <- 0.58

#####




#### INTENDED SPACING AND NON-SUSCEPTIBLE PERIOD  ####


# Intended spacing by parity (from 0)

intended_spacing <- c(42,   15,   5,   3,   1,    1,    1,   1,   1,   1,
                      1,   1,    1,    1,    1,    1,    1,   1,   1,   1)


# Postpartum amenorrhea after live birth

wait_postpartum <- function (n){
  # random truncated normal distribution with a lower of 0 to avoid negative values
  dist <- rtnorm(n, mean = 4, lower = 0, upper = Inf) 
  return(dist)
}

# Non-susceptible period after miscarriage (and induced abortion)
wait_postpartum_m <- 1


#####




#### ABORTION RATIO AND TIMING OF ABORTION ####

# Abortion data from MVWS Jaarraportage Wet afbreking zwangerschap (Wafz) (2017, 2021)

# Abortion ratio = number of abortions per 1,000 live births

abort_ratio <- data.table(year = 2000:2020, 
                          abort_ratio = c(132, 140, 146, 144, 150, 153, 154,
                                          156, 154, 153, 152, 154, 153, 156,
                                          152, 158, 154, 159, 164, 171, 169))

# Mean abortion ratio in 2000-2020 of around 154 abortions per 1,000 live births

mean(abort_ratio$abort_ratio)


# 2013 data on which week of pregnancy the abortion took place from 
# MVWS Jaarraportage (2017)

abortion_data <- 
  data.table(month = c(rep(2:6, each = 4)), # data in weeks, changed to months
             abortion = c(966, 4387, 5854, 4409, 3804, 2503, 
                          1236, 926, 742, 589, 668, 669, 783, 
                          652, 502, 524, 444, 485, 355, 98))

# Summing the abortion counts by month, and calculating shares by month

abortion_data <- 
  abortion_data[, lapply(.SD, sum), by = month
  ][, abortion := abortion/sum(abortion)]

# Function for determining in which month of pregnancy the abortion takes place

abortion_time <- function(n = 1){
  
  abortion <- abortion_data$abortion
  month = abortion_data$month
  
  sample(month, size = n, prob = abortion, replace = TRUE)
  
}

