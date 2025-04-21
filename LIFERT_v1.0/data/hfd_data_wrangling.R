
#### HUMAN FERTILITY DATABASE: COMPLETED COHORT FERTILITY, PARITY PROGRESSION AND
#### MEAN AGES AT BIRTHS ####

# Accessing data from Human Fertility & Mortality Databases
library(HMDHFDplus)

## IMPORTANT! 

# You need to register an account at https://www.humanfertility.org/ and add 
# your username and password in in the fields of the 'user' and 'pass' objects
# below


user <- ""
pass <- ""

# Cohort Completed Fertility Rate

ccfr <- 
  HMDHFDplus::readHFDweb(CNTRY = "NLD", 
                         item = "tfrVHbo",
                         username = user,
                         password = pass,
                         fixup = TRUE)


# Parity Progression Ratios

ppr <- 
  HMDHFDplus::readHFDweb(CNTRY = "NLD", 
                         item = "pprVHbo",
                         username = user,
                         password = pass,
                         fixup = TRUE)

# Mean age at births

mean_age_b <- 
  HMDHFDplus::readHFDweb(CNTRY = "NLD", 
                         item = "mabVHbo",
                         username = user,
                         password = pass,
                         fixup = TRUE)


# Mean age at births

# Difference between Cohort Mean Age at Birth (CMAB) and Cohort Mean Age at Birth
# (CMAB40) at age 40 for cohorts 1959-1969

cmab1 <- with(mean_age_b, CMAB1[Cohort %in% 1959:1969] - CMAB40_1[Cohort %in% 1959:1969])

cmab2 <- with(mean_age_b, CMAB2[Cohort %in% 1959:1969] - CMAB40_2[Cohort %in% 1959:1969])

cmab3 <- with(mean_age_b, CMAB3[Cohort %in% 1959:1969] - CMAB40_3[Cohort %in% 1959:1969])

cmab4 <- with(mean_age_b, CMAB4[Cohort %in% 1959:1969] - CMAB40_4[Cohort %in% 1959:1969])

cmab5 <- with(mean_age_b, CMAB5p[Cohort %in% 1959:1969] - CMAB40_5p[Cohort %in% 1959:1969])

# Plotting

# plot(cmab1, xlim = c(0,11), ylim = c(0,2))
# lines(cmab2)
# lines(cmab3)
# lines(cmab4)
# lines(cmab5)

# Mean ages at births for the 1979 cohort approximated by adding the difference
# between CMAB and CMAB40 for the cohorts 1969 and 1959 (same year difference
# as 1969-1979) to the difference between CMAB and CMAB40 for the 1969 cohort
# to CMAB40 for the 1979 cohort.

cmab1_1979 <- with(mean_age_b,CMAB40_1[Cohort == 1979]+(cmab1[11]+cmab1[11]-cmab1[1]))

cmab2_1979 <- with(mean_age_b,CMAB40_2[Cohort == 1979]+(cmab2[11]+cmab2[11]-cmab2[1]))

cmab3_1979 <- with(mean_age_b,CMAB40_3[Cohort == 1979]+(cmab3[11]+cmab3[11]-cmab3[1]))

cmab4_1979 <- with(mean_age_b,CMAB40_4[Cohort == 1979]+(cmab4[11]+cmab4[11]-cmab4[1]))

cmab5_1979 <- with(mean_age_b,CMAB40_5p[Cohort == 1979]+(cmab5[11]+cmab5[11]-cmab5[1]))


# Difference between Completed Cohort Fertility (CCF) and Completed Cohort Fertility
# for 40-year-olds 

ccf_diff <- with(ccfr, CCF[Cohort %in% 1959:1969]-CCF40[Cohort %in% 1959:1969])

# Estimated Completed Cohort Fertility for the 1979 cohort

# Adding the difference between the 1959 and 1969 difference between CCF and CCF40
# to the 1969 difference as an estimate of how large the gap is in 1979 (10 years later)
# because of the increasing trend in 'ccf_diff' over time

ccf_1979 <- with(ccfr, CCF40[Cohort == 1979]+(ccf_diff[11]+(ccf_diff[11]-ccf_diff[1])))


# Share of women with n+1 number of children from Parity Progression Ratios (PPR)
# starting at 0

w1_0 <- with(ppr, PPR0_1[Cohort == 1969]*1)

w1_1 <- with(ppr, PPR1_2[Cohort == 1969]*w1_0)

w1_2 <- with(ppr, PPR2_3[Cohort == 1969]*w1_1)

w1_3 <- with(ppr, PPR3_4[Cohort == 1969]*w1_2)


# Share of women who stay at parity n, which are the shares of women with 
# n number of children used in the results table in the paper

ppr0 <- 1-w1_0

ppr1 <- w1_0-w1_1

ppr2 <- w1_1-w1_2

ppr3 <- w1_2-w1_3

ppr4 <- 1-sum(ppr0,ppr1,ppr2,ppr3)

#####