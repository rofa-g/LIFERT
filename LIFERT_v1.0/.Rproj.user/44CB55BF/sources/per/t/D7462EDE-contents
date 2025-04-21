## Simulation model of Dutch fertility using mainly GGS and LISS survey data 
## Script for multiple parameter changes


######################## IMPORTANT ##########################

# EACH TIME YOU RE-RUN THE SCRIPT REMEMBER THAT YOU NEED TO UPDATE ALL THE FUNCTIONS
# SO RUN THE ENTIRE SCRIPT (CTRL+SHIFT+HOME AFTER THE SIMULATION FUNCTION
# AND THEN CTRL+ENTER FOR WINDOWS), NOT JUST THE SIMULATION FUNCTION

# FOR THIS SCRIPT RUN THE FUNCTION IMPORT (LINES 87-93) ONCE THEN 
# COMMENT IT OUT. OTHERWISE ANY CHANGES THAT ARE MADE TO THE PARAMETERS
# IN THIS SCRIPT WILL BE OVERWRITTEN INSTEAD OF THE FUNCTIONS BEING UPDATED
# (SEE LINES 569-576)

##############################################################


#### SETTING UP R ####

# Function for installing not yet installed packages 
# that are required for the code to run

using<-function(...) {
  libs<-unlist(list(...))
  req<-unlist(lapply(libs,require,character.only=TRUE))
  need<-libs[req==FALSE]
  if(length(need)>0){
    install.packages(need)
    lapply(need,require,character.only=TRUE)
  }
}


using("data.table", "MCMCglmm", "kableExtra", "magick",
      "magick", "dqrng", "parallel", "webshot2")

### SET SEED FOR REPRODUCIBILITY

set.seed(7)
#####






#### INPUT DATA ####

# Input lists for the functions 

behavioural_inputs <- readRDS("./simulation/dutch_simulation_inputs.RDS")

physiology_inputs <- readRDS("./simulation/physiology_inputs.RDS")

# Union shares

union_shares <- readRDS("./simulation/union_share_list.RDS")

# Mean ages at union first union formations and dissolutions

mean_ages_union <- readRDS("./simulation/union_age_duration_list.RDS")



#####


#### AGE PARAMETERS ####

# age range in years
age_y <- as.numeric(15:55)

# age range in months
age_m <- as.numeric(180:660)

#####



#### IMPORTING FUNCTIONS AND PROCESSED DATA ####

## RUN THESE LINES OF CODE ONCE (CTRL+SHIFT+HOME AFTER THE LAPPLY FUNCTION
## AND THEN CTRL+ENTER FOR WINDOWS) AND THEN COMMENT THEM OUT. 

# # Folder where functions are located
# path_f = "./functions_and_processing/"
# 
# # Collecting the file paths of all R script files in the 'functions' folder
# function_names <- list.files(path = path_f, pattern = "[.][RrSsQq]$", full.names = TRUE)
# 
# # Sourcing all the function R scripts
# lapply(function_names, source)


#####






#### CHANGING INTRAUTERINE MORTALITY ####

## ORIGINAL

 ium <- iu_mort()

## INTRAUTERINE MORTALITY FIXED AT THE LEVEL OF A 20-YEAR-OLD FOR COUNTERFACTUAL
## SCENARIO

 # ium <- rep(0.12, 481)

#####



#### CHANGING FECUNDABILITY ####

## ORIGINAL

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






#### CHANGES IN EDUCATIONAL STRUCTURE ####

## ORIGINAL

educational_attainment <- function(n = 1){

  education <- behavioural_inputs$edu_share
  edu_levels = c(1,2,3)

  sample(edu_levels, size = n, prob = education, replace = TRUE)

}

## FUTURE SCENARIO

# Changing educational structure to women ages 25-34 in 2022 
# (1988-1997 birth cohort)


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

## FOR THE EDUCATIONAL DECOMPOSITION OF THE FERTILITY GAP, SET EDUCATION
## TO LOW (edu_share_low) AND SIMULATE, THEN edu_share_high TO GET THE TOTAL DIFFERENCE
## IN THE FERTILITY GAP. USE THE SAME edu_share_low AS THE BASELINE FOR INDIVIDUAL
## AND CUMULATIVE PARAMETER CHANGES IN THE DECOMPOSITION OF THE DIFFERENCE IN THE 
## FERTILTIY GAP.

# edu_share_high <- c(0,0,1)
# 
# edu_share_low <- c(1,0,0)
# 
# educational_attainment <- function(n = 1){
# 
#   education <- edu_share_low # change this to either high or low
#   edu_levels = c(1,2,3)
# 
#   sample(edu_levels, size = n, prob = education, replace = TRUE)
# 
# }


#####




#### UNION FORMATION AND DISSOLUTION ####


# Share of women who ever cohabit by educational attainment (change to '_5pct'
# for the future scenario)

share_cohabited_edu <- union_shares$share_cohabited_edu

# FIRST COHABITATION 100%

# share_cohabited_edu <- c(1,1,1)


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


# POSTPONEMENT OF FIRST COHABITATION (1 YEAR AS EXAMPLE). FOR EACH INCREASE IN
# YEAR ADD 12 MONTHS TO THE 'rep(0, 12)' FUNCTION AND 1 YEAR TO THE 'NA_26y'
# VALUE. SO FOR A 2 YEAR POSTPONEMENT CHANGE THEM TO 'rep(0, 24)' AND 'NA_27y'
# RESPECTIVELY. DO THIS FOR ALL THREE COHABITATION DISTRIBUTIONS.
# THIS WORKS FOR UP TO 10 YEARS OF POSTPONEMENT, FOR FURTHER 
# POSTPONEMENT YOU NEED TO ADD NEW VALUES TO THE 'na_zero_vectors' SCRIPT LOCATED
# IN THE 'functions_and_processing' FOLDER.


# cohabitation1 <-
#   c(rep(0, 12), behavioural_inputs$cohabitation1_cdf, rep(NA_real_, NA_26y))
# 
# cohabitation2 <-
#   c(rep(0, 12), behavioural_inputs$cohabitation2_cdf, rep(NA_real_, NA_26y))
# 
# cohabitation3 <-
#   c(rep(0, 12), behavioural_inputs$cohabitation3_cdf, rep(NA_real_, NA_26y))
# 


# ALL COHABITATIONS BEGIN AT AGE 15

# cohabitation1 <-
#   c(rep(1, 301), rep(NA_real_, NA_25y))
# 
# cohabitation2 <-
#   c(rep(1, 301), rep(NA_real_, NA_25y))
# 
# cohabitation3 <-
#   c(rep(1, 301), rep(NA_real_, NA_25y))
# 



# Share of women who separate by educational attainment(change to '_5pct'
# for the future scenario)

share_separated_edu <- union_shares$share_separated_edu

# SEPARATION 0%

# share_separated_edu <- c(0,0,0)

# Probabilities of separation by duration of cohabitation
# Filled out vectors with NAs so they are as long as the data (481 months).

separation <- 
  c(behavioural_inputs$separation_cdf, rep(NA_real_, NA_20y)) 



# Share of women who marry by educational attainment(change to '_5pct'
# for the future scenario)

share_married_edu <- union_shares$share_married_edu

# MARRIAGE 100%

# share_married_edu <- c(1,1,1)

# Probabilities of transition from cohabitation to marriage by duration of
# cohabitation and education (isced 0-2, 3-4, 5-7). Filled out vectors with 
# NAs so they are as long as the data (481 months).

marriage1 <- 
  c(behavioural_inputs$cohab_mar_cdf1, rep(NA_real_, NA_20y))

marriage2 <- 
  c(behavioural_inputs$cohab_mar_cdf2, rep(NA_real_, NA_20y))

marriage3 <- 
  c(behavioural_inputs$cohab_mar_cdf3, rep(NA_real_, NA_20y))


# TRANSITION TIME TO MARRIAGE 1 MONTH

# marriage1 <-
#   c(rep(1, 241), rep(NA_real_, NA_20y))
# 
# marriage2 <-
#   c(rep(1, 241), rep(NA_real_, NA_20y))
# 
# marriage3 <-
#   c(rep(1, 241), rep(NA_real_, NA_20y))


# Share of women who divorce by educational attainment(change to '_5pct'
# for the future scenario)

share_divorced_edu <- union_shares$share_divorced_edu

# DIVORCE 0%

# share_divorced_edu <- c(0,0,0)

# Probability of divorce by duration of marriage (months) and education
# Multiplied by the share of women who divorce, so the maximum value of the
# CDF is the share of women who divorce. Filled out vectors with NAs so they
# are as long as the data (481 months).

divorce <- 
  c(behavioural_inputs$divorce_cdf, rep(NA_real_, NA_35y))


# Share of women who re-partner by educational attainment(change to '_5pct'
# for the future scenario)

share_repartnered_edu <- union_shares$share_repartnered_edu

# 100% REPARTNERING

# share_repartnered_edu <- c(1,1,1)

# Probability of re-partnering by duration of singlehood
# Multiplied by the share of women who re-partner, so the maximum value of the
# CDF is the share of women who re-partner. Filled out vectors with NA's so they
# are as long as the data (481 months).

repartner <-
  c(behavioural_inputs$repartner_cdf, rep(NA_real_, NA_10y))


# TIME TO REPARTNER 1 MONTH

# repartner <-
#   c(rep(1, 121), rep(NA_real_, NA_10y))
# 


#####




## Initial life course data which is modified in the simulation ##

## THIS FUNCTION IS MODIFIED WHEN MAKING THE PARAMETER ADJUSTMENTS
## SEE INSTRUCTIONS COMMENTED IN CAPS AT THE PLACES WHERE THE CHANGES
## ARE MADE.

#### DATA USED AND MODIFIED IN SIMULATIONS #####

# Lifecourse function

create_lifecourse <- function() {
  
  
  d <- data.table(
    
    # Age
    
    age_m = age_m,           # ages in months
    age_y = age_m/12,        # ages in years
    age_ster = age_ster()/12,  # age at sterility
    
    
    # Biological determinants of fertility
    
    fecundability = fec(), # Fecundability
    
    # Postpartum amenorrhea for live births
    postpartum = c(ceiling(wait_postpartum(20)), rep(NA_real_, NA_20m)),
    # non-susceptible period for intrauterine mortality and induced abortion
    postpartum_m = c(rep(wait_postpartum_m, 20), rep(NA_real_, NA_20m)), 
    
    
    # Union formation and dissolution probabilities
    
    
    # probability of separation
    separation =  separation,
    
    # probability of divorce
    divorce = divorce,
    
    # probability of re-cohabiting
    repartner =  repartner,
    
    # Intended number of children
    
    intended_children = rep(intended_children(), len_40y),
    
    # Education
    
    education = rep(educational_attainment(), len_40y),
    
    # Month in which miscarriage occurs
    
    misc_month = c(miscarriage_time(20), rep(NA_real_, NA_20m)),
    
    # Month in which abortions occur
    
    abortion_month = c(abortion_time(20), rep(NA_real_, NA_20m)),
    
    
    # Random numbers for draws, vectorised here to improve computation speed
    
    RNG_u  = dqrunif(len_40y),  # Random numbers for union function
    RNG_c1 = dqrunif(len_40y),  # Random numbers for conception function
    RNG_c2 = dqrunif(len_40y),  # Random numbers for conception function
    RNG_c3 = dqrunif(len_40y)   # Random numbers for conception function                        
  )
  # Delay of first conception due to enrolment in education, cannot set fecundability
  # to exactly 0 because of the if statement that ends the while loop when fecundability
  # reaches 0, so I multiply by 0.000001 instead. 
  
  # FOR EDUCATIONAL DECOMPOSITION SET 'age_y <= 22' TO COMPARE WITH THE BASELINE
  # OF ALL WOMEN BEING LOW EDUCATED, REMEMBER TO CHANGE BACK TO 'age_y <= 18'
  # FOR THE INDIVIDUAL PARAMETER CONTRIBUTIONS
  
  d[, fecundability := fifelse(education == 1 & age_y <= 18, 0.000000000001,
                               fecundability)]
  
  d[, fecundability := fifelse(education == 2 & age_y <= 20, 0.000000000001,
                               fecundability)]
  
  d[, fecundability := fifelse(education == 3 & age_y <= 22, 0.000000000001,
                               fecundability)]
  
  # Generating the misc_month2 variable for miscarriages that occur after month 1
  
  d[, misc_month2 := fifelse(misc_month %in% 1:8, misc_month - 1, misc_month)]
  
  
  # Different cohabitation probability distributions by educational attainment
  
  #  SET cohabitation3 FOR education == 1 FOR THE DECOMPOSITION
  
  d[, cohabitation := fcase(education == 1, cohabitation1,
                            education == 2, cohabitation2,
                            education == 3, cohabitation3)]
  
  # Different marriage probability distributions by educational attainment
  
  #  SET marriage3 FOR education == 1 FOR THE DECOMPOSITION
  
  d[, marriage := fcase(education == 1, marriage1,
                        education == 2, marriage2,
                        education == 3, marriage3)]
  
  
  # Share of women by education who experience the union event
  
  # INDIVIDUALLY CHANGE THE NUMBER 1'S TO 3'S IN THE BRACKETS FOR education == 1,
  # SIMULATE, AND THEN CHANGE BACK AGAIN AND DO THE SAME FOR THE NEXT BRACKET. 
  # SO FIRST CHANGE 'share_cohabited_edu[1]' TO 'share_cohabited_edu[3]', SIMULATE,
  # THEN CHANGE IT BACK TO 'share_cohabited_edu[1]'AND DO THE SAME FOR THE OTHER 4 BRACKETS
  
  d[, share_union := fcase(
    education == 1, c(share_cohabited_edu[1],
                      share_married_edu[1],
                      share_separated_edu[1],
                      share_divorced_edu[1],
                      share_repartnered_edu[1],rep(NA_real_, (len_40y-5))),
    education == 2, c(share_cohabited_edu[2],
                      share_married_edu[2],
                      share_separated_edu[2],
                      share_divorced_edu[2],
                      share_repartnered_edu[2], rep(NA_real_, (len_40y-5))),
    education == 3, c(share_cohabited_edu[3],
                      share_married_edu[3],
                      share_separated_edu[3],
                      share_divorced_edu[3],
                      share_repartnered_edu[3], rep(NA_real_, (len_40y-5))))]
  
  
  setorderv(x = d, cols = colnames_input_data)
  
  
  return(d)
}


#####

## SIMPLE TROUBLESHOOTING OF THE SIMULATION
#
# z = 1
# 
# repeat{
# 
# z = z + 1  
#   
# test <- 
# create_lifecourse()
# 
# test2 <- union_function(test)
# 
# test3 <- reproduction_f(test2)
# 
#  if(z > 1000){
#    break
# }
# 
# }
# 
# test4 <- summary_f1(test3)


#### UPDATING FUNCTIONS THAT DO NOT CHANGE WITH THE CHANGES IN PARAMETERS ####
#### BUT NEED TO BE UPDATED WITH THE CHANGED PARAMETERS                   ####

# Folder where functions are located
path_f = "./functions_and_processing/unchanged_functions"

# Collecting the file paths of all R script files in the 'functions' folder
function_names <- list.files(path = path_f, pattern = "[.][RrSsQq]$", full.names = TRUE)

# Sourcing all the function R scripts
lapply(function_names, source)

#####







#### RUNNING THE SIMULATION ####

# Choose either serial or parallel processing. For serial processing leave
# 'cores_par = 1'. Sample size is then n*times. If you choose parallel processing,
# assign the number of cores and times the simulation runs based on the CPU you
# use and the sample size you want (n*cores*times = sample size). It is
# recommended to limit the number of cores to the number of physical cores
# (not logical cores, i.e. threads) the CPU has, and leave some (at least one)
# cores free if performing other tasks on the computer at the same time.


# Number of physical cores available in computer that is used. Set logical = TRUE
# to get the number of physical cores + threads

# detectCores(logical = FALSE)


test_vec <- list()     # empty data list that is populated in in each cluster
test_vec_sum <- list() # empty summary list that is populated in in each cluster


n <- 1                     # Number of simulation runs in each cluster. Multiply this
# value by cores and times to get the total sample size
cores_par <- 1             # Number of cores used 
times_par <- 1             # Number of times the repeat function repeats


# Names of objects and functions that need to be loaded to each core

export_names <- objects()

function_names <- as.vector(lsf.str())

# Setting the number of cores to use

cl <- makePSOCKcluster(cores_par, 
                       # less latency in communication between workers (Unix)
                       rscript_args=c("-e", shQuote('options(socketOptions="no-delay")')), 
                       # where large amounts of data are to be transferred and 
                       # all the nodes are little-endian, communication may be 
                       # substantially faster if useXDR is set to false 
                       useXDR = FALSE,
                       # If true (default) the workers will load the methods
                       # package: not loading it saves ca 30% of the startup 
                       # CPU time of the cluster
                       methods = FALSE)


# Exporting the objects that are the same for all cores

clusterExport(cl, export_names, envir=environment())

# Exporting the required functions

clusterExport(cl, function_names, envir=environment())

# Loading required packages to the cores

clusterEvalQ(cl, using("data.table", "MCMCglmm", "kableExtra", "magick",
                       "magick", "dqrng", "parallel"))





# Iteration counter
z = 1

# Time object
time <- c()



repeat{  
  
  # Stop parallel processing when the function stops, whether due to an error
  # or reaching the desired number of iterations
  on.exit(stopCluster(cl))
  
  # Start timer    
  time <- proc.time()[[3]]
  
  # Generating the data (lifecourse) that is modified   
  
  clusterEvalQ(cl, test_vec <- data.table(agent = 1:n, lifecourse = replicate(n, create_lifecourse(),
                                                                              simplify = FALSE)))
  
  
  # Running union function and conception function in parallel
  
  clusterEvalQ(cl,
               test_vec <- 
                 # Union function
                 test_vec[, lifecourse := lapply(lifecourse, union_f)
                          # Conception function         
                 ][, lifecourse := lapply(lifecourse, reproduction_f)
                   # Summary function  
                 ][, lifecourse := lapply(lifecourse, summary_f1)
                   # Unlisting the list column of data.tables  
                 ][, unlist(lifecourse, recursive = FALSE), by = (agent)]
  )
  
  # Adding the current iteration of data to the summary object
  clusterEvalQ(cl,
               test_vec_sum <-  
                 rbindlist(list(test_vec_sum, test_vec))
  )
  
  # Removing excess object 
  clusterEvalQ(cl,
               rm(test_vec)
  )
  
  
  
  
  # Print iteration
  print(z)
  
  # Time iteration
  print(proc.time()[[3]]-time)
  
  # Add one iteration to the count
  
  z = z + 1
  
  # Once the desired number of iterations has been reached, break the loop
  
  if(z > times_par){
    
    test_vec_sum <- rbindlist(
      clusterEvalQ(cl,
                   return(test_vec_sum)
      )
    )
    
    stopCluster(cl)
    
    break
  }
  
}

# Saving the summaries by individual 

# saveRDS(test_vec_sum, file = "./simulation/exact_replication_data/.RDS")


# Cohort summary list
object_test_sum <- summary_f2(test_vec_sum)


# Data table used to compare results
obj_sum_dat <- data.table("Indicator" = 
                             c("Mean age at first cohabitation ",
                               "Mean age at first marriage",
                               "Mean age at first separation, no previous divorce",
                               "Mean age at first divorce ",
                               "Mean age at first repartnering",
                               "Percent ever cohabited",
                               "Percent ever married ",
                               "Percent cohabited & never separated or married ",
                               "Percent marriage ","Percent separation",
                               "Percent divorce",
                               "Percent repartnering",
                               "Mean age at first birth", 
                               "Mean age second birth",
                               "Mean age third birth",
                               "Mean age fourth birth",
                               "Completed cohort fertility",
                               "Fertility gap (intended - completed)",
                               "0 children (%)",
                               "1 child (%)",
                               "2 children (%)",
                               "3 children (%)", 
                               "4+ children (%)",
                               "Miscarriages per live birth",
                               "Percent unintended pregnancies",
                               "Abortion ratio 2000-2020 (abortions per 1,000 live births)"
                               #,"Percent births outside of coresidential unions"
                          ),"Value" = unlist(unname(transpose(object_test_sum))))


# saving the output as an RDS file
# saveRDS(obj_sum_dat,
#         file = "./simulation/multiple_par_change/RDS_files/.RDS") # change file name


# Producing the output table of the summary data (for the table in the paper)

#
# test_vec_sum<-
# readRDS(file = "./simulation/exact_replication_data/base_model_100k_10feb25.RDS")

# object_test_sum <-
# summary_f2(test_vec_sum)

# 
# sum_table<-  data.table("Indicator" =
#  c("Mean age at first cohabitation (GGS, LISS)",
#    "Mean age at first marriage (CBS, 2004-2014)<sup>1</sup>",
#    "Mean age at first separation, no previous divorce (GGS, LISS)",
#    "Mean age at first divorce (GGS, LISS)",
#    "Mean age at first repartnering",
#    "Percent ever cohabited (GGS, estimate)",
#    "Percent ever married (CBS, estimate)<sup>2</sup>",
#    "Percent cohabited & never separated or married (CBS, estimate)<sup>3</sup>",
#    "Percent marriage (cohabitation to marriage; CBS, estimate)<sup>3</sup>",
#    "Percent separation (CBS, estimate)<sup>4</sup>", "Percent divorce (CBS, estimate)<sup>5</sup>",
#    "Percent repartnering (Finnish register data<sup>6</sup>, estimate)",
#    "Mean age at first birth (HFD, 1979 cohort)<sup>7</sup>",
#    "Mean age second birth (HFD, 1979 cohort)<sup>7</sup>",
#    "Mean age third birth (HFD, 1979 cohort)<sup>7</sup>",
#    "Mean age fourth birth (HFD, 1979 cohort)<sup>7</sup>",
#    "Completed cohort fertility within coresidential unions (total births in parenthesis) (HFD, 1979 cohort)<sup>7</sup>",
#    "Fertility gap within coresidential unions (total births in parenthesis)",
#    "0 children (HFD, 1969 cohort, %)",
#    "1 child (HFD, 1969 cohort, %)",
#    "2 children (HFD, 1969 cohort, %)",
#    "3 children (HFD, 1969 cohort, %)",
#    "4+ children (HFD, 1969 cohort, %)",
#    "Miscarriages per live birth",
#    "Percent unintended pregnancies<sup>8</sup>",
#    "Abortion ratio 2000-2020<sup>9</sup> (abortions per 1,000 live births)"),
# "Demographic data" =
#  c(24.5, 29.7, 28.5, 37.6, NA_real_, 95.0, 70.0, 10.0,
#                   58.0, 32, 27.6, 75, 29.2, 31.6, 33.1, 34.5, "1.66 (1.80)", "0.364 (0.227)",
#                   17.6, 18.5, 42.7, 15.6, 5.6, NA_real_, 20, 154),
# "Simulation results (1974-1984 cohort)" =
#   unlist(matrix(object_test_sum)))



## Exporting the table (fill in file name and format)
#
# knitr::kable(sum_table,
#              align = 'c',
#              format = "html",
#              escape = FALSE,
#              digits = 3,
#              table.attr = "style='width:100%;'") %>%
#   kable_styling(font_size = 26) %>%
#   kable_classic(html_font = "serif", full_width = TRUE) %>%
#   gsub("font-size: initial !important;",
#        "font-size: 26pt !important;",
#        .) %>%
#   save_kable(file = "./simulation/.png",  # change file name
#              zoom = 5,
#              bs_theme = "flatly",
#              density = 300)



## SUMMARY BY EDUCATION
# 
# object_test_sum_edu <-
#   summary_f3(test_vec_sum)


## Exporting the table (fill in file name and format)
#
# knitr::kable(object_test_sum_edu,
#              align = 'c',
#              format = "html",
#              escape = FALSE,
#              digits = 3,
#              table.attr = "style='width:100%;'") %>%
#   kable_styling(font_size = 26) %>%
#   kable_classic(html_font = "serif", full_width = TRUE) %>%
#   gsub("font-size: initial !important;",
#        "font-size: 26pt !important;",
#        .) %>%
#   save_kable(file = "./simulation/base_model_edu.png", # change file name
#              zoom = 5,
#              bs_theme = "flatly",
#              density = 300)







### SIMULATION TO ESTIMATE VARIATION BETWEEN MULTIPLE (10) RUNS OF 100,000 SAMPLE

test_vec <- list()     # empty data list that is populated in in each cluster
test_vec_sum <- list() # empty summary list that is populated in in each cluster
simulation_tab <- list()
simulation_sum <- list()


n <- 1                     # Number of simulation runs in each cluster. Multiply this
# value by cores and times to get the total sample size
cores_par <- 1             # Number of cores used 
times_par <- 1             # Number of times the repeat function repeats


# Names of objects and functions that need to be loaded to each core

export_names <- objects()

function_names <- as.vector(lsf.str())

# Setting the number of cores to use

cl <- makePSOCKcluster(cores_par, 
                       # less latency in communication between workers (Unix)
                       rscript_args=c("-e", shQuote('options(socketOptions="no-delay")')), 
                       # where large amounts of data are to be transferred and 
                       # all the nodes are little-endian, communication may be 
                       # substantially faster if useXDR is set to false 
                       useXDR = FALSE,
                       # If true (default) the workers will load the methods
                       # package: not loading it saves ca 30% of the startup 
                       # CPU time of the cluster
                       methods = FALSE)


# Exporting the objects that are the same for all cores

clusterExport(cl, export_names, envir=environment())

# Exporting the required functions

clusterExport(cl, function_names, envir=environment())

# Loading required packages to the cores

clusterEvalQ(cl, using("data.table", "MCMCglmm", "kableExtra", "magick",
                       "magick", "dqrng", "parallel"))





# Iteration counter for simulation
z = 1

# Iteration counter for summary

y = 1

# Time object
time <- c()

repeat{  
  
  on.exit(stopCluster(cl))
  
  # Start timer    
  time <- proc.time()[[3]]
  
  # Generating the data (lifecourse) that is modified   
  
  clusterEvalQ(cl, test_vec <- data.table(agent = 1:n, lifecourse = replicate(n, create_lifecourse(),
                                                                              simplify = FALSE)))
  
  
  # Running union function and conception function in parallel
  
  clusterEvalQ(cl,
               test_vec <- 
                 # Union function
                 test_vec[, lifecourse := lapply(lifecourse, union_f)
                          # Conception function         
                 ][, lifecourse := lapply(lifecourse, reproduction_f)
                   # Summary function  
                 ][, lifecourse := lapply(lifecourse, summary_f1)
                   # Unlisting the list column of data.tables  
                 ][, unlist(lifecourse, recursive = FALSE), by = (agent)]
  )
  
  # Adding the current iteration of data to the summary object
  clusterEvalQ(cl,
               test_vec_sum <-  
                 rbindlist(list(test_vec_sum, test_vec))
  )
  
  # Removing excess object 
  clusterEvalQ(cl,
               rm(test_vec)
  )
  
  
  if(z > times_par &
     y <= 10){
    
    test_vec_sum <- rbindlist(
      clusterEvalQ(cl,
                   return(test_vec_sum)
      )
    )
    
    z = 1
    
    simulation_sum <- summary_f2(test_vec_sum)
    simulation_tab <- rbindlist(list(simulation_tab, simulation_sum))
    
    test_vec <- list()
    test_vec_sum <- list() 
    
    y = y + 1
    
  }
  
  else if(y > 10){
    
    # Adding row with mean value of each column
    simulation_tab <- rbind(setDF(simulation_tab), c(sapply(setDF(simulation_tab), mean, na.rm = TRUE)))
    
    # Adding row with minimum value of each column
    simulation_tab <- rbind(setDF(simulation_tab), c(sapply(setDF(simulation_tab), min, na.rm = TRUE)))
    
    # Adding row with maximum value of each column
    simulation_tab <- rbind(setDF(simulation_tab), c(sapply(setDF(simulation_tab), max, na.rm = TRUE)))
    
    # Transposing the data
    simulation_tab <- setDT(data.table::transpose(simulation_tab, keep.names = "names"))
    
    # Setting the column names for the minimum and maximum values
    setnames(simulation_tab, (y + 1):(y + 3), c("mean", "min", "max"))
    
    stopCluster(cl)
    
    break
    
  } else {
    
    # Add one iteration to the count
    z = z + 1
    
    # Print iteration
    print(z-1)
    
    # Time iteration
    print(proc.time()[[3]]-time)
    
  }
  
}

# saveRDS(simulation_tab, file = "./simulation/multiple_par_change/RDS_files/variation_10runs_100k_10feb25.RDS")





