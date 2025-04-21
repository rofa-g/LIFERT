## Simulation model of Dutch fertility using mainly GGS and LISS survey data 



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

# Folder where functions are located
path_f = "./functions_and_processing/"

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


n <- 1                   # Number of simulation runs in each cluster. Multiply this
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
                              "Abortion ratio 2000-2020 (abortions per 1,000 live births)"),
                          "Value" = unlist(unname(transpose(object_test_sum))))


# saving the output as an RDS file
# saveRDS(obj_sum_dat, file = "./simulation/multiple_par_change/RDS_files/.RDS") # change file name


# Producing the output table of the summary data (for the table in the paper)


# object_test<-
# readRDS(file = "./simulation/exact_replication_data/base_model_100k_10feb25.RDS")
# 
# object_test_sum <-
# summary_f2(object_test)
# 
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
#              table.attr = "style='width:60%;'") %>%
#   kable_styling(font_size = 26) %>%
#   kable_classic(html_font = "serif", full_width = TRUE) %>%
#   gsub("font-size: initial !important;",
#        "font-size: 26pt !important;",
#        .) %>%
#   save_kable(file = "./simulation/#.png",  # change file name
#              zoom = 5,
#              bs_theme = "flatly",
#              density = 300)



## SUMMARY BY EDUCATION
#
# object_test_sum_edu <-
#   summary_f3(object_test)


## Exporting the table (fill in file name and format)
#
# knitr::kable(object_test_sum_edu, digits = 2,
#              align = 'c', format = "html",
#              escape = FALSE) %>%
#   kable_styling(font_size = 26) %>%
#   kable_classic(html_font = "serif", full_width = TRUE) %>%
#   gsub("font-size: initial !important;",
#        "font-size: 26pt !important;",
#        .) %>%
#   save_kable(file = "./simulation/.png", # change file name
#              zoom = 5,
#              bs_theme = "flatly",
#              density = 300)



### SIMULATION TO ESTIMATE VARIATION BETWEEN MULTIPLE (10) RUNS OF 100,000 SAMPLE

test_vec <- list()     # empty data list that is populated in in each cluster
test_vec_sum <- list() # empty summary list that is populated in in each cluster
simulation_tab <- list()
simulation_sum <- list()


n <- 1                   # Number of simulation runs in each cluster. Multiply this
# value by cores and times to get the total sample size
cores_par <- 1            # Number of cores used 
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

#saveRDS(simulation_tab, file = "./simulation/multiple_par_change/RDS_files/.RDS") #change file name





