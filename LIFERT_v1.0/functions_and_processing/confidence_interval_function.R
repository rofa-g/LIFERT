## Simulation of "Confidence intervals" 


# Column names for the empty data.tables

colname_sum_obj <- c( "agent",                 "N",                     "intended_children",     "miscarriage",           "unintended",            "num_abortions",        
                      "age_fb",                "age_2b",                "age_3b",                "age_4b",                "age_lb",                "age_cohabited",        
                      "age_separated",         "age_married",           "age_divorced",          "age_repartnered",       "num_cohabitations",     "num_marriages",        
                      "num_separations",       "num_divorces",          "num_repartnering",      "education",             "mean_age_cohabitation", "mean_age_married",     
                      "mean_age_separation",   "mean_age_divorce",      "mean_age_repartnered",  "pct_ever_partnered",    "pct_ever_married",      "pct_never_separated",  
                      "pct_marriage",          "pct_separation",        "pct_divorce",           "pct_repartner",         "mean_age_fb",           "mean_age_2b",          
                      "mean_age_3b",           "mean_age_4b",           "CTFR",                  "fert_gap",              "pct_0_children",        "pct_1_children",       
                      "pct_2_children",        "pct_3_children",        "pct_4_children",        "share_miscarriage",     "pct_unintended",        "share_abortions")


# Confidence intervals (mean, min, and max values for repeated simulations)

confidence_interval_f <- function(reps = 1, cores = 1, times = 1){
  
  # Iteration counter
  z = 1
  
  # Time object
  time <- c()
  
  # Empty data.tables that are populated in the function
  
  simulation_row <- setNames(data.table(matrix(nrow = 0, ncol = length(colname_sum_obj))), colname_sum_obj)
  
  simulation_sum <- setNames(data.table(matrix(nrow = 0, ncol = length(colname_sum_obj))), colname_sum_obj)
  
  simulation_tab <- setNames(data.table(matrix(nrow = 0, ncol = length(colname_sum_obj))), colname_sum_obj)
  
  
  repeat{
    
    # Start timer    
    time <- proc.time()[[3]]
    
    # Simulation
    simulation_row <- simulation_parallel(cores = cores, times = times)
    
    # Cohort summary
    simulation_sum <- summary_f2(simulation_row)
    
    # Combined data.table
    simulation_tab <- rbindlist(list(simulation_tab, simulation_sum))
    
    # Print iteration
    print(z)
    # Time iteration
    print(proc.time()[[3]]-time)
    
    # Add one to counter
    z = z + 1
    
    # Exit loop when counter exceeds 'reps'
    if(z > reps){
      break
    }
    
    
  }
  
  # Selecting the relevant columns
  simulation_tab <- simulation_tab[, 22:48]
  
  # Adding row with mean value of each column
  simulation_tab <- rbind(setDF(simulation_tab), c(sapply(setDF(simulation_tab), mean, na.rm = TRUE)))
  
  # Adding row with minimum value of each column
  simulation_tab <- rbind(setDF(simulation_tab), c(sapply(setDF(simulation_tab), min, na.rm = TRUE)))
  
  # Adding row with maximum value of each column
  simulation_tab <- rbind(setDF(simulation_tab), c(sapply(setDF(simulation_tab), max, na.rm = TRUE)))
  
  # Transposing the data
  simulation_tab <- setDT(transpose(simulation_tab, keep.names = "names"))
  
  # Setting the column names for the minimum and maximum values
  setnames(simulation_tab, (reps + 2):(reps + 4), c("mean", "min", "max"))
  
}
