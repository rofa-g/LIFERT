## Summary functions for summarising simulation output ##


#### SUMMARY FUNCTIONS ####

## FIRST STEP

# Summaries by agent (woman)

# Function for summaries of data data.tables

summary_f1 <- function(data){
  
  data <- 
    data[, age_fb := fifelse(all(N == 0), NA_real_, min(age_y[N == 1])),  # age at first birth
    ][, age_lb := fifelse(all(N == 0), NA_real_, min(age_y[N == max(N)]))  #age at last birth 
    ][, age_2b := fifelse(all(N < 2), NA_real_, min(age_y[N == 2]))  #age at last birth 
    ][, age_3b := fifelse(all(N < 3), NA_real_, min(age_y[N == 3]))  #age at last birth 
    ][, age_4b := fifelse(all(N < 4), NA_real_, min(age_y[N == 4]))  #age at last birth 
    ][, miscarriage := fifelse(max(miscarriage) == 0, NA_real_, max(miscarriage))
    #][, child_outside_cohab := sum(fifelse(conception == 9 & cohabiting == 0 & married == 0, 1, 0))
    ][, unintended := fifelse(max(unintended) == 0, NA_real_, max(unintended))
    ][, num_conceptions := fifelse(max(num_conceptions) == 0, NA_real_, max(num_conceptions))
    ][, lapply(.SD, function(x) fcase(min(x) == 1 & any(x > 1), max(x)-1,
                                      min(x) == 0 & any(x > 15) , min(x[x>0]),
                                      all(x == 0), NA_real_,
                                      default = max(x))), .SDcols = name_sum # counts from above vector
    ][, N := fifelse(is.na(N), 0, N) 
    ][, num_abortions := fifelse(is.na(num_abortions), 0, num_abortions)  
    ][, num_cohabitations := fifelse(is.na(num_cohabitations), 0, num_cohabitations)  
    ][, age_cohabited := (month_cohabited+180)/12  # age at first cohabitation
    ][, age_separated := fifelse(is.na(num_divorces), (month_separated+180)/12, NA_real_)  # age at first separation
    ][, age_repartnered := (month_repartnered+180)/12  # age at first repartnering 
    ][, age_married := (month_married+180)/12  # age at first marriage 
    ][, age_divorced := (month_divorced+180)/12  # age at first divorce
    ][, intended_children := fifelse(is.na(intended_children), 0, intended_children) # Intended number of children  
    ][, num_conceptions := fifelse(is.na(num_conceptions), 0, num_conceptions)
    ][, unintended := fifelse(is.na(unintended), 0, unintended)
    ][, ..name_keep] # select columns based on character vector, '..' means one level up
  # as the column names are one level above

}


## SECOND STEP

# Cohort summaries

# Summary function for summaries of summaries
summary_f2 <- function(data){
  data[, mean_age_cohabitation := mean(age_cohabited,na.rm=TRUE)
  ][, mean_age_married := mean(age_married,na.rm=TRUE) 
  ][, mean_age_separation := mean(age_separated,na.rm=TRUE)
  ][, mean_age_divorce := mean(age_divorced,na.rm=TRUE)
  ][, mean_age_repartnered := mean(age_repartnered,na.rm=TRUE)
  ][, pct_ever_partnered := 100-length(age_cohabited[is.na(age_cohabited)])/length(age_cohabited)*100  
  ][, pct_ever_married := 100-length(age_married[is.na(age_married)])/length(age_married)*100
  ][, pct_never_separated := length(age_separated[!is.na(age_cohabited) & is.na(age_separated) & is.na(age_married)])/length(agent)*100
  ][, pct_marriage := sum(na.omit(num_marriages))/sum(na.omit(num_cohabitations))*100
  ][, pct_separation := sum(na.omit(num_separations))/sum(na.omit(num_cohabitations))*100
  ][, pct_divorce := sum(na.omit(num_divorces))/sum(na.omit(num_marriages))*100
  ][, pct_repartner := sum(na.omit(num_repartnering))/sum(na.omit(num_separations), na.omit(num_divorces))*100  
  ][, mean_age_fb := mean(age_fb,na.rm=TRUE)
  ][, mean_age_2b := mean(age_2b,na.rm=TRUE)
  ][, mean_age_3b := mean(age_3b,na.rm=TRUE)
  ][, mean_age_4b := mean(age_4b,na.rm=TRUE)
  ][, CCF := mean(N)  
  ][, fert_gap := mean_intend_fam - mean(N)  
  ][, pct_0_children := length(N[N == 0])/length(agent)*100
  ][, pct_1_children := length(N[N == 1])/length(agent)*100
  ][, pct_2_children := length(N[N == 2])/length(agent)*100
  ][, pct_3_children := length(N[N == 3])/length(agent)*100 
  ][, pct_4_children := length(N[N >= 4])/length(agent)*100
  ][, share_miscarriage := sum(miscarriage, na.rm = TRUE)/sum(N) # Count conceptions instead to get share of miscarriages per conceptions??
  ][, pct_unintended := sum(unintended)/sum(num_conceptions)*100
  ][, share_abortions := sum(num_abortions)/sum(N)*1000
  # ][, pct_birth_outside_cohab := sum(!is.na(child_outside_cohab))/sum(N)*100
  ][, ..name_summary 
  ][1,] # Select only the first row of the table
  
}


# Function that takes the unlisted simulated data summarised by woman (summary_f1)
# and returns a table with a cohort summary by educational attainment 

summary_f3 <- function(data){
  
  data <- 
    data[, mean_age_cohabitation := mean(age_cohabited,na.rm=TRUE), by = education 
    ][, mean_age_married := mean(age_married,na.rm=TRUE), by = education 
    ][, mean_age_separation := mean(age_separated,na.rm=TRUE), by = education 
    ][, mean_age_divorce := mean(age_divorced,na.rm=TRUE), by = education 
    ][, mean_age_repartnered := mean(age_repartnered,na.rm=TRUE), by = education 
    ][, pct_ever_partnered := 100-length(age_cohabited[is.na(age_cohabited)])/length(age_cohabited)*100, by = education   
    ][, pct_ever_married := 100-length(age_married[is.na(age_married)])/length(age_married)*100, by = education 
    ][, pct_marriage := sum(na.omit(num_marriages))/sum(na.omit(num_cohabitations))*100, by = education 
    ][, pct_separation := sum(na.omit(num_separations))/sum(na.omit(num_cohabitations))*100, by = education  
    ][, pct_divorce := sum(na.omit(num_divorces))/sum(na.omit(num_marriages))*100, by = education 
    ][, pct_repartner := sum(na.omit(num_repartnering))/sum(na.omit(num_separations), na.omit(num_divorces))*100, by = education   
    ][, mean_age_fb := mean(age_fb,na.rm=TRUE), by = education 
    ][, mean_age_2b := mean(age_2b,na.rm=TRUE), by = education 
    ][, mean_age_3b := mean(age_3b,na.rm=TRUE), by = education 
    ][, mean_age_4b := mean(age_4b,na.rm=TRUE), by = education 
    ][, CCF := mean(N), by = education   
    ][, fert_gap := mean_intend_fam - mean(N), by = education   
    ][, pct_0_children := length(N[N == 0])/length(agent)*100, by = education 
    ][, pct_1_children := length(N[N == 1])/length(agent)*100, by = education 
    ][, pct_2_children := length(N[N == 2])/length(agent)*100, by = education 
    ][, pct_3_children := length(N[N == 3])/length(agent)*100, by = education 
    ][, pct_4_children := length(N[N >= 4])/length(agent)*100, by = education 
    ][, share_miscarriage := sum(miscarriage, na.rm = TRUE)/sum(N), by = education  # Count conceptions instead to get share of miscarriages per conceptions??
    ][, pct_unintended := sum(unintended, na.rm = TRUE)/sum(num_conceptions)*100, by = education 
    ][, share_abortions := sum(num_abortions)/sum(N)*1000, by = education  
    # ][, pct_birth_outside_cohab := sum(!is.na(child_outside_cohab))/sum(N)*100, by = education
    ][, ..keep_cols_edu]  
  
  data <- unique(data, by = "education")
  
  data <- data.table::transpose(data, keep.names = "Educational level", make.names = "education")
  
  data <- data[, `Educational level` := summary_names]
  
  setcolorder(data, neworder = c("Educational level", "1", "2", "3"))
  
  setnames(data, 2:4, isced_names)
  
  return(data)
}

#####
