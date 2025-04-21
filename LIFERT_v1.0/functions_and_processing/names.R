## Name vectors used in different functions ##



#### NAME VECTORS ####



## Educational levels

isced_names <- c("ISCED 0-2", "ISCED 3-4", "ISCED 5-8")


## Names for create_lifecourse

# Column names for input data (create_lifecourse function)

colnames_input_data <- 
  c("age_m",             "age_y",             "age_ster",          "fecundability",          
    "postpartum",        "postpartum_m",      "cohabitation",      "separation",        "marriage",         
    "divorce",           "repartner",         "intended_children", "education",         "RNG_u",            
    "RNG_c1",            "RNG_c2",            "RNG_c3",            "misc_month",        "misc_month2",      
    "abortion_month",    "share_union")


# Column names for summary by age and education

colname_sum_age_edu <- c(  "age_y", "education", "intended_children", "N" ,  "miscarriage" , "unintended" ,
                           "num_abortions", "num_cohabitations", "num_marriages", "num_separations" ,  "num_divorces" ,
                           "num_repartnering" )


# columns to be moved in create_lifecourse function

nm <- c("misc_month", "misc_month2", "abortion_month")





## Summary functions

# vector of columns for summaries (summary_f1)

name_sum <- c("N", "intended_children", "miscarriage", "unintended", "num_abortions", "month_married", 
              "month_divorced", "month_cohabited", "month_separated",
              "month_repartnered", "age_fb","age_2b", "age_3b","age_4b", "age_lb", "num_cohabitations",
              "num_marriages", "num_separations", "num_divorces", "num_repartnering", "education", "num_conceptions"
              #,"child_outside_cohab"
              )

# vector of columns that are kept (summary_f1)

name_keep <- c("N", "intended_children", "miscarriage", "unintended", "num_abortions", "age_fb","age_2b", "age_3b","age_4b", "age_lb",
               "age_cohabited", "age_separated", "age_married", "age_divorced",
               "age_repartnered","num_cohabitations",
               "num_marriages", "num_separations", "num_divorces", "num_repartnering", "education", "num_conceptions"
               #,"child_outside_cohab"
               )

# vector of columns that are kept (summary_f2)
name_summary <- c( "mean_age_cohabitation",   "mean_age_married",       "mean_age_separation",     "mean_age_divorce",       
                   "mean_age_repartnered",    "pct_ever_partnered",      "pct_ever_married",        "pct_never_separated",    
                   "pct_marriage",            "pct_separation",          "pct_divorce",            "pct_repartner",          
                   "mean_age_fb",             "mean_age_2b",             "mean_age_3b",             "mean_age_4b",            
                   "CCF",                    "fert_gap",                "pct_0_children",          "pct_1_children",         
                   "pct_2_children",          "pct_3_children",          "pct_4_children",          "share_miscarriage",      
                   "pct_unintended",          "share_abortions" #,"pct_birth_outside_cohab"
)

 
# colmns that are kept for the final summary table by education (summary_f3)

keep_cols_edu <- c("education",             "mean_age_cohabitation",   "mean_age_married",
                   "mean_age_separation",   "mean_age_divorce",      "mean_age_repartnered",   
                   "pct_ever_partnered",    "pct_ever_married",      "pct_marriage",          "pct_separation",          
                   "pct_divorce",      "pct_repartner",         "mean_age_fb",           "mean_age_2b",          
                   "mean_age_3b",           "mean_age_4b",           "CCF",                  "fert_gap",             
                   "pct_0_children",        "pct_1_children",        "pct_2_children",        "pct_3_children",       
                   "pct_4_children",        "share_miscarriage",     "pct_unintended",         
                   "share_abortions" #,"pct_birth_outside_cohab" 
)
# and without education

keep_cols <- keep_cols_edu[!keep_cols_edu == "education"]


# Names used in summary function (summary_f3)

summary_names <-   
  c("Mean age at first cohabitation","Mean age at first marriage", "Mean age at first separation",
    "Mean age at first divorce", "Mean age at first re-partnering",
    "Percent ever cohabited", "Percent ever married", "Percent marriage (cohabitation-marriage)",
    "Percent separation", "Percent divorce", "Percent re-partnering", "Mean age at first birth",
    "Mean age at second birth", "Mean age at third birth", "Mean age at fourth birth",
    "Completed Cohort Fertility", "Fertility gap", "Percent 0 children", "Percent 1 child",
    "Percent 2 children", "Percent 3 children", "Percent 4+ children", "Miscarriages per live birth",
    "Percent unintended pregnancies", 
    "Abortion ratio (abortions per 1,000 live births)"
    #,"Percent non-coresidential births"
)



#####
