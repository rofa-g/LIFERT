## Initial life course data which is modified in the simulation ##

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
  
  d[, fecundability := fifelse(education == 1 & age_y <= 18, 0.000000000001,
                               fecundability)]
  
  d[, fecundability := fifelse(education == 2 & age_y <= 20, 0.000000000001,
                               fecundability)]
  
  d[, fecundability := fifelse(education == 3 & age_y <= 22, 0.000000000001,
                               fecundability)]
  
  # Generating the misc_month2 variable for miscarriages that occur after month 1
  
  d[, misc_month2 := fifelse(misc_month %in% 1:8, misc_month - 1, misc_month)]
  
  
  # Different cohabitation probability distributions by educational attainment
  
  d[, cohabitation := fcase(education == 1, cohabitation1,
                            education == 2, cohabitation2,
                            education == 3, cohabitation3)]
  
  # Different marriage probability distributions by educational attainment
  
  d[, marriage := fcase(education == 1, marriage1,
                        education == 2, marriage2,
                        education == 3, marriage3)]
  
  
  # Share of women by education who experience the union event
  
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