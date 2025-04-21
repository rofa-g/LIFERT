
## Simulation of a woman's unions across her life course ##

#### UNION FORMATION SIMULATION #####

# Union formation function

union_f <- function(data){
  
  
  # Length of data
  
  len_dat        <- length(age_m)
  
  # Vectors
  
  age_m          <- data$age_m
  cohabitation   <- data$cohabitation
  separation     <- data$separation
  marriage       <- data$marriage
  divorce        <- data$divorce
  repartner      <- data$repartner
  
  # Output states and counts; vectors of zeroes
  
  
  cohabiting        <- rep(0, length = len_dat)
  married           <- rep(0, length = len_dat)
  separated         <- rep(0, length = len_dat)
  divorced          <- rep(0, length = len_dat)
  month_cohabited   <- rep(0, length = len_dat)
  month_married     <- rep(0, length = len_dat)
  month_separated   <- rep(0, length = len_dat)
  month_divorced    <- rep(0, length = len_dat)
  month_repartnered <- rep(0, length = len_dat)
  num_cohabitations <- rep(0, length = len_dat)
  num_marriages     <- rep(0, length = len_dat)
  num_separations   <- rep(0, length = len_dat)
  num_divorces      <- rep(0, length = len_dat)
  num_repartnering  <- rep(0, length = len_dat)
  
  # Random numbers for draws
  
  RNG_cohab         <- data$RNG_u[1]         # RNG for first cohabitation
  RNG_cohab_or_mar  <- data$RNG_u[2:20]      # RNG for higher order cohabitations
  RNG_mar           <- data$RNG_u[21:40]     # RNG for marriage
  RNG_sep           <- data$RNG_u[41:60]     # RNG for separation
  RNG_div           <- data$RNG_u[61:80]     # RNG for divorce
  RNG_repart        <- data$RNG_u[81:100]    # RNG for re-partnering
  RNG_share_sep     <- data$RNG_u[101:120]   # RNG for whether the woman separates
  RNG_share_div     <- data$RNG_u[121:140]   # RNG for whether the woman divorces
  RNG_share_repart  <- data$RNG_u[141:160]   # RNG for whether the woman re-partners
  
  
  # Transition probabilities
  
  p_cohab <- data$share_union[1] # Share who ever cohabit
  p_mar <- data$share_union[2] # Share who marry
  p_sep <- data$share_union[3] # Share who separate
  p_div <- data$share_union[4] # Share who divorce
  p_repart <- data$share_union[5] # Share who re-partner
  
  
  i = 1
  
  while (i <= len_dat){
    
    
    # If random number is greater than the probability of ever cohabiting, 
    # the woman never cohabits and the simulation ends
    if(p_cohab < RNG_cohab[num_cohabitations[i]+1] &
       num_cohabitations[i] == 0){
      i = i + len_dat
      
      # First cohabitation if cohabitation probability of first cohabitation is 
      # higher than randomly generated number and no previous cohabitations
    } else if(cohabitation[i] >= RNG_cohab[num_cohabitations[i]+1]
              & num_cohabitations[i] == 0){
      cohabiting[i:len_dat] = 1
      num_cohabitations[i:len_dat] = num_cohabitations[i] + 1
      month_cohabited[num_cohabitations[i]] = i 
      i = i + 1
      
      # If the current iteration is not the first month
    } else if(i > 1){
      
      # If currently cohabiting
      if(cohabiting[i] == 1){
        
        # If the cohabitation results in marriage
        if(RNG_cohab_or_mar[num_cohabitations[i]] <= p_mar){
          
          # Draw deciding the month in which the woman marries
          
          if(marriage[i-month_cohabited[num_cohabitations[i]]] >= RNG_mar[num_marriages[i]+1]){
            married[i:len_dat] = 1
            cohabiting[i:len_dat] = 0
            num_marriages[i:len_dat] = num_marriages[i] + 1
            month_married[num_marriages[i]] = i
            i = i + 1
          }
          
          # If no transition yet, move one month forward
          else{
            i = i + 1
          }
          
          # If the cohabitation does not result in marriage
        }else if(RNG_cohab_or_mar[num_cohabitations[i]] > p_mar){
          
          # If the woman stays cohabited for the rest of her reproductive life
          if(RNG_share_sep[num_separations[i] + 1] > p_sep){
            i = i + len_dat
          }
          
          # Else, if the cohabitation results in a separation
          else if(RNG_share_sep[num_separations[i] + 1] <= p_sep){
            
            # Draw deciding in which month the woman separates
            if(separation[i-month_cohabited[num_cohabitations[i]]] >= RNG_sep[num_separations[i]+1]){
              separated[i:len_dat] = 1
              cohabiting[i:len_dat] = 0
              num_separations[i:len_dat] = num_separations[i] + 1
              month_separated[num_separations[i]] = i
              i = i + 1
              
              # If no transition yet, move one month forward
            }else{
              i = i + 1
              
            }
            
          }
          
        }
        
        
        
        
        # If currently married
      } else if(married[i] == 1){
        
        # If the woman stays married for the rest of her reproductive life
        if(RNG_share_div[num_divorces[i] + 1] > p_div){
          i = i + len_dat
          }
        
        # Else, if the marriage  results in a divorce
        else if(RNG_share_div[num_divorces[i] + 1] <= p_div){
          
          # Draw deciding in which month the woman divorces
          if(divorce[i-month_married[num_marriages[i]]] >= RNG_div[num_divorces[i]+1]){
            divorced[i:len_dat] = 1
            married[i:len_dat] = 0
            num_divorces[i:len_dat] = num_divorces[i]+1
            month_divorced[num_divorces[i]] = i
            i = i + 1
            
            # If no transition yet, move one month forward
          }else{
            i = i + 1
          }
          
          
        }
        
        
        
        
        # If currently separated
      } else if(separated[i] == 1){
        
        # If the woman stays single for the rest of her reproductive life
       if(RNG_share_repart[num_repartnering[i] + 1] > p_repart){
          i = i + len_dat
          
        }
        
        # Else, if the separation results in re-partnering
        else if(RNG_share_repart[num_repartnering[i] + 1] <= p_repart){
          
          # Draw deciding in which month the woman re-partners
          if(repartner[i-month_separated[num_separations[i]]] >= RNG_repart[num_repartnering[i]+1]){
            cohabiting[i:len_dat] = 1
            separated[i:len_dat] = 0
            num_cohabitations[i:len_dat] = num_cohabitations[i] + 1
            num_repartnering[i:len_dat] = num_repartnering[i]+1
            month_cohabited[num_cohabitations[i]] = i
            month_repartnered[num_repartnering[i]] = i
            i = i + 1
            
            # If no transition yet, move one month forward
          }else{
            i = i + 1
          }
          
          
        }
        
        # If currently divorced
      } else if(divorced[i] == 1){
        
        # If the woman stays single for the rest of her reproductive life
        if(RNG_share_repart[num_repartnering[i] + 1] > p_repart){
          i = i + len_dat
        }
        
        # Else, if the divorce results in re-partnering
        else if(RNG_share_repart[num_repartnering[i] + 1] <= p_repart){
          
          # Draw deciding in which month the woman re-partners
          if(repartner[i-month_divorced[num_divorces[i]]] >= RNG_repart[num_repartnering[i]+1]){
            cohabiting[i:len_dat] = 1
            divorced[i:len_dat] = 0
            num_cohabitations[i:len_dat] = num_cohabitations[i] + 1
            num_repartnering[i:len_dat] = num_repartnering[i] + 1
            month_cohabited[num_cohabitations[i]] = i
            month_repartnered[num_repartnering[i]] = i
            i = i + 1
            
            # If no transition yet, move one month forward
          }else{
            i = i + 1
          }
          
        }
        
      }
      else{
        i = i + 1
        
        
        
      }
      
    }
    
    # If none of the above are true, change nothing and move one month forward
    # This condition is added in two places to account for both the case when 
    # the current iteration is the first month (here), and when it is not (above)
    else{
      i = i + 1
      
      
      
    }
    
  }
  # Replace the existing data with the modified data
  data$married            <- married
  data$divorced           <- divorced
  data$cohabiting         <- cohabiting
  data$separated          <- separated
  data$num_cohabitations  <- num_cohabitations
  data$month_cohabited    <- month_cohabited
  data$num_separations    <- num_separations
  data$month_separated    <- month_separated
  data$num_repartnering   <- num_repartnering
  data$month_repartnered  <- month_repartnered
  data$num_marriages      <- num_marriages
  data$month_married      <- month_married
  data$num_divorces       <- num_divorces
  data$month_divorced     <- month_divorced
  return(data)
}


#####
