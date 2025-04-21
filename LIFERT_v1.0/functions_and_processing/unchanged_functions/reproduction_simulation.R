## Simulation of a woman's reproductive events during her life course ##


#### SIMULATION OF REPRODUCTIVE LIFE COURSE ####

## (Human) reproduction function

reproduction_f <- function(data) {
  
  # Age
  
  age_y <-  data$age_y
  
  # Length of the data 
  len_dat           <- nrow(data)
  
  # Union formation
  cohabiting        <- data$cohabiting 
  married           <- data$married   
  separated         <- data$separated   
  divorced          <- data$divorced  
  month_cohabited   <- data$month_cohabited
  month_married     <- data$month_married   
  month_separated   <- data$month_separated
  month_divorced    <- data$month_divorced 
  month_repartnered <- data$month_repartnered
  num_cohabitations <- data$num_cohabitations 
  num_marriages     <- data$num_marriages
  num_separations   <- data$num_separations 
  num_divorces      <- data$num_divorces  
  num_repartnering  <- data$num_repartnering
  
  # Intended number of children
  intended_children <- data$intended_children
  
  # Biological determinants
  fecundability       <- data$fecundability
  iu_mort             <- ium   # Intrauterine mortality per month
  intended_spacing    <- intended_spacing
  spacing_efficacy    <- spacing_efficacy
  spacing_efficacy_30 <- spacing_efficacy_30
  stopping_efficacy   <- stopping_efficacy
  postpartum          <- data$postpartum
  postpartum_m        <- data$postpartum_m 
  misc_month          <- data$misc_month
  misc_month2         <- data$misc_month2
  abortion_month      <- data$abortion_month
  
  # Educational attainment
  
  education <- data$education
  
  # Biological outcomes
  
  conception  <- zero_vector # Vector for conceptions across lifespan
  spacing     <- zero_vector # Currently spacing
  stopping    <- zero_vector # Currently stopping
  miscarriage <- zero_vector # Count of the number of conceptions that failed
  
  # Probability of abortion
  
  prob_abortion <- prob_abortion # Probability of abortion
  num_abortions <- zero_vector   # Number of abortions
  
  # Output values
  num_conceptions <- zero_vector # Count of the number of conceptions
  N               <- zero_vector # Count of the number of live births
  unintended      <- zero_vector # Count number of unintended births 
  
  # Random number vectors
  
  RNG_c1          <- data$RNG_c1 # Random numbers for conception draws while spacing/stopping
  RNG_c2          <- data$RNG_c2 # Random numbers for conception draws when not spacing/stopping
  RNG_miscarriage <- data$RNG_c3[1:60]   # Random numbers for miscarriage draws
  RNG_abortion    <- data$RNG_c3[61:120] # Random numbers for abortion draws
  
  
  
  i = 1
  
  while(i <= (len_dat-9)) { # no conceptions may end outside censoring, 
    # subtract conception time from total duration
    
    # If it is the first month
    if(i == 1){
      
      # If the woman never cohabits, she never conceives, and the loop ends
      if(month_cohabited[i] == 0){
        i = i + len_dat
        
        # If the woman cohabits in the first month, move one month forward
      }else if(cohabiting[i] == 1){
        spacing[i] == 1
        i = i + 1
      }
      
      # If not cohabiting in the first month, move forward to first cohabitation
      else if(cohabiting[i] == 0) {
        i = month_cohabited[1] 
      }
      
      
      # If the woman has reached sterility, then no more conceptions. The loop ends.
    }else if(fecundability[i] == 0 & 
             conception[i-1] == 0) { # Sterile
      i = i + len_dat
      
      # If not cohabiting or married, move to next cohabitation (unless no more
      # repartnering)
      
    }else if(cohabiting[i] == 0 &
             married[i] == 0){
      i = i + 1
      
      
      # IF COHABITING OR MARRIED: 
    }else if(married[i] == 1 |
             cohabiting[i] == 1){
      
      # IF WOMAN HAS NOT REACHED HER INTENDED FERTILITY (SPACING BEGINS)  
      if(N[i] < intended_children[i]){
        
        # IF THERE WAS NO CONCEPTION DURING PREVIOUS MONTH (WOMAN IS NOT CONCEIVING) 
        if(conception[i-1] == 0){
          
          # IF THE WOMAN HAS NOT REACHED HER INTENDED SPACING BEFORE CONCEPTION
          if(spacing[i-1] < intended_spacing[N[i]+1]){
            
            # If contraception is successful, no conception, spacing starts or 
            # continues if intended spacing not yet reached, move one month forward
            if(RNG_c1[i] > spacing_efficacy*fecundability[i] & age_y[i] < 25 |
               RNG_c1[i] > spacing_efficacy_30*fecundability[i] & age_y[i] >= 25){
              conception[i] = 0
              spacing[i] = spacing[i-1] + 1
              i = i + 1
            }
            
            # Else, if contraception is unsuccessful; conception occurs,  
            # move one month forward. Add one conception.
            else if (RNG_c1[i] <= spacing_efficacy*fecundability[i] & age_y[i] < 25 |
                     RNG_c1[i] <= spacing_efficacy_30*fecundability[i] & age_y[i] >= 25 ){
              conception[i] = 1
              num_conceptions[i:len_dat] = num_conceptions[i-1]+1
              spacing[i] = spacing[i-1] + 1
              # Add one unintended pregnancy
              unintended[i:len_dat] = unintended[i-1] + 1 
              i = i + 1
            }
          }  
          
          # IF THE WOMAN REACHED HER INTENDED SPACING BEFORE CONCEPTION
          else if(spacing[i-1] >= intended_spacing[N[i]+1]){
            
            # Planned conception succeeds. Add one conception.
            if(RNG_c2[i] <= fecundability[i]){
              conception[i] = 1  
              num_conceptions[i:len_dat] = num_conceptions[i-1]+1
              spacing[i] = spacing[i-1] + 1
              i = i + 1  
            }
            # Planned conception fails. Add one month of spacing. 
            else if(RNG_c2[i] > fecundability[i]){
              conception[i] = 0  
              spacing[i] = spacing[i-1] + 1
              i = i + 1
            }
            
          }
          
        }  
        # IF CONCEPTION OCCURRED DURING PREVIOUS MONTH (WOMAN IS CONCEIVING)
        else if(conception[i-1] == 1){
          
          # Abortion occurs in case the conception is unintended, and regardless of
          # whether the pregnancy would have ended in a miscarriage or not, 
          # with the probability of prob_abortion. Add one abortion. 
          if(RNG_abortion[num_conceptions[i]+1] <= prob_abortion &
             unintended[i] == (unintended[i-2]+1)){
            num_abortions[i:len_dat] = num_abortions[i]+1
            spacing[i:(i+abortion_month[num_conceptions[i]+1]+postpartum_m[num_conceptions[i]+1])] = 
              c((spacing[i-1]+1):((spacing[i-1]+1) + abortion_month[num_conceptions[i]+1] + postpartum_m[num_conceptions[i]+1]))
            i = i + abortion_month[num_conceptions[i]+1] + postpartum_m[num_conceptions[i]+1]
          }
          
          
          # Intrauterine mortality before reaching intended number of children (spacing)
          # Miscarriage occurs during first month, 1 months of non-
          # susceptibility after miscarriage. Add one miscarriage
          else if (RNG_miscarriage[num_conceptions[i]+1] <= iu_mort[i]
                   # Miscarriage during first month of pregnancy
                   & misc_month[miscarriage[i]+1] == 1){
            # Add zeroes for conception over the months of non-suceptibility after 
            # miscarriage
            conception[i:(i+postpartum_m[miscarriage[i]+1])] = c(0, rep(0, postpartum_m[miscarriage[i]+1]))
            # Add spacing for period of non-susceptibility after miscarriage occurs
            spacing[i:(i+postpartum_m[miscarriage[i]+1])] = c((spacing[i-1]+1):((spacing[i-1]+1) + postpartum_m[miscarriage[i]+1]))
            # Add one miscarriage to the miscarriage counter
            miscarriage[(i+misc_month2[miscarriage[i]+1]):len_dat] = miscarriage[i] + 1
            # Progress the number of months of non-susceptibility after miscarriage
            i = i + 1 + postpartum_m[miscarriage[i]+1]
          } 
          # Miscarriage occurs after first month of conception, 1 month of non-
          # susceptibility after miscarriage. Add one miscarriage
          else if(RNG_miscarriage[num_conceptions[i]+1] <= iu_mort[i]
                  # Miscarriage NOT during first month of pregnancy
                  & misc_month[miscarriage[i]+1] != 1){
            # Count months of conception up until miscarriage and then
            # zeroes for conception over the months of non-suceptibility after 
            # miscarriage
            conception[i:((i-1)+misc_month2[miscarriage[i]+1]+postpartum_m[miscarriage[i]+1])] = 
              c((2:misc_month[miscarriage[i]+1]), rep(0, postpartum_m[miscarriage[i]+1]))
            # Continue spacing over the period of conception and non-susceptibility
            spacing[i:(i+misc_month2[miscarriage[i]+1]+postpartum_m[miscarriage[i]+1])] =
              c((spacing[i-1]+1):((spacing[i-1]+1) + misc_month2[miscarriage[i]+1] + postpartum_m[miscarriage[i]+1]))
            # Add one miscarriage to the miscarriage counter
            miscarriage[(i+misc_month2[miscarriage[i]+1]):len_dat] = miscarriage[i] + 1
            # Progress the number of months of conception and non-susceptibility
            i = i + misc_month[miscarriage[i]+1] + postpartum_m[miscarriage[i]+1]
          }
          
          # Unintended live birth is successful, conception increases by 1  
          # up to month 9, the number of children is increased by 1, 
          # move 8 months forward. Add one live birth. 
          else if (RNG_miscarriage[num_conceptions[i]+1] > iu_mort[i]
                   # '[i - 2]' here because the spacing is counted between conceptions 
                   # and the conception occurred in the previous month. 
                   # 'isTRUE' used because 'i > 2' is not specified separately
                   # and the condition can therefore end up as a length zero 
                   # element 'spacing[2-2=0]' causing an error. This error would only
                   # occur if the woman cohabits in the first month and conceives
                   # during the first month of cohabitation, which almost never
                   # occurs. 
                   & isTRUE(spacing[i-2] < (intended_spacing[N[i]+1]))){
            # move to successful live birth
            conception[i:(i+7)] = c(2:9) 
            # add one birth
            N[(i+8):len_dat] = N[i-1] + 1 
            # add spacing
            spacing[i:(i+7)] = (spacing[i-1]+1):(spacing[i-1]+8)
            i = i + 8
          }
          
          # Planned live birth is successful, conception increases by 1 
          # up to month 9, the number of children is increased by 1, 
          # move 8 months forward. Add one live birth. 
          else if (RNG_miscarriage[num_conceptions[i]+1] > iu_mort[i]
                   & isTRUE(spacing[i-2] >= (intended_spacing[N[i]+1]))){
            conception[i:(i+7)] = c(2:9) # move to successful live birth
            N[(i+8):len_dat] = N[i-1] + 1 # add one birth
            spacing[i:(i+7)] = (spacing[i-1]+1):(spacing[i-1]+8)
            i = i + 8
          }  
          
          
          
        }  
        
        # Non-susceptibility period after birth, spacing (postpartum amenorrhea)
        else if(isTRUE(conception[i-1] == 9)
                & i < (len_dat-postpartum[N[i]+1])){
          conception[i:(i + postpartum[N[i]+1])] = 0
          spacing[i:(i + postpartum[N[i]+1])] = c((spacing[i]):postpartum[N[i]+1])
          i = i + (postpartum[N[i]+1]) # postpartum amenorrhea 
        }
        
        # If conception occurs too close to censoring, make sure iteration does not
        # exceed age limit. 
        else if(conception[i-1] == 9
                & i >= (len_dat-postpartum[N[i]+1])){
          conception[i:len_dat] = 0
          i = i + (len_dat)
        }
      }
      
      
      # IF INTENDED FERTILITY HAS BEEN REACHED (STOPPING)
      else if(N[i] >= intended_children[i]){
        
        # IF THERE WAS NO CONCEPTION DURING PREVIOUS MONTH, WOMAN IS NOT CONCEIVING
        if(conception[i-1] == 0){
          
          # Stopping successful; no conception, stopping starts or continues
          if(RNG_c1[i] > stopping_efficacy*fecundability[i]){
            conception[i] = 0
            stopping[i] = stopping[i-1] + 1
            i = i + 1
          }
          
          # Stopping is unsuccessful; conception occurs. Add one conception.
          else if (RNG_c1[i] <= stopping_efficacy*fecundability[i]){
            conception[i] = 1
            num_conceptions[i:len_dat] = num_conceptions[i-1]+1
            stopping[i] = stopping[i-1] + 1
            # Add one unintended pregnancy
            unintended[i:len_dat] = unintended[i-1] + 1
            i = i + 1
          }
          
        }
        
        # IF CONCEPTION OCCURRED DURING PREVIOUS MONTH
        else if(conception[i-1] == 1){
          
          
          # Abortion occurs in case the conception is unintended, and regardless of
          # whether the pregnancy would have ended in a miscarriage or not, 
          # with the probability of prob_abortion. Add one abortion. 
          if(RNG_abortion[num_conceptions[i]+1] <= prob_abortion){
            num_abortions[i:len_dat] = num_abortions[i]+1
            stopping[i:(i+abortion_month[num_conceptions[i]+1]+postpartum_m[num_conceptions[i]+1])] = 
              c((stopping[i-1]+1):((stopping[i-1]+1) + abortion_month[num_conceptions[i]+1] + postpartum_m[num_conceptions[i]+1]))
            i = i + abortion_month[num_conceptions[i]+1] + postpartum_m[num_conceptions[i]+1]
          }
          
          # Intrauterine mortality after reaching intended number of children (stopping).
          # Miscarriage occurs during first month of conception, 1 month of non-
          # susceptibility after miscarriage.
          else if (RNG_miscarriage[num_conceptions[i]+1] <= iu_mort[i]
                   & misc_month[miscarriage[i]+1] == 1){
            conception[i:(i+postpartum_m[miscarriage[i]+1])] = c(0, rep(0, postpartum_m[miscarriage[i]+1]))
            stopping[i:(i+postpartum_m[miscarriage[i]+1])] = c((stopping[i-1]+1):((stopping[i-1]+1) + postpartum_m[miscarriage[i]+1]))
            miscarriage[(i+misc_month2[miscarriage[i]+1]):len_dat] = miscarriage[i] + 1
            i = i + 1 + postpartum_m[miscarriage[i]+1]
          } 
          
          # Miscarriage occurs after first month of conception, 1 month of non-
          # susceptibility after miscarriage.   
          else if(RNG_miscarriage[num_conceptions[i]+1] <= iu_mort[i]
                  & misc_month[miscarriage[i]+1] != 1){
            conception[i:((i-1)+misc_month2[miscarriage[i]+1]+postpartum_m[miscarriage[i]+1])] =
              c((2:misc_month[miscarriage[i]+1]), rep(0, postpartum_m[miscarriage[i]+1]))
            stopping[i:(i+misc_month2[miscarriage[i]+1]+postpartum_m[miscarriage[i]+1])] = 
              c((stopping[i-1]+1):((stopping[i-1]+1) + misc_month2[miscarriage[i]+1] + postpartum_m[miscarriage[i]+1]))
            miscarriage[(i+misc_month2[miscarriage[i]+1]):len_dat] = miscarriage[i] + 1
            i = i + misc_month[miscarriage[i]+1] + postpartum_m[miscarriage[i]+1]
          }
          
          # unintended live birth is successful, conception increases by 1 each month
          # up to month 9, move 8 months forward. Add one live birth and one 
          # unintended live birth.
          else if (RNG_miscarriage[num_conceptions[i]+1] > iu_mort[i] &
                   RNG_abortion[num_conceptions[i]+1] > prob_abortion){# No abortion
            conception[i:(i+7)] = c(2:9) # move to successful live birth
            N[(i+8):len_dat] = N[i-1] + 1 # add one birth
            stopping[i:(i+7)] = (stopping[i-1]+1):(stopping[i-1]+8)
            i = i + 8
          }
          
        }
        
        # Non-susceptibility period after birth, stopping (postpartum amenorrhea)
        else if(conception[i-1] == 9
                & i < (len_dat-postpartum[N[i]+1])){
          conception[i:(i + postpartum[N[i]+1])] = 0
          stopping[i:(i + postpartum[N[i]+1])] = c((stopping[i-1]+1):(stopping[i-1]+1+postpartum[N[i]+1]))
          i = i + (postpartum[N[i]+1])   
        }
        
        # If conception occurs too close to censoring, make sure iteration does not
        # exceed limit. 
        else if(conception[i-1] == 9
                & i >= (len_dat-postpartum[N[i]+1])){
          conception[i:len_dat] = 0
          i = i + len_dat 
        }
        
      }  
    } 
  }
  # Replace input data with modified data 
  
  data$conception      <- conception 
  data$spacing         <- spacing
  data$stopping        <- stopping
  data$N               <- N
  data$unintended      <- unintended
  data$miscarriage     <- miscarriage
  data$num_conceptions <- num_conceptions
  data$num_abortions   <- num_abortions
  return(data)
}


#####