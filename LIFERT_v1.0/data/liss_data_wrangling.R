## LISS PANEL DATA WRANGLING

############################### IMPORTANT !!! ##################################


# Please note that you need to access and download the raw data yourself via the LISS
# panel website for this script to work. The original file names were preserved,
# so there should be no need to manually change any file names for the code to 
# work once you have downloaded the data files and placed them in the correct 
# folders relative to the R project. 

# The data files used in this script are the Stata files (.dta) for:

# LISS Family and Household waves 1-15
# LISS monthly background variables from March 2007 to June 2023

# For the reference mean age at marriage download CBS data on the mean age at marriage 
# https://opendata.cbs.nl/#/CBS/nl/dataset/37772ned/table?defaultview&dl=293DA
# Select the download option "CSV zonder symbolen". 


# RUN THIS SCRIPT AFTER YOU HAVE RUN THE GGS WRANGLING SCRIPT AND BEFORE YOU 
# RUN THE SIMULATION SCRIPT

################################################################################


#### LOADING PACKAGES AND SETTING UP SESSION ####


library(haven)

library(tidyverse)
library(data.table)
library(rlang)
library(plm)
library(lmtest)
library(ggtext)
library(zoo)

# Fitting distributions to data
library(fitdistrplus)

# Landau distribution
library(harmonicmeanp)

# Package for truncating distributions
library(LaplacesDemon)

# Package that extends on dplyr join and fuzzyjoin
library(powerjoin)

# Parallel processing 
library(parallel)


#####



#### Colour blind palette ####

# The palette with black:
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

#####



#### Loading the data ####

# Wave 1 for intended family size

liss_wave_1 <- read_dta(file = "./data/LISS/cf08a_2p_EN.dta") %>%
  filter(cf08a003 == 2) # women

# Extended cohort range required for some distributions due to small sample size

liss_wave_1a <-
  liss_wave_1%>% 
  mutate(year_birth = 2008-cf08a004) %>% # year - age
  filter(year_birth %in% 1964:1984)# select birth cohort


# Wave 15 for (almost) completed fertility, which is compared with intended family 
# size

liss_wave_15 <- read_dta(file = "./data/LISS/cf22o_EN_1.0p.dta") %>%
  filter(cf22o004 == 2)

liss_wave_15a <-
  liss_wave_15 %>%
  mutate(year_birth = 2022-cf22o004) %>%
  filter(year_birth %in% 1974:1984)

#####



#### Preparing data import ####

# List of numbers for variable names

data_numbers <- c("08", "09", "10", "11", "12", "13", "14", "15", "16", "17", "18",
                 "19", "20", "21", "22")

# List of letters for variable names
  
data_letters <- letters[1:15]

# Combine letters and numbers into a single vector

data_numlet <- paste(data_numbers, data_letters, sep = "")


# Vector of survey years

survey_year <- c(2008:2022)

# Adding gumbel generalised extreme value distribution for fitting
# (not included in fitdist function)
dgumbel <- function(x, a, b) 1/b*exp((a-x)/b)*exp(-exp((a-x)/b))
pgumbel <- function(q, a, b) exp(-exp((a-q)/b))
qgumbel <- function(p, a, b) a-b*log(-log(p))



#####



#### BACKGROUND VARIABLES ####


# Empty list for all months of liss between March 2011 and June 2023


bg_data_list_full <- list()

# File names from directory


# Original files names

bg_data_names_full <- list.files(path = "./data/LISS/bg_vars/all_months/", pattern = ".dta")

# Loading data to the list

for (i in 1:187){
  
  bg_data_list_full[[i]] <- 
    read_dta(file = paste0("./data/LISS/bg_vars/all_months/", bg_data_names_full[i]),
             col_select = c("nomem_encr", "geslacht", "gebjaar", 
                            "oplmet")) %>% 
    zap_label()
  
  
}



# Naming the list elements with the file names

names(bg_data_list_full) <- gsub(".*_","", gsub("\\_EN.*", "", bg_data_names_full))


# List for joined data

joined_bg_list_full <- list()

# Coalesce joining the data into the joined data list
# coalesce_yx, beacause we want to update older values with
# values from the later waves

joined_bg_list_full[[1]] <- 
  power_full_join(bg_data_list_full[1:2], by = c("nomem_encr"),
                  conflict = coalesce_yx)

for(i in 1:15){
  
  joined_bg_list_full[[(i+1)]] <- 
    power_full_join(bg_data_list_full[(12*i-9):(12*i+2)], by = c("nomem_encr"),
                    copy = TRUE,
                    conflict = coalesce_yx)
  
}

joined_bg_list_full[[17]] <- 
  power_full_join(bg_data_list_full[182:187], by = c("nomem_encr"),
                  conflict = coalesce_yx)


# Names as years for full data

wave_names_full <- paste0(2007:2023)

# Setting names to the data sets

names(joined_bg_list_full) <- wave_names_full

# Changing the haven tibbles to data.tables and restricting the data to 
# women born between 1964 and 1984

for(i in 1:17){
  
  joined_bg_list_full[[i]] <- 
    setDT(joined_bg_list_full[[i]])[ geslacht == 2,
    ][gebjaar %in% 1964:1984,]
  
  
}

# Extracting highest attained level of education by coalesce joining so that
# conflicts are resolved by replacing the value in the data by the value in the
# joined data


bg_education_full <- 
  power_full_join(joined_bg_list_full, by = "nomem_encr",
                  conflict = coalesce_yx)

bg_education_full <-
  bg_education_full[, c("nomem_encr", "gebjaar", "oplmet")
  ][, oplmet := fcase(oplmet %in% c(1,2,8,9), 1,
                      oplmet %in% 3:4, 2,
                      oplmet %in% 5:6, 3,
                      oplmet == 7, NA_real_)
  ][ oplmet != is.na(oplmet)] %>% 
  rename("year_birth" = "gebjaar",
         "edu_level" = "oplmet")

## Combining with GGS data

# GGS data on educational attainment

ggs_edu_attainment <- 
  readRDS("./data/ggs_edu_attainment.RDS") %>% 
  rename("nomem_encr" = "arid",
         "edu_level" = "aeduc",
         "year_birth" = "abyear")

# Joining the data

edu_attainment_joined <- 
  bg_education_full %>% 
  filter(year_birth %in% 1974:1984) %>% 
  full_join(ggs_edu_attainment) %>% 
  count(edu_level) %>% 
  mutate(prob = n/sum(n), 
         N = sum(n))

# edu_attainment_joined %>%
#   ggplot()+
#   theme_minimal()+
#   geom_col(aes(x = edu_level, y = prob))+
#   annotate("text", x = 1, y = 0.45, label = paste0("N = ", edu_attainment_joined$N),
#            colour = "gray20", size = 5)+
#   labs(title = "Level of educational attainment 1974-1984")+
#   xlab("Level of education")+
#   ylab("Share")+
#   theme(text = element_text(size=14,  family="serif"),
#         plot.title = element_markdown(hjust = 0.5, size = 16),
#         plot.caption = element_markdown(hjust = 0))


# Function for assigning educational attainment 

edu_level_f <- function(n = 1){
  
  edu_level = c(1,2,3)
  edu_share = edu_attainment_joined$prob
  
  sample(edu_level, size = n,  prob = edu_share, replace = TRUE)
}
  



#####



#### FERTILITY INTENTIONS ####  

# cf08a129 How many children do you think you will have in the future?

# Only include biological children

# cf08a098 - cf08a112
# For each child, please indicate below whether you are the biological parent,
# the step parent, the adoptive parent or foster parent. If you are living 
# together with your partner's children, select the option 'step parent'.

# 1 biological parent
# 2 step parent
# 3 adoptive parent
# 4 foster parent  

# Find the column index range of variables cf08a098 to cf08a112 

which( colnames(liss_wave_1a) %in% c("cf08a098","cf08a112") )

# Data used to generate fertility intentions

liss_wave_1b <- 
  liss_wave_1a %>% 
  # We want only biological children for calculating fertility intentions 
  filter(apply(liss_wave_1a[c(104:118)],1,function(x) all(x == 1, na.rm = TRUE))) %>% 
  # Calculating the number of biological children per mother
  mutate(num_children = rowSums(.[104:118], na.rm = TRUE)) %>% 
  filter(year_birth %in% 1974:1984) 

# Generating fertility intentions

liss_wave_1b <-
  liss_wave_1b %>% 
  mutate(fert_intention = if_else(cf08a128 == 1, num_children+cf08a129, num_children),
         fert_intention = if_else(fert_intention > 5, 5, fert_intention))


fert_intent_edu <- liss_wave_1b %>% 
  zap_label() %>% 
  dplyr::select(nomem_encr, fert_intention) %>% 
  left_join(bg_education_full) %>% 
  group_by(edu_level) %>% 
  count(fert_intention) %>% 
  na.omit()
  

# Counts for fertility intentions (used in paper, because negligible differences
# between educational groups as shown in plot below)

fert_intent <-  
  liss_wave_1b %>% 
  dplyr::select(nomem_encr,year_birth, num_children, fert_intention) %>% 
  drop_na(fert_intention) %>% 
  mutate(N = length(nomem_encr))%>% 
  count(fert_intention) %>% 
  rename("num" = "fert_intention")



# Importing GGS fertility intentions data

ggs_fert_int <- 
readRDS(file = "./data/ideal_fert_dist_ggs.RDS")  


ggs_fert_int_edu <- 
  readRDS(file = "./data/ggs_intended_fert_edu.RDS")



# Combining LISS and GGS data

ideal_fert_dist_comb <- 
  ggs_fert_int %>% 
  dplyr::select(num, n) %>% 
  full_join(fert_intent, by = c("num")) %>% 
  mutate(n = n.x+n.y) %>% 
  dplyr::select(num, n) %>% 
  mutate(prob = n/sum(n),
         N = sum(n))

ideal_fert_dist_comb_edu <- 
  ggs_fert_int_edu %>% 
  dplyr::select(edu_level, fert_intention, n) %>% 
  full_join(fert_intent_edu, by = c("edu_level", "fert_intention")) %>% 
  mutate(n = n.x+n.y) %>% 
  dplyr::select(edu_level, fert_intention, n) %>% 
  group_by(edu_level) %>% 
  mutate(prob = n/sum(n),
         N = sum(n)) %>% 
  ungroup()




# Plotting
# 
# ideal_fert_dist_comb %>%
#   ggplot()+
#   theme_minimal()+
#   geom_col(aes(x = num, y = prob), fill = "gray20") +
#   scale_y_continuous(limits = c(0,0.6))+
#   scale_x_continuous(breaks = 0:5)+
#   annotate("text", x = 4, y = 0.5, label = paste0("N = ", ideal_fert_dist_comb$N),
#            size = 5, colour = "gray20")+
#   labs(title = "Intended fertility of Dutch women born during 1974-1984")+
#   xlab("Number of children")+
#   ylab("Proportion of women")+
#   theme(text = element_text(size=14,  family="serif"),
#         plot.title = element_markdown(hjust = 0.5, size = 16),
#         plot.caption = element_markdown(hjust = 0))



# By education

# ideal_fert_dist_comb_edu %>%
# ggplot()+
#   geom_col(aes(x = fert_intention, y = prob, group = as.factor(edu_level), fill = as.factor(edu_level)),
#            position = "dodge")+
#   scale_fill_manual(values = cbbPalette[c(2,4,7)], name = "Educational level",
#                     labels = c("ISCED 0-2", "ISCED 3-4","ISCED 5-7"))+
#   theme(text=element_text(size=14),
#         plot.title = element_markdown(hjust = 0.5, size = 16),
#         plot.caption = element_markdown(hjust = 0))+
#   labs(title = "Fertility intentions, women born 1974-1984",
#        fill = "Educational level")+
#   xlab("Number of children")+
#   ylab("Probability")

#####



#### UNION FORMATION HISTORIES ####
 
## Data for first wave (does not have some of the variables included in later waves)
# List of file names with location relative to folder where R project is located
family_household_names <- list.files(path = "./data/LISS/", pattern = ".dta", full.names = TRUE)


# Adding the first wave separately to the list because the wave did not contain
# variables 403 and 404, but the other waves did

union_frame_list <- list( 
  read_dta(file = family_household_names[1], 
           col_select = c("nomem_encr",
                          paste0("cf", data_numlet[1], "003"),# Sex of respondent
                          paste0("cf", data_numlet[1], "004"),# Age of respondent
                          paste0("cf", data_numlet[1], "024"),# Partner?
                          paste0("cf", data_numlet[1], "025"),# Cohabiting?
                          paste0("cf", data_numlet[1], "028"),# When relationship? 
                          paste0("cf", data_numlet[1], "029"),# When cohabiting?
                          paste0("cf", data_numlet[1], "030"),# Currently married?
                          paste0("cf", data_numlet[1], "031"),# When married?
                          paste0("cf", data_numlet[1], "033"),# Married to another partner before? 1 = yes
                          paste0("cf", data_numlet[1], "034"))) %>% # Separate from previous partner? 1 = yes
    
    mutate(cf08a403 = NA_real_, 
           cf08a404 = NA_real_) %>%  # Variables not available for first wave, use empty columns to replace 
    dplyr::select(nomem_encr, sort(names(.)))
)


## Data for waves 2-15

for (i in 2:(length(data_numlet)-1)){
  
  # Reading in only the columns that are needed from the .dta files and 
  # adding the resulting data.frames to the same list as LISS wave 1
  
  union_frame_list[[i]] <- 
    read_dta(file = family_household_names[i],
             col_select = c("nomem_encr",
                            paste0("cf", data_numlet[i], "003"),
                            paste0("cf", data_numlet[i], "004"),
                            paste0("cf", data_numlet[i], "024"),
                            paste0("cf", data_numlet[i], "025"),
                            paste0("cf", data_numlet[i], "028"),
                            paste0("cf", data_numlet[i], "029"),
                            paste0("cf", data_numlet[i], "030"),
                            paste0("cf", data_numlet[i], "031"),
                            paste0("cf", data_numlet[i], "033"),
                            paste0("cf", data_numlet[i], "034"),
                            # Different partner from last year, separated? 1 = yes
                            paste0("cf", data_numlet[i], "403"), 
                            # Partner last year, not this year. Separated? 1 = yes
                            paste0("cf", data_numlet[i], "404"))) %>% 
    # Sorting all but the first column by column names, since I refer to indices 
    # when mutating variables, and for some reason waves 13 and 14 have a different 
    # order in the variables despite me using the same code to extract the data
    dplyr::select(nomem_encr, sort(names(.)))
}


# Wave 15 separately because they added 'other' to the gender variable and 
# changed it to cf22o536

union_frame_list[[15]] <- 
  read_dta(file = family_household_names[15],
           col_select = c("nomem_encr",
                          paste0("cf", data_numlet[15], "536"), # new gender variable
                          paste0("cf", data_numlet[15], "004"),
                          paste0("cf", data_numlet[15], "024"),
                          paste0("cf", data_numlet[15], "025"),
                          paste0("cf", data_numlet[15], "028"),
                          paste0("cf", data_numlet[15], "029"),
                          paste0("cf", data_numlet[15], "030"),
                          paste0("cf", data_numlet[15], "031"),
                          paste0("cf", data_numlet[15], "033"),
                          paste0("cf", data_numlet[15], "034"),
                          # Different partner from last year, separated? 1 = yes
                          paste0("cf", data_numlet[15], "403"), 
                          # Partner last year, not this year. Separated? 1 = yes
                          paste0("cf", data_numlet[15], "404"))) %>% 
  # Sorting all but the first column by column names, since I refer to indices 
  # when mutating variables, and for some reason waves 13 and 14 have a different 
  # order in the variables despite me using the same code to extract the data
  dplyr::select(nomem_encr, sort(names(.))) %>% 
  relocate(cf22o536, .before = cf22o004)

## Setting names for the data.frames (haven tibbles) in the list

# Empty vector for the names         
union_names <- c()

# For loop to generate name vector
for(i in 1:length(data_numlet)){
  union_names[i] <- paste0("union_data_liss",i)
}

# Assigning names to the data frames
names(union_frame_list) <-  union_names


## Applying filters across the data.frames

union_frame_list <- 
  # Restrict to only women
lapply(union_frame_list, function(x) filter(x, x[[2]] == 2)) 



## For loop generating variables across the data frames in the list

# Mutating variables for year of birth of respondent, 
# age at first marriage, age at first cohabitation, if they separated,
# if they ever married, if they ever cohabited, and filtered by year of
# birth to get the same birth cohorts that I use for the GGS data (1974-1984)

for (i in 1:length(data_numlet)) {
  union_frame_list[[i]] <- union_frame_list[[i]] %>% 
    relocate(ends_with(c("403", "404")), .after = last_col()) %>% 
    mutate(year_birth = survey_year[i]-.[[3]],
           # Set age at cohabitation/marriage to a 0 for cases where we do not
           # have information and then removed. Otherwise coalesce will fill 
           # in NA's incorrectly.
           age_first_mar = case_when(
             # Was married to another partner before
             .[[10]] == 1 ~ 0,
             # No information on if the woman was married before
             is.na(.[[10]]) ~ .[[9]]-year_birth,
             # Was not married to another partner before
             .[[10]] == 2 ~ .[[9]]-year_birth,
             .default = NA_real_),
           age_re_mar = case_when(
             # Not married previously
             .[[10]] == 2 ~ 0,
             # No information on if the woman was married before
             is.na(.[[10]]) ~ 0,
             # Was married to another partner before
             .[[10]] == 1 ~ .[[9]]-year_birth,
             .default = NA_real_),
           age_first_cohab = case_when(
             # Was married to another partner before
             .[[10]] == 1 ~ 0,
             # No information on if the woman was married before or separated/lost
             # previous partner
             is.na(.[[10]]) | is.na(.[[11]]) ~ .[[7]]-year_birth,
             # Was not married to another partner before
             .[[10]] == 2 ~ .[[7]]-year_birth,
             # Separated from previous partner
             !is.na(.[[11]]) ~ 0,
             .default = NA_real_),
           age_re_cohab = case_when(
             # Not married previously
             .[[10]] == 2 ~ 0,
             # No information on if the woman was married before or separated/lost
             # previous partner
             is.na(.[[10]])  ~ 0,
             # Was  married to another partner before
             .[[10]] == 1 ~ .[[7]]-year_birth,
             .default = NA_real_),
           age_first_rel = case_when(
             # Was married to another partner before
             .[[10]] == 1 ~ 0,
             # No information on if the woman was married before or separated/lost
             # previous partner
             is.na(.[[10]]) | is.na(.[[11]]) ~ .[[6]]-year_birth,
             # Was not married to another partner before
             .[[10]] == 2 ~ .[[6]]-year_birth,
             # Separated from previous partner
             !is.na(.[[11]]) ~ 0,
             .default = NA_real_),
           separated = if_else(.[[11]] == 1 | .[[12]] == 1 | .[[13]] == 1, 1,0),
           ever_partnered = if_else(.[[4]] == 1 | .[[10]]  == 1, 1, 0),
           ever_married = if_else(.[[8]] == 1 | .[[10]]  == 1, 1, 0),
           ever_cohabited = if_else(.[[5]] == 1 | .[[10]]  == 1, 1, 0),
           cohab_to_mar = .[[9]]-.[[7]]) %>% 
    filter(year_birth %in% 1964:1984)
}


# Empty vectors for variable name vectors 

partner_v <- c()
cohab_v <- c()
cohab_y_v <- c()
mar_v <-  c()
mar_y_v <- c()
sep_prev_v <- c()
dif_part_v <- c()
part_last_not_this_v <- c()
cohab_duration_v <- c()
sex_res <- c()
relationship_v <- c()

# Variable name vectors

for(i in 1:length(data_numlet)){
  
  sex_res[i] <- paste0("cf", data_numlet[i], "003") # Sex of respondent
  
  partner_v[i] <- paste0("cf", data_numlet[i], "024") # Partner?
  
  cohab_v[i] <- paste0("cf", data_numlet[i], "025") # Cohabiting?
  
  relationship_v[i] <- paste0("cf", data_numlet[i], "028") # When relationship?
  
  cohab_y_v[i] <- paste0("cf", data_numlet[i], "029") # When cohabiting?
  
  mar_v[i] <- paste0("cf", data_numlet[i], "030") # Married?
  
  mar_y_v[i] <- paste0("cf", data_numlet[i], "031") # When married?
  
  sep_prev_v[i] <- paste0("cf", data_numlet[i], "034") # Separated from previous partner?
  
  # Different partner from last year, separated? 1 = yes
  dif_part_v[i] <- paste0("cf", data_numlet[i], "403") 
  
  # Partner last year, not this year. Separated? 1 = yes
  part_last_not_this_v[i] <- paste0("cf", data_numlet[i], "404")
  
  cohab_duration_v[i] <- paste0("cohab_duration", i)
  
}



## All data is joined to the first wave of LISS

# Powerjoin full_join with coalesce. For variables with the same name
# generated in the code above, if NA in LISS wave 1, add value from
# first wave when value is not NA. Does not replace values that are not NA. 



union_data_liss_joined <- 
  power_full_join(union_frame_list, by = c("nomem_encr"),
                  conflict = coalesce_xy)  %>% 
  # 31 women report marriage before cohabitation, but we model 
  # cohabitation to marriage, so remove durations below 0. 
  filter(cohab_to_mar >= 0 | is.na(cohab_to_mar)) %>%  
  power_left_join(bg_education_full, by = c("nomem_encr"),
                 conflict = coalesce_xy) 



#####



#### TIME BETWEEN COHABITATION AND MARRIAGE ####

# GGS data

ggs_cohab_to_mar <- 
readRDS("./data/ggs_cohab_to_mar.RDS") %>% 
  rename("edu_level" = "aeduc",
         "nomem_encr" = "arid", 
         "year_birth" = "abyear",
         "cohab_to_mar" = "a302bTdiff") %>% 
  mutate_at(c(1,2,4), as.numeric) %>% 
  dplyr::select(nomem_encr, year_birth, edu_level, cohab_to_mar)


# LISS data

liss_cohab_to_mar <- 
  union_data_liss_joined %>% 
  dplyr::select(nomem_encr, year_birth, edu_level, cohab_to_mar)

# Combining the data

combined_cohab_to_mar <- 
  rbind(ggs_cohab_to_mar, liss_cohab_to_mar) %>% 
  filter(!is.na(cohab_to_mar)) %>% 
  mutate(N = length(nomem_encr))
  
# By educational attainment

# combined_cohab_to_mar %>%
#   filter(!is.na(cohab_to_mar), !is.na(edu_level)) %>%
#   ggplot()+
#   theme_minimal()+
#   geom_bar(aes(x = cohab_to_mar, fill = as.factor(edu_level)),
#            position = "dodge")+
#   annotate("text", x = 20, y = 200, label = paste0("N = ", combined_cohab_to_mar$N),
#            colour = "gray20", size = 5)+
#   labs(title = "Transition time from cohabitation to marriage <BR> 1964-1984 cohort")+
#   xlab("Years")+
#   ylab("Count")+
#   theme(text = element_text(size=16,  family="serif"),
#         plot.title = element_markdown(hjust = 0.5, size = 18),
#         plot.caption = element_markdown(hjust = 0),
#         legend.position="bottom")


# ISCED 0-2
cohab_mar1_dist <- 
  combined_cohab_to_mar %>% 
  filter(edu_level == 1) %>% 
  dplyr::select(cohab_to_mar) 
  
cohab_mar1_dist <- cohab_mar1_dist$cohab_to_mar*12 


# ISCED 3-4
cohab_mar2_dist <- 
  combined_cohab_to_mar %>% 
  filter(edu_level == 2) %>% 
  dplyr::select(cohab_to_mar) 

cohab_mar2_dist <- cohab_mar2_dist$cohab_to_mar*12


# ISCED 5-7
cohab_mar3_dist <- 
  combined_cohab_to_mar %>% 
  filter(edu_level == 3) %>% 
  dplyr::select(cohab_to_mar) 

cohab_mar3_dist <- cohab_mar3_dist$cohab_to_mar*12


## ISCED 0-2


# Graph distribution 
plotdist(cohab_mar1_dist, histo = TRUE, demp = TRUE)

# Fits

cohab_mar1_normal <- fitdist(cohab_mar1_dist, 'norm')
cohab_mar1_logistic <- fitdist(cohab_mar1_dist, 'logis')
cohab_mar1_gumbel <- fitdist(cohab_mar1_dist, 'gumbel', start = list(a = 10, b = 20))
cohab_mar1_landau <- fitdist(cohab_mar1_dist, 'Landau', start = list(mu = 10, sigma = 20))
cohab_mar1_exp <- fitdist(cohab_mar1_dist, 'exp')

# Plot legend

plotlegend <- c('Normal', 'Logistic', 'Gumbel', 'Landau', 'Exponential')

# Plot 

denscomp(list(cohab_mar1_normal, cohab_mar1_logistic, cohab_mar1_gumbel, cohab_mar1_landau, cohab_mar1_exp), legendtext = plotlegend,
         fitlwd = 2, plotstyle = "ggplot", breaks = 15,
         main = "Fitting functions to the duration until cohab_mar1nering data",
         xlab = "Age")

# AIC

c(cohab_mar1_normal$aic, cohab_mar1_logistic$aic, cohab_mar1_gumbel$aic, cohab_mar1_landau$aic, cohab_mar1_exp$aic)

# Exponential function fits best

cohab_mar1_prob_cdf_f <- function(month_range = 0:240){
  
  prob1 <- 
    pexp(month_range, rate = cohab_mar1_exp[[1]])
  
  prob <-
    # Set the sum of the probabilities to share_partnered, since not all women have a partner
    prob1/max(prob1)
  
  return(prob)
  
}

## ISCED 3-4


# Graph distribution 
plotdist(cohab_mar2_dist, histo = TRUE, demp = TRUE)

# Fits

cohab_mar2_normal <- fitdist(cohab_mar2_dist, 'norm')
cohab_mar2_logistic <- fitdist(cohab_mar2_dist, 'logis')
cohab_mar2_gumbel <- fitdist(cohab_mar2_dist, 'gumbel', start = list(a = 10, b = 20))
cohab_mar2_landau <- fitdist(cohab_mar2_dist, 'Landau', start = list(mu = 10, sigma = 20))
cohab_mar2_exp <- fitdist(cohab_mar2_dist, 'exp')

# Plot legend

plotlegend <- c('Normal', 'Logistic', 'Gumbel', 'Landau', 'Exponential')

# Plot 

denscomp(list(cohab_mar2_normal, cohab_mar2_logistic, cohab_mar2_gumbel, cohab_mar2_landau, cohab_mar2_exp), legendtext = plotlegend,
         fitlwd = 2, plotstyle = "ggplot", breaks = 15,
         main = "Fitting functions to the duration until cohab_mar2nering data",
         xlab = "Age")

# AIC

c(cohab_mar2_normal$aic, cohab_mar2_logistic$aic, cohab_mar2_gumbel$aic, cohab_mar2_landau$aic, cohab_mar2_exp$aic)

# Exponential function fits best

cohab_mar2_prob_cdf_f <- function(month_range = 0:240){
  
  prob1 <- 
    pexp(month_range, rate = cohab_mar2_exp[[1]])
  
  prob <-
    # Set the sum of the probabilities to share_partnered, since not all women have a partner
    prob1/max(prob1)
  
  return(prob)
  
}

## ISCED 5-7


# Find starting values for parameters 

# Graph distribution 
plotdist(cohab_mar3_dist, histo = TRUE, demp = TRUE)

# Fits

cohab_mar3_normal <- fitdist(cohab_mar3_dist, 'norm')
cohab_mar3_logistic <- fitdist(cohab_mar3_dist, 'logis')
cohab_mar3_gumbel <- fitdist(cohab_mar3_dist, 'gumbel', start = list(a = 10, b = 20))
cohab_mar3_landau <- fitdist(cohab_mar3_dist, 'Landau', start = list(mu = 10, sigma = 20))
cohab_mar3_exp <- fitdist(cohab_mar3_dist, 'exp')

# Plot legend

plotlegend <- c('Normal', 'Logistic', 'Gumbel', 'Landau', 'Exponential')

# Plot 

denscomp(list(cohab_mar3_normal, cohab_mar3_logistic, cohab_mar3_gumbel, cohab_mar3_landau, cohab_mar3_exp), legendtext = plotlegend,
         fitlwd = 2, plotstyle = "ggplot", breaks = 15,
         main = "Fitting functions to the duration until cohab_mar3nering data",
         xlab = "Age")

# AIC

c(cohab_mar3_normal$aic, cohab_mar3_logistic$aic, cohab_mar3_gumbel$aic, cohab_mar3_landau$aic, cohab_mar3_exp$aic)

# Exponential function fits best

cohab_mar3_prob_cdf_f <- function(month_range = 0:240){
  
  prob1 <- 
    pexp(month_range, rate = cohab_mar3_exp[[1]])
  
  prob <-
    # Set the sum of the probabilities to share_partnered, since not all women have a partner
    prob1/max(prob1)
  
  return(prob)
  
}

#####



#### PREPARING DATA FOR WHILE LOOPS ####

# Setting up data in long format for while loop

# Extracting the variables from the previously joined data

test_union_l2 <- 
  union_data_liss_joined %>% 
  dplyr::select(nomem_encr, partner_v)

test_union_l2a <- 
  union_data_liss_joined %>% 
  dplyr::select(nomem_encr, relationship_v)

test_union_l2b <- 
  union_data_liss_joined %>% 
  dplyr::select(nomem_encr, cohab_v)

test_union_l2c <- 
  union_data_liss_joined %>% 
  dplyr::select(nomem_encr, cohab_y_v)

test_union_l2d <- 
  union_data_liss_joined %>% 
  dplyr::select(nomem_encr, mar_v)

test_union_l2e <- 
  union_data_liss_joined %>% 
  dplyr::select(nomem_encr, mar_y_v)


# Changing into long format and renaming variables to have a uniform grouping
# by LISS wave

test_union_l3 <- 
  test_union_l2 %>% 
  pivot_longer(cf08a024:cf22o024, names_to = "wave", values_to = "partner") %>% 
  mutate(wave = gsub("024", "", wave),
         partner = as.integer(partner))

test_union_l3a <- 
  test_union_l2a %>% 
  pivot_longer(cf08a028:cf22o028, names_to = "wave", values_to = "start_relation") %>% 
  mutate(wave = gsub("028", "", wave),
         start_relation = as.integer(start_relation))

test_union_l3b <- 
  test_union_l2b %>% 
  pivot_longer(cf08a025:cf22o025, names_to = "wave", values_to = "cohabiting") %>% 
  mutate(wave = gsub("025", "", wave)) 

test_union_l3c <- 
  test_union_l2c %>% 
  pivot_longer(cf08a029:cf22o029, names_to = "wave", values_to = "start_cohab") %>% 
  mutate(wave = gsub("029", "", wave)) 

test_union_l3d <- 
  test_union_l2d %>% 
  pivot_longer(cf08a030:cf22o030, names_to = "wave", values_to = "married") %>% 
  mutate(wave = gsub("030", "", wave))

test_union_l3e <- 
  test_union_l2e %>% 
  pivot_longer(cf08a031:cf22o031, names_to = "wave", values_to = "start_mar") %>% 
  mutate(wave = gsub("031", "", wave))

# Adding the objects into a list of data.frames

test_union_list <- list(test_union_l3, test_union_l3a,
                        test_union_l3b, test_union_l3c, 
                        test_union_l3d, test_union_l3e)



# Removing excess objects

rm(test_union_l2, test_union_l2a,
   test_union_l2b, test_union_l2c, 
   test_union_l2d, test_union_l2e,
   test_union_l3, test_union_l3a,
   test_union_l3b, test_union_l3c, 
   test_union_l3d, test_union_l3e)




# Joining the data.frames in the list into one data.frame

test_union_joined <- 
power_left_join(test_union_list, by = c("nomem_encr", "wave")) %>% 
  mutate(nomem_encr = as.numeric(nomem_encr),
         wave = case_when(
    wave == "cf08a" ~ 1,
    wave == "cf09b" ~ 2,
    wave == "cf10c" ~ 3,
    wave == "cf11d" ~ 4,
    wave == "cf12e" ~ 5,
    wave == "cf13f" ~ 6,
    wave == "cf14g" ~ 7,
    wave == "cf15h" ~ 8,
    wave == "cf16i" ~ 9,
    wave == "cf17j" ~ 10,
    wave == "cf18k" ~ 11,
    wave == "cf19l" ~ 12,
    wave == "cf20m" ~ 13,
    wave == "cf21n" ~ 14,
    wave == "cf22o" ~ 15
  ))

#####



#### WHILE LOOPS FOR DATA EDITING ####


# While loop that fills in missing starting dates for relationship based on 
# partnership status

missing_relation_f <- 
  function(data){
    
    partner = data$partner
    cohabiting = data$cohabiting
    married = data$married
    start_relation = data$start_relation
    start_cohab = data$start_cohab
    start_mar = data$start_mar
    survey_year = data$survey_year
    
    i = 1
    while(i <= 15){
      # If no reported start of relationship, but no partner in previous wave, 
      # start of relationship same as the year of the wave
      if(isTRUE(partner[i] == 1 & 
                partner[i-1] == 2 &
                is.na(start_relation[i]) &
                cohabiting[i] != 1 &
                married[i] != 1)){
        start_relation[i] = survey_year[i]
        i = i + 1
      # Same for cohabitation
      }else if(isTRUE(cohabiting[i] == 1 & 
                cohabiting[i-1] == 2 &
                is.na(start_cohab[i])& 
               married[i] != 1)){
         start_cohab[i] = survey_year[i]
         i = i + 1
      # Same for marriage   
      }else if(isTRUE(married[i] == 1 & 
                 married[i-1] == 2 &
                 is.na(start_mar[i]))){
        start_mar[i] = survey_year[i]
        i = i + 1 
        # If respondent has partner in the previous wave and current wave, but no 
        # reported start of relationship, carry forward starting date from previous
      }else if(isTRUE(partner[(i-1):i] == 1  &
                      is.na(start_relation[i]) & 
               cohabiting[(i-1):i] != 1 &
               married[(i-1):i] != 1)){
        start_relation[i] = start_relation[i-1]
        i = i + 1
        # Same for cohabitation 
      }else if(isTRUE(cohabiting[(i-1):i] == 1 &
                       married[(i-1):i] != 1 &
                      is.na(start_cohab[i]) &
               !is.na(start_cohab[i-1]))){
        start_cohab[i] = start_cohab[i-1]
        i = i + 1
        # Same for marriage
      }else if(isTRUE(married[(i-1):i] == 1 &
                      is.na(start_mar[i])&
               !is.na(start_mar[i-1]))){
        start_mar[i] = start_mar[i-1]
        i = i + 1  
        # Else, don't change anything
      }else {
        i = i + 1
      }
    }
    data$partner = partner
    data$cohabiting = cohabiting
    data$married = married
    data$start_relation = start_relation
    data$start_cohab = start_cohab 
    data$start_mar = start_mar
    return(data)
  }




# While loop that sets cohabiting and married to 2 when partner == 2

missing_cohab_mar_f <- 
  function(data){
    
    partner = data$partner
    cohabiting = data$cohabiting
    married = data$married
    
    i = 1
    
    while(i <= 15){
      
      if(isTRUE(partner[i] == 2)){
        cohabiting[i] = 2
        married[i] = 2
        i = i + 1
      }else{
        i = i + 1
      }
    }
    data$partner = partner
    data$cohabiting = cohabiting
    data$married = married
    return(data)
  }



# While loop that counts the number of relationships, cohabitations, and marriages
# that end during the 15 years of LISS as well as their duration


rel_dur_f_test <- 
  function(data){
    
    partner = data$partner
    cohabiting = data$cohabiting
    married = data$married
    start_relation = data$start_relation
    start_cohab = data$start_cohab
    start_mar = data$start_mar
    rel_duration = rep(0, 15)
    cohab_duration = rep(0, 15)
    mar_duration = rep(0, 15)
    rel_count = rep(0, 15)
    cohab_count = rep(0,15)
    mar_count = rep(0, 15)
    length_rel = rep(NA_real_, 15)
    length_cohab = rep(NA_real_, 15)
    length_mar = rep(NA_real_, 15)
    n = nrow(data)
    survey_year = survey_year
    
    
    
    i = 1
    
    while(i <= n){
      
      # If wave 1
      if(i == 1){
        # If the respondent has a partner in the first wave, the duration of that 
        # relationship is the survey year of wave 1 (2008) minus the starting year 
        # of that relationship. Respondent not cohabiting or married.  
        if(isTRUE(partner[i] == 1 &
                  cohabiting[i] == 2 &
                  married[i] == 2)){
          rel_duration[i] = survey_year[i]-start_relation[i]
          i = i + 1
          # Same for cohabitation, not married  
        }else if (isTRUE(cohabiting[i] == 1 &
                         married[i] == 2)){
          cohab_duration[i] = survey_year[i]-start_cohab[i]
          i = i + 1
          # Same for marriage  
        }else if(isTRUE(married[i] == 1)){
          mar_duration[i] = survey_year[i]-start_mar[i]
          i = i + 1
          # If no reported partner (reported no or NA), move to next wave.   
        } else {
          i = i + 1
        }
        
      }
      # If wave 2-14
      # MAKE SIMILAR FOR COHABITATION AND MARRIAGE
      # COUNTS AT END OF RELATIONSHIP, BUT BEFORE STORING LENGTH
      
      else if(i >= 2) {
        # If respondent is in a relationship but not cohabiting or married, and 
        # was not in a relationship in the previous wave: duration of relationship
        # is the survey year minus the starting date of the relationship
        if(isTRUE(partner[i] == 1 &
                  (partner[i-1] == 2 | is.na(partner[i-1])) &
                  (cohabiting[i] == 2 | is.na(cohabiting[i])) &
                  (married[i] == 2 | is.na(married[i])))){
          rel_duration[i] = survey_year[i]-start_relation[i]
          i = i + 1  
          
          # Same for cohabitation  
        }else if(isTRUE(cohabiting[i] == 1 &
                        (cohabiting[i-1] == 2 | is.na(cohabiting[i-1])) &
                        (married[i] == 2 | is.na(married[i])))){
          cohab_duration[i] = survey_year[i]-start_cohab[i]
          i = i + 1  
          # Same for marriage  
        }else if(isTRUE(married[i] == 1 &
                        (married[i-1] == 2 | is.na(married[i-1])))){
          mar_duration[i] = survey_year[i]-start_mar[i]
          i = i + 1  
          # If respondent reports having no partner after having had relationship
          # in the previous wave, relationship count + 1 and duration of relationship
          # registered
        } else if(isTRUE(partner[i] == 2 &
                         partner[i-1] == 1 &
                         (cohabiting[i-1] == 2 | is.na(cohabiting[i-1])) &
                         (married[i-1] == 2 | is.na(married[i-1])))) {
          rel_count[i:n] = rel_count[i-1] + 1
          length_rel[rel_count[i]] = rel_duration[i-1]
          i = i+1
          # Same for cohabitation 
        } else if(isTRUE(cohabiting[i] == 2 &
                         cohabiting[i-1] == 1 &
                         (married[i-1] == 2 | is.na(married[i-1])))) {
          cohab_count[i:n] = cohab_count[i-1] + 1
          length_cohab[cohab_count[i]] = cohab_duration[i-1]
          i = i+1
          # Same for marriage
        } else if(isTRUE(married[i] == 2 &
                         married[i-1] == 1)) {
          mar_count[i:n] = mar_count[i-1] + 1
          length_mar[mar_count[i]] = mar_duration[i-1]
          i = i+1
          # If respondent had partner in previous wave and reports having partner,
          # duration +1
        }else if(isTRUE(partner[i] == 1 &
                        partner[i-1] == 1 &
                        (cohabiting[i-1] == 2 | is.na(cohabiting[i-1])) &
                        (married[i-1] == 2 | is.na(married[i-1])))){
          rel_duration[i] = rel_duration[i-1]+1
          i = i+1
          # Same for cohabitation
        }else if(isTRUE(cohabiting[i] == 1 &
                        cohabiting[i-1] == 1 &
                        (married[i-1] == 2 | is.na(married[i-1])))){
          cohab_duration[i] = cohab_duration[i-1]+1
          i = i+1
          # Same for marriage
        }else if(isTRUE(married[i] == 1 &
                        married[i-1] == 1)){
          mar_duration[i] = mar_duration[i-1]+1
          i = i+1 
          # All other cases, do nothing
        }else {
          i = i + 1
        }
      }
    }
    
    data$rel_duration = rel_duration
    data$cohab_duration = cohab_duration
    data$mar_duration = mar_duration
    data$rel_count = rel_count 
    data$cohab_count = cohab_count
    data$mar_count = mar_count
    data$length_rel = length_rel
    data$length_cohab = length_cohab
    data$length_mar = length_mar
    
    return(data)
  }





## While loop for extracting the time between union dissolution and re-partnering

recohab_time_f <- 
  function(data){
    
    
    cohabiting = data$cohabiting
    n = nrow(data)
    recohab_time = rep(0, 15)
    count_recohab = rep(0, 15)
    length_recohab = rep(NA_real_, 15)
    
    i = 1
    
    while((i+1) <= n){
      if(isTRUE(cohabiting[i] == 2 &
                cohabiting[i-1] == 1)){
        recohab_time[i] = 1
        count_recohab[i:n] = count_recohab[i]+1
        i = i+1
      }else if(isTRUE(cohabiting[i] == 2 &
                      cohabiting[i-1] == 2)){
        recohab_time[i] = recohab_time[i-1]+1
        i = i+1
      }else if(isTRUE(cohabiting[i] == 1 &
                      cohabiting[i-1] == 2)){
        length_recohab[count_recohab[i]] = recohab_time[i-1]
        recohab_time[i] = 0
        i = i+1
      }else{
        i = i+1
      }
      
    }
    
    data$recohab_time = recohab_time
    data$count_recohab = count_recohab
    data$length_recohab = length_recohab
    
    return(data)
  }

#####



#### JOINING THE DATA ####



# Joining the data on partner status and starting year of cohabitation

test_union_joined_a <- 
  setDT(test_union_joined) %>% 
  group_by(nomem_encr) %>% 
  # Remove observations with only NA's in partner status or start of relationship
  # or cohabitation
  filter(any(!is.na(c(partner, start_relation, start_cohab)))) %>%  
  # Arrange by ID and wave
  arrange(nomem_encr, wave) %>% 
  # Last Observation Carried Forward for NA's in partner and marital status
  mutate(partner = na.locf0(partner, fromLast = FALSE)) %>%  
  # Preparing data for while loops by placing the variables that are used in 
  
  # a list data.frame column by ID
  summarise(rel_vars = list(data.table(wave, partner, start_relation, cohabiting,
                                       start_cohab, married,  start_mar, survey_year))) %>% 
  ungroup() %>% 
  setDT()



# Loops run by individual woman

test_union_joined_a <- 
  setDT(test_union_joined_a)[, rel_vars := lapply(rel_vars, missing_cohab_mar_f)
  ][, unlist(rel_vars, recursive = FALSE), by = nomem_encr
  ][, cohabiting := na.locf0(cohabiting, fromLast = FALSE)
  ][, married := na.locf0(married, fromLast = FALSE)
  ][, .(rel_vars = list(data.table(wave, partner, start_relation, cohabiting,
                                   start_cohab, married,  start_mar))), by = nomem_encr]



# Names for empty data.table to populate in parallel processing
colname_vec_union <- c("nomem_encr", "rel_vars")

# Empty list to populate

test_union_joined_a2 <- list()

# Parallelisation function 

parallel_union_join_f <- function(cores = 1){
  
  
  n = nrow(test_union_joined_a)
  
  # Stop clustering on exit
  on.exit(stopCluster(cl))
  
  # Setting the number of cores to use
  
  cl <- makePSOCKcluster(cores, useXDR = FALSE, methods = FALSE)
  
  # loading packages
  clusterEvalQ(cl, lapply(c("data.table", "zoo"), library, character.only = TRUE))
  
  export_names <- c("survey_year", "test_union_joined_a2")
  
  
  clusterExport(cl, export_names)
  
  # Index vector for splitting the data into equal parts between the cores,
  # works for up to 8 cores. Ceiling used to get integers, as n will not split
  # evenly for all potential number of cores used.
  
  idx <- 
    if(cores == 1){
      ceiling(c(0, n))
    }else if(cores == 2){
      ceiling(c(0, n*(1/2), n))
    }else if (cores == 3){
      ceiling(c(0, n*(1/3), n*(2/3), n))
    }else if (cores == 4){
      ceiling(c(0, n*(1/4), n*(2/4), n*(3/4), n))
    }else if (cores == 5){
      ceiling(c(0, n*(1/5), n*(2/5), n*(3/5), n*(4/5), n))
    }else if (cores == 6){
      ceiling(c(0, n*(1/6), n*(2/6), n*(3/6), n*(4/6), n*(5/6), n))
    }else if (cores == 7){
      ceiling(c(0, n*(1/7), n*(2/7), n*(3/7), n*(4/7), n*(5/7), n*(6/7), n))
    }else if (cores == 8){
      ceiling(c(0, n*(1/8), n*(2/8), n*(3/8), n*(4/8), n*(5/8), n*(6/8), n*(7/8), n))
    }
  
  
  # Row numbers for splitting the data
  
  row_number <- as.numeric(rownames(test_union_joined_a))
  
  # Exporting the split data.table used in the union and conception functions
  # to each core
  
  for (i in seq_along(cl)) { 
    # This splits the data into equal parts based on the index vector,
    test_union_joined_a2 <- test_union_joined_a2[row_number <= idx[i+1] & row_number > idx[i]]
    # Exporting the split data.tables to each core respectively
    clusterExport(cl[i], c("test_union_joined_a2"), envir=environment())
  }
  
  # Removing excess object
  
  rm(test_union_joined_a2)
  
  test_union_joined_a <- 
    test_union_joined_a[, rel_vars := parLapply(cl, rel_vars, missing_relation_f)
    ][, rel_vars := parLapply(cl, rel_vars, rel_dur_f_test)
    ][, rel_vars := parLapply(cl, rel_vars, recohab_time_f)]
  
}

# Simply run without specifying the number of cores for serial processing

parallel_union_join_f()


# Adding year of birth for women with missing information on education
missing_yb <- 
  union_data_liss_joined %>% 
  dplyr::select(nomem_encr, year_birth)


# Producing data.table with the durations of relationship, cohabitation, marriage

sum_union_tab <- 
  test_union_joined_a %>% 
  unnest(rel_vars) %>% 
  dplyr::select(nomem_encr, wave, length_rel, length_cohab, length_mar, length_recohab)%>% 
  group_by(nomem_encr, wave) %>% 
  filter(any(!is.na(c(length_rel, length_cohab, length_mar)))) %>% 
  ungroup() %>% 
  mutate(N_rel = length(na.omit(length_rel)),
         N_cohab = length(na.omit(length_cohab)),
         N_mar = length(na.omit(length_mar)),
         N_recohab = length(na.omit(length_recohab))) %>% 
  power_left_join(bg_education_full, by = "nomem_encr",
                  conflict = coalesce_xy) %>%  
  power_left_join(missing_yb, by = "nomem_encr",
                  conflict = coalesce_xy) 



#####



#### PROBABILITY OF RE-PARTNERING ####

# Durations from GGS

ggs_union_durations <- 
  readRDS("./data/ggs_mar_coh_duration.RDS") %>% 
  rename("edu_level" = "aeduc",
         "nomem_encr" = "arid")  %>% 
  pivot_wider(names_from = "married", values_from = "lengthrel") %>% 
  rename("length_mar" = "1",
         "length_cohab" = "2")  %>% 
  dplyr::select(-c(part)) 

# LISS durations

liss_repart <- 
  sum_union_tab %>% 
  dplyr::select(nomem_encr, edu_level, length_recohab) %>% 
  filter(!is.na(length_recohab))


# Pick out the durations between union dissolution and the next cohabitation

combined_repart_duration <- 
  readRDS("./data/ggs_mar_coh_duration.RDS") %>% 
  rename("edu_level" = "aeduc",
         "nomem_encr" = "arid")  %>% 
  pivot_wider(names_from = "married", values_from = "lengthrel") %>% 
  rename("length_mar" = "1",
         "length_cohab" = "2")  %>% 
  dplyr::select(-c(part)) %>% 
  dplyr::select(nomem_encr, edu_level, lengthrepart) %>% 
  na.omit() %>% 
  filter(lengthrepart >= 0,
         lengthrepart <= 10) %>% 
  rename("length_recohab" = "lengthrepart") %>% 
  full_join(liss_repart) %>% 
  mutate(N = length(nomem_encr))

# Mean duration between cohabitations

mean_length_repart <- with(combined_repart_duration, mean(length_recohab))




# Plotting

# combined_repart_duration %>% 
#   ggplot() +
#   theme_minimal()+
#   geom_bar(aes(x = length_recohab))+
#   annotate("text", x = 10, y = 50, label = paste0("N = ", combined_repart_duration$N),
#            colour = "gray20", size = 5)+
#   labs(title = "Duration between cohabitations for Dutch cohorts 1964-1984")+
#   xlab("Years")+
#   ylab("Count")+
#   theme(text = element_text(size=16,  family="serif"),
#         plot.title = element_markdown(hjust = 0.5, size = 18),
#         plot.caption = element_markdown(hjust = 0))

# By education. Virtually identical, sample sizes are too small to split by education

# combined_repart_duration %>% 
#   ggplot() +
#   theme_minimal()+
#   geom_bar(aes(x = length_recohab, fill = as.factor(edu_level)), position = "dodge")+
#   annotate("text", x = 10, y = 50, label = paste0("N = ", combined_repart_duration$N),
#            colour = "gray20", size = 5)+
#   labs(title = "Duration between cohabitations for Dutch <BR> cohorts 1964-1984")+
#   xlab("Years")+
#   ylab("Count")+
#   theme(text = element_text(size=16,  family="serif"),
#         plot.title = element_markdown(hjust = 0.5, size = 18),
#         plot.caption = element_markdown(hjust = 0),
#         legend.position="bottom")


# Share of re-partnering per dissolutions 

repart_dist <- combined_repart_duration$length_recohab*12

# Fitting distribution 

# Graph distribution 
plotdist(repart_dist, histo = TRUE, demp = TRUE)

# Fits

repart_normal <- fitdist(repart_dist, 'norm')
repart_logistic <- fitdist(repart_dist, 'logis')
repart_gumbel <- fitdist(repart_dist, 'gumbel', start = list(a = 20, b = 20))
repart_landau <- fitdist(repart_dist, 'Landau', start = list(mu = 20, sigma = 15))



# Plot legend

plotlegend <- c('Normal', 'Logistic', 'Gumbel', 'Landau')

# Plot 

denscomp(list(repart_normal, repart_logistic, repart_gumbel, repart_landau), legendtext = plotlegend,
         fitlwd = 2, plotstyle = "ggplot", breaks = 10,
         main = "Fitting functions to the duration until repartnering data",
         xlab = "Age")

#AIC

c(repart_normal$aic, repart_logistic$aic, repart_gumbel$aic, repart_landau$aic)

# Gumbel fits best

repart_prob_cdf_f <- function(month_range = 0:240){
  
  prob1 <- 
    pgumbel(month_range, a = repart_gumbel$estimate[1],
            b = repart_gumbel$estimate[2])
  
  prob <-
    # Set the sum of the probabilities to share_partnered, since not all women have a partner
    prob1/max(prob1)
  
  return(prob)
  
}

# plot(repart_prob_cdf_f())


#####



#### PROBABILITY OF UNION DISSOLUTION ####

ggs_union_durations <- 
  ggs_union_durations %>% 
  dplyr::select(-lengthrepart)

# Prepare LISS data for combining

sum_union_tab2 <- 
  as.data.frame(sum_union_tab)%>% 
  dplyr::select(nomem_encr, edu_level, length_cohab, length_mar)
  
# Combined durations

union_length_combined <- 
  rbind(ggs_union_durations,sum_union_tab2) %>% 
  filter(any(!is.na(c(length_cohab, length_mar)))) %>% 
  mutate(N_cohab = length(na.omit(length_cohab)),
         N_mar = length(na.omit(length_mar))) %>% 
  pivot_longer(length_cohab:length_mar, 
               names_to = "type", values_to = "value") %>% 
  na.omit() %>% 
  filter(value >= 0)

 
# Mean duration of marriage

mean_length_mar <- with(union_length_combined, mean(value[type == "length_mar"]))

# Mean duration of cohabitation

mean_length_cohab <- with(union_length_combined, mean(value[type == "length_cohab"]))


## Plotting


# Cohabitation

# union_length_combined %>% 
#   filter(type == "length_cohab") %>% 
#   ggplot()+
#   theme_minimal()+
#   geom_bar(aes(x = value))+
#   annotate("text", x = 13, y = 100, label = paste0("N = ", union_length_combined$N_cohab),
#            colour = "gray20", size = 5)+
#   labs(title = "Duration of cohabitation until separation for Dutch <BR> cohort 1964-1984")+
#   xlab("Years")+
#   ylab("Count")+
#   theme(text = element_text(size=16,  family="serif"),
#         plot.title = element_markdown(hjust = 0.5, size = 18),
#         plot.caption = element_markdown(hjust = 0))


# Marriage
 
# union_length_combined %>% 
#   filter(type == "length_mar") %>% 
#   ggplot()+
#   theme_minimal()+
#   geom_bar(aes(x = value))+
#   annotate("text", x = 17, y = 30, label = paste0("N = ", union_length_combined$N_mar),
#            colour = "gray20", size = 5)+
#   labs(title = "Duration of marriage until divorce for Dutch cohort 1964-1984")+
#   xlab("Years")+
#   ylab("Count")+
#   theme(text = element_text(size=16,  family="serif"),
#         plot.title = element_markdown(hjust = 0.5, size = 18),
#         plot.caption = element_markdown(hjust = 0))

# Marriage and cohabitation

# union_length_combined %>% 
#   ggplot(aes(fill = type))+
#   geom_bar(aes(x = value), position = "dodge")


# Cohabitation by education, almost identical distributions

# union_length_combined %>% 
#   filter(type == "length_cohab") %>% 
#   ggplot()+
#   theme_minimal()+
#   geom_bar(aes(x = value, fill = as.factor(edu_level)), position = "dodge")+
#   annotate("text", x = 13, y = 100, label = paste0("N = ", union_length_combined$N_cohab),
#            colour = "gray20", size = 5)+
#   labs(title = "Duration of cohabitation until separation <BR> for Dutch cohort 1964-1984")+
#   xlab("Years")+
#   ylab("Count")+
#   theme(text = element_text(size=16,  family="serif"),
#         plot.title = element_markdown(hjust = 0.5, size = 18),
#         plot.caption = element_markdown(hjust = 0),
#         legend.position = "bottom")



# Marriage by education, seemingly very little difference and sample size too 
# small to split
# union_length_combined %>% 
#   filter(type == "length_mar") %>% 
#   ggplot()+
#   theme_minimal()+
#   geom_bar(aes(x = value, fill = as.factor(edu_level)), position = "dodge")+
#   annotate("text", x = 17, y = 30, label = paste0("N = ", union_length_combined$N_mar),
#            colour = "gray20", size = 5)+
#   labs(title = "Duration of marriage until divorce for Dutch <BR> cohort 1964-1984")+
#   xlab("Years")+
#   ylab("Count")+
#   theme(text = element_text(size=16,  family="serif"),
#         plot.title = element_markdown(hjust = 0.5, size = 18),
#         plot.caption = element_markdown(hjust = 0),
#         legend.position="bottom")
# 



# Storing distributions for function fitting (adding constant to be able to fit
# some of the functions)

div_dist <- 
  union_length_combined$value[union_length_combined$type=="length_mar"]*12

sep_dist <- 
  union_length_combined$value[union_length_combined$type=="length_cohab" & union_length_combined$value < 20]*12


# DIVORCE

# Graph distribution 
plotdist(div_dist, histo = TRUE, demp = TRUE)

# fit to normal, gamma, lognormal, and weibull

div_normal <- fitdist(div_dist, 'norm')
div_logistic <- fitdist(div_dist, 'logis')
div_gumbel <- fitdist(div_dist, 'gumbel', start = list(a = 60, b = 50))

# Plot legend
plotlegend <- c('Normal', 'Logistic', 'Gumbel')

 
  denscomp(list(div_normal, div_logistic, div_gumbel), legendtext = plotlegend,
           fitlwd = 2, plotstyle = "ggplot", breaks = 20,
           main = "Fitting functions to the age at first divriage data",
           xlab = "Age")

# AIC

c(div_normal$aic, div_logistic$aic, div_gumbel$aic)    
  
# Gumbel fits best
  

# Probability functions

div_prob_cdf_f <- function(month_range = 0:240){
  
  prob1 <- 
    plogis(month_range, location = div_gumbel$estimate[1],
           scale = div_gumbel$estimate[2])
  prob <-
    # Set the sum of the probabilities to share_partnered, since not all women have a partner
    prob1/max(prob1)
  
  
  return(prob)
  
}


#SEPARATION

# Graph distribution 
plotdist(sep_dist, histo = TRUE, demp = TRUE)


# Distribution looks like exponential decay

sep_exp <- fitdist(sep_dist, 'exp')
sep_gumbel <- fitdist(sep_dist, 'gumbel', start = list(a = 20, b = 20))
sep_landau <- fitdist(sep_dist, 'Landau', start = list(mu = 27, sigma = 18))


# Plot legend
plotlegend <- c('Exponential', 'Gumbel', 'Landau')



denscomp(list(sep_exp, sep_gumbel, sep_landau), legendtext = plotlegend,
         fitlwd = 2, plotstyle = "ggplot", breaks =9,
         main = "Fitting functions to duration of cohabitation data",
         xlab = "Years")

# AIC

c(sep_exp$aic, sep_gumbel$aic, sep_landau$aic)

# Exponential fits best

sep_prob_cdf_f <- function(month_range = 0:240){
  
  prob1 <- 
    pexp(month_range, rate = sep_exp[[1]])
  
  prob <-
    # Set the sum of the probabilities to share_partnered, since not all women have a partner
    prob1/max(prob1)
  
  return(prob)
  
}

#####



#### AGE AT FIRST COHABITATION AND MARRIAGE ####

# Count and probabilities for marriage

# Combined data

count_age_mar <- 
union_data_liss_joined %>% 
  dplyr::select(nomem_encr, year_birth, age_first_mar, edu_level) %>% 
  filter(age_first_mar >=15) %>% 
  full_join(readRDS("./data/ggs_age_mar.RDS")) %>%
  filter(year_birth %in% 1974:1984) %>% 
  na.omit() %>% 
  mutate(age_first_mar_m = age_first_mar*12,
         N = length(age_first_mar)) 
  

# Mean age at first marriage  
mean_age_first_mar <- mean(count_age_mar$age_first_mar)
  
  
# Plotting age at first marriage by education

# count_age_mar %>% 
# ggplot()+
#   theme_minimal()+
#   geom_bar(aes(x = age_first_mar, fill = as.factor(edu_level)), position = "dodge")+
#   annotate("text", x = 40, y = 60, label = paste0("N=", count_age_mar$N), size = 5)+
#   ggtitle("Age at first marriage distribution for Dutch <BR> cohorts 1974-1984")+
#   theme(text = element_text(size=16,  family="serif"),
#         plot.title = element_markdown(hjust = 0.5, size = 18),
#         plot.caption = element_markdown(hjust = 0),
#         legend.position = "bottom")+
#   xlab("Age")+
#   ylab("Count")
  





# Count and probabilities for first cohabitation

count_age_cohab <- 
  union_data_liss_joined %>% 
  dplyr::select(nomem_encr, year_birth, edu_level, age_first_cohab) %>% 
  full_join(readRDS("./data/ggs_age_first_cohab.RDS")) %>% 
  filter(age_first_cohab >=15,
         year_birth %in% 1974:1984) %>% 
  # Age in months for simulation
  mutate(age_first_cohab_m = age_first_cohab*12,
         N = length(na.omit(age_first_cohab))) %>% 
  na.omit()

# Mean age at first cohabitation

mean_age_first_cohab <- mean(count_age_cohab$age_first_cohab)

# Plotting
  
# count_age_cohab %>% 
#   ggplot()+
#   theme_minimal()+
#   geom_bar(aes(x = age_first_cohab))+
#   annotate("text", x = 35, y = 100, label = paste0("N=", count_age_cohab$N),
#            size = 5)+
#   ggtitle("Age at first cohabitation distribution for Dutch <BR> cohorts 1974-1984")+
#   theme(text = element_text(size=16,  family="serif"),
#         plot.title = element_markdown(hjust = 0.5, size = 18),
#         plot.caption = element_markdown(hjust = 0))+
#   xlab("Age")+
#   ylab("Count")



# Plotting by education
# 
# count_age_cohab %>%
#   mutate(edu_level_f = factor(edu_level, levels = c(1,2,3),
#                               labels = c("ISCED 0-2", "ISCED 3-4", "ISCED 5-8"),
#                               ordered = TRUE)) %>%
#   ggplot()+
#   theme_classic()+
#   geom_bar(aes(x = age_first_cohab, fill = edu_level_f))+
#   scale_fill_manual(values = c(cbbPalette[7], cbbPalette[6], cbbPalette[4]))+
#   theme(text = element_text(size=16,  family="serif"),
#         plot.title = element_markdown(hjust = 0.5, size = 16),
#         plot.caption = element_markdown(hjust = 0))+
#   xlab("Age")+
#   ylab("Count")+
#   facet_wrap(~edu_level_f, dir = "v")+
#   guides(fill="none")+
#   scale_x_continuous(limits = c(15,45), breaks = seq(0,45,5))



# Vector inputs for fitting functions to the data


# Marriage

mar_dist <- with(count_age_mar, age_first_mar_m)

# Cohabitation by education

cohab_dist1 <- with(count_age_cohab, age_first_cohab_m[edu_level == 1])

cohab_dist2 <- with(count_age_cohab, age_first_cohab_m[edu_level == 2])

cohab_dist3 <- with(count_age_cohab, age_first_cohab_m[edu_level == 3])




## Finding a distribution that fits the data best

# MARRIAGE

# graph distribution (right-skewed)
plotdist(mar_dist, histo = TRUE, demp = TRUE)

# fit to normal, gamma, lognormal, and weibull

mar_normal <- fitdist(mar_dist, 'norm')
mar_gamma <- fitdist(mar_dist, 'gamma', lower = c(0, 0))
mar_lognormal <- fitdist(mar_dist, 'lnorm')
mar_weibull <- fitdist(mar_dist, 'weibull', lower = c(0, 0))
mar_logistic <- fitdist(mar_dist, 'logis')

# plot the fits of 3 options
plotlegend <- c('Normal', 'Gamma', 'Lognormal', 'Weibull', 'Logistic')

mar_dens <- 
denscomp(list(mar_normal, mar_gamma, mar_lognormal, mar_weibull, mar_logistic), legendtext = plotlegend,
         fitlwd = 2, plotstyle = "ggplot", breaks = 20,
         main = "Fitting functions to the age at first marriage data",
         xlab = "Age")

mar_dens+ggplot2::scale_x_continuous(breaks = seq(15*12,45*12, by = 12),
                                     labels = seq(15,45, by = 1))


# AIC, Lognormal fits best 

c(mar_normal$aic, mar_gamma$aic, mar_lognormal$aic, mar_weibull$aic, 
  mar_logistic$aic)



# Probability distribution of first marriage

mar_prob_cdf_f <- function(age_range = 180:540){
  
  prob1 <- 
    plnorm(age_range, meanlog = mar_lognormal$estimate[1],
           sdlog = mar_lognormal$estimate[2])
  prob <-
    # Set the sum of the probabilities to share_partnered, since not all women have a partner
    prob1/max(prob1)
  
  
  return(prob)
  
}





# COHABITATION BY EDUCATIONAL ATTAINMENT

# ISCED 0-2

# graph distribution (right-skewed)
plotdist(cohab_dist1, histo = TRUE, demp = TRUE)

# fit to normal, gamma, lognormal, and weibull

cohab_normal1 <- fitdist(cohab_dist1, 'norm')
cohab_gamma1 <- fitdist(cohab_dist1, 'gamma', lower = c(0, 0))
cohab_lognormal1 <- fitdist(cohab_dist1, 'lnorm')
cohab_weibull1 <- fitdist(cohab_dist1, 'weibull', lower = c(0, 0))
cohab_logistic1 <- fitdist(cohab_dist1, 'logis')
cohab_gumbel1 <- fitdist(cohab_dist1, 'gumbel', start=list(a=10, b=10))



# Plot legend
plotlegend <- c('Normal', 'Gamma', 'Lognormal', 'Weibull', 'Logistic', 'Gumbel')

cohab_dens <- 
  denscomp(list(cohab_normal1, cohab_gamma1, cohab_lognormal1, 
                cohab_weibull1, cohab_logistic1, cohab_gumbel1), legendtext = plotlegend,
           fitlwd = 2, plotstyle = "ggplot", breaks = 10,
           main = "Fitting functions to the age at first cohabitation data",
           xlab = "Age")

cohab_dens+ggplot2::scale_x_continuous(breaks = seq(15*12,45*12, by = 12),
                                       labels = seq(15,45, by = 1))



# AIC, Gumbel fits the data best 

c(cohab_normal1$aic, cohab_gamma1$aic, cohab_lognormal1$aic, cohab_weibull1$aic, 
  cohab_logistic1$aic, cohab_gumbel1$aic)





# Probability function (CDF)

cohab_prob1_cdf_f <- function(age_range = 180:540){
  
  prob1 <- 
    pgumbel(age_range, a = cohab_gumbel1$estimate[1],
            b = cohab_gumbel1$estimate[2])
  
  prob <-
    # Set the sum of the probabilities to share_married, since not all women marry
    prob1/max(prob1)
  
  return(prob)
  
}








# ISCED 3-4

# graph distribution (right-skewed)
plotdist(cohab_dist2, histo = TRUE, demp = TRUE)

# fit to normal, gamma, lognormal, and weibull

cohab_normal2 <- fitdist(cohab_dist2, 'norm')
cohab_gamma2 <- fitdist(cohab_dist2, 'gamma', lower = c(0, 0))
cohab_lognormal2 <- fitdist(cohab_dist2, 'lnorm')
cohab_weibull2 <- fitdist(cohab_dist2, 'weibull', lower = c(0, 0))
cohab_logistic2 <- fitdist(cohab_dist2, 'logis')
cohab_gumbel2 <- fitdist(cohab_dist2, 'gumbel', start=list(a=10, b=10))



# Plot legend
plotlegend <- c('Normal', 'Gamma', 'Lognormal', 'Weibull', 'Logistic', 'Gumbel')

cohab_dens <- 
  denscomp(list(cohab_normal2, cohab_gamma2, cohab_lognormal2, 
                cohab_weibull2, cohab_logistic2, cohab_gumbel2), legendtext = plotlegend,
           fitlwd = 2, plotstyle = "ggplot", breaks = 15,
           main = "Fitting functions to the age at first cohabitation data",
           xlab = "Age")

cohab_dens+ggplot2::scale_x_continuous(breaks = seq(15*12,45*12, by = 12),
                                       labels = seq(15,45, by = 1))

# AIC, gumbel fits best

c(cohab_normal2$aic, cohab_gamma2$aic, cohab_lognormal2$aic, cohab_weibull2$aic, 
  cohab_logistic2$aic, cohab_gumbel2$aic)




# Probability function (CDF)

cohab_prob2_cdf_f <- function(age_range = 180:540){
  
  prob1 <- 
    pgumbel(age_range, a = cohab_gumbel2$estimate[1],
            b = cohab_gumbel2$estimate[2])
  
  prob <-
    # Set the sum of the probabilities to share_married, since not all women marry
    prob1/max(prob1)
  
  return(prob)
  
}

# ISCED 5-7



# graph distribution (right-skewed)
plotdist(cohab_dist3, histo = TRUE, demp = TRUE)

# fit to normal, gamma, lognormal, and weibull

cohab_normal3 <- fitdist(cohab_dist3, 'norm')
cohab_gamma3 <- fitdist(cohab_dist3, 'gamma', lower = c(0, 0))
cohab_lognormal3 <- fitdist(cohab_dist3, 'lnorm')
cohab_weibull3 <- fitdist(cohab_dist3, 'weibull', lower = c(0, 0))
cohab_logistic3 <- fitdist(cohab_dist3, 'logis')
cohab_gumbel3 <- fitdist(cohab_dist3, 'gumbel', start=list(a=10, b=10))



# Plot legend
plotlegend <- c('Normal', 'Gamma', 'Lognormal', 'Weibull', 'Logistic', 'Gumbel')

cohab_dens <- 
  denscomp(list(cohab_normal3, cohab_gamma3, cohab_lognormal3, 
                cohab_weibull3, cohab_logistic3, cohab_gumbel3), legendtext = plotlegend,
           fitlwd = 2, plotstyle = "ggplot", breaks = 15,
           main = "Fitting functions to the age at first cohabitation data",
           xlab = "Age")

cohab_dens+ggplot2::scale_x_continuous(breaks = seq(15*12,45*12, by = 12),
                                       labels = seq(15,45, by = 1))


# AIC, Gumbel fits best

c(cohab_normal3$aic, cohab_gamma3$aic, cohab_lognormal3$aic, cohab_weibull3$aic, 
  cohab_logistic3$aic, cohab_gumbel3$aic)




# Probability function (CDF)

cohab_prob3_cdf_f <- function(age_range = 180:540){
  
  prob1 <- 
    pgumbel(age_range, a = cohab_gumbel3$estimate[1],
            b = cohab_gumbel3$estimate[2])
  
  prob <-
    # Set the sum of the probabilities to share_married, since not all women marry
    prob1/max(prob1)
  
  return(prob)
  
}


#####

 

#### SHARES FOR COHABITATION, MARRIAGE, SEPARATION, AND DIVORCE ####

# Making the share who divorce a variable and solving equation system:


# fc = first cohabitation
# m1, m2 = marriage
# c1, c2 = cohabitation
# s = separation
# d = divorce


#
#             m2
#       m1 -> d
# fc ->       
#       c1 -> c2
#             s
#


# (1-c1)*(1-d)=0.420
# 
# c1*(1-s)=0.1
# 
# c1*s=2*(1-c1)*d
# 
# c1*s+(1-c1)*d=0.480

# where: 

# m1 = 1-c1
# m2 = 1-d
# c2 = 1-s

share_cohab <- 0.95

share_mar <- (1-21/50)

share_sep <- 16/21 

share_div <- 8/29



# For 5 %-point changes in total shares (future predictions)

share_cohab_5pct <- share_cohab - 0.05
share_mar_5pct <- share_mar - 0.05
share_sep_5pct <- share_sep + 0.05
share_div_5pct <- share_div + 0.05

#####



#### EQUATION SYSTEMS TO CALCULATE UNION SHARES BY EDUCATION ####

# Union shares

ggs_cohab_edu <- readRDS("./data/share_cohab_edu.RDS")

ggs_divorce_edu <- readRDS("./data/share_divorce_edu.RDS")

ggs_mar_edu <- readRDS("./data/share_mar_edu.RDS")

ggs_separate_edu <- readRDS("./data/share_separation_edu.RDS")

ggs_repart_edu <- readRDS("./data/share_repartner_edu.RDS")

share_married <- share_mar
share_separated <- share_sep
share_divorced <- share_div
share_cohabited <- 0.95
share_cohab_mar <- 0.58


ggs_cohab_edu_5pct <- ggs_cohab_edu - 0.05
ggs_mar_edu_5pct <- ggs_mar_edu - 0.05
ggs_repart_edu_5pct <- ggs_repart_edu - 0.05
ggs_separate_edu_5pct <- ggs_separate_edu + 0.05
ggs_divorce_edu_5pct <- ggs_divorce_edu + 0.05


share_repartner_new <- 0.75 # New share based on Finnish 1969-1971 cohort (76.2%, men and women)
share_repartner_new_5pct <- 0.70



# Empty list to populate
share_output_list <- list()

# Function for producing education specific union shares that sum up to the 
# specified cohort averages of these shares

union_share_f <- 
  function(runs = 10){
    
    # Empty list which will contain the shares set to sum to 
    # the cohort average share
    
    share_output_list <-  list(share_separated = rep(0, 3),
                              share_divorced = rep(0, 3),
                              share_married = rep(0, 3),
                              share_cohabited = rep(0, 3),
                              share_repartnered = rep(0,3),
                              share_separated_5pct = rep(0, 3),
                              share_divorced_5pct = rep(0, 3),
                              share_married_5pct = rep(0, 3),
                              share_cohabited_5pct = rep(0, 3),
                              share_repartnered_5pct = rep(0, 3))  
    
    # Education specific shares from GGS data
    
    rel_share_edu_list <-  list(ggs_separate_edu,
                               ggs_divorce_edu,
                               ggs_mar_edu,
                               ggs_cohab_edu,
                               ggs_repart_edu,
                               ggs_separate_edu_5pct,
                               ggs_divorce_edu_5pct,
                               ggs_mar_edu_5pct,
                               ggs_cohab_edu_5pct,
                               ggs_repart_edu_5pct
                               
    )
    
    # Cohort averages for the shares
    
    rel_share_list <- list(share_sep, share_div, share_mar, share_cohab,
                           share_repartner_new,
                           share_sep_5pct, share_div_5pct, share_mar_5pct,
                           share_cohab_5pct, share_repartner_new_5pct) 
    
    # Share of women in each educational category
    
    edu_share <- edu_attainment_joined$prob
    
    for(i in 1:runs){
      
      # Share of women who experience union event by education
      
      p1 <- rel_share_edu_list[[i]][1]
      
      p2 <- rel_share_edu_list[[i]][2]
      
      p3 <- rel_share_edu_list[[i]][3]
      
      
      
      # edu_share1*x + edu_share2*y + edu_share3*z = share_union
      
      
      # x to y
      
      x_y <- p2/p1
      
      # x to z
      
      x_z <- p3/p1
      
      # y to z
      
      y_z <- p3/p2
      
      
      # Equations and solving roots
      
      
      f <- function(x) {edu_share[1]*x+edu_share[2]*x_y*x+
          edu_share[3]*x_z*x-rel_share_list[[i]]
      }
      
      x_1 <- uniroot(f, interval = c(0,1))
      
      x_1 <- x_1$root
      
      f2 <- function(x){edu_share[1]*x_1+edu_share[2]*x+
          edu_share[3]*y_z*x-rel_share_list[[i]]
        
      }
      
      x_2 <- uniroot(f2, interval = c(0,1))
      
      x_2 <- x_2$root
      
      
      f3 <- function(x){edu_share[1]*x_1+edu_share[2]*x_2+
          edu_share[3]*x-rel_share_list[[i]]
        
      }
      
      x_3 <- uniroot(f3, interval = c(0,1))
      
      x_3 <- x_3$root
      
      
      # Checking that the roots are correct
      
      # edu_share[1]*x_1+edu_share[2]*x_2+
      #   edu_share[3]*x_3-rel_share_list[[i]]
      
      
      share_output_list[[i]][1] <- x_1
      
      share_output_list[[i]][2] <- x_2
      
      share_output_list[[i]][3] <- x_3
      
      
    }
    # output list
    
   share_output_list<<-share_output_list
  }

# Using the above function to produce the list of education specific shares
# of union events

union_share_f()

#####











#### INPUT LIST FOR SIMULATION MODEL ####


cohabitation1_cdf <- cohab_prob1_cdf_f(180:480)
cohabitation2_cdf <- cohab_prob2_cdf_f(180:480)
cohabitation3_cdf <- cohab_prob3_cdf_f(180:480)

marriage_cdf <- mar_prob_cdf_f(180:660)

cohab_mar_cdf1 <- cohab_mar1_prob_cdf_f(0:240)
cohab_mar_cdf2 <- cohab_mar2_prob_cdf_f(0:240)
cohab_mar_cdf3 <- cohab_mar3_prob_cdf_f(0:240)

divorce_cdf <- div_prob_cdf_f(month_range = 0:420)
separation_cdf <- sep_prob_cdf_f(month_range = 0:240)
repartner_cdf <- repart_prob_cdf_f(0:120)

fert_int_both <- ideal_fert_dist_comb$prob
edu_share <- edu_attainment_joined$prob


# List of vectors for the model 

input_list <- 
  list(cohabitation1_cdf, cohabitation2_cdf, cohabitation3_cdf,
       marriage_cdf, cohab_mar_cdf1, cohab_mar_cdf2, cohab_mar_cdf3,
       divorce_cdf,  separation_cdf,
       repartner_cdf, fert_int_both, edu_share)

# Assigning names to the vectors in the list 
names(input_list) <-  c("cohabitation1_cdf", "cohabitation2_cdf", "cohabitation3_cdf",
                        "marriage_cdf", "cohab_mar_cdf1", "cohab_mar_cdf2",
                        "cohab_mar_cdf3",
                        "divorce_cdf",  "separation_cdf",
                         "repartner_cdf", "fert_int_both", "edu_share")

# Saving as .RDS file

saveRDS(input_list, file =
          "./simulation/dutch_simulation_inputs.RDS")


# Union shares

union_share_list <- list(share_cohabited, share_output_list$share_cohabited, 
                         share_married, share_output_list$share_married,
                         share_separated,share_output_list$share_separated,
                         share_divorced, share_output_list$share_divorced,
                         share_repartner_new,share_output_list$share_repartnered,
                         share_cohab_mar,
                         share_output_list$share_separated_5pct,
                         share_output_list$share_divorced_5pct,
                         share_output_list$share_married_5pct, 
                         share_output_list$share_cohabited_5pct,
                         share_output_list$share_repartnered_5pct)


names(union_share_list) <- c("share_cohabited", "share_cohabited_edu", "share_married", 
                             "share_married_edu", "share_separated", "share_separated_edu",
                              "share_divorced", "share_divorced_edu",
                              "share_repartnered", "share_repartnered_edu", "share_cohab_mar",
                             "share_separated_edu_5pct",
                             "share_divorced_edu_5pct",
                             "share_married_edu_5pct",
                             "share_cohabited_edu_5pct",
                             "share_repartnered_edu_5pct")

saveRDS(union_share_list, file ="./simulation/union_share_list.RDS") 





# The mean age at marriage used in the full model with cohabitation and repartnering

mean_age_first_mar2 <- mean_age_first_cohab+mean(combined_cohab_to_mar$cohab_to_mar)

mean_age_first_sep <- mean_age_first_cohab+mean_length_cohab

mean_age_first_div <- mean_age_first_mar2+mean_length_mar



# CBS data on the mean age at marriage 
# https://opendata.cbs.nl/#/CBS/nl/dataset/37772ned/table?defaultview&dl=293DA
# Select the download option "CSV zonder symbolen" and enter the file name in the
# 'file' option below. 

mean_age_mar_data <- fread(file = "./data/Huwen__kerncijfers_22022024_161621.csv",
                        header = TRUE, nrows = 73, dec=',')

# Selecting the year range

mean_age_mar_data <- 
  mean_age_mar_data[ Perioden %in% 2000:2022]


## Difference between the mean age at marriage and the mean age at first marriage 
## in 2022
## mean age at first marriage from: https://www.cbs.nl/nl-nl/visualisaties/dashboard-bevolking/levensloop/trouwen
# 36.4-32.8 = 3.6


# Calculating an estimate of the mean age at first marriage for the 1974-1984
# cohort by taking the mean age at marriage over the years 2000-2022 and subtracting
# the difference between the mean age at first marriage from the mean age at 
# marriage in 2022. 

mean_age_first_mar <- with(mean_age_mar_data,
                           mean(`Gemiddelde leeftijd bij huwelijksslui.../Vrouwen/Totaal vrouwen (jaar)`)-3.6)

# Mean age at first marriage in the complete model which is the mean age at 
# first cohabitation plus the mean transition time from cohabitation to marriage


union_age_duration_list <-
  list(mean_age_first_cohab, mean_age_first_mar,
       mean_age_first_sep, mean_age_first_div,
        mean_length_cohab, mean_length_mar, mean_length_repart)

names(union_age_duration_list) <-
  c("mean_age_first_cohab", "mean_age_first_mar",
   "mean_age_first_sep", "mean_age_first_div",
    "mean_length_cohab", "mean_length_mar", "mean_length_repart")

saveRDS(union_age_duration_list, file ="./simulation/union_age_duration_list.RDS")


#####

