# GGS  data wrangling

############################### IMPORTANT !!! ##################################


# Please note that you need to access and download the raw data yourself via the 
# GGS website for this script to work. The original file names were preserved,
# so there should be no need to manually change any file names for the code to 
# work once you have downloaded the data files and placed them in the correct 
# folders relative to the R project. 

# You need to register an account at https://www.humanfertility.org/ in order to
# access the Human Fertility Database data using the HMDHFDplus package


# The data files used in this script are the Stata files (.dta) for:

# GGS 1 Wave 1

# RUN THIS SCRIPT BEFORE YOU RUN THE LISS WRANGLING SCRIPT AND BEFORE YOU 
# RUN THE SIMULATION SCRIPT

################################################################################



#### Loading packages and setting up session ####

# Handling Stata files
library(haven)

# Data management
library(tidyverse)
library(data.table)

# Plotting
library(ggtext)



# Editing or getting rid of column labels from haven .dta imports
library(labelled) 

#####

#### Colour blind palette ####

cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

#####


#### Data sets ####

# SELECTING VARIABLES, FILTERING, RECODING 

ggs_wave1 <- 
  read_dta(file = "./data/GGS/GGS_Wave1_V.4.4.dta",
           # Select columns
           col_select = c("arid", "acountry", "asex", "abyear", "aeduc",
                          "numbiol","numpartnerships", "nummarriage", "numdivorce", "numdissol",
                          `a334y_1`:`a334y_7`, `a335a_1`:`a335a_7`,
                          `a344y_1`:`a344y_7`,"a622", "a626","a372bAgeR",
                          "a302bTdiff")) %>%
  # Include only the Netherlands
  filter(acountry == 18,
         # Include only women
         asex == 2) %>% 
  # Recoding education to match the LISS data
  mutate(aeduc = case_when(
    aeduc %in% 0:2 ~ "1", # isced 0-2
    aeduc %in%  3:4 ~ "2", # isced 3-4
    aeduc %in%  5:6 ~ "3", # isced 5-6
    .default = "NA"),
    aeduc = as.numeric(aeduc))


# Cohort for fertility intentions

ggs_wave1a <- 
  ggs_wave1 %>% 
  filter(as.numeric(abyear) %in% 1974:1984) 

ggs_wave1b <- 
  ggs_wave1 %>% 
  filter(as.numeric(abyear) %in% 1964:1984) 

ggs_wave1c <- 
  ggs_wave1 %>% 
  filter(as.numeric(abyear) %in% 1954:1964) 

ggs_wave1d <- 
  ggs_wave1 %>% 
  filter(as.numeric(abyear) %in% 1954:1984) 

ggs_wave1e <- 
  ggs_wave1 %>% 
  filter(as.numeric(abyear) %in% 1964:1974) 

ggs_wave1f <- 
  ggs_wave1 %>% 
  filter(as.numeric(abyear) %in% 1954:1974) 


#####


#### CALCULATING SHARES FOR DIFFERENT UNION STATUSES BY EDUCATION ####


## SHARE OF WOMEN WHO EVER COHABIT

# 1964-1984 cohort

ggs_cohab_edu <- 
  ggs_wave1b %>% 
  filter(!is.na(aeduc)) %>% # remove respondents with missing education
  group_by(aeduc) %>% 
  summarise(p = length(numpartnerships[numpartnerships > 0])/length(arid)) %>% 
  pull(p) # this pulls a vector out of the dataframe that results from summarise


# Saving for use in LISS data wrangling script
saveRDS(ggs_cohab_edu, "./data/share_cohab_edu.RDS")


## SHARE WHO MARRY BY EDUCATION

# 1954-1984 cohort

ggs_share_mar <- 
  ggs_wave1d %>% 
  filter(!is.na(aeduc)) %>% # remove respondents with missing education
  group_by(aeduc) %>% 
  summarise(p = sum(nummarriage)/sum(numpartnerships)) %>% 
  pull(p) # this pulls a vector out of the dataframe that results from summarise

# Saving for use in LISS data wrangling script
saveRDS(ggs_share_mar, "./data/share_mar_edu.RDS")


## SHARE WHO DIVORCE BY EDUCATION

# 1954-1964 cohort 

ggs_share_div_edu <- 
  ggs_wave1c %>% 
  filter(!is.na(aeduc)) %>% # remove respondents with missing education
  group_by(aeduc) %>% 
  summarise(p = sum(numdivorce)/sum(nummarriage)) %>% 
  pull(p) # this pulls a vector out of the dataframe that results from summarise


# Saving for use in LISS data wrangling script
saveRDS(ggs_share_div_edu, "./data/share_divorce_edu.RDS")


## SHARE WHO SEPARATE BY EDUCATIONAL ATTAINMENT

# 1954-1974 cohort split by education

ggs_share_sep_edu <- 
  ggs_wave1f %>% 
  filter(!is.na(aeduc)) %>% # remove respondents with missing education
  group_by(aeduc) %>% 
  summarise(p = sum(numdissol-numdivorce)/(sum(numpartnerships-nummarriage))) %>% 
  pull(p) # this pulls a vector out of the dataframe that results from summarise


# Saving for use in LISS data wrangling script
saveRDS(ggs_share_sep_edu, "./data/share_separation_edu.RDS")

## SHARE WHO REPARTNER BY EDUCATION

# 1954-1964 cohort split by education

ggs_share_repart_edu <- 
  ggs_wave1c %>% 
  filter(!is.na(aeduc)) %>% # remove respondents with missing education
  group_by(aeduc) %>%
  summarise(p = sum(if_else(numpartnerships > 0, numpartnerships - 1, numpartnerships))/sum(numdissol)) %>% 
  pull(p) # this pulls a vector out of the dataframe that results from summarise


# Saving for use in LISS data wrangling script
saveRDS(ggs_share_repart_edu, "./data/share_repartner_edu.RDS")

#####


#### EDUCATIONAL ATTAINMENT ####


edu_attainment <- 
  ggs_wave1a %>% 
  dplyr::select(arid, abyear, aeduc) %>% 
  # Need to be old enough to have completed tertiary education, 25 seems reasonable
  filter(abyear <= 1978) %>% 
  mutate(across(everything(), as.numeric))

# Saving for use in LISS data wrangling script
saveRDS(edu_attainment, file = "./data/ggs_edu_attainment.RDS")

######


#### DURATION BETWEEN COHABITATION AND MARRIAGE ####

ggs_cohab_to_mar <- 
  ggs_wave1a %>% 
  dplyr::select(arid, abyear, aeduc, a302bTdiff) #a302bTdiff is the duration

# Saving for use in LISS data wrangling script
saveRDS(ggs_cohab_to_mar, file = "./data/ggs_cohab_to_mar.RDS")

#####


#### AGE AT FIRST COHABITATION ####

# a334y_1: Yr starting living together with partner 1

# abyear: Year of birth of respondent

# arid: Respondent ID

# aeduc: educational attainment of respondent

ggs_age_first_cohab <- 
ggs_wave1b %>% 
  dplyr::select(arid, abyear, aeduc, a334y_1) %>% 
  mutate(age_first_cohab = a334y_1-abyear,
         age_first_cohab = ifelse(age_first_cohab <15, 15, age_first_cohab)) %>% 
  dplyr::select(!a334y_1) %>% 
  rename("nomem_encr" = "arid",
         "year_birth" = "abyear",
         "edu_level" = "aeduc") %>% 
  na.omit()

# Saving for use in LISS data wrangling script
saveRDS(ggs_age_first_cohab, file = "./data/ggs_age_first_cohab.RDS")

#####


#### INTENDED FAMILY SIZE ####

## a622: Intention to have a child during next 3 yrs

## a626: How many more children R wants to have

## numbiol: number of biological children the woman has had

# Choosing birth cohorts and generating fertility intention variable

ggs_wave1a <- 
ggs_wave1a %>% 
  # If the woman wants more children, ideal number is biological+additional wanted
  mutate(ideal_fert = case_when(a622 == 1801 | a622 == 1802 ~  a626+numbiol,
                                # If she wants no more children, ideal = biological
                                 a622 == 1803 ~ numbiol,
                                # Else missing
                                 .default = NA_real_
         ))

# Generating probability distribution for fertility intentions

ideal_fert_dist <- data.table(num = ggs_wave1a$ideal_fert,
                              year_birth = ggs_wave1a$abyear,
                              edu_level = ggs_wave1a$aeduc ) %>% 
  # few outliers above 5, recode them as 5
  mutate(num = if_else(num > 5, 5, num)) %>% 
  na.omit() %>% 
  count(num) %>% 
  # 23 cannot be correct, it is biologically impossible
  filter(num != 23) %>% 
  mutate(prob = n/sum(n),
         N = sum(n))



# Saving for use in LISS data wrangling script
saveRDS(ideal_fert_dist, "./data/ideal_fert_dist_ggs.RDS")


# Intentions by educational attainment

ideal_fert_dist_edu <- 
  ggs_wave1a %>% 
  dplyr::select(arid, ideal_fert, aeduc) %>% 
  na.omit() %>% 
  # few outliers above 5, recode them as 5
  mutate(ideal_fert = if_else(ideal_fert > 5, 5, ideal_fert)) %>% 
  group_by(aeduc) %>% 
  count(ideal_fert) %>%  
  # 23 cannot be correct, it is biologically impossible
  filter(ideal_fert != 23) %>% 
  mutate(prob = n/sum(n),
         N = sum(n)) %>% 
  ungroup() %>% 
  rename("edu_level" = "aeduc",
         "fert_intention" = "ideal_fert")

saveRDS(ideal_fert_dist_edu, "./data/ggs_intended_fert_edu.RDS")

#####






### DURATION OF UNIONS ####

# a334y_1: Yr starting living together with partner 1

# b335a_1: Was married to partner 1

# b344y_1: In what year did the relationship end with partner 1


# Variable names for beginning of relationship number #
begin_rel <- paste0("a334y_", 1:7, sep = "")

# Variable names for end of relationship number #
end_rel <- paste0("a344y_", 1:7, sep = "")

### Result frame

res <- 
  ggs_wave1d %>% # Need to use 1954-1984 range for sufficient sample size
  dplyr::select(arid, aeduc, a335a_1:a335a_7) 
  
names(res) <- gsub(x = names(res), pattern = "a335a_", replacement = "married_")



# For loop where each column is the observed length (years) of relationship number #

# Not recommended to use eval(parse), but I did not find alternatives that would
# work. Not a big problem here, since I only turn strings into objects, I don't write
# any code in text that I then evaluate. 

# Duration of (coresidential) relationships

for(i in 1:7){
  
  res[[paste0("lengthrel_", i)]] = 
    with(ggs_wave1d,
         # 'parse' transforms string to expression, 'eval' evaluates the expression
         # turns the vector of characters into a vector of objects that are evaluated
         eval(parse(text = end_rel[i]))-eval(parse(text = begin_rel[i])))
}


# Duration of time between end of previous (coresidential) 
# relationship and start of new relationship

for(i in 2:7){ # starts from 2 because duration is calculated from the previous
               # relationship end_rel[i-1]
  
  res[[paste0("lengthrepart_", i)]] = 
    with(ggs_wave1d,
         # 'parse' transforms string to expression, 'eval' evaluates the expression
         # turns the vector of characters into a vector of objects that are evaluated
         eval(parse(text = begin_rel[i]))-eval(parse(text = end_rel[i-1])))
}


# Pooling all relationship durations into one column

res <- res %>% 
  pivot_longer(cols = -c(arid, aeduc), names_sep = "_", names_to = c("type", "part"),
               values_to = c("value")) %>% 
  pivot_wider(names_from = "type", values_from = "value") %>% 
  group_by(arid, part) %>% 
  filter(any(!is.na(c(married, lengthrel, lengthrepart)))) %>% 
  ungroup() %>% 
  mutate(across(everything(), as.numeric))

# Save data for combining with LISS
saveRDS(res, file = "./data/ggs_mar_coh_duration.RDS")

# Plotting

# res %>%
#   ggplot(aes(x = lengthrel, fill = as.factor(married)))+
#   geom_bar(position = "dodge")+
#   scale_x_continuous(name = "Length of relationship in years",
#                      breaks = 0:10,
#                      labels = 0:10,
#                      limits = c(-1,10))+
#   scale_fill_manual(values = cbbPalette[2:3], labels = c("Marriage", "Cohabitation"))+
#   theme(text=element_text(size=14),
#         plot.title = element_markdown(hjust = 0.5, size = 16),
#         plot.caption = element_markdown(hjust = 0))+
#   labs(title = "Number of dissolutions by length of relationship and <br>
#        union type, women born 1974-1984")+
#   guides(fill=guide_legend(title="Union type"))+
#   ylab("Count")


# By education 

# res %>% 
#   ggplot(aes(x = lengthrel, fill = as.factor(married), linetype = as.factor(aeduc), colour = as.factor(aeduc)))+
#   geom_bar(position = "dodge", linewidth = 1.2)+
#   scale_x_continuous(name = "Length of relationship in years",
#                      breaks = 0:10, 
#                      labels = 0:10,
#                      limits = c(-1,10))+
#   scale_fill_manual(values = cbbPalette[2:3])+
#   scale_linetype_manual(values = c("solid", "dashed", "dotted"))+
#   scale_colour_manual(values = c("black", "black", "black"))+
#   theme(text=element_text(size=14),
#         plot.title = element_markdown(hjust = 0.5, size = 16),
#         plot.caption = element_markdown(hjust = 0))+
#   labs(title = "Number of dissolutions by length of relationship and <br>
#        union type, women born 1974-1984")+
#   ylab("Count")+
#   guides(linetype = guide_legend(override.aes = list(fill = NA
#                                                      , col = "black")))


#### AGE AT FIRST MARRIAGE ####

age_mar_dat <- ggs_wave1a %>% 
  mutate(age_first_mar = if_else(nummarriage == 1 & a372bAgeR >= 15, a372bAgeR, NA_real_ )) %>% 
  dplyr::select(arid, abyear, age_first_mar, aeduc) %>% 
  rename("nomem_encr" = "arid",
         "edu_level" = "aeduc",
         "year_birth" = "abyear")  %>% 
  na.omit() 

# Saving for use in LISS wrangling script
saveRDS(age_mar_dat, file = "./data/ggs_age_mar.RDS")


# Plotting

# age_mar_dat %>% 
#   ggplot()+
#   geom_bar(aes(x = age_first_mar))+
#   xlab("Age")+
#   ylab("Count")+
#   theme(text=element_text(size=14),
#         plot.title = element_markdown(hjust = 0.5, size = 16),
#         plot.caption = element_markdown(hjust = 0))+
#   labs(title = "Age at first marriage women born 1974-1984")

#####

