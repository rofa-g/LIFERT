## Figures and tables used in the paper

# Loading required packages

library(ggplot2)    # Plots
library(dplyr)      # Piping
library(ggpubr)     # ggplot formatting
library(ggtext)     # ggplot formatting
library(data.table) # data handling
library(kableExtra) # exporting output tables
library(magick)     # save_kable will have the best result with magick installed
library(forcats)    # fct_reorder
library(scales)     # label_wrap
library(flextable)  # export table as word file
library(officer)    # export table as word file
library(magrittr)   # export table as word file

#### DATA DESCRIPTION TABLE (2 PARTS) ####


inputs_info_table1 <- data.table(
  "Parameter" = c("Fecundability", "Intrauterine mortality", "Non-susceptible period",
                  "Age at permanent sterility", "Intended spacing", "Spacing & stopping contraception",
                  "Unintended pregnancy", "Abortion"
  ), 
  "Sample size" = c("-", "-", "-", "-", "-", "-", "-", "-"
                    
  ),
  "Processing/information" = c("Beta distribution (a = 3, b = 9, mean \U2248 0.25), 12.5 years of linear decline before age at sterility (Leridon 2004, 2017)",
                               "Fitted third degree polynomial distribution to values estimated by Leridon (1977, pp. 61-66), Month of miscarriage distribution based on clinical data<sup>1</sup>",
                               "Live birth: truncated normal distribution (mean = 4, lower = 0)<sup>2</sup>, miscarriage & abortion<sup>3</sup>: 1 month",
                               "Cubic spline interpolation between yearly data points from Leridon (2017), sampling from distribution",
                               "9 months of pregnancy + 5 months<sup>4</sup> waiting subtracted from the difference between the mean age at first cohabitation and the mean age at first birth, and HFD birth interval differences",
                               "spacing: 98.2% efficacy ages 15-25, 96.2% ages 26-55<sup>5</sup> stopping: 99.1% efficacy based on Leridon (2017)",
                               "Unintended pregnancies \U2248 20%<sup>6</sup>",
                               "58% probability, abortion ratio (154 abortions per 1,000 live births, years 2000-2020) and month of pregnancy in which abortion occurred set to match official statistics<sup>7</sup>"
                               
  ),
  "Format" = c("Pseudo-randomly generated from distribution",
               "Same distribution for all women, sampling from month of miscarriage distribution",
               "Live birth: sampling from distribution, miscarriage & abortion: same for all women",
               "Sampling from distribution",
               "Same distribution for all women",
               "Same for all women",
               "Same for all women",
               "Same for all women"
               
               
  ))


inputs_info_table2 <- data.table(
  "Parameter" = c("First cohabitation (and cohabitation)",
                  "Cohabitation to marriage",
                  "Separation",
                  "Re-partnering",
                  "Divorce",
                  "Education"
  ), 
  "Sample size (by education)" = c("Low: 168, Mid: 469, High: 651",
                    "Low: 309, Mid: 798, High: 705",
                    "724",
                    "250", 
                    "496",
                    "Low: 357, Mid: 1,007, High: 1,209"
                    
  ),
  "Processing/information" = c("Gumbel distribution (best fit to data, based on AIC), cohort share who ever cohabit 95% based on 1954-1964 GGS cohort and Bellani et al. (2017), education specific shares calculated with equation systems from the ratios between the educational shares of the 1964-1984 GGS cohort<sup>1</sup> that sum up to the cohort share, share who stay cohabited after first cohabitation 10%<sup>2</sup>",
                               "Exponential distribution (best fit to data, based on AIC), cohort share who marry of 58.0% based on CBS reports<sup>2</sup>, 70% share who ever married based on CBS estimates<sup>3</sup>, education specific shares estimated the same way as first cohabitation, but with 1954-1984 cohort<sup>4</sup>",
                               "Exponential distribution (best fit to data, based on AIC), cohort share who separate of 31.2% based on CBS report<sup>5</sup>, education specific shares estimated the same way as first cohabitation, but with 1954-1974 cohort<sup>4</sup>",
                               "Gumbel distribution (best fit to data, based on AIC), Re-partnering set at 75%, based on Finnish 1969-1971 birth cohort (76.2%)<sup>6</sup>, education specific shares estimated the same way as first cohabitation, but with 1954-1964 cohort<sup>4</sup>",
                               "Exponential distribution (best fit to data, based on AIC), cohort share who divorce of 27.6% based on CBS report<sup>7</sup> (separate + marry*divorce \U2248 48%)<sup>2</sup>, education specific shares estimated the same way as first cohabitation, but with 1954-1964 cohort<sup>4</sup>",
                               "Educational structure from LISS and GGS, duration of enrolment based on expected year of graduation"
                               
  ),
  "Format" = c("RNG* compared with CDF* value corresponding to the iteration/month",
               "\U2014\U3003\U2014",
               "\U2014\U3003\U2014",
               "\U2014\U3003\U2014",
               "\U2014\U3003\U2014",
               "Educational attainment sampled from distribution"
               
  ))

# ft1 <- flextable(inputs_info_table1) %>%  
# autofit()


# MS WORD EXPORT
# NOTE THAT THE SUPERSCRIPTS '<sup>#</sup>' NEED TO BE MANUALLY CHANGED IN THE
# OUTPUT DOCUMENT BECAUSE THERE IS NO CLEAN AND SIMPLE WAY TO ADD SUPERSCRIPTS
# USING FLEXTABLE WHEN THE OUPUT IS A MS WORD DOCUMENT

ft1 <- regulartable(inputs_info_table1) %>% 
  fontsize(size = 12, part = "all") %>% 
  font(fontname = "Times New Roman", part = "all") %>% 
  align(align = "center", part = "all") %>% 
  set_table_properties(layout = "autofit", width = 1) %>% 
  fit_to_width(max_width = 246.2, unit = "mm")

doc <- read_docx() %>%
  body_add_flextable(value = ft1, split = TRUE) %>%
  body_end_section_landscape() %>% # a landscape section is ending here
  print( target = "./plots/t1_descriptive_table_part_1.docx" )




ft2 <- regulartable(inputs_info_table2) %>% 
  fontsize(size = 12, part = "all") %>% 
  font(fontname = "Times New Roman", part = "all") %>% 
  align(align = "center", part = "all") %>% 
  set_table_properties(layout = "autofit", width = 1) %>% 
  fit_to_width(max_width = 246.2, unit = "mm")

doc <- read_docx() %>%
  body_add_flextable(value = ft2, split = TRUE) %>%
  body_end_section_landscape() %>% # a landscape section is ending here
  print( target = "./plots/t1_descriptive_table_part_2.docx" )

#####




## Fertility gap (table 2) 

fert_gap_param_t <- 
  data.table(Parameter = 
               c("Fecundability",
                 "Intrauterine mortality",
                 "Age at first cohabitation",
                 "Transition time from cohabitation to marriage",
                 "Time spent finding new partner",
                 "Share of cohabitations ending in separation",
                 "Share of marriages ending in divorce",
                 "Share who marry",
                 "Share who re-partner",
                 "Share who ever cohabit"),
             Adjustment = 
               c("No decline until sterility",
                 "Fixed at the level of 20-year-old",
                 "Set to age 15",
                 "Set to 1 month",
                 "Set to 1 month",
                 "Set to 0%",
                 "Set to 0%",
                 "Set to 100% ",
                 "Set to 100%",
                 "Set to 100%"), 
             `Change in parameter` = 
               c("No 12.5-year linear decline",
                 "No cubic increase",
                 "-8.66 years",
                 "-35 months (avg)",
                 "-29 months (avg)",
                 "-31.45 %-points",
                 "-25.59 %-points",
                 "+42.51 %-points",
                 "+26.59 %-points",
                 "+4.97 %-points"),
             `Contribution to fertility gap` =
               c(-0.0904, -0.01513, -0.09315, 0.02763, -0.03625,
                 -0.18002, -0.03593, -0.16175, -0.12797,
                 -0.03786),
             `Contribution in percent` = 
               c(-0.0904, -0.01513, -0.09315, 0.02763, -0.03625,
                 -0.18002, -0.03593, -0.16175, -0.12797,
                 -0.03786)/0.3422478*100)%>% 
  dplyr::mutate_if(is.numeric, signif, 3)



# Need to manually set the red and green colours for positive and negative contributions
# in MS Word

ft3 <- regulartable(fert_gap_param_t) %>% 
  fontsize(size = 12, part = "all") %>% 
  font(fontname = "Times New Roman", part = "all") %>% 
  align(align = "center", part = "all") %>% 
  set_table_properties(layout = "autofit", width = 1) %>% 
  fit_to_width(max_width = 159.2, unit = "mm")

doc <- read_docx() %>%
  body_add_flextable(value = ft3, split = TRUE) %>%
  print( target = "./plots/t2_fertility_gap_abs.docx" )










## Education data table (Table 3)

edu_parameter_adj_t <- 
  data.table(Parameter =
               c("Fertility gap",
                 "Age at first cohabitation",
                 "Share who ever cohabit (%)",
                 "Transition time to marriage (years)",
                 "Share of cohabitations resulting in marriage (%)",
                 "Share of cohabitations resulting in separation (%)",
                 "Share of marriages resulting in divorce (%)",
                 "Share of union dissolutions resulting in re-partnering (%)",
                 "Age at graduation (enrolled until age)"),
             `High education (ISCED 5-7)` = 
               c("0.41","25.43", "93.91", "4.32", "51.98", "25.17", "24.77", "72.55", "22"),
             `Low education (ISCED 0-2)` = 
               c("0.24","22.33", "99.85", "5.09", "64.25", "30.40", "30.76", "74.34", "18"),
             `Individual contribution to difference in the fertility gap` = 
               c(0.17433, 0.1168, 0.07425 , -0.01236, 0.07141, -0.04097, -0.01155, 0.00139, 0.00384),
             `Individual contribution in percent` =
               c(0.17433, 0.1168, 0.07425 , -0.01236, 0.07141, -0.04097, -0.01155, 0.00139, 0.00384)/0.17433*100#,
             # contribution_cumulative =
             #   c(0.17433, 0.11438, 0.15982, 0.15829, 0.2281, 0.17975, 0.16136, 0.17241, 0.18046)/0.17433*100 
  )%>% 
  dplyr::mutate_if(is.numeric, round, 2) 




# Need to manually set the red and green colours for positive and negative contributions
# in MS Word

ft4 <- regulartable(edu_parameter_adj_t) %>% 
  fontsize(size = 12, part = "all") %>% 
  font(fontname = "Times New Roman", part = "all") %>% 
  align(align = "center", part = "all") %>% 
  set_table_properties(layout = "autofit", width = 1) %>% 
  fit_to_width(max_width = 159.2, unit = "mm")

doc <- read_docx() %>%
  body_add_flextable(value = ft4, split = TRUE) %>%
  print( target = "./plots/t3_educational_gradient.docx" )











## Counterfactual future adjustments (Table 4)


future_adjustments <- data.table(
  Parameter = c(
    "Share of highly educated",
    "Share who separate",
    "Share who divorce",
    "Share who marry",
    "Share who re-partner",
    "Share that ever cohabit",
    "Age at first cohabitation",
    "Age at first cohabitation",
    "Age at first cohabitation",
    "Age at first cohabitation",
    "Age at first cohabitation"
  ), 
  Adjustment = c(
    "25-34-year-olds in 2022*",
    "Increase by 5%-points",
    "Increase by 5%-points",
    "Decrease by 5%-points",
    "Decrease by 5%-points",
    "Decrease by 5%-points",
    "Increase mean by 1 year",
    "Increase mean by 2 years",
    "Increase mean by 3 years",
    "Increase mean by 4 years",
    "Increase mean by 5 years"
  ),
  `Contribution to fertility gap` = c(
    0.01773, 0.02366, 0.01421, 0.02576, 0.02815, 0.0761, 0.03887, 0.07799,
    0.12118, 0.17162, 0.22558),
  `Contribution in percent` = 
    c(0.01773, 0.02366, 0.01421, 0.02576, 0.02815, 0.0761, 0.03887, 0.07799,
      0.12118, 0.17162, 0.22558)/0.3422478*100) %>% 
  dplyr::mutate_if(is.numeric, round, 2) 



ft5 <- regulartable(future_adjustments) %>% 
  fontsize(size = 12, part = "all") %>% 
  font(fontname = "Times New Roman", part = "all") %>% 
  align(align = "center", part = "all") %>% 
  set_table_properties(layout = "autofit", width = 1) %>% 
  fit_to_width(max_width = 159.2, unit = "mm")

doc <- read_docx() %>%
  body_add_flextable(value = ft5, split = TRUE) %>%
  print( target = "./plots/t4_future_adjustments.docx" )






# SIMULATION SUMMARY TABLES A1 AND A2

# NEED TO LOAD names.R and the functions summary_f2 and summary_f3
# from summary_functions.R for the below exports to work


test_vec_sum<-
readRDS(file = "./simulation/exact_replication_data/base_model_100k_10feb25.RDS")

object_test_sum <-
summary_f2(test_vec_sum)


sum_table<-  data.table("Indicator" =
 c("Mean age at first cohabitation (GGS, LISS)",
   "Mean age at first marriage (CBS, 2004-2014)<sup>1</sup>",
   "Mean age at first separation, no previous divorce (GGS, LISS)",
   "Mean age at first divorce (GGS, LISS)",
   "Mean age at first repartnering",
   "Percent ever cohabited (GGS, estimate)",
   "Percent ever married (CBS, estimate)<sup>2</sup>",
   "Percent cohabited & never separated or married (CBS, estimate)<sup>3</sup>",
   "Percent marriage (cohabitation to marriage; CBS, estimate)<sup>3</sup>",
   "Percent separation (CBS, estimate)<sup>4</sup>", "Percent divorce (CBS, estimate)<sup>5</sup>",
   "Percent repartnering (Finnish register data<sup>6</sup>, estimate)",
   "Mean age at first birth (HFD, 1979 cohort)<sup>7</sup>",
   "Mean age second birth (HFD, 1979 cohort)<sup>7</sup>",
   "Mean age third birth (HFD, 1979 cohort)<sup>7</sup>",
   "Mean age fourth birth (HFD, 1979 cohort)<sup>7</sup>",
   "Completed cohort fertility within coresidential unions (total births in parenthesis) (HFD, 1979 cohort)<sup>7</sup>",
   "Fertility gap within coresidential unions (total births in parenthesis)",
   "0 children (HFD, 1969 cohort, %)",
   "1 child (HFD, 1969 cohort, %)",
   "2 children (HFD, 1969 cohort, %)",
   "3 children (HFD, 1969 cohort, %)",
   "4+ children (HFD, 1969 cohort, %)",
   "Miscarriages per live birth",
   "Percent unintended pregnancies<sup>8</sup>",
   "Abortion ratio 2000-2020<sup>9</sup> (abortions per 1,000 live births)"),
"Demographic data" =
 c(24.5, 29.7, 28.5, 37.6, NA_real_, 95.0, 70.0, 10.0,
                  58.0, 32, 27.6, 75, 29.2, 31.6, 33.1, 34.5, "1.66 (1.80)", "0.364 (0.227)",
                  17.6, 18.5, 42.7, 15.6, 5.6, NA_real_, 20, 154),
"Simulation results (1974-1984 cohort)" =
  unlist(matrix(object_test_sum))) %>% 
  dplyr::mutate_if(is.numeric, round, 3)


ft6 <- regulartable(sum_table) %>% 
  fontsize(size = 12, part = "all") %>% 
  font(fontname = "Times New Roman", part = "all") %>% 
  align(align = "center", part = "all") %>% 
  set_table_properties(layout = "autofit", width = 1) %>% 
  fit_to_width(max_width = 159.2, unit = "mm")

doc <- read_docx() %>%
  body_add_flextable(value = ft6, split = TRUE) %>%
  print( target = "./plots/tA1_simulation_summary.docx" )






## SUMMARY BY EDUCATION

object_test_sum_edu <-
  summary_f3(test_vec_sum)%>% 
  dplyr::mutate_if(is.numeric, round, 3)

ft7 <- regulartable(object_test_sum_edu) %>% 
  fontsize(size = 12, part = "all") %>% 
  font(fontname = "Times New Roman", part = "all") %>% 
  align(align = "center", part = "all") %>% 
  set_table_properties(layout = "autofit", width = 1) %>% 
  fit_to_width(max_width = 159.2, unit = "mm")

doc <- read_docx() %>%
  body_add_flextable(value = ft7, split = TRUE) %>%
  print( target = "./plots/tA2_simulation_summary_edu.docx" )



