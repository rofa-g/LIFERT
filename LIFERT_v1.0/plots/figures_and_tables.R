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



#### DATA DESCRIPTION TABLE (2 PARTS) ####


inputs_info_table1 <- data.table(
  "Parameter" = c("<br>Fecundability", "<br>Intrauterine mortality", "<br>Non-susceptible period",
                  "<br>Age at permanent sterility", "Intended spacing", "Spacing & stopping contraception",
                  "<br>Unintended pregnancy", "Abortion"
  ), 
  "Sample size" = c("<br>-", "-", "<br>-", "<br>-", "-", "-", "<br>-", ""
                    
  ),
  "Processing/information" = c("<br>Beta distribution (a = 3, b  = 9, mean \U2248 0.25),<br>
                               12.5 years of linear decline before age at sterility (Leridon 2004, 2017)",
                               "<br>Fitted third degree polynomial distribution to 
                               values estimated by Leridon (1977, pp. 61-66),
                               Month of miscarriage distribution based on clinical data<sup>1</sup>",
                               "<br>Live birth: truncated normal distribution 
                               (mean = 4, lower = 0)<sup>2</sup>,<br> miscarriage & abortion<sup>3</sup>: 1 month",
                               "<br>Cubic spline interpolation between yearly data points
                               from Leridon (2017),<br> sampling from distribution",
                               "9 months of pregnancy + 5 months<sup>4</sup> waiting subtracted 
                               from the difference between the mean age
                               at first cohabitation and the mean age at first birth,
                               and HFD birth interval differences",
                               "spacing: 98.2% efficacy ages 15-25, 96.2% ages 26-55<sup>5</sup>
                               <br>stopping: 99.1% efficacy based on Leridon (2017)",
                               "Unintended pregnancies \U2248 20%<sup>6</sup>",
                               "<br>58% probability, abortion ratio (154 abortions per 1,000 live births, years 2000-2020) and month of pregnancy in which
                               abortion occurred set to match official statistics<sup>7</sup>"
                               
  ),
  "Format" = c("<br>Pseudo-randomly generated from distribution",
               "<br>Same distribution for all women,
                sampling from month of miscarriage distribution",
               "<br>Live birth: sampling from distribution,<br> miscarriage & abortion: same for all women",
               "<br>Sampling from distribution",
               "Same distribution for all women",
               "Same for all women",
               "Same for all women",
               "Same for all women"
               
               
  ))


inputs_info_table2 <- data.table(
  "Parameter" = c("First cohabitation (and cohabitation)",
                  "Cohabitation to marriage",
                  "Separation",
                  "<br>Re-partnering",
                  "<br>Divorce",
                  "Education"
  ), 
  "Sample size" = c("<br>ISCED 0-2: 168,<br> ISCED 3-4: 469,<br> ISCED 5-8: 651<br><br>",
                    "<br>ISCED 0-2: 309,<br>  ISCED 3-4: 798,<br> ISCED 5-8: 705<br><br>",
                    "<br>724<br><br>",
                    "<br><br>250<br><br>", 
                    "<br><br>496<br><br>",
                    "<br>ISCED 0-2: 357,<br> ISCED 3-4: 1,007,<br> ISCED 5-8: 1,209<br><br>"
                    
  ),
  "Processing/information" = c("Gumbel distribution (best fit to data, based on AIC),<br>
                               cohort share who ever cohabit 95% based on 1954-1964 GGS cohort and Bellani et al. (2017),
                               education specific shares calculated with equation systems
                               from the ratios between the educational shares of the 1964-1984 GGS cohort<sup>1</sup>
                               that sum up to the cohort share, share who stay cohabited after first cohabitation 10%<sup>2</sup>",
                               "Exponential distribution (best fit to data, based on AIC),<br>
                               cohort share who marry of 58.0% based on CBS reports<sup>2</sup>, 70% share
                               who ever married based on CBS estimates<sup>3</sup>, education
                               specific shares estimated the same way as first cohabitation, but with 1954-1984 cohort<sup>4</sup>",
                               "Exponential distribution (best fit to data, based on AIC),<br>
                               cohort share who separate of 31.2% based on CBS report<sup>5</sup>, education
                               specific shares estimated the same way as first cohabitation, but with 1954-1974 cohort<sup>4</sup>",
                               "<br>Gumbel distribution (best fit to data, based on AIC),<br>
                               Re-partnering set at 75%, based on Finnish 1969-1971 birth cohort (76.2%)<sup>6</sup>, education
                               specific shares estimated the same way as first cohabitation, but with 1954-1964 cohort<sup>4</sup>",
                               "<br>Exponential distribution (best fit to data, based on AIC),<br>
                               cohort share who divorce of 27.6% based on CBS report<sup>7</sup>
                               (separate + marry*divorce \U2248 48%)<sup>2</sup>, education
                               specific shares estimated the same way as first cohabitation, but with 1954-1964 cohort<sup>4</sup>",
                               "Educational structure from LISS and GGS, duration
                               of enrolment based on expected year of graduation"
                               
  ),
  "Format" = c("RNG* compared with CDF* value corresponding to the iteration/month",
               "\U2014\U3003\U2014",
               "\U2014\U3003\U2014",
               "<br>\U2014\U3003\U2014",
               "<br>\U2014\U3003\U2014",
               "Educational attainment sampled from distribution"
               
  ))

# Landscape output tables
# 
# knitr::kable(inputs_info_table1,
#              align = 'c', format = "html",
#              escape = FALSE) %>%
#   kable_classic(html_font = "serif") %>%
#   save_kable(file = "./plots/data_info_table3_new.png",
#              zoom = 3,
#              bs_theme = "flatly",
#              density = 300)

# 
# 
# knitr::kable(inputs_info_table2,
#              align = 'c', format = "html",
#              escape = FALSE) %>%
#   column_spec(2, width = "4cm") %>%
#   kable_classic(html_font = "serif") %>%
#   save_kable(file = "./plots/data_info_table4_new.png",
#              zoom = 3,
#              bs_theme = "flatly",
#              density = 300)

#####









#### PLOTTING FIGURES ####

## Preparing the data for plotting

# Colour blind palette
cb_palette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


# Reading in the data (to replicate paper results)

object_test <- readRDS("./simulation/exact_replication_data/base_model_100k_10feb25.RDS")

# Columns to select from the data

column_names <- c( "agent",             "N",                 "intended_children", "miscarriage",      
                   "unintended",        "num_abortions",     "age_fb",            "age_2b",           
                   "age_3b",            "age_4b",            "age_lb",            "age_cohabited",    
                   "age_separated",     "age_married",       "age_divorced",      "age_repartnered",  
                   "num_cohabitations", "num_marriages",     "num_separations",   "num_divorces",     
                   "num_repartnering",  "education")


# columns where NA should be changed to 0

na_to_0_cols <- c("miscarriage",        
                  "unintended",  "num_abortions",  "num_cohabitations", 
                  "num_marriages", "num_separations",  
                  "num_divorces", "num_repartnering" )

# Data used for plots

plot_data <- 
  object_test[, ..column_names
              # change NA to 0 for specified columns
  ][, (na_to_0_cols) := lapply(.SD, function(x) fifelse(is.na(x), 0, x)), .SDcols = na_to_0_cols
  ][, fert_gap := intended_children - N
  ][, education_factor := factor(education, levels = c(1,2,3),
                                 labels = c("ISCED 0-2",
                                            "ISCED 3-4",
                                            "ISCED 5-8"))]


## NUMBER OF SEPARATIONS AND FERTILITY GAP

plot_data %>% 
  count(fert_gap, num_separations) %>% 
  group_by(num_separations) %>% # remove this part to go from relative to absolute
  mutate(share = n/sum(n)) %>% 
  ggplot(aes(x = num_separations, y = fert_gap))+
  theme_classic()+
  geom_point(aes(size = share),
             colour = "gray15")+
  geom_text(aes(label = ifelse(fert_gap == 0, round(share, digits = 2), "")), colour = "white",
            family = "serif", size = 5, fontface = 2)+
  scale_size_continuous(range = c(2, 17), name="Share of women with *n* separations",
                        breaks = seq(0, 1, length.out = 6))+
  scale_y_continuous(breaks = seq(-3,5,1), limits = c(-3,5))+
  scale_x_continuous(breaks = seq(0,7,1), limits = c(0,7))+
  xlab("Number of separations")+
  ylab("Fertility gap")+
  theme(text = element_text(size=14,  family="serif"),
        plot.title = element_markdown(hjust = 0.5, size = 16),
        plot.caption = element_markdown(hjust = 0),
        legend.position = "bottom",
        legend.title = ggtext::element_markdown(hjust = 0.5),
        legend.title.position = "bottom")

ggsave("./plots/gap_separations.png", dpi = 600, height = 150, width = 159.2, units = "mm")



## NUMBER OF DIVORCES AND COMPLETED FERTILITY

plot_data %>% 
  mutate(num_divorces = ifelse(is.na(num_divorces), 0, num_divorces),
         fert_gap = intended_children - N) %>% 
  count(fert_gap, num_divorces) %>% 
  group_by(num_divorces) %>% # remove this part to go from relative to absolute
  mutate(share = n/sum(n)) %>%
  ggplot(aes(x = num_divorces, y = fert_gap, size = share),
         colour = "gray15")+
  theme_classic()+
  geom_point()+
  geom_text(aes(label = ifelse(fert_gap == 0, round(share, digits = 2), "")), colour = "white",
            family = "serif", size = 5, fontface = 2)+
  scale_size_continuous(range = c(2, 17), name="Share of women with *n* divorces",
                        breaks = seq(0, 1, length.out = 6))+
  scale_y_continuous(breaks = seq(-3,5,1), limits = c(-3,5))+
  xlab("Number of divorces")+
  ylab("Fertility gap")+
  theme(text = element_text(size=14,  family="serif"),
        plot.title = element_markdown(hjust = 0.5, size = 16),
        plot.caption = element_markdown(hjust = 0),
        legend.position = "bottom",
        legend.title = ggtext::element_markdown(hjust = 0.5),
        legend.title.position = "bottom")

ggsave("./plots/gap_divorce.png", dpi = 600, height = 150, width = 159.2, units = "mm")


## FERTILITY GAP AND AGE AT FIRST COHABITATION

plot_data %>% 
  mutate(age_fc = case_when(
    age_cohabited < 20 ~ "-20",
    age_cohabited < 25 & age_cohabited >= 20 ~ "20-25",
    age_cohabited < 30 & age_cohabited >= 25 ~ "25-30",
    age_cohabited < 35 & age_cohabited >= 30 ~ "30-35",
    age_cohabited >= 35  ~ "35-",
    is.na(age_cohabited) ~ "Never cohabited"
  )) %>% 
  filter(age_cohabited != "Never cohabited") %>% 
  count(age_fc, fert_gap) %>% 
  group_by(age_fc) %>% # remove this part to go from relative to absolute
  mutate(share = n/sum(n)) %>% 
  ungroup() %>% 
  ggplot(aes(x = age_fc, y = fert_gap, size = share), colour = "gray15")+
  theme_classic()+
  geom_point()+
  geom_text(aes(label = ifelse(fert_gap == 0, round(share, digits = 2), "")), colour = "white",
            family = "serif", size = 5, fontface = 2)+
  scale_size_continuous(range = c(1, 20), name="Share of women within age group",
                        breaks = seq(0, 1, length.out = 6))+ 
  scale_y_continuous(breaks = seq(-3,5,1), limits = c(-3,5))+
  xlab("Age at first cohabitation")+
  ylab("Fertility gap")+
  theme(text = element_text(size=14,  family="serif"),
        plot.title = element_markdown(hjust = 0.5, size = 16),
        plot.caption = element_markdown(hjust = 0),
        legend.position = "bottom",
        legend.title = ggtext::element_markdown(hjust = 0.5),
        legend.title.position = "bottom")

ggsave("./plots/gap_first_cohab.png", dpi = 600, height = 150, width = 159.2, units = "mm")


## INTENDED FAMILY SIZE AND COMPLETED FERTILITY BY EDUCATION

## Modifying data for plot

summ <- 
  plot_data %>% 
  group_by(intended_children, N, education_factor) %>% 
  summarise(n_group = n()) %>% 
  group_by(education_factor) %>% 
  mutate(sum = sum(n_group),
         prop = n_group / sum, 
         tot = sum(sum)) %>% 
  ungroup() %>% 
  mutate(diagonal = fifelse(intended_children == N, 1, 0))

ggplot(summ[order(summ$diagonal), ],
       aes(x = intended_children, y = N, group = education_factor, fill = prop)) +
  theme_classic()+
  geom_tile(aes(colour = as.factor(diagonal)), size = 1.2)+
  scale_color_manual(guide = "none", values = c("transparent", "white"))+
  geom_text(aes(label = round(100*prop)), colour = "white",
            family = "serif", size = 6, fontface = 2) +
  scale_fill_viridis_c(option = "H") +
  facet_wrap(~education_factor) +
  coord_fixed() +
  scale_x_continuous(breaks = 0:8, limits = c(-0.5, 5.5)) +
  scale_y_continuous(breaks = 0:8, limits = c(-0.5, 7.5)) +
  theme(legend.position = "none") +
  labs(x = "Intended family size",
       y = "Number of children")+
  theme(text = element_text(size = 14, family="serif"),
        plot.title = element_markdown(hjust = 0.5, size = 16),
        plot.caption = element_markdown(hjust = 0),
        plot.margin = unit(c(0,0,0,0), "cm"))

ggsave("./plots/intended_family_size.png", dpi = 600, height = 150, width = 159.2, units = "mm")

#####









#### TABLES FOR PARAMETER ADJUSTMENTS ####





## Fertility gap (table 2)

fert_gap_param_t <- 
  data.table(parameter = 
               c("Fecundability",
                 "Intrauterine mortality",
                 "Age at first cohabitation",
                 "Transition time from<br>cohabitation to marriage",
                 "Time spent finding new partner",
                 "Share of cohabitations<br>ending in separation",
                 "Share of marriages<br>ending in divorce",
                 "Share who marry",
                 "Share who re-partner",
                 "Share who ever cohabit"),
             adjustment = 
               c("No decline until sterility",
                 "Fixed at the level of 20 year-old",
                 "Set to age 15",
                 "Set to 1 month",
                 "Set to 1 month",
                 "Set to 0%",
                 "Set to 0%",
                 "Set to 100% ",
                 "Set to 100%",
                 "Set to 100%"), 
             change = 
               c("No 12.5 year<br>linear decline",
                 "No cubic increase",
                 "-8.66 years",
                 "-35 months (avg)",
                 "-29 months (avg)",
                 "-31.45 %-points",
                 "-25.59 %-points",
                 "+42.51 %-points",
                 "+26.59 %-points",
                 "+4.97 %-points"),
             contribution =
               c(-0.0904, -0.01513, -0.09315, 0.02763, -0.03625,
                 -0.18002, -0.03593, -0.16175, -0.12797,
                 -0.03786),
             contribution_pct = 
               c(-0.0904, -0.01513, -0.09315, 0.02763, -0.03625,
                 -0.18002, -0.03593, -0.16175, -0.12797,
                 -0.03786)/0.3422478*100)%>% 
  dplyr::mutate_if(is.numeric, signif, 3) %>% 
  mutate(contribution = cell_spec(contribution, color = case_when(
    contribution < 0 ~ "darkred",
    contribution > 0  ~ "darkgreen",
    .default = "black"
  )),
  contribution_pct = cell_spec(contribution_pct, color = case_when(
    contribution_pct < 0 ~ "darkred",
    contribution_pct > 0  ~ "darkgreen",
    .default = "black"
  )))



knitr::kable(fert_gap_param_t,
             align = 'c', format = "html",
             escape = FALSE,
             col.names = c(
               "Parameter",
               "Adjustment",
               "Change in parameter",
               "Contribution to fertility gap",
               "Contribution in percent"
             )) %>%
  kable_classic(html_font = "serif") %>%
  kable_styling(font_size = 25) %>%
  save_kable(file = "./plots/fert_gap_param_t_7mar24.png",
             zoom = 3,
             bs_theme = "flatly",
             density = 300)

## Education data table (Table 3)

edu_parameter_adj_t <- 
  data.table(par =
               c("Fertility gap",
                 "Age at first cohabitation",
                 "Share who ever cohabit (%)",
                 "Transition time to marriage (years)",
                 "Share of cohabitations resulting in marriage (%)",
                 "Share of cohabitations resulting in separation (%)",
                 "Share of marriages resulting in divorce (%)",
                 "Share of union dissolutions resulting in re-partnering (%)",
                 "Age at graduation (enrolled until age)"),
             high = 
               c("0.41","25.43", "93.91", "4.32", "51.98", "25.17", "24.77", "72.55", "22"),
             low = 
               c("0.24","22.33", "99.85", "5.09", "64.25", "30.40", "30.76", "74.34", "18"),
             contribution = 
               c(0.17433, 0.1168, 0.07425 , -0.01236, 0.07141, -0.04097, -0.01155, 0.00139, 0.00384),
             contribution_pct =
               c(0.17433, 0.1168, 0.07425 , -0.01236, 0.07141, -0.04097, -0.01155, 0.00139, 0.00384)/0.17433*100#,
             # contribution_cumulative =
             #   c(0.17433, 0.11438, 0.15982, 0.15829, 0.2281, 0.17975, 0.16136, 0.17241, 0.18046)/0.17433*100 
)%>% 
  dplyr::mutate_if(is.numeric, round, 2) %>%
  mutate(contribution = cell_spec(contribution, color = case_when(
    contribution < 0 ~ "darkred",
    contribution > 0 & contribution != round(0.17433, 2) ~ "darkgreen",
    .default = "black"
  )),
  contribution_pct = cell_spec(contribution_pct,
                               color = case_when(
    contribution_pct < 0 ~ "darkred",
    contribution_pct > 0 & contribution_pct != 100 ~ "darkgreen",
    .default = "black"
  )))




knitr::kable(edu_parameter_adj_t,
             align = 'c', format = "html",
             escape = FALSE,
             col.names = c(
               "Parameter",
               "High education<br>(ISCED 5-7)",
               "Low education<br>(ISCED 0-2)",
               "Individual contribution to difference<br>in the fertility gap",
               "Individual contribution in percent"#,
#               "Cumulative contribution in percent"
             )) %>%
  kable_classic(html_font = "serif") %>%
  kable_styling(font_size = 25) %>%
  row_spec(1,bold=T,hline_after = FALSE) %>%
  save_kable(file = "./plots/edu_diff_param_adj_7mar24.png",
             zoom = 3,
             bs_theme = "flatly",
             density = 300)

## Counterfactual future adjustments (Table 4)


future_adjustments <- data.table(
  parameters = c(
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
  adjustment = c(
    "25-34 year-olds in 2022*",
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
  contribution = c(
    0.01773, 0.02366, 0.01421, 0.02576, 0.02815, 0.0761, 0.03887, 0.07799,
    0.12118, 0.17162, 0.22558),
  contribution_pct = 
    c(0.01773, 0.02366, 0.01421, 0.02576, 0.02815, 0.0761, 0.03887, 0.07799,
      0.12118, 0.17162, 0.22558)/0.3422478*100) %>% 
  dplyr::mutate_if(is.numeric, round, 2) %>% 
  mutate(contribution = cell_spec(contribution, color = case_when(
    contribution < 0 ~ "darkred",
    contribution > 0  ~ "darkgreen",
    .default = "black"
  )),
  contribution_pct = cell_spec(contribution_pct, color = case_when(
    contribution_pct < 0 ~ "darkred",
    contribution_pct > 0  ~ "darkgreen",
    .default = "black"
  ))) 
  



knitr::kable(future_adjustments,
             align = 'c', format = "html", escape = FALSE,
             col.names = c(
               "Parameter",
               "Adjustment",
               "Contribution to fertility gap",
               "Contribution in percent"
             )) %>%
  kable_styling(font_size = 26) %>%
  kable_classic(html_font = "serif", full_width = TRUE) %>%
  gsub("font-size: initial !important;",
       "font-size: 26pt !important;",
       .) %>%
  save_kable(file = "./plots/parameter_contributions_7mar24.png",
             zoom = 3,
             bs_theme = "flatly",
             density = 300)



#####


#### Results from postponing first cohabitation using base model ####

# Data

postponement_10y <- 
data.table(year = seq(0,10, by = 1),
             fert_gap = c(0.3422478, 0.3811178, 0.4202378, 0.4634278, 0.5138678, 0.5678278,
                          0.6274278, 0.7061678, 0.7747078, 0.8635178, 0.9589278))

# Linear regression line based on first three observations
lm_fit <- lm(fert_gap ~ year, data = postponement_10y[1:3])


#Plotting 

postponement_10y %>% 
  ggplot(aes(x = year, y = fert_gap))+
  theme_classic()+
  geom_point(colour = "gray15", size = 3)+
  geom_smooth(linewidth = 1.2, se = FALSE, colour = cb_palette[6])+
  geom_abline(intercept = lm_fit$coefficients[1], slope = lm_fit$coefficients[2],
              colour = cb_palette[2], linewidth = 1.2, linetype = "dashed")+
  scale_x_continuous(breaks = seq(0,10,1), limits = c(0,10))+
  scale_y_continuous(breaks = seq(0.3,1,0.1), limits = c(0.3,1))+
  xlab("Years of postponement of first cohabitation")+
  ylab("Fertility gap")+
  theme(text = element_text(size=14,  family="serif"),
        plot.title = element_markdown(hjust = 0.5, size = 16),
        plot.caption = element_markdown(hjust = 0),
        legend.position = "bottom")

ggsave("./plots/postponement_10y_plot.png", dpi = 600, height = 150, width = 159.2, units = "mm")


##### 





#### FIGURE A5 ####

# Y axis labels

name1 <- c("Share who do NOT separate",
           "Share who marry",
           "Share who re-partner",
           "Share who ever cohabit",
           "Share who do NOT divorce")

# Contribution to fertility gap

contr1 <- c(-0.18002, -0.16175, -0.12797, -0.03786, -0.03593)/0.3422478*100

# Percentage point changes in parameters

change1 <- c(31.45, 42.51, 26.64, 4.98, 25.98)


# Plot data

plot_data2 <- data.frame(name = name1, 
                         value = contr1/change1)
# Plotting

plot_data2 %>% 
  ggplot(aes(y = fct_reorder(name, value, .desc = TRUE), x = value))+
  scale_y_discrete(labels = label_wrap(30))+
  scale_x_continuous(limits = c(-3,0), breaks = seq(-3,0,by = 0.5))+
  theme_classic()+
  geom_col(fill = cb_palette[3])+
  xlab("Percentage change in fertility gap per percentage
       point increase in parameter")+
  ylab("")+
  theme(text = element_text(size=14,  family="serif"),
        legend.position = "bottom")+
  theme(legend.title = ggtext::element_markdown())+
  guides(size = guide_legend(title.position = "bottom", title.hjust = 0.5))

# Saving as png

ggsave("./plots/relative_importance.png", width = 159.2, height = 130,
       units = "mm", dpi = 600)



## Social media plot (same as above, but different dimensions)

plot_data2 %>% 
  ggplot(aes(y = fct_reorder(name, value, .desc = TRUE), x = value))+
  scale_y_discrete(labels = label_wrap(10))+
  scale_x_continuous(limits = c(-3,0), breaks = seq(-3,0,by = 0.5))+
  theme_classic()+
  geom_col(fill = cb_palette[3])+
  xlab("% change in fertility gap per\n %-point increase in parameter")+
  ylab("")+
  labs(title = "Relative contributions of\n each union-related parameter")+
  theme(text = element_text(family="serif", size = 10),
        legend.position = "bottom",
        legend.title = ggtext::element_markdown(),
        legend.title.position = "plot",
        plot.title = element_text(hjust = 0.5),
        plot.title.position = "plot")+
  guides(size = guide_legend(title.position = "bottom", title.hjust = 0.5))

# Saving as png

ggsave("./plots/Social_Media_6622.png", width = 788, height = 940,
       units = "px")








#####



#### EXTRA ####



#### INTENDED FAMILY SIZE TABLE ####

# Loading required data

behavioural_inputs <- readRDS("./simulation/dutch_simulation_inputs.RDS")

# Mean intended family size (for calculating the fertility gap)

mean_intend_fam <- sum(behavioural_inputs$fert_int_both*c(0,1,2,3,4,5))


# Producing the table for intended family size and some of the inputs
# not included in the data summary

sum_hypo <- data.table("Indicator" =
                         c("Mean intended family size (number of children)",
                           "Intended 0 children (%)", "Intended 1 child (%)", "Intended 2 children (%)",
                           "Intended 3 children (%)", "Intended 4+ children (%)"),
                       "Values" = formatC(signif(c(mean_intend_fam, behavioural_inputs$fert_int_both[1]*100,
                                                   behavioural_inputs$fert_int_both[2]*100,
                                                   behavioural_inputs$fert_int_both[3]*100,
                                                   behavioural_inputs$fert_int_both[4]*100,
                                                   (behavioural_inputs$fert_int_both[5]+behavioural_inputs$fert_int_both[6])*100),
                                                 digits = 3),
                                          digits = 3,format="fg", flag="#"))


# Exporting the table as a .PNG
# 
# knitr::kable(sum_hypo,
#              align = 'c',
#              format = "html") %>%
#   kable_styling(font_size = 26) %>%
#   kable_classic(html_font = "serif", full_width = TRUE) %>%
#   gsub("font-size: initial !important;",
#        "font-size: 26pt !important;",
#        .) %>%
#   save_kable(file = "./plots/intended_family_size.png",
#              zoom = 5,
#              bs_theme = "flatly",
#              density = 300)

#####

