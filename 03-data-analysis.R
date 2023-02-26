################################################################################
##
## <PROJ> Transit Stops Paper
## <FILE> Data Analysis
## <INIT> 9 October 2022
## <AUTH> Matt Capaldi @ttalVlatt
##
################################################################################

## ---------------------------
##' [Libraries]
## ---------------------------

library(tidyverse)
library(estimatr)
library(car)
library(whitestrap)
library(gt)
library(gtsummary)
library(webshot2)
library(sjPlot)

##' ----------------------------
##' [Data Input]
##'-----------------------------

if(file.exists(file.path("data", "cleaned-transit-data.csv"))) {
   df <- read_csv(file.path("data", "cleaned-transit-data.csv"))
} else {source("02-data-cleaning.R")
  df <- read_csv(file.path("data", "cleaned-transit-data.csv"))
  }

##' ----------------------------
##' [Factor Setting]
##'-----------------------------

df <- df %>% mutate(across(
  .cols = c(transit_stop, locale, private, size_cat, student_profile,
            log_spend_imp, stu_faculty_imp, avg_netp_imp, log_tnf_imp),
  .fns = as.factor))
## turning catagorical and binary variables into factors for analysis

df$locale <- relevel(df$locale, ref = "Urban")
## Setting urban as the base level for locale
df$size_cat <- factor(df$size_cat, level = c("Below 1,000", "1,000 - 4,999", "5,000 - 9,999",
                                             "10,000 - 19,999", "20,000 or Above"))
## Reordering the size category factor to correct order

##' ----------------------------
##' [Analysis]
##'-----------------------------

ni_model_formula <- formula(
  award8 ~ transit_stop + locale + award8_np + private + size_cat + per_pell +
    student_profile + log_blk + log_hsp + per_fem + stu_faculty + 
    avg_netp + avg_pell + per_oncampus + log_spend + log_tnf +
    log_spend_imp + stu_faculty_imp + avg_netp_imp + log_tnf_imp)
## forumal for regression without the interaction term

model_formula <- formula(
  award8 ~ transit_stop + locale + award8_np + private + size_cat + per_pell +
      student_profile + log_blk + log_hsp + per_fem + stu_faculty + 
      avg_netp + avg_pell + per_oncampus + log_spend + log_tnf +
      transit_stop*locale +
    log_spend_imp + stu_faculty_imp + avg_netp_imp + log_tnf_imp)
## formula for regression with the interaction term

##' ----------------------------
##' [Robustness Checks]
##'-----------------------------

## creating non-robust regression for the purpose of robustness checks
y <- lm(model_formula,
        data = df)

## Testing variance inflation factor
vif(y,
    type = 'predictor')
## No VIFs > 10, highest is student_profile at 9.1 (7.9 without imputation dummies)

## Run white test for heteroskedasticity
white_test(y)
## Heteroskedasticity detected so use robust standard errors with lm_robust()

##' ----------------------------
##'[Run regression models]
##'-----------------------------

z <- lm_robust(ni_model_formula,
               data =df)
summary(z)
## Using lm_robust to account of heteroskedasticity

x <- lm_robust(model_formula,
               data = df)
summary(x)
## Using lm_robust to account of heteroskedasticity

##' ----------------------------
##' [Print Descriptives Table]
##'-----------------------------

##' [All variables]
df %>% 
  tbl_summary(include = c("award8", "transit_stop", "locale", "award8_np", "private", "size_cat", 
              "per_pell", "student_profile", "log_blk", "log_hsp", "per_fem", "stu_faculty",  
              "avg_netp", "avg_pell", "per_oncampus", "log_spend", "log_tnf"),
              by = transit_stop,
              type = all_continuous() ~ "continuous2",
              statistic = c(all_continuous() ~ c("{mean}",
                                                 "{sd}",
                                                 "{min} to {max}"),
                            all_categorical() ~ "{n} ({p}%)"),
              missing = "ifany",
              missing_text = "NAs",
              label = c(award8 ~ "Pell Completion Rate",
                        locale ~ "Locale",
                        award8_np ~ "Non-Pell Completion Rate",
                        avg_pell ~ "Average Pell Award",
                        log_spend ~ "Log of Instruction & Student Services Spending per Student",
                        private ~ "Private Control",
                        size_cat ~ "IPEDS Institution Size Classification",
                        per_pell ~ "Percent of Student Body Receiving Pell",
                        student_profile ~ "Carnegie: Undergraduate Profile Classification",
                        log_blk ~ "Log of Percentage Black Enrollment",
                        log_hsp ~ "Log of Percentage Hispanic Enrollment",
                        per_fem ~ "Percentage Female Enrollment",
                        stu_faculty ~ "Student to Faculty Ratio",
                        avg_pell ~ "Average Pell Grant Amount",
                        avg_netp ~ "Average Net Price for Families <$30,000",
                        per_oncampus ~ "Percent of Students at Least Partially on Campus",
                        log_tnf ~ "Log of Full Time Tuition & Fees (sticker price)")) %>%
  add_overall() %>%
  bold_labels() %>%
  modify_header(label = "**Variable**",
                stat_0 = "**Overall** <br> n = {N}",
                stat_1 = "No Transit Stop <br> n = {n}",
                stat_2 = "Transit Stop <br> n = {n}") %>%
  modify_footnote(everything() ~ NA) %>%
  modify_caption("***Descriptive Statistics by Treatment (all vars)***") %>%
  as_gt() %>%
  tab_style(style = list(cell_text(weight = "bold"),
                         cell_fill(color = "Grey")),
            locations = cells_body(columns = stat_0)) %>%
  opt_table_font("Times")  %>%
  gtsave("descriptives.rtf", path = file.path("tables-plots"))

##' ----------------------------
##' [Print Regression Table]
##'-----------------------------

##'[No Interaction]
z %>%
  tbl_regression(intercept = T,
                 include = -c(log_spend_imp, stu_faculty_imp, avg_netp_imp, log_tnf_imp),
                 estimate_fun = ~style_sigfig(.x, digits = 2), ##Only necessary to show the small Bs >0
                 label = c(transit_stop ~ "Transit Stop Within 0.5 Miles",
                           locale ~ "Locale",
                           award8_np ~ "Non-Pell Completion Rate",
                           avg_pell ~ "Average Pell Award",
                           log_spend ~ "Log of Instruction & Student Services Spending per Student",
                           private ~ "Private Control",
                           size_cat ~ "IPEDS Institution Size Classification",
                           per_pell ~ "Percent of Student Body Receiving Pell",
                           student_profile ~ "Carnegie: Undergraduate Profile Classification",
                           log_blk ~ "Log of Percentage Black Enrollment",
                           log_hsp ~ "Log of Percentage Hispanic Enrollment",
                           per_fem ~ "Percentage Female Enrollment",
                           stu_faculty ~ "Student to Faculty Ratio",
                           avg_pell ~ "Average Pell Grant Amount",
                           avg_netp ~ "Average Net Price for Families <$30,000",
                           per_oncampus ~ "Percent of Students at Least Partially on Campus",
                           log_tnf ~ "Log of Full Time Tuition & Fees (sticker price)")) %>%
  modify_column_unhide(column = c(std.error)) %>%
  add_glance_source_note(include = c(r.squared, adj.r.squared, nobs)) %>%
  add_significance_stars(hide_ci = F, hide_p = F) %>%
  modify_header(label = "**Variable**",
                estimate = "**β <br> Estimate**",
                std.error = "**β <br> Std. Error**") %>%
  modify_footnote(everything() ~ NA, abbreviation = T) %>%
  modify_caption("**Regression without Interaction**") %>%
  as_gt() %>%
  opt_table_font("Times") %>%
  gtsave("ni-regression.rtf", path = file.path("tables-plots"))

##' [With Interaction]
x %>%
  tbl_regression(intercept = T,
                 include = -c(log_spend_imp, stu_faculty_imp, avg_netp_imp, log_tnf_imp),
                 estimate_fun = ~style_sigfig(.x, digits = 2), ##Only necessary to show the small Bs >0
                 label = c(transit_stop ~ "Transit Stop Within 0.5 Miles",
                           locale ~ "Locale",
                           award8_np ~ "Non-Pell Completion Rate",
                           avg_pell ~ "Average Pell Award",
                           log_spend ~ "Log of Instruction & Student Services Spending per Student",
                           private ~ "Private Control",
                           size_cat ~ "IPEDS Institution Size Classification",
                           per_pell ~ "Percent of Student Body Receiving Pell",
                           student_profile ~ "Carnegie: Undergraduate Profile Classification",
                           log_blk ~ "Log of Percentage Black Enrollment",
                           log_hsp ~ "Log of Percentage Hispanic Enrollment",
                           per_fem ~ "Percentage Female Enrollment",
                           stu_faculty ~ "Student to Faculty Ratio",
                           avg_pell ~ "Average Pell Grant Amount",
                           avg_netp ~ "Average Net Price for Families <$30,000",
                           per_oncampus ~ "Percent of Students at Least Partially on Campus",
                           log_tnf ~ "Log of Full Time Tuition & Fees (sticker price)")) %>%
  modify_column_unhide(column = c(std.error)) %>%
  add_glance_source_note(include = c(r.squared, adj.r.squared, nobs)) %>%
  add_significance_stars(hide_ci = F, hide_p = F) %>%
  modify_header(label = "**Variable**",
                estimate = "**β <br> Estimate**",
                std.error = "**β <br> Std. Error**") %>%
  modify_footnote(everything() ~ NA, abbreviation = T) %>%
  modify_caption("**Regression with Interaction (all vars)**") %>%
  as_gt() %>%
  opt_table_font("Times") %>%
  gtsave("regression.rtf", path = file.path("tables-plots"))

##' ----------------------------
##' [Plot Interaction with sjPlot]
##'-----------------------------

plot_model(x, type = "pred", terms = c("transit_stop", "locale")) +
  scale_color_grey() +
  labs(title = "Plot of the Model Interaction",
       y = "Pell Completion Rate",
       x = "Transit Stop Within 0.5 Miles",
       color = "Locale") +
  theme_minimal() +
  theme(text = element_text("Times"),
        title = element_text(face = "bold"))

ggsave(file.path("tables-plots", "interaction-plot.png"))

## =============================================================================
## END SCRIPT
################################################################################
