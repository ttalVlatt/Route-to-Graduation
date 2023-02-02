################################################################################
##
## <PROJ> Transit Stops Paper
## <FILE> Data Cleaning
## <INIT> 8 October 2022
## <AUTH> Matt Capaldi @ttalVlatt
##
################################################################################

## ---------------------------
##' [Libraries]
## ---------------------------

library(tidyverse)
library(VIM)
library(moments)

##' ----------------------------
##' [Data Input]
##'-----------------------------

if(file.exists(file.path("data", "merged-transit-data.csv"))) {
   df <- read_csv(file.path("data", "merged-transit-data.csv"))
} else {source("01-data-building.R")
  df <- read_csv(file.path("data", "merged-transit-data.csv"))
  }

##' ----------------------------
##' [Variable Transformation]
##'-----------------------------

df$transit_stop <- ifelse(str_detect(df$`Distance Bin`, "0."), 1, 0)
## Transforming the nearest distance bin provided as multi-categorical
## to binary treatment, with within 0.2 and 0.5 miles being 1 else 0
## Aligns with USDOT definition of walking distance (see paper for more details)

df$per_oncampus <- ((df$no_distance + df$mixed_distance)/df$all_enrolled)*100
## Creating percent of students enrolled either partially or fully on campus

df$tnf <- as.numeric(df$tuition) + as.numeric(df$fee)
## creating sum of full-time in-state tuition and fees (avoids multicolinearity)

df$spend_stu <- (df$ins_exp + df$sup_exp + df$acs_exp)/df$tot_enroll
## creating sum of instruction and student services spending (avoids multicolinearity)

df$stu_faculty <- df$tot_enroll/df$t_faculty
## creating ration of students per full time faculty

df$per_fem <- (df$fem_enroll/df$tot_enroll)*100
## percent of enrollment who identified as female

df$per_blk <- (df$blk_enroll/df$tot_enroll)*100
## percent of enrollment who identified as black

df$per_hsp <- (df$hsp_enroll/df$tot_enroll)*100
## percent of enrollment who identified as hispanic

df$locale <- case_when(df$locale %in% c(11,12,13) ~ "Urban",
                       df$locale %in% c(21,22,23) ~ "Suburban",
                       df$locale %in% c(31,32,33) ~ "Town",
                       df$locale %in% c(41,42,43) ~ "Rural")
## collapsing the sub-categories of locale in primary categories
## urban, suburban, town, and rural

df$size_cat <- case_when(df$size_cat == 1 ~ "Below 1,000",
                         df$size_cat == 2 ~ "1,000 - 4,999",
                         df$size_cat == 3 ~ "5,000 - 9,999",
                         df$size_cat == 4 ~ "10,000 - 19,999",
                         df$size_cat == 5 ~ "20,000 or Above")
## Turning the numeric IPEDS size category to same thing but written out (for formatting) 

df$private <- ifelse(df$control == 2, 1, 0)
## Turning non-profit private control into a binary variable

##' ----------------------------
##' [Case Exclusion]
##'-----------------------------

summary(df)

##' [Selecting commuter colleges]
df <- df %>% filter(commuter_size %in% c(1,2,3,4,5,6,9,12,15))
## All size 2 years, and primarily non-residential 4 years
# 1661/3487 

df <- df %>% filter(carnegie %in% c(1:9, 14:23, 33))
## Excluding special focus institutions per Wright-Kim, Kelchen
# 1403/1661

df <- df %>% filter(cohort_size > 50)
## Per Kelchen and Wright_kim excluding schools with less than 50 Pell students
# 1365/1403

df <- df %>% filter(per_oncampus > 50)
## Excluding colleges with >50% students entirely distance education
# 1274/1365

##' [Missing outcome variable]
df <- df %>% filter(!is.na(award8))
# Drops 0 cases, but would drop missing outcome (per Wright-Kim et al.)

summary(df)

##' ----------------------------
##' [Variable Preparation]
##'-----------------------------

# Taking log of variables with skewed distributions, same as Wright-Kim et al.
skewness(df$spend_stu, na.rm = T)
df$log_spend <- log(df$spend_stu)
hist(df$log_spend)
skewness(df$log_spend, na.rm = T)

skewness(df$tnf, na.rm = T)
df$log_tnf <- log(df$tnf)
hist(df$log_tnf)
skewness(df$log_tnf, na.rm = T)

skewness(df$per_blk, na.rm = T)
df$log_blk <- log(df$per_blk + 1)
skewness(df$log_blk, na.rm = T)

skewness(df$per_hsp)
df$log_hsp <- log(df$per_hsp + 1)
skewness(df$log_hsp, na.rm = T)

summary(df)

##' ----------------------------
##' [Imputation of Missing Data]
##'-----------------------------

summary(df)

n <- subset(df, is.na(stu_faculty))
# 16 NAs for imputation
n <- subset(df, is.na(log_spend))
# 16 NAs for imputation (not the same schools as stu/fac ratio)
n <- subset(df, is.na(avg_netp))
# 6 NAs for imputation
n <- subset(df, is.na(log_tnf))
# 2 NAs for imputation (Hallmark and NW Technical College, school UNITID is
# not reported at all (i.e. no entry, not just NAs) in ic2019_ay.csv)

df_imp <- hotdeck(df,
                  variable = c("log_spend", "stu_faculty", "avg_netp", "log_tnf"),
                  domain_var = "private", # Group matches by
                  ord_var = c("carnegie", "student_profile", "per_pell", "avg_pell", "region")) # Order matches by
## Hot deck imputation on 4 variables missing data
## Grouped matches by domain_var
## Identifying suitable donors by ord_var


##' ----------------------------
##' [Data Output]
##'-----------------------------

df_imp <- df_imp %>% select(award8, award8_np,
  transit_stop, locale, private, size_cat, per_pell,
  student_profile, log_blk, log_hsp, per_fem, stu_faculty, 
  avg_netp, avg_pell, per_oncampus, log_spend, log_tnf,
  log_spend_imp, stu_faculty_imp, avg_netp_imp, log_tnf_imp)
## Selecting the cleaned variables needed for analysis


write_csv(df_imp, file.path("data", "cleaned-transit-data.csv"))

rm(list = ls()) #Clears all the created dfs

## =============================================================================
## END SCRIPT
################################################################################