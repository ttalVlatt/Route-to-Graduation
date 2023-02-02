################################################################################
##
## <PROJ> Transit Stops Paper
## <FILE> Data Building
## <INIT> 7 October 2022
## <AUTH> Matt Capaldi @ttalVlatt
##
################################################################################

## ---------------------------
##' [Libraries]
## ---------------------------

library(tidyverse)

##' ----------------------------
##' [Assigning IPEDS UNITIDs to SHS Dataset]
##'-----------------------------

df_stops <- read_csv(file.path("data", "raw-transit-data.csv"))
## Transit stops data from https://www.shs.foundation/shsf-transit-map
## Available from:
## https://public.tableau.com/app/profile/shsf/viz/TransitAccessibilityatCollegesUniversities/TransitDashboard
## Select the download icon in the bottom right, 
## then hit "data", then "full data", then "download"
## Open and rename this file "raw-transit-data.csv", don't change anything
## needed opening and re-saving (no edits, just renamed) 
## to reformat and read in smoothly

df_ics <- read_csv(file.path("data", "hd2019.csv")) %>%
  select(UNITID, INSTNM, STABBR, LATITUDE, LONGITUD)
## All other data are from IPEDS survey year 2019
## HD2019, IC2019_AY, EFFY2019, EF2019A_DIST, S2019_SIS, F1819_F1A, F1819_F2, SFA1819, OM2019
## Use the _rv revised version for all files except HD2019 and IC2019_AY (no _rv available)
## https://nces.ed.gov/ipeds/datacenter/DataFiles.aspx?year=2019

df_stops <- df_stops %>% unique()
## schools with multiple bus stops within the nearest bin had duplicate records

df <- left_join(df_stops, df_ics,
                by = c("Instnm" = "INSTNM", "Stabbr" = "STABBR"))
## Results in 34 added duplicate records and 41 NA matches that need sorting manually

df_na <- df %>% filter(is.na(UNITID))
df_na$UNITID <- case_when(str_detect(df_na$Instnm, "Dewey University") ~ 449135,
                          str_detect(df_na$Instnm, "Ivy Tech") ~ 150987,
                          str_detect(df_na$Instnm, "Community College of Rhode Island") ~ 150987,
                          str_detect(df_na$Instnm, "Community College of Vermont") ~ 230861)
## Deals with NAs, all expect Dewey University are issues where IPEDS reports
## one campus but transit stop data separates branch campuses.

df <- bind_rows(df %>% filter(!is.na(UNITID)), df_na)
## Joining back the fixed NAs with df without the NA rows

df_dups <- df %>% group_by(Instnm, Stabbr) %>% filter(n()>1)
df_dups <- df_dups %>% filter(round(LONGITUD, 3) == round(Longitude, 3))
## Selecting duplicate schools with same name and state, inverse issue as above
## where more than one campus in IPEDS and same name in SHS. 
## Matched records with Lat/Long (rounded for compatibility)

df <- bind_rows(df %>% group_by(Instnm, Stabbr) %>% filter(!n()>1), df_dups)

## x <- anti_join(df_stops, test, by = "Latitude")
## old test to see which colleges weren't joining (issue was rounding with the Latitude)

write_csv(df, file.path("data", "raw-transit-data-with-IPEDS-IDs.csv"))
## saves a copy with just the IPEDS IDs merged in

##' ----------------------------
##' [Merging In Relevant IPEDS Variables]
##'-----------------------------

##' [Institutional Characteristics]
df_ics <- read_csv(file.path("data", "hd2019.csv")) %>%
  select(UNITID, 
         control = CONTROL, #Public/private
         locale = LOCALE, #Urban/Rural classification
         region = OBEREG, #Region of US
         carnegie = C18BASIC, #Basic Carnegie classification
         student_profile = C18UGPRF, #Carnegie Undergrad Profile (selectivenss)
         commuter_size = C18SZSET, #Carnegie commuting classification
         size_cat = INSTSIZE, #Enrollment size in bands
         fips = FIPS, #State code
         county = COUNTYCD) #County

df <- left_join(df, df_ics, by = "UNITID")

##' [Total Enrollment]
df_enroll <- read_csv(file.path("data", "effy2019_rv.csv")) %>%
  filter(EFFYLEV == 2) %>% 
  ## Undergraduate students (since we are looking at Pell)
  select(UNITID, 
         tot_enroll = EFYTOTLT, #Total enrollment
         fem_enroll = EFYTOTLW, #Female enrollment
         blk_enroll = EFYBKAAT, #Black enrollment
         hsp_enroll = EFYHISPT) #Hispanic enrollment

df <- left_join(df, df_enroll, by = "UNITID")

##' [Financial Aid]
df_finaid <- read_csv(file.path("data", "sfa1819_rv.csv")) %>%
  select(UNITID, 
         per_pell = UPGRNTP, #Percent of undergraduates who are Pell recipients
         avg_pell = UPGRNTA, #Average amount of Pell grant aid awarded
         avg_netp_pub = NPIS412, #Average net price for students below $30k public
         avg_netp_pri = NPT412) %>% #Same figure for private schools
  mutate(avg_netp = coalesce(avg_netp_pub, avg_netp_pri))
  # collapses the separated public and private columns into avg_netp

df <- left_join(df, df_finaid, by = "UNITID")

##' [Finances]

## For public colleges
df_fin_pub <- read_csv(file.path("data", "f1819_f1a_rv.csv")) %>%
  select(UNITID, 
         ins_exp = F1C011, #Instruction exp
         acs_exp = F1C051, #Academic Support exp
         sup_exp = F1C061) #Student Services exp

## For private colleges
df_fin_pri <- read_csv(file.path("data", "f1819_f2_rv.csv"))  %>%
  select(UNITID, 
         ins_exp = F2E011, #Instruction exp
         acs_exp = F2E041, #Academic Support exp
         sup_exp = F2E051) #Student Services exp

## Merge both public and private into original data
df <- left_join(df %>% filter(control == 1), df_fin_pub, by = "UNITID") %>%
  bind_rows(left_join(df %>% filter(control == 2), df_fin_pri, by = "UNITID"))

##'[Faculty Count]
df_fac <- read_csv(file.path("data", "s2019_sis_rv.csv")) %>%
  filter(FACSTAT == 0) %>%
  ## all full time faculty regardless of rank (part time faculty not reported)
  select(UNITID, 
         t_faculty = SISTOTL) #Total full time faculty

df <- left_join(df, df_fac, by = "UNITID")

##'[Tuition]
df_cost <- read_csv(file.path("data", "ic2019_ay.csv")) %>%
  filter(TUITION2 != ".") %>% #Excluding schools who report "." for undergraduate tuition,
  # These are specialist schools (313) that only report medical, chiropractic tuition, etc.
  # Specialist schools are dropped in the cleaning script anyway, but removing these
  # schools here prevents error messages in loading the data
  select(UNITID, 
         tuition = TUITION2, # In-state full-time tuition
         fee = FEE2) #In-state full-time fees

df <- left_join(df, df_cost, by = "UNITID")

##'[Distance Status]
df_dist <- read_csv(file.path("data", "ef2019a_dist_rv.csv")) %>%
  filter(EFDELEV == 2) %>%
  select(UNITID,
         all_enrolled = EFDETOT, # Total enrollment
         mixed_distance = EFDESOM, # Enrollment in both online/in person courses
         no_distance = EFDENON) # Enrollment in all in person courses

df <- left_join(df, df_dist, by = "UNITID")

##'[Pell Outcome Measures]
df_outs <- read_csv(file.path("data", "om2019_rv.csv")) %>%
  filter(OMCHRT == 51) %>%
  # Cohort of Pell Grant Recipients (Full and Part time)
  select(UNITID,
         cohort_size = OMACHRT, #Size of Pell grant cohort
         award8 = OMAWDP8) #Percent awarded anything within 8 years of entering

df <- left_join(df, df_outs, by = "UNITID")

##'[Non-Pell Outcome Measures]
df_outs_np <- read_csv(file.path("data", "om2019_rv.csv")) %>%
  filter(OMCHRT == 52) %>%
  # Cohort of Non-Pell Grant Recipients (Full and Part time)
  select(UNITID,
         award8_np = OMAWDP8) #Percent awarded anything within 8 years of entering

df <- left_join(df, df_outs_np, by = "UNITID")

##' ----------------------------
##' [Data Output]
##'-----------------------------

write_csv(df, file.path("data", "merged-transit-data.csv"))

rm(list = ls()) #Clears all the created dfs

## =============================================================================
## END SCRIPT
################################################################################