# Script name: 00_POPULATION_p3-7-11.R
# 
# Author: M.Chen, Inserm
#
# Doing: Prepare the covariates data set from phase 3 (1991-1994), 7 (2002-2004), 11 (2012-2013):
#   - socio-demographic variables (sex, age, ethnicity, marital status, last occupational position), 
#   - lifestyle variables (smoking status, alcohol intake, fruits & vegetable consumption),
#   - general health indicators (BMI, mental and physical component score, 
#     multimorbidity index including stroke, CHD, HF, cancers, depression, dementia, Parkinson, COPD, hypertension, diabetes and arthritis)

# Data imputation:
#   - if missing data in phase 3,  imputation with phase 1
#   - if missing data in phase 7,  imputation with phase 5
#--------------------------------------------------------------------
# Packages
# > Tools
library(tidyr)
library(plyr)
library(dplyr)
library(haven)
library(purrr)
library(lubridate)
library(vcd) # kappa

#--------------------------------------------------------------------
# Sample participants (n = 4006)
data <- read_dta("E:\\PC_FIXE\\Data\\04_DAILY_ACT_SUM\\2020-05-15\\WW_L40M100V400_update052020.dta") %>%
  filter(exclusion == 0) %>% 
  select(stno)

sample_stno <- unique(data$stno)

#--------------------------------------------------------------------
# Socio-demographics
tab_scd <- read_dta("E:\\PC_FIXE\\Data\\03_SCALAR_VAR\\data_L40M100_052020_forMathide.dta") %>%
  filter(stno %in% sample_stno)

# Data containing dates of health issues 
data2 <- read_dta("E:\\PC_FIXE\\Data\\03_SCALAR_VAR\\data_L40M100_052020_forMathide.dta") %>% 
  filter(stno %in% sample_stno)

# Additional data on all participants 
particip <- read_dta("E:\\PC_FIXE\\Data\\05_WHITEHALL\\particip.dta") %>% 
  filter(stno %in% sample_stno)

# ////////////////// PHASE 3 \\\\\\\\\\\\\\\\\\

#--------------------------------------------------------------------
# Data from screening and questionnaires
# > phase 3 
tab_s3 <- read_dta("E:\\PC_FIXE\\Data\\05_WHITEHALL\\s3quest.dta") %>%
  left_join(read_dta("E:\\PC_FIXE\\Data\\05_WHITEHALL\\s3screen.dta"),
            by = "stno") %>% 
  filter(stno %in% sample_stno)

# > phase 1 (for imputation)
tab_s1 <- read_dta("E:\\PC_FIXE\\Data\\05_WHITEHALL\\s1quest.dta") %>%
  left_join(read_dta("E:\\PC_FIXE\\Data\\05_WHITEHALL\\s1screen.dta"),
            by = "stno") %>% 
  filter(stno %in% sample_stno)

#--------------------------------------------------------------------
# Preparing data from phase 3 and 1 as long tables to ease imputation process (11 > 9, 7 > 5 > 9 if not used for 11)

# Covariables from phase 3
datas3 <- tab_s3 %>% 
  #filter(stno %in% unique(data$stno)) %>% 
  select(stno, xstatusx, xgrlump,
         xesmoke, xunitwk0, xfruitvg, xmodhrs, xvighrs,
         xbmi, xsbp, xdbp, xantihyp
         #xmcs, xpcs, xwaist1 
  ) %>%
  gather(key = "variable", value = "xvalue", -stno) %>% 
  mutate(variable = substring(variable, 2))

# Covariables from phase 5
datas1 <- tab_s1 %>%
  filter(stno %in% unique(data$stno)) %>% 
  select(stno, statusx, grlump,
         esmoke, unitwk0, fruitveg, modhrs, vighrs,
         bmi, sbp, dbp, antihyp 
         #mcs, pcs, waist1 # do not exist in phase 1
  ) %>%
  gather(key = "variable", value = "value", -stno) 

# Merge all data
# > data phase 3
data_temp <- datas3 %>% 
  # > data phase 1
  left_join(datas1, by = c("stno", "variable")) 

#--------------------------------------------------------------------
# Covariates imputation
# > Checking for missing data in phase 7
# > Missing data in phase 7 are imputed using data from phase 5

data_imp <- data_temp %>% 
  # Imputation
  mutate(xvalue_i = if_else(is.na(xvalue) == F, xvalue, value)) %>% 
  select(-xvalue, -value) %>% 
  # Data formating: Get one column for each imputed variable 
  pivot_wider_spec(
    # > specification for name of the columns
    tibble(
      .name = c(paste0("x", unique(data_temp$variable), "_i")),
      .value = c(rep("xvalue_i", 11)),
      variable = unique(data_temp$variable))
  )

#--------------------------------------------------------------------
# Building covariates sets

cov_main_3 <- tab_s3 %>% 
  # Age at screening date
  select(stno, xage_s) %>% 
  # Impute age at phase 3 using dates of screening and age at phase 11
  left_join(particip %>% select(stno, xphdate, fdatscrn), by = "stno") %>%
  left_join(tab_scd %>% select(stno, fage_s), by = "stno") %>%
  mutate(xphdate_i = if_else(is.na(xphdate) == F, xphdate, min(particip$xphdate, na.rm = T)),
         xage_s_i = if_else(is.na(xage_s) == F, xage_s, fage_s - time_length(fdatscrn - xphdate_i, "years"))) %>% 
  select(-xage_s, -fage_s, -xphdate, -xphdate_i, -fdatscrn) %>% 
  # Add other socio-demographics 
  left_join(tab_scd %>% select(stno, sex, ethnicity, edu_imp, edu_conti), by = "stno") %>%
  # Add imputed variables 
  left_join(data_imp, by = "stno") %>% 
  # Add other variables
  left_join(tab_s3 %>% select(stno, xmcs, xpcs, xwaist1), by = "stno") %>% 
  mutate(
    
    # Socio-demographics
    # > sex (being a woman = ref)
    sex_2 = if_else(sex == 2, 0, 1),
    # > ethnicity (ref = white)
    ethnicity_i = if_else(ethnicity == 1, 0, 1),
    # > marital status (ref = married, cohabitating)
    xstatusx_i_2 = if_else(xstatusx_i == 1, 0, 1),
    # > last occupational position (ref = administrative + prof/executive)
    xgrlump_i_2 = if_else(xgrlump_i %in% c(1,2), 0, 1),
    # > last occupational position (ordinal)
    xgrlump_i_ordinal = if_else(xgrlump_i == 1, 0,
                                if_else(xgrlump_i == 2, 0.5, 
                                        1)),
    # > education (ref = high)
    edu_2_a = if_else(edu_imp %in% c(4,5), 0, 1),
    # > education (ref = high + intermediate)
    edu_2_b = if_else(edu_imp %in% c(3,4,5), 0, 1),
    
    # ---------------------------------------------------
    # Lifestyle
    # > smoking status (never-smoker = ref)
    ex_smoker      = if_else(xesmoke_i == 2, 1, 0),
    current_smoker = if_else(xesmoke_i == 3, 1, 0),
    
    # > alcohol intake (3 categories)
    xunitwk0_i_3 = if_else(xunitwk0_i == 0, 0, 
                           if_else(xunitwk0_i > 0 & xunitwk0_i <= 14, 1, 
                                   2)),
    # > alcohol intake (moderate drinker = ref)
    alc_0       = if_else(xunitwk0_i == 0, 1, 0), # no drinkers
    alc_more_14 = if_else(xunitwk0_i > 14, 1, 0), # heavy drinkers
    
    # > fruits and vegetable consumption (3 categories)
    xfruitvg_i_3 = if_else(xfruitvg_i < 7, 0, if_else(xfruitvg_i == 7, 1, 2)),
    # > fruits & vegetables intake (more than daily = ref)
    fg_not_daily = if_else(xfruitvg_i_3 == 0, 1, 0), # less than daily
    fg_daily     = if_else(xfruitvg_i_3 == 1, 1, 0), # daily
    # > fruits & vegetables intake (twice daily and more = ref)
    fg_2         = if_else(xfruitvg_i <= 7, 1, 0),
    
    # For sensitivity analyses
    # > self-reported hours of MVPA (continuous variable)
    mvpa_hrs_cont = xmodhrs_i + xvighrs_i,
    # > self-reported hours of MVPA (3 categories)
    mvpa_hrs = if_else(mvpa_hrs_cont == 0, 0, 
                       if_else(mvpa_hrs_cont > 0 & mvpa_hrs_cont < 2.5, 1, 
                               2)),
    # > self-reported hours of MVPA (more than 2.5 hours = ref)
    mvpa_inactive  = if_else(mvpa_hrs == 0, 1, 0), # inactive participants
    mvpa_less_2.5  = if_else(mvpa_hrs == 1, 1, 0), # active participants, but not filling mvpa guidelines
    
    # ---------------------------------------------------
    # Cardiometabolic risk factors
    # > BMI (3 categories)
    xbmi_i_3 = if_else(xbmi_i < 25.0, 0,
                       if_else(xbmi_i >= 25.0 & xbmi_i < 30.0, 1, 2)),
    # > BMI (normal = ref)
    bmi_overweight = if_else(xbmi_i >= 25.0 & xbmi_i < 30.0, 1, 0), # overweight
    bmi_obese = if_else(xbmi_i >= 30.0, 1, 0), # obeses
    
    # > Waist circunference (3 categories)
    xwaist_i_3 = if_else(sex == 1, 
                         if_else(xwaist1 < 94, 0, if_else(xwaist1 >= 94 & xwaist1 < 102, 1, 2)),
                         if_else(xwaist1 < 80, 0, if_else(xwaist1 >= 80 & xwaist1 < 88, 1, 2))),
    
    # > Hypertension
    hypertension = if_else(xsbp_i >= 140 | xdbp_i >= 90 | xantihyp_i == 1, 1, 0),
    
    # ---------------------------------------------------
    # Health-related risk factors
    # mental component score (per 10 points decrement)
    mental_condition   = -(xmcs - 50)/10,
    # physical component score (per 10 points increase)
    physical_condition = -(xpcs - 50)/10)

#--------------------------------------------------------------------
# Compute multimorbidity index (dementia, cancers, arthritis, COPD, depression and Parkinson)
# (prevalence at phase 7)

# Identify prevalent cases of:
tab_prev_cases_3 <- data2 %>% 
  left_join(particip %>% select(stno, xphdate), 
            by = "stno") %>% 
  select(stno, xphdate,
         # > dementia
         demence, "d_demence" = demence_date,
         # > cancers
         b_c2allc, allcancers_2, d_allcancers_2,
         # > arthritis (osteoarthritis + rheumatoid arthritis)
         arthritis, d_arthritis, rhearthritis, d_rhearthritis,
         # > Chronic Obstructive Pulmonary Disease (COPD)
         copd_hes, d_copd_hes,
         # > depression
         depression, d_depression,
         # > parkinson
         parkinson, "d_parkinson" = parkinson_date,
         # > stroke
         stroke, "d_stroke" = end_stroke,
         # > chd
         chd, "d_chd" = end_chd,
         # > hf
         "hf" = hfailure_hes, "d_hf" = end_hf,
         # > diabetes
         diabetes, d_diabetes
  ) %>% 
  # Impute date of participation
  mutate(xphdate_i = if_else(is.na(xphdate) == F, xphdate, min(particip$xphdate, na.rm = T))) %>% 
  # Create binary variables for each disease (prevalence at phase 11)
  mutate(
    prev_demence  = if_else(demence      == 1 & d_demence      <= xphdate_i, 1, 0),
    prev_cancers  = if_else(allcancers_2 == 1 & d_allcancers_2 <= xphdate_i | 
                              b_c2allc     == 1, 1, 0),  # some cancers were diagnosed before s1 
    prev_arthrit  = if_else(arthritis    == 1 & d_arthritis    <= xphdate_i | 
                              rhearthritis == 1 & d_rhearthritis <= xphdate_i, 1, 0),
    prev_copd     = if_else(copd_hes     == 1 & d_copd_hes     <= xphdate_i, 1, 0),
    prev_depress  = if_else(depression   == 1 & d_depression   <= xphdate_i, 1, 0),
    prev_parkins  = if_else(parkinson    == 1 & d_parkinson    <= xphdate_i, 1, 0),
    prev_stroke   = if_else(stroke       == 1 & d_stroke       <= xphdate_i, 1, 0),
    prev_chd      = if_else(chd          == 1 & d_chd          <= xphdate_i, 1, 0),
    prev_hf       = if_else(hf           == 1 & d_hf           <= xphdate_i, 1, 0),
    prev_diabetes = if_else(diabetes     == 1 & d_diabetes     <= xphdate_i, 1, 0)) %>% 
  select(stno, starts_with("prev_")) %>%
  # replace NA by 0 in the prev_ covariables
  gather(key = "disease", value = "prev_case", starts_with("prev_")) %>% 
  mutate(prev_case = if_else(is.na(prev_case) == T, 0, prev_case)) %>% 
  spread(key = "disease", value = "prev_case")

#--------------------------------------------------------------------
# Merge tables

tab_3 <- cov_main_3 %>% 
  left_join(tab_prev_cases_3, by = "stno") %>%
  # Compute MMI score
  group_by(stno) %>%
  mutate(
    mmm_index = sum(prev_arthrit, 
                    prev_cancers, 
                    prev_copd, 
                    prev_demence, 
                    prev_depress, 
                    prev_parkins, 
                    prev_chd, prev_stroke, prev_hf,
                    prev_diabetes,
                    hypertension,
                    na.rm = T)
  )

#--------------------------------------------------------------------
# Drop rows with missing covariables

tab_3_fin <- tab_3 %>% 
  drop_na(sex_2, xage_s_i, ethnicity_i, xstatusx_i_2, xgrlump_i_ordinal, 
          ex_smoker, current_smoker, alc_0, alc_more_14, fg_2, 
          bmi_overweight, bmi_obese, 
          mental_condition, physical_condition, mmm_index)

# > Data with final covariates set
nrow(tab_3_fin)

# check if no NAs 
tab_3_fin %>% 
  select(sex_2, xage_s_i, ethnicity_i, xstatusx_i_2, xgrlump_i_ordinal, 
         ex_smoker, current_smoker, alc_0, alc_more_14, fg_2, 
         bmi_overweight, bmi_obese, 
         mental_condition, physical_condition, mmm_index) %>% 
  gather(key = "variable", value = "value", -stno) %>%
  filter(is.na(value) == T) %>% 
  group_by(variable) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n))

#--------------------------------------------------------------------
# > Save all covariables table
save(tab_3, file = "E:\\PC_FIXE\\Analysis\\01_ARTICLE_1\\05_FINAL_ANALYSES_FIGURES_2\\PHASE_3\\Data\\tab_full_cov_s3.rda")
# > Save data without NA
save(tab_3_fin, file = "E:\\PC_FIXE\\Analysis\\01_ARTICLE_1\\05_FINAL_ANALYSES_FIGURES_2\\PHASE_3\\Data\\tab_cov_s3.rda")

# ////////////////// PHASE 7 \\\\\\\\\\\\\\\\\\

#--------------------------------------------------------------------
# Data from screening and questionnaires
# > phase 7 
tab_s7 <- read_dta("E:\\PC_FIXE\\Data\\05_WHITEHALL\\s7quest.dta") %>%
  left_join(read_dta("E:\\PC_FIXE\\Data\\05_WHITEHALL\\s7screen.dta"),
            by = "stno") %>% 
  filter(stno %in% sample_stno)

tab_s7_cov_imp <- read_dta("E:\\PC_FIXE\\Data\\05_WHITEHALL\\s7_covariates_imputation.dta") %>%
  filter(stno %in% sample_stno)

# > phase 5 (for imputation)
tab_s5 <- read_dta("E:\\PC_FIXE\\Data\\05_WHITEHALL\\s5quest.dta") %>%
  left_join(read_dta("E:\\PC_FIXE\\Data\\05_WHITEHALL\\s5screen.dta"),
            by = "stno") %>% 
  filter(stno %in% sample_stno)

#--------------------------------------------------------------------
# Preparing data from phase 7 and phase 5 as long tables to ease imputation process (11 > 9, 7 > 5 > 9 if not used for 11)

# Covariables from phase 7
datas7 <- tab_s7 %>% 
  #filter(stno %in% unique(data$stno)) %>% 
  select(stno, mstatusx, mlgrlump,
         mesmoke, munitwk0, mfruitvg, mmodhr_s, mvighr_s,
         mbmi, msbp, mdbp, mantihyp, 
         mmcs, mpcs, 
         mwaist1) %>%
  gather(key = "variable", value = "mvalue", -stno) %>% 
  mutate(variable = substring(variable, 2))

# Covariables from phase 5
datas5 <- tab_s5 %>%
  filter(stno %in% unique(data$stno)) %>% 
  select(stno, tstatusx, tlgrlump,
         tesmoke, tunitwk0, tfruitvg, tmodhr_s, tvighr_s,
         tbmi, tsbp, tdbp, tantihyp, 
         tmcs, tpcs, 
         twaist1) %>%
  gather(key = "variable", value = "tvalue", -stno) %>% 
  mutate(variable = substring(variable, 2))

# Merge all data
# > data phase 7
data_temp <- datas7 %>% 
  # > data phase 5
  left_join(datas5, by = c("stno", "variable")) 

#--------------------------------------------------------------------
# Covariates imputation
# > Checking for missing data in phase 7
# > Missing data in phase 7 are imputed using data from phase 5

data_imp <- data_temp %>% 
  # Imputation
  mutate(mvalue_i = if_else(is.na(mvalue) == F, mvalue, tvalue)) %>% 
  select(-mvalue, -tvalue) %>% 
  # Data formating: Get one column for each imputed variable 
  pivot_wider_spec(
    # > specification for name of the columns
    tibble(
      .name = c(paste0("m", unique(data_temp$variable), "_i")),
      .value = c(rep("mvalue_i", 14)),
      variable = unique(data_temp$variable))
  )

#--------------------------------------------------------------------
# Building covariates sets

cov_main_7 <- tab_s7 %>% 
  # Age at screening date
  select(stno, mage_s) %>% 
  # Impute age at phase 7 using dates of screening and age at phase 11
  left_join(particip %>% select(stno, mphdate, fdatscrn), by = "stno") %>%
  left_join(tab_scd %>% select(stno, fage_s), by = "stno") %>%
  mutate(mphdate_i = if_else(is.na(mphdate) == F, mphdate, min(particip$mphdate, na.rm = T)),
         mage_s_i = if_else(is.na(mage_s) == F, mage_s, fage_s - time_length(fdatscrn - mphdate_i, "years"))) %>% 
  select(-mage_s, -fage_s, -mphdate, -mphdate_i, -fdatscrn) %>% 
  # Add other socio-demographics 
  left_join(tab_scd %>% select(stno, sex, ethnicity, edu_imp, edu_conti), by = "stno") %>%
  # Add imputed variables 
  left_join(data_imp, by = "stno") %>% 
  mutate(
    
    # Socio-demographics
    # > sex (being a woman = ref)
    sex_2 = if_else(sex == 2, 0, 1),
    # > ethnicity (ref = white)
    ethnicity_i = if_else(ethnicity == 1, 0, 1),
    # > marital status (ref = married, cohabitating)
    mstatusx_i_2 = if_else(mstatusx_i == 1, 0, 1),
    # > last occupational position (ref = administrative + prof/executive)
    mlgrlump_i_2 = if_else(mlgrlump_i %in% c(1,2), 0, 1),
    # > last occupational position (ordinal)
    mlgrlump_i_ordinal = if_else(mlgrlump_i == 1, 0,
                                 if_else(mlgrlump_i == 2, 0.5, 
                                         1)),
    # > education (ref = high)
    edu_2_a = if_else(edu_imp %in% c(4,5), 0, 1),
    # > education (ref = high + intermediate)
    edu_2_b = if_else(edu_imp %in% c(3,4,5), 0, 1),
    
    # ---------------------------------------------------
    # Lifestyle
    # > smoking status (never-smoker = ref)
    ex_smoker      = if_else(mesmoke_i == 2, 1, 0),
    current_smoker = if_else(mesmoke_i == 3, 1, 0),
    
    # > alcohol intake (3 categories)
    munitwk0_i_3 = if_else(munitwk0_i == 0, 0, 
                           if_else(munitwk0_i > 0 & munitwk0_i <= 14, 1, 
                                   2)),
    # > alcohol intake (moderate drinker = ref)
    alc_0       = if_else(munitwk0_i == 0, 1, 0), # no drinkers
    alc_more_14 = if_else(munitwk0_i > 14, 1, 0), # heavy drinkers
    
    # > fruits and vegetable consumption (3 categories)
    mfruitvg_i_3 = if_else(mfruitvg_i < 7, 0, if_else(mfruitvg_i == 7, 1, 2)),
    # > fruits & vegetables intake (more than daily = ref)
    fg_not_daily = if_else(mfruitvg_i_3 == 0, 1, 0), # less than daily
    fg_daily     = if_else(mfruitvg_i_3 == 1, 1, 0), # daily
    # > fruits & vegetables intake (twice daily and more = ref)
    fg_2         = if_else(mfruitvg_i <= 7, 1, 0),
    
    # For sensitivity analyses
    # > self-reported hours of MVPA (continuous variable)
    mvpa_hrs_cont = mmodhr_s_i + mvighr_s_i,
    # > self-reported hours of MVPA (3 categories)
    mvpa_hrs = if_else(mvpa_hrs_cont == 0, 0, 
                       if_else(mvpa_hrs_cont > 0 & mvpa_hrs_cont < 2.5, 1, 
                               2)),
    # > self-reported hours of MVPA (more than 2.5 hours = ref)
    mvpa_inactive  = if_else(mvpa_hrs == 0, 1, 0), # inactive participants
    mvpa_less_2.5  = if_else(mvpa_hrs == 1, 1, 0), # active participants, but not filling mvpa guidelines
    
    # ---------------------------------------------------
    # Cardiometabolic risk factors
    # > BMI (3 categories)
    mbmi_i_3 = if_else(mbmi_i < 25.0, 0,
                       if_else(mbmi_i >= 25.0 & mbmi_i < 30.0, 1, 2)),
    # > BMI (normal = ref)
    bmi_overweight = if_else(mbmi_i >= 25.0 & mbmi_i < 30.0, 1, 0), # overweight
    bmi_obese = if_else(mbmi_i >= 30.0, 1, 0), # obeses
    
    # > Waist circunference (3 categories)
    mwaist_i_3 = if_else(sex == 1, 
                         if_else(mwaist1_i < 94, 0, if_else(mwaist1_i >= 94 & mwaist1_i < 102, 1, 2)),
                         if_else(mwaist1_i < 80, 0, if_else(mwaist1_i >= 80 & mwaist1_i < 88, 1, 2))),
    
    # > Hypertension
    hypertension = if_else(msbp_i >= 140 | mdbp_i >= 90 | mantihyp_i == 1, 1, 0),
    
    # ---------------------------------------------------
    # Health-related risk factors
    # mental component score (per 10 points decrement)
    mental_condition   = -(mmcs_i - 50)/10,
    # physical component score (per 10 points increase)
    physical_condition = -(mpcs_i - 50)/10)

#--------------------------------------------------------------------
# Compute multimorbidity index (dementia, cancers, arthritis, COPD, depression and Parkinson)
# (prevalence at phase 7)

# Identify prevalent cases of:
tab_prev_cases_7 <- data2 %>% 
  left_join(particip %>% select(stno, mphdate), 
            by = "stno") %>% 
  select(stno, mphdate,
         # > dementia
         demence, "d_demence" = demence_date,
         # > cancers
         b_c2allc, allcancers_2, d_allcancers_2,
         # > arthritis (osteoarthritis + rheumatoid arthritis)
         arthritis, d_arthritis, rhearthritis, d_rhearthritis,
         # > Chronic Obstructive Pulmonary Disease (COPD)
         copd_hes, d_copd_hes,
         # > depression
         depression, d_depression,
         # > parkinson
         parkinson, "d_parkinson" = parkinson_date,
         # > stroke
         stroke, "d_stroke" = end_stroke,
         # > chd
         chd, "d_chd" = end_chd,
         # > hf
         "hf" = hfailure_hes, "d_hf" = end_hf,
         # > diabetes
         diabetes, d_diabetes
  ) %>% 
  # Impute date of participation
  mutate(mphdate_i = if_else(is.na(mphdate) == F, mphdate, min(particip$mphdate, na.rm = T))) %>% 
  # Create binary variables for each disease (prevalence at phase 11)
  mutate(
    prev_demence  = if_else(demence      == 1 & d_demence      <= mphdate_i, 1, 0),
    prev_cancers  = if_else(allcancers_2 == 1 & d_allcancers_2 <= mphdate_i | 
                              b_c2allc     == 1, 1, 0),  # some cancers were diagnosed before s1 
    prev_arthrit  = if_else(arthritis    == 1 & d_arthritis    <= mphdate_i | 
                              rhearthritis == 1 & d_rhearthritis <= mphdate_i, 1, 0),
    prev_copd     = if_else(copd_hes     == 1 & d_copd_hes     <= mphdate_i, 1, 0),
    prev_depress  = if_else(depression   == 1 & d_depression   <= mphdate_i, 1, 0),
    prev_parkins  = if_else(parkinson    == 1 & d_parkinson    <= mphdate_i, 1, 0),
    prev_stroke   = if_else(stroke       == 1 & d_stroke       <= mphdate_i, 1, 0),
    prev_chd      = if_else(chd          == 1 & d_chd          <= mphdate_i, 1, 0),
    prev_hf       = if_else(hf           == 1 & d_hf           <= mphdate_i, 1, 0),
    prev_diabetes = if_else(diabetes     == 1 & d_diabetes     <= mphdate_i, 1, 0)) %>% 
  select(stno, starts_with("prev_")) %>%
  # replace NA by 0 in the prev_ covariables
  gather(key = "disease", value = "prev_case", starts_with("prev_")) %>% 
  mutate(prev_case = if_else(is.na(prev_case) == T, 0, prev_case)) %>% 
  spread(key = "disease", value = "prev_case")

#--------------------------------------------------------------------
# Merge tables

tab_7 <- cov_main_7 %>% 
  left_join(tab_prev_cases_7, by = "stno") %>%
  # Compute MMI score
  group_by(stno) %>%
  mutate(
    mmm_index = sum(prev_arthrit, 
                    prev_cancers, 
                    prev_copd, 
                    prev_demence, 
                    prev_depress, 
                    prev_parkins, 
                    prev_chd, prev_stroke, prev_hf,
                    prev_diabetes,
                    hypertension,
                    na.rm = T)
  )

#--------------------------------------------------------------------
# Drop rows with missing covariables

tab_7_fin <- tab_7 %>% 
  drop_na(sex_2, mage_s_i, ethnicity_i, mstatusx_i_2, mlgrlump_i_ordinal, 
          ex_smoker, current_smoker, alc_0, alc_more_14, fg_2, 
          bmi_overweight, bmi_obese, 
          mental_condition, physical_condition, mmm_index)

#--------------------------------------------------------------------
# > Save all covariables table
save(tab_7, file = "E:\\PC_FIXE\\Analysis\\01_ARTICLE_1\\05_FINAL_ANALYSES_FIGURES_2\\PHASE_7\\Data\\tab_full_cov_s7.rda")

# > Save data without NA
save(tab_7_fin, file = "E:\\PC_FIXE\\Analysis\\01_ARTICLE_1\\05_FINAL_ANALYSES_FIGURES_2\\PHASE_7\\Data\\tab_cov_s7.rda")

# ////////////////// PHASE 11 \\\\\\\\\\\\\\\\\\

#--------------------------------------------------------------------
# Data from screening and questionnaires
# > phase 11 
tab_s11 <- read_dta("E:\\PC_FIXE\\Data\\05_WHITEHALL\\s11quest.dta") %>%
  left_join(read.csv("E:\\PC_FIXE\\Data\\05_WHITEHALL\\S11screen_txt.csv"),
            by = "stno") %>% 
  filter(stno %in% sample_stno)

# > phase 9 (for imputation) 
tab_s9 <- read_dta("E:\\PC_FIXE\\Data\\05_WHITEHALL\\s9quest.dta") %>% 
  left_join(read_dta("E:\\PC_FIXE\\Data\\05_WHITEHALL\\s9screen.dta"),
            by = "stno") %>% 
  filter(stno %in% sample_stno)

#--------------------------------------------------------------------
# Preparing data from phase 11 as long tables to ease imputation process (11 > 9)

# Covariables from phase 11
datas11 <- tab_s11 %>% 
  select(stno, fstatusx,
         fesmoke, funitwk0, ffruitvg, fmodhr_s, fvighr_s, fspapubf, fspavstf,
         fbmi, fsbp, fdbp, fdbp, fantihyp, fblchol, flipdrg, fldl, fhdl,
         fmcs, fpcs, 
         fwaist1) %>%
  gather(key = "variable", value = "fvalue", -stno) %>% 
  mutate(variable = substring(variable, 2))

# Covariables from phase 9
datas9 <- tab_s9 %>%
  select(stno, jstatusx,
         jesmoke, junitwk0, jfruitvg, jmodhr_s, jvighr_s, jspapubf, jspavstf,
         jbmi, jsbp, jdbp, jdbp, jantihyp, jblchol, jlipdrg, jldl, jhdl,
         jmcs, jpcs, 
         jwaist1) %>%
  gather(key = "variable", value = "jvalue", -stno) %>% 
  mutate(variable = substring(variable, 2))

# Merge all data
# > data phase 11
data_temp <- datas11 %>% 
  # > data phase 9 
  left_join(datas9, by = c("stno", "variable"))

#--------------------------------------------------------------------
# Covariates imputation
# > Checking for missing data in phase 11
# > Missing data in phase 11 are imputed using data from phase 9

data_imp <- data_temp %>% 
  # Imputation
  mutate(fvalue_i = if_else(is.na(fvalue) == F, fvalue, jvalue)) %>% 
  select(-fvalue, -jvalue) %>%
  # Data formating: Get one column for each imputed variable 
  pivot_wider_spec(
    # > specification for name of the columns
    tibble(
      .name = c(paste0("f", unique(data_temp$variable), "_i")),
      .value = c(rep("fvalue_i", 19)),
      variable = unique(data_temp$variable))
  )

#--------------------------------------------------------------------
# Building covariates sets

cov_main_11 <- tab_scd %>% 
  select(stno, fage_s, sex, ethnicity, edu_imp, edu_conti, flgrlump_i) %>%
  # Add imputed variables 
  left_join(data_imp, by = "stno") %>% 
  mutate(
    
    # Socio-demographics
    # > sex (being a woman = ref)
    sex_2 = if_else(sex == 2, 0, 1),
    # > ethnicity (ref = white)
    ethnicity_i = if_else(ethnicity == 1, 0, 1),
    # > marital status (ref = married, cohabitating)
    fstatusx_i_2 = if_else(fstatusx_i == 1, 0, 1),
    # > last occupational position (ref = administrative + prof/executive)
    flgrlump_i_2 = if_else(flgrlump_i %in% c(1,2), 0, 1),
    # > last occupational position (ordinal)
    flgrlump_i_ordinal = if_else(flgrlump_i == 1, 0,
                                 if_else(flgrlump_i == 2, 0.5, 
                                         1)),
    # > education (ref = high)
    edu_2_a = if_else(edu_imp %in% c(4,5), 0, 1),
    # > education (ref = high + intermediate)
    edu_2_b = if_else(edu_imp %in% c(3,4,5), 0, 1),
    
    # ---------------------------------------------------
    # Lifestyle
    # > smoking status (never-smoker = ref)
    ex_smoker = if_else(fesmoke_i == 2, 1, 0),
    current_smoker = if_else(fesmoke_i == 3, 1, 0),
    
    # > alcohol intake (3 categories)
    funitwk0_i_3 = if_else(funitwk0_i == 0, 0, 
                           if_else(funitwk0_i > 0 & funitwk0_i <= 14, 1, 
                                   2)),
    # > alcohol intake (moderate drinker = ref)
    alc_0       = if_else(funitwk0_i == 0, 1, 0), # no drinkers
    alc_more_14 = if_else(funitwk0_i > 14, 1, 0), # heavy drinkers
    
    # > fruits and vegetable consumption (3 categories)
    ffruitvg_i_3 = if_else(ffruitvg_i < 7, 0, if_else(ffruitvg_i == 7, 1, 2)),
    # > fruits & vegetables intake (more than daily = ref)
    fg_not_daily = if_else(ffruitvg_i_3 == 0, 1, 0), # less than daily
    fg_daily     = if_else(ffruitvg_i_3 == 1, 1, 0), # daily
    # > fruits & vegetables intake (twice daily and more = ref)
    fg_2         = if_else(ffruitvg_i <= 7, 1, 0),
    
    # For posthoc analyses
    # > self-reported hours of MVPA (continuous variable)
    mvpa_hrs_cont = fmodhr_s_i + fvighr_s_i,
    # > self-reported hours of MVPA (3 categories)
    mvpa_hrs = if_else(mvpa_hrs_cont == 0, 0, 
                       if_else(mvpa_hrs_cont > 0 & mvpa_hrs_cont < 2.5, 1, 
                               2)),
    # > self-reported hours of MVPA (more than 2.5 hours = ref)
    mvpa_inactive  = if_else(mvpa_hrs == 0, 1, 0), # inactive participants
    mvpa_less_2.5  = if_else(mvpa_hrs == 1, 1, 0), # active participants, but not filling mvpa guidelines
    # > social interactions
    club_rev       = 4 - fspapubf_i,
    frd_rev        = 4 - fspavstf_i,
    social_act     = club_rev + frd_rev,
    
    # ---------------------------------------------------
    # Cardiometabolic risk factors
    # > BMI (3 categories)
    fbmi_i_3 = if_else(fbmi_i < 25.0, 0,
                       if_else(fbmi_i >= 25.0 & fbmi_i < 30.0, 1, 2)),
    # > BMI (normal = ref)
    bmi_overweight = if_else(fbmi_i >= 25.0 & fbmi_i < 30.0, 1, 0), # overweight
    bmi_obese = if_else(fbmi_i >= 30.0, 1, 0), # obeses
    
    # > Waist circunference (3 categories)
    fwaist_i_3 = if_else(sex == 1, 
                         if_else(fwaist1_i < 94, 0, if_else(fwaist1_i >= 94 & fwaist1_i < 102, 1, 2)),
                         if_else(fwaist1_i < 80, 0, if_else(fwaist1_i >= 80 & fwaist1_i < 88, 1, 2))),
    
    # > Hypertension
    hypertension = if_else(fsbp_i >= 140 | fdbp_i >= 90 | fantihyp_i == 1, 1, 0),
    
    # ---------------------------------------------------
    # Health-related risk factors
    # mental condition (per 10 points increase)
    mental_condition = -(fmcs_i - 50)/10,
    # physical condition (per 10 points increase)
    physical_condition = -(fpcs_i - 50)/10)

#--------------------------------------------------------------------
# Identify prevalent health issues
tab_prev_cases_11 <- data2 %>%
  select(stno, fdatscrn,
         # > demence
         demence, "d_demence" = demence_date,
         # > cancers
         b_c2allc, allcancers_2, d_allcancers_2,
         # > arthritis (osteoarthritis + rheumatoid arthritis)
         arthritis, d_arthritis, rhearthritis, d_rhearthritis,
         # > Chronic Obstructive Pulmonary Disease (COPD)
         copd_hes, d_copd_hes,
         # > depression
         depression, d_depression,
         # > parkinson
         parkinson, "d_parkinson" = parkinson_date,
         # > stroke
         stroke, "d_stroke" = end_stroke,
         # > chd
         chd, "d_chd" = end_chd,
         # > hf
         "hf" = hfailure_hes, "d_hf" = end_hf,
         # > diabetes
         diabetes, d_diabetes
  ) %>% 
  # Create binary variables for each disease (prevalence at phase 11)
  mutate(
    prev_demence  = if_else(demence      == 1 & d_demence      <= fdatscrn, 1, 0),
    prev_cancers  = if_else(allcancers_2 == 1 & d_allcancers_2 <= fdatscrn | 
                              b_c2allc     == 1, 1, 0),  # some cancers were diagnosed before s1 
    prev_arthrit  = if_else(arthritis    == 1 & d_arthritis    <= fdatscrn | 
                              rhearthritis == 1 & d_rhearthritis <= fdatscrn, 1, 0),
    prev_copd     = if_else(copd_hes     == 1 & d_copd_hes     <= fdatscrn, 1, 0),
    prev_depress  = if_else(depression   == 1 & d_depression   <= fdatscrn, 1, 0),
    prev_parkins  = if_else(parkinson    == 1 & d_parkinson    <= fdatscrn, 1, 0),
    prev_stroke   = if_else(stroke       == 1 & d_stroke       <= fdatscrn, 1, 0),
    prev_chd      = if_else(chd          == 1 & d_chd          <= fdatscrn, 1, 0),
    prev_hf       = if_else(hf           == 1 & d_hf           <= fdatscrn, 1, 0),
    prev_diabetes = if_else(diabetes     == 1 & d_diabetes     <= fdatscrn, 1, 0)) %>% 
  select(stno, starts_with("prev_")) %>%
  # replace NA by 0 in the prev_ covariables
  gather(key = "disease", value = "prev_case", starts_with("prev_")) %>% 
  mutate(prev_case = if_else(is.na(prev_case) == T, 0, prev_case)) %>% 
  spread(key = "disease", value = "prev_case")

#--------------------------------------------------------------------
# Merge 
tab_11 <- cov_main_11 %>% 
  left_join(tab_prev_cases_11, by = "stno") %>%
  # Compute MMI score
  group_by(stno) %>%
  mutate(
    mmm_index = sum(prev_arthrit, 
                    prev_cancers, 
                    prev_copd, 
                    prev_demence, 
                    prev_depress, 
                    prev_parkins, 
                    prev_chd, prev_stroke, prev_hf,
                    prev_diabetes,
                    hypertension,
                    na.rm = T)
  )

#--------------------------------------------------------------------
# Drop rows with missing covariables

tab_11_fin <- tab_11 %>% 
  drop_na(sex_2, fage_s, ethnicity, fstatusx_i_2, edu_imp, flgrlump_i_ordinal,
          fesmoke_i, alc_0, alc_more_14, fg_2, 
          bmi_overweight, bmi_obese,
          mmm_index, fpcs_i, fmcs_i) 

#--------------------------------------------------------------------
# > Save all covariables table
save(tab_11, file = "E:\\PC_FIXE\\Analysis\\01_ARTICLE_1\\05_FINAL_ANALYSES_FIGURES_2\\PHASE_11\\Data\\tab_full_cov_s11.rda")

# > Save data without NA
save(tab_11_fin, file = "E:\\PC_FIXE\\Analysis\\01_ARTICLE_1\\05_FINAL_ANALYSES_FIGURES_2\\PHASE_11\\Data\\tab_cov_s11.rda")

