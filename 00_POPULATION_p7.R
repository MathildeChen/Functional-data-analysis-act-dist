# Script name: 00_POPULATION_p7.R
# 
# Author: M.Chen, Inserm
#
# Doing: Prepare the covariates data set from phase 7 (2002-2004):
#   - socio-demographic variables (sex, age, ethnicity, marital status, last occupational position), 
#   - lifestyle variables (smoking status, alcohol intake, fruits & vegetable consumption),
#   - general health indicators (BMI, mental and physical component score, 
#     multimorbidity index including stroke, CHD, HF, cancers, depression, dementia, Parkinson, COPD, hypertension, diabetes and arthritis)

# Data imputation:
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
# Sample partcipants (n = 4006)
data <- read_dta("E:\\PC_FIXE\\Data\\04_DAILY_ACT_SUM\\2020-05-15\\WW_L40M100V400_update052020.dta") %>%
  filter(exclusion == 0) %>% 
  select(stno)

sample_stno <- unique(data$stno)

#--------------------------------------------------------------------
# Socio-demographics
tab_scd <- read_dta("E:\\PC_FIXE\\Data\\03_SCALAR_VAR\\data_L40M100_052020_forMathide.dta") %>%
  filter(stno %in% sample_stno)

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

# Data containing dates of health issues 
data2 <- read_dta("E:\\PC_FIXE\\Data\\03_SCALAR_VAR\\data_L40M100_052020_forMathide.dta") %>% 
  filter(stno %in% sample_stno)

# Additional data on all participants 
particip <- read_dta("E:\\PC_FIXE\\Data\\05_WHITEHALL\\particip.dta") %>% 
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

# NA values per variable
data_imp %>% 
  gather(key = "variable", value = "value", -stno) %>%
  filter(is.na(value) == T) %>% 
  group_by(variable) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n))

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

# Number of prevalent cases at phase 7 in each diseases for each sex
tab_prev_cases_7 %>% 
  select(stno, starts_with("prev")) %>% 
  left_join(tab_scd %>% select(stno, sex), by = "stno") %>% 
  gather(key = "disease", value = "prev", starts_with("prev")) %>%
  group_by(sex, disease) %>%
  summarise(nb_prev_case = sum(prev, na.rm = T)) %>% 
  spread(key = "sex", value = nb_prev_case)

#disease           `Men``Women`
#1  prev_arthrit    115    77
#2  prev_cancers     81    71
#3  prev_chd        227    71
#4  prev_copd         5     0
#5  prev_demence      1     0
#6  prev_depress    146   106
#7  prev_diabetes   165    57
#8  prev_hf           6     3
#9  prev_parkins      0     0
#10 prev_stroke      17     6

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

table(tab_7$mmm_index, exclude = NULL)
#0    1     2     3    4 
#2183 1291  432   87   13 

#--------------------------------------------------------------------
# > Check / contingency table

# continuous variables
summary(tab_7$mage_s_i) # Mean: 60.25 years
summary(tab_7$mental_condition) # Mean: -0.2184, 143 NAs
summary(tab_7$physical_condition) # Mean: -0.02576, 143 NAs

# categorical and binary variables
tab_7 %>% 
  gather(key = "variable", value = "value", 
         ethnicity_i, edu_imp, edu_2_a, edu_2_b, mlgrlump_i_ordinal, mlgrlump_i_2, mstatusx_i_2,
         mesmoke_i, munitwk0_i_3, mfruitvg_i_3, fg_2, mvpa_inactive, mvpa_less_2.5,
         mbmi_i_3, mwaist_i_3, 
         mmm_index,
         starts_with("prev_")) %>% 
  group_by(sex_2, variable, value) %>% 
  summarise(n = n()) %>%
  mutate(freq = round((n / sum(n))*100,2),
         lab = paste0(n, " (", freq, "%)")) %>% 
  select(-n, -freq) %>%
  spread(key = "sex_2", value = lab)

#1	edu_2_a	0	238 (22.95%)	1003 (33.78%)
#2	edu_2_a	1	799 (77.05%)	1966 (66.22%)

#3	edu_2_b	0	495 (47.73%)	1862 (62.71%)
#4	edu_2_b	1	542 (52.27%)	1107 (37.29%)

#5	edu_imp	1	180 (17.36%)	191 (6.43%)
#6	edu_imp	2	362 (34.91%)	916 (30.85%)
#7	edu_imp	3	257 (24.78%)	859 (28.93%)
#8	edu_imp	4	183 (17.65%)	734 (24.72%)
#9	edu_imp	5	55 (5.3%)	269 (9.06%)

#28	mlgrlump_i	1	265 (25.55%)	1632 (54.97%)
#29	mlgrlump_i	2	488 (47.06%)	1175 (39.58%)
#30	mlgrlump_i	3	258 (24.88%)	102 (3.44%)
#31	mlgrlump_i	NA	26 (2.51%)	60 (2.02%)

#32	mlgrlump_i_2	0	753 (72.61%)	2807 (94.54%)
#33	mlgrlump_i_2	1	284 (27.39%)	162 (5.46%)

#10	ethnicity_i	0	904 (87.17%)	2800 (94.31%)
#11	ethnicity_i	1	132 (12.73%)	165 (5.56%)
#12	ethnicity_i	NA	1 (0.1%)	4 (0.13%)

#39	mstatusx_i_2	0	565 (54.48%)	2417 (81.41%)
#40	mstatusx_i_2	1	439 (42.33%)	470 (15.83%)
#41	mstatusx_i_2	NA	33 (3.18%)	82 (2.76%)

#20	mesmoke_i	1	568 (54.77%)	1390 (46.82%)
#21	mesmoke_i	2	350 (33.75%)	1308 (44.06%)
#22	mesmoke_i	3	80 (7.71%)	186 (6.26%)
#23	mesmoke_i	NA	39 (3.76%)	85 (2.86%)

#24	mfruitvg_i_3	0	194 (18.71%)	726 (24.45%)
#25	mfruitvg_i_3	1	321 (30.95%)	1051 (35.4%)
#26	mfruitvg_i_3	2	485 (46.77%)	1110 (37.39%)
#27	mfruitvg_i_3	NA	37 (3.57%)	82 (2.76%)

#13	fg_2	0	485 (46.77%)	1110 (37.39%)
#14	fg_2	1	515 (49.66%)	1777 (59.85%)
#15	fg_2	NA	37 (3.57%)	82 (2.76%)

#1  mvpa_inactive     0 901 (86.89%) 2729 (91.92%)
#2  mvpa_inactive     1 100 (9.64%)  159 (5.36%)  
#3  mvpa_inactive    NA 36 (3.47%)   81 (2.73%)  

#4  mvpa_less_2.5     0 572 (55.16%) 1935 (65.17%)
#5  mvpa_less_2.5     1 429 (41.37%) 953 (32.1%)  
#6  mvpa_less_2.5    NA 36 (3.47%)   81 (2.73%) 

#16	mbmi_i_3	0	412 (39.73%)	1057 (35.6%)
#17	mbmi_i_3	1	342 (32.98%)	1400 (47.15%)
#18	mbmi_i_3	2	238 (22.95%)	396 (13.34%)
#19	mbmi_i_3	NA	45 (4.34%)	116 (3.91%)

#34	mmm_index	0	520 (50.14%)	1663 (56.01%)
#35	mmm_index	1	346 (33.37%)	945 (31.83%)
#36	mmm_index	2	134 (12.92%)	298 (10.04%)
#37	mmm_index	3	30 (2.89%)	  57 (1.92%)
#38	mmm_index	4	7 (0.68%)	    6 (0.2%)

#42	munitwk0_i_3	0	258 (24.88%)	314 (10.58%)
#43	munitwk0_i_3	1	597 (57.57%)	1496 (50.39%)
#44	munitwk0_i_3	2	138 (13.31%)	1076 (36.24%)
#45	munitwk0_i_3	NA	44 (4.24%)	83 (2.8%)

#46	mwaist_i_3	0	447 (43.11%)	1534 (51.67%)
#47	mwaist_i_3	1	224 (21.6%)	791 (26.64%)
#48	mwaist_i_3	2	316 (30.47%)	518 (17.45%)
#49	mwaist_i_3	NA	50 (4.82%)	126 (4.24%)

#50	prev_arthrit	0	960 (92.57%)	2854 (96.13%)
#51	prev_arthrit	1	77 (7.43%)	115 (3.87%)

#52	prev_cancers	0	966 (93.15%)	2888 (97.27%)
#53	prev_cancers	1	71 (6.85%)	81 (2.73%)

#54	prev_chd	0	966 (93.15%)	2742 (92.35%)
#55	prev_chd	1	71 (6.85%)	227 (7.65%)

#56	prev_copd	0	1037 (100%)	2964 (99.83%)
#57	prev_copd	1	NA	5 (0.17%)

#58	prev_demence	0	1037 (100%)	2968 (99.97%)
#59	prev_demence	1	NA	1 (0.03%)

#60	prev_depress	0	931 (89.78%)	2823 (95.08%)
#61	prev_depress	1	106 (10.22%)	146 (4.92%)

#62	prev_diabetes	0	980 (94.5%)	2804 (94.44%)
#63	prev_diabetes	1	57 (5.5%)	165 (5.56%)

#64	prev_hf	0	1034 (99.71%)	2963 (99.8%)
#65	prev_hf	1	3 (0.29%)	6 (0.2%)

#66	prev_parkins	0	1037 (100%)	2969 (100%)

#67	prev_stroke	0	1031 (99.42%)	2952 (99.43%)
#68	prev_stroke	1	6 (0.58%)	17 (0.57%)

# NA values per variable
tab_7 %>% 
  select(sex_2, mage_s_i, ethnicity_i, mstatusx_i_2, mlgrlump_i_ordinal, 
         ex_smoker, current_smoker, alc_0, alc_more_14, fg_2, mvpa_inactive, mvpa_less_2.5,
         bmi_overweight, bmi_obese, 
         mental_condition, physical_condition, mmm_index) %>% 
  gather(key = "variable", value = "value", -stno) %>%
  filter(is.na(value) == T) %>% 
  group_by(variable) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n))

# variable                  n
#1 bmi_obese               161
#2 bmi_overweight          161
#3 mental_condition        143
#4 physical_condition      143
#5 alc_0                   127
#6 alc_more_14             127
#7 current_smoker          124
#8 ex_smoker               124
#9 fg_2                    119
#  mvpa_inactive           117
#  mvpa_less_2.5           117
#10 mstatusx_i_2           115
#11 mlgrlump_i_ordinal     86
#12 ethnicity_i            5

#-------------------------------------------------------------------
# Check correlations between similar continous variables
# > Mental conditions, physical conditions, mmm index
cor(tab_7$physical_condition, tab_7$mental_condition, "complete.obs")
#  -0.005
cor(tab_7$physical_condition, tab_7$mmm_index, "complete.obs")
# -0.28
cor(tab_7$mmm_index, tab_7$mental_condition, "complete.obs")
# 0.04

# Check agreement between similar categorical variables
# > BMI and WC
Kappa(table(tab_7$mbmi_i_3, tab_7$mwaist_i_3))
agreementplot(t(table(tab_7$mbmi_i_3, tab_7$mwaist_i_3)))
# Unweighted K=0.5654, p < 2.2e-16

# > Education, last grade
chisq.test(tab_7$edu_imp, tab_7$mlgrlump_i)
# p-value < 2.2e-16
# U shape PA in education categories, so we choose last occupational grade

#--------------------------------------------------------------------
# Drop rows with missing covariables

tab_7_fin <- tab_7 %>% 
  drop_na(sex_2, mage_s_i, ethnicity_i, mstatusx_i_2, mlgrlump_i_ordinal, 
          ex_smoker, current_smoker, alc_0, alc_more_14, fg_2, 
          bmi_overweight, bmi_obese, 
          mental_condition, physical_condition, mmm_index)

# > Data with final covariates set
nrow(tab_7_fin)

# check if no NAs 
tab_7_fin %>% 
  select(stno, sex_2, mage_s_i, ethnicity_i, mstatusx_i_2, mlgrlump_i_ordinal, 
         ex_smoker, current_smoker, alc_0, alc_more_14, fg_2, 
         bmi_overweight, bmi_obese, 
         mental_condition, physical_condition, mmm_index) %>% 
  gather(key = "variable", value = "value", -stno) %>%
  filter(is.na(value) == T) %>% 
  group_by(variable) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n))

stop()

#--------------------------------------------------------------------
# > Save all covariables table
save(tab_7, file = "E:\\PC_FIXE\\Analysis\\01_ARTICLE_1\\05_FINAL_ANALYSES_FIGURES_2\\PHASE_7\\Data\\tab_full_cov_s7.rda")

# > Save data without NA
save(tab_7_fin, file = "E:\\PC_FIXE\\Analysis\\01_ARTICLE_1\\05_FINAL_ANALYSES_FIGURES_2\\PHASE_7\\Data\\tab_cov_s7.rda")








