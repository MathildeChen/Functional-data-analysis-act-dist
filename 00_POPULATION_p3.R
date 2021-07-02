# Script name: 00_POPULATION_p3.R
# 
# Author: M.Chen, Inserm
#
# Doing: Prepare the covariates data set from phase 3 (1991-1994):
#   - socio-demographic variables (sex, age, ethnicity, marital status, last occupational position), 
#   - lifestyle variables (smoking status, alcohol intake, fruits & vegetable consumption),
#   - general health indicators (BMI, mental and physical component score, 
#     multimorbidity index including stroke, CHD, HF, cancers, depression, dementia, Parkinson, COPD, hypertension, diabetes and arthritis)

# Data imputation:
#   - if missing data in phase 3,  imputation with phase 1
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

# Data containing dates of health issues 
data2 <- read_dta("E:\\PC_FIXE\\Data\\03_SCALAR_VAR\\data_L40M100_052020_forMathide.dta") %>% 
  filter(stno %in% sample_stno)

# Additional data on all participants 
particip <- read_dta("E:\\PC_FIXE\\Data\\05_WHITEHALL\\particip.dta") %>% 
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

# NA values per variable
data_imp %>% 
  gather(key = "variable", value = "value", -stno) %>%
  filter(is.na(value) == T) %>% 
  group_by(variable) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n))

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

# Number of prevalent cases at phase 3 in each diseases for each sex
tab_prev_cases_3 %>% 
  select(stno, starts_with("prev")) %>% 
  left_join(tab_scd %>% select(stno, sex), by = "stno") %>% 
  gather(key = "disease", value = "prev", starts_with("prev")) %>%
  group_by(sex, disease) %>%
  summarise(nb_prev_case = sum(prev, na.rm = T)) %>% 
  spread(key = "sex", value = nb_prev_case)

#disease           `Men``Women`
#1  prev_arthrit      0     0
#2  prev_cancers     19    17
#3  prev_chd         39    13
#4  prev_copd         0     0
#5  prev_demence      0     0
#6  prev_depress     34    27
#7  prev_diabetes    33    11
#8  prev_hf           0     0
#9  prev_parkins      0     0
#10 prev_stroke       1     0

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

table(tab_3$mmm_index, exclude = NULL)

#--------------------------------------------------------------------
# > Check / contingency table

# continuous variables
summary(tab_3$xage_s_i) # Mean: 49.1 years
summary(tab_3$mental_condition) # Mean: -0.09985, 188 NAs
summary(tab_3$physical_condition) # Mean: 0.28378, 188 NAs

# categorical and binary variables
tab_3 %>% 
  gather(key = "variable", value = "value", 
         ethnicity_i, edu_imp, edu_2_a, edu_2_b, xgrlump_i_ordinal, xgrlump_i_2, xstatusx_i_2,
         xesmoke_i, xunitwk0_i_3, xfruitvg_i_3, fg_2, mvpa_inactive, mvpa_less_2.5,
         xbmi_i_3, xwaist_i_3, 
         mmm_index,
         starts_with("prev_")) %>% 
  group_by(sex_2, variable, value) %>% 
  summarise(n = n()) %>%
  mutate(freq = round((n / sum(n))*100,2),
         lab = paste0(n, " (", freq, "%)")) %>% 
  select(-n, -freq) %>%
  spread(key = "sex_2", value = lab)

               # Women        # Men  
#1	edu_2_a	0.0	238 (22.95%)	1003 (33.78%)
#2	edu_2_a	1.0	799 (77.05%)	1966 (66.22%)

#3	edu_2_b	0.0	495 (47.73%)	1862 (62.71%)
#4	edu_2_b	1.0	542 (52.27%)	1107 (37.29%)

#5	edu_imp	1.0	180 (17.36%)	191 (6.43%)
#6	edu_imp	2.0	362 (34.91%)	916 (30.85%)
#7	edu_imp	3.0	257 (24.78%)	859 (28.93%)
#8	edu_imp	4.0	183 (17.65%)	734 (24.72%)
#9	edu_imp	5.0	55 (5.3%)	269 (9.06%)

#10	ethnicity_i	0.0	904 (87.17%)	2800 (94.31%)
#11	ethnicity_i	1.0	132 (12.73%)	165 (5.56%)
#12	ethnicity_i	NA	1 (0.1%)	4 (0.13%)

#52	xgrlump_i_2	0.0	715 (68.95%)	2850 (95.99%)
#53	xgrlump_i_2	1.0	322 (31.05%)	119 (4.01%)

#54	xgrlump_i_ordinal	0.0	217 (20.93%)	1494 (50.32%)
#55	xgrlump_i_ordinal	0.5	498 (48.02%)	1356 (45.67%)
#56	xgrlump_i_ordinal	1.0	322 (31.05%)	119 (4.01%)

#57	xstatusx_i_2	0.0	638 (61.52%)	2489 (83.83%)
#58	xstatusx_i_2	1.0	398 (38.38%)	480 (16.17%)
#59	xstatusx_i_2	NA	1 (0.1%)	NA

#44	xesmoke_i	1.0	606 (58.44%)	1511 (50.89%)
#45	xesmoke_i	2.0	306 (29.51%)	1164 (39.21%)
#46	xesmoke_i	3.0	122 (11.76%)	290 (9.77%)
#47	xesmoke_i	NA	3 (0.29%)	4 (0.13%)

#60	xunitwk0_i_3	0.0	270 (26.04%)	373 (12.56%)
#61	xunitwk0_i_3	1.0	659 (63.55%)	1694 (57.06%)
#62	xunitwk0_i_3	2.0	107 (10.32%)	901 (30.35%)
#63	xunitwk0_i_3	NA	1 (0.1%)	1 (0.03%)

#48	xfruitvg_i_3	0.0	306 (29.51%)	1062 (35.77%)
#49	xfruitvg_i_3	1.0	386 (37.22%)	1202 (40.49%)
#50	xfruitvg_i_3	2.0	298 (28.74%)	566 (19.06%)
#51	xfruitvg_i_3	NA	47 (4.53%)	139 (4.68%)

#13	fg_2	0.0	298 (28.74%)	566 (19.06%)
#14	fg_2	1.0	692 (66.73%)	2264 (76.25%)
#15	fg_2	NA	47 (4.53%)	139 (4.68%)

#20	mvpa_inactive	0.0	745 (71.84%)	2709 (91.24%)
#21	mvpa_inactive	1.0	289 (27.87%)	250 (8.42%)
#22	mvpa_inactive	NA	3 (0.29%)	10 (0.34%)

#23	mvpa_less_2.5	0.0	655 (63.16%)	1956 (65.88%)
#24	mvpa_less_2.5	1.0	379 (36.55%)	1003 (33.78%)
#25	mvpa_less_2.5	NA	3 (0.29%)	10 (0.34%)

#41	xbmi_i_3	0.0	580 (55.93%)	1670 (56.25%)
#42	xbmi_i_3	1.0	329 (31.73%)	1135 (38.23%)
#43	xbmi_i_3	2.0	128 (12.34%)	164 (5.52%)

#64	xwaist_i_3	0.0	702 (67.7%)	2205 (74.27%)
#65	xwaist_i_3	1.0	147 (14.18%)	389 (13.1%)
#66	xwaist_i_3	2.0	112 (10.8%)	148 (4.98%)
#67	xwaist_i_3	NA	76 (7.33%)	227 (7.65%)

#16	mmm_index	0.0	849 (81.87%)	2329 (78.44%)
#17	mmm_index	1.0	173 (16.68%)	601 (20.24%)
#18	mmm_index	2.0	14 (1.35%)	37 (1.25%)
#19	mmm_index	3.0	1 (0.1%)	2 (0.07%)

#26	prev_arthrit	0.0	1037 (100%)	2969 (100%)

#27	prev_cancers	0.0	1020 (98.36%)	2950 (99.36%)
#28	prev_cancers	1.0	17 (1.64%)	19 (0.64%)

#29	prev_chd	0.0	1024 (98.75%)	2930 (98.69%)
#30	prev_chd	1.0	13 (1.25%)	39 (1.31%)

#31	prev_copd	0.0	1037 (100%)	2969 (100%)

#32	prev_demence	0.0	1037 (100%)	2969 (100%)

#33	prev_depress	0.0	1010 (97.4%)	2935 (98.85%)
#34	prev_depress	1.0	27 (2.6%)	34 (1.15%)

#35	prev_diabetes	0.0	1026 (98.94%)	2936 (98.89%)
#36	prev_diabetes	1.0	11 (1.06%)	33 (1.11%)

#37	prev_hf	0.0	1037 (100%)	2969 (100%)

#38	prev_parkins	0.0	1037 (100%)	2969 (100%)

#39	prev_stroke	0.0	1037 (100%)	2968 (99.97%)
#40	prev_stroke	1.0	NA	1 (0.03%)

# NA values per variable
tab_3 %>% 
  select(sex_2, xage_s_i, ethnicity_i, xstatusx_i_2, xgrlump_i_ordinal, 
         ex_smoker, current_smoker, alc_0, alc_more_14, fg_2, 
         bmi_overweight, bmi_obese, 
         mental_condition, physical_condition, mmm_index) %>% 
  gather(key = "variable", value = "value", -stno) %>%
  filter(is.na(value) == T) %>% 
  group_by(variable) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n))

# variable              n
#1 mental_condition     188
#2 physical_condition   188
#3 fg_2                 186
#4 current_smoker         7
#5 ex_smoker              7
#6 ethnicity_i            5
#7 alc_0                  2
#8 alc_more_14            2
#9 xstatusx_i_2           1

#-------------------------------------------------------------------
# Check correlations between similar continous variables
# > Mental conditions, physical conditions, mmm index
cor(tab_3$physical_condition, tab_3$mental_condition, "complete.obs")
#  -0.157
cor(tab_3$physical_condition, tab_3$mmm_index, "complete.obs")
# -0.11
cor(tab_3$mmm_index, tab_3$mental_condition, "complete.obs")
# 0.02

# Check agreement between similar categorical variables
# > BMI and WC
Kappa(table(tab_3$xbmi_i_3, tab_3$xwaist_i_3))
agreementplot(t(table(tab_3$xbmi_i_3, tab_3$xwaist_i_3)))
# Unweighted K=0.4392, p < 2.2e-16

# > Education, last grade
chisq.test(tab_3$edu_imp, tab_3$xgrlump_i)
# p-value < 2.2e-16
# U shape PA in education categories, so we choose last occupational grade

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

stop()

#--------------------------------------------------------------------
# > Save all covariables table
save(tab_3, file = "E:\\PC_FIXE\\Analysis\\01_ARTICLE_1\\05_FINAL_ANALYSES_FIGURES_2\\PHASE_3\\Data\\tab_full_cov_s3.rda")

# > Save data without NA
save(tab_3_fin, file = "E:\\PC_FIXE\\Analysis\\01_ARTICLE_1\\05_FINAL_ANALYSES_FIGURES_2\\PHASE_3\\Data\\tab_cov_s3.rda")








