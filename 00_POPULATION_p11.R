# Script name: 00_POPULATION_p11.R
# 
# Author: M.Chen, Inserm, 2021
#
# Doing: Prepare the covariates data set from phase 11 (2012-2013):

# Data imputation:
#   - if missing data in phase 11, imputation with phase 9
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
# Sample participants 
# Participants with valid accelerometery data (n = 4006)
sample_stno <- read_dta("E:\\PC_FIXE\\Data\\04_DAILY_ACT_SUM\\2021-05-03\\data_03052021.dta")

sample_stno <- unique(sample_stno$stno)

#--------------------------------------------------------------------
# Socio-demographics
tab_scd <- read_dta("E:\\PC_FIXE\\Data\\03_SCALAR_VAR\\data_L40M100_052020_forMathide.dta") %>%
  filter(stno %in% sample_stno)

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

# Data containing dates of health issues 
data2 <- read_dta("E:\\PC_FIXE\\Data\\03_SCALAR_VAR\\data_L40M100_052020_forMathide.dta") %>% 
  filter(stno %in% sample_stno)

# Additional data on all participants 
particip <- read_dta("E:\\PC_FIXE\\Data\\05_WHITEHALL\\particip.dta") %>% 
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


# NA values per variable
data_imp %>% 
  gather(key = "variable", value = "value", -stno) %>%
  filter(is.na(value) == T) %>% 
  group_by(variable) %>% 
  dplyr::summarise(n = n()) %>% 
  arrange(desc(n))

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

# Number of prevalent cases at phase 11 in each diseases for each sex
tab_prev_cases_11 %>% 
  #filter(exclusion == 0) %>% 
  select(stno, starts_with("prev")) %>% 
  left_join(tab_scd %>% select(stno, sex), by = "stno") %>% 
  gather(key = "disease", value = "prev", starts_with("prev")) %>%
  group_by(sex, disease) %>%
  dplyr::summarise(nb_prev_case = sum(prev, na.rm = T)) %>% 
  spread(key = "sex", value = nb_prev_case)

#   disease        `Men` `Women`
#1  prev_arthrit    342   213
#2  prev_cancers    293   119
#3  prev_chd        470   127
#4  prev_copd        28     9
#5  prev_demence      7     4
#6  prev_depress    238   165
#7  prev_diabetes   362   129
#8  prev_hf          26    10
#9  prev_parkins     14     6
#10 prev_stroke      52    14

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

# > Check / contingency table

# continuous variables
summary(tab_11$fage_s) # Mean: 69.4 years
summary(tab_11$mental_condition) # Mean: -0.38, 24 NAs
summary(tab_11$physical_condition) # Mean: -0.17, 24 NAs

# categorical and binary variables
tab_11 %>% 
  gather(key = "variable", value = "value", 
         ethnicity_i, edu_imp, edu_2_a, edu_2_b, flgrlump_i, flgrlump_i_2, fstatusx_i_2,
         fesmoke_i, funitwk0_i_3, ffruitvg_i_3, fg_2, mvpa_inactive, mvpa_less_2.5, social_act, 
         fbmi_i_3, fwaist_i_3, 
         mmm_index,
         starts_with("prev_")) %>% 
  group_by(sex_2, variable, value) %>% 
  dplyr::summarise(n = n()) %>%
  mutate(freq = round((n / sum(n))*100,2),
         lab = paste0(n, " (", freq, "%)")) %>% 
  select(-n, -freq) %>%
  spread(key = "sex_2", value = lab)

#             Women         Men
#1	edu_2_a	0	238 (22.95%)	1003 (33.78%)
#2	edu_2_a	1	799 (77.05%)	1966 (66.22%)

#3	edu_2_b	0	495 (47.73%)	1862 (62.71%)
#4	edu_2_b	1	542 (52.27%)	1107 (37.29%)

#5	edu_imp	1	180 (17.36%)	191 (6.43%)
#6	edu_imp	2	362 (34.91%)	916 (30.85%)
#7	edu_imp	3	257 (24.78%)	859 (28.93%)
#8	edu_imp	4	183 (17.65%)	734 (24.72%)
#9	edu_imp	5	55 (5.3%)	269 (9.06%)

#10	ethnicity_i	0	904 (87.17%)	2800 (94.31%)
#11	ethnicity_i	1	132 (12.73%)	165 (5.56%)
#12	ethnicity_i	NA	1 (0.1%)	4 (0.13%)

#13	fbmi_i_3	0	420 (40.5%)	1129 (38.03%)
#14	fbmi_i_3	1	351 (33.85%)	1374 (46.28%)
#15	fbmi_i_3	2	265 (25.55%)	463 (15.59%)
#16	fbmi_i_3	NA	1 (0.1%)	3 (0.1%)

#17	fesmoke_i	1	561 (54.1%)	1332 (44.86%)
#18	fesmoke_i	2	402 (38.77%)	1504 (50.66%)
#19	fesmoke_i	3	39 (3.76%)	87 (2.93%)
#20	fesmoke_i	NA	35 (3.38%)	46 (1.55%)

#21	ffruitvg_i_3	0	182 (17.55%)	642 (21.62%)
#22	ffruitvg_i_3	1	169 (16.3%)	684 (23.04%)
#23	ffruitvg_i_3	2	684 (65.96%)	1638 (55.17%)
#24	ffruitvg_i_3	NA	2 (0.19%)	5 (0.17%)

#25	fg_2	0	684 (65.96%)	1638 (55.17%)
#26	fg_2	1	351 (33.85%)	1326 (44.66%)
#   fg_2  NA	2 (0.19%)	  5 (0.17%)

#27	flgrlump_i	1	281 (27.1%)	1685 (56.75%)
#28	flgrlump_i	2	505 (48.7%)	1174 (39.54%)
#29	flgrlump_i	3	251 (24.2%)	110 (3.7%)

#30	flgrlump_i_2	0	786 (75.8%)	2859 (96.3%)
#31	flgrlump_i_2	1	251 (24.2%)	110 (3.7%)

#32	fstatusx_i_2	0	542 (52.27%)	2448 (82.45%)
#33	fstatusx_i_2	1	493 (47.54%)	521 (17.55%)
#34	fstatusx_i_2	NA	2 (0.19%)	NA

#35	funitwk0_i_3	0	349 (33.65%)	467 (15.73%)
#36	funitwk0_i_3	1	589 (56.8%)	1670 (56.25%)
#37	funitwk0_i_3	2	98 (9.45%)	830 (27.96%)
#38	funitwk0_i_3	NA	1 (0.1%)	2 (0.07%)

#39	fwaist_i_3	0	309 (29.8%)	1210 (40.75%)
#40	fwaist_i_3	1	265 (25.55%)	835 (28.12%)
#41	fwaist_i_3	2	462 (44.55%)	922 (31.05%)
#42	fwaist_i_3	NA	1 (0.1%)	2 (0.07%)

#43	mmm_index	0	297 (28.64%)	919 (30.95%)
#44	mmm_index	1	386 (37.22%)	1081 (36.41%)
#45	mmm_index	2	215 (20.73%)	655 (22.06%)
#46	mmm_index	3	100 (9.64%)	242 (8.15%)
#47	mmm_index	4	30 (2.89%)	56 (1.89%)
#48	mmm_index	5	8 (0.77%)	14 (0.47%)
#49	mmm_index	6	1 (0.1%)	1 (0.03%)
#50	mmm_index	7	NA	      1 (0.03%)

#52 mvpa_inactive	0	920 (88.72%)	2752 (92.69%)
#53	mvpa_inactive	1	115 (11.09%)	214 (7.21%)
#54	mvpa_inactive	NA	2 (0.19%)	3 (0.1%)

#55	mvpa_less_2.5	0	587 (56.61%)	1993 (67.13%)
#56	mvpa_less_2.5	1	448 (43.2%)	973 (32.77%)
#57	mvpa_less_2.5	NA	2 (0.19%)	3 (0.1%)

#51	prev_arthrit	0	824 (79.46%)	2627 (88.48%)
#52	prev_arthrit	1	213 (20.54%)	342 (11.52%)

#53	prev_cancers	0	918 (88.52%)	2676 (90.13%)
#54	prev_cancers	1	119 (11.48%)	293 (9.87%)

#55	prev_chd	0	910 (87.75%)	2499 (84.17%)
#56	prev_chd	1	127 (12.25%)	470 (15.83%)

#57	prev_copd	0	1028 (99.13%)	2941 (99.06%)
#58	prev_copd	1	9 (0.87%)	28 (0.94%)

#59	prev_demence	0	1033 (99.61%)	2962 (99.76%)
#60	prev_demence	1	4 (0.39%)	7 (0.24%)

#61	prev_depress	0	872 (84.09%)	2731 (91.98%)
#62	prev_depress	1	165 (15.91%)	238 (8.02%)

#63	prev_diabetes	0	908 (87.56%)	2607 (87.81%)
#64	prev_diabetes	1	129 (12.44%)	362 (12.19%)

#65	prev_hf	0	1027 (99.04%)	2943 (99.12%)
#66	prev_hf	1	10 (0.96%)	26 (0.88%)

#67	prev_parkins	0	1031 (99.42%)	2955 (99.53%)
#68	prev_parkins	1	6 (0.58%)	14 (0.47%)

#69	prev_stroke	0	1023 (98.65%)	2917 (98.25%)
#70	prev_stroke	1	14 (1.35%)	52 (1.75%)

#78 social_act	0	  22 (2.12%)	   32 (1.08%)
#79	social_act	1	 116 (11.19%)	  209 (7.04%)
#80	social_act	2	 176 (16.97%)	  452 (15.22%)
#81	social_act	3	 284 (27.39%)	  681 (22.94%)
#82	social_act	4	 223 (21.5%)	  649 (21.86%)
#83	social_act	5	 145 (13.98%)	  541 (18.22%)
#84	social_act	6	  63 (6.08%)	  394 (13.27%)
#85	social_act	NA	 8 (0.77%)	   11 (0.37%)

#-------------------------------------------------------------------
# Check correlations between similar continous variables
# > Mental conditions, physical conditions, mmm index
cor(tab_11$physical_condition, tab_11$mental_condition, "complete.obs")
#  0.003
cor(tab_11$physical_condition, tab_11$mmm_index, "complete.obs")
# -0.353
cor(tab_11$mmm_index, tab_11$mental_condition, "complete.obs")
# -0.076

# Check agreement between similar categorical variables
# > BMI and WC
Kappa(table(tab_11$fbmi_i_3, tab_11$fwaist_i_3))
agreementplot(t(table(tab_11$fbmi_i_3, tab_11$fwaist_i_3)))
# Unweighted K=0.5114, p < 2.2e-16

# > Education, last grade
chisq.test(tab_11$edu_imp, tab_11$flgrlump_i)
# p-value < 2.2e-16
# --> need to check which model is the best and if the associations are similar

# > Social act between men and women
t.test(tab_11_fin[which(tab_11_fin$sex_2 == 0),]$social_act, tab_11_fin[which(tab_11_fin$sex_2 == 1),]$social_act)

#--------------------------------------------------------------------
# Drop rows with missing covariables

# NA values per variable
tab_11 %>% 
  gather(key = "variable", value = "value", -stno) %>%
  filter(is.na(value) == T) %>% 
  group_by(variable) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n))

# Pre-selected variables
tab_11 %>% 
  select(stno, sex_2, fage_s, ethnicity, fstatusx_i_2, edu_imp, flgrlump_i_ordinal,
         fesmoke_i, alc_0, alc_more_14, fg_2, 
         bmi_overweight, bmi_obese,
         mmm_index, fpcs_i, fmcs_i) %>% 
  gather(key = "variable", value = "value", -stno) %>%
  filter(is.na(value) == T) %>% 
  group_by(variable) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n))

#   variable           n
#1  fesmoke_i         81
#2  fmcs_i            24
#3  fpcs_i            24
#4  fg_2               7
#5  ethnicity          5
#6  bmi_obese          4
#7  bmi_overweight     4
#8  alc_0              3
#9  alc_more_14        3
#10 fstatusx_i_2       2

tab_11_fin <- tab_11 %>% 
  drop_na(sex_2, fage_s, ethnicity, fstatusx_i_2, edu_imp, flgrlump_i_ordinal,
          fesmoke_i, alc_0, alc_more_14, fg_2, 
          bmi_overweight, bmi_obese,
          mmm_index, fpcs_i, fmcs_i) 

dim(tab_11_fin)

# check if no NAs 
tab_11_fin %>% 
  select(stno, sex_2, fage_s, ethnicity, fstatusx_i_2, edu_imp, flgrlump_i_ordinal,
         fesmoke_i, alc_0, alc_more_14, fg_2, 
         bmi_overweight, bmi_obese,
         mmm_index, fpcs_i, fmcs_i) %>% 
  gather(key = "variable", value = "value", -stno) %>%
  filter(is.na(value) == T) %>% 
  group_by(variable) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n))

stop()
#--------------------------------------------------------------------
# > Save all covariables table
save(tab_11, file = "E:\\PC_FIXE\\Analysis\\01_ARTICLE_1\\05_FINAL_ANALYSES_FIGURES_2\\PHASE_11\\Data\\tab_full_cov_s11.rda")

# > Save data without NA
save(tab_11_fin, file = "E:\\PC_FIXE\\Analysis\\01_ARTICLE_1\\05_FINAL_ANALYSES_FIGURES_2\\PHASE_11\\Data\\tab_cov_s11.rda")



