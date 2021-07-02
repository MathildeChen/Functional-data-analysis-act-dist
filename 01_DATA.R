# Script name: 01_DATA.R
# 
# Author: M.Chen, Inserm, 2021
#
# Doing: Preparing data for cross-sectional and longitudinal models (with all data)
# -> for full population (N = 3579) (non-stratified models)
# -> for men (n = 2688) (stratified models)
# -> for women (n = 891) (stratified models)
# 
# Data include 
# - the functional outcome: individual activity distribution on the acceleration continuum
#   or the scalar outcome: individual daily duration of different activity behaviors (SB, LIPA, MVPA) + Time spent in 10 min bouts of MVPA
# - the scalar covariates: 
#   > mean daily waking time, 
#   > socio-demographics, 
#   > lifestyle covariates, 
#   > health related factors
# collected in 1991-1994, 2002-2004 and 2012-2013
# - the interaction terms between age, sex and other covariates
# 
#
# This script need to be ran before fitting the models
#
#--------------------------------------------------------------------
# Packages

library(tidyr)
library(plyr)
library(dplyr)
library(purrr)
library(stringr)
library(testthat)

#--------------------------------------------------------------------
# Data

# > Physical activity (phase 11)
load("E:\\PC_FIXE\\Analysis\\01_ARTICLE_1\\02_FUNCTION_ON_SCALAR\\test_refund\\data\\2020-05-15_act_4006_trap.rda")

# > Covariates (phase 3, 7 and 11)
load("E:\\PC_FIXE\\Analysis\\01_ARTICLE_1\\05_FINAL_ANALYSES_FIGURES_2\\ALL_PHASES\\Data\\tab_cov.rda")

#--------------------------------------------------------------------
# Full population (N = 3579) (non-stratified models)

# > Individual activity distribution (for functional models)
tab_pa <- act %>%
  select(-f_i_2) %>%
  filter(stno %in% unique(TAB$stno)) %>% 
  spread(key = x, value = A_i) %>% 
  arrange(stno) %>% 
  select(-starts_with("dur_"))

# > Time spent in different PA behavior (for linear models)
tab_time <- act %>%
  select(-f_i_2) %>%
  filter(stno %in% unique(TAB$stno)) %>% 
  spread(key = x, value = A_i) %>% 
  arrange(stno) %>% 
  select(stno, starts_with("dur_"))

#----------------
# > Mean age at each phase
mean_age_11 <- mean(TAB$fage_s)
mean_age_7 <- mean(TAB$mage_s_i)
mean_age_3 <- mean(TAB$xage_s_i)

#----------------
# > Covariates phase 11
tab_cov_11 <- TAB %>%
  # Select relevant variables for phase 11
  select(stno, sex_2, ethnicity_i, starts_with("f")) %>%
  rename_at(4:16, substring, 2) %>% 
  # Add daily waking time
  left_join(tab_time %>% select(stno, dur_day_min_pla), by = "stno") %>%
  # Compute standardized covariates
  mutate(
    # Facteur de confusion 
    O_waking_time = dur_day_min_pla - 16*60,
    # Age (standardized, 5 years increment)
    A_age_conti_5 = (age_s - mean_age_11)/5
    ) %>%
  # Select and rename covariates
  select(stno, 
         # Confusion factor
         O_waking_time, 
         # Socio-demographics
         "A_sex" = "sex_2", A_age_conti_5, "A_ethnicity" = "ethnicity_i", "A_socio_eco_cont" = "lgrlump_i_ordinal", "A_marital_status" = "statusx_i_2",
         # Behavioral covariates
         "B_ex_smokers" = "ex_smoker", "B_current_smokers" = "current_smoker", "B_alc_0" = "alc_0", "B_alc_more_14" = "alc_more_14", "B_fg_2" = "fg_2", 
         # for sensitivity analysis
         #"B_mvpa_inactive" = "mvpa_inactive", "B_mvpa_less_2.5" = "mvpa_less_2.5",
         # Cardiometabolic risk factors
         "C_bmi_overweight" = "bmi_overweight", "C_bmi_obese" = "bmi_obese", 
         # Health related indicators
         "D_mmm_index" = mmm_index, "D_pcs" = physical_condition, "D_mcs" = mental_condition) 

# > Covariates phase 7
tab_cov_7 <- TAB %>%
  # Select relevant variables for phase 7
  select(stno, sex_2, ethnicity_i, starts_with("m")) %>%
  rename_at(4:16, substring, 2) %>% 
  # Add daily waking time
  left_join(tab_time %>% select(stno, dur_day_min_pla), by = "stno") %>%
  # Compute standardized covariates
  mutate(
    # Facteur de confusion 
    O_waking_time = dur_day_min_pla - 16*60,
    # Age (standardized, 5 years increment)
    A_age_conti_5 = (age_s_i - mean_age_7)/5
  ) %>%
  # Select and rename covariates
  select(stno, 
         # Confusion factor
         O_waking_time, 
         # Socio-demographics
         "A_sex" = "sex_2", A_age_conti_5, "A_ethnicity" = "ethnicity_i", "A_socio_eco_cont" = "lgrlump_i_ordinal", "A_marital_status" = "statusx_i_2",
         # Behavioral covariates
         "B_ex_smokers" = "ex_smoker", "B_current_smokers" = "current_smoker", "B_alc_0" = "alc_0", "B_alc_more_14" = "alc_more_14", "B_fg_2" = "fg_2", 
         # for sensitivity analysis
         #"B_mvpa_inactive" = "mvpa_inactive", "B_mvpa_less_2.5" = "mvpa_less_2.5",
         # Cardiometabolic risk factors
         "C_bmi_overweight" = "bmi_overweight", "C_bmi_obese" = "bmi_obese", 
         # Health related indicators
         "D_mmm_index" = mmm_index, "D_pcs" = physical_condition, "D_mcs" = mental_condition)

# > Covariates phase 3
tab_cov_3 <- TAB %>%
  # Select relevant variables for phase 11
  select(stno, sex_2, ethnicity_i, starts_with("x")) %>%
  rename_at(4:16, substring, 2) %>% 
  # Add daily waking time
  left_join(tab_time %>% select(stno, dur_day_min_pla), by = "stno") %>%
  # Compute standardized covariates
  mutate(
    # Facteur de confusion 
    O_waking_time = dur_day_min_pla - 16*60,
    # Age (standardized, 5 years increment)
    A_age_conti_5 = (age_s_i - mean_age_3)/5
  ) %>%
  # Select and rename covariates
  select(stno, 
         # Confusion factor
         O_waking_time, 
         # Socio-demographics
         "A_sex" = "sex_2", A_age_conti_5, "A_ethnicity" = "ethnicity_i", "A_socio_eco_cont" = "grlump_i_ordinal", "A_marital_status" = "statusx_i_2",
         # Behavioral covariates
         "B_ex_smokers" = "ex_smoker", "B_current_smokers" = "current_smoker", "B_alc_0" = "alc_0", "B_alc_more_14" = "alc_more_14", "B_fg_2" = "fg_2", 
         # for sensitivity analysis
         #"B_mvpa_inactive" = "mvpa_inactive", "B_mvpa_less_2.5" = "mvpa_less_2.5",
         # Cardiometabolic risk factors
         "C_bmi_overweight" = "bmi_overweight", "C_bmi_obese" = "bmi_obese", 
         # Health related indicators
         "D_mmm_index" = mmm_index, "D_pcs" = physical_condition, "D_mcs" = mental_condition)

#----------------
# > NA values
sapply(tab_cov_11, function(x) sum(is.na(x)))
sapply(tab_cov_7, function(x) sum(is.na(x)))
sapply(tab_cov_3, function(x) sum(is.na(x)))

#----------------
# > Interaction terms (interaction with sex, and age)
# phase 11
tab_int_11 <- tab_cov_11  %>%
  gather(key = "var", value = "value", -stno, -A_age_conti_5 , -A_sex, -O_waking_time) %>% 
  mutate(age_x = value*A_age_conti_5,
         sex_x = value*A_sex) %>% 
  select(-value) %>%
  pivot_wider(names_from = var, values_from = c("age_x", "sex_x"), names_sep = "_") %>%
  mutate(age_x_sex = A_age_conti_5*A_sex) %>%
  select(-A_age_conti_5, -A_sex, -O_waking_time)
# phase 7
tab_int_7 <- tab_cov_7  %>%
  gather(key = "var", value = "value", -stno, -A_age_conti_5 , -A_sex, -O_waking_time) %>% 
  mutate(age_x = value*A_age_conti_5,
         sex_x = value*A_sex) %>% 
  select(-value) %>%
  pivot_wider(names_from = var, values_from = c("age_x", "sex_x"), names_sep = "_") %>%
  mutate(age_x_sex = A_age_conti_5*A_sex) %>%
  select(-A_age_conti_5, -A_sex, -O_waking_time)
# phase 3
tab_int_3 <- tab_cov_3  %>%
  gather(key = "var", value = "value", -stno, -A_age_conti_5 , -A_sex, -O_waking_time) %>% 
  mutate(age_x = value*A_age_conti_5,
         sex_x = value*A_sex) %>% 
  select(-value) %>%
  pivot_wider(names_from = var, values_from = c("age_x", "sex_x"), names_sep = "_") %>%
  mutate(age_x_sex = A_age_conti_5*A_sex) %>%
  select(-A_age_conti_5, -A_sex, -O_waking_time)

#----------------
# > Joindre toutes les tables 
# phase 11
data0_11_full <- tab_pa %>%
  left_join(tab_time, by = "stno") %>%
  left_join(tab_cov_11, by = "stno") %>%
  left_join(tab_int_11, by = "stno")
# phase 7
data0_7_full <- tab_pa %>%
  left_join(tab_time, by = "stno") %>%
  left_join(tab_cov_7, by = "stno") %>%
  left_join(tab_int_7, by = "stno")
# phase 3
data0_3_full <- tab_pa %>%
  left_join(tab_time, by = "stno") %>%
  left_join(tab_cov_3, by = "stno") %>%
  left_join(tab_int_3, by = "stno")

#----------------
# > Data for functional models
# = a list containing functional outcome, grid and covariates
data1_11_full <- NULL
data1_7_full <- NULL
data1_3_full <- NULL

# Exposures = Covariates
data1_11_full <- as.list(data0_11_full[,-c(1:156)])
data1_7_full <- as.list(data0_7_full[,-c(1:156)])
data1_3_full <- as.list(data0_3_full[,-c(1:156)])

# Functional outcome = PA distribution (a matrix, one column per grid point)
YY_Activity <- as.matrix(data0_11_full[,c(2:151)],
                         nrow = length(unique(TAB$stno)), 
                         ncol = 150)

# Grid = acceleration values on which is computed PA density
xi_dat <- unique(act$x)
xi <- as.vector(xi_dat)
I <- (xi < 0.5) # reduce grid

data1_11_full$xi <- xi[I]
data1_11_full$Y_Activity <- as.matrix(YY_Activity[,I])

data1_7_full$xi <- xi[I]
data1_7_full$Y_Activity <- as.matrix(YY_Activity[,I])

data1_3_full$xi <- xi[I]
data1_3_full$Y_Activity <- as.matrix(YY_Activity[,I])

# > Checks 
# Dimensions of objects
lapply(data1_11_full[-which(names(data1_11_full) %in% c("Y_Activity","xi"))], 
       function(x) expect_equal(length(x), dim(data1_11_full$Y_Activity)[1])) %>% 
  plyr::ldply(., data.frame, .id = "var")
lapply(data1_7_full[-which(names(data1_7_full) %in% c("Y_Activity","xi"))], 
       function(x) expect_equal(length(x), dim(data1_7_full$Y_Activity)[1])) %>% 
  plyr::ldply(., data.frame, .id = "var")
lapply(data1_3_full[-which(names(data1_3_full) %in% c("Y_Activity","xi"))], 
       function(x) expect_equal(length(x), dim(data1_3_full$Y_Activity)[1])) %>% 
  plyr::ldply(., data.frame, .id = "var")

expect_equal(length(data1_11_full$xi), dim(data1_11_full$Y_Activity)[2])
expect_equal(length(data1_7_full$xi), dim(data1_7_full$Y_Activity)[2])
expect_equal(length(data1_3_full$xi), dim(data1_3_full$Y_Activity)[2])

# NA
sapply(data1_11_full, function(x) sum(is.na(x)))
sapply(data1_7_full, function(x) sum(is.na(x)))
sapply(data1_3_full, function(x) sum(is.na(x)))

#--------------------------------------------------------------------
# Hommes (N = 2688)

# > Mean age in men at each phase
mean_age_11_man <- mean(TAB[which(TAB$sex_2 == 1),]$fage_s)
mean_age_7_man <- mean(TAB[which(TAB$sex_2 == 1),]$mage_s_i)
mean_age_3_man <- mean(TAB[which(TAB$sex_2 == 1),]$xage_s_i)

#----------------
# > Covariates phase 11 for men
tab_cov_11_man <- TAB %>%
  # Sélectionner les données pour les hommes
  filter(sex_2 == 1) %>% 
  # Select relevant variables for phase 11
  select(stno, sex_2, ethnicity_i, starts_with("f")) %>%
  rename_at(4:16, substring, 2) %>% 
  # Add daily waking time
  left_join(tab_time %>% select(stno, dur_day_min_pla), by = "stno") %>%
  # Compute standardized covariates
  mutate(
    # Facteur de confusion 
    O_waking_time = dur_day_min_pla - 16*60,
    # Age (standardized, 5 years increment)
    A_age_conti_5 = (age_s - mean_age_11_man)/5
  ) %>%
  # Select and rename covariates
  select(stno, 
         # Confusion factor
         O_waking_time, 
         # Socio-demographics
         "A_sex" = "sex_2", A_age_conti_5, "A_ethnicity" = "ethnicity_i", "A_socio_eco_cont" = "lgrlump_i_ordinal", "A_marital_status" = "statusx_i_2",
         # Behavioral covariates
         "B_ex_smokers" = "ex_smoker", "B_current_smokers" = "current_smoker", "B_alc_0" = "alc_0", "B_alc_more_14" = "alc_more_14", "B_fg_2" = "fg_2", 
         # for sensitivity analysis
         #"B_mvpa_inactive" = "mvpa_inactive", "B_mvpa_less_2.5" = "mvpa_less_2.5",
         # Cardiometabolic risk factors
         "C_bmi_overweight" = "bmi_overweight", "C_bmi_obese" = "bmi_obese", 
         # Health related indicators
         "D_mmm_index" = mmm_index, "D_pcs" = physical_condition, "D_mcs" = mental_condition) 

# > Covariates phase 7
tab_cov_7_man <- TAB %>%
  # Sélectionner les données pour les hommes
  filter(sex_2 == 1) %>%
  # Select relevant variables for phase 7
  select(stno, sex_2, ethnicity_i, starts_with("m")) %>%
  rename_at(4:16, substring, 2) %>% 
  # Add daily waking time
  left_join(tab_time %>% select(stno, dur_day_min_pla), by = "stno") %>%
  # Compute standardized covariates
  mutate(
    # Facteur de confusion 
    O_waking_time = dur_day_min_pla - 16*60,
    # Age (standardized, 5 years increment)
    A_age_conti_5 = (age_s_i - mean_age_7_man)/5
  ) %>%
  # Select and rename covariates
  select(stno, 
         # Confusion factor
         O_waking_time, 
         # Socio-demographics
         "A_sex" = "sex_2", A_age_conti_5, "A_ethnicity" = "ethnicity_i", "A_socio_eco_cont" = "lgrlump_i_ordinal", "A_marital_status" = "statusx_i_2",
         # Behavioral covariates
         "B_ex_smokers" = "ex_smoker", "B_current_smokers" = "current_smoker", "B_alc_0" = "alc_0", "B_alc_more_14" = "alc_more_14", "B_fg_2" = "fg_2", 
         # for sensitivity analysis
         #"B_mvpa_inactive" = "mvpa_inactive", "B_mvpa_less_2.5" = "mvpa_less_2.5",
         # Cardiometabolic risk factors
         "C_bmi_overweight" = "bmi_overweight", "C_bmi_obese" = "bmi_obese", 
         # Health related indicators
         "D_mmm_index" = mmm_index, "D_pcs" = physical_condition, "D_mcs" = mental_condition)

# > Covariates phase 3
tab_cov_3_man <- TAB %>%
  # Sélectionner les données pour les hommes
  filter(sex_2 == 1) %>%
  # Select relevant variables for phase 11
  select(stno, sex_2, ethnicity_i, starts_with("x")) %>%
  rename_at(4:16, substring, 2) %>% 
  # Add daily waking time
  left_join(tab_time %>% select(stno, dur_day_min_pla), by = "stno") %>%
  # Compute standardized covariates
  mutate(
    # Facteur de confusion 
    O_waking_time = dur_day_min_pla - 16*60,
    # Age (standardized, 5 years increment)
    A_age_conti_5 = (age_s_i - mean_age_3_man)/5
  ) %>%
  # Select and rename covariates
  select(stno, 
         # Confusion factor
         O_waking_time, 
         # Socio-demographics
         "A_sex" = "sex_2", A_age_conti_5, "A_ethnicity" = "ethnicity_i", "A_socio_eco_cont" = "grlump_i_ordinal", "A_marital_status" = "statusx_i_2",
         # Behavioral covariates
         "B_ex_smokers" = "ex_smoker", "B_current_smokers" = "current_smoker", "B_alc_0" = "alc_0", "B_alc_more_14" = "alc_more_14", "B_fg_2" = "fg_2", 
         # for sensitivity analysis
         #"B_mvpa_inactive" = "mvpa_inactive", "B_mvpa_less_2.5" = "mvpa_less_2.5",
         # Cardiometabolic risk factors
         "C_bmi_overweight" = "bmi_overweight", "C_bmi_obese" = "bmi_obese", 
         # Health related indicators
         "D_mmm_index" = mmm_index, "D_pcs" = physical_condition, "D_mcs" = mental_condition)

#----------------
# > Interaction terms
tab_int_11_man <- tab_cov_11_man  %>%
  gather(key = "var", value = "value", -stno, -A_age_conti_5 , -O_waking_time) %>% 
  mutate(age_x = value*A_age_conti_5) %>% 
  select(-value) %>%
  pivot_wider(names_from = var, values_from = "age_x", names_glue = "age_x_{var}") %>%
  select(-A_age_conti_5, -O_waking_time)

tab_int_7_man <- tab_cov_7_man  %>%
  gather(key = "var", value = "value", -stno, -A_age_conti_5 , -O_waking_time) %>% 
  mutate(age_x = value*A_age_conti_5) %>% 
  select(-value) %>%
  pivot_wider(names_from = var, values_from = "age_x", names_glue = "age_x_{var}") %>%
  select(-A_age_conti_5, -O_waking_time)

tab_int_3_man <- tab_cov_3_man  %>%
  gather(key = "var", value = "value", -stno, -A_age_conti_5 , -O_waking_time) %>% 
  mutate(age_x = value*A_age_conti_5) %>% 
  select(-value) %>%
  pivot_wider(names_from = var, values_from = "age_x", names_glue = "age_x_{var}") %>%
  select(-A_age_conti_5, -O_waking_time)

#----------------
# > Merge tables
# phase 11
data0_11_man <- tab_pa %>%
  left_join(tab_time, by = "stno") %>%
  right_join(tab_cov_11_man, by = "stno") %>% # join on tab_cov_man to keep only data for men
  left_join(tab_int_11_man, by = "stno")
# phase 7
data0_7_man <- tab_pa %>%
  left_join(tab_time, by = "stno") %>%
  right_join(tab_cov_7_man, by = "stno") %>% # join on tab_cov_man to keep only data for men
  left_join(tab_int_7_man, by = "stno")
# phase 3
data0_3_man <- tab_pa %>%
  left_join(tab_time, by = "stno") %>%
  right_join(tab_cov_3_man, by = "stno") %>% # join on tab_cov_man to keep only data for men
  left_join(tab_int_3_man, by = "stno")

#----------------
# > Data for functional models
# = a list containing functional outcome, grid and covariates
data1_11_man <- NULL
data1_7_man <- NULL
data1_3_man <- NULL

# Exposure = Covariates
data1_11_man <- as.list(data0_11_man[,-c(1:156)])
data1_7_man <- as.list(data0_7_man[,-c(1:156)])
data1_3_man <- as.list(data0_3_man[,-c(1:156)])

# Grid
data1_11_man$xi <- xi[I]
data1_7_man$xi  <- xi[I]
data1_3_man$xi  <- xi[I]

# Outcome = PA distribution
YY_Activity_man <- as.matrix(data0_11_man[,c(2:151)],
                             nrow = length(unique(data0_11_man$stno)), 
                             ncol = 150)

data1_11_man$Y_Activity <- as.matrix(YY_Activity_man[,I])
data1_7_man$Y_Activity <- as.matrix(YY_Activity_man[,I])
data1_3_man$Y_Activity <- as.matrix(YY_Activity_man[,I])

# > Checks 
# Dimensions of objects
lapply(data1_11_man[-which(names(data1_11_man) %in% c("Y_Activity","xi"))], 
       function(x) expect_equal(length(x), dim(data1_11_man$Y_Activity)[1])) %>% 
  plyr::ldply(., data.frame, .id = "var")
lapply(data1_7_man[-which(names(data1_7_man) %in% c("Y_Activity","xi"))], 
       function(x) expect_equal(length(x), dim(data1_7_man$Y_Activity)[1])) %>% 
  plyr::ldply(., data.frame, .id = "var")
lapply(data1_3_man[-which(names(data1_3_man) %in% c("Y_Activity","xi"))], 
       function(x) expect_equal(length(x), dim(data1_3_man$Y_Activity)[1])) %>% 
  plyr::ldply(., data.frame, .id = "var")

expect_equal(length(data1_11_man$xi), dim(data1_11_man$Y_Activity)[2])
expect_equal(length(data1_7_man$xi), dim(data1_7_man$Y_Activity)[2])
expect_equal(length(data1_3_man$xi), dim(data1_3_man$Y_Activity)[2])

# NA
sapply(data1_11_man, function(x) sum(is.na(x)))
sapply(data1_7_man, function(x) sum(is.na(x)))
sapply(data1_3_man, function(x) sum(is.na(x)))

#--------------------------------------------------------------------
# Femmes (N = 891)

# > Mean age in men at each phase
mean_age_11_woman <- mean(TAB[which(TAB$sex_2 == 0),]$fage_s)
mean_age_7_woman <- mean(TAB[which(TAB$sex_2 == 0),]$mage_s_i)
mean_age_3_woman <- mean(TAB[which(TAB$sex_2 == 0),]$xage_s_i)

#----------------
# > Covariates phase 11 for women
tab_cov_11_woman <- TAB %>%
  # Sélectionner les données pour les femmes
  filter(sex_2 == 0) %>% 
  # Select relevant variables for phase 11
  select(stno, sex_2, ethnicity_i, starts_with("f")) %>%
  rename_at(4:16, substring, 2) %>% 
  # Add daily waking time
  left_join(tab_time %>% select(stno, dur_day_min_pla), by = "stno") %>%
  # Compute standardized covariates
  mutate(
    # Facteur de confusion 
    O_waking_time = dur_day_min_pla - 16*60,
    # Age (standardized, 5 years increment)
    A_age_conti_5 = (age_s - mean_age_11_woman)/5
  ) %>%
  # Select and rename covariates
  select(stno, 
         # Confusion factor
         O_waking_time, 
         # Socio-demographics
         "A_sex" = "sex_2", A_age_conti_5, "A_ethnicity" = "ethnicity_i", "A_socio_eco_cont" = "lgrlump_i_ordinal", "A_marital_status" = "statusx_i_2",
         # Behavioral covariates
         "B_ex_smokers" = "ex_smoker", "B_current_smokers" = "current_smoker", "B_alc_0" = "alc_0", "B_alc_more_14" = "alc_more_14", "B_fg_2" = "fg_2", 
         # for sensitivity analysis
         #"B_mvpa_inactive" = "mvpa_inactive", "B_mvpa_less_2.5" = "mvpa_less_2.5",
         # Cardiometabolic risk factors
         "C_bmi_overweight" = "bmi_overweight", "C_bmi_obese" = "bmi_obese", 
         # Health related indicators
         "D_mmm_index" = mmm_index, "D_pcs" = physical_condition, "D_mcs" = mental_condition) 

# > Covariates phase 7
tab_cov_7_woman <- TAB %>%
  # Sélectionner les données pour les femmes
  filter(sex_2 == 0) %>%
  # Select relevant variables for phase 7
  select(stno, sex_2, ethnicity_i, starts_with("m")) %>%
  rename_at(4:16, substring, 2) %>% 
  # Add daily waking time
  left_join(tab_time %>% select(stno, dur_day_min_pla), by = "stno") %>%
  # Compute standardized covariates
  mutate(
    # Facteur de confusion 
    O_waking_time = dur_day_min_pla - 16*60,
    # Age (standardized, 5 years increment)
    A_age_conti_5 = (age_s_i - mean_age_7_woman)/5
  ) %>%
  # Select and rename covariates
  select(stno, 
         # Confusion factor
         O_waking_time, 
         # Socio-demographics
         "A_sex" = "sex_2", A_age_conti_5, "A_ethnicity" = "ethnicity_i", "A_socio_eco_cont" = "lgrlump_i_ordinal", "A_marital_status" = "statusx_i_2",
         # Behavioral covariates
         "B_ex_smokers" = "ex_smoker", "B_current_smokers" = "current_smoker", "B_alc_0" = "alc_0", "B_alc_more_14" = "alc_more_14", "B_fg_2" = "fg_2", 
         # for sensitivity analysis
         #"B_mvpa_inactive" = "mvpa_inactive", "B_mvpa_less_2.5" = "mvpa_less_2.5",
         # Cardiometabolic risk factors
         "C_bmi_overweight" = "bmi_overweight", "C_bmi_obese" = "bmi_obese", 
         # Health related indicators
         "D_mmm_index" = mmm_index, "D_pcs" = physical_condition, "D_mcs" = mental_condition)

# > Covariates phase 3
tab_cov_3_woman <- TAB %>%
  # Sélectionner les données pour les femmes
  filter(sex_2 == 0) %>%
  # Select relevant variables for phase 11
  select(stno, sex_2, ethnicity_i, starts_with("x")) %>%
  rename_at(4:16, substring, 2) %>% 
  # Add daily waking time
  left_join(tab_time %>% select(stno, dur_day_min_pla), by = "stno") %>%
  # Compute standardized covariates
  mutate(
    # Facteur de confusion 
    O_waking_time = dur_day_min_pla - 16*60,
    # Age (standardized, 5 years increment)
    A_age_conti_5 = (age_s_i - mean_age_3_woman)/5
  ) %>%
  # Select and rename covariates
  select(stno, 
         # Confusion factor
         O_waking_time, 
         # Socio-demographics
         "A_sex" = "sex_2", A_age_conti_5, "A_ethnicity" = "ethnicity_i", "A_socio_eco_cont" = "grlump_i_ordinal", "A_marital_status" = "statusx_i_2",
         # Behavioral covariates
         "B_ex_smokers" = "ex_smoker", "B_current_smokers" = "current_smoker", "B_alc_0" = "alc_0", "B_alc_more_14" = "alc_more_14", "B_fg_2" = "fg_2", 
         # for sensitivity analysis
         #"B_mvpa_inactive" = "mvpa_inactive", "B_mvpa_less_2.5" = "mvpa_less_2.5",
         # Cardiometabolic risk factors
         "C_bmi_overweight" = "bmi_overweight", "C_bmi_obese" = "bmi_obese", 
         # Health related indicators
         "D_mmm_index" = mmm_index, "D_pcs" = physical_condition, "D_mcs" = mental_condition)

#----------------
# > Interaction terms
tab_int_11_woman <- tab_cov_11_woman  %>%
  gather(key = "var", value = "value", -stno, -A_age_conti_5 , -O_waking_time) %>% 
  mutate(age_x = value*A_age_conti_5) %>% 
  select(-value) %>%
  pivot_wider(names_from = var, values_from = "age_x", names_glue = "age_x_{var}") %>%
  select(-A_age_conti_5, -O_waking_time)

tab_int_7_woman <- tab_cov_7_woman  %>%
  gather(key = "var", value = "value", -stno, -A_age_conti_5 , -O_waking_time) %>% 
  mutate(age_x = value*A_age_conti_5) %>% 
  select(-value) %>%
  pivot_wider(names_from = var, values_from = "age_x", names_glue = "age_x_{var}") %>%
  select(-A_age_conti_5, -O_waking_time)

tab_int_3_woman <- tab_cov_3_woman  %>%
  gather(key = "var", value = "value", -stno, -A_age_conti_5 , -O_waking_time) %>% 
  mutate(age_x = value*A_age_conti_5) %>% 
  select(-value) %>%
  pivot_wider(names_from = var, values_from = "age_x", names_glue = "age_x_{var}") %>%
  select(-A_age_conti_5, -O_waking_time)

#----------------
# > Merge tables
# phase 11
data0_11_woman <- tab_pa %>%
  left_join(tab_time, by = "stno") %>%
  right_join(tab_cov_11_woman, by = "stno") %>% # join on tab_cov_man to keep only data for men
  left_join(tab_int_11_woman, by = "stno")
# phase 7
data0_7_woman <- tab_pa %>%
  left_join(tab_time, by = "stno") %>%
  right_join(tab_cov_7_woman, by = "stno") %>% # join on tab_cov_man to keep only data for men
  left_join(tab_int_7_woman, by = "stno")
# phase 3
data0_3_woman <- tab_pa %>%
  left_join(tab_time, by = "stno") %>%
  right_join(tab_cov_3_woman, by = "stno") %>% # join on tab_cov_man to keep only data for men
  left_join(tab_int_3_woman, by = "stno")

#----------------
# > Data for functional models
# = a list containing functional outcome, grid and covariates
data1_11_woman <- NULL
data1_7_woman <- NULL
data1_3_woman <- NULL

# Exposure = Covariates
data1_11_woman <- as.list(data0_11_woman[,-c(1:156)])
data1_7_woman <- as.list(data0_7_woman[,-c(1:156)])
data1_3_woman <- as.list(data0_3_woman[,-c(1:156)])

# Grid
data1_11_woman$xi <- xi[I]
data1_7_woman$xi  <- xi[I]
data1_3_woman$xi  <- xi[I]

# Outcome = PA distribution
YY_Activity_woman <- as.matrix(data0_11_woman[,c(2:151)],
                             nrow = length(unique(data0_11_woman$stno)), 
                             ncol = 150)

data1_11_woman$Y_Activity <- as.matrix(YY_Activity_woman[,I])
data1_7_woman$Y_Activity <- as.matrix(YY_Activity_woman[,I])
data1_3_woman$Y_Activity <- as.matrix(YY_Activity_woman[,I])

# > Checks 
# Dimensions of objects
lapply(data1_11_woman[-which(names(data1_11_woman) %in% c("Y_Activity","xi"))], 
       function(x) expect_equal(length(x), dim(data1_11_woman$Y_Activity)[1])) %>% 
  plyr::ldply(., data.frame, .id = "var")
lapply(data1_7_woman[-which(names(data1_7_woman) %in% c("Y_Activity","xi"))], 
       function(x) expect_equal(length(x), dim(data1_7_woman$Y_Activity)[1])) %>% 
  plyr::ldply(., data.frame, .id = "var")
lapply(data1_3_woman[-which(names(data1_3_woman) %in% c("Y_Activity","xi"))], 
       function(x) expect_equal(length(x), dim(data1_3_woman$Y_Activity)[1])) %>% 
  plyr::ldply(., data.frame, .id = "var")

expect_equal(length(data1_11_woman$xi), dim(data1_11_woman$Y_Activity)[2])
expect_equal(length(data1_7_woman$xi), dim(data1_7_woman$Y_Activity)[2])
expect_equal(length(data1_3_woman$xi), dim(data1_3_woman$Y_Activity)[2])

# NA
sapply(data1_11_woman, function(x) sum(is.na(x)))
sapply(data1_7_woman, function(x) sum(is.na(x)))
sapply(data1_3_woman, function(x) sum(is.na(x)))


#--------------------------------------------------------------------
# Save

# > Group dataframes
# phase 11
data_11_full       <- list(lm = data0_11_full,  fm = data1_11_full)
data_man_11_full   <- list(lm = data0_11_man,   fm = data1_11_man)
data_woman_11_full <- list(lm = data0_11_woman, fm = data1_11_woman)

# phase 7
data_7_full       <- list(lm = data0_7_full,  fm = data1_7_full)
data_man_7_full   <- list(lm = data0_7_man,   fm = data1_7_man)
data_woman_7_full <- list(lm = data0_7_woman, fm = data1_7_woman)

# phase 3
data_3_full       <- list(lm = data0_3_full,  fm = data1_3_full)
data_man_3_full   <- list(lm = data0_3_man,   fm = data1_3_man)
data_woman_3_full <- list(lm = data0_3_woman, fm = data1_3_woman)


# > Path to save
path <- "E:\\PC_FIXE\\Analysis\\01_ARTICLE_1\\05_FINAL_ANALYSES_FIGURES_2\\ALL_PHASES\\Data"

save(data_11_full,       file = paste0(path, "\\full_data_11.rda"))
save(data_man_11_full,   file = paste0(path, "\\full_data_man_11.rda"))
save(data_woman_11_full, file = paste0(path, "\\full_data_woman_11.rda"))

save(data_7_full,       file = paste0(path, "\\full_data_7.rda"))
save(data_man_7_full,   file = paste0(path, "\\full_data_man_7.rda"))
save(data_woman_7_full, file = paste0(path, "\\full_data_woman_7.rda"))

save(data_3_full,       file = paste0(path, "\\full_data_3.rda"))
save(data_man_3_full,   file = paste0(path, "\\full_data_man_3.rda"))
save(data_woman_3_full, file = paste0(path, "\\full_data_woman_3.rda"))

