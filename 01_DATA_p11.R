# Script name: 01_DATA_p11.R
# 
# Author: M.Chen, Inserm, 2021
#
# Doing: Preparing data for cross-sectional models 
# -> for full population (N = 3896) (non-stratified models)
# -> for men (n = 2910) (stratified models)
# -> for women (n = 986) (stratified models)
# 
# Data include 
# - the functional outcome: individual activity distribution on the acceleration continuum
#   or the scalar outcome: individual daily duration of different activity behaviors (SB, LIPA, MVPA) + Time spent in 10 min bouts of MVPA
# - the scalar covariates: 
#   > mean daily waking time, 
#   > socio-demographics, 
#   > behavioral covariates, 
#   > health related factors
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

# > Covariates (phase 11)
load("E:\\PC_FIXE\\Analysis\\01_ARTICLE_1\\05_FINAL_ANALYSES_FIGURES_2\\PHASE_11\\Data\\tab_cov_s11.rda")

#--------------------------------------------------------------------
# Full population (N = 3896) (non-stratified models)

# > Individual activity distribution (for functional models)
tab_pa <- act %>%
  select(-f_i_2) %>%
  filter(stno %in% unique(tab_11_fin$stno)) %>% 
  spread(key = x, value = A_i) %>% 
  arrange(stno) %>% 
  select(-starts_with("dur_"))

# > Time spent in different PA behavior (for linear models)
tab_time <- act %>%
  select(-f_i_2) %>%
  filter(stno %in% unique(tab_11_fin$stno)) %>% 
  spread(key = x, value = A_i) %>% 
  arrange(stno) %>% 
  select(stno, starts_with("dur_"))

# > Covariates
tab_cov <- tab_11_fin %>%
  # Add daily waking time
  left_join(tab_time %>% select(stno, dur_day_min_pla), by = "stno") %>%
  # Compute standardized covariates
  mutate(
    # Facteur de confusion 
    O_waking_time = dur_day_min_pla - 16*60,
    # Age (standardized, 5 years increment)
    A_age_conti_5 = (fage_s - mean(tab_11_fin$fage_s))/5,
    # Physical and mental condition scores (standardized, )
    ) %>%
  # Select and rename covariates
  select(stno, 
         # Confusion factor
         O_waking_time, 
         # Socio-demographics
         "A_sex" = "sex_2", A_age_conti_5, "A_ethnicity" = "ethnicity_i", "A_socio_eco_cont" = "flgrlump_i_ordinal", "A_marital_status" = "fstatusx_i_2",
         # Behavioral covariates
         "B_ex_smokers" = "ex_smoker", "B_current_smokers" = "current_smoker", "B_alc_0" = "alc_0", "B_alc_more_14" = "alc_more_14", "B_fg_2" = "fg_2", 
         # for sensitivity analysis
         "B_mvpa_inactive" = "mvpa_inactive", "B_mvpa_less_2.5" = "mvpa_less_2.5",
         # Cardiometabolic risk factors
         "C_bmi_overweight" = "bmi_overweight", "C_bmi_obese" = "bmi_obese", 
         # Health related indicators
         "D_mmm_index" = mmm_index, "D_pcs" = physical_condition, "D_mcs" = mental_condition) 

# > NA values
sapply(tab_cov,function(x) sum(is.na(x)))
# some NA values for mvpa (for posthoc analaysis)

# > Interaction terms (interaction with sex, and age)
tab_int <- tab_cov  %>%
  gather(key = "var", value = "value", -stno, -A_age_conti_5 , -A_sex, -O_waking_time) %>% 
  mutate(age_x = value*A_age_conti_5,
         sex_x = value*A_sex) %>% 
  select(-value) %>%
  pivot_wider(names_from = var, values_from = c("age_x", "sex_x"), names_sep = "_") %>%
  mutate(age_x_sex = A_age_conti_5*A_sex) %>%
  select(-A_age_conti_5, -A_sex, -O_waking_time)


# > Joindre toutes les tables 
data0 <- tab_pa %>%
  left_join(tab_time, by = "stno") %>%
  left_join(tab_cov, by = "stno") %>%
  left_join(tab_int, by = "stno")

# > Data for functional models
# = a list containing functional outcome, grid and covariates
data1 <- NULL

# Exposures = Covariates
data1 <- as.list(data0[,-c(1:156)])

# Functional outcome = PA distribution (a matrix, one column per grid point)
YY_Activity <- as.matrix(data0[,c(2:151)],
                         nrow = length(unique(tab_11_fin$stno)), 
                         ncol = 150)

# Grid = acceleration values on which is computed PA density
xi_dat <- unique(act$x)
xi <- as.vector(xi_dat)
I <- (xi < 0.5) # reduce grid

data1$xi <- xi[I]
data1$Y_Activity <- as.matrix(YY_Activity[,I])

# > Checks 
# Dimensions of objects
lapply(data1[-which(names(data1) %in% c("Y_Activity","xi"))], 
       function(x) expect_equal(length(x), dim(data1$Y_Activity)[1])) %>% 
  plyr::ldply(., data.frame, .id = "var")

expect_equal(length(data1$xi), dim(data1$Y_Activity)[2])

# NA
sapply(data1, function(x) sum(is.na(x)))

#--------------------------------------------------------------------
# Hommes (N = 2910)

# > Mean age in men
mean_age_man   <- mean(tab_11_fin[which(tab_11_fin$sex_2 == 1),]$fage_s)

# > Mean and sd social activity (for sensitivity analysis)
mean_social_act_man <- mean(tab_11_fin[which(tab_11_fin$sex_2 == 1),]$social_act, na.rm = T)
sd_social_act_man   <- sd(tab_11_fin[which(tab_11_fin$sex_2 == 1),]$social_act, na.rm = T)

# > Covariates chez les hommes
tab_cov_man <- tab_11_fin %>%
  # Sélectionner les données pour les hommes
  filter(sex_2 == 1) %>% 
  # Add daily waking time
  left_join(tab_time %>% select(stno, dur_day_min_pla), by = "stno") %>%
  # Compute standardized covariates
  mutate(
    # Facteur de confusion 
    O_waking_time = dur_day_min_pla - 16*60,
    # Age (standardized, 5 years increment)
    A_age_conti_5 = (fage_s - mean_age_man)/5,
    # Social interaction (standardized, 1 SD increment)
    B_social_act = (social_act - mean_social_act_man)/sd_social_act_man
    ) %>%
  # Select and rename covariates
  select(stno, 
         # Confusion factor
         O_waking_time, 
         # Socio-demographics
         "A_sex" = "sex_2", A_age_conti_5, "A_ethnicity" = "ethnicity_i", "A_socio_eco_cont" = "flgrlump_i_ordinal", "A_marital_status" = "fstatusx_i_2",
         # Behavioral covariates
         "B_ex_smokers" = "ex_smoker", "B_current_smokers" = "current_smoker", "B_alc_0" = "alc_0", "B_alc_more_14" = "alc_more_14", "B_fg_2" = "fg_2", 
         # for sensitivity analysis
         B_social_act, "B_mvpa_inactive" = "mvpa_inactive", "B_mvpa_less_2.5" = "mvpa_less_2.5",
         # Cardiometabolic risk factors
         "C_bmi_overweight" = "bmi_overweight", "C_bmi_obese" = "bmi_obese", 
         # Health related indicators
         "D_mmm_index" = mmm_index, "D_pcs" = physical_condition, "D_mcs" = mental_condition) 

# > Interaction terms
tab_int_man <- tab_cov_man  %>%
  gather(key = "var", value = "value", -stno, -A_age_conti_5 , -O_waking_time) %>% 
  mutate(age_x = value*A_age_conti_5) %>% 
  select(-value) %>%
  pivot_wider(names_from = var, values_from = "age_x", names_glue = "age_x_{var}") %>%
  select(-A_age_conti_5, -O_waking_time)

# > Merge tables
data0_man <- tab_pa %>%
  left_join(tab_time, by = "stno") %>%
  right_join(tab_cov_man, by = "stno") %>% # join on tab_cov_man to keep only data for men
  left_join(tab_int_man, by = "stno")

# > Data for functional models
# = a list containing functional outcome, grid and covariates
data1_man <- NULL

# Exposure = Covariates
data1_man <- as.list(data0_man[,-c(1:156)])

# Grid
data1_man$xi <- xi[I]

# Outcome = PA distribution
YY_Activity_man <- as.matrix(data0_man[,c(2:151)],
                             nrow = length(unique(data0_man$stno)), 
                             ncol = 150)

data1_man$Y_Activity <- as.matrix(YY_Activity_man[,I])

# > Checks 
# Dimensions of objects
lapply(data1_man[-which(names(data1_man) %in% c("Y_Activity","xi"))], 
       function(x) expect_equal(length(x), dim(data1_man$Y_Activity)[1])) %>% 
  plyr::ldply(., data.frame, .id = "var")

expect_equal(length(data1_man$xi), dim(data1_man$Y_Activity)[2])

# NA
sapply(data1_man, function(x) sum(is.na(x)))

#--------------------------------------------------------------------
# Femmes (N = 986)

# > Mean age in women
mean_age_woman   <- mean(tab_11_fin[which(tab_11_fin$sex_2 == 0),]$fage_s)

# > Mean and sd social activity (for sensitivity analysis)
mean_social_act_woman <- mean(tab_11_fin[which(tab_11_fin$sex_2 == 0),]$social_act, na.rm = T)
sd_social_act_woman   <- sd(tab_11_fin[which(tab_11_fin$sex_2 == 0),]$social_act, na.rm = T)

# > Covariates fro women
tab_cov_woman <- tab_11_fin %>%
  # Sélectionner les données pour les femmes
  filter(sex_2 == 0) %>% 
  # Add daily waking time
  left_join(tab_time %>% select(stno, dur_day_min_pla), by = "stno") %>%
  # Compute standardized covariates
  mutate(
    # Confusion factor
    O_waking_time = dur_day_min_pla - 16*60,
    # Age (standardized, 5 years increment)
    A_age_conti_5 = (fage_s - mean_age_woman)/5,
    # Social interaction (standardized, 1 SD increment)
    B_social_act = (social_act - mean_social_act_woman)/sd_social_act_woman
  ) %>%
  # Select and rename covariates
  select(stno, 
         # Confusion factor
         O_waking_time, 
         # Socio-demographics
         "A_sex" = "sex_2", A_age_conti_5, "A_ethnicity" = "ethnicity_i", "A_socio_eco_cont" = "flgrlump_i_ordinal", "A_marital_status" = "fstatusx_i_2",
         # Behavioral covariates
         "B_ex_smokers" = "ex_smoker", "B_current_smokers" = "current_smoker", "B_alc_0" = "alc_0", "B_alc_more_14" = "alc_more_14", "B_fg_2" = "fg_2", 
         # for sensitivity analysis
         B_social_act, "B_mvpa_inactive" = "mvpa_inactive", "B_mvpa_less_2.5" = "mvpa_less_2.5",
         # Cardiometabolic risk factors
         "C_bmi_overweight" = "bmi_overweight", "C_bmi_obese" = "bmi_obese", 
         # Health related indicators
         "D_mmm_index" = mmm_index, "D_pcs" = physical_condition, "D_mcs" = mental_condition) 

# > Interaction terms
tab_int_woman <- tab_cov_woman  %>%
  gather(key = "var", value = "value", -stno, -A_age_conti_5 , -O_waking_time) %>% 
  mutate(age_x = value*A_age_conti_5) %>% 
  select(-value) %>%
  pivot_wider(names_from = var, values_from = "age_x", names_glue = "age_x_{var}") %>%
  select(-A_age_conti_5, -O_waking_time)

# > Merge tables
data0_woman <- tab_pa %>%
  left_join(tab_time, by = "stno") %>%
  right_join(tab_cov_woman, by = "stno") %>% # join on tab_cov_woman to keep only data for women
  left_join(tab_int_woman, by = "stno")

# > Data for functional models
# = a list containing functional outcome, grid and covariates
data1_woman <- NULL

# Exposure = Covariates
data1_woman <- as.list(data0_woman[,-c(1:156)])

# Grid
data1_woman$xi <- xi[I]

# Outcome = PA distribution
YY_Activity_woman <- as.matrix(data0_woman[,c(2:151)],
                               nrow = length(unique(data0_woman$stno)), 
                               ncol = 150)

data1_woman$Y_Activity <- as.matrix(YY_Activity_woman[,I])

# > Checks 
# Dimensions of objects
lapply(data1_woman[-which(names(data1_woman) %in% c("Y_Activity","xi"))], 
       function(x) expect_equal(length(x), dim(data1_woman$Y_Activity)[1])) %>% 
  plyr::ldply(., data.frame, .id = "var")

expect_equal(length(data1_woman$xi), dim(data1_woman$Y_Activity)[2])

# NA
sapply(data1_woman, function(x) sum(is.na(x)))

#--------------------------------------------------------------------
# Save

# > Group dataframes
full_data_11  <- list(lm = data0,       fm = data1)
data_man_11   <- list(lm = data0_man,   fm = data1_man)
data_woman_11 <- list(lm = data0_woman, fm = data1_woman)

# > Full
path <- "E:\\PC_FIXE\\Analysis\\01_ARTICLE_1\\05_FINAL_ANALYSES_FIGURES_2\\PHASE_11\\Data"
save(full_data_11,  file = paste0(path, "\\full_data_11.rda"))
save(data_man_11,   file = paste0(path, "\\data_man_11.rda"))
save(data_woman_11, file = paste0(path, "\\data_woman_11.rda"))

