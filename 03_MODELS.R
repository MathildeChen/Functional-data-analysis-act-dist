# Script name: 03_MODELS.R
# 
# Author: M.Chen, Inserm, 2021
#
# Doing: Fitting 
# > functional models: 
#     - Outcome:      daily PA distribution (phase 11)
#     - Risk factors: Set of covariates (cross-sectional analyses: phase 11; prospective analyses: phase 3 or 7)
# > linear models: 
#     - Outcome:      daily duration of SB, LIPA and MVPA (phase 11)
#     - Risk factors: Set of covariates (cross-sectional analyses: phase 11; prospective analyses: phase 3 or 7)

# Script structure: 

# -------- PART 1 -------- 
# Cross-sectional analyses (associations at phase 11) 

# 1.1 Non stratified analyses
# - Functional and linear models adjusted for socio-demographics and mean waking time
# - Functional model fully adjusted
# - Functional model fully adjusted and additionnally adjusted for interactions between covariates and sex

# 1.2. Stratified analyses
# - Functional and linear models for men
# - Functional and linear models for women

# -------- PART 2 -------- 
# Prospective analyses (associations at phase 3 and 7)

# 2.1 Non stratified analyses
# - Functional model fully adjusted
# - Functional model fully adjusted and additionnally adjusted for interactions between covariates and sex

# 2.2. Stratified analyses
# - Functional and linear models for men at phases 3 and 7
# - Functional and linear models for women at phases 3 and 7

# -------- PART 3 -------- 
# Sensitivity analyses 

# 3.1 Stratified models including earlier PA
# - Functional and linear models for men at phases 3 and 7
# - Functional and linear models for women at phases 3 and 7

# 3.2 Stratified models including participants with data for all phases
# - Functional and linear models for men at phases 3, 7 and 11
# - Functional and linear models for women at phases 3, 7 and 11

#--------------------------------------------------------------------
# Packages
# > Tools
library(broom)
library(xlsx)
library(lmtest)

#--------------------------------------------------------------------
# Functions to extract models coefficients
# Load other packages (tidyverse etc.)
source("E:\\PC_FIXE\\Analysis\\01_ARTICLE_1\\05_FINAL_ANALYSES_FIGURES_2\\02_FUNCTIONS.R")

#--------------------------------------------------------------------
# Data
path <- "E:\\PC_FIXE\\Analysis\\01_ARTICLE_1\\05_FINAL_ANALYSES_FIGURES_2"

# > Phase 11
load(paste0(path, "\\PHASE_11\\Data\\full_data_11.rda"))
load(paste0(path, "\\PHASE_11\\Data\\data_man_11.rda"))
load(paste0(path, "\\PHASE_11\\Data\\data_woman_11.rda"))

data0_11 <- full_data_11$lm
data1_11 <- full_data_11$fm

data0_11_man <- data_man_11$lm
data1_11_man <- data_man_11$fm

data0_11_woman <- data_woman_11$lm
data1_11_woman <- data_woman_11$fm

# > Phase 7
load(paste0(path, "\\PHASE_7\\Data\\full_data_7.rda"))
load(paste0(path, "\\PHASE_7\\Data\\data_man_7.rda"))
load(paste0(path, "\\PHASE_7\\Data\\data_woman_7.rda"))

data0_7 <- full_data_7$lm
data1_7 <- full_data_7$fm

data0_7_man <- data_man_7$lm
data1_7_man <- data_man_7$fm

data0_7_woman <- data_woman_7$lm
data1_7_woman <- data_woman_7$fm

# > Phase 3
load(paste0(path, "\\PHASE_3\\Data\\full_data_3.rda"))
load(paste0(path, "\\PHASE_3\\Data\\data_man_3.rda"))
load(paste0(path, "\\PHASE_3\\Data\\data_woman_3.rda"))

data0_3 <- full_data_3$lm
data1_3 <- full_data_3$fm

data0_3_man <- data_man_3$lm
data1_3_man <- data_man_3$fm

data0_3_woman <- data_woman_3$lm
data1_3_woman <- data_woman_3$fm


#--------------------------------------------------------------------
# -------- PART 1 -------- 
# Cross-sectional analyses (associations at phase 11) 

# 1.1 Non stratified analyses

# Linear models (covariates: sex)
# > Models fit
lm1_sb   <- lm(dur_day_total_in_min_pla   ~ A_sex, data = data0_11)
lm1_lipa <- lm(dur_day_total_lig_min_pla  ~ A_sex, data = data0_11)
lm1_mvpa <- lm(dur_day_total_mvpa_min_pla ~ A_sex, data = data0_11)

# > Models outputs 
summary(lm1_sb)
summary(lm1_lipa)
summary(lm1_mvpa)

# > 95%CI for mean time spent in each level 
confint(lm1_sb, level = 0.95)
confint(lm1_lipa, level = 0.95)
confint(lm1_mvpa, level = 0.95)

# Functional model (covariates: sex, age, ethnicity, last occupational position & waking time) 
xi <- data1_11$xi

fm1_min <- pffr(Y_Activity ~ O_waking_time + A_sex + A_age_conti_5 + A_ethnicity + A_socio_eco_cont, 
               yind = xi, 
               bs.yindex = list(bs = "ps", k=-1), 
               bs.int = list(bs = "ps", k = 50),
               data = data1_11)

# > Model outputs 
summary(fm1_min)

#--------------------------------------------------------------------
# - Fully adjusted models
# Linear models
# > Models fit
lm1_full_sb   <- lm(dur_day_total_in_min_pla ~ O_waking_time + 
                      A_sex + A_age_conti_5 + A_ethnicity + A_socio_eco_cont + A_marital_status + 
                      B_ex_smokers + B_current_smokers + B_alc_0 + B_alc_more_14 + B_fg_2 + 
                      C_bmi_overweight + C_bmi_obese + 
                      D_mmm_index + D_pcs + D_mcs, data = data0_11)

lm1_full_lipa <- lm(dur_day_total_lig_min_pla ~ O_waking_time + 
                      A_sex + A_age_conti_5 + A_ethnicity + A_socio_eco_cont + A_marital_status + 
                      B_ex_smokers + B_current_smokers + B_alc_0 + B_alc_more_14 + B_fg_2 + 
                      C_bmi_overweight + C_bmi_obese + 
                      D_mmm_index + D_pcs + D_mcs, data = data0_11)

lm1_full_mvpa <- lm(dur_day_total_mvpa_min_pla ~ O_waking_time + 
                      A_sex + A_age_conti_5 + A_ethnicity + A_socio_eco_cont + A_marital_status + 
                      B_ex_smokers + B_current_smokers + B_alc_0 + B_alc_more_14 + B_fg_2 + 
                      C_bmi_overweight + C_bmi_obese + 
                      D_mmm_index + D_pcs + D_mcs, data = data0_11)

# > Models outputs
summary(lm1_full_sb)
summary(lm1_full_lipa)
summary(lm1_full_mvpa)

# Functional model
# > Model fit
fm1_full <- pffr(Y_Activity ~ O_waking_time + 
                   A_sex + A_age_conti_5 + A_ethnicity + A_socio_eco_cont + A_marital_status + 
                   B_ex_smokers + B_current_smokers + B_alc_0 + B_alc_more_14 + B_fg_2 + 
                   C_bmi_overweight + C_bmi_obese + 
                   D_mmm_index + D_pcs + D_mcs, 
                 yind = xi, 
                 bs.yindex = list(bs = "ps", k=-1), 
                 bs.int = list(bs = "ps", k = 50),
                 data = data1_11)


summary(fm1_full)

#--------------------------------------------------------------------
# - Functional model fully adjusted and additionnally adjusted for interactions between covariates and sex
# Linear models
# > Models fit
lm1_full_int_sb   <- lm(dur_day_total_in_min_pla ~ O_waking_time + 
                      A_sex + A_age_conti_5 + A_ethnicity + A_socio_eco_cont + A_marital_status + 
                      B_ex_smokers + B_current_smokers + B_alc_0 + B_alc_more_14 + B_fg_2 + 
                      C_bmi_overweight + C_bmi_obese + 
                      D_mmm_index + D_pcs + D_mcs + 
                      age_x_sex + sex_x_A_ethnicity + sex_x_A_socio_eco_cont + sex_x_A_marital_status + 
                      sex_x_B_ex_smokers + sex_x_B_current_smokers + sex_x_B_alc_0 + sex_x_B_alc_more_14 + sex_x_B_fg_2 + 
                      sex_x_C_bmi_overweight + sex_x_C_bmi_obese + 
                      sex_x_D_mmm_index + sex_x_D_pcs + sex_x_D_mcs, data = data0_11)

lm1_full_int_lipa <- lm(dur_day_total_lig_min_pla ~ O_waking_time + 
                      A_sex + A_age_conti_5 + A_ethnicity + A_socio_eco_cont + A_marital_status + 
                      B_ex_smokers + B_current_smokers + B_alc_0 + B_alc_more_14 + B_fg_2 + 
                      C_bmi_overweight + C_bmi_obese + 
                      D_mmm_index + D_pcs + D_mcs + 
                      age_x_sex + sex_x_A_ethnicity + sex_x_A_socio_eco_cont + sex_x_A_marital_status + 
                      sex_x_B_ex_smokers + sex_x_B_current_smokers + sex_x_B_alc_0 + sex_x_B_alc_more_14 + sex_x_B_fg_2 + 
                      sex_x_C_bmi_overweight + sex_x_C_bmi_obese + 
                      sex_x_D_mmm_index + sex_x_D_pcs + sex_x_D_mcs, data = data0_11)

lm1_full_int_mvpa <- lm(dur_day_total_mvpa_min_pla ~ O_waking_time + 
                      A_sex + A_age_conti_5 + A_ethnicity + A_socio_eco_cont + A_marital_status + 
                      B_ex_smokers + B_current_smokers + B_alc_0 + B_alc_more_14 + B_fg_2 + 
                      C_bmi_overweight + C_bmi_obese + 
                      D_mmm_index + D_pcs + D_mcs + 
                      age_x_sex + sex_x_A_ethnicity + sex_x_A_socio_eco_cont + sex_x_A_marital_status + 
                      sex_x_B_ex_smokers + sex_x_B_current_smokers + sex_x_B_alc_0 + sex_x_B_alc_more_14 + sex_x_B_fg_2 + 
                      sex_x_C_bmi_overweight + sex_x_C_bmi_obese + 
                      sex_x_D_mmm_index + sex_x_D_pcs + sex_x_D_mcs, data = data0_11)

# > Models outpus
summary(lm1_full_int_sb)
summary(lm1_full_int_lipa)
summary(lm1_full_int_mvpa)

# Functional model
# > Model fit
fm1_full_int <- pffr(Y_Activity ~ O_waking_time + 
                      A_sex + A_age_conti_5 + A_ethnicity + A_socio_eco_cont + A_marital_status + 
                      B_ex_smokers + B_current_smokers + B_alc_0 + B_alc_more_14 + B_fg_2 + 
                      C_bmi_overweight + C_bmi_obese + 
                      D_mmm_index + D_pcs + D_mcs + 
                      age_x_sex + sex_x_A_ethnicity + sex_x_A_socio_eco_cont + sex_x_A_marital_status + 
                      sex_x_B_ex_smokers + sex_x_B_current_smokers + sex_x_B_alc_0 + sex_x_B_alc_more_14 + sex_x_B_fg_2 + 
                      sex_x_C_bmi_overweight + sex_x_C_bmi_obese + 
                      sex_x_D_mmm_index + sex_x_D_pcs + sex_x_D_mcs, 
                    yind = xi, 
                    bs.yindex = list(bs = "ps", k=-1), 
                    bs.int = list(bs = "ps", k = 50),
                    data = data1_11)

# Significativity of interactions with sex
data.frame(var = as.vector(row.names(summary(fm1_full_int)$s.table)), 
           p.value = summary(fm1_full_int)$s.table[,4], row.names = NULL) %>% 
  # Global association significativity 
  mutate(global.sign = if_else(p.value < 0.05, 1, 0)) %>% 
  # Labels 
  mutate(p.value.lab = if_else(p.value < 0.05 & p.value >= 0.01, "*", 
                       if_else(p.value < 0.01 & p.value >= 0.001, "**",
                       if_else(p.value < 0.001 & p.value >= 0, "***", 
                       " ")))) %>% 
  # Keep only significative interactions
  mutate(int = if_else(substr(var, 1,6) == "sex_x_" | var == "age_x_sex", 1, 0)) %>%
  filter(int == 1, global.sign == 1)

# Fit model with only significant interactions
fm1_int <- pffr(Y_Activity ~ O_waking_time + 
                  A_sex + A_age_conti_5 + A_ethnicity + A_socio_eco_cont + # A_marital_status + # (not significant anymore after first run)
                  B_ex_smokers + B_current_smokers + B_alc_0 + B_alc_more_14 + B_fg_2 + 
                  C_bmi_overweight + C_bmi_obese + 
                  D_mmm_index + D_pcs + D_mcs + 
                  sex_x_A_ethnicity + sex_x_A_socio_eco_cont + sex_x_A_marital_status + 
                  sex_x_B_ex_smokers + sex_x_B_current_smokers +  
                  sex_x_C_bmi_obese + 
                  sex_x_D_mcs,
               yind = xi, 
               bs.yindex = list(bs = "ps", k=-1), 
               bs.int = list(bs = "ps", k = 50),
               data = data1_11)


summary(fm1_int)


#--------------------------------------------------------------------
# 1.2. Stratified analyses

# > Full models
vars <- "A_age_conti_5 + A_ethnicity + A_socio_eco_cont + A_marital_status + B_ex_smokers + B_current_smokers + B_alc_0 + B_alc_more_14 + B_fg_2 + C_bmi_overweight + C_bmi_obese + D_mmm_index + D_pcs + D_mcs"

# > Models fitting
m2_11_man   <- flm_fm_fitting(vars = vars, data_lm = data0_11_man,   data_fm = data1_11_man)
m2_11_woman <- flm_fm_fitting(vars = vars, data_lm = data0_11_woman, data_fm = data1_11_woman)

# > Coefficients extraction and integration (for functional models)
coef_m2_11_man <- flm_fm_coef(m2_11_man)
coef_m2_11_woman <- flm_fm_coef(m2_11_woman)

limits.max.plots <- max(abs(coef_m2_11_man$coef_fm$area), abs(coef_m2_11_woman$coef_fm$area))
limits.min.plots <- -limits.max.plots

# > Plots
flm_fm_plot(flm_fm_coef = coef_m2_11_man, limits.min = limits.min.plots, limits.max = limits.max.plots)
flm_fm_plot(flm_fm_coef = coef_m2_11_woman, limits.min = limits.min.plots, limits.max = limits.max.plots)

# > Sequentially adjusted models

# > Socio-demographics
vars_1 <- "A_age_conti_5 + A_ethnicity + A_socio_eco_cont + A_marital_status"
m2_man_1   <- flm_fm_fitting(vars = vars_1, data_lm = data0_11_man,   data_fm = data1_11_man)
m2_woman_1 <- flm_fm_fitting(vars = vars_1, data_lm = data0_11_woman, data_fm = data1_11_woman)
coef_m2_man_1 <- flm_fm_coef(m2_man_1)
coef_m2_woman_1 <- flm_fm_coef(m2_woman_1)

flm_fm_plot(flm_fm_coef = coef_m2_man_1, limits.min = limits.min.plots, limits.max = limits.max.plots) %>% ggsave(filename = "E:\\PC_FIXE\\Analysis\\01_ARTICLE_1\\05_FINAL_ANALYSES_FIGURES_2\\Tables_Figures\\Sequentially adjusted models\\A1_man.png", width = 10, height = 3, dpi = 300)
flm_fm_plot(flm_fm_coef = coef_m2_woman_1, limits.min = limits.min.plots, limits.max = limits.max.plots) %>% ggsave(filename = "E:\\PC_FIXE\\Analysis\\01_ARTICLE_1\\05_FINAL_ANALYSES_FIGURES_2\\Tables_Figures\\Sequentially adjusted models\\A1_woman.png", width = 10, height = 3, dpi = 300)

# > Socio-demographics + Life-style 
vars_2 <- "A_age_conti_5 + A_ethnicity + A_socio_eco_cont + A_marital_status + B_ex_smokers + B_current_smokers + B_alc_0 + B_alc_more_14 + B_fg_2"
m2_man_2   <- flm_fm_fitting(vars = vars_2, data_lm = data0_11_man,   data_fm = data1_11_man)
m2_woman_2 <- flm_fm_fitting(vars = vars_2, data_lm = data0_11_woman, data_fm = data1_11_woman)
coef_m2_man_2 <- flm_fm_coef(m2_man_2)
coef_m2_woman_2 <- flm_fm_coef(m2_woman_2)

flm_fm_plot(flm_fm_coef = coef_m2_man_2, limits.min = limits.min.plots, limits.max = limits.max.plots) %>% ggsave(filename = "E:\\PC_FIXE\\Analysis\\01_ARTICLE_1\\05_FINAL_ANALYSES_FIGURES_2\\Tables_Figures\\Sequentially adjusted models\\A2_man.png", width = 10, height = 5, dpi = 300)
flm_fm_plot(flm_fm_coef = coef_m2_woman_2, limits.min = limits.min.plots, limits.max = limits.max.plots) %>% ggsave(filename = "E:\\PC_FIXE\\Analysis\\01_ARTICLE_1\\05_FINAL_ANALYSES_FIGURES_2\\Tables_Figures\\Sequentially adjusted models\\A2_woman.png", width = 10, height = 5, dpi = 300)

# > Socio-demographics + Life-style + BMI
vars_3 <- "A_age_conti_5 + A_ethnicity + A_socio_eco_cont + A_marital_status + B_ex_smokers + B_current_smokers + B_alc_0 + B_alc_more_14 + B_fg_2 + C_bmi_overweight + C_bmi_obese"
m2_man_3   <- flm_fm_fitting(vars = vars_3, data_lm = data0_11_man,   data_fm = data1_11_man)
m2_woman_3 <- flm_fm_fitting(vars = vars_3, data_lm = data0_11_woman, data_fm = data1_11_woman)
coef_m2_man_3 <- flm_fm_coef(m2_man_3)
coef_m2_woman_3 <- flm_fm_coef(m2_woman_3)

flm_fm_plot(flm_fm_coef = coef_m2_man_3, limits.min = limits.min.plots, limits.max = limits.max.plots) %>% ggsave(filename = "E:\\PC_FIXE\\Analysis\\01_ARTICLE_1\\05_FINAL_ANALYSES_FIGURES_2\\Tables_Figures\\Sequentially adjusted models\\A3_man.png", width = 10, height = 5, dpi = 300)
flm_fm_plot(flm_fm_coef = coef_m2_woman_3, limits.min = limits.min.plots, limits.max = limits.max.plots) %>% ggsave(filename = "E:\\PC_FIXE\\Analysis\\01_ARTICLE_1\\05_FINAL_ANALYSES_FIGURES_2\\Tables_Figures\\Sequentially adjusted models\\A3_woman.png", width = 10, height = 5, dpi = 300)

# > MCS, PCS and mmm index
# Without PCS and MCS
vars <- "A_age_conti_5 + A_ethnicity + A_socio_eco_cont + A_marital_status + B_ex_smokers + B_current_smokers + B_alc_0 + B_alc_more_14 + B_fg_2 + C_bmi_overweight + C_bmi_obese + D_mmm_index"

m2_man_o   <- flm_fm_fitting(vars = vars, data_lm = data0_11_man,   data_fm = data1_11_man)
m2_woman_o <- flm_fm_fitting(vars = vars, data_lm = data0_11_woman, data_fm = data1_11_woman)

# LR tests (if we add PCS and MCS, do we improve goodness of fit?)
lrtest(m2_11_man$fm_fit, m2_man_o$fm_fit)
lrtest(m2_11_woman$fm_fit, m2_woman_o$fm_fit)
# in both cases, p.value < 0.001, so include PCS and MCS significantly increased goodness of fit

# Without MCS
vars <- "A_age_conti_5 + A_ethnicity + A_socio_eco_cont + A_marital_status + B_ex_smokers + B_current_smokers + B_alc_0 + B_alc_more_14 + B_fg_2 + C_bmi_overweight + C_bmi_obese + D_mmm_index + D_pcs"

m2_man_p   <- flm_fm_fitting(vars = vars, data_lm = data0_11_man,   data_fm = data1_11_man)
m2_woman_p <- flm_fm_fitting(vars = vars, data_lm = data0_11_woman, data_fm = data1_11_woman)

# LR tests (if we add MCS, do we improve goodness of fit?)
lrtest(m2_11_man$fm_fit, m2_man_p$fm_fit)
lrtest(m2_11_woman$fm_fit, m2_woman_p$fm_fit)

# Without PCS
vars <- "A_age_conti_5 + A_ethnicity + A_socio_eco_cont + A_marital_status + B_ex_smokers + B_current_smokers + B_alc_0 + B_alc_more_14 + B_fg_2 + C_bmi_overweight + C_bmi_obese + D_mmm_index + D_mcs"

m2_man_m   <- flm_fm_fitting(vars = vars, data_lm = data0_11_man,   data_fm = data1_11_man)
m2_woman_m <- flm_fm_fitting(vars = vars, data_lm = data0_11_woman, data_fm = data1_11_woman)

# LR tests (if we add MCS, do we improve goodness of fit?)
lrtest(m2_11_man$fm_fit, m2_man_m$fm_fit)
lrtest(m2_11_woman$fm_fit, m2_woman_m$fm_fit)

#--------------------------------------------------------------------
# -------- PART 2 -------- 
# Prospective analyses (associations at phase 3 and 7)

#--------------------------------------------------------------------
# 2.1. Non stratified analyses

# - Fully adjusted models
# Linear models
# > Models fit
# phase 7
lm1_full_sb_7   <- lm(dur_day_total_in_min_pla ~ O_waking_time + 
                      A_sex + A_age_conti_5 + A_ethnicity + A_socio_eco_cont + A_marital_status + 
                      B_ex_smokers + B_current_smokers + B_alc_0 + B_alc_more_14 + B_fg_2 + 
                      C_bmi_overweight + C_bmi_obese + 
                      D_mmm_index + D_pcs + D_mcs, data = data0_7)

lm1_full_lipa_7 <- lm(dur_day_total_lig_min_pla ~ O_waking_time + 
                      A_sex + A_age_conti_5 + A_ethnicity + A_socio_eco_cont + A_marital_status + 
                      B_ex_smokers + B_current_smokers + B_alc_0 + B_alc_more_14 + B_fg_2 + 
                      C_bmi_overweight + C_bmi_obese + 
                      D_mmm_index + D_pcs + D_mcs, data = data0_7)

lm1_full_mvpa_7 <- lm(dur_day_total_mvpa_min_pla ~ O_waking_time + 
                      A_sex + A_age_conti_5 + A_ethnicity + A_socio_eco_cont + A_marital_status + 
                      B_ex_smokers + B_current_smokers + B_alc_0 + B_alc_more_14 + B_fg_2 + 
                      C_bmi_overweight + C_bmi_obese + 
                      D_mmm_index + D_pcs + D_mcs, data = data0_7)
# phase 3
lm1_full_sb_3   <- lm(dur_day_total_in_min_pla ~ O_waking_time + 
                        A_sex + A_age_conti_5 + A_ethnicity + A_socio_eco_cont + A_marital_status + 
                        B_ex_smokers + B_current_smokers + B_alc_0 + B_alc_more_14 + B_fg_2 + 
                        C_bmi_overweight + C_bmi_obese + 
                        D_mmm_index + D_pcs + D_mcs, data = data0_3)

lm1_full_lipa_3 <- lm(dur_day_total_lig_min_pla ~ O_waking_time + 
                        A_sex + A_age_conti_5 + A_ethnicity + A_socio_eco_cont + A_marital_status + 
                        B_ex_smokers + B_current_smokers + B_alc_0 + B_alc_more_14 + B_fg_2 + 
                        C_bmi_overweight + C_bmi_obese + 
                        D_mmm_index + D_pcs + D_mcs, data = data0_3)

lm1_full_mvpa_3 <- lm(dur_day_total_mvpa_min_pla ~ O_waking_time + 
                        A_sex + A_age_conti_5 + A_ethnicity + A_socio_eco_cont + A_marital_status + 
                        B_ex_smokers + B_current_smokers + B_alc_0 + B_alc_more_14 + B_fg_2 + 
                        C_bmi_overweight + C_bmi_obese + 
                        D_mmm_index + D_pcs + D_mcs, data = data0_3)


# > Models outpus
summary(lm1_full_sb_7)
summary(lm1_full_lipa_7)
summary(lm1_full_mvpa_7)

summary(lm1_full_sb_3)
summary(lm1_full_lipa_3)
summary(lm1_full_mvpa_3)

# Functional model
# > Model fit
fm1_full_7 <- pffr(Y_Activity ~ O_waking_time + 
                   A_sex + A_age_conti_5 + A_ethnicity + A_socio_eco_cont + A_marital_status + 
                   B_ex_smokers + B_current_smokers + B_alc_0 + B_alc_more_14 + B_fg_2 + 
                   C_bmi_overweight + C_bmi_obese + 
                   D_mmm_index + D_pcs + D_mcs, 
                 yind = xi, 
                 bs.yindex = list(bs = "ps", k=-1), 
                 bs.int = list(bs = "ps", k = 50),
                 data = data1_7)

fm1_full_3 <- pffr(Y_Activity ~ O_waking_time + 
                     A_sex + A_age_conti_5 + A_ethnicity + A_socio_eco_cont + A_marital_status + 
                     B_ex_smokers + B_current_smokers + B_alc_0 + B_alc_more_14 + B_fg_2 + 
                     C_bmi_overweight + C_bmi_obese + 
                     D_mmm_index + D_pcs + D_mcs, 
                   yind = xi, 
                   bs.yindex = list(bs = "ps", k=-1), 
                   bs.int = list(bs = "ps", k = 50),
                   data = data1_3)

summary(fm1_full_7)
summary(fm1_full_3)

# - Functional model fully adjusted and additionnally 
#   adjusted for interactions between covariates and sex

# Linear models
# > Models fit
# phase 7
lm1_full_int_sb_7   <- lm(dur_day_total_in_min_pla ~ O_waking_time + 
                          A_sex + A_age_conti_5 + A_ethnicity + A_socio_eco_cont + A_marital_status + 
                          B_ex_smokers + B_current_smokers + B_alc_0 + B_alc_more_14 + B_fg_2 + 
                          C_bmi_overweight + C_bmi_obese + 
                          D_mmm_index + D_pcs + D_mcs + 
                          age_x_sex + sex_x_A_ethnicity + sex_x_A_socio_eco_cont + sex_x_A_marital_status + 
                          sex_x_B_ex_smokers + sex_x_B_current_smokers + sex_x_B_alc_0 + sex_x_B_alc_more_14 + sex_x_B_fg_2 + 
                          sex_x_C_bmi_overweight + sex_x_C_bmi_obese + 
                          sex_x_D_mmm_index + sex_x_D_pcs + sex_x_D_mcs, data = data0_7)

lm1_full_int_lipa_7 <- lm(dur_day_total_lig_min_pla ~ O_waking_time + 
                          A_sex + A_age_conti_5 + A_ethnicity + A_socio_eco_cont + A_marital_status + 
                          B_ex_smokers + B_current_smokers + B_alc_0 + B_alc_more_14 + B_fg_2 + 
                          C_bmi_overweight + C_bmi_obese + 
                          D_mmm_index + D_pcs + D_mcs + 
                          age_x_sex + sex_x_A_ethnicity + sex_x_A_socio_eco_cont + sex_x_A_marital_status + 
                          sex_x_B_ex_smokers + sex_x_B_current_smokers + sex_x_B_alc_0 + sex_x_B_alc_more_14 + sex_x_B_fg_2 + 
                          sex_x_C_bmi_overweight + sex_x_C_bmi_obese + 
                          sex_x_D_mmm_index + sex_x_D_pcs + sex_x_D_mcs, data = data0_7)

lm1_full_int_mvpa_7 <- lm(dur_day_total_mvpa_min_pla ~ O_waking_time + 
                          A_sex + A_age_conti_5 + A_ethnicity + A_socio_eco_cont + A_marital_status + 
                          B_ex_smokers + B_current_smokers + B_alc_0 + B_alc_more_14 + B_fg_2 + 
                          C_bmi_overweight + C_bmi_obese + 
                          D_mmm_index + D_pcs + D_mcs + 
                          age_x_sex + sex_x_A_ethnicity + sex_x_A_socio_eco_cont + sex_x_A_marital_status + 
                          sex_x_B_ex_smokers + sex_x_B_current_smokers + sex_x_B_alc_0 + sex_x_B_alc_more_14 + sex_x_B_fg_2 + 
                          sex_x_C_bmi_overweight + sex_x_C_bmi_obese + 
                          sex_x_D_mmm_index + sex_x_D_pcs + sex_x_D_mcs, data = data0_7)
# phase 3
lm1_full_int_sb_3   <- lm(dur_day_total_in_min_pla ~ O_waking_time + 
                            A_sex + A_age_conti_5 + A_ethnicity + A_socio_eco_cont + A_marital_status + 
                            B_ex_smokers + B_current_smokers + B_alc_0 + B_alc_more_14 + B_fg_2 + 
                            C_bmi_overweight + C_bmi_obese + 
                            D_mmm_index + D_pcs + D_mcs + 
                            age_x_sex + sex_x_A_ethnicity + sex_x_A_socio_eco_cont + sex_x_A_marital_status + 
                            sex_x_B_ex_smokers + sex_x_B_current_smokers + sex_x_B_alc_0 + sex_x_B_alc_more_14 + sex_x_B_fg_2 + 
                            sex_x_C_bmi_overweight + sex_x_C_bmi_obese + 
                            sex_x_D_mmm_index + sex_x_D_pcs + sex_x_D_mcs, data = data0_3)

lm1_full_int_lipa_3 <- lm(dur_day_total_lig_min_pla ~ O_waking_time + 
                            A_sex + A_age_conti_5 + A_ethnicity + A_socio_eco_cont + A_marital_status + 
                            B_ex_smokers + B_current_smokers + B_alc_0 + B_alc_more_14 + B_fg_2 + 
                            C_bmi_overweight + C_bmi_obese + 
                            D_mmm_index + D_pcs + D_mcs + 
                            age_x_sex + sex_x_A_ethnicity + sex_x_A_socio_eco_cont + sex_x_A_marital_status + 
                            sex_x_B_ex_smokers + sex_x_B_current_smokers + sex_x_B_alc_0 + sex_x_B_alc_more_14 + sex_x_B_fg_2 + 
                            sex_x_C_bmi_overweight + sex_x_C_bmi_obese + 
                            sex_x_D_mmm_index + sex_x_D_pcs + sex_x_D_mcs, data = data0_3)

lm1_full_int_mvpa_3 <- lm(dur_day_total_mvpa_min_pla ~ O_waking_time + 
                            A_sex + A_age_conti_5 + A_ethnicity + A_socio_eco_cont + A_marital_status + 
                            B_ex_smokers + B_current_smokers + B_alc_0 + B_alc_more_14 + B_fg_2 + 
                            C_bmi_overweight + C_bmi_obese + 
                            D_mmm_index + D_pcs + D_mcs + 
                            age_x_sex + sex_x_A_ethnicity + sex_x_A_socio_eco_cont + sex_x_A_marital_status + 
                            sex_x_B_ex_smokers + sex_x_B_current_smokers + sex_x_B_alc_0 + sex_x_B_alc_more_14 + sex_x_B_fg_2 + 
                            sex_x_C_bmi_overweight + sex_x_C_bmi_obese + 
                            sex_x_D_mmm_index + sex_x_D_pcs + sex_x_D_mcs, data = data0_3)


# > Models outpus
summary(lm1_full_int_sb_7)
summary(lm1_full_int_lipa_7)
summary(lm1_full_int_mvpa_7)
summary(lm1_full_int_sb_3)
summary(lm1_full_int_lipa_3)
summary(lm1_full_int_mvpa_3)

# Functional models
# > Model fit
# phase 7
fm1_full_int_7 <- pffr(Y_Activity ~ O_waking_time + 
                       A_sex + A_age_conti_5 + A_ethnicity + A_socio_eco_cont + A_marital_status + 
                       B_ex_smokers + B_current_smokers + B_alc_0 + B_alc_more_14 + B_fg_2 + 
                       C_bmi_overweight + C_bmi_obese + 
                       D_mmm_index + D_pcs + D_mcs + 
                       age_x_sex + sex_x_A_ethnicity + sex_x_A_socio_eco_cont + sex_x_A_marital_status + 
                       sex_x_B_ex_smokers + sex_x_B_current_smokers + sex_x_B_alc_0 + sex_x_B_alc_more_14 + sex_x_B_fg_2 + 
                       sex_x_C_bmi_overweight + sex_x_C_bmi_obese + 
                       sex_x_D_mmm_index + sex_x_D_pcs + sex_x_D_mcs, 
                     yind = xi, 
                     bs.yindex = list(bs = "ps", k=-1), 
                     bs.int = list(bs = "ps", k = 50),
                     data = data1_7)

summary(fm1_full_int_7)

fm1_int_7 <- pffr(Y_Activity ~ O_waking_time + 
                         A_sex + A_age_conti_5 + A_ethnicity + A_socio_eco_cont + A_marital_status + 
                         B_ex_smokers + B_current_smokers + B_alc_0 + B_alc_more_14 + B_fg_2 + 
                         C_bmi_overweight + C_bmi_obese + 
                         D_mmm_index + D_pcs + D_mcs + 
                         sex_x_A_ethnicity + sex_x_A_socio_eco_cont +
                         sex_x_B_ex_smokers + sex_x_B_current_smokers + sex_x_B_alc_0 + 
                         sex_x_C_bmi_overweight +
                         sex_x_D_mmm_index + sex_x_D_mcs, 
                       yind = xi, 
                       bs.yindex = list(bs = "ps", k=-1), 
                       bs.int = list(bs = "ps", k = 50),
                       data = data1_7)
summary(fm1_int_7)
# phase 3
fm1_full_int_3 <- pffr(Y_Activity ~ O_waking_time + 
                         A_sex + A_age_conti_5 + A_ethnicity + A_socio_eco_cont + A_marital_status + 
                         B_ex_smokers + B_current_smokers + B_alc_0 + B_alc_more_14 + B_fg_2 + 
                         C_bmi_overweight + C_bmi_obese + 
                         D_mmm_index + D_pcs + D_mcs + 
                         age_x_sex + sex_x_A_ethnicity + sex_x_A_socio_eco_cont + sex_x_A_marital_status + 
                         sex_x_B_ex_smokers + sex_x_B_current_smokers + sex_x_B_alc_0 + sex_x_B_alc_more_14 + sex_x_B_fg_2 + 
                         sex_x_C_bmi_overweight + sex_x_C_bmi_obese + 
                         sex_x_D_mmm_index + sex_x_D_pcs + sex_x_D_mcs, 
                       yind = xi, 
                       bs.yindex = list(bs = "ps", k=-1), 
                       bs.int = list(bs = "ps", k = 50),
                       data = data1_3)

summary(fm1_full_int_3)


fm1_int_3 <- pffr(Y_Activity ~ O_waking_time + 
                         A_sex + A_age_conti_5 + A_ethnicity + A_socio_eco_cont + A_marital_status + 
                         B_ex_smokers + B_current_smokers + B_alc_0 + B_alc_more_14 + B_fg_2 + 
                         C_bmi_overweight + C_bmi_obese + 
                         D_mmm_index + D_pcs + D_mcs + 
                         sex_x_A_ethnicity + sex_x_A_socio_eco_cont + sex_x_A_marital_status + 
                         #sex_x_B_ex_smokers +  # disapears after first round
                         sex_x_B_current_smokers + sex_x_B_alc_0 +
                         sex_x_C_bmi_overweight + # sex_x_C_bmi_obese + # disapears after first round
                         sex_x_D_mmm_index, 
                       yind = xi, 
                       bs.yindex = list(bs = "ps", k=-1), 
                       bs.int = list(bs = "ps", k = 50),
                       data = data1_3)
summary(fm1_int_3)


#--------------------------------------------------------------------
# 2.2.Stratified analyses

# > Covariates (socio-demographics, behavioral, cardiometabolic and health-related covariates)
vars <- "A_age_conti_5 + A_ethnicity + A_socio_eco_cont + A_marital_status + B_ex_smokers + B_current_smokers + B_alc_0 + B_alc_more_14 + B_fg_2 + C_bmi_overweight + C_bmi_obese + D_mmm_index + D_pcs + D_mcs"

# > Stratified full (linear and functional) models at phase 7
# > Models fitting
m2_7_man   <- flm_fm_fitting(vars = vars, data_lm = data0_7_man,   data_fm = data1_7_man)
m2_7_woman <- flm_fm_fitting(vars = vars, data_lm = data0_7_woman, data_fm = data1_7_woman)

# > Coefficients extraction and integration (for functional models)
coef_m2_7_man <- flm_fm_coef(m2_7_man)
coef_m2_7_woman <- flm_fm_coef(m2_7_woman)

# > Plots
flm_fm_plot(flm_fm_coef = coef_m2_7_man, limits.min = limits.min.plots, limits.max = limits.max.plots)
flm_fm_plot(flm_fm_coef = coef_m2_7_woman, limits.min = limits.min.plots, limits.max = limits.max.plots)


# > Stratified full (linear and functional) models at phase 3
# > Models fitting
m2_3_man   <- flm_fm_fitting(vars = vars, data_lm = data0_3_man,   data_fm = data1_3_man)
m2_3_woman <- flm_fm_fitting(vars = vars, data_lm = data0_3_woman, data_fm = data1_3_woman)

# > Coefficients extraction and integration (for functional models)
coef_m2_3_man <- flm_fm_coef(m2_3_man)
coef_m2_3_woman <- flm_fm_coef(m2_3_woman)

# > Plots
flm_fm_plot(flm_fm_coef = coef_m2_3_man, limits.min = limits.min.plots, limits.max = limits.max.plots) #%>% ggsave(filename = "E:\\PC_FIXE\\Analysis\\01_ARTICLE_1\\05_FINAL_ANALYSES_FIGURES_2\\Tables_Figures\\Transversal associations\\Phase_3_man.png", width = 10, height = 5, dpi = 300)
flm_fm_plot(flm_fm_coef = coef_m2_3_woman, limits.min = limits.min.plots, limits.max = limits.max.plots) #%>% ggsave(filename = "E:\\PC_FIXE\\Analysis\\01_ARTICLE_1\\05_FINAL_ANALYSES_FIGURES_2\\Tables_Figures\\Transversal associations\\Phase_3_woman.png", width = 10, height = 5, dpi = 300)

#--------------------------------------------------------------------
# -------- PART 3 -------- 
# Sensitivity analyses

#--------------------------------------------------------------------
# 3.1. Including past PA 

# > Check if there is missing data for self-reported PA
table(data0_7$B_mvpa_inactive, exclude = NULL)
table(data0_7$B_mvpa_less_2.5, exclude = NULL)
table(data0_3$B_mvpa_inactive, exclude = NULL)
table(data0_3$B_mvpa_less_2.5, exclude = NULL)
# In this case, no change in the population study

# > Covariates (socio-demographics, behavioral, cardiometabolic and health-related covariates)
vars_s1 <- "A_age_conti_5 + A_ethnicity + A_socio_eco_cont + A_marital_status + B_ex_smokers + B_current_smokers + B_alc_0 + B_alc_more_14 + B_fg_2 + B_mvpa_inactive + B_mvpa_less_2.5 + C_bmi_overweight + C_bmi_obese + D_mmm_index + D_pcs + D_mcs"

# > Stratified full (linear and functional) models at 
# phase 7
# > Models fitting
m2_7_man_s1   <- flm_fm_fitting(vars = vars_s1, data_lm = data0_7_man,   data_fm = data1_7_man)
m2_7_woman_s1 <- flm_fm_fitting(vars = vars_s1, data_lm = data0_7_woman, data_fm = data1_7_woman)

# > Coefficients extraction and integration (for functional models)
coef_m2_7_man_s1 <- flm_fm_coef(m2_7_man_s1)
coef_m2_7_woman_s1 <- flm_fm_coef(m2_7_woman_s1)

# phase 3
# > Models fitting
m2_3_man_s1   <- flm_fm_fitting(vars = vars_s1, data_lm = data0_3_man,   data_fm = data1_3_man)
m2_3_woman_s1 <- flm_fm_fitting(vars = vars_s1, data_lm = data0_3_woman, data_fm = data1_3_woman)

# > Coefficients extraction and integration (for functional models)
coef_m2_3_man_s1 <- flm_fm_coef(m2_3_man_s1)
coef_m2_3_woman_s1 <- flm_fm_coef(m2_3_woman_s1)

# > Model comparison
# men
lrtest(m2_7_man_s1$fm_fit, m2_7_man$fm_fit)
lrtest(m2_3_man_s1$fm_fit, m2_3_man$fm_fit)
# women
lrtest(m2_7_woman_s1$fm_fit, m2_7_woman$fm_fit)
lrtest(m2_3_woman_s1$fm_fit, m2_3_woman$fm_fit)
# p < 2.2e-16: adding PA improves the goodness of fit

#--------------------------------------------------------------------
# 3.2. Study sample limited to participants with data for all phases

# Data
path <- "E:\\PC_FIXE\\Analysis\\01_ARTICLE_1\\05_FINAL_ANALYSES_FIGURES_2\\ALL_PHASES\\Data\\"

# > Phase 11
load(paste0(path, "full_data_11.rda"))
load(paste0(path, "full_data_man_11.rda"))
load(paste0(path, "full_data_woman_11.rda"))

data0_11_s2 <- data_11_full$lm
data1_11_s2 <- data_11_full$fm

data0_11_man_s2 <- data_man_11_full$lm
data1_11_man_s2 <- data_man_11_full$fm

data0_11_woman_s2 <- data_woman_11_full$lm
data1_11_woman_s2 <- data_woman_11_full$fm

# > Phase 7
load(paste0(path, "full_data_7.rda"))
load(paste0(path, "full_data_man_7.rda"))
load(paste0(path, "full_data_woman_7.rda"))

data0_7_s2 <- data_7_full$lm
data1_7_s2 <- data_7_full$fm

data0_7_man_s2 <- data_man_7_full$lm
data1_7_man_s2 <- data_man_7_full$fm

data0_7_woman_s2 <- data_woman_7_full$lm
data1_7_woman_s2 <- data_woman_7_full$fm

# > Phase 3
load(paste0(path, "full_data_3.rda"))
load(paste0(path, "full_data_man_3.rda"))
load(paste0(path, "full_data_woman_3.rda"))

data0_3_s2 <- data_3_full$lm
data1_3_s2 <- data_3_full$fm

data0_3_man_s2 <- data_man_3_full$lm
data1_3_man_s2 <- data_man_3_full$fm

data0_3_woman_s2 <- data_woman_3_full$lm
data1_3_woman_s2 <- data_woman_3_full$fm

# --------
# Model fitting
# > Full models
vars <- "A_age_conti_5 + A_ethnicity + A_socio_eco_cont + A_marital_status + B_ex_smokers + B_current_smokers + B_alc_0 + B_alc_more_14 + B_fg_2 + C_bmi_overweight + C_bmi_obese + D_mmm_index + D_pcs + D_mcs"

# Phase 11
# > Models fitting
m2_11_man_s2   <- flm_fm_fitting(vars = vars, data_lm = data0_11_man_s2,   data_fm = data1_11_man_s2)
m2_11_woman_s2 <- flm_fm_fitting(vars = vars, data_lm = data0_11_woman_s2, data_fm = data1_11_woman_s2)

# > Coefficients extraction and integration (for functional models)
coef_m2_11_man_s2   <- flm_fm_coef(m2_11_man_s2)
coef_m2_11_woman_s2 <- flm_fm_coef(m2_11_woman_s2)

# Phase 7
# > Models fitting
m2_7_man_s2   <- flm_fm_fitting(vars = vars, data_lm = data0_7_man_s2,   data_fm = data1_7_man_s2)
m2_7_woman_s2 <- flm_fm_fitting(vars = vars, data_lm = data0_7_woman_s2, data_fm = data1_7_woman_s2)

# > Coefficients extraction and integration (for functional models)
coef_m2_7_man_s2   <- flm_fm_coef(m2_7_man_s2)
coef_m2_7_woman_s2 <- flm_fm_coef(m2_7_woman_s2)

# Phase 3
# > Models fitting
m2_3_man_s2   <- flm_fm_fitting(vars = vars, data_lm = data0_3_man_s2,   data_fm = data1_3_man_s2)
m2_3_woman_s2 <- flm_fm_fitting(vars = vars, data_lm = data0_3_woman_s2, data_fm = data1_3_woman_s2)

# > Coefficients extraction and integration (for functional models)
coef_m2_3_man_s2   <- flm_fm_coef(m2_3_man_s2)
coef_m2_3_woman_s2 <- flm_fm_coef(m2_3_woman_s2)

#--------------------------------------------------------------------
# 3.3. Including social interactions

# > Check if there is missing data for social interaction
summary(data0_11_man$B_social_act)
summary(data0_11_woman$B_social_act)

# > Covariates (socio-demographics, behavioral, cardiometabolic and health-related covariates)
vars_s3 <- "A_age_conti_5 + A_ethnicity + A_socio_eco_cont + A_marital_status + B_ex_smokers + B_current_smokers + B_alc_0 + B_alc_more_14 + B_social_act + B_fg_2 + C_bmi_overweight + C_bmi_obese + D_mmm_index + D_pcs + D_mcs"

# > Stratified full (linear and functional) models at phase 11
# > Models fitting
m2_11_man_s3   <- flm_fm_fitting(vars = vars_s3, data_lm = data0_11_man,   data_fm = data1_11_man)
m2_11_woman_s3 <- flm_fm_fitting(vars = vars_s3, data_lm = data0_11_woman, data_fm = data1_11_woman)

# > Coefficients extraction and integration (for functional models)
coef_m2_11_man_s3 <- flm_fm_coef(m2_11_man_s3)
coef_m2_11_woman_s3 <- flm_fm_coef(m2_11_woman_s3)

# > Model comparison
# men
AIC(m2_11_man_s3$fm_fit)
AIC(m2_11_man$fm_fit)
BIC(m2_11_man_s3$fm_fit)
BIC(m2_11_man$fm_fit)

# women
AIC(m2_11_woman_s3$fm_fit)
AIC(m2_11_woman$fm_fit)
BIC(m2_11_woman_s3$fm_fit)
BIC(m2_11_woman$fm_fit)

# > For both sex, models including social interactions
# show lower AIC and BIC 

stop()
# > plots
flm_fm_plot(flm_fm_coef = coef_m2_11_man_s3, limits.min = limits.min.plots, limits.max = limits.max.plots) 
flm_fm_plot(flm_fm_coef = coef_m2_11_woman_s3, limits.min = limits.min.plots, limits.max = limits.max.plots)

library(cowplot)
plot_grid(
        coef_m2_11_man$coef_fm %>% 
          filter(var %in% c("B_alc_0(xi)", "B_alc_more_14(xi)")) %>% 
          ggplot(.) + 
          geom_hline(yintercept = 0, color = "red", lty = 2) + 
          geom_ribbon(aes(x = xi, ymin = ic_down, ymax = ic_up), fill = "lightgrey", alpha = 0.5) +  
          geom_line(aes(x = xi, y = value)) + 
          facet_grid(. ~ var) + 
          ggtitle("Without social interactions") + 
          labs(x = "log(acc + 1)", y = "Value") +
          theme_bw(),
          
          coef_m2_11_man_s3$coef_fm %>% 
            filter(var %in% c("B_alc_0(xi)", "B_alc_more_14(xi)")) %>% 
            ggplot(.) + 
            geom_hline(yintercept = 0, color = "red", lty = 2) + 
            geom_ribbon(aes(x = xi, ymin = ic_down, ymax = ic_up), fill = "lightgrey", alpha = 0.5) +  
            geom_line(aes(x = xi, y = value)) + 
            facet_grid(. ~ var) + 
            ggtitle("With social interactions") + 
            labs(x = "log(acc + 1)", y = "Value") +
            theme_bw(), 
          ncol = 1)

coef_m2_11_man$coef_fm %>% 
  filter(var %in% c("B_alc_0(xi)", "B_alc_more_14(xi)")) %>% 
  mutate(model = "without social interaction") %>% 
  rbind(coef_m2_11_man_s3$coef_fm %>% 
          filter(var %in% c("B_alc_0(xi)", "B_alc_more_14(xi)")) %>% 
          mutate(model = "with social interaction")) %>%
  ggplot(.) + 
  geom_vline(aes(xintercept = log(1+0.04)), color = "black") +
  geom_vline(aes(xintercept = log(1+0.1)), color = "black") +
  geom_hline(yintercept = 0, color = "black") + 
  geom_ribbon(aes(x = xi, ymin = ic_down, ymax = ic_up, fill = model), alpha = 0.3) +  
  geom_line(aes(x = xi, y = value, linetype = model, color = model), size = 0.8) + 
  facet_grid(. ~ var) + 
  labs(x = "log(acc + 1)", y = "Value") +
  theme_bw() + 
  theme(legend.position = "bottom") + 
  lims(y = c(-1000, 1000))



coef_m2_11_man_s3$coef_fm %>% 
  filter(var %in% c("B_alc_0(xi)", "B_alc_more_14(xi)")) %>% 
  ggplot(.) + 
  geom_col(aes(x = xi, y = area)) + 
  facet_grid(. ~ var) + 
  ggtitle("Functional coefficients integrated over 0.005 g increments") + 
  labs(x = "log(acc + 1)", y = "Time (minutes)") + 
  theme_bw()
