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

# Functional model (covariates: sex, age, ethnicity, last occupational position & waking time) 
xi <- data1_11$xi

fm0_min <- pffr(Y_Activity ~ A_sex, 
                yind = xi, 
                bs.yindex = list(bs = "ps", k=-1), 
                bs.int = list(bs = "ps", k = 50),
                data = data1_11)

fm1_min <- pffr(Y_Activity ~ O_waking_time + A_sex + A_age_conti_5 + A_ethnicity + A_socio_eco_cont, 
                yind = xi, 
                bs.yindex = list(bs = "ps", k=-1), 
                bs.int = list(bs = "ps", k = 50),
                data = data1_11)
fm1_min2 <- pffr(Y_Activity ~ O_waking_time + A_sex + A_age_conti_5 + A_ethnicity, 
                yind = xi, 
                bs.yindex = list(bs = "ps", k=-1), 
                bs.int = list(bs = "ps", k = 50),
                data = data1_11)

m0_11 <- flm_fm_fitting(vars = "A_sex", data_lm = data0_11, data_fm = data1_11)

m1_11 <- flm_fm_fitting(vars = "A_sex + A_age_conti_5 + A_ethnicity", data_lm = data0_11, data_fm = data1_11)
summary(m1_11$lm_fit$SB)
confint(m1_11$lm_fit$SB, level = 0.95)
confint(m1_11$lm_fit$LIPA, level = 0.95)
confint(m1_11$lm_fit$MVPA, level = 0.95)

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

# p values
sum_man_11   <- summary(m2_11_man$fm_fit)
sum_man_11_tab <- data.frame(sum_man_11$s.table) %>% 
  mutate(p.value2 = round(p.value, 3))


sum_woman_11 <- summary(m2_11_woman$fm_fit)
sum_woman_11_tab <- data.frame(sum_woman_11$s.table) %>% 
  mutate(p.value2 = round(p.value, 3))
#--------------------------------------------------------------------
# -------- PART 2 -------- 
# Prospective analyses (associations at phase 3 and 7)

#--------------------------------------------------------------------
# 2.2.Stratified analyses

# > Stratified full (linear and functional) models at phase 7
# > Models fitting
m2_7_man   <- flm_fm_fitting(vars = vars, data_lm = data0_7_man,   data_fm = data1_7_man)
m2_7_woman <- flm_fm_fitting(vars = vars, data_lm = data0_7_woman, data_fm = data1_7_woman)

# > Coefficients extraction and integration (for functional models)
coef_m2_7_man <- flm_fm_coef(m2_7_man)
coef_m2_7_woman <- flm_fm_coef(m2_7_woman)

# p values
sum_man_7   <- summary(m2_7_man$fm_fit)
sum_man_7_tab <- data.frame(sum_man_7$s.table) %>% 
  mutate(p.value2 = round(p.value, 3))


sum_woman_7 <- summary(m2_7_woman$fm_fit)
sum_woman_7_tab <- data.frame(sum_woman_7$s.table) %>% 
  mutate(p.value2 = round(p.value, 3))

# > Stratified full (linear and functional) models at phase 3
# > Models fitting
m2_3_man   <- flm_fm_fitting(vars = vars, data_lm = data0_3_man,   data_fm = data1_3_man)
m2_3_woman <- flm_fm_fitting(vars = vars, data_lm = data0_3_woman, data_fm = data1_3_woman)

# > Coefficients extraction and integration (for functional models)
coef_m2_3_man <- flm_fm_coef(m2_3_man)
coef_m2_3_woman <- flm_fm_coef(m2_3_woman)

# p values
sum_man_3   <- summary(m2_3_man$fm_fit)
sum_man_3_tab <- data.frame(sum_man_3$s.table) %>% 
  mutate(p.value2 = round(p.value, 3))


sum_woman_3 <- summary(m2_3_woman$fm_fit)
sum_woman_3_tab <- data.frame(sum_woman_3$s.table) %>% 
  mutate(p.value2 = round(p.value, 3))

# Power analysis

library("sensemakr")
library("WebPower")

vars_indiv <- scan(text = vars, what = "", sep = "+")
list.wp.p <- list()

tab_power_w <- list(
  phase_11 = m2_11_woman, 
  phase_7 = m2_7_woman,
  phase_3 = m2_3_woman) %>% 
  
  map_dfr(., ~{ 
    
    list.wp <- list()
    
    for(v in vars_indiv)
    {
      
      n1 <- length(.x$lm_fit$SB$effects) # sample size
      cat(paste0(n1, "\n"))
      f2_sb_v   <- as.numeric(as.character(partial_f2(.x$lm_fit$SB, covariates = gsub(" ", "", v))))         # effect size for the covariate for SB
      f2_lipa_v <- as.numeric(as.character(partial_f2(.x$lm_fit$LIPA, covariates = gsub(" ", "", v))))     # effect size for the covariate for LIPA
      f2_mvpa_v <- as.numeric(as.character(partial_f2(.x$lm_fit$MVPA, covariates = gsub(" ", "", v))))     # effect size for the covariate for MVPA
      
      p1 <- length(.x$lm_fit$SB$coefficients)-1 # nb of covariates (nb of coef - 1 for the intercept)
      p2 <- p1-1
      
      # Compute sample size needed to obtain 80% power
      #wp.sb   <- wp.regression(n=NULL, p1 = p1, p2 = p2, f2 = f2_sb_v, alpha = 0.05, power = 0.8)
      #wp.lipa <- wp.regression(n=NULL, p1 = p1, p2 = p2, f2 = f2_lipa_v, alpha = 0.05, power = 0.8)
      #wp.mvpa <- wp.regression(n=NULL, p1 = p1, p2 = p2, f2 = f2_mvpa_v, alpha = 0.05, power = 0.8)
      
      # Compute power of current analyses
      wp.sb2   <- wp.regression(n=n1, p1 = p1, p2 = p2, f2 = f2_sb_v, alpha = 0.05, power = NULL)
      wp.lipa2 <- wp.regression(n=n1, p1 = p1, p2 = p2, f2 = f2_lipa_v, alpha = 0.05, power = NULL)
      wp.mvpa2 <- wp.regression(n=n1, p1 = p1, p2 = p2, f2 = f2_mvpa_v, alpha = 0.05, power = NULL)
      
      out <- data.frame(
        #wp.n = c(wp.sb$n, wp.lipa$n, wp.mvpa$n),
        wp.power = c(wp.sb2$power, wp.lipa2$power, wp.mvpa2$power),
        act = c("SB", "LIPA", "MVPA")
      )
      
      list.wp[[paste0(v)]]<- out
      
    }
    
    plyr::ldply(list.wp, data.frame, .id ="var")
    
    }, .id="phase") %>% mutate(sex="Women")
  
# in men
tab_power_m <- list(
  phase_11 = m2_11_man, 
  phase_7 = m2_7_man,
  phase_3 = m2_3_man) %>% 
  
  map_dfr(., ~{ 
    
    list.wp <- list()
    
    for(v in vars_indiv)
    {
      
      n1 <- length(.x$lm_fit$SB$effects) # sample size
      cat(paste0(n1, "\n"))
      f2_sb_v   <- as.numeric(as.character(partial_f2(.x$lm_fit$SB, covariates = gsub(" ", "", v))))         # effect size for the covariate for SB
      f2_lipa_v <- as.numeric(as.character(partial_f2(.x$lm_fit$LIPA, covariates = gsub(" ", "", v))))     # effect size for the covariate for LIPA
      f2_mvpa_v <- as.numeric(as.character(partial_f2(.x$lm_fit$MVPA, covariates = gsub(" ", "", v))))     # effect size for the covariate for MVPA
      
      p1 <- length(.x$lm_fit$SB$coefficients)-1 # nb of covariates (nb of coef - 1 for the intercept)
      p2 <- p1-1
      
      # Compute sample size needed to obtain 80% power
      #wp.sb   <- wp.regression(n=NULL, p1 = p1, p2 = p2, f2 = f2_sb_v, alpha = 0.05, power = 0.8)
      #wp.lipa <- wp.regression(n=NULL, p1 = p1, p2 = p2, f2 = f2_lipa_v, alpha = 0.05, power = 0.8)
      #wp.mvpa <- wp.regression(n=NULL, p1 = p1, p2 = p2, f2 = f2_mvpa_v, alpha = 0.05, power = 0.8)
      
      # Compute power of current analyses
      wp.sb2   <- wp.regression(n=n1, p1 = p1, p2 = p2, f2 = f2_sb_v, alpha = 0.05, power = NULL)
      wp.lipa2 <- wp.regression(n=n1, p1 = p1, p2 = p2, f2 = f2_lipa_v, alpha = 0.05, power = NULL)
      wp.mvpa2 <- wp.regression(n=n1, p1 = p1, p2 = p2, f2 = f2_mvpa_v, alpha = 0.05, power = NULL)
      
      out <- data.frame(
        #wp.n = c(wp.sb$n, wp.lipa$n, wp.mvpa$n),
        wp.power = c(wp.sb2$power, wp.lipa2$power, wp.mvpa2$power),
        act = c("SB", "LIPA", "MVPA")
      )
      
      list.wp[[paste0(v)]]<- out
      
    }
    
    plyr::ldply(list.wp, data.frame, .id ="var")
    
  }, .id="phase") %>% mutate(sex="Men")

tab_power <- rbind(tab_power_m, tab_power_w) %>% 
  mutate(sex = factor(sex, levels = c("Men", "Women")), 
         act = factor(act, levels = c("SB", "LIPA", "MVPA")),
         phase = factor(phase, levels = c("phase_3", "phase_7", "phase_11")))

tab_power%>% 
  group_by(sex, phase, act) %>% 
  summarise(min(wp.power), max(wp.power))

tab_power %>% 
  arrange(sex, phase, act, wp.power) %>%
  group_by(phase, act) %>% 
  slice(c(1, 2, n()-1, n())) %>% 
  split(.$phase)


tab_power %>% 
  mutate(phase=recode(phase, "phase_3"="1991-1993", "phase_7"="2002-2004", "phase_11"="2012-2013")) %>% 
  ggplot(., aes(x = wp.power, y = act, fill=sex)) + 
  geom_boxplot() + 
  #geom_jitter(aes(col = if_else(wp.power<0.25, var))) + 
  geom_vline(xintercept=0.8, color ="red", lty=2)+
  theme_cowplot() + theme(plot.caption = element_text(size = 10, color = "black", hjust = 0)) +
  coord_flip() + 
  scale_fill_manual(values=c("grey", "grey50"), name="Sex") +
  facet_grid(. ~phase) +
  labs(y="", x="Power",
       caption = "Abbrevations: SB: sedentary behavior; LIPA: light-intensity physical activity;\nMVPA: moderate-to-vigorous physical activity.")

tab_power %>% 
  mutate(var=paste0(str_trim(var),"(xi)")) %>%
  left_join(table.name, by = "var") %>%
  mutate(var.group=factor(var.group, levels=c("Socio-\ndemographic\nfactors", "Behavioral\nfactors", "Health-related\nfactors"))) %>%
  ggplot(., aes(x=wp.power, y = act, fill=sex)) + 
  geom_boxplot() +
  facet_grid(.~var.group) + 
  geom_vline(xintercept=0.8, color ="red", lty=2)+
  theme_cowplot() + theme(plot.caption = element_text(size = 10, color = "black", hjust = 0)) +
  coord_flip()+
  scale_fill_manual(values=c("grey", "grey50"), name="Sex") +
  labs(y="", x="Power",
       caption = "Abbrevations: SB: sedentary behavior; LIPA: light-intensity physical activity;\nMVPA: moderate-to-vigorous physical activity.")



#--------------------------------------------------------------------
# -------- PART 3 -------- 
# Sensitivity analyses

#--------------------------------------------------------------------
# 3.1. Including past PA 

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

# > Covariates (socio-demographics, behavioral, cardiometabolic and health-related covariates)
vars_s3 <- "A_age_conti_5 + A_ethnicity + A_socio_eco_cont + A_marital_status + B_ex_smokers + B_current_smokers + B_alc_0 + B_alc_more_14 + B_social_act + B_fg_2 + C_bmi_overweight + C_bmi_obese + D_mmm_index + D_pcs + D_mcs"

# > Stratified full (linear and functional) models at phase 11
# > Models fitting
m2_11_man_s3   <- flm_fm_fitting(vars = vars_s3, data_lm = data0_11_man,   data_fm = data1_11_man)
m2_11_woman_s3 <- flm_fm_fitting(vars = vars_s3, data_lm = data0_11_woman, data_fm = data1_11_woman)

# > Coefficients extraction and integration (for functional models)
coef_m2_11_man_s3 <- flm_fm_coef(m2_11_man_s3)
coef_m2_11_woman_s3 <- flm_fm_coef(m2_11_woman_s3)







