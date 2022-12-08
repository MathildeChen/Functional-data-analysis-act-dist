# Script name: 01_3_ACTIVITY_DISTRIBUTION.R
# 
# Author: M.Chen, Inserm
#
# Doing: Compute activity distribution from mean daily waking time and density functions
#--------------------------------------------------------------------

# Features
Sys.setlocale("LC_ALL","English")

# Packages
# > Tools
library(tidyr)
library(plyr)
library(dplyr)
library(purrr)
library(stringr)
library(haven)

#--------------------------------------------------------------------
# Data loading

# > Intensity density functions
load("E:\\PC_FIXE\\Data\\02_PA_DIST\\2020-05-15_dst_4006_trap.rda")

dst_2 <- dst_2 %>%
  select(-f_i, -surf) 

# > Person summary = Data set containing the correct set of patients (n = 4006). One line = patient
#   Contains mean daily waking time 
stata_data <- read_dta("E:\\PC_FIXE\\Data\\04_DAILY_ACT_SUM\\2020-05-15\\WW_L40M100V400_update052020.dta") %>% 
  filter(exclusion == 0)
#--------------------------------------------------------------------
# Merge density functions data and mean waking time 
# and compute participants activity distribution

act <- dst_2 %>% 
  mutate(stno = as.numeric(as.character(stno))) %>%
  left_join(stata_data %>% select(stno, dur_day_min_pla, dur_day_total_in_min_pla, dur_day_total_lig_min_pla, dur_day_total_mod_min_pla, dur_day_total_vig_min_pla, dur_day_mvpa_bts_10_min_pla), 
            by = "stno") %>% 
  mutate(dur_day_total_mvpa_min_pla = dur_day_total_mod_min_pla + dur_day_total_vig_min_pla, 
         A_i = f_i_2*dur_day_min_pla) %>%
  select(-dur_day_total_mod_min_pla, -dur_day_total_vig_min_pla)

#--------------------------------------------------------------------
# Save
save(act, file = "E:\\PC_FIXE\\Analysis\\01_ARTICLE_1\\02_FUNCTION_ON_SCALAR\\test_refund\\data\\2020-05-15_act_4006_trap.rda")




