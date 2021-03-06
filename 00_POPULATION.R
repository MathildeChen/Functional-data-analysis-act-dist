# Script name: 00_POPULATION.R
# 
# Author: M.Chen, Inserm
#
# Doing: Merge covariates dataset from phase 3 (1991-1994), 7 (2002-2004) 
# and 11 (2012-2013)
#
#--------------------------------------------------------------------
# Packages
# > Tools
library(tidyr)
library(plyr)
library(dplyr)

#--------------------------------------------------------------------
# Data
# > Phase 11
load("E:\\PC_FIXE\\Analysis\\01_ARTICLE_1\\05_FINAL_ANALYSES_FIGURES_2\\PHASE_11\\Data\\tab_full_cov_s11.rda")
s11 <- tab_11 %>% 
  select(stno, sex_2, fage_s, ethnicity_i, fstatusx_i_2, flgrlump_i_ordinal, 
         ex_smoker, current_smoker, alc_0, alc_more_14, fg_2, 
         bmi_overweight, bmi_obese, 
         mental_condition, physical_condition, mmm_index) %>% 
  rename_at(vars(-stno, -fage_s, -fstatusx_i_2, -flgrlump_i_ordinal), ~ paste0("f", .))

# > Phase 7
load("E:\\PC_FIXE\\Analysis\\01_ARTICLE_1\\05_FINAL_ANALYSES_FIGURES_2\\PHASE_7\\Data\\tab_full_cov_s7.rda") 
s7 <- tab_7 %>% 
  select(stno, sex_2, mage_s_i, ethnicity_i, mstatusx_i_2, mlgrlump_i_ordinal, 
         ex_smoker, current_smoker, alc_0, alc_more_14, fg_2, 
         bmi_overweight, bmi_obese, 
         mental_condition, physical_condition, mmm_index) %>% 
  rename_at(vars(-stno, -mage_s_i, -mstatusx_i_2, -mlgrlump_i_ordinal), ~ paste0("m", .))

# > Phase 3
load("E:\\PC_FIXE\\Analysis\\01_ARTICLE_1\\05_FINAL_ANALYSES_FIGURES_2\\PHASE_3\\Data\\tab_full_cov_s3.rda")
s3 <- tab_3 %>% 
  select(stno, sex_2, xage_s_i, ethnicity_i, xstatusx_i_2, xgrlump_i_ordinal, 
         ex_smoker, current_smoker, alc_0, alc_more_14, fg_2, 
         bmi_overweight, bmi_obese, 
         mental_condition, physical_condition, mmm_index) %>% 
  rename_at(vars(-stno, -xage_s_i, -xstatusx_i_2, -xgrlump_i_ordinal), ~ paste0("x", .))

#--------------------------------------------------------------------
# Merge dataframes
# > Three phases
TAB <- s11 %>% 
  left_join(s7, by = "stno") %>% 
  left_join(s3, by = "stno")

# > Check not changing socio-demographics
testthat::expect_equal(TAB$fsex_2, TAB$msex_2, TAB$xsex_2)
testthat::expect_equal(TAB$fethnicity_i, TAB$methnicity_i, TAB$xethnicity_i)

full_TAB <- TAB %>% 
  select(-msex_2, -xsex_2, -methnicity_i, -xethnicity_i) %>% 
  rename("sex_2" = "fsex_2", "ethnicity_i" = "fethnicity_i")

# > Check the N
full_TAB %>% 
  drop_na() %>% 
  dim(.)

#--------------------------------------------------------------------
# > Save all covariables table
save(full_TAB, 
     file = "E:\\PC_FIXE\\Analysis\\01_ARTICLE_1\\05_FINAL_ANALYSES_FIGURES_2\\ALL_PHASES\\Data\\tab_full_cov.rda")

# > Save data without NA
TAB <- full_TAB %>% 
  drop_na()

table(TAB$sex_2, exclude = NULL)

save(TAB, 
     file = "E:\\PC_FIXE\\Analysis\\01_ARTICLE_1\\05_FINAL_ANALYSES_FIGURES_2\\ALL_PHASES\\Data\\tab_cov.rda")



