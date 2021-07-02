# Script name: 02_TABLES_FIGURES.R
# 
# Author: M.Chen, Inserm
#
# Doing: Generating tables and plots of article 1
# 
# --- TABLES ---
# Table 1: Characteristics of the study population in 2012-2013

# --- FIGURES ---
# Figure 1: PA distribution by sex and coefficient for sex (from minimally adjusted functional model)
# Figure 2: Associations between covariates and PA at phase 11 for men
# Figure 3: Associations between covariates and PA at phase 11 for women
# Figure 4: Associations through the follow-up between covariates (at phases 3, 7 and 11) and PA (at phase 11) for men
# Figure 5: Associations through the follow-up between covariates (at phases 3, 7 and 11) and PA (at phase 11) for women

# --- SUPPLEMENTARY ---
# Supplementary Figure 1: Flowchart (not in this script)
# Supplementary Table 1: Characteristics of the study population in 1991-1994 and in 2002-2004
# Supplementary Table 2: Interactions of sex with sociodemographic, lifestyle and health-related factors in 1991-1994, 2002-2004 and 2012-2013. 
# Supplementary Table 3: Associations through the follow-up between covariates and PA at phase 11 in men (linear coefficients)
# Supplementary Table 4: Associations through the follow-up between covariates and PA at phase 11 in women (linear coefficients)
# Supplementary Figure 2: Association between sociodemographic, lifestyle and health-related factors in 2012-2013 with acceleration distribution in 2012-2013 in men and women.
# Supplementary Figure 3: Association between sociodemographic, lifestyle and health-related factors in 1991-1994 and in 2002-2004 with acceleration in men, estimated using functional regression adjusted on all covariates, waking time and on self-reported MVPA
# Supplementary Figure 4: Association between sociodemographic, lifestyle and health-related factors in 1991-1994 and in 2002-2004 with acceleration in women, estimated using functional regression adjusted on all covariates, waking time and on self-reported MVPA. 
# Supplementary Figure 5: Association between sociodemographic, lifestyle and health-related factors in 1991-1994, 2002-2004 and 2012-2013 with acceleration in men with all data at each time point (n = 2688)
# Supplementary Figure 6: Association between sociodemographic, lifestyle and health-related factors in 1991-1994, 2002-2004 and 2012-2013 with acceleration in women with all data at each time point (n = 891)
# Supplementary Figure 7: Association between sociodemographic, lifestyle and health-related factors with acceleration in men, estimated using functional regression adjusted on all covariates, waking time and on social interactions
# Supplementary Figure 8: Association between sociodemographic, lifestyle and health-related factors with acceleration in women, estimated using functional regression adjusted on all covariates, waking time and on social interactions

# Path for saving tables and plots 
path <- "E:\\PC_FIXE\\Analysis\\01_ARTICLE_1\\05_FINAL_ANALYSES_FIGURES_2\\Tables_Figures\\"

#--------------------------------------------------------------------
# Packages
# > Graphics
library(ggplot2)
library(cowplot)
library(wesanderson)
library(xlsx)

# > Colors palette
pal <- wes_palette(name = "Zissou1", 5, "discrete")
pal_bicolor <- pal[c(1,5)]

#--------------------------------------------------------------------
# Data
# > Characteristics of the study population 
#   in 2012-2013
load("E:\\PC_FIXE\\Analysis\\01_ARTICLE_1\\05_FINAL_ANALYSES_FIGURES_2\\PHASE_11\\Data\\tab_cov_s11.rda")
#   in 2002-2004
load("E:\\PC_FIXE\\Analysis\\01_ARTICLE_1\\05_FINAL_ANALYSES_FIGURES_2\\PHASE_7\\Data\\tab_cov_s7.rda")
#   in 1991-1994
load("E:\\PC_FIXE\\Analysis\\01_ARTICLE_1\\05_FINAL_ANALYSES_FIGURES_2\\PHASE_3\\Data\\tab_cov_s3.rda")

# > Models
source("E:\\PC_FIXE\\Analysis\\01_ARTICLE_1\\05_FINAL_ANALYSES_FIGURES_2\\03_MODELS.R")

#--------------------------------------------------------------------
# --- Table 1 --- 
# Population characteristics at phase 11

# > Categorical and binary variables
t1 <- tab_11_fin %>% 
  select(stno, sex_2, ethnicity_i, flgrlump_i_ordinal, fstatusx_i_2, 
         fesmoke_i, funitwk0_i_3, fg_2, 
         fbmi_i_3) %>%
  # Mise en forme du tableau
  mutate(sex_2 = if_else(sex_2 == 0, "Women", "Men"),
         ethnicity_i = if_else(ethnicity_i == 0, "White", "Non-white"),
         flgrlump_i_ordinal = if_else(flgrlump_i_ordinal == 0, "Administrative", if_else(flgrlump_i_ordinal == 0.5, "Prof/exec", "Clerical/support")), 
         fstatusx_i_2 = if_else(fstatusx_i_2 == 0, "Married/cohabitating", "Not married/cohabitating"), 
         fesmoke_i = if_else(fesmoke_i == 1, "Smoking status: never smokers", if_else(fesmoke_i == 2, "Smoking status: ex-smokers", "Smoking status: current smokers")), 
         funitwk0_i_3 = if_else(funitwk0_i_3 == 0, "Alcohol intake: none", if_else(funitwk0_i_3 == 1, "Alcohol intake: 1-14 units/week", "Alcohol intake: more than 14 units/week")), 
         fg_2 = if_else(fg_2 == 1, "Fruits and vegetables intake: less than twice daily", "Fruits and vegetables intake: twice daily and more"),
         fbmi_i_3 = if_else(fbmi_i_3 == 0, "BMI: normal", if_else(fbmi_i_3 == 1, "BMI: overweight", "BMI: obese"))) %>%
  rename("A2. Ethnicity"                  = "ethnicity_i", 
         "A3. Occupational position"      = "flgrlump_i_ordinal", 
         "A4. Marital status"             = "fstatusx_i_2", 
         "B1. Smoking status"             = "fesmoke_i",
         "B2. Alcohol intake"             = "funitwk0_i_3", 
         "B3. Fruits & vegetables intake" = "fg_2",
         "C1. BMI"                        = "fbmi_i_3") %>% 
  gather(key = "Variables", value = "value", -stno, -sex_2) %>%
  group_by(sex_2, Variables, value) %>% 
  count() %>%
  group_by(sex_2, Variables) %>%
  mutate(freq = (n/sum(n))*100) %>% 
  mutate(lab = paste0(n, " (", round(freq, digits = 1), ")")) %>%
  ungroup() %>% 
  select(-n, -freq) %>%
  spread(key = sex_2, value = "lab") %>% 
  rename("Value" = "value") %>% 
  mutate(Value = factor(Value, levels = c("White", "Non-white",
                                          "Administrative", "Prof/exec", "Clerical/support",
                                          "Married/cohabitating", "Not married/cohabitating", 
                                          "Smoking status: never smokers", "Smoking status: ex-smokers", "Smoking status: current smokers", 
                                          "Alcohol intake: none", "Alcohol intake: 1-14 units/week", "Alcohol intake: more than 14 units/week", 
                                          "Fruits and vegetables intake: less than twice daily", "Fruits and vegetables intake: twice daily and more",
                                          "BMI: normal", "BMI: overweight", "BMI: obese"))) %>% 
  arrange(Value) %>%
  mutate(Value = as.character(Value)) %>%
  left_join(., 
            # Tableau des p.values 
            tab_11_fin %>% 
              select(stno, sex_2, ethnicity_i, flgrlump_i_ordinal, fstatusx_i_2, 
                     fesmoke_i, funitwk0_i_3, fg_2, 
                     fbmi_i_3) %>%
              gather(key = "Variables", value = "value", -stno, -sex_2) %>%
              split(.$Variables) %>%
              map_dfr(., ~ { 
                
                data.frame(Variables = unique(.x$Variables),
                           p.value = chisq.test(.x$sex_2, .x$value)$p.value) %>%
                  mutate(p.value = round(p.value, digits = 5))
                
              }) %>% 
              mutate(Variables = recode(Variables, 
                                        "ethnicity_i"         = "A2. Ethnicity", 
                                        "flgrlump_i_ordinal"  = "A3. Occupational position", 
                                        "fstatusx_i_2"        = "A4. Marital status", 
                                        "fesmoke_i"           = "B1. Smoking status",
                                        "funitwk0_i_3"        = "B2. Alcohol intake", 
                                        "fg_2"                = "B3. Fruits & vegetables intake",
                                        "fbmi_i_3"            = "C1. BMI")),
            by = "Variables") %>%
  filter(Value %in% c("Non-white", "Administrative", "Prof/exec", "Clerical/support", "Not married/cohabitating", 
                      "Smoking status: never smokers", "Smoking status: ex-smokers", "Smoking status: current smokers", 
                      "Alcohol intake: none", "Alcohol intake: 1-14 units/week", "Alcohol intake: more than 14 units/week", 
                      "Fruits and vegetables intake: less than twice daily", 
                      "BMI: normal", "BMI: overweight", "BMI: obese"))

# > Variables quantitatives  
t2 <- tab_11_fin %>% 
  select(stno, sex_2,
         "A1. Age, mean (SD)" = "fage_s", 
         "D1. Mental component score, mean (SD)" = "fmcs_i",
         "D2. Physical component score, mean (SD)" = "fpcs_i",
         "D3. Multimorbidity index, mean (SD)" = "mmm_index") %>%
  gather("Variables", "value", -stno, -sex_2) %>% 
  mutate(sex_2 = if_else(sex_2 == 1, "Men", "Women")) %>%
  group_by(Variables, sex_2) %>%
  summarise(mean = mean(value, na.rm = T),
            sd = sd(value, na.rm = T)) %>% 
  mutate(Value = substr(Variables, 5, nchar(Variables)), 
         lab = paste0(format(round(mean, digits = 1), nsmall = 1), " (", format(round(sd, digits = 1), nsmall = 1), ")")) %>% 
  select(-mean, -sd) %>%
  spread(sex_2, lab) %>%
  left_join(., 
            tab_11_fin %>% 
              select(stno, sex_2,
                     "A1. Age, mean (SD)" = "fage_s", 
                     "D1. Mental component score, mean (SD)" = "fmcs_i",
                     "D2. Physical component score, mean (SD)" = "fpcs_i",
                     "D3. Multimorbidity index, mean (SD)" = "mmm_index") %>%
              gather(key = "variable", value = "value", -stno, -sex_2) %>%
              split(.$variable) %>%
              map_dfr(., ~ {
                
                data.frame(Variables = unique(.x$variable),
                           p.value = t.test(.x[which(.x$sex_2 == 0),"value"], 
                                            .x[which(.x$sex_2 == 1),"value"])$p.value) %>%
                  mutate(p.value = round(p.value, digits = 5))
                
              }),
            by = "Variables"
  )


# Additional t.test for mean duration of waking time, SB, LIPA and MVPA in men and women
ta <- data0_11 %>% 
  select(stno, A_sex, starts_with("dur")) %>% 
  gather(key="activity_behavior", value = "duration", starts_with("dur")) %>%
  mutate(A_sex = recode(A_sex, "0" = "Women", "1" = "Men"),
         activity_behavior = recode(activity_behavior, 
                                    "dur_day_min_pla" = "Waking time, mean (SD)",
                                    "dur_day_mvpa_bts_10_min_pla" = "MVPA, mean (SD) (10 min bouts)",
                                    "dur_day_total_in_min_pla" = "SB, mean (SD)",
                                    "dur_day_total_lig_min_pla" = "LIPA, mean (SD)",
                                    "dur_day_total_mvpa_min_pla" = "MVPA, mean (SD)"),
         activity_behavior = factor(activity_behavior, levels = c("Waking time, mean (SD)", "SB, mean (SD)", "LIPA, mean (SD)", "MVPA, mean (SD)", "MVPA, mean (SD) (10 min bouts)"))) %>%
  group_by(A_sex, activity_behavior) %>%
  summarise(mean_duration = mean(duration),
            sd_duration = sd(duration)) %>% 
  mutate(lab = paste0(format(round(mean_duration, 1), nsmall = 1), " (", format(round(sd_duration, 1), nsmall = 1), ")")) %>%
  select(-mean_duration, -sd_duration) %>%
  spread(key = "A_sex", value = lab) %>% 
  right_join(data.frame(activity_behavior = c("Waking time, mean (SD)", "SB, mean (SD)", "LIPA, mean (SD)", "MVPA, mean (SD)"),
           p.value = c(t.test(data0_11[which(data0_11$A_sex == 0),"dur_day_min_pla"], 
                              data0_11[which(data0_11$A_sex == 1),"dur_day_min_pla"])$p.value,
                       t.test(data0_11[which(data0_11$A_sex == 0),"dur_day_total_in_min_pla"], 
                              data0_11[which(data0_11$A_sex == 1),"dur_day_total_in_min_pla"])$p.value,
                       t.test(data0_11[which(data0_11$A_sex == 0),"dur_day_total_lig_min_pla"], 
                              data0_11[which(data0_11$A_sex == 1),"dur_day_total_lig_min_pla"])$p.value,
                       t.test(data0_11[which(data0_11$A_sex == 0),"dur_day_total_mvpa_min_pla"], 
                              data0_11[which(data0_11$A_sex == 1),"dur_day_total_mvpa_min_pla"])$p.value)),
           by = "activity_behavior")

# % of time
tb <- data0_11 %>% 
  select(stno, A_sex, starts_with("dur_day_total"), -dur_day_mvpa_bts_10_min_pla) %>% 
  gather(key="activity_behavior", value = "duration", starts_with("dur")) %>%
  mutate(A_sex = recode(A_sex, "0" = "Women", "1" = "Men"),
         activity_behavior = recode(activity_behavior, 
                                    "dur_day_total_in_min_pla" = "SB (%)",
                                    "dur_day_total_lig_min_pla" = "LIPA (%)",
                                    "dur_day_total_mvpa_min_pla" = "MVPA (%)"),
         activity_behavior = factor(activity_behavior, levels = c("SB (%)", "LIPA (%)", "MVPA (%)"))) %>%
  group_by(A_sex, activity_behavior) %>%
  summarise(mean_duration = mean(duration)) %>% 
  group_by(A_sex) %>% 
  mutate(sum = sum(mean_duration)) %>%
  mutate(prop = paste0(format(round((mean_duration/sum)*100, 2), nsmall = 2),"%")) %>% 
  select(-sum, -mean_duration) %>% 
  spread(A_sex, prop) %>% 
  mutate(p.value = "")

# Save
write.xlsx2(x = rbind(data.frame(t1), 
                      data.frame(t2)) %>%
              arrange(Variables) %>% 
              select("Covariables" = "Value", 
                     "Men (n = 2910)" = "Men",
                     "Women (n = 986)" = "Women", 
                     p.value) %>% 
              rbind(., 
                    rbind(data.frame(ta), data.frame(tb)) %>% 
                      rename("Covariables" = "activity_behavior",
                             "Men (n = 2910)" = "Men",
                             "Women (n = 986)" = "Women")), 
            file = paste0(path, "Table_1.xlsx"),
            row.names = FALSE)

# Confidence intervals for difference in PA behaviours
model_sb_ci   <- lm(dur_day_total_in_min_pla   ~ A_sex, data = data0_11)
model_lipa_ci <- lm(dur_day_total_lig_min_pla  ~ A_sex, data = data0_11)
model_mvpa_ci <- lm(dur_day_total_mvpa_min_pla ~ A_sex, data = data0_11)

confint(model_sb_ci, level = 0.95)
confint(model_lipa_ci, level = 0.95)
confint(model_mvpa_ci, level = 0.95)

#--------------------------------------------------------------------
# --- Figure 1 --- 
# > PA distribution by sex and functional coefficient for sex (from minimally adjusted models)  

# > Functional coefficient for sex
coef_fm1_min <- coef(fm1_min, seWithMean = FALSE, useVc = FALSE)$smterms
names(coef_fm1_min)
coef_sex <- coef_fm1_min[["A_sex(xi)"]]$coef
coef_values <- coef_sex$xi.vec

# > Predicted distribution (and 95% CI) for each sex
pred_sex_min <- predict(fm1_min, 
                        se.fit = TRUE, 
                        type = "response",
                        newdata = list(A_sex            = as.vector(c(0,1)),
                                       A_age_conti_5    = as.vector(c(0,0)),
                                       A_ethnicity      = as.vector(c(0,0)),
                                       A_socio_eco_cont = as.vector(c(0,0)),
                                       O_waking_time    = as.vector(c(mean(data1_11$O_waking_time), mean(data1_11$O_waking_time))))) 

# > Integration 
pred_sex <- data.frame(
  Sex     = c(rep("Women", 117), rep("Men", 117)), 
  x_i     = c(data1_11$xi, data1_11$xi),
  f_i     = c(pred_sex_min$fit[1,], pred_sex_min$fit[2,]),
  se_i    = c(pred_sex_min$se.fit[1,], pred_sex_min$se.fit[2,])) %>% 
  # Compute 95% CI
  mutate(
    ic_up   = f_i + 1.96*se_i,
    ic_down = f_i - 1.96*se_i) %>%
  # Compute area under the curve 
  group_by(Sex) %>%
  mutate(
    area_cum    = cumtrapz(x_i, f_i),
    area_simple = area_cum - lag(area_cum, default = 0))

# > Sum of the integral on the same intervals than the functional coefficient
area_values2 <- list()

for(i in 2:length(coef_values))
{ 
  # Keep only distribution which is in the interval
  temp <- pred_sex[which(pred_sex$x_i >= coef_values[i-1] & pred_sex$x_i < coef_values[i]),]
  # Area computation
  area_values2[[paste0(coef_values[i])]] <- temp %>% 
    group_by(Sex) %>%
    summarise(area = sum(area_simple, na.rm = T))
  
}

# Check
# > total area must be equal to mean waking time 
plyr::ldply(area_values2, data.frame, .id = "xi.vec") %>%
  group_by(Sex) %>% 
  summarise(sum(area))
# > area on PA range
plyr::ldply(area_values2, data.frame, .id = "xi.vec") %>% 
  mutate(xi.vec = as.numeric(as.character(xi.vec))) %>% 
  mutate(activity_behavior = if_else(xi.vec < log(0.04 + 1), "SB",
                             if_else(xi.vec > log(0.10 + 1), "MVPA", 
                              "LIPA"))) %>% 
  group_by(Sex, activity_behavior) %>% 
  summarise(sum(area))
# --> some differences to explore

# Figure 1
# - a) Time spent in each sex in 0.05g acceleration intervals estimated after integration of 
#      activity distribution estimated by function-on-scalar regression model mutually 
#      adjusted for sex, age (continuous, 5 years increment), education and ethnicity as well as for waking time.

p1a <- plyr::ldply(area_values2, data.frame, .id = "xi.vec") %>%
  mutate(xi.vec = as.numeric(as.character(xi.vec))) %>%
  filter(xi.vec < log(0.2+1)) %>% 
  ggplot(.) + 
  geom_col(aes(x = xi.vec-0.00491952/2, y = area, fill = Sex), position = position_dodge2()) +
  geom_vline(aes(xintercept = log(1+0.04)), linetype = 2, color = "black") +
  geom_vline(aes(xintercept = log(1+0.1)), linetype = 2, color = "black") +
  theme_classic() +
  theme(legend.position = "top",
        axis.title.x = element_blank()) +
  labs(y = "Minutes/day") + 
  scale_fill_manual(values = pal[c(1,5)], na.translate=FALSE) +
  scale_x_continuous(labels = c(seq(0, 0.2, by = 0.05)), breaks = log(seq(0, 0.2, by = 0.05) + 1))

# - b) Time difference between men and women for each 0.05g intensity interval. 
#      corresponding to integrated functional coefficient for sex in the mini-
#      mally adjusted model. 

p1b <- plyr::ldply(area_values2, data.frame, .id = "xi.vec") %>%
  spread(key = "Sex", value = "area") %>%
  mutate(xi.vec = as.numeric(as.character(xi.vec))) %>%
  mutate(diff = Men - Women) %>%
  filter(xi.vec < log(0.2+1)) %>% 
  ggplot(.) + 
  geom_col(aes(x = xi.vec - 0.00491952/2, y = diff), fill = "grey") +
  geom_vline(aes(xintercept = log(1+0.04)), linetype = 2, color = "black") +
  geom_vline(aes(xintercept = log(1+0.1)), linetype = 2, color = "black") +
  theme_classic() +
  theme(legend.position = "top") +
  labs(x = expression(paste("Acceleration (in ", italic("g"), ")")), y = "Minutes/day") + 
  scale_fill_manual(values = pal[c(1,5)]) +
  scale_x_continuous(labels = c(seq(0, 0.2, by = 0.05)), breaks = log(seq(0, 0.2, by = 0.05) + 1))

# Figure 1
p1 <- plot_grid(p1a, p1b, nrow = 2, rel_heights = c(5,3), labels = c("A.", "B."), label_fontface = "plain", label_size = 10)

# Saving plot
ggsave(plot     = p1, 
       filename = paste0(path, "Figure_1.png"),
       width    = 8,
       height   = 6,
       dpi      = 300)

ggsave(plot     = p1, 
       filename = paste0(path, "Figure_1.pdf"),
       width    = 8,
       height   = 6,
       dpi      = 300)

#--------------------------------------------------------------------
# --- Figure 2 --- 
# Association between socio-demographic, lifestyle and health-related factors 
# and acceleration distribution in 2012-2013 in men (n = 2910).

# > Limits for plots legends
limits.max.plots <- max(abs(coef_m2_3_man$coef_fm$area), abs(coef_m2_3_woman$coef_fm$area),
                        abs(coef_m2_7_man$coef_fm$area), abs(coef_m2_7_woman$coef_fm$area),
                        abs(coef_m2_11_man$coef_fm$area), abs(coef_m2_11_woman$coef_fm$area),
                        abs(coef_m2_3_man_s1$coef_fm$area), abs(coef_m2_3_woman_s1$coef_fm$area),
                        abs(coef_m2_3_man_s2$coef_fm$area), abs(coef_m2_3_woman_s2$coef_fm$area),
                        abs(coef_m2_7_man_s1$coef_fm$area), abs(coef_m2_7_woman_s1$coef_fm$area),
                        abs(coef_m2_7_man_s2$coef_fm$area), abs(coef_m2_7_woman_s2$coef_fm$area),
                        abs(coef_m2_11_man_s2$coef_fm$area), abs(coef_m2_11_woman_s2$coef_fm$area),
                        abs(coef_m2_11_man_s3$coef_fm$area), abs(coef_m2_11_woman_s3$coef_fm$area))
limits.min.plots <- -limits.max.plots

p2 <- flm_fm_plot(flm_fm_coef = coef_m2_11_man, 
                  limits.min = limits.min.plots, 
                  limits.max = limits.max.plots)

ggsave(plot     = p2,
       filename = paste0(path, "Figure_2.png"),
       width    = 10,
       height   = 4.5,
       dpi      = 300)
ggsave(plot     = p2,
       filename = paste0(path, "Figure_2.pdf"),
       width    = 10,
       height   = 4.5,
       dpi      = 300)

#--------------------------------------------------------------------
# --- Figure 3 --- 
# Association between socio-demographic, lifestyle and health-related factors 
# and acceleration distribution in 2012-2013 in women (n = 986).

p3 <- flm_fm_plot(flm_fm_coef = coef_m2_11_woman, 
                  limits.min = limits.min.plots, 
                  limits.max = limits.max.plots)
ggsave(plot     = p3,
       filename = paste0(path, "Figure_3.png"),
       width    = 10,
       height   = 4.5,
       dpi      = 300)
ggsave(plot     = p3,
       filename = paste0(path, "Figure_3.pdf"),
       width    = 10,
       height   = 4.5,
       dpi      = 300)

#--------------------------------------------------------------------
# --- Figure 4 --- 
# Association between socio-demographic, lifestyle and health-related factors 
# and acceleration distribution through the follow-up in men.

p4 <- plot_grid(
  ffm_plot(fm_model = coef_m2_3_man, legend = TRUE, title = "A. 1991-1993 (N = 2823)", limits.max = limits.max.plots, limits.min = limits.min.plots),
  ffm_plot(fm_model = coef_m2_7_man, legend = FALSE, title = "B. 2002-2004 (N = 2828)", limits.max = limits.max.plots, limits.min = limits.min.plots),
  ffm_plot(fm_model = coef_m2_11_man, legend = FALSE, title = "C. 2012-2013 (N = 2910)", limits.max = limits.max.plots, limits.min = limits.min.plots),
  ncol = 3, 
  rel_widths = c(0.5, 0.25, 0.25))

ggsave(plot = p4, 
       filename = paste0(path, "Figure_4.png"),
       width    = 11,
       height   = 4.5,
       dpi      = 300)
ggsave(plot = p4, 
       filename = paste0(path, "Figure_4.pdf"),
       width    = 11,
       height   = 4.5,
       dpi      = 300)

#--------------------------------------------------------------------
# --- Figure 5 --- 
# Association between socio-demographic, lifestyle and health-related factors 
# and acceleration distribution through the follow-up in women (n = 986).

p5 <- plot_grid(
  ffm_plot(fm_model = coef_m2_3_woman, legend = TRUE, title = "A. 1991-1994 (N = 985)", limits.min = limits.min.plots, limits.max = limits.max.plots),
  ffm_plot(fm_model = coef_m2_7_woman, legend = FALSE, title = "B. 2002-2004 (N = 954)", limits.min = limits.min.plots, limits.max = limits.max.plots),
  ffm_plot(fm_model = coef_m2_11_woman, legend = FALSE, title = "C. 2012-2013 (N = 986)", limits.min = limits.min.plots, limits.max = limits.max.plots),
  ncol = 3, 
  rel_widths = c(0.5, 0.25, 0.25))

ggsave(plot = p5, 
       filename = paste0(path, "Figure_5.png"),
       width    = 11,
       height   = 4.5,
       dpi      = 300)
ggsave(plot = p5, 
       filename = paste0(path, "Figure_5.pdf"),
       width    = 11,
       height   = 4.5,
       dpi      = 300)

#--------------------------------------------------------------------
# --- Supplementary Table 1 ---
# Population characteristics at phase 3 and 7

# Population characteristics at phase 3
# > Categorical and binary variables
sup.t1.a <- tab_3_fin %>% 
  select(stno, sex_2, ethnicity_i, xgrlump_i_ordinal, xstatusx_i_2, 
         xesmoke_i, xunitwk0_i_3, fg_2, 
         xbmi_i_3) %>%
  # Mise en forme du tableau
  mutate(sex_2 = if_else(sex_2 == 0, "Women", "Men"),
         ethnicity_i = if_else(ethnicity_i == 0, "White", "Non-white"),
         xgrlump_i_ordinal = if_else(xgrlump_i_ordinal == 0, "Administrative", if_else(xgrlump_i_ordinal == 0.5, "Prof/exec", "Clerical/support")), 
         xstatusx_i_2 = if_else(xstatusx_i_2 == 0, "Married/cohabitating", "Not married/cohabitating"), 
         xesmoke_i = if_else(xesmoke_i == 1, "Smoking status: never smokers", if_else(xesmoke_i == 2, "Smoking status: ex-smokers", "Smoking status: current smokers")), 
         xunitwk0_i_3 = if_else(xunitwk0_i_3 == 0, "Alcohol intake: none", if_else(xunitwk0_i_3 == 1, "Alcohol intake: 1-14 units/week", "Alcohol intake: more than 14 units/week")), 
         fg_2 = if_else(fg_2 == 1, "Fruits and vegetables intake: less than twice daily", "Fruits and vegetables intake: twice daily and more"),
         xbmi_i_3 = if_else(xbmi_i_3 == 0, "BMI: normal", if_else(xbmi_i_3 == 1, "BMI: overweight", "BMI: obese"))) %>%
  rename("A2. Ethnicity"                  = "ethnicity_i", 
         "A3. Occupational position"      = "xgrlump_i_ordinal", 
         "A4. Marital status"             = "xstatusx_i_2", 
         "B1. Smoking status"             = "xesmoke_i",
         "B2. Alcohol intake"             = "xunitwk0_i_3", 
         "B3. Fruits & vegetables intake" = "fg_2",
         "C1. BMI"                        = "xbmi_i_3") %>% 
  gather(key = "Variables", value = "value", -stno, -sex_2) %>%
  group_by(sex_2, Variables, value) %>% 
  count() %>%
  group_by(sex_2, Variables) %>%
  mutate(freq = (n/sum(n))*100) %>% 
  mutate(lab = paste0(n, " (", round(freq, digits = 1), ")")) %>%
  ungroup() %>% 
  select(-n, -freq) %>%
  spread(key = sex_2, value = "lab") %>% 
  rename("Value" = "value") %>% 
  mutate(Value = factor(Value, levels = c("White", "Non-white",
                                          "Administrative", "Prof/exec", "Clerical/support",
                                          "Married/cohabitating", "Not married/cohabitating", 
                                          "Smoking status: never smokers", "Smoking status: ex-smokers", "Smoking status: current smokers", 
                                          "Alcohol intake: none", "Alcohol intake: 1-14 units/week", "Alcohol intake: more than 14 units/week", 
                                          "Fruits and vegetables intake: less than twice daily", "Fruits and vegetables intake: twice daily and more",
                                          "BMI: normal", "BMI: overweight", "BMI: obese"))) %>% 
  arrange(Value) %>%
  mutate(Value = as.character(Value)) %>%
  left_join(., 
            # Tableau des p.values 
            tab_3_fin %>% 
              select(stno, sex_2, ethnicity_i, xgrlump_i_ordinal, xstatusx_i_2, 
                     xesmoke_i, xunitwk0_i_3, fg_2, 
                     xbmi_i_3) %>%
              gather(key = "Variables", value = "value", -stno, -sex_2) %>%
              split(.$Variables) %>%
              map_dfr(., ~ { 
                
                data.frame(Variables = unique(.x$Variables),
                           p.value = chisq.test(.x$sex_2, .x$value)$p.value) %>%
                  mutate(p.value = round(p.value, digits = 3),
                         p.value = as.character(p.value),
                         p.value = if_else(p.value < 0.001, "< 0.001", p.value))
                
              }) %>% 
              mutate(Variables = recode(Variables, 
                                        "ethnicity_i"         = "A2. Ethnicity", 
                                        "xgrlump_i_ordinal"   = "A3. Occupational position", 
                                        "xstatusx_i_2"        = "A4. Marital status", 
                                        "xesmoke_i"           = "B1. Smoking status",
                                        "xunitwk0_i_3"        = "B2. Alcohol intake", 
                                        "fg_2"                = "B3. Fruits & vegetables intake",
                                        "xbmi_i_3"            = "C1. BMI")),
            by = "Variables") %>%
  filter(Value %in% c("Non-white", "Administrative", "Prof/exec", "Clerical/support", "Not married/cohabitating", 
                      "Smoking status: never smokers", "Smoking status: ex-smokers", "Smoking status: current smokers", 
                      "Alcohol intake: none", "Alcohol intake: 1-14 units/week", "Alcohol intake: more than 14 units/week", 
                      "Fruits and vegetables intake: less than twice daily", 
                      "BMI: normal", "BMI: overweight", "BMI: obese"))

# > Variables quantitatives  
sup.t1.b <- tab_3_fin %>% 
  select(stno, sex_2,
         "A1. Age, mean (SD)" = "xage_s_i", 
         "D1. Mental component score, mean (SD)" = "xmcs",
         "D2. Physical component score, mean (SD)" = "xpcs",
         "D3. Multimorbidity index, mean (SD)" = "mmm_index") %>%
  gather("Variables", "value", -stno, -sex_2) %>% 
  mutate(sex_2 = if_else(sex_2 == 1, "Men", "Women")) %>%
  group_by(Variables, sex_2) %>%
  summarise(mean = mean(value, na.rm = T),
            sd = sd(value, na.rm = T)) %>% 
  mutate(Value = substr(Variables, 5, nchar(Variables)), 
         lab = paste0(round(mean, digits = 1), " (", round(sd, digits = 1), ")")) %>% 
  select(-mean, -sd) %>%
  spread(sex_2, lab) %>%
  left_join(., 
            tab_3_fin %>% 
              select(stno, sex_2,
                     "A1. Age, mean (SD)" = "xage_s_i", 
                     "D1. Mental component score, mean (SD)" = "xmcs",
                     "D2. Physical component score, mean (SD)" = "xpcs",
                     "D3. Multimorbidity index, mean (SD)" = "mmm_index") %>%
              gather(key = "variable", value = "value", -stno, -sex_2) %>%
              split(.$variable) %>%
              map_dfr(., ~ {
                
                data.frame(Variables = unique(.x$variable),
                           p.value = t.test(.x[which(.x$sex_2 == 0),"value"], 
                                            .x[which(.x$sex_2 == 1),"value"])$p.value) %>%
                  mutate(p.value = round(p.value, digits = 3),
                         p.value = as.character(p.value),
                         p.value = if_else(p.value < 0.001, "< 0.001", p.value))
                
              }),
            by = "Variables"
  )

# > Save
write.xlsx2(x = rbind(data.frame(sup.t1.a), data.frame(sup.t1.b)) %>%
              arrange(Variables) %>% 
              select("Covariables" = "Value", 
                     "Men (n = 2823)" = "Men",
                     "Women (n = 985)" = "Women", 
                     p.value), 
            file = paste0(path, "Supp_Table_1.xlsx"),
            row.names = FALSE, 
            sheetName = "Phase 3")


# Population characteristics at phase 7
# > Categorical and binary variables
sup.t1.c <- tab_7_fin %>% 
  select(stno, sex_2, ethnicity_i, mlgrlump_i_ordinal, mstatusx_i_2, 
         mesmoke_i, munitwk0_i_3, fg_2, 
         mbmi_i_3) %>%
  # Mise en forme du tableau
  mutate(sex_2 = if_else(sex_2 == 0, "Women", "Men"),
         ethnicity_i = if_else(ethnicity_i == 0, "White", "Non-white"),
         mlgrlump_i_ordinal = if_else(mlgrlump_i_ordinal == 0, "Administrative", if_else(mlgrlump_i_ordinal == 0.5, "Prof/exec", "Clerical/support")), 
         mstatusx_i_2 = if_else(mstatusx_i_2 == 0, "Married/cohabitating", "Not married/cohabitating"), 
         mesmoke_i = if_else(mesmoke_i == 1, "Smoking status: never smokers", if_else(mesmoke_i == 2, "Smoking status: ex-smokers", "Smoking status: current smokers")), 
         munitwk0_i_3 = if_else(munitwk0_i_3 == 0, "Alcohol intake: none", if_else(munitwk0_i_3 == 1, "Alcohol intake: 1-14 units/week", "Alcohol intake: more than 14 units/week")), 
         fg_2 = if_else(fg_2 == 1, "Fruits and vegetables intake: less than twice daily", "Fruits and vegetables intake: twice daily and more"),
         mbmi_i_3 = if_else(mbmi_i_3 == 0, "BMI: normal", if_else(mbmi_i_3 == 1, "BMI: overweight", "BMI: obese"))) %>%
  rename("A2. Ethnicity"                  = "ethnicity_i", 
         "A3. Occupational position"      = "mlgrlump_i_ordinal", 
         "A4. Marital status"             = "mstatusx_i_2", 
         "B1. Smoking status"             = "mesmoke_i",
         "B2. Alcohol intake"             = "munitwk0_i_3", 
         "B3. Fruits & vegetables intake" = "fg_2",
         "C1. BMI"                        = "mbmi_i_3") %>% 
  gather(key = "Variables", value = "value", -stno, -sex_2) %>%
  group_by(sex_2, Variables, value) %>% 
  count() %>%
  group_by(sex_2, Variables) %>%
  mutate(freq = (n/sum(n))*100) %>% 
  mutate(lab = paste0(n, " (", round(freq, digits = 1), ")")) %>%
  ungroup() %>% 
  select(-n, -freq) %>%
  spread(key = sex_2, value = "lab") %>% 
  rename("Value" = "value") %>% 
  mutate(Value = factor(Value, levels = c("White", "Non-white",
                                          "Administrative", "Prof/exec", "Clerical/support",
                                          "Married/cohabitating", "Not married/cohabitating", 
                                          "Smoking status: never smokers", "Smoking status: ex-smokers", "Smoking status: current smokers", 
                                          "Alcohol intake: none", "Alcohol intake: 1-14 units/week", "Alcohol intake: more than 14 units/week", 
                                          "Fruits and vegetables intake: less than twice daily", "Fruits and vegetables intake: twice daily and more",
                                          "BMI: normal", "BMI: overweight", "BMI: obese"))) %>% 
  arrange(Value) %>%
  mutate(Value = as.character(Value)) %>%
  left_join(., 
            # Tableau des p.values 
            tab_7_fin %>% 
              select(stno, sex_2, ethnicity_i, mlgrlump_i_ordinal, mstatusx_i_2, 
                     mesmoke_i, munitwk0_i_3, fg_2, 
                     mbmi_i_3) %>%
              gather(key = "Variables", value = "value", -stno, -sex_2) %>%
              split(.$Variables) %>%
              map_dfr(., ~ { 
                
                data.frame(Variables = unique(.x$Variables),
                           p.value = chisq.test(.x$sex_2, .x$value)$p.value) %>%
                  mutate(p.value = round(p.value, digits = 3),
                         p.value = as.character(p.value),
                         p.value = if_else(p.value < 0.001, "< 0.001", p.value))
                
              }) %>% 
              mutate(Variables = recode(Variables, 
                                        "ethnicity_i"         = "A2. Ethnicity", 
                                        "mlgrlump_i_ordinal"  = "A3. Occupational position",
                                        "mstatusx_i_2"        = "A4. Marital status", 
                                        "mesmoke_i"           = "B1. Smoking status",
                                        "munitwk0_i_3"        = "B2. Alcohol intake", 
                                        "fg_2"                = "B3. Fruits & vegetables intake",
                                        "mbmi_i_3"            = "C1. BMI")),
            by = "Variables") %>%
  filter(Value %in% c("Non-white", "Administrative", "Prof/exec", "Clerical/support", "Not married/cohabitating", 
                      "Smoking status: never smokers", "Smoking status: ex-smokers", "Smoking status: current smokers", 
                      "Alcohol intake: none", "Alcohol intake: 1-14 units/week", "Alcohol intake: more than 14 units/week", 
                      "Fruits and vegetables intake: less than twice daily", 
                      "BMI: normal", "BMI: overweight", "BMI: obese"))

# > Variables quantitatives  
sup.t1.d <- tab_7_fin %>% 
  select(stno, sex_2,
         "A1. Age, mean (SD)" = "mage_s_i", 
         "D1. Mental component score, mean (SD)" = "mmcs_i",
         "D2. Physical component score, mean (SD)" = "mpcs_i",
         "D3. Multimorbidity index, mean (SD)" = "mmm_index") %>%
  gather("Variables", "value", -stno, -sex_2) %>% 
  mutate(sex_2 = if_else(sex_2 == 1, "Men", "Women")) %>%
  group_by(Variables, sex_2) %>%
  summarise(mean = mean(value, na.rm = T),
            sd = sd(value, na.rm = T)) %>% 
  mutate(Value = substr(Variables, 5, nchar(Variables)), 
         lab = paste0(round(mean, digits = 1), " (", round(sd, digits = 1), ")")) %>% 
  select(-mean, -sd) %>%
  spread(sex_2, lab) %>%
  left_join(., 
            tab_7_fin %>% 
              select(stno, sex_2,
                     "A1. Age, mean (SD)" = "mage_s_i", 
                     "D1. Mental component score, mean (SD)" = "mmcs_i",
                     "D2. Physical component score, mean (SD)" = "mpcs_i",
                     "D3. Multimorbidity index, mean (SD)" = "mmm_index") %>%
              gather(key = "variable", value = "value", -stno, -sex_2) %>%
              split(.$variable) %>%
              map_dfr(., ~ {
                
                data.frame(Variables = unique(.x$variable),
                           p.value = t.test(.x[which(.x$sex_2 == 0),"value"], 
                                            .x[which(.x$sex_2 == 1),"value"])$p.value) %>%
                  mutate(p.value = round(p.value, digits = 3),
                         p.value = as.character(p.value),
                         p.value = if_else(p.value < 0.001, "< 0.001", p.value))
                
              }),
            by = "Variables"
  )

# > Save
write.xlsx2(x = rbind(data.frame(sup.t1.c), data.frame(sup.t1.d)) %>%
              arrange(Variables) %>% 
              select("Covariables" = "Value", 
                     "Men (n = 2828)" = "Men",
                     "Women (n = 954)" = "Women", 
                     p.value), 
            file = paste0(path, "Supp_Table_1.xlsx"),
            row.names = FALSE, 
            sheetName = "Phase 7", 
            append = T)

#--------------------------------------------------------------------
# --- Supplementary table 2 --- 
# p for sex interactions at each time point

data.frame(var = as.vector(row.names(summary(fm1_full_int)$s.table))) %>% 
  left_join(data.frame(var = as.vector(row.names(summary(fm1_full_int_3)$s.table)), 
                       p.value.3 = summary(fm1_full_int_3)$s.table[,4], row.names = NULL), 
            by = "var") %>% 
  left_join(data.frame(var = as.vector(row.names(summary(fm1_full_int_7)$s.table)), 
                       p.value.7 = summary(fm1_full_int_7)$s.table[,4], row.names = NULL), 
            by = "var") %>%
  left_join(data.frame(var = as.vector(row.names(summary(fm1_full_int)$s.table)), 
                       p.value.11 = summary(fm1_full_int)$s.table[,4], row.names = NULL), 
            by = "var") %>% 
  # Keep only significative interactions
  mutate(int = if_else(substr(var, 1,6) == "sex_x_" | var == "age_x_sex(xi)", 1, 0)) %>%
  filter(int == 1) %>% 
  select(-int) %>% 
  # Change p.value format
  gather(key = "phase", value = "p", -var) %>%
  mutate(p = round(p, 4),
         p = as.character(p),
         p = if_else(p == "0", "<0.0001", p),
         #p = if_else(is.na(p) == T, "NS", p)
         ) %>% 
  spread(key = "phase", value = p) %>% 
  # Table format
  mutate(var = gsub("sex_x_", "", var),
         var = if_else(var == "age_x_sex(xi)", "A_age_conti_5(xi)", var)) %>% 
  left_join(table.name, by = "var") %>% 
  select(var.group, var.name, p.value.3, p.value.7, p.value.11) %>% 
  rename("1991-1994" = "p.value.3",
         "2002-2004" = "p.value.7",
         "2012-213" = "p.value.11") 

#--------------------------------------------------------------------
# --- Supplementary table 3 --- 
# Regression coefficients of linear models for men
# > phase 11
sup.tab.3.11 <- coef_m2_11_man$coef_lm %>% 
  select(activity_behavior, var.group, var.name, "coef" = "lab", ci) %>% 
  mutate(lab = paste0(coef, " (", ci, ")")) %>% 
  select(-coef, -ci) %>% 
  spread(key = "activity_behavior", value = "lab") %>% 
  # Ordering labels
  mutate(var.name = factor(var.name, levels = c("Age (per 5 years increment)", "Non-white", "Lower occupational position", "Not married/cohabitating", 
                                                "Smoking status: past vs never smoker","Smoking status: current vs never smoker", 
                                                "Alcohol intake: none vs moderate", "Alcohol intake: high vs moderate", 
                                                paste0("Fruits & vegetables intake: < vs ", intToUtf8(8805)," than twice daily"), 
                                                "BMI: overweight vs normal","BMI: obese vs normal",
                                                "Mental component score (per 10 points decrement)",
                                                "Physical component score (per 10 points decrement)",
                                                "Number of chronic conditions (per new condition)"))) %>% 
  arrange(var.name)%>%
  rename("Covariate name" = "var.name",
         "Covriate subgroup" = "var.group")

# > phase 7
sup.tab.3.7 <- coef_m2_7_man$coef_lm %>% 
  select(activity_behavior, var.group, var.name, "coef" = "lab", ci) %>% 
  mutate(lab = paste0(coef, " (", ci, ")")) %>% 
  select(-coef, -ci) %>% 
  spread(key = "activity_behavior", value = "lab") %>% 
  # Ordering labels
  mutate(var.name = factor(var.name, levels = c("Age (per 5 years increment)", "Non-white", "Lower occupational position", "Not married/cohabitating", 
                                                "Smoking status: past vs never smoker","Smoking status: current vs never smoker", 
                                                "Alcohol intake: none vs moderate", "Alcohol intake: high vs moderate", 
                                                paste0("Fruits & vegetables intake: < vs ", intToUtf8(8805)," than twice daily"), 
                                                "BMI: overweight vs normal","BMI: obese vs normal",
                                                "Mental component score (per 10 points decrement)",
                                                "Physical component score (per 10 points decrement)",
                                                "Number of chronic conditions (per new condition)"))) %>% 
  arrange(var.name)%>%
  rename("Covariate name" = "var.name",
         "Covriate subgroup" = "var.group")

# > phase 3
sup.tab.3.3 <- coef_m2_3_man$coef_lm %>% 
  select(activity_behavior, var.group, var.name, "coef" = "lab", ci) %>% 
  mutate(lab = paste0(coef, " (", ci, ")")) %>% 
  select(-coef, -ci) %>% 
  spread(key = "activity_behavior", value = "lab") %>% 
  # Ordering labels
  mutate(var.name = factor(var.name, levels = c("Age (per 5 years increment)", "Non-white", "Lower occupational position", "Not married/cohabitating", 
                                                "Smoking status: past vs never smoker","Smoking status: current vs never smoker", 
                                                "Alcohol intake: none vs moderate", "Alcohol intake: high vs moderate", 
                                                paste0("Fruits & vegetables intake: < vs ", intToUtf8(8805)," than twice daily"), 
                                                "BMI: overweight vs normal","BMI: obese vs normal",
                                                "Mental component score (per 10 points decrement)",
                                                "Physical component score (per 10 points decrement)",
                                                "Number of chronic conditions (per new condition)"))) %>% 
  arrange(var.name)%>%
  rename("Covariate name" = "var.name",
         "Covriate subgroup" = "var.group")

# > Save
write.xlsx2(x = sup.tab.3.3, 
            file = paste0(path, "Supp_Table_3.xlsx"),
            row.names = FALSE, 
            sheetName = "Phase 3")

write.xlsx2(x = sup.tab.3.7, 
            file = paste0(path, "Supp_Table_3.xlsx"),
            row.names = FALSE, 
            sheetName = "Phase 7", 
            append = T)

write.xlsx2(x = sup.tab.3.11, 
            file = paste0(path, "Supp_Table_3.xlsx"),
            row.names = FALSE, 
            sheetName = "Phase 11", 
            append = T)

#--------------------------------------------------------------------
# --- Supplementary table 4 --- 
# Regression coefficients of linear models for women
# > phase 11
sup.tab.4.11 <- coef_m2_11_woman$coef_lm %>% 
  select(activity_behavior, var.group, var.name, "coef" = "lab", ci) %>% 
  mutate(lab = paste0(coef, " (", ci, ")")) %>% 
  select(-coef, -ci) %>% 
  spread(key = "activity_behavior", value = "lab") %>% 
  # Ordering labels
  mutate(var.name = factor(var.name, levels = c("Age (per 5 years increment)", "Non-white", "Lower occupational position", "Not married/cohabitating", 
                                                "Smoking status: past vs never smoker","Smoking status: current vs never smoker", 
                                                "Alcohol intake: none vs moderate", "Alcohol intake: high vs moderate", 
                                                paste0("Fruits & vegetables intake: < vs ", intToUtf8(8805)," than twice daily"), 
                                                "BMI: overweight vs normal","BMI: obese vs normal",
                                                "Mental component score (per 10 points decrement)",
                                                "Physical component score (per 10 points decrement)",
                                                "Number of chronic conditions (per new condition)"))) %>% 
  arrange(var.name)%>%
  rename("Covariate name" = "var.name",
         "Covriate subgroup" = "var.group")

# > phase 7
sup.tab.4.7 <- coef_m2_7_woman$coef_lm %>% 
  select(activity_behavior, var.group, var.name, "coef" = "lab", ci) %>% 
  mutate(lab = paste0(coef, " (", ci, ")")) %>% 
  select(-coef, -ci) %>% 
  spread(key = "activity_behavior", value = "lab") %>% 
  # Ordering labels
  mutate(var.name = factor(var.name, levels = c("Age (per 5 years increment)", "Non-white", "Lower occupational position", "Not married/cohabitating", 
                                                "Smoking status: past vs never smoker","Smoking status: current vs never smoker", 
                                                "Alcohol intake: none vs moderate", "Alcohol intake: high vs moderate", 
                                                paste0("Fruits & vegetables intake: < vs ", intToUtf8(8805)," than twice daily"), 
                                                "BMI: overweight vs normal","BMI: obese vs normal",
                                                "Mental component score (per 10 points decrement)",
                                                "Physical component score (per 10 points decrement)",
                                                "Number of chronic conditions (per new condition)"))) %>% 
  arrange(var.name)%>%
  rename("Covariate name" = "var.name",
         "Covriate subgroup" = "var.group")

# > phase 3
sup.tab.4.3 <- coef_m2_3_woman$coef_lm %>% 
  select(activity_behavior, var.group, var.name, "coef" = "lab", ci) %>% 
  mutate(lab = paste0(coef, " (", ci, ")")) %>% 
  select(-coef, -ci) %>% 
  spread(key = "activity_behavior", value = "lab") %>% 
  # Ordering labels
  mutate(var.name = factor(var.name, levels = c("Age (per 5 years increment)", "Non-white", "Lower occupational position", "Not married/cohabitating", 
                                                "Smoking status: past vs never smoker","Smoking status: current vs never smoker", 
                                                "Alcohol intake: none vs moderate", "Alcohol intake: high vs moderate", 
                                                paste0("Fruits & vegetables intake: < vs ", intToUtf8(8805)," than twice daily"), 
                                                "BMI: overweight vs normal","BMI: obese vs normal",
                                                "Mental component score (per 10 points decrement)",
                                                "Physical component score (per 10 points decrement)",
                                                "Number of chronic conditions (per new condition)"))) %>% 
  arrange(var.name)%>%
  rename("Covariate name" = "var.name",
         "Covriate subgroup" = "var.group")

# > Save
write.xlsx2(x = sup.tab.4.3, 
            file = paste0(path, "Supp_Table_4.xlsx"),
            row.names = FALSE, 
            sheetName = "Phase 3")

write.xlsx2(x = sup.tab.4.7, 
            file = paste0(path, "Supp_Table_4.xlsx"),
            row.names = FALSE, 
            sheetName = "Phase 7", 
            append = T)

write.xlsx2(x = sup.tab.4.11, 
            file = paste0(path, "Supp_Table_4.xlsx"),
            row.names = FALSE, 
            sheetName = "Phase 11", 
            append = T)


#--------------------------------------------------------------------
# --- Supplementary Figure 2 --- 
# Association between sociodemographic, lifestyle and health-related factors in 2012-2013 with acceleration distribution in 2012-2013 in men (A) and women (B).

sp2 <- plot_grid(
  ffm_plot(fm_model = coef_m2_11_man, legend = TRUE, title = "A. Men (n = 2910)", limits.max = limits.max.plots, limits.min = limits.min.plots),
  ffm_plot(fm_model = coef_m2_11_woman, legend = FALSE, title = "B. Women (n = 986)", limits.max = limits.max.plots, limits.min = limits.min.plots),
  rel_widths = c(0.65, 0.35),
  ncol = 2) 

ggsave(plot = sp2, 
       filename = paste0(path, "Supp_Figure_2.png"),
       width    = 10,
       height   = 4.5,
       dpi      = 300)
ggsave(plot = sp2, 
       filename = paste0(path, "Supp_Figure_2.pdf"),
       width    = 10,
       height   = 4.5,
       dpi      = 300)

#--------------------------------------------------------------------
# --- Supplementary Figures 3 & 4 --- 
# > Post-hoc analysis adusted on past PA 

ggsave(flm_fm_plot(flm_fm_coef = coef_m2_7_man_s1, limits.min = limits.min.plots, limits.max = limits.max.plots),
       filename = paste0(path, "Sensitivity analyses\\Phase_7_man_past_PA.png"),
       width    = 10,
       height   = 5,
       dpi      = 300)

ggsave(flm_fm_plot(flm_fm_coef = coef_m2_7_woman_s1, limits.min = limits.min.plots, limits.max = limits.max.plots),
       filename = paste0(path, "Sensitivity analyses\\Phase_7_woman_past_PA.png"),
       width    = 10,
       height   = 5,
       dpi      = 300)

ggsave(flm_fm_plot(flm_fm_coef = coef_m2_3_man_s1, limits.min = limits.min.plots, limits.max = limits.max.plots),
       filename = paste0(path, "Sensitivity analyses\\Phase_3_man_past_PA.png"),
       width    = 10,
       height   = 5,
       dpi      = 300)

ggsave(flm_fm_plot(flm_fm_coef = coef_m2_3_woman_s1, limits.min = limits.min.plots, limits.max = limits.max.plots),
       filename = paste0(path, "Sensitivity analyses\\Phase_3_woman_past_PA.png"),
       width    = 10,
       height   = 5,
       dpi      = 300)

# > Men
# Supplementary Figure 3 - Association between sociodemographic, lifestyle and health-related factors in (A) 1991-1994 and in (B) 2002-2004 with acceleration in men, estimated using functional regression adjusted on all covariates, waking time and on self-reported MVPA. 

sp3 <- plot_grid(
  ffm_plot(fm_model = coef_m2_3_man_s1, legend = TRUE, title = "A. 1991-1993 (n = 2823)", limits.max = limits.max.plots, limits.min = limits.min.plots),
  ffm_plot(fm_model = coef_m2_7_man_s1, legend = FALSE, title = "B. 2002-2004 (n = 2828)", limits.max = limits.max.plots, limits.min = limits.min.plots),
  rel_widths = c(0.65, 0.35),
  ncol = 2)
  
ggsave(plot = sp3, 
       filename = paste0(path, "Supp_Figure_3.png"),
       width    = 10,
       height   = 5,
       dpi      = 300)
ggsave(plot = sp3, 
       filename = paste0(path, "Supp_Figure_3.pdf"),
       width    = 10,
       height   = 5,
       dpi      = 300)

# > Women
# Supplementary Figure 4 - Association between sociodemographic, lifestyle and health-related factors in (A) 1991-1994 and in (B) 2002-2004 with acceleration in women, estimated using functional regression adjusted on all covariates, waking time and on self-reported MVPA. 

sp4 <- plot_grid(
  ffm_plot(fm_model = coef_m2_3_woman_s1, legend = TRUE, title = "A. 1991-1993 (n = 985)", limits.max = limits.max.plots, limits.min = limits.min.plots),
  ffm_plot(fm_model = coef_m2_7_woman_s1, legend = FALSE, title = "B. 2002-2004 (n = 954)", limits.max = limits.max.plots, limits.min = limits.min.plots),
  rel_widths = c(0.65, 0.35),
  ncol = 2)

ggsave(plot = sp4, 
       filename = paste0(path, "Supp_Figure_4.png"),
       width    = 10,
       height   = 5,
       dpi      = 300)
ggsave(plot = sp4, 
       filename = paste0(path, "Supp_Figure_4.pdf"),
       width    = 10,
       height   = 5,
       dpi      = 300)

#--------------------------------------------------------------------
# --- Supplementary Figures 5 & 6 --- 
# > Analysis 2 - individuals with all data 

# phase 3
flm_fm_plot(flm_fm_coef = coef_m2_3_man_s2, limits.min = limits.min.plots, limits.max = limits.max.plots)
ggsave(filename = paste0(path, "Sensitivity analyses\\Phase_3_man_2688.png"),
       width    = 10,
       height   = 5,
       dpi      = 300)

flm_fm_plot(flm_fm_coef = coef_m2_3_woman_s2, limits.min = limits.min.plots, limits.max = limits.max.plots)
ggsave(filename = paste0(path, "Sensitivity analyses\\Phase_3_woman_891.png"),
       width    = 10,
       height   = 5,
       dpi      = 300)

# phase 7
flm_fm_plot(flm_fm_coef = coef_m2_7_man_s2, limits.min = limits.min.plots, limits.max = limits.max.plots)
ggsave(filename = paste0(path, "Sensitivity analyses\\Phase_7_man_2688.png"),
       width    = 10,
       height   = 5,
       dpi      = 300)

flm_fm_plot(flm_fm_coef = coef_m2_7_woman_s2, limits.min = limits.min.plots, limits.max = limits.max.plots)
ggsave(filename = paste0(path, "Sensitivity analyses\\Phase_7_woman_891.png"),
       width    = 10,
       height   = 5,
       dpi      = 300)

# phase 11
flm_fm_plot(flm_fm_coef = coef_m2_11_man_s2, limits.min = limits.min.plots, limits.max = limits.max.plots)
ggsave(filename = paste0(path, "Sensitivity analyses\\Phase_11_man_2688.png"),
       width    = 10,
       height   = 5,
       dpi      = 300)

flm_fm_plot(flm_fm_coef = coef_m2_11_woman_s2, limits.min = limits.min.plots, limits.max = limits.max.plots)
ggsave(filename = paste0(path, "Sensitivity analyses\\Phase_11_woman_891.png"),
       width    = 10,
       height   = 5,
       dpi      = 300)

# phases 3, 7 and 11
sp5 <- plot_grid(ffm_plot(fm_model = coef_m2_3_man_s2, legend = TRUE, title = "A. 1991-1993", limits.min = limits.min.plots, limits.max = limits.max.plots),
          ffm_plot(fm_model = coef_m2_7_man_s2, legend = FALSE, title = "B. 2002-2004", limits.min = limits.min.plots, limits.max = limits.max.plots),
          ffm_plot(fm_model = coef_m2_11_man_s2, legend = FALSE, title = "C. 2012-2013", limits.min = limits.min.plots, limits.max = limits.max.plots),
          ncol = 3, 
          rel_widths = c(0.5, 0.225, 0.225))

ggsave(plot = sp5, 
       filename = paste0(path, "Supp_Figure_5.png"),
       width    = 11,
       height   = 5,
       dpi      = 300)
ggsave(plot = sp5, 
       filename = paste0(path, "Supp_Figure_5.pdf"),
       width    = 11,
       height   = 5,
       dpi      = 300)

sp6 <- plot_grid(
  ffm_plot(fm_model = coef_m2_3_woman_s2, legend = TRUE, title = "A. 1991-1993", limits.min = limits.min.plots, limits.max = limits.max.plots),
  ffm_plot(fm_model = coef_m2_7_woman_s2, legend = FALSE, title = "B. 2002-2004", limits.min = limits.min.plots, limits.max = limits.max.plots),
  ffm_plot(fm_model = coef_m2_11_woman_s2, legend = FALSE, title = "C. 2012-2013", limits.min = limits.min.plots, limits.max = limits.max.plots),
  ncol = 3, 
  rel_widths = c(0.5, 0.225, 0.225))

ggsave(plot = sp6, 
       filename = paste0(path, "Supp_Figure_6.png"),
       width    = 11,
       height   = 5,
       dpi      = 300)
ggsave(plot = sp6, 
       filename = paste0(path, "Supp_Figure_6.pdf"),
       width    = 11,
       height   = 5,
       dpi      = 300)

#--------------------------------------------------------------------
# --- Supplementary Figures 7 & 8 --- 
# > Analysis 3 - post-hoc analysis additionally adjusted for social interactions

# > Figure S7: Association between sociodemographic, lifestyle and health-related factors with acceleration in men, estimated using functional regression adjusted on all covariates, waking time and on social interactions. 
sp7 <- flm_fm_plot(coef_m2_11_man_s3, limits.max = limits.max.plots, limits.min = limits.min.plots)
    
ggsave(plot = sp7, 
       filename = paste0(path, "Supp_Figure_7.png"),
       width    = 11,
       height   = 4.5,
       dpi      = 300)
ggsave(plot = sp7, 
       filename = paste0(path, "Supp_Figure_7.pdf"),
       width    = 11,
       height   = 4.55,
       dpi      = 300)

# > Figure S7: Association between sociodemographic, lifestyle and health-related factors with acceleration in women, estimated using functional regression adjusted on all covariates, waking time and on social interactions. 
sp8 <- flm_fm_plot(coef_m2_11_woman_s3, limits.max = limits.max.plots, limits.min = limits.min.plots)
  
ggsave(plot = sp8, 
       filename = paste0(path, "Supp_Figure_8.png"),
       width    = 11,
       height   = 4.5,
       dpi      = 300)
ggsave(plot = sp8, 
       filename = paste0(path, "Supp_Figure_8.pdf"),
       width    = 11,
       height   = 4.5,
       dpi      = 300)
#--------------------------------------------------------------------
# --- Saving models outputs ---

path <- "E:\\PC_FIXE\\Analysis\\01_ARTICLE_1\\05_FINAL_ANALYSES_FIGURES_2\\Tables_Figures\\"

# > Cross-sectional models
list(
  A1_sb = summary(lm1_sb),
  A2_lipa = summary(lm1_lipa),
  A3_mvpa = summary(lm1_mvpa),
  B1_sb_full = summary(lm1_full_sb),
  B2_lipa_full = summary(lm1_full_lipa),
  B3_mvpa_full = summary(lm1_full_mvpa),
  C1_sb_full_int = summary(lm1_full_int_sb),
  C2_lipa_full_int = summary(lm1_full_int_lipa),
  C3_mvpa_full_int = summary(lm1_full_int_mvpa),
  D1_sb_man = summary(m2_11_man$lm_fit$SB),
  D2_lipa_man = summary(m2_11_man$lm_fit$LIPA),
  D3_mvpa_man = summary(m2_11_man$lm_fit$MVPA),
  E1_sb_woman = summary(m2_11_woman$lm_fit$SB),
  E2_lipa_woman = summary(m2_11_woman$lm_fit$LIPA),
  E3_mvpa_woman = summary(m2_11_woman$lm_fit$MVPA)
) %>% 
  map_dfr(tidy, .id = "model") %>%
  split(.$model) %>% 
  map(~ {  
    
    write.xlsx2(x = .x, 
                file = paste0(path, "Models_outputs\\Models_output_phase_11.xlsx"),
                sheetName = paste0(unique(.x$model)),
                append = T)
    
    
  })

# Cannot be extract by tidy(), need to be done by hand
summary(fm1_min)
summary(fm1_full)
summary(fm1_full_int)
summary(fm1_int)
summary(m2_11_man$fm_fit)
summary(m2_11_woman$fm_fit)

# > Cross-sectional models
# phase 7
list(
  #A1_sb = summary(lm1_sb),
  #A2_lipa = summary(lm1_lipa),
  #A3_mvpa = summary(lm1_mvpa),
  B1_sb_full = summary(lm1_full_sb_7),
  B2_lipa_full = summary(lm1_full_lipa_7),
  B3_mvpa_full = summary(lm1_full_mvpa_7),
  C1_sb_full_int = summary(lm1_full_int_sb_7),
  C2_lipa_full_int = summary(lm1_full_int_lipa_7),
  C3_mvpa_full_int = summary(lm1_full_int_mvpa_7),
  D1_sb_man = summary(m2_7_man$lm_fit$SB),
  D2_lipa_man = summary(m2_7_man$lm_fit$LIPA),
  D3_mvpa_man = summary(m2_7_man$lm_fit$MVPA),
  E1_sb_woman = summary(m2_7_woman$lm_fit$SB),
  E2_lipa_woman = summary(m2_7_woman$lm_fit$LIPA),
  E3_mvpa_woman = summary(m2_7_woman$lm_fit$MVPA)
) %>% 
  map_dfr(tidy, .id = "model") %>%
  split(.$model) %>% 
  map(~ {  
    
    write.xlsx2(x = .x, 
                file = paste0(path, "Models_outputs\\Models_output_phase_7.xlsx"),
                sheetName = paste0(unique(.x$model)),
                append = T)
    
    
  })

# Cannot be extract by tidy(), need to be done by hand
summary(fm1_full_7)
summary(fm1_full_int_7)
summary(fm1_int_7)
summary(m2_7_man$fm_fit)
summary(m2_7_woman$fm_fit)

# phase 3
list(
  #A1_sb = summary(lm1_sb),
  #A2_lipa = summary(lm1_lipa),
  #A3_mvpa = summary(lm1_mvpa),
  B1_sb_full = summary(lm1_full_sb_3),
  B2_lipa_full = summary(lm1_full_lipa_3),
  B3_mvpa_full = summary(lm1_full_mvpa_3),
  C1_sb_full_int = summary(lm1_full_int_sb_3),
  C2_lipa_full_int = summary(lm1_full_int_lipa_3),
  C3_mvpa_full_int = summary(lm1_full_int_mvpa_3),
  D1_sb_man = summary(m2_3_man$lm_fit$SB),
  D2_lipa_man = summary(m2_3_man$lm_fit$LIPA),
  D3_mvpa_man = summary(m2_3_man$lm_fit$MVPA),
  E1_sb_woman = summary(m2_3_woman$lm_fit$SB),
  E2_lipa_woman = summary(m2_3_woman$lm_fit$LIPA),
  E3_mvpa_woman = summary(m2_3_woman$lm_fit$MVPA)
) %>% 
  map_dfr(tidy, .id = "model") %>%
  split(.$model) %>% 
  map(~ {  
    
    write.xlsx2(x = .x, 
                file = paste0(path, "Models_outputs\\Models_output_phase_3.xlsx"),
                sheetName = paste0(unique(.x$model)),
                append = T)
    
    
  })

# Cannot be extract by tidy(), need to be done by hand
summary(fm1_full_3)
summary(fm1_full_int_3)
summary(fm1_int_3)
summary(m2_3_man$fm_fit)
summary(m2_3_woman$fm_fit)




