# Script name: 01_2_STANDARDIZE_DENSITY.R
# 
# Author: M.Chen, Inserm
#
# Doing: Standardize density estimate
#--------------------------------------------------------------------

# Features
Sys.setlocale("LC_ALL","English")

# Packages
# > Tools
library(tidyr)
library(plyr)
library(dplyr)
library(purrr)
library(pracma)

#--------------------------------------------------------------------
# Data loading
load("E:\\PC_FIXE\\Data\\02_PA_DIST\\2020-05-15_dst_4006.rda")

#--------------------------------------------------------------------
# Compute total area under the curve

area_dst <- dst %>% 
  group_by(stno) %>% 
  summarise(surf = trapz(x, f_i))

summary(area_dst$surf) # > Some are not equal to 1

# Standardize density function by total area under the curve
dst_2 <- dst %>% 
  left_join(area_dst, by = "stno") %>% 
  mutate(f_i_2 = f_i/surf)

# Recompute total area 
area_dst_2 <- dst_2 %>% 
  group_by(stno) %>% 
  summarise(surf = trapz(x, f_i_2))

# Check if new total area = 1
summary(area_dst_2$surf)


#--------------------------------------------------------------------
# Save
save(dst_2, file = "E:\\PC_FIXE\\Data\\02_PA_DIST\\2020-05-15_dst_4006_trap.rda")

library(ggplot2)
dst_2 %>% 
  filter(stno == unique(dst_2$stno)[1],
         x < 0.3) %>% 
  ggplot(.) +
  geom_line(aes(x = x, y = f_i_2, color = stno)) + 
  geom_vline(aes(xintercept = 0.04), lty = 2) + 
  geom_vline(aes(xintercept = 0.1), lty = 2) + 
  theme_classic() + 
  theme(legend.position = "none") + 
  labs(x = expression(paste("Acceleration (in ", italic("g"), ")")), y = "Density")

