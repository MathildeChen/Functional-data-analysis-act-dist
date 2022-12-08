# Script name: 01_1_KERNEL_DENSITY.R
# 
# Author: M.Chen, Inserm
#
# Doing: Estimating physical activity density with 60sec epoch data 
# (2020-05-15 data release)
#--------------------------------------------------------------------

# Features
Sys.setlocale("LC_ALL","English")

# Packages
# > Tools
library(tidyr)
library(plyr)
library(dplyr)
library(haven)
library(stringr)
library(purrr)
library(vroom)
library(ks)

# > Time managment
library(lubridate)
library(hms)

# > Graphics
library(ggplot2)
library(cowplot)
library(wesanderson)

# > Graphical settings
pal <- wes_palette("Zissou1", 5, type = "discrete")

#--------------------------------------------------------------------
# Information on participant

# > Person summary = Data set containing the correct set of patients (n = 4006). One line = patient
stata_data <- read_dta("E:\\PC_FIXE\\Data\\04_DAILY_ACT_SUM\\2020-05-15\\WW_L40M100V400_update052020.dta") %>%
  filter(exclusion == 0)

#--------------------------------------------------------------------
# Load data from participants with complete data 
# i.e. no invalid day

# > Path of 60sec epoch files
path.files <- "E:\\PC_FIXE\\Data\\01_TIME_SERIES\\2020_05_15\\ms5.outraw\\40_100_400\\"

# > Load raw data from participants with complete data 
acc_data <- data.frame(files = list.files(path = path.files,
                                          pattern = ".csv$", 
                                          recursive = TRUE)) %>% 
  mutate(stno = str_extract(files, "[0-9]+")) %>% 
  filter(stno %in% unique(stata_data$stno)) %>%
  split(.$stno, drop = T) %>%
  map_dfr(~ {
    
    i_dat <- vroom(file = paste0(path.files, unique(.x$files)), 
                   delim = ",")
    
  }, .id = "stno") %>% 
  # Compute logarithme of acc. data in g (because some data are skewed)
  mutate(ACC_g = ACC/1000, # convert acc in mg in acc in g
         log_ACC_g = log(1 + ACC_g), # compute the logarithm of the acc in g
         log_ACC = log(1 + ACC), # compute the logarithm of the acc in mg
         timestamp = as.POSIXct(timenum, origin="1970-1-1", tz="Europe/London")) # compute the timestamp

# > Delete unvalid days (i.e. days where less than 33% of the epoch are invalid) 
#   and sleep period 
acc_data_valid <- acc_data %>% 
  filter(invalid_wakinghours < 33,
         SleepPeriodTime == 0)

#--------------------------------------------------------------------

# 1) log(acc + 1), with acc in mg 

# > Coordinates where densities will be estimated
# > Min and max of the wanted coordinates
min.pts <- 0
max.pts <- 15000

# > 100 log distributed points between min and max values in log scale
pts <- ( exp( seq(min.pts, log(1 + max.pts), length.out = 150) ) - 1 ) / 100

#plot(x = pts_1, y = rep(0, 150), type = "p", pch= "l")

# > Estimate median bandwidth
h.med <- acc_data_valid %>% 
  split(.$stno) %>%
  # > Extract data from the large dataset
  map_dbl(~ {
    
    # > Data (neperian log of the acc in g)
    acc <- .x$log_ACC
    
    # > Density function estimation for the tested participant
    fit <- kde(acc,
               eval.points = pts, # at given points
               positive = TRUE) # only positive data
    
    # > Get bandwidth value
    fit$h
    
  }) %>%
  # > Median bandwidth
  median(.)

# Estimate individual density function with a commun bandwidth

dst <- acc_data_valid %>% 
  # > Keep only acceleration during waking time and valid days (needs to be implemented)
  
  split(.$stno) %>% 
  map_dfr(~ {
    
    # > Data (neperian log of the acc in g)
    acc <- .x$log_ACC
    
    # > Density function estimation for the tested participant
    fit <- kde(acc, 
               eval.points = pts, # at given coordinates
               h = h.med, # with a given bandwidth 
               positive = TRUE) # only positive data
    
    # > Continuous density function for positive values
    data.frame(x = fit$eval.points, # coordinates at which the function was estimated
               f_i = fit$estimate) # estimated function
    
  }, .id = "stno")

# > Check if there is no negative values
dst[f_i < 0,]

# > Mean density 
dst %>% 
  dplyr::group_by(x) %>%
  summarise(mean = mean(f_i),
            upper = quantile(f_i, probs = 0.75),
            lower = quantile(f_i, probs = 0.25)) %>% 
  mutate(group = if_else(x >= 0.1, "3. MVPA [0.1;0.5[", if_else(x < 0.04, "1. SD [0;0.04[", "2. LIPA [0.04;0.1["))) %>%
  ggplot(.) + 
  geom_ribbon(aes(x = x, ymin = lower, ymax = upper), alpha = 0.7, fill = pal[2]) + 
  geom_line(aes(x = x, y = mean), color = pal[1]) +
  geom_point(aes(x = pts_1, y = 0), color = pal[4], size = 0.1) + 
  #geom_area(aes(x = x, y = q.val, group = q.type), color = pal[1], lty = 2) +
  theme_bw() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.background = element_rect(fill = "white", colour = "white"),
        strip.background = element_rect(colour = "white", fill = "white")) + 
  facet_wrap(~group, ncol = 1, scales = "free")  +
  labs(x = "1) log(acc+1) (acc in g)", y = "Density") 

#--------------------------------------------------------------------
# Save
save(dst, file = "\\\\172.27.137.244\\vieillissement$\\Post doc & PhD\\Mathilde\\Analysis\\01_ARTICLE_1\\02_FUNCTION_ON_SCALAR\\test_refund\\2020-05-15_dst_4006.rda")





