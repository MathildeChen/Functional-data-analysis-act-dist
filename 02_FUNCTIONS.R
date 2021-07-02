# Script name: 02_FUNCTIONS.R
# 
# Author: M.Chen, Inserm, 2020
#
# Doing: Loading functions 
# - to fit longitudinal models
# - to extract coefficents and p.values
# - to plot the associations

# Source this script to use the functions

#--------------------------------------------------------------------
# Packages
# > Tools
library(pracma)
library(refund)
library(tidyverse)
library(testthat)

# > Graphics
library(ggplot2)
library(cowplot)
library(wesanderson)

#--------------------------------------------------------------------
# Graphical settings
# > Color palettes

pal <- wes_palette(name = "Zissou1", 5, "discrete")
pal_bicolor <- pal[c(1,5)] 

# > Theme for plots
theme_perso <- theme(
  axis.title.x = element_text(size = 9),
  axis.text = element_text(size = 8, color = "black"),
  axis.line = element_blank(),
  strip.placement = "outside",
  strip.text = element_text(size = 8, face = "bold"),
  legend.justification = "center",
  legend.position = "bottom",
  legend.text = element_text(size = 8),
  legend.title = element_text(size = 8),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  plot.title = element_text(face = "plain", size = 9),
  plot.subtitle = element_text(face = "plain", size = 9),
  #strip.text = element_text(color = "black"),
  panel.background = element_rect(fill = "darkgrey"))

# > Table with covariables labels for plots 
table.name <- tibble::tribble(
  ~var,                         ~var.name,
  "O_waking_time(xi)",          "Daily waking time (confusion factor)",
  "A_sex(xi)",                  "Being a man",
  "A_age_conti_5(xi)",          "Age (per 5 years)",
  "A_ethnicity(xi)",            "Non-white",
  "A_socio_eco_cont(xi)",       "Lower occupational position",
  "A_marital_status(xi)",       "Not married/cohabitating",
  "B_ex_smokers(xi)",           "Smoking status: past vs never smoker",
  "B_current_smokers(xi)",      "Smoking status: current vs never smoker",
  "B_alc_0(xi)",                "Alcohol intake: none vs moderate",
  "B_alc_more_14(xi)",          "Alcohol intake: high vs moderate",
  "B_social_act(xi)",           "Social interactions (per 1 SD increment)",
  "B_fg_2(xi)",                 paste0("Fruits & vegetables intake: < vs ", intToUtf8(8805)," than twice daily"),
  "B_mvpa_inactive(xi)",        paste0("Self-reported MVPA: none vs ", intToUtf8(8805), " 2.5 h/week"),
  "B_mvpa_less_2.5(xi)",        paste0("Self-reported MVPA: < vs ", intToUtf8(8805), " 2.5 h/week"),
  "C_bmi_overweight(xi)",       "BMI: overweight vs normal",
  "C_bmi_obese(xi)",            "BMI: obese vs normal",
  "D_mmm_index(xi)",            "Number of chronic conditions (per new condition)",
  "D_pcs(xi)",                  "Physical health score (per 10 points)",
  "D_mcs(xi)",                  "Mental health score (per 10 points)") %>% 
  # Ordering labels
  mutate(var.name = factor(var.name, levels = c("Daily waking time (confusion factor)",
                                                "Not married/cohabitating", "Lower occupational position", "Non-white", "Age (per 5 years)", "Being a man", 
                                                paste0("Self-reported MVPA: < vs ", intToUtf8(8805), " 2.5 h/week"), paste0("Self-reported MVPA: none vs ", intToUtf8(8805), " 2.5 h/week"), 
                                                paste0("Fruits & vegetables intake: < vs ", intToUtf8(8805)," than twice daily"),
                                                "Social interactions (per 1 SD increment)",
                                                "Alcohol intake: high vs moderate", "Alcohol intake: none vs moderate", 
                                                "Smoking status: current vs never smoker", "Smoking status: past vs never smoker",
                                                "Number of chronic conditions (per new condition)",
                                                "Physical health score (per 10 points)",
                                                "Mental health score (per 10 points)",
                                                "BMI: obese vs normal", "BMI: overweight vs normal"))) %>% 
  # Create variable categories
  mutate(var.group = if_else(str_detect(var, "O_") == TRUE, "Confusion factor", 
                     if_else(str_detect(var, "A_") == TRUE, "Socio-\ndemographics", 
                     if_else(str_detect(var, "B_") == TRUE, "Behavioural",
                     #if_else(str_detect(var, "B_") == TRUE, "Behavioral",
                     #if_else(str_detect(var, "B_") == TRUE, "Lifestyle",
                     #if_else(str_detect(var, "C_") == TRUE, "Cardiometabolic\nrisk factors",
                     "General\nhealth"))))

#--------------------------------------------------------------------
# Functions

# > Function to integrate functional coefficient on 0.005 intervals
farea <- function(model, vars){
  
  # > Coef extraction
  coefs <- coef(model, seWithMean = FALSE, useVc = FALSE)$smterms
  
  # > Take only selected covariates 
  coefs_2 <- coefs[which(names(coefs) %in% vars)]
  
  # > Put data into one data.frame
  out <- coefs_2 %>% 
    map_dfr(~ { .x$coef %>% 
        # > Rename some variables
        rename(ref = 2, "xi" = "xi.vec")  %>%
        # > Compute 95% IC (under normality hypothese)
        mutate(ic_up   = value + 1.96*se,
               ic_down = value - 1.96*se) %>% 
        # > Compute significativity range of the coefficient
        mutate(sign_temp = ic_up*ic_down, 
               sign = if_else(sign_temp > 0, "Significativ", "No significativ")) %>% 
        # > Integrate functional coefficient on 0.05 intervals
        mutate(area_cum = cumtrapz(xi, value),
               area = lead(area_cum, default = 0) - area_cum) %>% 
        # > Data formatting
        select(-ref, -sign_temp, -area_cum)
      
    }, .id = "var")
  
  return(out)
  
}

# > Function to adjust linear and functional models
flm_fm_fitting <- function(data_lm = NULL, data_fm = NULL, vars = NULL){
  
  # If data are not provided, default is the full population
  if(is.null(data_lm) == T)
  {
    data_lm <- data0
    warning("Warning: data_lm is not provided: linear models are fitted on the full population")
  }
  if(is.null(data_fm) == T)
  {
    data_lm <- data1
    warning("Warning: data_fm is not provided: functional model is fitted on the full population")
  }
  # If vars is not provided, default is the full set of covariates
  if(is.null(vars) == T)
  {
    vars <- paste(names(data0 %>% 
                          select(starts_with("A_"), starts_with("B_"), starts_with("C_"), starts_with("D_"))), collapse = " + ")
    
    warning(paste0("Warning: is.null(vars) == T, exposure is not provided: the full model is fitted\n", vars))
    
  }
  
  # ============================================================
  # Fitting linear models
  # SB daily time ~ vars
  lm_s <- lm(formula = as.formula(paste0("dur_day_total_in_min_pla ~ O_waking_time + ", vars)), 
             data = data_lm)
  # LIPA daily time ~ vars
  lm_l <- lm(formula = as.formula(paste0("dur_day_total_lig_min_pla ~ O_waking_time + ", vars)), 
             data = data_lm)
  # MVPA daily time ~ vars 
  lm_m <- lm(formula = as.formula(paste0("dur_day_total_mvpa_min_pla ~ O_waking_time + ", vars)), 
             data = data_lm)
  
  # ============================================================
  # Fitting functional model 
  fm <- pffr(formula = as.formula(paste0("Y_Activity ~  O_waking_time + ", vars)), 
             yind = xi, 
             bs.yindex = list(bs = "ps", k=-1), 
             bs.int = list(bs = "ps", k = 50),
             data = data_fm)
  
  # Extracting coefficients, 95% CI and significativity range from functional model
  
  # Output
  flm_fm_fit <- list(
    
    # Linear fits
    lm_fit = list("SB"   = lm_s, 
                  "LIPA" = lm_l, 
                  "MVPA" = lm_m), 
    # Functional fit
    fm_fit = fm
    
  )
  
  return(flm_fm_fit)
  
}

# > Function to extract models coefficients and association significativity level
flm_fm_coef <- function(lm_fm_fit){
  
  # Test if the lm_fm_fit 
  if(names(lm_fm_fit)[1] != "lm_fit" | names(lm_fm_fit)[2] != "fm_fit")
  { 
    
    stop("lm_fm_fit should be an object computed using flm_fm_fitting() function provided in the function.R script")  
    
  }
  
  # ============================================================
  # Linear models
  
  # Extracting regression coefficients, 95% CI and p.values from linear models 
  coef_lm <- lm_fm_fit$lm_fit %>% 
    map_dfr(., ~ { 
      
      cbind(data.frame(var = names(.x$coefficients),
                       coef = .x$coefficients,
                       p.value = summary(.x)$coefficients[,4]), 
            confint(.x, level = 0.95)) %>%
        rename("d" = 4, "u" = 5) %>%
        mutate(lab = paste0(formatC(coef, digits = 1, format = "f")),
               ci = paste0(formatC(d, digits = 1, format = "f"), ", ", formatC(u, digits = 1, format = "f")),
               p.value = round(p.value, digits = 3),
               col = if_else(p.value < 0.05 & coef > 0, 1, if_else(p.value < 0.05 & coef < 0, -1, 0))) %>% 
        select(var, lab, ci, p.value, col) %>% 
        # Global association significativity 
        mutate(global.sign = if_else(p.value < 0.05, 1, 0)) %>% 
        # Labels 
        mutate(p.value.lab = if_else(p.value < 0.05 & p.value >= 0.01, "*", 
                             if_else(p.value < 0.01 & p.value >= 0.001, "**",
                             if_else(p.value < 0.001 & p.value >= 0, "***", 
                             " "))))
      
    }, .id = "model") %>% 
    filter(var != "(Intercept)",
           var != "O_waking_time") %>% 
    left_join(table.name %>% 
                mutate(var = str_sub(var, start = 1, end = nchar(var)-4)) %>% 
                mutate(var.group = factor(var.group, levels = rev(c("General\nhealth", 
                                                                    #"Cardiometabolic\nrisk factors", 
                                                                    "Behavioural",
                                                                    #"Behavioral",
                                                                    #"Lifestyle", 
                                                                    "Socio-\ndemographics")))), 
              by = "var") %>%
    mutate(activity_behavior = factor(model, levels = c("SB", "LIPA", "MVPA")))
  
  # ============================================================
  # Functional model 
  
  # Extracting coefficients, 95% CI and significativity range from functional model
  associations_fm <- farea(model = lm_fm_fit$fm_fit, 
                           vars = unique(table.name$var))
  
  # Extracting the overall significativity 
  p.val_fm <- data.frame(var = as.vector(row.names(summary(lm_fm_fit$fm_fit)$s.table)), 
                         p.value = summary(lm_fm_fit$fm_fit)$s.table[,4], row.names = NULL) %>% 
    # Global association significativity 
    mutate(global.sign = if_else(p.value < 0.05, 1, 0)) %>% 
    # Labels 
    mutate(p.value.lab = if_else(p.value < 0.05 & p.value >= 0.01, "*", 
                         if_else(p.value < 0.01 & p.value >= 0.001, "**",
                         if_else(p.value < 0.001 & p.value >= 0, "***", 
                         " "))))
  
  # Merge both tables
  coef_fm <- left_join(associations_fm, p.val_fm, by = "var") %>% 
    mutate(activity_behavior = if_else(xi <  log(0.04 + 1), "SB", 
                                       if_else(xi >= log(0.1  + 1), "MVPA", "LIPA")),
           activity_behavior = factor(activity_behavior, levels = c("SB", "LIPA", "MVPA"))) %>% 
    filter(var != "Intercept(xi))",
           var != "O_waking_time(xi)") %>% 
    left_join(table.name, by = "var") %>% 
    mutate(var.group = factor(var.group, levels = rev(c("General\nhealth", 
                                                        #"Cardiometabolic\nrisk factors", 
                                                        "Behavioural",
                                                        #"Behavioral",
                                                        #"Lifestyle", 
                                                        "Socio-\ndemographics")))) 
  
  # Output
  flm_fm_coef <- list(
    coef_lm = coef_lm, 
    coef_fm = coef_fm
  )
  
  return(flm_fm_coef)
  
}

# > Function to plot the result from flm_fm functions
# flm_fm_object = a list of coefficients for linear model (coef_lm) and coefficients for functional model (coef_fm)
flm_fm_plot <- function(flm_fm_coef, limits.min = NULL, limits.max = NULL){
  
  # Check if limits are provided. 
  if(is.null(limits.min) == T)
  {
    limits.min <- min(flm_fm_coef$coef_fm$area)
    warning("No min limits provided, taking the min area by defaut")
  } 
  if(is.null(limits.max) == T)
  {
    limits.max <- max(flm_fm_coef$coef_fm$area)
    warning("No max limits provided, taking the max area by defaut")
  }
  
  # ============================================================
  # Plot for functional coefficient
  
  # Number of variables, used in linear models plot
  n.vars <- length(unique(flm_fm_coef$coef_lm$var))
  
  # If only 1 variable (except confusion factor)
  if(n.vars == 1)
  {
    
    # Transparency setting
    alpha_global_sign <- if_else(unique(flm_fm_coef$coef_fm$global.sign) == 1, 1, 0.1)
    
    # Plot
    p_fm <- flm_fm_coef$coef_fm %>% 
      ggplot(data = .) +
      geom_tile(aes(x = xi+0.00491952/2, y = var.name, fill = area), 
                #alpha = alpha_global_sign, 
                height = 0.75) + 
      geom_vline(aes(xintercept = log(1 + 0.04)), linetype = 2, color = "black") +
      geom_vline(aes(xintercept = log(1 + 0.1)), linetype = 2, color = "black") + 
      geom_text(aes(x = -0.01, y = var.name, label = p.value.lab), col = "black", size = 3, check_overlap = T) +
      scale_x_continuous(labels = c(seq(0, 0.2, by = 0.05)), breaks = log(seq(0, 0.2, by = 0.05) + 1), limits = c(-0.01, log(0.2 + 1))) + 
      scale_fill_gradientn(colours = c("darkred", pal[5], "orange", pal[3], "white", pal[2], "blue", "darkblue", "black"), 
                           values = c(1, (0 - limits.min)/(limits.max - limits.min), 0), 
                           limits = c(limits.min, limits.max),
                           breaks = c(-20, -15, -10, -5, 0, 5, 10, 15, 20),
                           guide = guide_colourbar(title = "min", 
                                                   title.position = "right",
                                                   title.vjust = 0.2,
                                                   barheight = 0.3, 
                                                   barwidth = 20,
                                                   label = T)) +
      ggtitle("A. Function-on-scalar regression", subtitle = expression(paste("difference in time (minutes/day) spent in 0.005 ", italic("g"), " intervals"))) +
      #labs(x = expression(paste("Acceleration (in ", italic("g"), ")")), caption = "Signif. codes:  <0.001 '***'; [0.001-0.1[ '**'; [0.01-0.05] '*' >0.05 ' ' ")  +
      labs(x = expression(paste("Acceleration (in ", italic("g"), ")")))  +
      theme_perso + 
      theme(legend.position = "top",
            axis.title.y = element_blank(),
            #panel.spacing.x=unit(0, "lines"), 
            panel.spacing = unit(0, "lines"),
            plot.caption = element_text(size = 8, color = "black", hjust = 0))
    
  }
  # If more than 1 variable  
  if(n.vars > 1)
  {
    
    p_fm <- flm_fm_coef$coef_fm %>% 
      ggplot(data = .) +
      geom_tile(aes(x = xi+0.00491952/2, y = var.name, fill = area), 
                    #alpha = factor(global.sign)), 
                    height = 0.75) + 
      geom_vline(aes(xintercept = log(1 + 0.04)), linetype = 2, color = "black") +
      geom_vline(aes(xintercept = log(1 + 0.1)), linetype = 2, color = "black") + 
      geom_text(aes(x = -0.01, y = var.name, label = p.value.lab), col = "black", size = 3, check_overlap = T) +
      scale_alpha_discrete(range = c(0.1,1), guide = "none") +
      scale_x_continuous(labels = c(seq(0, 0.2, by = 0.05)), breaks = log(seq(0, 0.2, by = 0.05) + 1), limits = c(-0.01, log(0.2 + 1))) + 
      scale_fill_gradientn(colours = c("darkred", pal[5], "orange", pal[3], "white", pal[2], "blue", "darkblue", "black"), 
                           limits = c(limits.min, limits.max),
                           breaks = c(-20, -15, -10, -5, 0, 5, 10, 15, 20),
                           values = c(1, (0 - limits.min)/(limits.max - limits.min), 0), 
                           guide = guide_colourbar(title = "min", 
                                                   title.position = "right",
                                                   title.vjust = 0.2,
                                                   barheight = 0.3, 
                                                   barwidth = 20,
                                                   label = T)) +
      facet_grid(var.group ~ ., scales = "free", space = "free", switch = "y") + 
      ggtitle("A. Function-on-scalar regression", subtitle = expression(paste("difference in time (minutes/day) spent in 0.005 ", italic("g"), " intervals"))) +
      #labs(x = expression(paste("Acceleration (in ", italic("g"), ")")), caption = "Signif. codes:  <0.001 '***'; [0.001-0.1[ '**'; [0.01-0.05] '*' >0.05 ' ' ")  +
      labs(x = expression(paste("Acceleration (in ", italic("g"), ")")))  +
      theme_perso + 
      theme(legend.position = "top",
            axis.title.y = element_blank(),
            #panel.spacing.x=unit(0, "lines"), 
            panel.spacing = unit(0, "lines"),
            plot.caption = element_text(size = 8, color = "black", hjust = 0))
    
  }
  
  # ============================================================
  # Plot for linear model
  p_lm <- flm_fm_coef$coef_lm %>% 
    mutate(col = factor(col, levels = c(-1,1,0))) %>% 
    ggplot(., aes(x = activity_behavior, y = var.name)) + 
    geom_tile(aes(fill = factor(col)), height = 0.75) +
    geom_text(aes(label = paste0(lab, " ", p.value.lab)), size = 3) +
    geom_vline(aes(xintercept = 1.75), linetype = 2, color = "black") +
    geom_vline(aes(xintercept = 2.75), linetype = 2, color = "black") + 
    scale_fill_manual(values = c(pal[1], pal[5], "white")) + 
    facet_grid(var.group ~ activity_behavior, scales = "free", space = "free", switch = "y")  +
    ggtitle("B. Linear regression", subtitle = "difference in time (minutes/day) spent in\nSB, LIPA and MVPA\n") + 
    #labs(x = " ", y = " ", caption = " ") +
    labs(x = " ", y = " ") +
    theme_perso + 
    theme(legend.position = "none",
          strip.background = element_blank(),
          strip.text.y = element_blank(),
          axis.ticks = element_blank(),
          axis.text.x = element_text(color = "transparent"),
          axis.text.y = element_blank(),
          panel.spacing.x=unit(0, "lines"),
          #panel.background = element_rect(fill = "transparent"),
          panel.spacing = unit(0, "lines"),
          plot.caption = element_text(size = 8, color = "black")) 
  
  
  # Arrange both plots in one 
  p <- plot_grid(p_fm, p_lm, 
                 ncol = 2, 
                 rel_widths = c(6/8, 2/8))
  return(p)
  
}


# > Function to plot the results from a fm model
# flm_fm_object = a list of coefficients for linear model (coef_lm) and coefficients for functional model (coef_fm)
ffm_plot <- function(fm_model, 
                     limits.min = NULL, 
                     limits.max = NULL, 
                     legend = TRUE,
                     title = NULL){
  
  # Check if limits are provided. 
  if(is.null(limits.min) == T)
  {
    limits.min <- min(fm_model$coef_fm$area)
    warning("No min limits provided, taking the min area by defaut")
  } 
  if(is.null(limits.max) == T)
  {
    limits.max <- max(fm_model$coef_fm$area)
    warning("No max limits provided, taking the max area by defaut")
  }
  
  # ============================================================
  # Plot for functional coefficient
  # If you want to plot with the labels
  if(legend == TRUE)
  {
    
    p_fm <- fm_model$coef_fm %>% 
      ggplot(data = .) +
      geom_tile(aes(x = xi+0.00491952/2, y = var.name, fill = area), 
                #alpha = factor(global.sign)), 
                height = 0.75) + 
      geom_vline(aes(xintercept = log(1 + 0.04)), linetype = 2, color = "black") +
      geom_vline(aes(xintercept = log(1 + 0.1)), linetype = 2, color = "black") + 
      geom_text(aes(x = -0.01, y = var.name, label = p.value.lab), col = "black", size = 3, check_overlap = T) +
      #scale_alpha_discrete(range = c(0.1,1), guide = "none") +
      scale_x_continuous(labels = c(seq(0, 0.2, by = 0.05)), breaks = log(seq(0, 0.2, by = 0.05) + 1), limits = c(-0.01, log(0.2 + 1))) + 
      scale_fill_gradientn(colours = c("darkred", pal[5], "orange", pal[3], "white", pal[2], "blue", "darkblue", "black"), 
                           limits = c(limits.min, limits.max),
                           breaks = c(-20, -10, 0, 10, 20),
                           values = c(1, (0 - limits.min)/(limits.max - limits.min), 0), 
                           guide = guide_colourbar(title = "min", 
                                                   title.position = "right",
                                                   title.vjust = 0.2,
                                                   barheight = 0.3, 
                                                   barwidth = 10,
                                                   label = T)) +
      facet_grid(var.group ~ ., scales = "free", space = "free", switch = "y") + 
      ggtitle(paste0(title)) +
      #labs(x = expression(paste("Acceleration (in ", italic("g"), ")")), caption = "Signif. codes:  <0.001 '***'; [0.001-0.1[ '**'; [0.01-0.05] '*' >0.05 ' ' ")  +
      labs(x = expression(paste("Acceleration (in ", italic("g"), ")")))  +
      theme_perso + 
      theme(legend.position = "top",
            axis.title.y = element_blank(),
            #panel.spacing.x=unit(0, "lines"), 
            panel.spacing = unit(0, "lines"),
            plot.caption = element_text(size = 8, color = "black", hjust = 0))
    
  }
  # Without labels
  if(legend == FALSE)
  {
    
    p_fm <- fm_model$coef_fm %>% 
      ggplot(data = .) +
      geom_tile(aes(x = xi+0.00491952/2, y = var.name, fill = area), 
                height = 0.75) + 
      geom_vline(aes(xintercept = log(1 + 0.04)), linetype = 2, color = "black") +
      geom_vline(aes(xintercept = log(1 + 0.1)), linetype = 2, color = "black") + 
      geom_text(aes(x = -0.01, y = var.name, label = p.value.lab), col = "black", size = 3, check_overlap = T) +
      #scale_alpha_discrete(range = c(0.1,1), guide = "none") +
      scale_x_continuous(labels = c(seq(0, 0.2, by = 0.05)), breaks = log(seq(0, 0.2, by = 0.05) + 1), limits = c(-0.01, log(0.2 + 1))) + 
      scale_fill_gradientn(colours = c("darkred", pal[5], "orange", pal[3], "white", pal[2], "blue", "darkblue", "black"), 
                           limits = c(limits.min, limits.max),
                           breaks = c(-20, -10, 0, 10, 20),
                           values = c(1, (0 - limits.min)/(limits.max - limits.min), 0), 
                           guide = guide_colourbar(title = " ", 
                                                   title.position = "right",
                                                   title.vjust = 0.2,
                                                   barheight = 0.3, 
                                                   barwidth = 0,
                                                   label = T)) +
      facet_grid(var.group ~ ., scales = "free", space = "free", switch = "y") + 
      ggtitle(paste0(title)) +
      #labs(x = expression(paste("Acceleration (in ", italic("g"), ")")), caption = "Signif. codes:  <0.001 '***'; [0.001-0.1[ '**'; [0.01-0.05] '*' >0.05 ' ' ")  +
      labs(x = expression(paste("Acceleration (in ", italic("g"), ")")))  +
      theme_perso + 
      theme(legend.position = "top",
            legend.text = element_text(color = "white"),
            axis.title.y = element_blank(),
            strip.background = element_blank(),
            strip.text.y = element_blank(),
            axis.text.y = element_blank(),
            #panel.spacing.x=unit(0, "lines"), 
            panel.spacing = unit(0, "lines"),
            plot.caption = element_text(size = 8, color = "black", hjust = 0))
    
  }
  
  return(p_fm)
  
}

