### PACKAGES

library(tidyverse)
library(mvtnorm)
library(brms)
library(cmdstanr)
library(reshape2)
library(tidybayes)
library(gridExtra)
library(ggpubr)
library(factoextra)
library(grid)
library(lme4)
library(rstatix)
library(cowplot)
library(ggtext)

### SOME FUNCTIONS

# some helpful functions and formulas

MakePlotData <- function(in_data) {
  out_data <- in_data %>%
    mutate(Focal = paste("Focal:", Focal)) %>%
    mutate(Resident = paste("Resident:", Resident)) %>%
    mutate(Inducer = as.factor(Inducer))
  return(out_data)
}

# Beverton-Holt model including effect of induction
bh_ind_formula <- bf(log(Seeds + 1) ~ log(lambda / (1  + betaBUG * BugInd + betaCEN * CenInd + betaPAP * PapInd + betaSIN * SinInd + Resident.Density * ((1 - ResLog) * (1 - FocLog) * (nonalphaBUG * BugLog + nonalphaCEN * CenLog + nonalphaPAP * PapLog + nonalphaSIN * SinLog) + (1 - ResLog) * FocLog * (focalphaBUG * BugLog + focalphaCEN * CenLog + focalphaPAP * PapLog + focalphaSIN * SinLog) + (1 - FocLog) * ResLog * (resalphaBUG * BugLog + resalphaCEN * CenLog + resalphaPAP * PapLog + resalphaSIN * SinLog)))),
                     lambda ~ 1,
                     betaBUG ~ 1, betaCEN ~ 1, betaPAP ~ 1, betaSIN ~ 1,
                     nonalphaBUG ~ 1, nonalphaCEN ~ 1, nonalphaPAP ~ 1, nonalphaSIN ~ 1,
                     focalphaBUG ~ 1, focalphaCEN ~ 1, focalphaPAP ~ 1, focalphaSIN ~ 1,
                     resalphaBUG ~ 1, resalphaCEN ~ 1, resalphaPAP ~ 1, resalphaSIN ~ 1,
                     nl = TRUE)

# Law-Watkinson model including effect of induction
lw_ind_formula <- bf(log(Seeds + 1) ~ log(lambda / (1  + betaBUG * BugInd + betaCEN * CenInd + betaPAP * PapInd + betaSIN * SinInd + Resident.Density ^ ((1 - ResLog) * (1 - FocLog) * (nonalphaBUG * BugLog + nonalphaCEN * CenLog + nonalphaPAP * PapLog + nonalphaSIN * SinLog) + (1 - ResLog) * FocLog * (focalphaBUG * BugLog + focalphaCEN * CenLog + focalphaPAP * PapLog + focalphaSIN * SinLog) + (1 - FocLog) * ResLog * (resalphaBUG * BugLog + resalphaCEN * CenLog + resalphaPAP * PapLog + resalphaSIN * SinLog)))),
                     lambda ~ 1,
                     betaBUG ~ 1, betaCEN ~ 1, betaPAP ~ 1, betaSIN ~ 1,
                     nonalphaBUG ~ 1, nonalphaCEN ~ 1, nonalphaPAP ~ 1, nonalphaSIN ~ 1,
                     focalphaBUG ~ 1, focalphaCEN ~ 1, focalphaPAP ~ 1, focalphaSIN ~ 1,
                     resalphaBUG ~ 1, resalphaCEN ~ 1, resalphaPAP ~ 1, resalphaSIN ~ 1,
                     nl = TRUE)

# Law-Watkinson model including effect of induction
ricker_ind_formula <- bf(log(Seeds + 1) ~ log(lambda * exp(-(betaBUG * BugInd + betaCEN * CenInd + betaPAP * PapInd + betaSIN * SinInd)) * exp(- Resident.Density * ((1 - ResLog) * (1 - FocLog) * (nonalphaBUG * BugLog + nonalphaCEN * CenLog + nonalphaPAP * PapLog + nonalphaSIN * SinLog) + (1 - ResLog) * FocLog * (focalphaBUG * BugLog + focalphaCEN * CenLog + focalphaPAP * PapLog + focalphaSIN * SinLog) + (1 - FocLog) * ResLog * (resalphaBUG * BugLog + resalphaCEN * CenLog + resalphaPAP * PapLog + resalphaSIN * SinLog)))),
                         lambda ~ 1,
                         betaBUG ~ 1, betaCEN ~ 1, betaPAP ~ 1, betaSIN ~ 1,
                         nonalphaBUG ~ 1, nonalphaCEN ~ 1, nonalphaPAP ~ 1, nonalphaSIN ~ 1,
                         focalphaBUG ~ 1, focalphaCEN ~ 1, focalphaPAP ~ 1, focalphaSIN ~ 1,
                         resalphaBUG ~ 1, resalphaCEN ~ 1, resalphaPAP ~ 1, resalphaSIN ~ 1,
                         nl = TRUE)

# Law-Watkinson model including effect of induction
lv_ind_formula <- bf(log(Seeds + 1) ~ log(lambda - (betaBUG * BugInd + betaCEN * CenInd + betaPAP * PapInd + betaSIN * SinInd) - Resident.Density * ((1 - ResLog) * (1 - FocLog) * (nonalphaBUG * BugLog + nonalphaCEN * CenLog + nonalphaPAP * PapLog + nonalphaSIN * SinLog) + (1 - ResLog) * FocLog * (focalphaBUG * BugLog + focalphaCEN * CenLog + focalphaPAP * PapLog + focalphaSIN * SinLog) + (1 - FocLog) * ResLog * (resalphaBUG * BugLog + resalphaCEN * CenLog + resalphaPAP * PapLog + resalphaSIN * SinLog))),
                     lambda ~ 1,
                     betaBUG ~ 1, betaCEN ~ 1, betaPAP ~ 1, betaSIN ~ 1,
                     nonalphaBUG ~ 1, nonalphaCEN ~ 1, nonalphaPAP ~ 1, nonalphaSIN ~ 1,
                     focalphaBUG ~ 1, focalphaCEN ~ 1, focalphaPAP ~ 1, focalphaSIN ~ 1,
                     resalphaBUG ~ 1, resalphaCEN ~ 1, resalphaPAP ~ 1, resalphaSIN ~ 1,
                     nl = TRUE)

GetLambdaPriors <- function(lambda_data) {
  
  lambda_mean <- mean(lambda_data$Seeds)
  lambda_sigma <- sd(lambda_data$Seeds)
  lambda_min <- min(lambda_data$Seeds)
  lambda_max <- max(lambda_data$Seeds)
  
  lambda_prior_str <- paste0("normal(", lambda_mean, ", ", lambda_sigma, ")")
  lambda_sigma_str <- paste0("normal(", lambda_sigma, ", ", lambda_sigma, ")")
  
  lambda_priors <- c(set_prior(lambda_prior_str,
                               class = "Intercept",
                               lb = lambda_min,
                               ub = lambda_max),
                     set_prior(lambda_sigma_str,
                               class = "sigma",
                               lb = .001, ub = 2 * lambda_sigma))
  return(lambda_priors)
}

GetInductionPriors <- function(lambda_fit, lambda_data, lambda_ratio = 1) {
  # extracting the statistics of lambda
  lambda_mean <- summary(lambda_fit)$fixed$Estimate
  lambda_sigma <- summary(lambda_fit)$spec_pars$Estimate
  
  lambda_min <- min(lambda_data$Seeds)
  lambda_max <- max(lambda_data$Seeds)
  
  # setting up a prior dist for growth rates using the previous fits
  lambda_prior_str <- paste0("normal(", lambda_mean, ", ", lambda_sigma * lambda_ratio, ")")
  
  # setting priors for induction
  ind_priors <- c(set_prior(lambda_prior_str, nlpar = "lambda", lb = lambda_min, ub = lambda_max),
                  set_prior("normal(1, 5)", nlpar = "betaBUG", lb = .001, ub = 10),
                  set_prior("normal(1, 5)", nlpar = "betaCEN", lb = .001, ub = 10),
                  set_prior("normal(1, 5)", nlpar = "betaPAP", lb = .001, ub = 10),
                  set_prior("normal(1, 5)", nlpar = "betaSIN", lb = .001, ub = 10),
                  set_prior("normal(0.1, 5)", nlpar = "nonalphaBUG", lb = .001, ub = 2),
                  set_prior("normal(0.1, 5)", nlpar = "nonalphaCEN", lb = .001, ub = 2),
                  set_prior("normal(0.1, 5)", nlpar = "nonalphaPAP", lb = .001, ub = 2),
                  set_prior("normal(0.1, 5)", nlpar = "nonalphaSIN", lb = .001, ub = 2),
                  set_prior("normal(0.1, 5)", nlpar = "focalphaBUG", lb = .001, ub = 2),
                  set_prior("normal(0.1, 5)", nlpar = "focalphaCEN", lb = .001, ub = 2),
                  set_prior("normal(0.1, 5)", nlpar = "focalphaPAP", lb = .001, ub = 2),
                  set_prior("normal(0.1, 5)", nlpar = "focalphaSIN", lb = .001, ub = 2),
                  set_prior("normal(0.1, 5)", nlpar = "resalphaBUG", lb = .001, ub = 2),
                  set_prior("normal(0.1, 5)", nlpar = "resalphaCEN", lb = .001, ub = 2),
                  set_prior("normal(0.1, 5)", nlpar = "resalphaPAP", lb = .001, ub = 2),
                  set_prior("normal(0.1, 5)", nlpar = "resalphaSIN", lb = .001, ub = 2))
  return(ind_priors)
}

GetAlphaPriors <- function(ind_fit, lambda_fit, lambda_data, ind_ratio = 1, lambda_ratio = 1) {
  
  # extracting the statistics of lambda
  lambda_mean <- summary(lambda_fit)$fixed$Estimate
  lambda_sigma <- summary(lambda_fit)$spec_pars$Estimate
  
  lambda_min <- min(lambda_data$Seeds)
  lambda_max <- max(lambda_data$Seeds)
  
  # setting up a prior dist for growth rates using the previous fits
  lambda_prior_str <- paste0("normal(", lambda_mean, ", ", lambda_sigma * lambda_ratio, ")")
  
  ind_stats <- summary(ind_fit)$fixed[2:5, 1:2]
  
  bug_ind_prior_str <- paste0("normal(", ind_stats[1, 1], ", ", ind_stats[1, 2] * ind_ratio, ")")
  cen_ind_prior_str <- paste0("normal(", ind_stats[2, 1], ", ", ind_stats[2, 2] * ind_ratio, ")")
  pap_ind_prior_str <- paste0("normal(", ind_stats[3, 1], ", ", ind_stats[3, 2] * ind_ratio, ")")
  sin_ind_prior_str <- paste0("normal(", ind_stats[4, 1], ", ", ind_stats[4, 2] * ind_ratio, ")")
  
  # defining all priors for the BH model to fit alphas
  alpha_priors <- c(set_prior(lambda_prior_str, nlpar = "lambda", lb = lambda_min, ub = lambda_max),
                    set_prior(bug_ind_prior_str, nlpar = "betaBUG", lb = .001, ub = 10),
                    set_prior(cen_ind_prior_str, nlpar = "betaCEN", lb = .001, ub = 10),
                    set_prior(pap_ind_prior_str, nlpar = "betaPAP", lb = .001, ub = 10),
                    set_prior(sin_ind_prior_str, nlpar = "betaSIN", lb = .001, ub = 10),
                    set_prior("normal(0.05, 5)", nlpar = "nonalphaBUG", lb = .001, ub = 1),
                    set_prior("normal(0.05, 5)", nlpar = "nonalphaCEN", lb = .001, ub = 1),
                    set_prior("normal(0.05, 5)", nlpar = "nonalphaPAP", lb = .001, ub = 1),
                    set_prior("normal(0.05, 5)", nlpar = "nonalphaSIN", lb = .001, ub = 1),
                    set_prior("normal(0.05, 5)", nlpar = "focalphaBUG", lb = .001, ub = 1),
                    set_prior("normal(0.05, 5)", nlpar = "focalphaCEN", lb = .001, ub = 1),
                    set_prior("normal(0.05, 5)", nlpar = "focalphaPAP", lb = .001, ub = 1),
                    set_prior("normal(0.05, 5)", nlpar = "focalphaSIN", lb = .001, ub = 1),
                    set_prior("normal(0.05, 5)", nlpar = "resalphaBUG", lb = .001, ub = 1),
                    set_prior("normal(0.05, 5)", nlpar = "resalphaCEN", lb = .001, ub = 1),
                    set_prior("normal(0.05, 5)", nlpar = "resalphaPAP", lb = .001, ub = 1),
                    set_prior("normal(0.05, 5)", nlpar = "resalphaSIN", lb = .001, ub = 1))
  
  return(alpha_priors)
}
