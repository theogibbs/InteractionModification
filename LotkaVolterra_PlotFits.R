### PACKAGES ##################################################################################################

source("./Functions.R")

theme_set(theme_classic()) # theme ggplot

### Plotting script

all_draw_data <- read_csv2("simdata/LotkaVolterra_PosteriorDraws_PriorGradient.csv")

### INSERT WAIC / LOO PLOTTING

fit_eval_data <- all_draw_data %>%
  group_by(Focal, LambdaRatio) %>%
  summarise(LooEstimate = unique(LooEstimate),
            LooSE = unique(LooSE),
            WaicEstimate = unique(WaicEstimate),
            WaicSE = unique(WaicSE))

loo_data <- fit_eval_data[,1:4]
loo_data$Stat <- "LOO"
colnames(loo_data) <- c("Focal", "LambdaRatio", "Estimate", "SE", "Stat")

waic_data <- fit_eval_data[,c(1:2, 5:6)]
waic_data$Stat <- "WAIC"
colnames(waic_data) <- c("Focal", "LambdaRatio", "Estimate", "SE", "Stat")

plot_eval_data <- rbind(loo_data, waic_data) %>%
  mutate(Focal = paste("Focal:", Focal))

plFitEvals <- ggplot(plot_eval_data, aes(x = LambdaRatio, y = Estimate)) +
  geom_point(size = 2) +
  geom_linerange(alpha = 0.5, aes(min = Estimate - SE, max = Estimate + SE)) +
  labs(x = "Ratio of Variance in Prior") +
  facet_wrap(Stat ~ Focal, scales = "free", nrow = 2) +
  theme(text = element_text(size=15),
        axis.text.x = element_text(angle = 45, vjust = 0.5),
        strip.text.x = element_text(size = 15),
        strip.text.y = element_text(size = 10),
        legend.text=element_text(size = 15),
        strip.background = element_blank())
plFitEvals

jpeg("./figs/SIFigLotkaVolterraVarFitEvals.jpeg",
     width = 2500, height = 1500, res = 300)
plFitEvals
dev.off()

### 

plot_draws <- all_draw_data %>%
  select(-c("LooEstimate", "LooSE", "WaicEstimate", "WaicSE")) %>%
  melt(id.vars = c(".chain", ".iteration", ".draw", "Focal", "LambdaRatio", "BetaRatio")) %>%
  mutate(Focal = toupper(Focal))

lambda_draws <- plot_draws %>%
  filter(variable == "lambda") %>%
  mutate(Focal = paste("Focal:", Focal))

plLambdas <- ggplot(lambda_draws, aes(x = value, color = as.factor(LambdaRatio))) +
  geom_density(size = 1) + theme_classic() +
  facet_wrap(~Focal, nrow = 1, scales = "free") +
  labs(x = "Growth Rate", y = "", color = "Prior\nVariance\nRatio") +
  theme(text = element_text(size=15),
        axis.text.x = element_text(angle = 45, vjust = 0.5),
        strip.text.x = element_text(size = 15),
        strip.text.y = element_text(size = 10),
        legend.text=element_text(size = 15),
        strip.background = element_blank())
plLambdas

jpeg("./figs/SIFigLotkaVolterraLambdaRatios.jpeg",
     width = 3500, height = 1000, res = 300)
plLambdas
dev.off()

beta_draws <- plot_draws %>%
  filter(substr(variable, 1, 4) == "beta") %>%
  mutate(Resident = substr(variable, 5, 7)) %>%
  mutate(Focal = paste("Focal:", Focal)) %>%
  mutate(Resident = paste("Resident:", Resident))

plBetas <- ggplot(beta_draws, aes(x = value, color = as.factor(LambdaRatio))) +
  geom_density(size = 1) + theme_classic() +
  scale_y_continuous(position = "right") +
  facet_grid(Focal ~ Resident, switch = "y") +
  labs(x = expression("Induction Coefficient" ~(beta[ij])), y = "", color ="Prior\nVariance\nRatio") +
  theme(text = element_text(size=15),
        axis.text.x = element_text(angle = 45, vjust = 0.5),
        strip.text.x = element_text(size = 15),
        strip.text.y = element_text(size = 15),
        legend.text=element_text(size = 15),
        strip.background = element_blank())
plBetas

jpeg("./figs/SIFigLotkaVolterraBetaRatios.jpeg",
     width = 3500, height = 2500, res = 300)
plBetas
dev.off()

var_trend_draws <- plot_draws %>%
  mutate(Parameter = case_when(variable == "lambda" ~ "Lambda",
                               substr(variable, 1, 4) == "beta" ~ "Beta",
                               substr(variable, 1, 8) == "nonalpha" ~ "No Induction Alpha",
                               substr(variable, 1, 8) == "focalpha" ~ "Focal Alpha",
                               .default = "Resident Alpha")) %>%
  mutate(Parameter = factor(Parameter, levels = c("Lambda",
                                                  "Beta",
                                                  "No Induction Alpha",
                                                  "Focal Alpha",
                                                  "Resident Alpha"))) %>%
  group_by(Focal, Parameter, LambdaRatio, BetaRatio, variable) %>%
  summarise(MeanValue = mean(value)) %>%
  filter(!(substr(variable, 1, 8) == "resalpha" & Focal == substr(variable, 9, 11))) %>%
  mutate(Focal = paste("Focal:", Focal))

plVarTrends <- ggplot(var_trend_draws, aes(x = LambdaRatio, y = MeanValue, color = variable)) +
  geom_point(size = 2) + theme_classic() +
  scale_x_log10() +
  geom_line(alpha = 0.5) +
  facet_wrap(Focal~Parameter, scales = "free", nrow = 4) +
  labs(x = "Prior Variance Ratio", y = "") +
  theme(text = element_text(size=15),
        legend.position = "none",
        strip.text.x = element_text(size = 15),
        strip.text.y = element_text(size = 15),
        legend.text=element_text(size = 15),
        strip.background = element_blank())
plVarTrends

jpeg("./figs/SIFigLotkaVolterraVarianceTrends.jpeg",
     width = 4000, height = 3000, res = 300)
plVarTrends
dev.off()

alpha_diff_draws <- plot_draws %>%
  filter(substr(variable, 4, 8) == "alpha") %>%
  mutate(Resident = substr(variable, 9, 11)) %>%
  mutate(Induction = substr(variable, 1, 3)) %>%
  mutate(Induction = case_when(Induction == "non" ~ "None",
                               Induction == "foc" ~ "Focal",
                               Induction == "res" ~ "Resident")) %>%
  mutate(Induction = factor(Induction, levels = c("None", "Focal", "Resident"))) %>%
  filter(!((Induction == "Resident") & (Focal == Resident)))

alpha_non_draws <- alpha_diff_draws %>%
  filter(Induction == "None")

alpha_foc_draws <- alpha_diff_draws %>%
  filter(Induction == "Focal")

alpha_res_draws <- alpha_diff_draws %>%
  filter(Induction == "Resident")

alpha_intra_res_draws <- alpha_foc_draws %>%
  filter(Focal == Resident) %>%
  mutate(Induction = "Resident") %>%
  mutate(variable = substr(variable, 4, 11)) %>%
  mutate(variable = paste0("res", variable))

alpha_res_draws <- rbind(alpha_res_draws, alpha_intra_res_draws)

alpha_merge_draws <- merge(alpha_non_draws,
                           alpha_foc_draws,
                           by = c(".chain",
                                  ".iteration",
                                  ".draw",
                                  "LambdaRatio",
                                  "BetaRatio",
                                  "Focal",
                                  "Resident"))

alpha_merge_draws <- merge(alpha_merge_draws,
                           alpha_res_draws,
                           by = c(".chain",
                                  ".iteration",
                                  ".draw",
                                  "LambdaRatio",
                                  "BetaRatio",
                                  "Focal",
                                  "Resident"))

alpha_diff_draws <- alpha_merge_draws %>%
  mutate(DiffFoc = value.y / value.x) %>%
  mutate(DiffRes = value / value.x) %>%
  select(c("Resident", "Focal", "LambdaRatio", "DiffFoc", "DiffRes")) %>%
  melt(id.vars = c("Focal", "Resident", "LambdaRatio")) %>%
  mutate(variable = ifelse(variable == "DiffFoc", "Focal Induction", "Resident Induction")) %>%
  mutate(InteractionID = paste(Focal, "growing in", Resident)) %>%
  mutate(InteractionID = fct_rev(InteractionID))

plVarAlphaDiffs <- ggplot(alpha_diff_draws, aes(x = value, y = InteractionID,
                                                fill = after_stat(x < 1))) +
  stat_halfeye() +
  facet_grid(variable~LambdaRatio, labeller = label_bquote(cols = "Ratio" == .(LambdaRatio))) +
  geom_vline(xintercept = 1, alpha = 0.75, linetype = "dashed") +
  theme(text = element_text(size=15),
        legend.position = "none",
        legend.text=element_text(size = 15),
        strip.background = element_blank()) +
  labs(x = "Ratio of Induced to Not Induced Alpha", y = "Species Interaction") +
  scale_x_log10()
plVarAlphaDiffs

jpeg("./figs/SIFigLotkaVolterraVarAlphaDiffs.jpeg",
     width = 4000, height = 2750, res = 300)
plVarAlphaDiffs
dev.off()

comp_summary_df <- alpha_diff_draws %>%
  group_by(Resident, Focal, LambdaRatio, variable) %>%
  summarise(ProbMoreCompetitive = mean(value > 0))
comp_summary_df



car_cap_data <- data.frame()
for(cur_foc in Foc) {
  
  for(cur_ind in c("non", "foc")) {
    
    for(lambda_ratio in unique(all_draw_data$LambdaRatio)) {
      
      cur_alpha <- paste0(cur_ind, "alpha", toupper(cur_foc))
      
      print(cur_foc)
      print(cur_alpha)
      print(lambda_ratio)
      
      cur_draws <- all_draw_data %>%
        filter(Focal == cur_foc) %>%
        filter(LambdaRatio == lambda_ratio) %>%
        select("lambda", all_of(cur_alpha))
      
      print(names(cur_draws))
      
      cur_car_cap <- data.frame(Focal = cur_foc, Nbar = log(cur_draws[,1]) / cur_draws[,2])
      names(cur_car_cap) <- c("Focal", "Nbar")
      cur_car_cap$Induction <- cur_ind
      cur_car_cap$LambdaRatio <- lambda_ratio
      car_cap_data <- rbind(car_cap_data, cur_car_cap)
    }
    
  }
  
}

car_cap_plotting <- car_cap_data %>%
  mutate(Induction = ifelse(Induction == "foc", "Focal", "None")) %>%
  mutate(Induction = factor(Induction, levels = c("None", "Focal")))

plCarryingCapacity <- ggplot(car_cap_plotting, aes(x = Nbar, linetype = Induction, color = as.factor(LambdaRatio))) +
  geom_density() + scale_x_log10() +
  facet_wrap(~Focal, nrow = 1, scales = "free") +
  labs(x = "Carrying Capacity with Focal Induction", y = "") +
  theme(text = element_text(size=10),
        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5),
        strip.text.x = element_text(size = 10),
        strip.text.y = element_text(size = 10),
        legend.text=element_text(size = 10),
        legend.box = "horizontal") +
  labs(color = "Ratio")
plCarryingCapacity

jpeg("./figs/SIFigLotkaVolterraVarCarryingCapacities.jpeg",
     width = 2500, height = 750, res = 300)
plCarryingCapacity
dev.off()

igr_data <- data.frame()

for(cur_foc in Foc) {
  
  print(cur_foc)
  
  res_indices <- Res
  res_indices <- res_indices[!(res_indices == cur_foc)]
  
  for(cur_res in res_indices) {
    
    
    
    for(cur_ind_type in c("non", "foc", "res")) {
      
      if(cur_ind_type == "res") {
        induction_label <- "foc"
      } else {
        induction_label <- cur_ind_type
      }
      
      for(lambda_ratio in unique(all_draw_data$LambdaRatio)) {
        
        cur_car_cap <- car_cap_data %>%
          filter(Focal == cur_res) %>%
          filter(Induction == induction_label) %>%
          filter(LambdaRatio == lambda_ratio)
        
        cur_alpha <- paste0(cur_ind_type, "alpha", toupper(cur_res))
        
        cur_draws <- all_draw_data %>%
          filter(Focal == cur_foc) %>%
          filter(LambdaRatio == lambda_ratio) %>%
          select("lambda", all_of(cur_alpha))
        
        cur_igrs <- cur_draws[,1] * exp(-cur_car_cap$Nbar * cur_draws[,2])
        names(cur_igrs) <- "IGR"
        
        cur_igr_data <- data.frame(Focal = cur_foc,
                                   Resident = cur_res, 
                                   Induction = cur_ind_type,
                                   IGR = cur_igrs,
                                   LambdaRatio = lambda_ratio)
        
        igr_data <- rbind(igr_data, cur_igr_data)
        
      }
      
    }
  }
}

plot_igr_data <- igr_data %>%
  mutate(Resident = paste("Resident:", Resident)) %>%
  mutate(Focal = paste("Focal:", Focal)) %>%
  mutate(Induction = case_when(Induction == "non" ~ "None",
                               Induction == "foc" ~ "Focal",
                               Induction == "res" ~ "Resident")) %>%
  mutate(Induction = factor(Induction, levels = c("None", "Focal", "Resident")))

plot_igr_data <- plot_igr_data %>%
  filter(Induction != "Focal")

plIGR <- ggplot(plot_igr_data, aes(x = IGR, color = as.factor(LambdaRatio), linetype = Induction)) +
  geom_density() +
  facet_wrap(Focal ~ Resident, scales = 'free', ncol = 3) +
  scale_x_log10() +
  geom_vline(xintercept = 1, linetype = "dashed", color = 'gray') +
  labs(x = "Invasion Growth Rate", y = "", color = "Ratio") +
  theme(text = element_text(size=15),
        strip.text.x = element_text(size = 15),
        strip.text.y = element_text(size = 10),
        legend.text=element_text(size = 15),
        strip.background = element_blank())
plIGR

jpeg("./figs/SIFigLotkaVolterraVarIGR.jpeg",
     width = 3500, height = 2500, res = 300)
plIGR
dev.off()


### NOW PLOTTING RESULTS FOR A SPECIFIC FIT IE CHOOSING A PRIOR

draw_data <- all_draw_data %>%
  filter(LambdaRatio == 0.1) %>%
  select(-c(LambdaRatio, BetaRatio))

plot_draws <- draw_data %>%
  melt(id.vars = c(".chain", ".iteration", ".draw", "Focal")) %>%
  mutate(Focal = toupper(Focal))

mat_draws <- plot_draws %>%
  filter(substr(variable, 4, 8) == "alpha") %>%
  mutate(Resident = substr(variable, 9, 11)) %>%
  mutate(Induction = substr(variable, 1, 3)) %>%
  mutate(Induction = case_when(Induction == "non" ~ "No Induction",
                               Induction == "foc" ~ "Focal Induction",
                               Induction == "res" ~ "Resident Induction"))

mat_data <- mat_draws %>%
  group_by(Focal, Resident, Induction, variable) %>%
  summarise(Estimate = mean(value)) %>%
  mutate(Estimate = round(Estimate, 3)) %>%
  filter(!((Induction == "Resident Induction") & (Focal == Resident)))

res_duplicate_data <- mat_data %>%
  filter(Induction == "Focal Induction") %>%
  filter(Focal == Resident) %>%
  mutate(Induction = "Resident Induction")
mat_data <- rbind(mat_data, res_duplicate_data) %>%
  mutate(Induction = factor(Induction, levels = c("No Induction",
                                                  "Focal Induction",
                                                  "Resident Induction")))

plMatrices <- ggplot(mat_data, aes(x = Resident, y = Focal,
                                   fill = Estimate, label = Estimate)) +
  geom_tile() +
  geom_text() +
  scale_fill_gradient(high="blue", low="white") +
  facet_wrap(~Induction) +
  theme_classic() +
  labs(x = "Resident Species", y = "Focal Species") +
  scale_y_discrete(limits=rev) +
  theme(legend.position = "none",
        axis.line=element_blank(),
        axis.ticks=element_blank(),
        axis.text.y = element_text(angle = 90),
        text = element_text(size=15),
        strip.text.x = element_text(size = 15),
        strip.text.y = element_text(size = 10),
        legend.text=element_text(size = 15),
        strip.background = element_blank())
plMatrices

jpeg("./figs/SIFigLotkaVolterraMatrices.jpeg",
     width = 3500, height = 1500, res = 300)
plMatrices
dev.off()

fit_params <- plot_draws %>%
  group_by(Focal, variable) %>%
  summarise(Estimate = mean(value)) %>%
  mutate(LambdaLog = variable == "lambda") %>%
  mutate(BetaLog = substr(variable, 1, 4) == "beta") %>%
  mutate(AlphaLog = substr(variable, 4, 8) == "alpha") %>%
  mutate(Resident = case_when(LambdaLog ~ "None",
                              BetaLog ~ substr(variable, 5, 7),
                              AlphaLog ~ substr(variable, 9, 11))) %>%
  mutate(Induction = case_when(LambdaLog ~ "None",
                               BetaLog ~ ifelse(Focal == Resident, "Focal", "Resident"),
                               AlphaLog ~ case_when(substr(variable, 1, 3) == "non" ~ "None",
                                                    substr(variable, 1, 3) == "foc" ~ "Focal",
                                                    substr(variable, 1, 3) == "res" ~ "Resident"))) %>%
  filter(!((Induction == "Resident") & (Focal == Resident)))

pred_data <- crossing(Resident.Density = 0,
                      Focal = toupper(Foc),
                      Resident = toupper(Res),
                      Induction = c("None", "Focal", "Resident")) %>%
  filter(!((Induction == "Resident") & (Focal == Resident))) %>%
  mutate(Inducer = case_when(Induction == "None" ~ "None",
                             Induction == "Focal" ~ Focal,
                             Induction == "Resident" ~ Resident)) %>%
  mutate(Lambda = 0, Beta = 0, Alpha = 0)

for(i in 1:nrow(pred_data)) {
  
  print(paste(i, "out of", nrow(pred_data)))
  cur_data <- pred_data[i,]
  cur_foc <- cur_data$Focal
  cur_res <- cur_data$Resident
  cur_induction <- cur_data$Induction
  cur_inducer <- cur_data$Inducer
  
  cur_lambda <- fit_params %>%
    filter(LambdaLog) %>%
    filter(Focal == cur_foc)
  cur_lambda <- cur_lambda$Estimate
  
  if(cur_induction == "None") {
    cur_beta <- 0
  } else {
    cur_beta <- fit_params %>%
      filter(BetaLog) %>%
      filter(Focal == cur_foc) %>%
      filter(Resident == cur_inducer)
    cur_beta <- cur_beta$Estimate
  }
  
  cur_alpha <- fit_params %>%
    filter(AlphaLog) %>%
    filter(Focal == cur_foc) %>%
    filter(Resident == cur_res) %>%
    filter(Induction == cur_induction)
  cur_alpha <- cur_alpha$Estimate
  
  pred_data$Lambda[i] <- cur_lambda
  pred_data$Beta[i] <- cur_beta
  pred_data$Alpha[i] <- cur_alpha
  
}

full_pred_data <- data.frame()

proc_fit_data <- proc_data %>%
  mutate(Focal = toupper(Focal),
         Resident = toupper(Resident))

for(i in 1:nrow(pred_data)) {
  
  cur_data <- pred_data[i,]
  cur_foc <- cur_data$Focal
  cur_res <- cur_data$Resident
  cur_induction <- cur_data$Induction
  
  res_dens <- proc_fit_data %>%
    filter(Focal == cur_foc) %>%
    filter(Resident == cur_res) %>%
    filter(Induction == cur_induction)
  
  max_res <- max(res_dens$Resident.Density)
  res_dens <- seq(0, max_res, length.out = 100)
  
  cur_pred_data <- cur_data
  cur_pred_data <- do.call("rbind", replicate(length(res_dens), cur_data, simplify = FALSE))
  cur_pred_data$Resident.Density <- res_dens
  
  full_pred_data <- rbind(full_pred_data, cur_pred_data)
  
}


full_pred_data <- full_pred_data %>%
  mutate(PredSeeds = (Lambda * exp( - (Beta + Alpha * Resident.Density))))

no_res_proc_data <- proc_fit_data %>%
  filter(Resident == "NONE")

no_ind_proc_data <- no_res_proc_data %>%
  filter(Inducer == "None")

new_res <- rep(c("BUG", "CEN", "PAP", "SIN"), each = nrow(no_ind_proc_data))

no_ind_proc_data <- do.call("rbind", replicate(4, no_ind_proc_data, simplify = FALSE))
no_ind_proc_data$Resident <- new_res

foc_ind_proc_fit_data <- no_res_proc_data %>%
  filter(Induction == "Focal")

new_res <- rep(c("BUG", "CEN", "PAP", "SIN"), each = nrow(foc_ind_proc_fit_data))

foc_ind_proc_fit_data <- do.call("rbind", replicate(4, foc_ind_proc_fit_data, simplify = FALSE))
foc_ind_proc_fit_data$Resident <- new_res

ind_proc_fit_data <- no_res_proc_data %>%
  filter(Induction == "Resident") %>%
  mutate(Resident = toupper(Inducer))

proc_fit_data <- rbind(proc_fit_data %>%
                         filter(Resident != "NONE"),
                       no_ind_proc_data,
                       foc_ind_proc_fit_data,
                       ind_proc_fit_data)

proc_fit_data <- MakePlotData(proc_fit_data) %>%
  mutate(Induction = factor(Induction, levels = c("None", "Focal", "Resident")))
full_pred_data <- MakePlotData(full_pred_data)

# plotting predictions
plFits <- ggplot(proc_fit_data,
                 aes(x = Resident.Density, y = Seeds, color = Induction)) +
  geom_point(alpha = 0.5) +
  theme_classic() +
  geom_line(data = full_pred_data, aes(x = Resident.Density, y = PredSeeds)) +
  scale_y_log10() +
  facet_wrap(Focal ~ Resident, scales = 'free') +
  theme(legend.position = "top",
        text = element_text(size=15),
        strip.text.x = element_text(size = 10),
        strip.text.y = element_text(size = 10),
        legend.text=element_text(size = 15),
        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5),
        strip.background = element_blank()) +
  xlab("Resident Density")
plFits

jpeg("./figs/SIFigLotkaVolterraFits.jpeg",
     width = 3000, height = 2500, res = 300)
plFits
dev.off()

# computing niche and fitness differences with and without resident induced plasticity

nfd_data <- data.frame()

for(cur_foc in Foc) {
  
  print(cur_foc)
  
  res_indices <- Res
  res_indices <- res_indices[!(res_indices == cur_foc)]
  
  
  for(cur_res in res_indices) {
    
    for(cur_ind_type in c("non", "res")) {
      
      
      if(cur_ind_type == "res") {
        cur_intra_label <- "foc"
      } else {
        cur_intra_label <- cur_ind_type
      }
      
      cur_alpha_ff <- paste0(cur_intra_label, "alpha", toupper(cur_foc))
      cur_alpha_rr <- paste0(cur_intra_label, "alpha", toupper(cur_res))
      
      cur_alpha_fr <- paste0(cur_ind_type, "alpha", toupper(cur_res))
      cur_alpha_rf <- paste0(cur_ind_type, "alpha", toupper(cur_foc))
      
      cur_foc_draws <- draw_data %>%
        filter(Focal == cur_foc) %>%
        select("Focal", "lambda", all_of(cur_alpha_ff), all_of(cur_alpha_fr))
      
      cur_res_draws <- draw_data %>%
        filter(Focal == cur_res) %>%
        select("Focal", "lambda", all_of(cur_alpha_rr), all_of(cur_alpha_rf))
      
      cur_lambda_f <- cur_foc_draws$lambda
      cur_lambda_r <- cur_res_draws$lambda
      
      cur_alpha_ff <- cur_foc_draws[,3]
      cur_alpha_rr <- cur_res_draws[,3]
      cur_alpha_fr <- cur_foc_draws[,4]
      cur_alpha_rf <- cur_res_draws[,4]
      
      cur_niche_diff <- sqrt(cur_alpha_rf * cur_alpha_fr / cur_alpha_ff / cur_alpha_rr)
      names(cur_niche_diff) <- "NicheDiff"
      
      cur_comp_ab_f <- log(cur_lambda_f) / sqrt(cur_alpha_ff * cur_alpha_fr)
      cur_comp_ab_r <- log(cur_lambda_r) / sqrt(cur_alpha_rr * cur_alpha_rf)
      
      cur_fit_diff <- cur_comp_ab_f / cur_comp_ab_r
      names(cur_fit_diff) <- "FitDiff"
      
      cur_nfd_data <- data.frame(Focal = cur_foc,
                                 Resident = cur_res, 
                                 Induction = cur_ind_type,
                                 NicheDiff = cur_niche_diff,
                                 FitDiff = cur_fit_diff)
      
      nfd_data <- rbind(nfd_data, cur_nfd_data)
      
    }
  }
}

non_nfd_data <- nfd_data %>%
  filter(Induction == "non") %>%
  mutate(SpInt = paste(Focal, "growing in", Resident)) %>%
  select("SpInt", "NicheDiff", "FitDiff")

res_nfd_data <- nfd_data %>%
  filter(Induction == "res") %>%
  mutate(SpInt = paste(Focal, "growing in", Resident)) %>%
  select("SpInt", "NicheDiff", "FitDiff")

plot_nfd_data <- non_nfd_data
plot_nfd_data$ResNicheDiff <- res_nfd_data$NicheDiff
plot_nfd_data$ResFitDiff <- res_nfd_data$FitDiff

#ggplot(plot_nfd_data, aes(x = NicheDiff, y = ResNicheDiff, color = SpInt)) +
#  geom_point(alpha = 0.01) + scale_x_log10() + scale_y_log10() +
#  geom_abline(slope = 1, intercept = 0)


#ggplot(plot_nfd_data, aes(x = FitDiff, y = ResFitDiff, color = SpInt)) +
#  geom_point(alpha = 0.01) + scale_x_log10() + scale_y_log10() +
#  geom_abline(slope = 1, intercept = 0)

plot_nfd_data <- plot_nfd_data %>%
  mutate(DeltaNicheDiff = ResNicheDiff / NicheDiff) %>%
  mutate(DeltaFitDiff = ResFitDiff / FitDiff) %>%
  select("SpInt", DeltaNicheDiff, DeltaFitDiff) %>%
  melt(id.vars = c("SpInt")) %>%
  mutate(SpInt = fct_rev(SpInt)) %>%
  mutate(variable = ifelse(variable == "DeltaNicheDiff", "Niche Difference", "Fitness Difference")) %>%
  mutate(variable = factor(variable, levels = c("Niche Difference", "Fitness Difference")))

plNFDiffs <- ggplot(plot_nfd_data, aes(x = value, y = SpInt, fill = after_stat(x < 1))) +
  stat_halfeye() +
  facet_grid(~variable) +
  geom_vline(xintercept = 1, alpha = 0.75, linetype = "dashed") +
  theme(text = element_text(size=15),
        legend.position = "none",
        legend.text=element_text(size = 15),
        strip.background = element_blank()) +
  labs(x = "Ratio of Resident Induced and Not Induced Coexistence Metric",
       y = "Species Interaction") + scale_x_log10()
plNFDiffs

jpeg("./figs/SIFigLotkaVolterraNFDiffs.jpeg",
     width = 3500, height = 2000, res = 300)
plNFDiffs
dev.off()

### inducer patterns


ind_data <- data.frame()

for(cur_foc in Foc) {
  
  print(cur_foc)
  
  res_indices <- Res
  res_indices <- res_indices[!(res_indices == cur_foc)]
  
  cur_non_alphas <- paste0("nonalpha", toupper(res_indices))
  cur_res_alphas <- paste0("resalpha", toupper(res_indices))
  
  cur_draws <- draw_data %>%
    filter(Focal == cur_foc) %>%
    select(all_of(cur_res_alphas), all_of(cur_non_alphas))
  
  cur_ratios <- cur_draws[,1:3] / cur_draws[,4:6]
  
  cur_susc <- apply(cur_ratios, 1, prod)
  
  cur_draws <- draw_data %>%
    filter(Focal != cur_foc)
  cur_foc_ids <- cur_draws$Focal
  
  cur_draws <- cur_draws[,grepl(toupper(cur_foc), colnames(draw_data))]
  cur_draws <- cur_draws[,!grepl("beta", colnames(cur_draws))]
  cur_draws <- cur_draws[,!grepl("foc", colnames(cur_draws))]
  
  cur_ratios <- cur_draws[,2] / cur_draws[,1]
  names(cur_ratios) <- "Ratio"
  
  cur_ratios <- data.frame(Focal = cur_foc_ids, Ratio = cur_ratios)
  cur_ratios <- cur_ratios %>%
    pivot_wider(names_from = Focal, values_from = Ratio, values_fn = list) %>%
    unnest(cols = everything())
  
  cur_strs <- apply(cur_ratios, 1, prod)
  
  cur_ind_data <- data.frame(Focal = cur_foc,
                             Susc = cur_susc,
                             Strength = cur_strs)
  
  ind_data <- rbind(ind_data, cur_ind_data)
}

melt_ind_data <- ind_data %>%
  melt(id.vars = c("Focal")) %>%
  mutate(variable = ifelse(variable == "Susc", "(A) Induction Susceptibility", "(B) Induction Strength"))

plIndPatterns <- ggplot(melt_ind_data, aes(x = Focal, y = value)) +
  stat_halfeye() + facet_wrap(~variable, scales = "free") + scale_y_log10() +
  labs(x = "Focal Species", y = " ") +
  theme(text = element_text(size=15),
        legend.position = "none",
        legend.text=element_text(size = 15),
        strip.background = element_blank())
plIndPatterns

plIndSuscStr <- ggplot(ind_data, aes(x = Susc, y = Strength)) +
  geom_point(alpha = 0.05, aes(color = Focal)) + scale_x_log10() + scale_y_log10() +
  geom_smooth(formula = y ~ x) +
  guides(color = guide_legend(override.aes = list(alpha = 1) ) ) +
  theme(text = element_text(size=15),
        strip.text.x = element_text(size = 15),
        strip.text.y = element_text(size = 10),
        legend.text=element_text(size = 15),
        strip.background = element_blank()) +
  ggtitle("(C)") +
  labs(x = "Induction Susceptibility", y = "Induction Strength")
plIndSuscStr


jpeg("./figs/SIFigLotkaVolterraIndPatterns.jpeg",
     width = 4500, height = 1500, res = 300)
grid.arrange(plIndPatterns, plIndSuscStr, nrow = 1)
dev.off()

