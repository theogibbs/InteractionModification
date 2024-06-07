### PACKAGES ##################################################################################################

source("./Functions.R")

theme_set(theme_classic()) # theme ggplot

### Plotting script

all_draw_data <- read_csv2("simdata/BevertonHolt_PosteriorDraws_BetaGradient.csv")

## recording seed germination and survival probabilities

germ_probs <- c(0.301, 0.46, 0.034, 0.452)
names(germ_probs) <- c("Bug", "Cen", "Pap", "Sin")

surv_probs <- c(0.2306, 0.4128, 0.1660, 0.7741)
names(surv_probs) <- c("Bug", "Cen", "Pap", "Sin")

### INSERT WAIC / LOO PLOTTING

fit_eval_data <- all_draw_data %>%
  group_by(Focal, BetaRatio) %>%
  summarise(LooEstimate = unique(LooEstimate),
            LooSE = unique(LooSE),
            WaicEstimate = unique(WaicEstimate),
            WaicSE = unique(WaicSE))

loo_data <- fit_eval_data[,1:4]
loo_data$Stat <- "LOO"
colnames(loo_data) <- c("Focal", "BetaRatio", "Estimate", "SE", "Stat")

waic_data <- fit_eval_data[,c(1:2, 5:6)]
waic_data$Stat <- "WAIC"
colnames(waic_data) <- c("Focal", "BetaRatio", "Estimate", "SE", "Stat")

plot_eval_data <- rbind(loo_data, waic_data) %>%
  mutate(Focal = paste("Focal:", Focal))

plFitEvals <- ggplot(plot_eval_data, aes(x = BetaRatio, y = Estimate)) +
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

jpeg("./figs/SIFigBetaVarFitEvals.jpeg",
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

plLambdas <- ggplot(lambda_draws, aes(x = value, color = as.factor(BetaRatio))) +
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

jpeg("./figs/SIFigBetaVarLambdaRatios.jpeg",
     width = 3500, height = 1000, res = 300)
plLambdas
dev.off()

beta_draws <- plot_draws %>%
  filter(substr(variable, 1, 4) == "beta") %>%
  mutate(Resident = substr(variable, 5, 7)) %>%
  mutate(Focal = paste("Focal:", Focal)) %>%
  mutate(Resident = paste("Resident:", Resident))

plBetas <- ggplot(beta_draws, aes(x = value, color = as.factor(BetaRatio))) +
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

jpeg("./figs/SIFigBetaVarBetaRatios.jpeg",
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

plVarTrends <- ggplot(var_trend_draws, aes(x = BetaRatio, y = MeanValue, color = variable)) +
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

jpeg("./figs/SIFigBetaVarTrends.jpeg",
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
  mutate(DiffFoc = value.y - value.x) %>%
  mutate(DiffRes = value - value.x) %>%
  select(c("Resident", "Focal", "BetaRatio", "DiffFoc", "DiffRes")) %>%
  melt(id.vars = c("Focal", "Resident", "BetaRatio")) %>%
  mutate(variable = ifelse(variable == "DiffFoc", "Focal Induction", "Resident Induction")) %>%
  mutate(InteractionID = paste(Focal, "growing in", Resident)) %>%
  mutate(InteractionID = fct_rev(InteractionID))

plVarAlphaDiffs <- ggplot(alpha_diff_draws, aes(x = value, y = InteractionID,
                                                fill = after_stat(x < 0))) +
  stat_halfeye() +
  facet_grid(variable~BetaRatio, labeller = label_bquote(cols = "Ratio" == .(BetaRatio))) +
  geom_vline(xintercept = 0, alpha = 0.75, linetype = "dashed") +
  theme(text = element_text(size=15),
        legend.position = "none",
        legend.text=element_text(size = 15),
        strip.background = element_blank()) +
  labs(x = "Difference in Induced and Not Induced Alpha", y = "Species Interaction")
plVarAlphaDiffs

jpeg("./figs/SIFigBetaVarAlphaDiffs.jpeg",
     width = 4000, height = 2750, res = 300)
plVarAlphaDiffs
dev.off()

comp_summary_df <- alpha_diff_draws %>%
  group_by(Resident, Focal, BetaRatio, variable) %>%
  summarise(ProbMoreCompetitive = mean(value > 0))
comp_summary_df



car_cap_data <- data.frame()
for(cur_foc in Foc) {
  
  for(cur_ind in c("non", "foc")) {
    
    for(beta_ratio in unique(all_draw_data$BetaRatio)) {
      
      cur_alpha <- paste0(cur_ind, "alpha", toupper(cur_foc))
      
      print(cur_foc)
      print(cur_alpha)
      print(beta_ratio)
      
      cur_draws <- all_draw_data %>%
        filter(Focal == cur_foc) %>%
        filter(BetaRatio == beta_ratio) %>%
        select("lambda", all_of(cur_alpha))
      
      print(names(cur_draws))
      
      cur_germ <- germ_probs[names(germ_probs) == cur_foc]
      cur_surv <- surv_probs[names(surv_probs) == cur_foc]
      cur_eta <- cur_germ * cur_draws[,1] / (1 - (1 - cur_germ) * cur_surv)
      cur_car_cap_val <- (cur_eta - 1) / cur_germ / cur_draws[,2]
      
      cur_car_cap <- data.frame(Focal = cur_foc, Nbar = cur_car_cap_val)
      names(cur_car_cap) <- c("Focal", "Nbar")
      cur_car_cap$Induction <- cur_ind
      cur_car_cap$BetaRatio <- beta_ratio
      car_cap_data <- rbind(car_cap_data, cur_car_cap)
    }
    
  }
  
}

car_cap_plotting <- car_cap_data %>%
  mutate(Induction = ifelse(Induction == "foc", "Focal", "None")) %>%
  mutate(Induction = factor(Induction, levels = c("None", "Focal")))

plCarryingCapacity <- ggplot(car_cap_plotting, aes(x = Nbar, linetype = Induction, color = as.factor(BetaRatio))) +
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

jpeg("./figs/SIFigBetaVarCarryingCapacities.jpeg",
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
      
      for(beta_ratio in unique(all_draw_data$BetaRatio)) {
        
        cur_car_cap <- car_cap_data %>%
          filter(Focal == cur_res) %>%
          filter(Induction == induction_label) %>%
          filter(BetaRatio == beta_ratio)
        
        cur_alpha <- paste0(cur_ind_type, "alpha", toupper(cur_res))
        cur_foc_germ <- germ_probs[names(germ_probs) == cur_foc]
        cur_foc_surv <- surv_probs[names(surv_probs) == cur_foc]
        cur_res_germ <- germ_probs[names(germ_probs) == cur_res]
        
        cur_draws <- all_draw_data %>%
          filter(Focal == cur_foc) %>%
          filter(BetaRatio == beta_ratio) %>%
          select("lambda", all_of(cur_alpha))
        
        cur_igrs <- cur_foc_germ * cur_draws[,1] / (1 + cur_draws[,2] * cur_res_germ * cur_car_cap$Nbar) + (1 - cur_foc_germ) * cur_foc_surv
        names(cur_igrs) <- "IGR"
        
        cur_igr_data <- data.frame(Focal = cur_foc,
                                   Resident = cur_res, 
                                   Induction = cur_ind_type,
                                   IGR = cur_igrs,
                                   BetaRatio = beta_ratio)
        
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

plIGR <- ggplot(plot_igr_data, aes(x = IGR, color = as.factor(BetaRatio), linetype = Induction)) +
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

jpeg("./figs/SIFigBetaVarIGR.jpeg",
     width = 3500, height = 2500, res = 300)
plIGR
dev.off()



