### PACKAGES ##################################################################################################

source("./Functions.R")

theme_set(theme_classic()) # theme ggplot

### Plotting script

all_draw_data <- read_csv2("simdata/BevertonHolt_PosteriorDraws_PriorGradient.csv")


data <- read.csv('Plasticity.Data.Focals.v16.csv',
                 header=T,
                 na.strings="",
                 stringsAsFactors = F)  # based on data from Raw.Data.All v15     Missing data is set to NA

# Remove scientific notation
options(scipen=999)

### DATA CLEANING  ##################################################################################################

# Remove abandoned treatments and missing data
data<- data[!is.na(data$Treatment), ]    # remove abandoned treatments

#### wrong species in dry weight bag!!!  Removing data!
data<- data[!data$Combo == '8-A:11',  ] #  8-A:11   0 (Pap)   in Bug-8
data<- data[!data$Combo == '11-A:2',  ] # 11-A:2    0 (Sil)  in Bug-5

# Error in plant ID 
data<- data[!data$Combo == '4-B:8',  ] # 4-B:8   # Weird note about correct ID. Analysis suggest it is a large outlier providing evidence it was not correct

# very weird data  --> ALL AFFECT PAP_vs_BUG!!!!
# 7-B:11  Pap(Pap) in Bug-3
data<- data[!data$Combo == '7-B:11',  ]
# 30-A:13 Pap(Pap) in Bug-6
data<- data[!data$Combo == '30-A:13',  ]
# 27-A:1  None(Pap) in Bug-9
data<- data[!data$Combo == '27-A:1',  ]
# 19-A:13 None(Pap) in Bug-7
data<- data[!data$Combo == '19-A:13',  ]
# 13-A:15 Bug(Pap) in Bug-4
data<- data[!data$Combo == '13-A:15',  ]

#  other PROBLEMS TO DEAL WITH
#  - TOO FEW INDUCERS   (REMOVE? CHANGE TREATMENT?)
# 1-B:7    Sil(Sil) in Sin-1
# 9-B:5   Sil (Sil) in Sin-2
# 21-A:12   Sil (Sil)  Sil-6 
# 25-B:8    Sil (Sil) in Cen-5

### DEFINING NEW VARIABLES  ##################################################################################################

# Inducer.Type  ,  Resident Density to m^2 + remove missing data, Resident Density Vectors, 
#  CONVERT  Pods -> Seed Number (Quite variable) , **HIGH RANGE in Values for Poppy and Silene. See   SeedPodSeedcount_2.xlsx **

# Add in new Column  Inducer.Type  (None, Focal, Resident)
data<- cbind(data, 'Inducer.Type'=NA)
data$Inducer.Type[which(data$Focal == data$Inducer)] <- 'Focal'
data$Inducer.Type[which(data$Inducer == 'None')] <- 'None'    
data$Inducer.Type[ is.na(data$Inducer.Type)] <- 'Resident'

# Convert resident density to m^2
data$Resident.Density <- data$Measured.Resident.Density.15cmRadius.June.24 / (pi*0.15^2)

# remove missing data for resident density and pods
data<- data[!is.na(data$Resident.Density), ]  # missing resident density   - eliminates 4 data points
data<- data[!is.na(data$Pods), ] # missing pod number  - lose 1
rownames(data)<- 1:dim(data)[1]  # reset row names    

##  Creating density vectors -- Used in modeling
data<- cbind(data, 'dens.Bug'=0,'dens.Cen'=0,'dens.Pap'=0,'dens.Sil'=0,'dens.Sin'=0 )

data$dens.Bug[which(data$Resident=='Bug')]<- data$Resident.Density[which(data$Resident=='Bug')]
data$dens.Cen[which(data$Resident=='Cen')]<- data$Resident.Density[which(data$Resident=='Cen')]       
data$dens.Pap[which(data$Resident=='Pap')]<- data$Resident.Density[which(data$Resident=='Pap')]
data$dens.Sil[which(data$Resident=='Sil')]<- data$Resident.Density[which(data$Resident=='Sil')]
data$dens.Sin[which(data$Resident=='Sin')]<- data$Resident.Density[which(data$Resident=='Sin')]

# Convert Pods to viable seed number
# 
# average seeds / pods  - Counted 50 pods per species by myriam 2013  SeedPodSeedcount_2.xlsx
# seed viability provided from  Seed Info.xlsx   
data<- cbind(data, 'Seeds'<- NA)

data$Seeds[data$Focal=='Bug']<- data$Pods[data$Focal=='Bug'] * 4  * 0.97513 # Bug 4 seeds/pod
data$Seeds[data$Focal=='Cen']<- data$Pods[data$Focal=='Cen'] * 18.5 * 0.7384# Bug 18.5 seeds/pod
data$Seeds[data$Focal=='Pap']<- data$Pods[data$Focal=='Pap'] * 591 * 0.9567  # Pap 591.0 
data$Seeds[data$Focal=='Sil']<- data$Pods[data$Focal=='Sil'] * 205 * 0.9491  # Sil 205.0 
data$Seeds[data$Focal=='Sin']<- data$Pods[data$Focal=='Sin'] * 8.7 * 0.982095  # Sil 8.7 seeds/pod

## recording seed germination and survival probabilities

germ_probs <- c(0.301, 0.46, 0.034, 0.452)
names(germ_probs) <- c("Bug", "Cen", "Pap", "Sin")

surv_probs <- c(0.2306, 0.4128, 0.1660, 0.7741)
names(surv_probs) <- c("Bug", "Cen", "Pap", "Sin")

### PRELIMINARY PLOTS OF RESIDENT DENSITIES ########################################################################################

# Log Seeds (fecundity) data
data <- cbind(data, 'log.Seeds'= log(data$Seeds+1)) # log Seeds production +1

Foc<- c('Bug','Cen','Pap','Sin')  # focal species
Ind<- c('None','Bug','Cen','Pap','Sin') # inducers
Res<- c('Bug','Cen','Pap','Sin') # resident species

shapes <- c(10,21,22,23,25)
names(shapes) <- c("None",'Bug','Cen','Pap','Sin')

#  colour  Same deal
colours <- c('black', 'purple','green','red','Orange')
names(colours) <- c("None",'Bug','Cen','Pap','Sin')

# Resident Densities using actual counts not densities
spp<-c('Bug','Cen', 'Pap', 'Sil','Sin')
for (i in 1:length(spp)) {
  Res<- spp[i]
  d.temp<- data[data$Resident==spp[i], ]
  #pdf(paste("Local ", Res," Density.pdf",sep=''),width=6,height=6) 
  plot(as.factor(d.temp$Resident.code),
       d.temp$Measured.Resident.Density.15cmRadius.June.24,
       ylab='LOCAL Resident Density', cex.axis=0.6)
  
} # for resident density graphs

### EXCLUDING SPECIES AND SETTINGS

# REMOVE SILENE?!? adjust this toggle:
Silene.include<- FALSE # WRITE  TRUE to INCLUDE it - Write FALSE to exclude it  ONLY WORKS for some of the analyses

## PLOTS as LOG?  TRUE or FALSE!!
log.plots<- TRUE

# if you are removing Silene - rename species factors and colours. 
# also remove it from the data set

if(Silene.include){
  Foc<- c('Bug','Cen','Pap','Sil','Sin')  # focal species
  Ind<- c('None','Bug','Cen','Pap','Sil','Sin') # inducers
  Res<- c('Bug','Cen','Pap','Sil','Sin') # resident species
  
  # SHAPES of Points:  each species gets it own consistent point shape. 
  shapes <- c(10,21,22,23,24,25)
  names(shapes) <- c("None",'Bug','Cen','Pap','Sil','Sin')
  
  # colour  Same deal
  colours <- c('black', 'purple','green','red','blue','Orange')
  names(colours) <- c("None",'Bug','Cen','Pap','Sil','Sin')
  
} else {  # exclude SILENE
  Foc<- c('Bug','Cen','Pap','Sin')  # focal species
  Ind<- c('None','Bug','Cen','Pap','Sin') # inducers
  Res<- c('Bug','Cen','Pap','Sin') # resident species
  
  shapes <- c(10,21,22,23,25)
  names(shapes) <- c("None",'Bug','Cen','Pap','Sin')
  
  #  colour  Same deal
  colours <- c('black', 'purple','green','red','Orange')
  names(colours) <- c("None",'Bug','Cen','Pap','Sin')
  
  data<- subset(data, Focal != 'Sil' & Inducer != 'Sil' & Resident != 'Sil')
  
}

# subsetting data to what we need for fitting

proc_data <- data %>%
  dplyr::select(c("Resident.Density",
           "Seeds",
           "Inducer",
           "Focal",
           "Resident",
           "Inducer.Type"))
names(proc_data)[names(proc_data) == "Inducer.Type"] <- "Induction"

proc_data <- proc_data %>%
  mutate(FocLog = Induction == "Focal") %>%
  mutate(ResLog = Induction == "Resident") %>%
  mutate(BugLog = Resident == "Bug") %>%
  mutate(CenLog = Resident == "Cen") %>%
  mutate(PapLog = Resident == "Pap") %>%
  mutate(SinLog = Resident == "Sin") %>%
  mutate(BugInd = Inducer == "Bug") %>%
  mutate(CenInd = Inducer == "Cen") %>%
  mutate(PapInd = Inducer == "Pap") %>%
  mutate(SinInd = Inducer == "Sin")

### INSERT WAIC / LOO PLOTTING

fit_eval_data <- all_draw_data %>%
  dplyr::group_by(Focal, LambdaRatio) %>%
  dplyr::summarise(LooEstimate = unique(LooEstimate),
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

jpeg("./figs/SIFigVarFitEvals.jpeg",
     width = 2500, height = 1500, res = 300)
plFitEvals
dev.off()

### 

plot_draws <- all_draw_data %>%
  dplyr::select(-c("LooEstimate", "LooSE", "WaicEstimate", "WaicSE")) %>%
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

jpeg("./figs/SIFigLambdaRatios.jpeg",
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

jpeg("./figs/SIFigBetaRatios.jpeg",
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
  dplyr::summarise(MeanValue = mean(value)) %>%
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

jpeg("./figs/SIFigVarianceTrends.jpeg",
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

alpha_draws <- alpha_diff_draws %>%
  mutate(Focal = paste("Focal:", Focal)) %>%
  mutate(Resident = paste("Resident:", Resident))

plAlphas <- ggplot(alpha_draws, aes(x = value, color = as.factor(LambdaRatio), linetype = Induction)) +
  geom_density(size = 1) + theme_classic() +
  scale_y_continuous() +
  scale_x_log10() +
  facet_wrap(Focal ~ Resident, scales = "free") +
  labs(x = expression("Competition Coefficient" ~(alpha[ij]~"|"[j])), y = "", color ="Prior\nVariance\nRatio") +
  theme(text = element_text(size=15),
        axis.text.x = element_text(angle = 45, vjust = 0.5),
        strip.text.x = element_text(size = 15),
        strip.text.y = element_text(size = 15),
        legend.text=element_text(size = 15),
        strip.background = element_blank()) +
  scale_linetype_manual(values = c(1, 3, 5))
plAlphas

jpeg("./figs/SIFigAlphaRatios.jpeg",
     width = 4500, height = 3500, res = 300)
plAlphas
dev.off()



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
  dplyr::select(c("Resident", "Focal", "LambdaRatio", "DiffFoc", "DiffRes")) %>%
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

jpeg("./figs/SIFigVarAlphaDiffs.jpeg",
     width = 4000, height = 2750, res = 300)
plVarAlphaDiffs
dev.off()

comp_summary_df <- alpha_diff_draws %>%
  dplyr::group_by(Resident, Focal, LambdaRatio, variable) %>%
  dplyr::summarise(ProbMoreCompetitive = mean(value > 1))
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
        dplyr::select("lambda", all_of(cur_alpha))
      
      print(names(cur_draws))
      
      cur_germ <- germ_probs[names(germ_probs) == cur_foc]
      cur_surv <- surv_probs[names(surv_probs) == cur_foc]
      cur_eta <- cur_germ * cur_draws[,1] / (1 - (1 - cur_germ) * cur_surv)
      cur_car_cap_val <- (cur_eta - 1) / cur_germ / cur_draws[,2]
      
      cur_car_cap <- data.frame(Focal = cur_foc, Nbar = cur_car_cap_val)
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
  labs(x = "Carrying Capacity", y = "") +
  theme(text = element_text(size=10),
        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5),
        strip.text.x = element_text(size = 10),
        strip.text.y = element_text(size = 10),
        legend.text=element_text(size = 10),
        legend.box = "horizontal") +
  labs(color = "Ratio")
plCarryingCapacity

jpeg("./figs/SIFigVarCarryingCapacities.jpeg",
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
        cur_foc_germ <- germ_probs[names(germ_probs) == cur_foc]
        cur_foc_surv <- surv_probs[names(surv_probs) == cur_foc]
        cur_res_germ <- germ_probs[names(germ_probs) == cur_res]
        
        cur_draws <- all_draw_data %>%
          filter(Focal == cur_foc) %>%
          filter(LambdaRatio == lambda_ratio) %>%
          dplyr::select("lambda", all_of(cur_alpha))
        
        cur_igrs <- cur_foc_germ * cur_draws[,1] / (1 + cur_draws[,2] * cur_res_germ * cur_car_cap$Nbar) + (1 - cur_foc_germ) * cur_foc_surv
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

jpeg("./figs/SIFigVarIGR.jpeg",
     width = 3500, height = 2500, res = 300)
plIGR
dev.off()


### NOW PLOTTING RESULTS FOR A SPECIFIC FIT IE CHOOSING A PRIOR

draw_data <- all_draw_data %>%
  filter(LambdaRatio == 0.1) %>%
  dplyr::select(-c(LambdaRatio, BetaRatio))

plot_draws <- draw_data %>%
  melt(id.vars = c(".chain", ".iteration", ".draw", "Focal")) %>%
  mutate(Focal = toupper(Focal))

lambda_draws <- plot_draws %>%
  filter(variable == "lambda") %>%
  mutate(Focal = paste("Focal:", Focal))

plLambdas <- ggplot(lambda_draws, aes(x = value)) +
  geom_density(size = 1, color = "blue") + theme_classic() +
  facet_wrap(~Focal, nrow = 1, scales = "free") +
  labs(x = "Growth Rate", y = "") +
  theme(text = element_text(size=15),
        strip.text.x = element_text(size = 15),
        strip.text.y = element_text(size = 10),
        legend.text=element_text(size = 15),
        strip.background = element_blank())
plLambdas

jpeg("./figs/SIFigLambdas.jpeg",
     width = 3500, height = 1000, res = 300)
plLambdas
dev.off()

beta_draws <- plot_draws %>%
  filter(substr(variable, 1, 4) == "beta") %>%
  mutate(Resident = substr(variable, 5, 7)) %>%
  mutate(Focal = paste("Focal:", Focal)) %>%
  mutate(Resident = paste("Resident:", Resident))

plBetas <- ggplot(beta_draws, aes(x = value)) +
  geom_density(size = 1, color = "blue") + theme_classic() +
  facet_wrap(Focal ~ Resident, scales = "free") +
  labs(x = "Induction", y = "") +
  theme(text = element_text(size=15),
        strip.text.x = element_text(size = 15),
        strip.text.y = element_text(size = 15),
        legend.text=element_text(size = 15),
        strip.background = element_blank())
plBetas

jpeg("./figs/SIFigBetas.jpeg",
     width = 3500, height = 2500, res = 300)
plBetas
dev.off()

# some inducer number analysis

inducer_num_data <- data %>%
  select(c("Focal", "Inducer", "Inducer.Type", "Resident"))
inducer_num_data$IndNum <- data$X..inducers.on.May.7..I.hitnk.this.is.how.many.we.left..No.Sin.is.above.7
inducer_num_data$IndNum[inducer_num_data$IndNum == "15+"] <- 15
inducer_num_data$IndNum <- as.numeric(inducer_num_data$IndNum)

inducer_num_data <- inducer_num_data %>%
  filter(Inducer.Type != "None") %>%
  mutate(Focal = paste("Focal:", Focal))

ggplot(inducer_num_data, aes(x = Inducer.Type, y = IndNum)) +
  geom_point() + facet_grid(Focal~Resident)

plNumInd <- ggplot(inducer_num_data, aes(x = IndNum)) +
  geom_histogram(bins = 10, color = "black", fill = "white") +
  facet_wrap(~toupper(Inducer),
             nrow = 1) +
  theme(text = element_text(size=15),
        strip.text.x = element_text(size = 15),
        strip.text.y = element_text(size = 15),
        legend.text=element_text(size = 15),
        strip.background = element_blank()) +
  labs(x = "Number of inducers", y = "Count")
plNumInd

jpeg("./figs/SIFigNumInd.jpeg",
     width = 3500, height = 1000, res = 300)
plNumInd
dev.off()

mean_ind <- inducer_num_data %>%
  group_by(Inducer) %>%
  summarise(MeanInd = mean(IndNum))

adj_beta_draws <- beta_draws %>%
  mutate(NumInd = case_when(substr(Resident, 11, 13) == "BUG" ~ mean_ind$MeanInd[1],
                            substr(Resident, 11, 13) == "CEN" ~ mean_ind$MeanInd[2],
                            substr(Resident, 11, 13) == "PAP" ~ mean_ind$MeanInd[3],
                            substr(Resident, 11, 13) == "SIN" ~ mean_ind$MeanInd[4])) %>%
  mutate(AdjBeta = value / NumInd)


plAdjBetas <- ggplot(adj_beta_draws, aes(x = AdjBeta)) +
  geom_density(size = 1, color = "blue") + theme_classic() +
  facet_wrap(Focal ~ Resident, scales = "free") +
  labs(x = "Induction", y = "") +
  scale_x_log10() +
  theme(text = element_text(size=15),
        strip.text.x = element_text(size = 15),
        strip.text.y = element_text(size = 15),
        legend.text=element_text(size = 15),
        strip.background = element_blank())
plAdjBetas


# alpha analysis

alpha_draws <- plot_draws %>%
  filter(substr(variable, 4, 8) == "alpha") %>%
  mutate(Resident = substr(variable, 9, 11)) %>%
  mutate(Induction = substr(variable, 1, 3)) %>%
  mutate(Induction = case_when(Induction == "non" ~ "None",
                               Induction == "foc" ~ "Focal",
                               Induction == "res" ~ "Resident")) %>%
  mutate(Induction = factor(Induction, levels = c("None", "Focal", "Resident"))) %>%
  filter(!((Induction == "Resident") & (Focal == Resident))) %>%
  mutate(Focal = paste("Focal:", Focal)) %>%
  mutate(Resident = paste("Resident:", Resident))

plAlphas <- ggplot(alpha_draws, aes(x = value, color = Induction)) +
  geom_density(size = 1) + theme_classic() +
  facet_wrap(Focal ~ Resident, scales = "free") +
  labs(x = "Competition Coefficient", y = "") +
  theme(text = element_text(size=15),
        strip.text.x = element_text(size = 15),
        strip.text.y = element_text(size = 15),
        legend.text=element_text(size = 15),
        strip.background = element_blank())
plAlphas

jpeg("./figs/SIFigAlphaHists.jpeg",
     width = 3500, height = 2500, res = 300)
plAlphas
dev.off()

mat_draws <- plot_draws %>%
  filter(substr(variable, 4, 8) == "alpha") %>%
  mutate(Resident = substr(variable, 9, 11)) %>%
  mutate(Induction = substr(variable, 1, 3)) %>%
  mutate(Induction = case_when(Induction == "non" ~ "No Induction",
                               Induction == "foc" ~ "Focal Induction",
                               Induction == "res" ~ "Resident Induction"))

mat_data <- mat_draws %>%
  group_by(Focal, Resident, Induction, variable) %>%
  dplyr::summarise(Estimate = mean(value)) %>%
  mutate(Estimate = round(Estimate, 3)) %>%
  filter(!((Induction == "Resident Induction") & (Focal == Resident)))

res_duplicate_data <- mat_data %>%
  filter(Induction == "Focal Induction") %>%
  filter(Focal == Resident) %>%
  mutate(Induction = "Resident Induction")
mat_data <- rbind(mat_data, res_duplicate_data) %>%
  mutate(Induction = factor(Induction, levels = c("No Induction",
                                                  "Focal Induction",
                                                  "Resident Induction"))) %>%
  mutate(FillData = min(Estimate, 0.2))

plMatrices <- ggplot(mat_data, aes(x = Resident, y = Focal,
                                   fill = FillData, label = Estimate)) +
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
        strip.background = element_blank(),
        plot.caption = element_text(hjust = 0, face= "italic"),
        plot.title.position = "plot",
        plot.caption.position =  "plot")
plMatrices

jpeg("./figs/SIFigMatrices.jpeg",
     width = 3500, height = 1500, res = 300)
plMatrices
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
                                  "Focal",
                                  "Resident"))

alpha_merge_draws <- merge(alpha_merge_draws,
                           alpha_res_draws,
                           by = c(".chain",
                                  ".iteration",
                                  ".draw",
                                  "Focal",
                                  "Resident"))

alpha_diff_draws <- alpha_merge_draws %>%
  mutate(DiffFoc = value.y / value.x) %>%
  mutate(DiffRes = value / value.x) %>%
  dplyr::select(c("Resident", "Focal", "DiffFoc", "DiffRes")) %>%
  melt(id.vars = c("Focal", "Resident")) %>%
  mutate(variable = ifelse(variable == "DiffFoc", "Focal\nInduction", "Resident\nInduction")) %>%
  mutate(Resident = case_when(Resident == "BUG" ~ "B. arvensis",
                              Resident == "CEN" ~ "C. cyanus",
                              Resident == "PAP" ~ "P. rhoeas",
                              Resident == "SIN" ~ "S. arvensis")) %>%
  mutate(Focal = case_when(Focal == "BUG" ~ "B. arvensis",
                           Focal == "CEN" ~ "C. cyanus",
                           Focal == "PAP" ~ "P. rhoeas",
                           Focal == "SIN" ~ "S. arvensis")) %>%
  mutate(InteractionID = paste0(Focal, "\n", Resident)) %>%
  mutate(InteractionID = fct_rev(InteractionID)) %>%
  mutate(value = log(value))

intra_ints <- unique(alpha_diff_draws[,c("Focal", "Resident", "variable")])
intra_ints$value <- 1
intra_ints$Intra <- factor(as.vector(diag(1, 4, 4)))
intra_ints <- intra_ints %>%
  mutate(variable = factor(variable, levels = c("Resident\nInduction", "Focal\nInduction")))

plot_alpha_diff_draws <- alpha_diff_draws %>%
  filter(!((Focal == Resident) & (variable == "Resident\nInduction"))) %>%
  mutate(variable = factor(variable, levels = c("Resident\nInduction", "Focal\nInduction")))


label_data <- data.frame(x = c(0.475, 0.5, 0.53),
                         y = c(0, 0.5, 1),
                         label = c("", "Responding Species", ""))

plLabel <- ggplot(label_data, aes(x = x, y = y, label = label)) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.grid = element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank(),
        plot.margin=unit(c(0,0,0,0),"mm"),
        plot.title = element_text(hjust = 0.5, size = 17.5)) +
  coord_fixed() +
  geom_text(size = 4.5, angle = 90)
plLabel

plAlphaDiffs <- ggplot(plot_alpha_diff_draws, aes(x = value, y = variable,
                                                  fill = after_stat(x < 0))) +
  geom_rect(data = intra_ints,
            aes(fill = Intra, xmin = -Inf, xmax = Inf,
                ymin = stage("Resident\nInduction", after_scale = ymin - Inf),
                ymax = stage("Focal\nInduction", after_scale = ymax + Inf)),
            alpha = 0.35) +
  stat_slab(aes(alpha = (after_stat(level))), .width = c(.89, 1)) +
  scale_alpha_manual(values = c(0.5, 1, 0.5)) +
  scale_y_discrete(position = "right") +
  facet_grid(Focal ~ Resident, switch = "y") +
  geom_vline(xintercept = 0, alpha = 0.75, linetype = "dashed") +
  theme(text = element_text(size=10),
        strip.text = element_text(face = "italic"),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        legend.text=element_text(size = 15),
        strip.background = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 0.5),
        plot.caption = element_text(hjust = 0, face= "italic"),
        plot.title.position = "plot",
        plot.caption.position =  "plot") +
  scale_fill_manual(values=c("white", "lightgrey", "#E69F00", "#56B4E9")) +
  labs(x = "Log Ratio of Induced to Not Induced Interaction Strength", y = "",
       color = "Induction", title = "Affecting Species") +
  xlim(c(-5, 5))
plAlphaDiffs

layout_vec <- c(1, rep(2, times = 12))
layout_mat <- matrix(layout_vec, nrow = 1, ncol = length(layout_vec))
jpeg("./figs/Fig3Alphas.jpeg",
     width = 2250, height = 1250, res = 300)
grid.arrange(plLabel, plAlphaDiffs, layout_matrix = layout_mat)
#plot_grid(plLabel, plAlphaDiffs, align = "h", axis = "t")
dev.off()

ggplot(alpha_diff_draws, aes(x = value, y = InteractionID,
                             fill = after_stat(x < 0))) +
  stat_halfeye() +
  facet_wrap(~variable) +
  geom_vline(xintercept = 0, alpha = 0.75, linetype = "dashed") +
  theme(text = element_text(size=15),
        legend.position = "none",
        legend.text=element_text(size = 15),
        strip.background = element_blank(),
        plot.caption = element_text(hjust = 0, face= "italic"),
        plot.title.position = "plot",
        plot.caption.position =  "plot") +
  labs(x = "Log Ratio of Induced to Not Induced Alpha", y = "Species Interaction") +
  scale_fill_manual(values=c("#E69F00", "#56B4E9")) +
  annotate("rect", xmin = -6, xmax = 6,
           ymin = 0.9, ymax = 1.9, 
           alpha = 0.1) +
  annotate("rect", xmin = -6, xmax = 6,
           ymin = 5.9, ymax = 6.9, 
           alpha = 0.1) +
  annotate("rect", xmin = -6, xmax = 6,
           ymin = 10.9, ymax = 11.9, 
           alpha = 0.1) +
  annotate("rect", xmin = -6, xmax = 6,
           ymin = 15.9, ymax = 16.9,
           alpha = 0.1)

summary_alpha_diffs <- alpha_diff_draws %>%
  group_by(Resident, Focal, variable) %>%
  dplyr::summarise(ProbMoreCompetitive = mean(value > 0))
summary_alpha_diffs

alpha_diff_draws %>%
  group_by(variable) %>%
  dplyr::summarise(ProbMoreCompetitive = mean(value > 0))

alpha_diff_draws %>%
  filter(Focal == Resident) %>%
  group_by(variable) %>%
  dplyr::summarise(ProbMoreCompetitive = mean(value > 0))

alpha_diff_draws %>%
  filter(Focal != Resident) %>%
  group_by(variable) %>%
  dplyr::summarise(ProbMoreCompetitive = mean(value > 0))

fit_params <- plot_draws %>%
  group_by(Focal, variable) %>%
  dplyr::summarise(Estimate = mean(value)) %>%
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
  mutate(PredSeeds = (Lambda / (1 + Beta + Alpha * Resident.Density)))

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

jpeg("./figs/SIFigFits.jpeg",
     width = 3000, height = 2500, res = 300)
plFits
dev.off()

### igr analysis and plotting

car_cap_data <- data.frame()
for(cur_foc in Foc) {
  
  for(cur_ind in c("non", "foc")) {
    
    cur_alpha <- paste0(cur_ind, "alpha", toupper(cur_foc))
    
    print(cur_foc)
    print(cur_alpha)
    
    cur_draws <- draw_data %>%
      filter(Focal == cur_foc) %>%
      dplyr::select("lambda", all_of(cur_alpha))
    
    print(names(cur_draws))
    
    cur_germ <- germ_probs[names(germ_probs) == cur_foc]
    cur_surv <- surv_probs[names(surv_probs) == cur_foc]
    cur_eta <- cur_germ * cur_draws[,1] / (1 - (1 - cur_germ) * cur_surv)
    cur_car_cap_val <- (cur_eta - 1) / cur_germ / cur_draws[,2]
    
    cur_car_cap <- data.frame(Focal = cur_foc, Nbar = cur_car_cap_val)
    names(cur_car_cap) <- c("Focal", "Nbar")
    cur_car_cap$Induction <- cur_ind
    car_cap_data <- rbind(car_cap_data, cur_car_cap)
  }
  
}

car_cap_plotting <- car_cap_data %>%
  mutate(Induction = ifelse(Induction == "foc", "Focal", "None")) %>%
  mutate(Induction = factor(Induction, levels = c("None", "Focal")))

plCarryingCapacity <- ggplot(car_cap_plotting, aes(x = Nbar, color = Induction)) +
  geom_density() + scale_x_log10() +
  facet_wrap(~Focal, nrow = 1, scales = "free") +
  labs(x = "Carrying Capacity with Focal Induction", y = "") +
  theme(text = element_text(size=10),
        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5),
        strip.text.x = element_text(size = 10),
        strip.text.y = element_text(size = 10),
        legend.text=element_text(size = 10))
plCarryingCapacity

jpeg("./figs/SIFigCarryingCapacities.jpeg",
     width = 2000, height = 500, res = 300)
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
      
      cur_car_cap <- car_cap_data %>%
        filter(Focal == cur_res) %>%
        filter(Induction == induction_label)
      
      cur_alpha <- paste0(cur_ind_type, "alpha", toupper(cur_res))
      
      cur_foc_germ <- germ_probs[names(germ_probs) == cur_foc]
      cur_foc_surv <- surv_probs[names(surv_probs) == cur_foc]
      cur_res_germ <- germ_probs[names(germ_probs) == cur_res]
      
      cur_draws <- draw_data %>%
        filter(Focal == cur_foc) %>%
        dplyr::select("lambda", all_of(cur_alpha))
      
      cur_igrs <- cur_foc_germ * cur_draws[,1] / (1 + cur_draws[,2] * cur_res_germ * cur_car_cap$Nbar) + (1 - cur_foc_germ) * cur_foc_surv
      names(cur_igrs) <- "IGR"
      
      cur_igr_data <- data.frame(Focal = cur_foc,
                                 Resident = cur_res, 
                                 Induction = cur_ind_type,
                                 IGR = cur_igrs)
      
      igr_data <- rbind(igr_data, cur_igr_data)
      
    }
  }
}

wide_igr_data <- data.frame()

sp_pairs <- combn(x = Foc, m = 2) %>%
  t() %>%
  as.data.frame()

for(i in 1:nrow(sp_pairs)) {
  cur_sp1 <- sp_pairs$V1[i]
  cur_sp2 <- sp_pairs$V2[i]
  
  cur_sp1_data <- igr_data %>%
    filter(Focal == cur_sp1) %>%
    filter(Resident == cur_sp2)
  colnames(cur_sp1_data) <- c("Sp1", "Sp2", "Induction", "Sp1IGR")
  
  cur_sp2_data <- igr_data %>%
    filter(Focal == cur_sp2) %>%
    filter(Resident == cur_sp1)
  colnames(cur_sp2_data)[colnames(cur_sp2_data) == "IGR"] <- "Sp2IGR"
  
  prod(cur_sp1_data$Induction == cur_sp2_data$Induction)
  
  cur_wide_data <- cur_sp1_data
  cur_wide_data$Sp2IGR <- cur_sp2_data$Sp2IGR
  
  wide_igr_data <- rbind(wide_igr_data, cur_wide_data)
  
}

plot_wide_igr_data <- wide_igr_data %>%
  mutate(Induction = case_when(Induction == "non" ~ "None",
                               Induction == "foc" ~ "Focal",
                               Induction == "res" ~ "Resident")) %>%
  mutate(Induction = factor(Induction, levels = c("None", "Focal", "Resident"))) %>%
  filter(Induction != "Focal") %>%
  mutate(Sp1 = case_when(toupper(Sp1) == "BUG" ~ "B. arvensis",
                         toupper(Sp1) == "CEN" ~ "C. cyanus",
                         toupper(Sp1) == "PAP" ~ "P. rhoeas",
                         toupper(Sp1) == "SIN" ~ "S. arvensis")) %>%
  mutate(Sp2 = case_when(toupper(Sp2) == "BUG" ~ "B. arvensis",
                         toupper(Sp2) == "CEN" ~ "C. cyanus",
                         toupper(Sp2) == "PAP" ~ "P. rhoeas",
                         toupper(Sp2) == "SIN" ~ "S. arvensis")) %>%
  mutate(Sp1 = paste0("Species 1: *", Sp1, "*")) %>%
  mutate(Sp2 = paste0("Species 2: *", Sp2, "*")) %>%
  mutate(SpLabel = paste(Sp1, "  \n", Sp2))

plot_wide_igr_data$Counter <- 1:nrow(plot_wide_igr_data)

sub_wide_data <- plot_wide_igr_data %>%
  filter(Counter %% 10 == 0)

plIGR <- ggplot(sub_wide_data, aes(x = Sp1IGR, y = Sp2IGR, color = Induction, shape = Induction)) +
  geom_point(alpha = 0.025, size = 3) +
  facet_wrap( ~ SpLabel, nrow = 2) +
  scale_x_log10() + scale_y_log10() +
  geom_hline(yintercept = 1, color = 'black', linetype = "dotted") +
  geom_vline(xintercept = 1, color = 'black', linetype = "dotted") +
  labs(x = "Invasion Growth Rate of Species 1", y = "Invasion Growth Rate of Species 2") +
  theme(text = element_text(size=15),
        panel.spacing = unit(2, "lines"),
        strip.text = ggtext::element_markdown(),
        legend.text=element_text(size = 12),
        legend.position = c(0.95, 0.85),
        legend.background = element_rect(fill = "white", colour = "black"),
        strip.background = element_blank(),
        plot.caption = element_text(hjust = 0, face= "italic"),
        plot.title.position = "plot",
        plot.caption.position =  "plot") +
  scale_color_manual(values=c("darkblue", "darkred")) +
  guides(colour = guide_legend(override.aes = list(alpha = 1))) +
  ggtitle("(A)")

ann_text <- data.frame(Sp1IGR = 0.325, Sp2IGR = 600, Induction = "None",
                       SpLabel = "Species 1: *B. arvensis*   \n Species 2: *C. cyanus*")
plIGR <- plIGR + geom_text(data = ann_text, label = "Species 2 Wins", color = "black", size = 3.5)

ann_text <- data.frame(Sp1IGR = 0.325, Sp2IGR = 0.25, Induction = "None",
                       SpLabel = "Species 1: *B. arvensis*   \n Species 2: *C. cyanus*")
plIGR <- plIGR + geom_text(data = ann_text, label = "Priority Effect", color = "black", size = 3.5)

ann_text <- data.frame(Sp1IGR = 15, Sp2IGR = 0.25, Induction = "None",
                       SpLabel = "Species 1: *B. arvensis*   \n Species 2: *C. cyanus*")
plIGR <- plIGR + geom_text(data = ann_text, label = "Species 1 Wins", color = "black", size = 3.5)

ann_text <- data.frame(Sp1IGR = 15, Sp2IGR = 600, Induction = "None",
                       SpLabel = "Species 1: *B. arvensis*   \n Species 2: *C. cyanus*")
plIGR <- plIGR + geom_text(data = ann_text, label = "Coexistence", color = "black", size = 3.5)

plIGR


#grob <- grobTree(textGrob("Coexistence", x=0.775,  y=0.95, hjust=0,
#                          gp=gpar(col="black", fontsize=10)))

#plIGR <- plIGR + annotation_custom(grob)

#grob <- grobTree(textGrob("Species 2 Wins", x=0.05,  y=0.95, hjust=0,
#                          gp=gpar(col="black", fontsize=10)))

#plIGR <- plIGR + annotation_custom(grob)

#grob <- grobTree(textGrob("Species 1 Wins", x=0.725,  y=0.05, hjust=0,
#                          gp=gpar(col="black", fontsize=10)))

#plIGR <- plIGR + annotation_custom(grob)

#grob <- grobTree(textGrob("Priority Effect", x=0.05,  y=0.05, hjust=0,
#                          gp=gpar(col="black", fontsize=10)))

#plIGR <- plIGR + annotation_custom(grob)

plot_igr_data <- igr_data %>%
  mutate(Resident = paste("Resident:", toupper(Resident))) %>%
  mutate(Focal = paste("Focal:", toupper(Focal))) %>%
  mutate(Induction = case_when(Induction == "non" ~ "None",
                               Induction == "foc" ~ "Focal",
                               Induction == "res" ~ "Resident")) %>%
  mutate(Induction = factor(Induction, levels = c("None", "Focal", "Resident")))

plot_igr_data <- plot_igr_data %>%
  filter(Induction != "Focal")

plIGRHists <- ggplot(plot_igr_data, aes(x = IGR, color = Induction, linetype = Induction)) +
  geom_density() +
  facet_wrap(Focal ~ Resident, scales = 'free', ncol = 3) +
  scale_x_log10() +
  geom_vline(xintercept = 1, color = 'gray') +
  labs(x = "Invasion Growth Rate (IGR)", y = "") +
  theme(text = element_text(size=15),
        strip.text.x = element_text(size = 15),
        strip.text.y = element_text(size = 10),
        legend.text=element_text(size = 15),
        strip.background = element_blank(),
        legend.position = "top",
        plot.caption = element_text(hjust = 0, face= "italic"),
        plot.title.position = "plot",
        plot.caption.position =  "plot") +
  scale_color_manual(values=c("#000000", "#CC79A7"))

jpeg("./figs/SIFigIGRHists.jpeg",
     width = 2500, height = 2500, res = 300)
plIGRHists
dev.off()

summary_igr <- plot_igr_data %>%
  group_by(Focal, Resident, Induction) %>%
  dplyr::summarise(ProbInvasion = mean(IGR > 1))
summary_igr

igr_non_draws <- plot_igr_data %>%
  filter(Induction == "None")

igr_res_draws <- plot_igr_data %>%
  filter(Induction == "Resident")

igr_merge_draws <- igr_non_draws
igr_merge_draws$ResIGR <- igr_res_draws$IGR

igr_diff_draws <- igr_merge_draws %>%
  mutate(Diff = log(ResIGR / IGR)) %>%
  dplyr::select(c("Resident", "Focal", "Diff")) %>%
  melt(id.vars = c("Focal", "Resident")) %>%
  mutate(InteractionID = paste0(Focal, "\n", Resident)) %>%
  mutate(InteractionID = fct_rev(InteractionID))

comp_diff_draws <- igr_diff_draws %>%
  group_by(InteractionID) %>%
  summarise(ProbHarderToInvade = mean(value < 0))
comp_diff_draws

plIGRDiffs <- ggplot(igr_diff_draws, aes(x = value, y = InteractionID,
                                             fill = after_stat(x < 0))) +
  stat_halfeye() +
  geom_vline(xintercept = 0, alpha = 0.75, linetype = "dashed") +
  theme(text = element_text(size=15),
        legend.position = "none",
        legend.text=element_text(size = 15),
        strip.background = element_blank(),
        plot.caption = element_text(hjust = 0, face= "italic"),
        plot.title.position = "plot",
        plot.caption.position =  "plot") +
  labs(x = "Log Ratio of Induced to Not Induced IGR", y = "") +
  scale_fill_manual(values=c("#E69F00", "#56B4E9"))
plIGRDiffs

jpeg("./figs/SIFigIGRDiffs.jpeg",
     width = 1750, height = 2500, res = 300)
plIGRDiffs
dev.off()

bug_igr_draws <- data.frame(ResBugIGR = rep(0, times = 20000),
                            NonResBugIGR = rep(0, times = 20000))

res_count <- 0
non_res_count <- 0

for(cur_foc in toupper(Foc)) {
  res <- toupper(Foc)
  res <- res[res != cur_foc]
  cur_in_foc <- paste("Focal:", cur_foc)
  res <- paste("Resident:", res)
  for(cur_res in res) {
    cur_igr_draws <- igr_diff_draws %>%
      filter(Focal == cur_in_foc) %>%
      filter(Resident == cur_res)
    print(paste("Should be TRUE:", nrow(cur_igr_draws) == 20000))
    
    if(cur_res == "Resident: BUG") {
      bug_igr_draws$ResBugIGR <- bug_igr_draws$ResBugIGR + cur_igr_draws$value
      res_count <- res_count + 1
    } else {
      bug_igr_draws$NonResBugIGR <- bug_igr_draws$NonResBugIGR + cur_igr_draws$value
      non_res_count <- non_res_count + 1
    }
  }
}

bug_igr_draws$ResBugIGR <- bug_igr_draws$ResBugIGR / res_count
bug_igr_draws$NonResBugIGR <- bug_igr_draws$NonResBugIGR / non_res_count

melt_bug_igr_draws <- melt(bug_igr_draws) %>%
  mutate(variable = fct_rev(ifelse(variable == "ResBugIGR",
                           "B. arvensis",
                           "Not B. arvensis")))

plBUGIGR <- ggplot(melt_bug_igr_draws,
       aes(x = value, y = variable, fill = after_stat(x < 0))) +
  stat_slab(aes(alpha = (after_stat(level))), .width = c(.89, 1)) +
  scale_alpha_manual(values = c(0.5, 1, 0.5)) +
  geom_vline(xintercept = 0, alpha = 0.75, linetype = "dashed") +
  theme(text = element_text(size=15),
        strip.text.x = element_text(size = 15),
        strip.text.y = element_text(size = 10),
        legend.text=element_text(size = 15),
        legend.position = "none",
        strip.background = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 0.5),
        axis.text.y = element_text(face = "italic"),
        plot.caption = element_text(hjust = 0, face= "italic"),
        plot.title.position = "plot",
        plot.caption.position =  "plot") +
  scale_fill_manual(values=c("#E69F00", "#56B4E9")) +
  labs(x = "Logged Invasion Growth Rate\nAveraged Across Species", y = "Resident") +
  ggtitle("(B)")
plBUGIGR


### are the betas related to the alphas?

beta_draws <- plot_draws %>%
  filter(substr(variable, 1, 4) == "beta") %>%
  mutate(Resident = substr(variable, 5, 7))

non_alpha_draws <- plot_draws %>%
  filter(substr(variable, 1, 8) == "nonalpha") %>%
  mutate(Resident = substr(variable, 9, 11))

foc_alpha_draws <- plot_draws %>%
  filter(substr(variable, 1, 8) == "focalpha") %>%
  mutate(Resident = substr(variable, 9, 11))

res_alpha_draws <- plot_draws %>%
  filter(substr(variable, 1, 8) == "resalpha") %>%
  mutate(Resident = substr(variable, 9, 11))

non_beta_alpha_draws <- merge(beta_draws,
                              non_alpha_draws,
                              by = c(".chain",
                                     ".iteration",
                                     ".draw",
                                     "Focal",
                                     "Resident"))

foc_beta_alpha_draws <- merge(beta_draws,
                              foc_alpha_draws,
                              by = c(".chain",
                                     ".iteration",
                                     ".draw",
                                     "Focal",
                                     "Resident"))


res_beta_alpha_draws <- merge(beta_draws,
                              res_alpha_draws,
                              by = c(".chain",
                                     ".iteration",
                                     ".draw",
                                     "Focal",
                                     "Resident")) %>%
  filter(!(Focal == Resident))

res_intra_draws <- foc_beta_alpha_draws %>%
  filter(Focal == Resident) %>%
  mutate(variable.y = substr(variable.y, 4, 11)) %>%
  mutate(variable.y = paste0("res", variable.y))

res_beta_alpha_draws <- rbind(res_beta_alpha_draws,
                              res_intra_draws)

beta_alpha_draws <- rbind(non_beta_alpha_draws,
                          foc_beta_alpha_draws,
                          res_beta_alpha_draws) %>%
  mutate(SpInt = paste(Focal, Resident)) %>%
  mutate(Induction = substr(variable.y, 1, 3)) %>%
  mutate(Induction = case_when(Induction == "non" ~ "No Induction",
                               Induction == "foc" ~ "Focal Induction",
                               Induction == "res" ~ "Resident Induction")) %>%
  mutate(Induction = factor(Induction, levels = c("No Induction",
                                                  "Focal Induction",
                                                  "Resident Induction")))

plBetaAlphas <- ggplot(beta_alpha_draws, aes(x = value.x, y = value.y)) +
  geom_point(alpha = 0.01, aes(color = SpInt)) + scale_x_log10() + scale_y_log10() +
  facet_wrap(~Induction) + labs(x = expression("Beta" ~(beta[ij])),
                                y = expression("Alpha" ~(alpha[ij])),
                                color = "Species\nInteraction") +
  geom_smooth(formula = y ~ x) +
  guides(color = guide_legend(override.aes = list(alpha = 1) ) ) +
  theme(text = element_text(size=15),
        strip.text.x = element_text(size = 15),
        strip.text.y = element_text(size = 10),
        legend.text=element_text(size = 15),
        strip.background = element_blank())
plBetaAlphas

jpeg("./figs/SIFigBetaAlphas.jpeg",
     width = 3500, height = 1500, res = 300)
plBetaAlphas
dev.off()


# computing niche and fitness differences with and without resident induced plasticity

nfd_data <- data.frame()
comp_ab_data <- data.frame()

for(cur_foc in Foc[1:3]) {
  
  print(cur_foc)
  
  res_indices <- Res
  res_indices <- res_indices[(which(Res == cur_foc) + 1):length(Res)]
  
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
        dplyr::select("Focal", "lambda", all_of(cur_alpha_ff), all_of(cur_alpha_fr))
      
      cur_res_draws <- draw_data %>%
        filter(Focal == cur_res) %>%
        dplyr::select("Focal", "lambda", all_of(cur_alpha_rr), all_of(cur_alpha_rf))
      
      cur_lambda_f <- cur_foc_draws$lambda
      cur_lambda_r <- cur_res_draws$lambda
      
      cur_alpha_ff <- cur_foc_draws[,3]
      cur_alpha_rr <- cur_res_draws[,3]
      cur_alpha_fr <- cur_foc_draws[,4]
      cur_alpha_rf <- cur_res_draws[,4]
      
      cur_niche_diff <- sqrt(cur_alpha_rf * cur_alpha_fr / cur_alpha_ff / cur_alpha_rr)
      names(cur_niche_diff) <- "NicheDiff"
      
      cur_germ_f <- germ_probs[names(germ_probs) == cur_foc]
      cur_surv_f <- surv_probs[names(surv_probs) == cur_foc]
      
      cur_germ_r <- germ_probs[names(germ_probs) == cur_res]
      cur_surv_r <- surv_probs[names(surv_probs) == cur_res]
      
      cur_eta_f <- cur_germ_f * cur_lambda_f / (1 - (1 - cur_germ_f) * cur_surv_f)
      cur_eta_r <- cur_germ_r * cur_lambda_r / (1 - (1 - cur_germ_r) * cur_surv_r)
      
      cur_comp_ab_f <- (cur_eta_f - 1) / sqrt(cur_alpha_ff * cur_alpha_fr)
      cur_comp_ab_r <- (cur_eta_r - 1) / sqrt(cur_alpha_rr * cur_alpha_rf)
      
      names(cur_comp_ab_f) <- "CompAbFoc"
      names(cur_comp_ab_r) <- "CompAbRes"
      
      cur_comp_ab_f_nolambda <- 1 / sqrt(cur_alpha_ff * cur_alpha_fr)
      cur_comp_ab_r_nolambda <- 1 / sqrt(cur_alpha_rr * cur_alpha_rf)
      
      names(cur_comp_ab_f_nolambda) <- "CompAbFoc"
      names(cur_comp_ab_r_nolambda) <- "CompAbRes"
      
      cur_comp_ab_data <- data.frame(Focal = cur_foc,
                                     Resident = cur_res,
                                     Induction = cur_ind_type,
                                     CompAbFoc = cur_comp_ab_f,
                                     CompAbRes = cur_comp_ab_r)
      
      comp_ab_data <- rbind(comp_ab_data, cur_comp_ab_data)
      
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
  mutate(SpInt = paste0(toupper(Focal), "\n", toupper(Resident))) %>%
  dplyr::select("SpInt", "NicheDiff", "FitDiff")

res_nfd_data <- nfd_data %>%
  filter(Induction == "res") %>%
  mutate(SpInt = paste0(toupper(Focal), "\n", toupper(Resident))) %>%
  dplyr::select("SpInt", "NicheDiff", "FitDiff")

plot_nfd_data <- non_nfd_data
plot_nfd_data$ResNicheDiff <- res_nfd_data$NicheDiff
plot_nfd_data$ResFitDiff <- res_nfd_data$FitDiff

comp_ab_data <- comp_ab_data %>%
  filter(Induction == "non")

foc_comp_ab_data <- comp_ab_data %>%
  select(!c("Induction", "CompAbRes"))
names(foc_comp_ab_data) <- c("Focal", "Resident", "CompAb")

res_comp_ab_data <- comp_ab_data %>%
  select(!c("Induction", "CompAbFoc"))
res_comp_ab_data <- res_comp_ab_data[,c(2, 1, 3)]
names(res_comp_ab_data) <- c("Focal", "Resident", "CompAb")

comp_ab_data <- rbind(foc_comp_ab_data, res_comp_ab_data)
plCompAbDensities <- ggplot(comp_ab_data, aes(x = CompAb)) +
  facet_grid(Focal~Resident) + geom_density() + scale_x_log10()
plCompAbDensities

#ggplot(plot_nfd_data, aes(x = NicheDiff, y = ResNicheDiff, color = SpInt)) +
#  geom_point(alpha = 0.01) + scale_x_log10() + scale_y_log10() +
#  geom_abline(slope = 1, intercept = 0)


#ggplot(plot_nfd_data, aes(x = FitDiff, y = ResFitDiff, color = SpInt)) +
#  geom_point(alpha = 0.01) + scale_x_log10() + scale_y_log10() +
#  geom_abline(slope = 1, intercept = 0)

plot_nfd_data <- plot_nfd_data %>%
  mutate(DeltaNicheDiff = log(ResNicheDiff / NicheDiff)) %>%
  mutate(DeltaFitDiff = log(ResFitDiff / FitDiff)) %>%
  dplyr::select("SpInt", DeltaNicheDiff, DeltaFitDiff) %>%
  melt(id.vars = c("SpInt")) %>%
  mutate(SpInt = fct_rev(SpInt)) %>%
  mutate(variable = ifelse(variable == "DeltaNicheDiff", "Change in Niche Overlap", "Change in Fitness Difference")) %>%
  mutate(variable = factor(variable, levels = c("Change in Niche Overlap", "Change in Fitness Difference")))

plNFDiffs <- ggplot(plot_nfd_data, aes(x = value, y = SpInt, fill = after_stat(x < 0))) +
         stat_halfeye() +
         facet_grid(~variable) +
         geom_vline(xintercept = 0, alpha = 0.75, linetype = "dashed") +
         theme(text = element_text(size=15),
               legend.position = "none",
               legend.text=element_text(size = 15),
               strip.background = element_blank()) +
         labs(x = "Log Ratio of Resident Induced to Not Induced Coexistence Metric",
              y = "Species Pair") +
  scale_fill_manual(values=c("#E69F00", "#56B4E9"))
plNFDiffs

jpeg("./figs/SIFigNFDiffs.jpeg",
     width = 2250, height = 1500, res = 300)
plNFDiffs
dev.off()

# computing the decomposition of niche difference via plasticity

decomp_data <- data.frame()

for(cur_foc in Foc[1:3]) {
  
  print(cur_foc)
  
  res_indices <- Res
  res_indices <- res_indices[(which(Res == cur_foc) + 1):length(Res)]
  
  for(cur_res in res_indices) {
    
    cur_alpha_ff_0 <- paste0("non", "alpha", toupper(cur_foc))
    cur_alpha_rr_0 <- paste0("non", "alpha", toupper(cur_res))
    cur_alpha_fr_0 <- paste0("non", "alpha", toupper(cur_res))
    cur_alpha_rf_0 <- paste0("non", "alpha", toupper(cur_foc))
    
    cur_alpha_rr_r <- paste0("foc", "alpha", toupper(cur_res))
    cur_alpha_fr_r <- paste0("res", "alpha", toupper(cur_res))
    cur_alpha_rf_r <- paste0("foc", "alpha", toupper(cur_foc))
    
    cur_alpha_ff_f <- paste0("foc", "alpha", toupper(cur_foc))
    cur_alpha_fr_f <- paste0("foc", "alpha", toupper(cur_res))
    cur_alpha_rf_f <- paste0("res", "alpha", toupper(cur_foc))
    
    cur_foc_draws <- draw_data %>%
      filter(Focal == cur_foc) %>%
      dplyr::select("Focal",
             all_of(cur_alpha_ff_0), all_of(cur_alpha_fr_0),
             all_of(cur_alpha_fr_r),
             all_of(cur_alpha_ff_f), all_of(cur_alpha_fr_f))
    
    cur_res_draws <- draw_data %>%
      filter(Focal == cur_res) %>%
      dplyr::select("Focal",
             all_of(cur_alpha_rr_0), all_of(cur_alpha_rf_0),
             all_of(cur_alpha_rr_r), all_of(cur_alpha_rf_r),
             all_of(cur_alpha_rf_f))
    
    cur_alpha_ff_0 <- cur_foc_draws[,2]
    cur_alpha_rr_0 <- cur_res_draws[,2]
    cur_alpha_fr_0 <- cur_foc_draws[,3]
    cur_alpha_rf_0 <- cur_res_draws[,3]
    
    cur_alpha_ff_r <- cur_alpha_ff_0
    cur_alpha_rr_r <- cur_res_draws[,4]
    cur_alpha_fr_r <- cur_foc_draws[,4]
    cur_alpha_rf_r <- cur_res_draws[,5]
    
    cur_alpha_ff_f <- cur_foc_draws[,5]
    cur_alpha_rr_f <- cur_alpha_rr_0
    cur_alpha_fr_f <- cur_foc_draws[,6]
    cur_alpha_rf_f <- cur_res_draws[,6]
    
    cur_niche_diff <- sqrt(cur_alpha_rf_0 * cur_alpha_fr_0 / cur_alpha_ff_0 / cur_alpha_rr_0)
    names(cur_niche_diff) <- "NicheDiff"
    
    cur_ind_niche_diff <- sqrt(cur_alpha_rf_f * cur_alpha_fr_r / cur_alpha_ff_f / cur_alpha_rr_r)
    names(cur_ind_niche_diff) <- "IndNicheDiff"
    
    cur_foc_niche_diff <- sqrt(cur_alpha_rf_f * cur_alpha_fr_f / cur_alpha_ff_f / cur_alpha_rr_f)
    cur_res_niche_diff <- sqrt(cur_alpha_rf_r * cur_alpha_fr_r / cur_alpha_ff_r / cur_alpha_rr_r)
    
    cur_decomp_niche_diff <- sqrt(cur_foc_niche_diff * cur_res_niche_diff)
    names(cur_decomp_niche_diff) <- "DecompNicheDiff"
    
    cur_foc_fit_diff <- sqrt(cur_alpha_rr_f * cur_alpha_rf_f / cur_alpha_fr_f / cur_alpha_ff_f)
    cur_res_fit_diff <- sqrt(cur_alpha_rr_r * cur_alpha_rf_r / cur_alpha_fr_r / cur_alpha_ff_r)
    
    cur_ind_fit_diff <- sqrt(cur_foc_fit_diff / cur_res_fit_diff)
    names(cur_ind_fit_diff) <- "DecompFitDiff"
    
    cur_decomp_data <- data.frame(Focal = cur_foc,
                                  Resident = cur_res, 
                                  NicheDiff = cur_niche_diff,
                                  IndNicheDiff = cur_ind_niche_diff,
                                  DecompNicheDiff = cur_decomp_niche_diff,
                                  DecompFitDiff = cur_ind_fit_diff)
    
    decomp_data <- rbind(decomp_data, cur_decomp_data)
    
  }
}

plot_decomp_data <- decomp_data %>%
  mutate(Focal = case_when(toupper(Focal) == "BUG" ~ "B. arvensis",
                           toupper(Focal) == "CEN" ~ "C. cyanus",
                           toupper(Focal) == "PAP" ~ "P. rhoeas",
                           toupper(Focal) == "SIN" ~ "S. arvensis")) %>%
  mutate(Resident = case_when(toupper(Resident) == "BUG" ~ "B. arvensis",
                              toupper(Resident) == "CEN" ~ "C. cyanus",
                              toupper(Resident) == "PAP" ~ "P. rhoeas",
                              toupper(Resident) == "SIN" ~ "S. arvensis")) %>%
  mutate(SpInt = paste0(Focal, "\n", Resident)) %>%
  dplyr::select("SpInt", "NicheDiff", "IndNicheDiff", "DecompNicheDiff", "DecompFitDiff") %>%
  mutate(DecompNicheDiff = DecompNicheDiff / NicheDiff) %>%
  mutate(NicheDiff = -log(NicheDiff),
         IndNicheDiff = -log(IndNicheDiff),
         DecompNicheDiff = -log(DecompNicheDiff),
         DecompFitDiff = -log(DecompFitDiff)) %>%
  melt(id.vars = c("SpInt")) %>%
  mutate(SpInt = fct_rev(SpInt))

plot_decomp_data <- plot_decomp_data %>%
  filter(variable %in% c("DecompNicheDiff", "DecompFitDiff"))


plot_fd_data <- plot_nfd_data %>%
  filter(variable == "Change in Fitness Difference") %>%
  mutate(Focal = case_when(substr(SpInt, 1, 3) == "BUG" ~ "B. arvensis",
                           substr(SpInt, 1, 3) == "CEN" ~ "C. cyanus",
                           substr(SpInt, 1, 3) == "PAP" ~ "P. rhoeas",
                           substr(SpInt, 1, 3) == "SIN" ~ "S. arvensis")) %>%
  mutate(Resident = case_when(substr(SpInt, 5, 7) == "BUG" ~ "B. arvensis",
                              substr(SpInt, 5, 7) == "CEN" ~ "C. cyanus",
                              substr(SpInt, 5, 7) == "PAP" ~ "P. rhoeas",
                              substr(SpInt, 5, 7) == "SIN" ~ "S. arvensis")) %>%
  mutate(SpInt = paste0(Focal, "\n", Resident)) %>%
  select(!c("Focal", "Resident"))

plot_decomp_data <- rbind(plot_decomp_data, plot_fd_data) %>%
  mutate(variable = case_when(variable == "DecompNicheDiff" ~ "(C) Change in average\nniche difference due to plasticity",
                              variable == "DecompFitDiff" ~ "(D) Niche difference from\nrare species competitive advantage",
                              variable == "Change in Fitness Difference" ~ "(E) Change in fitness\ndifference due to plasticity"))

plDecomp <- ggplot(plot_decomp_data, aes(x = value, y = SpInt, fill = after_stat(x < 0))) +
  stat_slab(aes(alpha = (after_stat(level))), .width = c(.89, 1)) +
  scale_alpha_manual(values = c(0.5, 1, 0.5)) +
  facet_grid( ~ variable) +
  geom_vline(xintercept = 0, alpha = 0.75, linetype = "dashed") +
  theme(text = element_text(size=15),
        strip.text.x = element_text(size = 15),
        strip.text.y = element_text(size = 10),
        legend.text=element_text(size = 15),
        legend.position = "none",
        strip.background = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 0.5),
        axis.text.y = element_text(face = "italic"),
        plot.caption = element_text(hjust = 0, face= "italic"),
        plot.title.position = "plot",
        plot.caption.position =  "plot") +
  scale_fill_manual(values=c("#E69F00", "#56B4E9")) +
  labs(x = "Logged Coexistence Metric", y = "Species Pair")
plDecomp

jpeg("./figs/FigIGRNFD.jpeg",
     width = 4800, height = 3200, res = 300)
grid.arrange(plIGR, plBUGIGR,
             plDecomp, layout_matrix = rbind(c(1, 1, 1, 1, 2, 2), c(3, 3, 3, 3, 3, 3)))
dev.off()

plot_decomp_data <- decomp_data %>%
  mutate(DecompNicheDiff = DecompNicheDiff / NicheDiff) %>%
  mutate(SpInt = paste0(toupper(Focal), "\n", toupper(Resident))) %>%
  dplyr::select("SpInt", "NicheDiff", "DecompNicheDiff", "DecompFitDiff") %>%
  melt(id.vars = c("SpInt")) %>%
  mutate(SpInt = fct_rev(SpInt)) %>%
  mutate(variable = case_when(variable == "NicheDiff" ~ "Niche Difference",
                              variable == "DecompNicheDiff" ~ "Ratio of Decomposed to Normal Niche Difference",
                              variable == "DecompFitDiff" ~ "Decomposed Fitness Difference")) %>%
  mutate(variable = factor(variable, levels = c("Niche Difference",
                                                "Ratio of Decomposed to Normal Niche Difference",
                                                "Decomposed Fitness Difference")))

plRelDecomp <- ggplot(plot_decomp_data, aes(x = -log(value), y = SpInt, fill = after_stat(x < 0))) +
  stat_halfeye() +
  facet_grid(~variable) +
  geom_vline(xintercept = 0, alpha = 0.75, linetype = "dashed") +
  theme(text = element_text(size=15),
        legend.position = "none",
        legend.text=element_text(size = 15),
        strip.background = element_blank()) +
  labs(x = "Logged Coexistence Metric",
       y = "Species Pair") +
  scale_fill_manual(values=c("#E69F00", "#56B4E9"))
plRelDecomp

# competition patterns

comp_data <- data.frame()

for(cur_foc in Foc) {
  
  print(cur_foc)
  
  cur_non_alphas <- paste0("nonalpha", toupper(Res))
  cur_res_alphas <- paste0("resalpha", toupper(Res))
  cur_foc_alphas <- paste0("focalpha", toupper(Res))
  
  cur_draws <- draw_data %>%
    filter(Focal == cur_foc) %>%
    dplyr::select(all_of(cur_non_alphas), all_of(cur_res_alphas), all_of(cur_foc_alphas))
  
  cur_foc_ratios <- cur_draws[,9:12] / cur_draws[,1:4]
  colnames(cur_foc_ratios) <- substr(colnames(cur_foc_ratios), 9, 11)
  
  cur_intra_ind <- which(colnames(cur_foc_ratios) == toupper(cur_foc))
  cur_intra_foc_ratios <- cur_foc_ratios[,cur_intra_ind]
  
  cur_inter_foc_ratios <- apply(cur_foc_ratios[,-cur_intra_ind], 1, prod)
  
  cur_res_ratios <- cur_draws[,5:8] / cur_draws[,1:4]
  colnames(cur_res_ratios) <- substr(colnames(cur_res_ratios), 9, 11)
  
  cur_inter_res_ratios <- apply(cur_res_ratios[,-cur_intra_ind], 1, prod)
  
  cur_inter_ratios <- (cur_inter_foc_ratios * cur_inter_res_ratios)^(1/6)
  
  cur_com_data <- data.frame(Focal = cur_foc,
                             Interspecific = cur_inter_ratios,
                             Intraspecific = cur_intra_foc_ratios)
  
  comp_data <- rbind(comp_data, cur_com_data)
}

melt_comp_data <- comp_data %>%
  melt(id.vars = c("Focal")) %>%
  mutate(value = log(value)) %>%
  mutate(variable = factor(variable, levels = c("Intraspecific",
                                                "Interspecific"))) %>%
  mutate(Focal = case_when(Focal == "Bug" ~ "B. arvensis",
                           Focal == "Cen" ~ "C. cyanus",
                           Focal == "Pap" ~ "P. rhoeas",
                           Focal == "Sin" ~ "S. arvensis")) %>%
  mutate(Focal = factor(Focal, levels = c("S. arvensis",
                                          "P. rhoeas",
                                          "C. cyanus",
                                          "B. arvensis")))

plCompPatterns <- ggplot(melt_comp_data, aes(y = Focal, x = value, fill = after_stat(x < 0))) +
  stat_slab(aes(alpha = (after_stat(level))), .width = c(.89, 1)) +
  scale_alpha_manual(values = c(0.5, 1, 0.5)) +
  facet_grid(~variable) +
  labs(x = "Average Log Ratio of Induced\nto Not Induced Interaction Strength", y = "") +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.5) +
  theme(text = element_text(size=20),
        legend.position = "none",
        axis.text.y = element_text(face = "italic"), 
        legend.text=element_text(size = 20),
        panel.spacing = unit(2, "lines"),
        strip.background = element_blank(),
        plot.caption = element_text(hjust = 0, face= "italic"),
        plot.title.position = "plot",
        plot.margin=unit(c(1,1,1,1),"cm"),
        aspect.ratio = 1,
        strip.placement = "outside",
        plot.caption.position =  "plot") +
  scale_fill_manual(values=c("#E69F00", "#56B4E9")) +
  ggtitle("(A)")
plCompPatterns




### inducer patterns

ind_data <- data.frame()
ind_int_data <- data.frame()

for(cur_foc in Foc) {
  
  print(cur_foc)
  
  res_indices <- Res
  res_indices <- res_indices[!(res_indices == cur_foc)]
  
  cur_non_alphas <- paste0("nonalpha", toupper(res_indices))
  cur_res_alphas <- paste0("resalpha", toupper(res_indices))
  
  cur_draws <- draw_data %>%
    filter(Focal == cur_foc) %>%
    dplyr::select(all_of(cur_res_alphas), all_of(cur_non_alphas))
  
  cur_susc_ratios <- cur_draws[,1:3] / cur_draws[,4:6]
  colnames(cur_susc_ratios) <- substr(colnames(cur_susc_ratios), 9, 11)
  
  cur_susc <- apply(cur_susc_ratios, 1, prod)
  
  melt_susc <- melt(cur_susc_ratios)
  
  cur_draws <- draw_data %>%
    filter(Focal != cur_foc)
  cur_foc_ids <- cur_draws$Focal
  
  cur_draws <- cur_draws[,grepl(toupper(cur_foc), colnames(draw_data))]
  cur_draws <- cur_draws[,!grepl("beta", colnames(cur_draws))]
  cur_draws <- cur_draws[,!grepl("foc", colnames(cur_draws))]
  
  cur_str_ratios <- cur_draws[,2] / cur_draws[,1]
  names(cur_str_ratios) <- "Ratio"

  cur_str_ratios <- data.frame(Focal = cur_foc_ids, Ratio = cur_str_ratios)
  cur_str_ratios <- cur_str_ratios %>%
    pivot_wider(names_from = Focal, values_from = Ratio, values_fn = list) %>%
    unnest(cols = everything())
  
  cur_strs <- apply(cur_str_ratios, 1, prod)
  
  colnames(cur_str_ratios) <- toupper(colnames(cur_str_ratios))
  melt_strs <- melt(cur_str_ratios)
  
  if(!prod(melt_susc$variable == melt_strs$variable)) {
    print("Data frames did not line up!")
  } else {
    cur_melts <- data.frame(Focal = toupper(cur_foc),
                            SpID = paste("Focal:", toupper(cur_foc), "\nResident:", melt_susc$variable), 
                            Susc = log(melt_susc$value),
                            Str = log(melt_strs$value))
  }
  
  ind_int_data <- rbind(ind_int_data, cur_melts)
  
  cur_ind_data <- data.frame(Focal = cur_foc,
                             Susc = cur_susc,
                             Strength = cur_strs)
  
  ind_data <- rbind(ind_data, cur_ind_data)
}

melt_ind_data <- ind_data %>%
  melt(id.vars = c("Focal")) %>%
  mutate(variable = ifelse(variable == "Susc", "Induction Response", "Induction Effect")) %>%
  mutate(value = log(value)) %>%
  mutate(Focal = case_when(Focal == "Bug" ~ "B. arvensis",
                           Focal == "Cen" ~ "C. cyanus",
                           Focal == "Pap" ~ "P. rhoeas",
                           Focal == "Sin" ~ "S. arvensis")) %>%
  mutate(Focal = factor(Focal, levels = c("S. arvensis",
                                          "P. rhoeas",
                                          "C. cyanus",
                                          "B. arvensis")))

plIndPatterns <- ggplot(melt_ind_data, aes(y = Focal, x = value, fill = after_stat(x < 0))) +
  stat_slab(aes(alpha = (after_stat(level))), .width = c(.89, 1)) +
  scale_alpha_manual(values = c(0.5, 1, 0.5)) +
  facet_grid(~variable, switch="both") +
  labs(x = "", y = "") +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.5) +
  theme(text = element_text(size=20),
        legend.position = "none",
        axis.text.y = element_text(face = "italic"), 
        legend.text=element_text(size = 20),
        panel.spacing = unit(2, "lines"),
        strip.background = element_blank(),
        plot.caption = element_text(hjust = 0, face= "italic"),
        plot.title.position = "plot",
        plot.margin=unit(c(1,1,1,1),"cm"),
        aspect.ratio = 1,
        strip.placement = "outside",
        plot.caption.position =  "plot") +
  scale_fill_manual(values=c("#E69F00", "#56B4E9")) +
  ggtitle("(B)")
plIndPatterns

summary_ind_patterns <- melt_ind_data %>%
  group_by(Focal, variable) %>%
  dplyr::summarise(ProbInc = mean(value > 0))
summary_ind_patterns

melt_ind_int_data <- ind_int_data %>%
  group_by(Focal, SpID) %>%
  dplyr::summarise(MedianSusc = median(Susc), MedianStr = median(Str),
            LowerSusc = quantile(Susc, probs = 0.05), LowerStr = quantile(Str, probs = 0.05),
            UpperSusc = quantile(Susc, probs = 0.95), UpperStr = quantile(Str, probs = 0.95)) %>%
  mutate(Focal = case_when(Focal == "BUG" ~ "B. arvensis",
                           Focal == "CEN" ~ "C. cyanus",
                           Focal == "PAP" ~ "P. rhoeas",
                           Focal == "SIN" ~ "S. arvensis"))

plIndSuscStr <- ggplot(melt_ind_int_data, aes(x = MedianSusc, y = MedianStr)) +
  stat_cor(label.y = 2.5) + #this means at 35th unit in the y axis, the r squared and p value will be shown
  stat_regline_equation(label.y = 1.5) +#this means at 30th unit regresion line equation will be shown
  geom_point(size = 4, aes(color = Focal)) +
  geom_smooth(formula = y ~ x, method = "lm", alpha = 0.25, color = "black", se = F) +
  geom_errorbar(aes(ymin = LowerStr, ymax = UpperStr, color = Focal), size = 1, alpha = 0.5) +
  geom_errorbarh(aes(xmin = LowerSusc, xmax = UpperSusc, color = Focal), size = 1, alpha = 0.5) +
  guides(color = guide_legend(override.aes = list(alpha = 1) ) ) +
  theme(text = element_text(size=20),
        strip.text.x = element_text(size = 15),
        strip.text.y = element_text(size = 15),
        legend.text=element_text(size = 15, face = "italic"),
        strip.background = element_blank(),
        legend.spacing.y = unit(0.5, 'cm'),
        legend.position = "right",
        plot.caption = element_text(hjust = 0, face= "italic"),
        plot.title.position = "plot",
        plot.caption.position =  "plot") +
  guides(color = guide_legend(byrow = TRUE)) +
  ggtitle("(B)") +
  labs(x = expression("Induction Response Per Interaction" ~ (log(alpha[ij]~"|"[j] / alpha[ij]~"|"[0]))),
       y = expression(atop("Induction Effect Per Interaction", (log(alpha[ji]~"|"[i] / alpha[ji]~"|"[0])))))
plIndSuscStr


ind_int_vs_comp_ab <- ind_int_data %>%
  mutate(SpID = substr(SpID, 23, 25))
colnames(ind_int_vs_comp_ab) <- c("Focal", "Resident", "IndResp", "IndEffect")

ind_int_vs_comp_ab$CompAb <- comp_ab_data$CompAb

ggplot(ind_int_vs_comp_ab, aes(x = log(CompAb), y = IndResp, color = Focal)) +
  geom_point(alpha = 0.05)

ggplot(ind_int_vs_comp_ab, aes(x = log(CompAb), y = IndEffect, color = Focal)) +
  geom_point(alpha = 0.05)

ind_int_vs_comp_ab <- ind_int_vs_comp_ab %>%
  mutate(CompAb = log(CompAb)) %>%
  mutate(draw = rep(1:20000, times = 12)) %>%
  group_by(Focal, draw) %>%
  dplyr:: summarise(CompAb = sum(CompAb), IndResp = sum(IndResp), IndEffect = sum(IndEffect)) %>%
  group_by(Focal) %>%
  dplyr::summarise(MedianCompAb = median(CompAb), MedianResp = median(IndResp), MedianEffect = median(IndEffect),
                   LowerCompAb = quantile(CompAb, probs = 0.05), LowerResp = quantile(IndResp, probs = 0.05),
                   LowerEffect = quantile(IndEffect, probs = 0.05),
                   UpperCompAb = quantile(CompAb, probs = 0.95), UpperResp = quantile(IndResp, probs = 0.95),
                   UpperEffect = quantile(IndEffect, probs = 0.95))

plRespVsComp <- ggplot(ind_int_vs_comp_ab, aes(x = MedianCompAb, y = MedianResp)) +
  stat_cor(label.y = 5) + #this means at 35th unit in the y axis, the r squared and p value will be shown
  stat_regline_equation(label.y = 4) +#this means at 30th unit regresion line equation will be shown
  geom_point(size = 4, aes(color = Focal)) +
  geom_smooth(formula = y ~ x, method = "lm", alpha = 0.25, color = "black", se = F) +
  geom_errorbar(aes(ymin = LowerResp, ymax = UpperResp, color = Focal), size = 1, alpha = 0.5) +
  geom_errorbarh(aes(xmin = LowerCompAb, xmax = UpperCompAb, color = Focal), size = 1, alpha = 0.5) +
  guides(color = guide_legend(override.aes = list(alpha = 1) ) ) +
  theme(text = element_text(size=20),
        strip.text.x = element_text(size = 20),
        strip.text.y = element_text(size = 20),
        legend.text=element_text(size = 20),
        strip.background = element_blank(),
        legend.spacing.y = unit(0.5, 'cm'),
        plot.caption = element_text(hjust = 0, face= "italic"),
        plot.title.position = "plot",
        plot.caption.position =  "plot") +
  guides(color = guide_legend(byrow = TRUE)) +
  labs(x = "Competitive Ability",
       y = expression(atop("Induction Response Per Interaction", (log(alpha[ij]~"|"[j] / alpha[ij]~"|"[0])))))
plRespVsComp


plEffectVsComp <- ggplot(ind_int_vs_comp_ab, aes(x = MedianCompAb, y = MedianEffect)) +
  stat_cor(label.y = 2.5) + #this means at 35th unit in the y axis, the r squared and p value will be shown
  stat_regline_equation(label.y = 1.5) +#this means at 30th unit regresion line equation will be shown
  geom_point(size = 4, aes(color = Focal)) +
  geom_smooth(formula = y ~ x, method = "lm", alpha = 0.25, color = "black", se = F) +
  geom_errorbar(aes(ymin = LowerEffect, ymax = UpperEffect, color = Focal), size = 1, alpha = 0.5) +
  geom_errorbarh(aes(xmin = LowerCompAb, xmax = UpperCompAb, color = Focal), size = 1, alpha = 0.5) +
  guides(color = guide_legend(override.aes = list(alpha = 1) ) ) +
  theme(text = element_text(size=20),
        strip.text.x = element_text(size = 20),
        strip.text.y = element_text(size = 20),
        legend.text=element_text(size = 20),
        strip.background = element_blank(),
        legend.spacing.y = unit(0.5, 'cm'),
        plot.caption = element_text(hjust = 0, face= "italic"),
        plot.title.position = "plot",
        plot.caption.position =  "plot") +
  guides(color = guide_legend(byrow = TRUE)) +
  labs(x = "Competitive Ability",
       y = expression(atop("Induction Effect Per Interaction", (log(alpha[ji]~"|"[i] / alpha[ji]~"|"[0])))))
plEffectVsComp

jpeg("./figs/Fig4IndPatterns.jpeg",
     width = 2400, height = 3000, res = 300)
plot_grid(plCompPatterns, plIndPatterns, nrow = 2)
dev.off()

