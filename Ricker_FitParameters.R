### PACKAGES ##################################################################################################

source("./Functions.R")

### READING IN THE DATA AND SETTING OPTIONS  #################################################################################

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

### PRELIMINARY PLOTS OF RESIDENT DENSITIES ########################################################################################

# Log Seeds (fecundity) data
data <- cbind(data, 'log.Seeds'= log(data$Seeds+1)) # log Seeds production +1

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
  select(c("Resident.Density",
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

plot_data <- MakePlotData(proc_data)

num_iter <- 10000
in_lambda_ratios <- c(0.1, 0.5, 1)

draw_data <- data.frame()

start_time <- Sys.time()

for(in_lambda_ratio in in_lambda_ratios) {
  
  print(paste0("Lambda ratio ",
               which(in_lambda_ratio == in_lambda_ratios ),
               " out of ",
               length(in_lambda_ratios),
               "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"))
  
  in_ind_ratio <- in_lambda_ratio
  
  for(focal in Foc) {
    
    print(paste("FOCAL SPECIES:",
                focal,
                "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"))
    
    cur_data <- proc_data %>%
      filter(Focal == focal)
    
    
    
    # first, we need to fit the lambdas alone to get
    # priors for the eventual fit with all params
    
    # filtering to no competition data
    lambda_data <- cur_data %>%
      filter(Resident.Density == 0) %>%
      filter(Inducer == "None")
    
    lambda_priors <- GetLambdaPriors(lambda_data)
    
    # fitting the growth rates
    lambda_fit <- brm(data = lambda_data,
                      family = gaussian,
                      Seeds ~ 1,
                      prior = lambda_priors,
                      iter = num_iter, refresh = 0, chains = 4,
                      backend = "cmdstanr")
    
    # fitting induction terms to get priors for them
    ind_priors <- GetInductionPriors(lambda_fit, lambda_data,
                                     lambda_ratio = in_lambda_ratio)
    
    # subsetting the data
    induction_data <- cur_data %>%
      filter(Resident.Density == 0)
    
    ind_fit <- brm(ricker_ind_formula,
                   data = induction_data,
                   prior = ind_priors,
                   iter = num_iter,
                   backend = "cmdstanr")
    
    alpha_priors <- GetAlphaPriors(ind_fit, lambda_fit, lambda_data,
                                   lambda_ratio = in_lambda_ratio,
                                   ind_ratio = in_ind_ratio)
    
    model_fit <- brm(ricker_ind_formula,
                     data = cur_data,
                     prior = alpha_priors,
                     iter = num_iter,
                     backend = "cmdstanr")
    
    cur_draw_data <- model_fit %>%
      spread_draws(b_lambda_Intercept,
                   b_betaBUG_Intercept,
                   b_betaCEN_Intercept,
                   b_betaPAP_Intercept,
                   b_betaSIN_Intercept,
                   b_nonalphaBUG_Intercept,
                   b_nonalphaCEN_Intercept,
                   b_nonalphaPAP_Intercept,
                   b_nonalphaSIN_Intercept,
                   b_focalphaBUG_Intercept,
                   b_focalphaCEN_Intercept,
                   b_focalphaPAP_Intercept,
                   b_focalphaSIN_Intercept,
                   b_resalphaBUG_Intercept,
                   b_resalphaCEN_Intercept,
                   b_resalphaPAP_Intercept,
                   b_resalphaSIN_Intercept)
    
    colnames(cur_draw_data) <- c(".chain",
                                 ".iteration",
                                 ".draw",
                                 "lambda",
                                 "betaBUG",
                                 "betaCEN",
                                 "betaPAP",
                                 "betaSIN",
                                 "nonalphaBUG",
                                 "nonalphaCEN",
                                 "nonalphaPAP",
                                 "nonalphaSIN",
                                 "focalphaBUG",
                                 "focalphaCEN",
                                 "focalphaPAP",
                                 "focalphaSIN",
                                 "resalphaBUG",
                                 "resalphaCEN",
                                 "resalphaPAP",
                                 "resalphaSIN")
    
    cur_draw_data$Focal <- focal
    
    cur_draw_data$LambdaRatio <- in_lambda_ratio
    cur_draw_data$BetaRatio <- in_ind_ratio
    
    loo_estimates <- loo(model_fit)$estimates[3, 1:2]
    waic_estimates <- waic(model_fit)$estimates[3, 1:2]
    
    cur_draw_data$LooEstimate <- loo_estimates[1]
    cur_draw_data$LooSE <- loo_estimates[2]
    
    cur_draw_data$WaicEstimate <- waic_estimates[1]
    cur_draw_data$WaicSE <- waic_estimates[2]
    
    draw_data <- rbind(draw_data, cur_draw_data)
    
  }
  
}

end_time <- Sys.time()
total_time <- end_time - start_time
print(total_time)

print(warnings())

# writing out the data
filename <- "Ricker_PosteriorDraws_PriorGradient"
cur_file <- paste0("./simdata/", filename, ".csv")
write_csv2(draw_data, file = cur_file)

