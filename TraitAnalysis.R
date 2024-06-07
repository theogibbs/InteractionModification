### PACKAGES ##################################################################################################

source("./Functions.R")

### READING IN THE DATA AND SETTING OPTIONS  #################################################################################


# REMOVE SILENE?!? adjust this toggle:
Silene.include <- FALSE # WRITE  TRUE to INCLUDE it - Write FALSE to exclude it

## PLOTS as LOG?  TRUE or FALSE!!   ---- NOT IMPLEMENTED
log.plots <- FALSE

# reading in whole plant data
plant <- read.csv('Plant.Pheno.data.June3.v2.csv', header = TRUE, na.strings ="", stringsAsFactors = FALSE)

Silene.include <- FALSE
## SPP inclusion
if(Silene.include){
  Foc<- c('Bug','Cen','Pap','Sil','Sin')  # focal species
  Ind<- c('None','Bug','Cen','Pap','Sil','Sin') # inducers
  
  # SHAPES of Points:  each species gets it own consistent point shape. 
  shapes <- c(10,21,22,23,24,25)
  names(shapes) <- c("None",'Bug','Cen','Pap','Sil','Sin')
  
  # colour  Same deal
  colours <- c('black', 'purple','green','red','blue','Orange')
  names(colours) <- c("None",'Bug','Cen','Pap','Sil','Sin')
  
  # set the order of the inducers
  plant$INDUCER<-     factor(plant$INDUCER, levels=c("None",'Bug','Cen','Pap','Sil','Sin'))
  
} else {  # exclude SILENE
  Foc<- c('Bug','Cen','Pap','Sin')  # focal species
  Ind<- c('None','Bug','Cen','Pap','Sin') # inducers
  
  shapes <- c(10,21,22,23,25)
  names(shapes) <- c("None",'Bug','Cen','Pap','Sin')
  
  #  colour  Same deal
  colours <- c('black', 'purple','green','red','Orange')
  names(colours) <- c("None",'Bug','Cen','Pap','Sin')
  
  # set the order of the inducers
  plant <- plant %>%
    filter(FOCAL != "Sil") %>%
    filter(INDUCER != "Sil")
  plant$INDUCER <- factor(plant$INDUCER, levels=c("None",'Bug','Cen','Pap','Sin'))
  
}

plant$SEED.LEAF.HEIGHT<- as.numeric(plant$SEED.LEAF.HEIGHT)
plant$INTERNODE.1 <- as.numeric(plant$INTERNODE.1)
plant$INTERNODE.3 <- as.numeric(plant$INTERNODE.3)
plant$INTERNODE.4 <- as.numeric(plant$INTERNODE.4)

theme_set(theme_classic()) # theme ggplot

summary(plant)

remove_commented_data <- FALSE

if(remove_commented_data) {
  sub_plant <- plant %>%
    filter(is.na(COMMENTS))
} else {
  sub_plant <- plant
}

sub_plant <- sub_plant[,-c(1, 4:9, 22)]

agg_plant <- sub_plant %>%
  mutate(Induction = ifelse(INDUCER == "None", "Not Induced", "Induced")) %>%
  mutate(Induction = factor(Induction, levels = c("Not Induced", "Induced")))

melt_agg_plant <- melt(agg_plant, id.vars = c("FOCAL", "INDUCER", "Induction")) %>%
  group_by(variable) %>%
  mutate(stand_val = (value - mean(value)) / sd(value))
colnames(melt_agg_plant) <- c("FOCAL", "INDUCER", "INDUCTION", "TRAIT", "VALUE", "STANDVAL")

filt_plant <- melt_agg_plant %>%
  filter(TRAIT %in% c("HEIGHT.CM", "NUMB.LEAVES", "SHAPE")) %>%
  mutate(FOCAL = toupper(FOCAL)) %>%
  mutate(INDUCER = toupper(INDUCER)) %>%
  mutate(INDUCER = factor(INDUCER, levels = c("NONE", "BUG", "CEN", "PAP", "SIN")))

plant_shape <- filt_plant %>%
  filter(TRAIT == "SHAPE")

shape_draws <- data.frame()
loo_data <- data.frame()
for(cur_foc in toupper(Foc)) {
  cur_shape <- plant_shape %>%
    filter(FOCAL == cur_foc)
  
  cur_fit <- brm(formula = VALUE ~ INDUCER, data = cur_shape, backend = "cmdstanr", family = gaussian)
  
  
  cur_loo <- loo(cur_fit)$estimates[3, 1:2]
  cur_loo <- as.data.frame(t(cur_loo))
  loo_data <- rbind(loo_data, cur_loo)
  
  cur_draws <- cur_fit %>%
    spread_draws(b_INDUCERBUG,
                 b_INDUCERCEN,
                 b_INDUCERPAP,
                 b_INDUCERSIN)
  
  colnames(cur_draws) <- c(".chain",
                           ".iteration",
                           ".draw",
                           "BUG",
                           "CEN",
                           "PAP",
                           "SIN")
  
  cur_draws$Focal <- cur_foc
  shape_draws <- rbind(shape_draws, cur_draws)
  
}

melt_shape_draws <- shape_draws %>%
  select(!c(".chain", ".iteration", ".draw")) %>%
  melt(id.vars = c("Focal")) %>%
  mutate(variable = case_when(variable == "BUG" ~ "B. arvensis",
                              variable == "CEN" ~ "C. cyanus",
                              variable == "PAP" ~ "P. rhoeas",
                              variable== "SIN" ~ "S. arvensis")) %>%
  mutate(variable = factor(variable, levels = rev(c("B. arvensis", "C. cyanus", "P. rhoeas", "S. arvensis")))) %>%
  mutate(Focal = case_when(Focal == "BUG" ~ "B. arvensis",
                           Focal == "CEN" ~ "C. cyanus",
                           Focal == "PAP" ~ "P. rhoeas",
                           Focal== "SIN" ~ "S. arvensis")) 

plShapeComp <- ggplot(melt_shape_draws, aes(x = value, y = variable, color = rev(variable))) +
  stat_pointinterval(.width = c(0.89, 0.95)) +
  facet_grid( ~ Focal, scales = "free_x") +
  geom_vline(xintercept = 0, alpha = 0.75, linetype = "dashed") +
  theme(text = element_text(size=10),
        legend.position = "none",
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(face = "italic"),
        strip.text = element_text(face = "italic"),
        legend.text=element_text(size = 15),
        strip.background = element_blank(),
        plot.caption = element_text(hjust = 0, face= "italic"),
        plot.title.position = "plot",
        plot.caption.position =  "plot") +
  ggtitle("(B)") +
  labs(x = "Estimated Change in Plant Shape from Neighbor Induction", y = "Inducing Species")
plShapeComp

### turning to leaf phenology analysis

leaf <- read.csv('Leaf.Pheno.data.June3.v2.csv', header = T, na.strings = "",stringsAsFactors = F)

Silene.include <- FALSE
## SPP inclusion
if(Silene.include){
  Foc<- c('Bug','Cen','Pap','Sil','Sin')  # focal species
  Ind<- c('None','Bug','Cen','Pap','Sil','Sin') # inducers
  
  # SHAPES of Points:  each species gets it own consistent point shape. 
  shapes <- c(10,21,22,23,24,25)
  names(shapes) <- c("None",'Bug','Cen','Pap','Sil','Sin')
  
  # colour  Same deal
  colours <- c('black', 'purple','green','red','blue','Orange')
  names(colours) <- c("None",'Bug','Cen','Pap','Sil','Sin')
  
  # set the order of the inducers
  leaf$INDUCER<-     factor(leaf$INDUCER, levels=c("None",'Bug','Cen','Pap','Sil','Sin'))
  
} else {  # exclude SILENE
  Foc<- c('Bug','Cen','Pap','Sin')  # focal species
  Ind<- c('None','Bug','Cen','Pap','Sin') # inducers
  
  shapes <- c(10,21,22,23,25)
  names(shapes) <- c("None",'Bug','Cen','Pap','Sin')
  
  #  colour  Same deal
  colours <- c('black', 'purple','green','red','Orange')
  names(colours) <- c("None",'Bug','Cen','Pap','Sin')
  
  # set the order of the inducers
  leaf <- leaf %>%
    filter(FOCAL != "Sil") %>%
    filter(INDUCER != "Sil")
  leaf$INDUCER <- factor(leaf$INDUCER, levels=c("None",'Bug','Cen','Pap','Sin'))
  
}

### NEED TO CONVERT SQUARES TO CENTIMETERS!!!

### DID SOME PRELIMINARY DATA CLEANING HERE BUT SHOULD CHECK WITH OTHERS

summary(leaf)

leaf <- leaf %>%
  filter(FOCAL != 0)

remove_commented_data <- FALSE

if(remove_commented_data) {
  sub_leaf <- leaf %>%
    filter(is.na(COMMENTS))
} else {
  sub_leaf <- leaf
}

sub_leaf <- sub_leaf[,-c(3:9, ncol(sub_leaf))]

lab_leaf <- sub_leaf %>%
  mutate(Label = paste0(FOCAL, " (", INDUCER, ")"))

summary(lab_leaf)

qual_leaf <- lab_leaf[,c(1:3, 7:8, ncol(lab_leaf))]
lab_leaf <- lab_leaf[,-c(7:8)]

melt_leaf <- melt(lab_leaf, id.vars = c("FOCAL", "INDUCER", "Label", "LEAF.TYPE")) %>%
  mutate(Induction = ifelse(INDUCER == "None", "Not Induced", "Induced"))

long_leaf <- melt_leaf %>%
  filter(LEAF.TYPE == "Longest")

leaf_area <- long_leaf %>%
  filter(variable == "TOTAL.AREA") %>%
  mutate(Induction = factor(Induction, levels = c("Not Induced", "Induced"))) %>%
  mutate(value = value * 0.4^2) %>% # convert from squares to centimeters
  mutate(FOCAL = toupper(FOCAL)) %>%
  mutate(INDUCER = toupper(INDUCER)) %>%
  mutate(INDUCER = factor(INDUCER, levels = c("NONE", "BUG", "CEN", "PAP", "SIN")))

area_draws <- data.frame()
loo_data <- data.frame()
for(cur_foc in toupper(Foc)) {
  cur_area <- leaf_area %>%
    filter(FOCAL == cur_foc)
  
  cur_fit <- brm(formula = value ~ INDUCER, data = cur_area, backend = "cmdstanr", family = lognormal)
  
  cur_loo <- loo(cur_fit)$estimates[3, 1:2]
  cur_loo <- as.data.frame(t(cur_loo))
  loo_data <- rbind(loo_data, cur_loo)
  
  cur_draws <- cur_fit %>%
    spread_draws(b_INDUCERBUG,
                 b_INDUCERCEN,
                 b_INDUCERPAP,
                 b_INDUCERSIN)
  
  colnames(cur_draws) <- c(".chain",
                           ".iteration",
                           ".draw",
                           "BUG",
                           "CEN",
                           "PAP",
                           "SIN")
  
  cur_draws$Focal <- cur_foc
  area_draws <- rbind(area_draws, cur_draws)
  
}

melt_area_draws <- area_draws %>%
  select(!c(".chain", ".iteration", ".draw")) %>%
  melt(id.vars = c("Focal")) %>%
  mutate(variable = case_when(variable == "BUG" ~ "B. arvensis",
                              variable == "CEN" ~ "C. cyanus",
                              variable == "PAP" ~ "P. rhoeas",
                              variable== "SIN" ~ "S. arvensis")) %>%
  mutate(variable = factor(variable, levels = rev(c("B. arvensis", "C. cyanus", "P. rhoeas", "S. arvensis")))) %>%
  mutate(Focal = case_when(Focal == "BUG" ~ "B. arvensis",
                           Focal == "CEN" ~ "C. cyanus",
                           Focal == "PAP" ~ "P. rhoeas",
                           Focal== "SIN" ~ "S. arvensis"))

plAreaComp <- ggplot(melt_area_draws, aes(x = value, y = variable, color = rev(variable))) +
  stat_pointinterval(.width = c(0.89, 0.95)) +
  facet_grid( ~ Focal, scales = "free_x") +
  geom_vline(xintercept = 0, alpha = 0.75, linetype = "dashed") +
  theme(text = element_text(size=10),
        legend.position = "none",
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(face = "italic"),
        strip.text = element_text(face = "italic"),
        legend.text=element_text(size = 15),
        strip.background = element_blank(),
        plot.caption = element_text(hjust = 0, face= "italic"),
        plot.title.position = "plot",
        plot.caption.position =  "plot") +
  ggtitle("(C)") +
  labs(x = expression("Estimated Change in Leaf Area from Neighbor Induction (cm" ^2 ~ ")"), y = "Inducing Species")
plAreaComp

############ HEIGHT ANALYSIS ######################## ######################## ######################## ########################

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

sub_data <- data

sub_data <- sub_data[,c(3:4, 18, 19, 20, 24)]
colnames(sub_data) <- c("Inducer",
                        "Focal",
                        "Height",
                        "June1st",
                        "June24th",
                        "Weight")
sub_data$Height <- as.numeric(sub_data$Height)
sub_data$Weight <- as.numeric(sub_data$Weight)


sub_data <- sub_data %>%
  mutate(Induction = ifelse(Inducer == "None",
                            "Not Induced",
                            "Induced"))

hw_data <- sub_data[,-c(4:5)] %>%
  melt(id.vars = c("Inducer", "Focal", "Induction")) %>%
  mutate(Induction = factor(Induction, levels = c("Not Induced", "Induced"))) %>%
  mutate(Inducer = toupper(Inducer)) %>%
  mutate(Inducer = factor(Inducer, levels = c("NONE", "BUG", "CEN", "PAP", "SIN"))) %>%
  mutate(Focal = toupper(Focal))

plant_height <- hw_data %>%
  filter(variable == "Height")

loo_data <- data.frame()
height_draws <- data.frame()
for(cur_foc in toupper(Foc)) {
  cur_height <- plant_height %>%
    filter(Focal == cur_foc)
  
  cur_fit <- brm(formula = value ~ Inducer, data = cur_height, backend = "cmdstanr", family = gaussian)
  #plot(cur_fit)
  
  cur_loo <- loo(cur_fit)$estimates[3, 1:2]
  cur_loo <- as.data.frame(t(cur_loo))
  loo_data <- rbind(loo_data, cur_loo)
  
  cur_draws <- cur_fit %>%
    spread_draws(b_InducerBUG,
                 b_InducerCEN,
                 b_InducerPAP,
                 b_InducerSIN)
  
  colnames(cur_draws) <- c(".chain",
                           ".iteration",
                           ".draw",
                           "BUG",
                           "CEN",
                           "PAP",
                           "SIN")
  
  cur_draws$Focal <- cur_foc
  height_draws <- rbind(height_draws, cur_draws)
  
}

melt_height_draws <- height_draws %>%
  select(!c(".chain", ".iteration", ".draw")) %>%
  melt(id.vars = c("Focal")) %>%
  mutate(variable = case_when(variable == "BUG" ~ "B. arvensis",
                              variable == "CEN" ~ "C. cyanus",
                              variable == "PAP" ~ "P. rhoeas",
                              variable== "SIN" ~ "S. arvensis")) %>%
  mutate(variable = factor(variable, levels = rev(c("B. arvensis", "C. cyanus", "P. rhoeas", "S. arvensis")))) %>%
  mutate(Focal = case_when(Focal == "BUG" ~ "B. arvensis",
                           Focal == "CEN" ~ "C. cyanus",
                           Focal == "PAP" ~ "P. rhoeas",
                           Focal== "SIN" ~ "S. arvensis")) 

plHeightComp <- ggplot(melt_height_draws, aes(x = value, y = variable, color = rev(variable))) +
  stat_pointinterval(.width = c(0.89, 0.95)) +
  facet_grid( ~ Focal, scales = "free_x") +
  geom_vline(xintercept = 0, alpha = 0.75, linetype = "dashed") +
  theme(text = element_text(size=10),
        legend.position = "none",
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(face = "italic"),
        strip.text = element_text(face = "italic"),
        legend.text=element_text(size = 15),
        strip.background = element_blank(),
        plot.caption = element_text(hjust = 0, face= "italic"),
        plot.title.position = "plot",
        plot.caption.position =  "plot") +
  ggtitle("(A)") +
  labs(x = "Estimated Change in Plant Height from Neighbor Induction (cm)", y = "Inducing Species")
plHeightComp

jpeg("./figs/FigTraits.jpeg",
     width = 2000, height = 2250, res = 300)
plot_grid(plHeightComp, plShapeComp, plAreaComp, nrow = 3)
dev.off()


