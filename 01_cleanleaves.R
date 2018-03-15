#Leaf damage analysis

#Load packages
##Data tidying
library(plyr)
library(dplyr)
library(tidyr)
##Assertions
library(assertr)
##Modelling
library(nlme)
library(lme4)
##Gettina an comparing r squared
library(piecewiseSEM)
##Model analysis and visualization
library(car)
library(visreg)

#Load data
distance <- read.csv("C://Users/Pierre/OneDrive/Projects/bromagro/Data/Distance.csv",
                     sep = ";")
damage <- read.csv("C://Users/Pierre/OneDrive/Projects/bromagro/Data/LeafDamage.csv",
                   sep = ";")
bromzy <- read.csv("C://Users/Pierre/OneDrive/Projects/bromagro/Data/Bromzy.csv", 
                   sep = ";")
##check data has been read correctly
str(distance)
str(damage)

# Getting distance ready --------------------------------------------------

#Tidying and assertions
str(distance)
##Remove the weird column
distance <- distance[,1:10]
str(distance)

##Missing bromeliad.
si <- c("ER", "ER", "ER", "ER")
sa <- c("B", "B", "B", "B")
da <- c("02.05.2017", "02.05.2017", "02.05.2017", "02.05.2017")
tree <- c("B3", "B3", "B3", "B3")
trea <- c("wr", "wr", "wr", "wr")
sq <- c("i", "ii", "iii", "iv")
br <- c("d", "d", "d", "d")
dis <- c(NA, NA, NA, NA)
dia <- c(NA, NA, NA, NA)
lo <- c(NA, NA, NA, NA)
missing_tree <- data.frame(si, sa, da, tree, trea, sq, br, dis, dia, lo)
names(missing_tree) <- names(distance)
distance <- rbind(distance, missing_tree)

##Correct wrong  quadrat distance
distance$Square[distance$Site == "DO" 
                & distance$Tree == "F23"
                & distance$Bromeliad_. == "b"
                &distance$Distance == 167] <- "ii"
distance$Square[distance$Site == "ER" 
                & distance$Tree == "M12"
                & distance$Bromeliad_. == "d"
                &distance$Distance == 201.9] <- "iii"

#Add new variables
##height and volume
distance <- 
  distance %>% 
  mutate(height = (Longest_leaf_length^2- (Diameter/2)^2)^0.5) %>% 
  mutate(volume = pi*((Diameter/2)^2)*height/3) 
##density index
###Create
calc_index <- 
  distance %>% 
  mutate(index = volume/Distance) %>% 
  mutate(largeleaf = Longest_leaf_length/Distance) %>% 
  dplyr::select(Site, Tree, Treatment, Square, index, largeleaf) %>% 
  group_by(Site, Tree, Treatment,Square) %>% 
  summarise_all(funs(sum))

###Bind
distance <- 
  distance %>%
  left_join(calc_index) %>% 
  rename(Bromeliad = Bromeliad_.)





# Getting damage ready ------------------------------------------------------

#Tidying & Assertions
str(damage)
##Coerce variables into their actual category
damage$Leaf <- as.character(damage$Leaf)
str(damage)

#Tree mistakes
###One extra tree
damage$Tree[damage$Tree == "Al1"] <- "AI1"
###Two trees with wrong name
levels(damage$Tree) <- c(levels(damage$Tree), "J13", "J19")
damage$Tree[damage$Tree == "J15"] <- "J13"
damage$Tree[damage$Tree == "J21"] <- "J19"
damage$Tree <- droplevels(damage$Tree)

###Some trees with more than one treatment
damage$Treatment[damage$Tree == "C3"] <- "w"
damage$Treatment[damage$Site == "DO" & damage$Tree == "G6"] <- "wr"
damage$Treatment[damage$Site == "CP" & damage$Tree == "G6"] <- "w"

#Bringing in new values
##Percentages
damage <- 
  damage %>% 
  mutate(okarea = Actual_area - Miner - Hemiptera_scraper) %>% 
  mutate(propdamage = 1- okarea/Original_area)
##Density Indices
leafdamage <- 
  damage %>%
  dplyr::select(-Analyst, -Day, -Miner, -Hemiptera_scraper) %>% 
  left_join(calc_index) %>% 
  filter(Tree != "B3")
leafdamage$index[which(leafdamage$Treatment == "wo")] <- 0  
leafdamage$index[which(leafdamage$Treatment == "wr" & leafdamage$Sampling == "A")] <- 0
leafdamage$largeleaf[which(leafdamage$Treatment == "wo")] <- 0  
leafdamage$largeleaf[which(leafdamage$Treatment == "wr" & leafdamage$Sampling == "A")] <- 0
###Unite tree and site otherwise R think there are only 3 sites
leafdamage <- 
  leafdamage %>% 
  unite(alltrees, Site, Tree, sep = "_", remove = F) %>% 
  unite(quadrats, alltrees, Square, sep = "_", remove = F)
###Checking
table(leafdamage$Square)
str(leafdamage)
leafdamage$alltrees <-as.factor(leafdamage$alltrees)
leafdamage$quadrats <-as.factor(leafdamage$quadrats)
##Bromeliad number and size
bromnumber <- 
  distance %>% 
  dplyr::select(Site, Tree, Longest_leaf_length, Bromeliad_.) %>% 
  unite(alltrees, Site, Tree, sep ="_")
bromnumber <- unique(bromnumber)
bromnumber$Bromeliad_. <- 1
bromnumber <- 
  bromnumber %>% 
  group_by(alltrees) %>% 
  summarise_each(funs(sum)) %>% 
  mutate(meansize = Longest_leaf_length/Bromeliad_.) %>% 
  dplyr::select(-Longest_leaf_length)
###Bind and update
leafdamage <- 
  leafdamage %>% 
  left_join(bromnumber)
leafdamage$Bromeliad_.[which(leafdamage$Treatment == "wo")] <- 0
leafdamage$meansize[leafdamage$Treatment == "wo"] <- 0
leafdamage$Bromeliad_.[which(leafdamage$Treatment == "wr" & leafdamage$Sampling == "A")] <- 0
leafdamage$meansize[leafdamage$Treatment == "wr" & leafdamage$Sampling == "A"] <- 0

##Make names better
names(leafdamage)[names(leafdamage) == "Bromeliad_."] <- "broms"
names(leafdamage)[names(leafdamage) == "Actual_area"] <- "actual"
names(leafdamage)[names(leafdamage) == "Original_area"] <- "original"

##Presence/absence
str(leafdamage)
leafdamage$presence <- "no"
leafdamage$presence[which(leafdamage$Treatment == "wr" & leafdamage$Sampling == "B")] <- "yes"
leafdamage$presence[which(leafdamage$Treatment == "w")] <- "yes"
leafdamage$presence <- as.factor(leafdamage$presence)

##Rechecking everything
leafdamage$alltrees <-as.factor(leafdamage$alltrees)
leafdamage$Tree <-as.factor(leafdamage$Tree)
leafdamage$Treatment <-as.factor(leafdamage$Treatment)
str(leafdamage)
leafdamage_noNAs <- na.exclude(leafdamage)

#Adding variables for additional models
##Damage difference
damagediff <- 
  leafdamage %>% 
  ungroup () %>% 
  dplyr::select(alltrees, Site, Sampling, Square, Leaf, propdamage) %>% 
  spread(key =Sampling, propdamage) %>% 
  mutate(damdiff = B - A) %>% 
  dplyr::select(-A, -B)
indexdiff <- 
  leafdamage %>% 
  ungroup()  %>%
  dplyr::select(alltrees, Site, Sampling, Square, Leaf, largeleaf) %>% 
  spread(key =Sampling, largeleaf) %>% 
  mutate(sizdiff = B - A) %>% 
  dplyr::select(-B, -A)
totaldiff <- 
  damagediff %>% 
  left_join(indexdiff)

# Pattern visualisation ---------------------------------------------------

##Everything
par(mfrow =c(1,2))
plot(leafdamage$propdamage ~ leafdamage$index)
plot(leafdamage$propdamage ~ leafdamage$largeleaf)
par(mfrow = c(1,1))

##B and A difference per treatment
par(mfrow = c(2,3))
plot(leafdamage$propdamage[leafdamage$Sampling == "B"
                          & leafdamage$Treatment == "wr"],
     ylim =c(0,1)) 
plot(leafdamage$propdamage[leafdamage$Sampling == "B"
                          & leafdamage$Treatment == "w"], 
     ylim =c(0,1)) 
plot(leafdamage$propdamage[leafdamage$Sampling == "B"
                          & leafdamage$Treatment == "wo"], 
     ylim =c(0,1)) 
plot(leafdamage$propdamage[leafdamage$Sampling == "A"
                          & leafdamage$Treatment == "wr"], 
     ylim =c(0,1))
plot(leafdamage$propdamage[leafdamage$Sampling == "A"
                          & leafdamage$Treatment == "w"], 
     ylim =c(0,1))
plot(leafdamage$propdamage[leafdamage$Sampling == "A"
                          & leafdamage$Treatment == "wo"],
     ylim =c(0,1))
par(mfrow = c(1,1))
###Same but removing 0s
par(mfrow = c(2,3))
plot(leafdamage$propdamage[leafdamage$Sampling == "B"
                           & leafdamage$Treatment == "wr"
                           & leafdamage$propdamage > 0], 
     ylim =c(0,1)) 
plot(leafdamage$propdamage[leafdamage$Sampling == "B"
                           & leafdamage$Treatment == "w"
                           & leafdamage$propdamage > 0],
     ylim =c(0,1)) 
plot(leafdamage$propdamage[leafdamage$Sampling == "B"
                           & leafdamage$Treatment == "wo"
                           & leafdamage$propdamage > 0], 
     ylim =c(0,1)) 
plot(leafdamage$propdamage[leafdamage$Sampling == "A"
                           & leafdamage$Treatment == "wr"
                           & leafdamage$propdamage > 0], 
     ylim =c(0,1))
plot(leafdamage$propdamage[leafdamage$Sampling == "A"
                           & leafdamage$Treatment == "w"
                           & leafdamage$propdamage > 0],
     ylim =c(0,1))
plot(leafdamage$propdamage[leafdamage$Sampling == "A"
                           & leafdamage$Treatment == "wo"
                           & leafdamage$propdamage > 0], 
     ylim =c(0,1))
par(mfrow =c(1,1))

##Looking at means

mean(leafdamage_noNAs$propdamage[leafdamage_noNAs$propdamage > 0])
mean(leafdamage_noNAs$propdamage)

##looking at distributions
par(mfrow =c(2,1))
hist(leafdamage$propdamage[leafdamage$propdamage > 0])
par(mfrow =c(1,1))

##Is there an original pattern
plot(leafdamage$propdamage[leafdamage$Sampling == "B" 
                           & leafdamage$Treatment == c("wr", "w")] ~
       leafdamage$largeleaf[leafdamage$Sampling == "B" 
                            & leafdamage$Treatment == c("wr", "w")])
plot(propdamage[leafdamage$Sampling == "B"
                & leafdamage$Treatment == "w"] ~ 
       largeleaf[leafdamage$Sampling == "B"
                 & leafdamage$Treatment == "w"],
     xlab = "proximity index", 
     ylab = "leaf damage", 
     data = leafdamage) 

##lets plot everything
##B
plot(leafdamage$largeleaf[leafdamage$Sampling == "B"],
     leafdamage$propdamage[leafdamage$Sampling == "B"],
     col = c("red", "blue", "green")[factor(leafdamage$Treatment[leafdamage$Sampling == "B"])],
     pch = c(17))
##A
plot(leafdamage$largeleaf[leafdamage$Sampling == "A"],
     leafdamage$propdamage[leafdamage$Sampling == "A"],
     col = c("red", "blue", "green")[factor(leafdamage$Treatment)[leafdamage$Sampling == "A"]],
     pch = c(17))

##How many 0s total
nrow(leafdamage_noNAs %>% 
       filter(propdamage == 0))

nrow(leafdamage_noNAs %>% filter(propdamage == 0))/nrow(leafdamage_noNAs)
nrow(leafdamage_noNAs %>% 
       filter(propdamage < 0.05))
# Models with % damage pooled---------------------------------------------------
#Data
testmodel <- 
  leafdamage_noNAs %>% 
  filter(Site =="CP")
poolmodel <- 
  leafdamage_noNAs %>% 
  dplyr::select(Site, alltrees, Sampling, quadrats, Treatment, propdamage, largeleaf, broms, meansize, presence) %>% 
  group_by(Site, alltrees, Sampling, quadrats, Treatment, presence) %>% 
  summarise_all(funs(mean))
pooltest <-
  poolmodel %>% 
  dplyr::filter(Site =="CP")

#Base
##largeleaf
leafmodel_largeleaf <- 
  glmer(propdamage+0.01 ~ 
          largeleaf*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="Gamma"(link="log"),
        data = poolmodel)
plot(leafmodel_largeleaf)
qqnorm(residuals(leafmodel_largeleaf))
qqline(residuals(leafmodel_largeleaf), col = "red")
summary(leafmodel_largeleaf)
Anova(leafmodel_largeleaf)
##nestindex
leafmodel_nestindex<- 
  glmer(propdamage+0.01 ~ 
          nestindex*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="Gamma"(link="log"),
        data = poolinside)
plot(leafmodel_nestindex)
qqnorm(residuals(leafmodel_nestindex))
qqline(residuals(leafmodel_nestindex), col = "red")
summary(leafmodel_nestindex)
Anova(leafmodel_nestindex)
##nestabund
leafmodel_nestabund<- 
  glmer(propdamage+0.01 ~ 
          log(nestabund+1)*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="Gamma"(link="inverse"),
        data = poolinside)
plot(leafmodel_nestabund)
qqnorm(residuals(leafmodel_nestabund))
qqline(residuals(leafmodel_nestabund), col = "red")
summary(leafmodel_nestabund)
Anova(leafmodel_nestabund)



#Treatment
leafmodelb <- 
  glmer(propdamage+0.01 ~ 
          Treatment*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="Gamma"(link="inverse"),
        data = poolmodel)
plot(leafmodelb)
qqnorm(residuals(leafmodelb))
qqline(residuals(leafmodelb), col ="red")
summary(leafmodelb)
Anova(leafmodelb)
rsquared(leafmodelb)


#Bromeliad number
leafmodelc <- 
  glmer(propdamage+0.01 ~ 
          broms*Sampling +
          (1|Site/alltrees/quadrats),
        family ="Gamma"(link="log"),
        data = poolmodel)
plot(leafmodelc)
qqnorm(residuals(leafmodelc))
qqline(residuals(leafmodelc), col ="red")
summary(leafmodelc)
Anova(leafmodelc) 


#bromeliad total volume
leafmodeld <- 
  glmer(propdamage+0.01 ~ 
          log(broms*meansize +1)*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="Gamma"(link="log"),
        data = poolmodel)
plot(leafmodeld)
qqnorm(residuals(leafmodeld))
qqline(residuals(leafmodeld), col ="red")
summary(leafmodeld)
Anova(leafmodeld) 


#Just with bromeliad presence, absence
leafmodele <- 
  glmer(propdamage + 0.01~ 
          presence*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="Gamma"(link="log"),
        data = poolmodel)
plot(leafmodele)
qqnorm(residuals(leafmodele))
qqline(residuals(leafmodele), col ="red")
summary(leafmodele)
Anova(leafmodele)




# Model Testing Binomial on damage presence----------------------------------------------------------------
#largeleaf
leafmodel_dampres <- 
  glmer(round(propdamage+0.49999) ~ 
          largeleaf*Sampling + (1|Site/alltrees/Square), 
        family ="binomial"(link ="probit"),
        data = leafdamage_noNAs)
plot(leafmodel_dampres)
summary(leafmodel_dampres)
Anova(leafmodel_dampres)
#presence
leafmodel_damprespresence <- 
  glmer(round(propdamage+0.49999) ~ 
          presence*Sampling + (1|Site/alltrees/Square), 
        family ="binomial"(link ="probit"),
        data = leafdamage_noNAs)
plot(leafmodel_damprespresence)
summary(leafmodel_damprespresence)
Anova(leafmodel_damprespresence)
#numbers
leafmodel_dampresnumber <- 
  glmer(round(propdamage+0.49999) ~ 
          broms*Sampling + (1|Site/alltrees/Square), 
        family ="binomial"(link ="probit"),
        data = leafdamage_noNAs)
plot(leafmodel_dampresnumber)
summary(leafmodel_dampresnumber)
Anova(leafmodel_dampresnumber)
#nestindex
leafmodel_dampres_nestindex <- 
  glmer(round(propdamage+0.49999) ~ 
          log(nestindex+0.001)*Sampling + (1|Site/alltrees/Square), 
        family ="binomial"(link ="probit"),
        data = insidepred_noNAs)
plot(leafmodel_dampres_nestindex)
summary(leafmodel_dampres_nestindex)
Anova(leafmodel_dampres_nestindex)
#nestabund
leafmodel_dampres_nestabund <- 
  glmer(round(propdamage+0.49999) ~ 
          log(nestabund+1)*Sampling + (1|Site/alltrees/Square), 
        family ="binomial"(link ="probit"),
        data = insidepred_noNAs)
plot(leafmodel_dampres_nestabund)
summary(leafmodel_dampres_nestabund)
Anova(leafmodel_dampres_nestabund)


# Damage difference model -----------------------------------------------
leafmodel_diff<- 
  lmer(damdiff^0.3 ~ 
         sizdiff +
         (1|Site/alltrees/Square), 
       data = totaldiff, na.action = na.exclude)  
plot(leafmodel_diff)
qqnorm(residuals(leafmodel_diff))
qqline(residuals(leafmodel_diff), col ="red")
summary(leafmodel_diff)
Anova(leafmodel_diff)






# What if we reverse the logic -------------------------------------------
leafmodel_reversea <- 
  glmmPQL(1-propdamage ~ 
          largeleaf*Sampling,
          random = ~1|Site/alltrees/quadrats, 
        family ="quasibinomial"(link ="probit"),
        data = poolmodel)
plot(leafmodel_reversea)
qqnorm(residuals(leafmodel_reversea))
qqline(residuals(leafmodel_reversea), col ="red")
summary(leafmodel_reversea)
Anova(leafmodel_reversea)

# Model with area ----------------------------------------------------
leafmodel_area <- 
  glmer.nb(round((original-okarea)*100)~ 
             log(original) +
             (1|alltrees/Square), 
           data = testmodel)
qqnorm(residuals(leafmodel_area))
qqline(residuals(leafmodel_area), col = "red")
plot(leafmodel_area)
summary(leafmodel_area)
Anova(leafmodel_area)

rsquared(list(leafmodel_area))


