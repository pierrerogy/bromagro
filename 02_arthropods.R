#Arthropod analysis

#Load packages
##Data tidying
library(plyr)
library(dplyr)
library(tidyr)
##Assertions
library(assertr)
##Diversity
library(vegan)
##Modelling
library(lme4)
##Model analysis and visualization
library(car)
library(visreg)

#Load and check data
lamona <- read.csv("C://Users/Pierre/OneDrive/Projects/bromagro/Data/LaMona.csv",
                   sep = ";")
bromzy <- read.csv("C://Users/Pierre/OneDrive/Projects/bromagro/Data/Bromzy.csv", 
                   sep = ";")
observation <- read.csv("C://Users/Pierre/OneDrive/Projects/bromagro/Data/Observation.csv", 
                        sep = ";")
str(lamona)
str(bromzy)
str(observation)


# Getting lamona ready ----------------------------------------------------

#Tidying & Assertions
str(lamona)

##Removing weird column
lamona <-  lamona[,1:17]
##Remove things that have nothing to do with the analysis, eg mozzies....
lamona <- lamona[!lamona$Species == "Remove",]
lamona <- lamona[!lamona$Species == "Remove?",]
##Just add nada for ootheca feeding behaviour
lamona$Diet[which(lamona$Subfamily == "Ootheca")] <- "nada"
lamona$Diet <- droplevels(lamona$Diet)
##Putting Acari as Order name for mites, even though it is not
levels(lamona$Order) <- c(levels(lamona$Order), "Acari")
lamona$Order[which(lamona$Order == "")] <- "Acari"
lamona$Order <- droplevels(lamona$Order)
##Add 1 where I forgot to
lamona$Abundance[which(is.na(lamona$Abundance))] <- 1
##recheck
str(lamona)

##Combine specimens per quadrat based on morphospecies
puramona <- 
  lamona %>% 
  dplyr::select(Order, Suborder, Family, Morphospecies, Diet, Site, Sampling, Tree, Treatment, Square, Abundance) %>% 
  unite(alltrees, Site, Tree, sep ="_", remove =F) %>% 
  unite(quadrats, alltrees, Square, sep ="_", remove = F) %>% 
  group_by(Order, Suborder, Family, Morphospecies, Diet, Site, alltrees, quadrats, Sampling, Tree, Treatment, Square) %>% 
  summarise_all(funs(sum))

##Now let us start assertions
is.integer(lamona$Abundance)
stopifnot(lamona$Abundance > 0)
##Trees
tidytest <- 
  lamona %>% 
  ungroup() %>% 
  dplyr::select(Site, Tree, Treatment)
tidytest <- (unique(tidytest))
nrow(tidytest)
###One tree with two treatments, 119 because CPA2 missing
lamona$Treatment[which(lamona$Tree == "D18")] <- "wo"
tidytest <- 
  lamona %>% 
  ungroup() %>% 
  dplyr::select(Site, Tree, Treatment)
tidytest <- (unique(tidytest))
nrow(tidytest)


##Making sure we have the right number of tree in all treatments
tidytest <- 
  lamona %>% 
  ungroup() %>% 
  dplyr::select(Site, Tree, Treatment) %>% 
  filter(Treatment == "wo")
nrow(unique(tidytest))

###60, so good because we had an extra wo at ER and did not do A2A
###so did not vacuum for A
tidytest <- 
  lamona %>%
  ungroup() %>% 
  dplyr::select(Site, Tree, Treatment) %>% 
  filter(Treatment == "wo" & Site == "ER")
nrow(unique(tidytest))
tidytest <- 
  lamona %>%
  ungroup() %>% 
  dplyr::select(Site, Tree, Treatment) %>% 
  filter(Treatment == "wo" & Site == "CP")
nrow(unique(tidytest)) ###all good on this side
###Just making sure the number is right for another treatment
tidytest <- 
  lamona %>% 
  ungroup() %>% 
  dplyr::select(Site, Tree, Treatment) %>% 
  filter(Treatment == "wr")
nrow(unique(tidytest))



##Now check if we have the two sampling with same number of trees
tidytest <- 
  lamona %>%
  ungroup() %>% 
  dplyr::select(Site, Sampling, Tree, Treatment) 
nrow(unique(tidytest))
####238 total, so good, but is it 119/119?
tidytest <- 
  lamona %>% 
  ungroup() %>% 
  dplyr::select(Site, Sampling, Tree, Treatment) %>%
  filter(Sampling == "B")
nrow(unique(tidytest))


##Square
tidytest <- 
  lamona %>% 
  ungroup() %>% 
  unite(alltrees, Site, Tree, Sampling, sep="_") %>% 
  dplyr::select(alltrees, Square)
tidytest <- unique(tidytest)
nrow(tidytest)
ee <- table(tidytest$alltrees) 
print(which(ee != 4))




###Missing sample vector
sit <- c("CP", "CP", "CP", "CP", "CP", "DO", "DO", "DO", "DO", "DO", "DO", "DO", "DO", "DO", "DO", "DO", "ER", "ER", "ER", "ER", "ER", "ER", "ER", "ER")
sam <- c("B", "A", "A","B", "A", "B", "B", "B", "B", "A", "A", "A", "B", "A", "B", "A","B", "B", "A", "B", "B", "A", "B", "B")
tree <- c("G1", "I1", "J1", "J6", "L10", "A10", "C16", "C8", "F10", "G9", "H5", "H7", "H7", "H9", "I1", "I4", "B3", "B3", "C3", "G6", "J13", "O9", "S11", "S11")
trea <- c("wo", "wo", "wr", "wr", "wo", "wo", "wo", "wo", "wo", "wo", "w", "wr", "wr", "wo", "w", "wo", "wr", "wr", "w", "wo", "wo", "wr", "wo", "wo")
squ <- c("iv", "ii", "iii", "ii", "iv", "ii", "ii", "iv", "ii", "ii", "ii", "ii", "iv", "ii", "iii", "iv", "iii", "iv", "iii", "iii", "iv", "iii", "i", "ii")
empty_samples <- data.frame(cbind(sit, sam, tree, trea, squ))
empty_samples <- 
  empty_samples %>% 
  unite(alltrees, sit, tree, sep= "_", remove = F) %>% 
  dplyr::select(sit, sam, trea, alltrees, squ)
###just making sure it is right
tidytest <- 
  lamona %>% 
  ungroup() %>% 
  dplyr::select(Site, Sampling, Tree, Treatment, Square)
tidytest <- unique(tidytest)
nrow(tidytest)
colnames(empty_samples) <- colnames(tidytest)
tidytest <- 
  rbind(tidytest, empty_samples)
tidytest <- 
  tidytest %>% 
  ungroup() %>% 
  unite(alltrees, Site, Tree, Sampling, sep="_") %>% 
  dplyr::select(alltrees, Square)
table(tidytest$Square) 
table(tidytest$alltrees)
print(which(table(tidytest$alltrees) != 4))
str(puramona)

##bind index
calc_index <- 
  calc_index %>% 
  dplyr::select(-index)
puramona <- 
  puramona %>% 
  left_join(calc_index)
puramona$largeleaf[which(puramona$Treatment == "wo")] <- 
  0  
puramona$largeleaf[which(puramona$Treatment == "wr" & puramona$Sampling == "A")] <- 
  0
#bind pred stuff
puramona <- 
  puramona %>% 
  left_join(poolinside[,-7])

##spreading
puramona_spread_species <- 
  na.omit(
  puramona %>% 
  ungroup() %>% 
  dplyr::select(Morphospecies, Site, alltrees, quadrats, Sampling, Treatment, largeleaf, Abundance) %>% 
  group_by(Morphospecies, Site, alltrees, quadrats, Sampling, Treatment, largeleaf) %>% 
  summarise_all(funs(sum)) %>% 
  spread(key = Morphospecies, Abundance, fill =0))

puramona_spread_diet <- 
  na.omit(
  puramona %>% 
  ungroup() %>% 
  dplyr::select(Diet, Site, alltrees, quadrats, Sampling, Treatment, largeleaf, Abundance) %>% 
  group_by(Diet, Site, alltrees, quadrats, Sampling, Treatment, largeleaf) %>% 
  summarise_all(funs(sum)) %>% 
  spread(key = Diet, Abundance, fill =0))


# Getting bromzy ready ----------------------------------------------------
str(bromzy)
#Remove things that have nothing to do with the analysis, eg aquatic stuff
bromzy <- bromzy[!bromzy$Species == "Remove",]
bromzy <- bromzy[!bromzy$Species == "Remove?",]
##Just add nada for ootheca feeding behaviour
bromzy$Diet[which(bromzy$Subfamily == "Ootheca")] <- "nada"
bromzy$Diet <- droplevels(bromzy$Diet)
##Additional level in nest factor
levels(bromzy$Abundance)
bromzy$Abundance[which(bromzy$Abundance == "nest ")] <- "nest"
bromzy$Abundance <- droplevels(bromzy$Abundance)
##Add 1 where I forgot to
bromzy$Abundance[bromzy$Abundance == ""] <- 1
bromzy$Abundance[bromzy$Abundance == " "] <- 1
bromzy$Abundance <- droplevels(bromzy$Abundance)
##recheck
str(bromzy)
levels(bromzy$Tree)
levels(bromzy$Tree) <- c(levels(bromzy$Tree), "J5")
bromzy$Tree[bromzy$Tree == "J5 "] <- "J5"
bromzy$Tree <- droplevels(bromzy$Tree)
##Removing rows with individual ant counts when nest has already been counted
###probably a morphospecies oversplit
bromzy <- 
  bromzy[!(bromzy$Abundance== 16 & bromzy$Tree== "B3" & bromzy$Morphospecies =="Ant_A"),]
bromzy <- 
  bromzy[!(bromzy$Abundance== 1 & bromzy$Tree== "E8" & bromzy$Morphospecies =="Ant_X"),]
bromzy <- 
  bromzy[!(bromzy$Abundance== 1 & bromzy$Tree== "O18" & bromzy$Morphospecies =="Ant_X"),]

###Making sure we have 58 trees, because only brom on DOD7 was empty
tidytest <- 
  bromzy %>% 
  ungroup() %>% 
  unite(alltrees, Site, Tree, sep ="_") %>% 
  dplyr::select(alltrees)
tidytest <- unique(tidytest)

###Counting how many bromeliads we have here
tidytest2 <- 
  bromzy %>% 
  ungroup() %>% 
  unite(allbroms, Site, Tree, Bromeliad, sep ="_", remove =F) %>% 
  dplyr::select(Site, Tree, allbroms)
tidytest2 <- unique(tidytest2)
nrow(tidytest2)
tidytest <- 
  distance %>% 
  unite(allbromies, Site, Tree, Bromeliad_., sep= "_", remove = F) %>% 
  dplyr::select(Site, Tree, allbromies)
tidytest <- unique(tidytest)
nrow(tidytest)
table(tidytest$Tree)
table(tidytest2$Tree)
###Creating frame for empty ones
####F14a NA BECAUSE FELL FROM TREE
sit <- c("DO", "ER", "CP", "CP", "DO")
sam <- c("B", "A", "B", "B", "B")
tree <-c("E11", "F14", "J1", "J2", "D7") 
trea <- c("wr", "w", "wr", "wr", "wr")
brom <- c("e", "a", "b", "c", "a")
div <- c(0, NA, 0, 0, 0)
empty_broms <- rbind(sit, sam, tree, trea, brom, div)

#Final frame
##Add nest category
###Create column and add approximate nest size levels
bromzy$nestcat <- NA
levels(bromzy$Abundance) <- c(levels(bromzy$Abundance), 0)
bromzy$Abundance[bromzy$Abundance == "nest "] <- "nest"
levels(bromzy$Abundance) <-  c(levels(bromzy$Abundance), 50, 100, 200, 1000)
###Approximate nest size
bromzy$Abundance[which(bromzy$Abundance == "nest" & bromzy$Morphospecies == "Ant_X")] <- "50"
bromzy$Abundance[which(bromzy$Abundance == "nest" & bromzy$Morphospecies == "Ant_E")] <- "50"
bromzy$Abundance[which(bromzy$Abundance == "nest" & bromzy$Morphospecies == "Ant_AA")] <- "50"
bromzy$Abundance[which(bromzy$Abundance == "nest" & bromzy$Morphospecies == "Ant_BF")] <- "100"
bromzy$Abundance[which(bromzy$Abundance == "nest" & bromzy$Morphospecies == "Ant_AK")] <- "50"
bromzy$Abundance[which(bromzy$Abundance == "nest" & bromzy$Morphospecies == "Ant_AB")] <- "100"
bromzy$Abundance[which(bromzy$Abundance == "nest" & bromzy$Morphospecies == "Ant_AS")] <- "100"
bromzy$Abundance[which(bromzy$Abundance == "nest" & bromzy$Morphospecies == "Ant_H")] <- "100"
bromzy$Abundance[which(bromzy$Abundance == "nest" & bromzy$Morphospecies == "Ant_F")] <- "100"
bromzy$Abundance[which(bromzy$Abundance == "nest" & bromzy$Morphospecies == "Ant_AH")] <- "100"
bromzy$Abundance[which(bromzy$Abundance == "nest" & bromzy$Morphospecies == "Ant_BD")] <- "100"
bromzy$Abundance[which(bromzy$Abundance == "nest" & bromzy$Morphospecies == "Ant_A")] <- "200"
bromzy$Abundance[which(bromzy$Abundance == "nest" & bromzy$Morphospecies == "Ant_G")] <- "200"
bromzy$Abundance[which(bromzy$Abundance == "nest" & bromzy$Morphospecies == "Ant_Z")] <- "200"
bromzy$Abundance[which(bromzy$Abundance == "100" & bromzy$Morphospecies == "Ant_AH" & bromzy$Tree =="G11" & bromzy$Bromeliad == "c")] <- "1000"
###add nest category
bromzy$nestcat[which(bromzy$Abundance == "50")] <- "A"
bromzy$nestcat[which(bromzy$Abundance == "100")] <- "B"
bromzy$nestcat[which(bromzy$Abundance == "200")] <- "C"
bromzy$nestcat[which(bromzy$Abundance == "1000")] <- "W"
##finishing it
bromzy$Abundance <-  as.numeric(as.character(bromzy$Abundance))
bromzy$nestcat <-  as.factor(bromzy$nestcat)
str(bromzy)
##Frame and correct alter ego nests
purabromzy <- 
  bromzy %>% 
  unite(alltrees, Site, Tree, sep ="_", remove =F) %>% 
  dplyr::select(Order, Suborder, Family, Site, Sampling, alltrees, Tree, Treatment, Bromeliad, nestcat,Morphospecies, Diet, Abundance) %>% 
  group_by(Order, Suborder, Family, Site, Sampling, alltrees, Tree, Treatment, Bromeliad, nestcat, Morphospecies, Diet) %>% 
  summarise_all(funs(sum))
purabromzy$Abundance[purabromzy$Abundance == 600] <- 400
##Remove ant nests
nonestbromzy <- 
  purabromzy %>% 
  filter(is.na(nestcat)) 

# Making damage bromeliad predators inside frame --------------------------
#without nests
nonestcalc <- 
  purabromzy %>% 
  ungroup() %>% 
  filter(is.na(nestcat)) %>% 
  filter(Diet == "pred") %>% 
  dplyr::select(Site, Tree, Bromeliad, Abundance) %>%
  group_by(Site, Tree, Bromeliad) %>%
  summarise_all(funs(sum)) %>% 
  rename(nonestabund = Abundance)
 

#with nests
insidepred <-
  purabromzy %>% 
  ungroup() %>% 
  filter(Diet == "pred") %>% 
  dplyr::select(Site, Tree, Bromeliad, Abundance) %>%
  group_by(Site, Tree, Bromeliad) %>%
  summarise_all(funs(sum)) %>% 
  left_join(distance) %>% 
  left_join(nonestcalc) %>% 
  mutate(nestindex = Abundance/Distance) %>% 
  mutate(nonestindex = nonestabund/Distance) %>% 
  rename(nestabund = Abundance) %>% 
  dplyr::select(Site, Tree, Treatment, Square, nestabund, nestindex, nonestabund,nonestindex) %>% 
  group_by(Site, Tree, Treatment, Square) %>% 
  summarise_all(funs(sum)) %>% 
  unite(alltrees, Site, Tree, sep= "_", remove = F) %>% 
  unite(quadrats, alltrees, Square, sep= "_", remove = F)

#merging with damage
insidepred <- 
  leafdamage[,1:13] %>% 
  left_join(insidepred)
insidepred$nestabund[which(insidepred$Treatment == "wo")] <-
  0
insidepred$nestindex[insidepred$Treatment == "wo"] <-
  0
insidepred$nonestabund[which(insidepred$Treatment == "wo")] <-
  0
insidepred$nonestindex[insidepred$Treatment == "wo"] <-
  0
insidepred$nestabund[which(insidepred$Treatment == "wr" & insidepred$Sampling == "A")] <- 
  0
insidepred$nestindex[insidepred$Treatment == "wr" & insidepred$Sampling == "A"] <- 
  0
insidepred$nonestabund[which(insidepred$Treatment == "wr" & insidepred$Sampling == "A")] <- 
  0
insidepred$nonestindex[insidepred$Treatment == "wr" & insidepred$Sampling == "A"] <- 
  0
insidepred <- 
  cbind(insidepred, leafdamage$largeleaf)
insidepred <- 
  insidepred %>% 
  rename(largeleaf = `leafdamage$largeleaf`)
##pooling by quadrat
insidepred_noNAs <- 
  na.omit(insidepred)
poolinside <- 
  insidepred_noNAs %>% 
  dplyr::select(Site, alltrees, Sampling, quadrats, Treatment, propdamage, largeleaf, nestabund, nestindex, nonestabund,nonestindex) %>% 
  group_by(Site, alltrees, Sampling, quadrats, Treatment) %>% 
  summarise_all(funs(mean))


# Getting observation ready -----------------------------------------------
str(observation)
#Removing weird columns
observation <- observation[,1:16]
observation$Abundance <- 1

observation_adonis <- 
  observation %>% 
  filter(Site == "CP") %>% 
  select(Site, Time, Tree, Bromeliads, Species, Abundance) %>% 
  group_by(Site, Time, Tree, Bromeliads, Species, Abundance) %>% 
  summarise_all(funs(sum)) %>% 
  spread(key=Species, Abundance, fill =0)
dist <- vegdist(observation_adonis[,6:268], method="bray", binary=FALSE, diag=FALSE, upper=FALSE,
                na.rm = FALSE)
observation_adonis$Time[which(observation_adonis$Time == "Afternoon")] <- "Morning"
observation_adonis <- droplevels(observation_adonis)
observation_adonis <- na.exclude(observation_adonis)
adonis(dist ~ Bromeliads + Time + Time*Bromeliads, 
       data = observation, permutations = 999, method = "bray")


# Lamona sample-based curves ------------------------------------------------------
pdf("rarefaction_mona.pdf", height = 25, width = 30)
par(mfrow = c(3,3), mar = c(5.1, 5.1, 4.1, 2.1))

##Overall
###CP
raremonaCP <- 
  puramona %>% 
  ungroup() %>% 
  filter(Site == "CP") %>% 
  dplyr::select(Site, alltrees, Square, Sampling, Morphospecies, Abundance) %>% 
  unite(quadrat, alltrees, Square, Sampling, sep ="_", remove =F) %>% 
  group_by(Site, alltrees, Square, quadrat, Sampling, Morphospecies) %>% 
  summarise_all(funs(sum)) %>% 
  spread(key = Morphospecies, Abundance, fill =0)
plot(specaccum(raremonaCP[,6:800]), 
     ci.type = "poly", ci.col = "lightblue",
     col ="blue",lwd = 2, ci.lty = 0, xlim = c(0,350), ylim=c(0,1000), 
     main = "CP", xlab= "", ylab ="Overall", cex.lab =2, cex.main =2)
###DO
raremonaDO <- 
  puramona %>% 
  ungroup() %>% 
  filter(Site == "DO") %>% 
  dplyr::select(Site, alltrees, Square, Sampling, Morphospecies, Abundance) %>% 
  unite(quadrat, alltrees, Square, Sampling, sep ="_", remove =F) %>% 
  group_by(Site, alltrees, Square, quadrat, Sampling, Morphospecies) %>% 
  summarise_all(funs(sum)) %>% 
  spread(key = Morphospecies, Abundance, fill =0)
plot(specaccum(raremonaDO[,6:506]), ci.type = "poly", ci.col = "lightblue",
     col ="blue",lwd = 2, ci.lty = 0, xlim = c(0,350), ylim=c(0,1000),
     ylab ="", cex.lab =2, xlab= "", main = "DO", cex.main =2)
###ER
raremonaER <- 
  puramona %>% 
  ungroup() %>% 
  filter(Site == "ER") %>% 
  dplyr::select(Site, alltrees, Square, Sampling, Morphospecies, Abundance) %>% 
  unite(quadrat, alltrees, Square, Sampling, sep ="_", remove =F) %>% 
  group_by(Site, alltrees, Square, quadrat, Sampling, Morphospecies) %>% 
  summarise_all(funs(sum)) %>% 
  spread(key = Morphospecies, Abundance, fill =0)
plot(specaccum(raremonaER[,6:613]), ci.type = "poly", ci.col = "lightblue",
     col ="blue",lwd = 2, ci.lty = 0, xlim = c(0,350), ylim=c(0,1000),
     xlab= "", ylab ="", cex.main =2, main ="ER")


##Before
###CP
raremonaCPB <- 
  puramona %>% 
  ungroup() %>% 
  filter(Site == "CP" & Sampling =="B") %>% 
  dplyr::select(Site, alltrees, Square, Sampling, Morphospecies, Abundance) %>% 
  unite(quadrat, alltrees, Square, sep ="_", remove =F) %>% 
  group_by(Site, alltrees, Square, quadrat, Sampling, Morphospecies) %>% 
  summarise_all(funs(sum)) %>% 
  spread(key = Morphospecies, Abundance, fill =0)
plot(specaccum(raremonaCPB[,6:405]), ci.type = "poly", ci.col = "lightblue",
     col ="blue",lwd = 2, ci.lty = 0, xlim = c(0,350), ylim=c(0,1000),
     cex.lab =2, ylab ="Before", xlab = "")
###DO
raremonaDOB <- 
  puramona %>% 
  ungroup() %>% 
  filter(Site == "DO" & Sampling =="B") %>% 
  dplyr::select(Site, alltrees, Square, Sampling, Morphospecies, Abundance) %>% 
  unite(quadrat, alltrees, Square, sep ="_", remove =F) %>% 
  group_by(Site, alltrees, Square, quadrat, Sampling, Morphospecies) %>% 
  summarise_all(funs(sum)) %>% 
  spread(key = Morphospecies, Abundance, fill =0)
plot(specaccum(raremonaDOB[,6:305]), ci.type = "poly", ci.col = "lightblue",
     col ="blue",lwd = 2, ci.lty = 0, xlim = c(0,350), ylim=c(0,1000),
     xlab= "", ylab ="")
###ER
raremonaERB <- 
  puramona %>% 
  ungroup() %>% 
  filter(Site == "ER" & Sampling =="B") %>% 
  dplyr::select(Site, alltrees, Square, Sampling, Morphospecies, Abundance) %>% 
  unite(quadrat, alltrees, Square, sep ="_", remove =F) %>% 
  group_by(Site, alltrees, Square, quadrat, Sampling, Morphospecies) %>% 
  summarise_all(funs(sum)) %>% 
  spread(key = Morphospecies, Abundance, fill =0)
plot(specaccum(raremonaERB[,6:316]), ci.type = "poly", ci.col = "lightblue",
     col ="blue",lwd = 2, ci.lty = 0, xlim = c(0,350), ylim=c(0,1000),
     xlab= "", ylab ="", cex.lab =2)

##After
###CP
raremonaCPA <- 
  puramona %>% 
  ungroup() %>% 
  filter(Site == "CP" & Sampling =="A") %>% 
  dplyr::select(Site, alltrees, Square, Sampling, Morphospecies, Abundance) %>% 
  unite(quadrat, alltrees, Square, sep ="_", remove =F) %>% 
  group_by(Site, alltrees, Square, quadrat, Sampling, Morphospecies) %>% 
  summarise_all(funs(sum)) %>% 
  spread(key = Morphospecies, Abundance, fill =0)
plot(specaccum(raremonaCPA[,6:593]), ci.type = "poly", ci.col = "lightblue",
     col ="blue",lwd = 2, ci.lty = 0, xlim = c(0,350), ylim=c(0,1000),
     ylab ="After", xlab = "Number of samples", cex.lab = 2)
###DO
raremonaDOA <- 
  puramona %>% 
  ungroup() %>% 
  filter(Site == "DO" & Sampling =="A") %>% 
  dplyr::select(Site, alltrees, Square, Sampling, Morphospecies, Abundance) %>% 
  unite(quadrat, alltrees, Square, sep ="_", remove =F) %>% 
  group_by(Site, alltrees, Square, quadrat, Sampling, Morphospecies) %>% 
  summarise_all(funs(sum)) %>% 
  spread(key = Morphospecies, Abundance, fill =0)
plot(specaccum(raremonaDOA[,6:318]), ci.type = "poly", ci.col = "lightblue",
     col ="blue",lwd = 2, ci.lty = 0, xlim = c(0,350), ylim=c(0,1000),
     xlab= "Number of sampes", ylab ="", cex.lab = 2)
###ER
raremonaERA <- 
  puramona %>% 
  ungroup() %>% 
  filter(Site == "ER" & Sampling =="A") %>% 
  dplyr::select(Site, alltrees, Square, Sampling, Morphospecies, Abundance) %>% 
  unite(quadrat, alltrees, Square, sep ="_", remove =F) %>% 
  group_by(Site, alltrees, Square, quadrat, Sampling, Morphospecies) %>% 
  summarise_all(funs(sum)) %>% 
  spread(key = Morphospecies, Abundance, fill =0)
plot(specaccum(raremonaERA[,6:432]), ci.type = "poly", ci.col = "lightblue",
     col ="blue",lwd = 2, ci.lty = 0, xlim = c(0,350), ylim=c(0,1000),
     xlab= "Number of samples", ylab ="", cex.lab =2)
##Closing everything
dev.off()
par(mfrow = c(1,1))











# Bromzy sample-based curves ----------------------------------------------
pdf("rarefaction_brom.pdf", height = 25, width = 30)
par(mfrow = c(3,3), mar = c(5.1, 5.1, 4.1, 2.1))
#Overall
##CP
rarebromCP <- 
  nonestbromzy %>% 
  ungroup() %>% 
  filter(Site == "CP") %>% 
  unite(allbroms, alltrees, Bromeliad, sep = "_", remove = F) %>% 
  dplyr::select(allbroms, Sampling, Morphospecies, Abundance) %>% 
  group_by(allbroms, Sampling, Morphospecies) %>% 
  summarise_all(funs(sum)) %>% 
  spread(key = Morphospecies, Abundance, fill =0)
plot(specaccum(rarebromCP[,3:165]), 
     ci.type = "poly", ci.col = "lightblue",
     col ="blue",lwd = 2, ci.lty = 0, xlim = c(0,75), ylim=c(0,250), 
     main = "CP", xlab= "", ylab ="Overall", cex.lab =2, cex.main =2)
##DO
rarebromDO <- 
  nonestbromzy %>% 
  ungroup() %>% 
  filter(Site == "DO") %>% 
  unite(allbroms, alltrees, Bromeliad, sep = "_", remove = F) %>% 
  dplyr::select(allbroms, Sampling, Morphospecies, Abundance) %>% 
  group_by(allbroms, Sampling, Morphospecies) %>% 
  summarise_all(funs(sum)) %>% 
  spread(key = Morphospecies, Abundance, fill =0)
plot(specaccum(rarebromDO[,3:128]), 
     ci.type = "poly", ci.col = "lightblue",
     col ="blue",lwd = 2, ci.lty = 0, xlim = c(0,75), ylim=c(0,250), 
     xlab= "", ylab ="", main ="DO", cex.main =2)
##ER
rarebromER <- 
  nonestbromzy %>% 
  ungroup() %>% 
  filter(Site == "ER") %>% 
  unite(allbroms, alltrees, Bromeliad, sep = "_", remove = F) %>% 
  dplyr::select(allbroms, Sampling, Morphospecies, Abundance) %>% 
  group_by(allbroms, Sampling, Morphospecies) %>% 
  summarise_all(funs(sum)) %>% 
  spread(key = Morphospecies, Abundance, fill =0)
plot(specaccum(rarebromER[,3:123]), 
     ci.type = "poly", ci.col = "lightblue",
     col ="blue",lwd = 2, ci.lty = 0, xlim = c(0,75), ylim=c(0,250), 
     xlab= "", ylab ="", main = "ER", cex.main =2)

#Before
##CP
rarebromCPB <- 
  nonestbromzy %>% 
  ungroup() %>% 
  filter(Site == "CP" & Sampling == "B") %>% 
  unite(allbroms, alltrees, Bromeliad, sep = "_", remove = F) %>% 
  dplyr::select(allbroms, Sampling, Morphospecies, Abundance) %>% 
  group_by(allbroms, Sampling, Morphospecies) %>% 
  summarise_all(funs(sum)) %>% 
  spread(key = Morphospecies, Abundance, fill =0)
plot(specaccum(rarebromCPB[,3:76]), 
     ci.type = "poly", ci.col = "lightblue",
     col ="blue",lwd = 2, ci.lty = 0, xlim = c(0,75), ylim=c(0,250), 
     xlab= "", ylab ="Before", cex.lab =2, cex.main =2)
##DO
rarebromDOB <- 
  nonestbromzy %>% 
  ungroup() %>% 
  filter(Site == "DO" & Sampling == "B") %>% 
  unite(allbroms, alltrees, Bromeliad, sep = "_", remove = F) %>% 
  dplyr::select(allbroms, Sampling, Morphospecies, Abundance) %>% 
  group_by(allbroms, Sampling, Morphospecies) %>% 
  summarise_all(funs(sum)) %>% 
  spread(key = Morphospecies, Abundance, fill =0)
plot(specaccum(rarebromDOB[,3:49]), 
     ci.type = "poly", ci.col = "lightblue",
     col ="blue",lwd = 2, ci.lty = 0, xlim = c(0,75), ylim=c(0,250), 
     xlab= "", ylab ="", cex.lab =2, cex.main =2)
##ER
rarebromERB <- 
  nonestbromzy %>% 
  ungroup() %>% 
  filter(Site == "ER" & Sampling == "B") %>% 
  unite(allbroms, alltrees, Bromeliad, sep = "_", remove = F) %>% 
  dplyr::select(allbroms, Sampling, Morphospecies, Abundance) %>% 
  group_by(allbroms, Sampling, Morphospecies) %>% 
  summarise_all(funs(sum)) %>% 
  spread(key = Morphospecies, Abundance, fill =0)
plot(specaccum(rarebromERB[,3:73]), 
     ci.type = "poly", ci.col = "lightblue",
     col ="blue",lwd = 2, ci.lty = 0, xlim = c(0,75), ylim=c(0,250), 
     xlab= "", ylab ="", cex.lab =2, cex.main =2)

#After
##CP
rarebromCPA <- 
  nonestbromzy %>% 
  ungroup() %>% 
  filter(Site == "CP" & Sampling == "A") %>% 
  unite(allbroms, alltrees, Bromeliad, sep = "_", remove = F) %>% 
  dplyr::select(allbroms, Sampling, Morphospecies, Abundance) %>% 
  group_by(allbroms, Sampling, Morphospecies) %>% 
  summarise_all(funs(sum)) %>% 
  spread(key = Morphospecies, Abundance, fill =0)
plot(specaccum(rarebromCPA[,3:124]), 
     ci.type = "poly", ci.col = "lightblue",
     col ="blue",lwd = 2, ci.lty = 0, xlim = c(0,75), ylim=c(0,150), 
     main = "", xlab= "Number of samples", ylab ="After", cex.lab =2)
##DO
rarebromDOA <- 
  nonestbromzy %>% 
  ungroup() %>% 
  filter(Site == "DO" & Sampling == "A") %>% 
  unite(allbroms, alltrees, Bromeliad, sep = "_", remove = F) %>% 
  dplyr::select(allbroms, Sampling, Morphospecies, Abundance) %>% 
  group_by(allbroms, Sampling, Morphospecies) %>% 
  summarise_all(funs(sum)) %>% 
  spread(key = Morphospecies, Abundance, fill =0)
plot(specaccum(rarebromDOA[,3:95]), 
     ci.type = "poly", ci.col = "lightblue",
     col ="blue",lwd = 2, ci.lty = 0, xlim = c(0,75), ylim=c(0,150), 
     xlab= "Number of samples", ylab ="", cex.lab =2, cex.main =2)
##ER
rarebromERA <- 
  nonestbromzy %>% 
  ungroup() %>% 
  filter(Site == "ER" & Sampling == "A") %>% 
  unite(allbroms, alltrees, Bromeliad, sep = "_", remove = F) %>% 
  dplyr::select(allbroms, Sampling, Morphospecies, Abundance) %>% 
  group_by(allbroms, Sampling, Morphospecies) %>% 
  summarise_all(funs(sum)) %>% 
  spread(key = Morphospecies, Abundance, fill =0)
plot(specaccum(rarebromERA[,3:75]), 
     ci.type = "poly", ci.col = "lightblue",
     col ="blue",lwd = 2, ci.lty = 0, xlim = c(0,75), ylim=c(0,150), 
     xlab= "Number of samples", ylab ="", cex.lab =2, cex.main =2)
##Closing
dev.off()
par(mfrow= c(1,1))








# Mona overall barplots for richness and abundance per order and diet--------------------------------------------------------------------
#Abundance
##order barplot
ordemona <- 
  puramona %>% 
  ungroup() %>% 
  dplyr::select(Order, Abundance) %>%
  group_by(Order) %>% 
  summarise_all(funs(sum)) 
##diet barplot
comemona <-
  puramona %>% 
  ungroup() %>% 
  dplyr::select(Diet, Abundance) %>%
  group_by(Diet) %>% 
  summarise_all(funs(sum)) 
#Richness
##Order barplot
ric_ordemona <- 
  puramona %>% 
  ungroup() %>% 
  dplyr::select(Order, Morphospecies)
ric_ordemona <- unique(ric_ordemona) 
ric_ordemona$Morphospecies <- 1
ric_ordemona <- 
  ric_ordemona %>% 
  group_by(Order) %>% 
  summarise_all(funs(sum)) 
##Diet barplot
ric_comemona <- 
  puramona %>% 
  ungroup() %>% 
  dplyr::select(Diet, Morphospecies)
ric_comemona <- unique(ric_comemona) 
ric_comemona$Morphospecies <- 1
ric_comemona <- 
  ric_comemona %>% 
  group_by(Diet) %>% 
  summarise_all(funs(sum)) 

#Plot
##Create PDF
pdf("mona_order_diet_overall.pdf",height = 10,width = 10)
##set par
par(mfrow = c(2,2), mar = c(6, 5.1, 2.1, 2.1))
##Ordemona 
barplot(ordemona$Abundance[order(ordemona$Abundance, decreasing = T)], 
        names.arg = ordemona$Order[order(ordemona$Abundance, decreasing = T)], 
        horiz = F, las = 3, cex.names = 0.9, 
        ylab = "Abundance", ylim = c(0, 3000), cex.main =2,
        main = "Per order", cex.lab = 2)
##Comemona
barplot(comemona$Abundance[order(comemona$Abundance, decreasing = T)], 
        names.arg = comemona$Diet[order(comemona$Abundance, decreasing = T)], 
        horiz = F, las = 3, cex.main = 2, 
        ylim = c(0, 3000), main = "Per diet")
##ric_ordemona
barplot(ric_ordemona$Morphospecies[order(ric_ordemona$Morphospecies, decreasing = T)], 
        names.arg = ric_ordemona$Order[order(ric_ordemona$Morphospecies, decreasing = T)], 
        horiz = F, las = 3, ylab = "Richness", cex.lab =2,
        ylim = c(0, 400))

##ric_comemona
barplot(ric_comemona$Morphospecies[order(ric_comemona$Morphospecies, decreasing = T)], 
        names.arg = ric_comemona$Diet[order(ric_comemona$Morphospecies, decreasing = T)], 
        horiz = F, las = 3,  
        ylim = c(0, 400))
##Close
dev.off()
par(mfrow = c(1,1))

# Bromzy overall barplots for richness and abundance per order and diet -----------
#No nest
#Abundance
##per order
order_nonestbromzy <- 
  nonestbromzy %>% 
  ungroup() %>% 
  dplyr::select(Order, Abundance) %>%
  group_by(Order) %>% 
  summarise_all(funs(sum)) 
##per diet
comer_nonestbromzy <- 
  nonestbromzy %>% 
  ungroup() %>% 
  dplyr::select(Diet, Abundance) %>%
  group_by(Diet) %>% 
  summarise_all(funs(sum))  
#Richness
##per order
ricorder_nonestbromzy <- 
  nonestbromzy %>% 
  ungroup() %>% 
  dplyr::select(Order, Morphospecies)
ricorder_nonestbromzy <- unique(ricorder_nonestbromzy) 
ricorder_nonestbromzy$Morphospecies <- 1
ricorder_nonestbromzy <- 
  ricorder_nonestbromzy %>% 
  group_by(Order) %>% 
  summarise_all(funs(sum)) 
##per diet
riccomer_nonestbromzy <- 
  nonestbromzy %>% 
  ungroup() %>% 
  dplyr::select(Diet, Morphospecies)
riccomer_nonestbromzy <- unique(riccomer_nonestbromzy) 
riccomer_nonestbromzy$Morphospecies <- 1
riccomer_nonestbromzy <- 
  riccomer_nonestbromzy %>% 
  group_by(Diet) %>% 
  summarise_all(funs(sum)) 

#With nest
#Abundance
##per order
order_bromzy <- 
  bromzy %>% 
  ungroup() %>% 
  dplyr::select(Order, Abundance) %>%
  group_by(Order) %>% 
  summarise_all(funs(sum)) 
##per diet
comer_bromzy <- 
  bromzy %>% 
  ungroup() %>% 
  dplyr::select(Diet, Abundance) %>%
  group_by(Diet) %>% 
  summarise_all(funs(sum))  
#Richness
##per order
ricorder_bromzy <- 
  bromzy %>% 
  ungroup() %>% 
  dplyr::select(Order, Morphospecies)
ricorder_bromzy <- unique(ricorder_bromzy) 
ricorder_bromzy$Morphospecies <- 1
ricorder_bromzy <- 
  ricorder_bromzy %>% 
  group_by(Order) %>% 
  summarise_all(funs(sum)) 
##per diet
riccomer_bromzy <- 
  bromzy %>% 
  ungroup() %>% 
  dplyr::select(Diet, Morphospecies)
riccomer_bromzy <- unique(riccomer_bromzy) 
riccomer_bromzy$Morphospecies <- 1
riccomer_bromzy <- 
  riccomer_bromzy %>% 
  group_by(Diet) %>% 
  summarise_all(funs(sum)) 

#Plots
##Create PDF
pdf("bromeliad_order_diet_abundance.pdf",height = 10,width = 10)
##set par
par(mfrow = c(2,2), mar = c(6, 5.1, 2.5, 2.1))
##order_nonestbromzy
barplot(order_nonestbromzy$Abundance[order(order_nonestbromzy$Abundance, decreasing = T)], 
        names.arg = order_nonestbromzy$Order[order(order_nonestbromzy$Abundance, decreasing = T)], 
        horiz = F, las = 3, cex.names = 0.9, ylab = "Abundance", ylim = c(0, 500),
        main = "Abundance per order", cex.main =2, cex.lab =2)
##comer_nonestbromzy
barplot(comer_nonestbromzy$Abundance[order(comer_nonestbromzy$Abundance, decreasing = T)], 
        names.arg = comer_nonestbromzy$Diet[order(comer_nonestbromzy$Abundance, decreasing = T)], 
        horiz = F, las = 3, cex.names = 1, ylab = "", ylim = c(0, 500),
        main = "Abundance per diet", cex.main =2, cex.lab =2)
##order_bromzy
barplot(order_bromzy$Abundance[order(order_bromzy$Abundance, decreasing = T)], 
        names.arg = order_bromzy$Order[order(order_bromzy$Abundance, decreasing = T)], 
        horiz = F, las = 3, cex.names = 0.9, ylab = "Abundance (including ant nests, log)",
        log ="y", cex.main =2, cex.lab =2)
##comer_bromzy
barplot(comer_bromzy$Abundance[order(comer_bromzy$Abundance, decreasing = T)], 
        names.arg = comer_bromzy$Diet[order(comer_bromzy$Abundance, decreasing = T)], 
        horiz = F, las = 3, cex.names = 1, ylab = "", log = "y", cex.main =2, cex.lab =2)
##close dev.off()
dev.off()
##Bringing back par
par(mfrow = c(1,1))

#Richness
##Create PDF
pdf("bromeliad_order_diet_richness.pdf",height = 15,width = 15)
##set par
par(mfrow = c(2,2), mar = c(6, 5.1, 2.5, 2.1))
  
##ricorder_nonestbromzy
barplot(ricorder_nonestbromzy$Morphospecies[order(ricorder_nonestbromzy$Morphospecies, decreasing = T)], 
        names.arg = ricorder_nonestbromzy$Order[order(ricorder_nonestbromzy$Morphospecies, decreasing = T)], 
        horiz = F, las = 3, ylab = "Richness ", ylim = c(0, 140),
        main = "Richness per order", cex.main =2, cex.lab =2)
##riccomer_nonestbromzy
barplot(riccomer_nonestbromzy$Morphospecies[order(riccomer_nonestbromzy$Morphospecies, decreasing = T)], 
        names.arg = riccomer_nonestbromzy$Diet[order(riccomer_nonestbromzy$Morphospecies, decreasing = T)], 
        horiz = F, las = 3, ylim = c(0, 140),
        main = "Richness per diet", cex.main =2, cex.lab =2)
##ricorder_bromzy
barplot(ricorder_bromzy$Morphospecies[order(ricorder_bromzy$Morphospecies, decreasing = T)], 
        names.arg = ricorder_bromzy$Order[order(ricorder_bromzy$Morphospecies, decreasing = T)], 
        horiz = F, las = 3, ylab = "Richness (including ant nests)",
        ylim = c(0, 140), cex.main =2, cex.lab =2)
##riccomer_bromzy
barplot(riccomer_bromzy$Morphospecies[order(riccomer_bromzy$Morphospecies, decreasing = T)], 
        names.arg = riccomer_bromzy$Diet[order(riccomer_bromzy$Morphospecies, decreasing = T)], 
        horiz = F, las = 3, ylim = c(0, 140), cex.main =2, cex.lab =2)
##close dev.off()
dev.off()
##Bringing back par
par(mfrow = c(1,1))






# Lamona NMDS before after ----------------------------------------------------------------
pdf("mona_NMDS.pdf", height = 25, width = 30)
par(mfrow = c(3,3), mar = c(5.5, 5.1, 4.1, 2.1))


#Overall 
##CP
ricamonaCP <- 
  puramona %>% 
  ungroup() %>% 
  filter(Site == "CP") %>% 
  dplyr::select(Tree, Sampling, Treatment, Morphospecies, Abundance) %>% 
  unite(treesamp, Tree, Sampling, remove =F) %>% 
  group_by(Sampling, Tree,Treatment, treesamp, Morphospecies) %>% 
  summarise_all(funs(sum)) %>% 
  spread(key = Morphospecies, Abundance, fill =0)
ricamonaCP <- data.frame(ricamonaCP, row.names = ricamonaCP$treesamp)
str(ricamonaCP)
##Dist
chaoCP <- vegdist(ricamonaCP[,6:799], method = "chao")
##NMDS
ricamonaCP.NMDS <- 
  metaMDS(chaoCP, k = 2, trymax = 20, 
          engine = c("monoMDS", "isoMDS"), autotransform =F, plot = F)
##extracting and plotting
datascores <- as.data.frame(scores(ricamonaCP.NMDS))
datascores$sampling <- ricamonaCP$Sampling
datascores$treatment <- ricamonaCP$Treatment
plot(datascores$NMDS2 ~ datascores$NMDS1, pch = c(16,17)[datascores$sampling], cex =2, 
     col= c('yellow','blue','red')[datascores$treatment], xlim =c(-1,1), ylim =c(-0.5,0.5),
     xlab = "", ylab = "Overall", main = "CP", cex.main = 2, cex.lab =2)
legend ("topleft", legend = c("w", "wo", "wr"), 
        pch = 15, pt.bg = c('yellow','blue','red'), col = c('yellow','blue','red'))
legend("bottomleft", legend = "Stress = 0.265")
##DO
ricamonaDO <- 
  puramona %>% 
  ungroup() %>% 
  filter(Site == "DO") %>% 
  dplyr::select(Tree, Sampling, Treatment, Morphospecies, Abundance) %>% 
  unite(treesamp, Tree, Sampling, remove =F) %>% 
  group_by(Sampling, Tree,Treatment, treesamp, Morphospecies) %>% 
  summarise_all(funs(sum)) %>% 
  spread(key = Morphospecies, Abundance, fill =0)
ricamonaDO <- data.frame(ricamonaDO, row.names = ricamonaDO$treesamp)
str(ricamonaDO)
##Dist
chaoDO <- vegdist(ricamonaDO[,5:505], method = "chao")
##NMDS
ricamonaDO.NMDS <- 
  metaMDS(chaoDO, k = 2, trymax = 20, 
          engine = c("monoMDS", "isoMDS"), autotransform =F, plot = F)
##extracting and plotting
datascores <- as.data.frame(scores(ricamonaDO.NMDS))
datascores$sampling <- ricamonaDO$Sampling
datascores$treatment <- ricamonaDO$Treatment
plot(datascores$NMDS2 ~ datascores$NMDS1, pch = c(16,17)[datascores$sampling], cex =2, 
     col= c('yellow','blue','red')[datascores$treatment], xlim =c(-1,1), ylim =c(-0.5,0.5),
     xlab = "", ylab = "", main ="DO", cex.main =2)
legend("bottomleft", legend = "Stress = 0.253")
##ER
ricamonaER <- 
  puramona %>% 
  ungroup() %>% 
  filter(Site == "ER") %>% 
  dplyr::select(Tree, Sampling, Treatment, Morphospecies, Abundance) %>% 
  unite(treesamp, Tree, Sampling, remove =F) %>% 
  group_by(Sampling, Tree,Treatment, treesamp, Morphospecies) %>% 
  summarise_all(funs(sum)) %>% 
  spread(key = Morphospecies, Abundance, fill =0)
ricamonaER <- data.frame(ricamonaER, row.names = ricamonaER$treesamp)
str(ricamonaER)
##Dist
chaoER <- vegdist(ricamonaER[,5:612], method = "chao")
##NMDS
ricamonaER.NMDS <- 
  metaMDS(chaoER, k = 2, trymax = 20, 
          engine = c("monoMDS", "isoMDS"), autotransform = F, plot = F)
##extracting and plotting
datascores <- as.data.frame(scores(ricamonaER.NMDS))
datascores$sampling <- ricamonaER$Sampling
datascores$treatment <- ricamonaER$Treatment
plot(datascores$NMDS2 ~ datascores$NMDS1, pch = c(16,17)[datascores$sampling], cex =2, 
     col= c('yellow','blue','red')[datascores$treatment], xlim =c(-1,1), ylim =c(-0.5,0.5),
     xlab = "", ylab = "", main ="ER", cex.main = 2)
legend("topright", legend = c("After", "Before"), pch = c(1, 2))
legend("bottomleft", legend = "Stress = 0.260")


#Before
##CP
ricamonaCPB <- 
  puramona %>% 
  ungroup() %>% 
  filter(Site == "CP" & Sampling == "B") %>% 
  dplyr::select(Tree,Treatment, Morphospecies, Abundance) %>%
  group_by(Tree,Treatment, Morphospecies) %>% 
  summarise_all(funs(sum)) %>% 
  spread(key = Morphospecies, Abundance, fill =0)
ricamonaCPB <- data.frame(ricamonaCPB, row.names = ricamonaCPB$Tree)
str(ricamonaCPB)
##Dist
chaoCPB <- vegdist(ricamonaCPB[,3:402], method = "chao")
##NMDS
ricamonaCPB.NMDS <- 
  metaMDS(chaoCPB, k = 2, trymax = 20, 
          engine = c("monoMDS", "isoMDS"), autotransform =F, plot = F)
###extracting and plotting
datascores <- as.data.frame(scores(ricamonaCPB.NMDS))
datascores$treatment <- ricamonaCPB$Treatment
plot(datascores$NMDS2 ~ datascores$NMDS1, pch = 17, cex =2,
     col= c('yellow','blue','red')[datascores$treatment], xlim =c(-1,1), 
     ylim =c(-0.5,0.5),
     xlab = "", ylab = "Before", cex.lab =2)
legend ("bottomleft", legend = "Stress = 0.280")
##DO
ricamonaDOB <- 
  puramona %>% 
  ungroup() %>% 
  filter(Site == "DO" & Sampling == "B") %>% 
  dplyr::select(Tree,Treatment, Morphospecies, Abundance) %>%
  group_by(Tree,Treatment, Morphospecies) %>% 
  summarise_all(funs(sum)) %>% 
  spread(key = Morphospecies, Abundance, fill =0)
ricamonaDOB <- data.frame(ricamonaDOB, row.names = ricamonaDOB$Tree)
str(ricamonaDOB)
##Dist
chaoDOB <- vegdist(ricamonaDOB[,3:302], method = "chao")
##NMDS
ricamonaDOB.NMDS <- 
  metaMDS(chaoDOB, k = 2, trymax = 20, 
          engine = c("monoMDS", "isoMDS"), autotransform =F)
##extracting and plotting
datascores <- as.data.frame(scores(ricamonaDOB.NMDS))
datascores$treatment <- ricamonaDOB$Treatment
plot(datascores$NMDS2 ~ datascores$NMDS1, pch = 17, cex =2,
     col= c('yellow','blue','red')[datascores$treatment], xlim =c(-1,1), 
     ylim =c(-0.5,0.5), xlab = "", ylab = "")
legend ("bottomleft", legend = "Stress =0.268")
##ER
ricamonaERB <- 
  puramona %>% 
  ungroup() %>% 
  filter(Site == "ER" & Sampling == "B") %>% 
  dplyr::select(Tree,Treatment, Morphospecies, Abundance) %>%
  group_by(Tree,Treatment, Morphospecies) %>% 
  summarise_all(funs(sum)) %>% 
  spread(key = Morphospecies, Abundance, fill =0)
ricamonaERB <- data.frame(ricamonaERB, row.names = ricamonaERB$Tree)
str(ricamonaERB)
##Dist
chaoERB <- vegdist(ricamonaERB[,3:313], method = "chao")
##NMDS
ricamonaERB.NMDS <- 
  metaMDS(chaoERB, k = 2, trymax = 20, 
          engine = c("monoMDS", "isoMDS"), autotransform =F, plot = F)
##extracting and plotting
datascores <- as.data.frame(scores(ricamonaERB.NMDS))
datascores$treatment <- ricamonaERB$Treatment
plot(datascores$NMDS2 ~ datascores$NMDS1, pch = 17, cex =2,
     col= c('yellow','blue','red')[datascores$treatment], xlim =c(-1,1), 
     ylim =c(-0.5,0.5),
     xlab = "", ylab = "")
legend ("bottomleft", legend = "Stress = 0.233")




#After
##CP
ricamonaCPA <- 
  puramona %>% 
  ungroup() %>% 
  filter(Site == "CP" & Sampling == "A") %>% 
  dplyr::select(Tree,Treatment, Morphospecies, Abundance) %>%
  group_by(Tree,Treatment, Morphospecies) %>% 
  summarise_all(funs(sum)) %>% 
  spread(key = Morphospecies, Abundance, fill =0)
ricamonaCPA <- data.frame(ricamonaCPA, row.names = ricamonaCPA$Tree)
str(ricamonaCPA)
##Dist
chaoCPA <- vegdist(ricamonaCPA[,3:590], method = "chao")
##NMDS
ricamonaCPA.NMDS <- 
  metaMDS(chaoCPB, k = 2, trymax = 20, 
          engine = c("monoMDS", "isoMDS"), autotransform =F, plot = F)
##extracting and plotting
datascores <- as.data.frame(scores(ricamonaCPA.NMDS))
datascores$treatment <- ricamonaCPA$Treatment
plot(datascores$NMDS2 ~ datascores$NMDS1, pch = 16, cex =2,
     col= c('yellow','blue','red')[datascores$treatment], xlim =c(-1,1), 
     ylim =c(-0.5,0.5),xlab = "", ylab = "After", cex.lab =2)
legend ("bottomleft", legend = "Stress = 0.280")
##DO
ricamonaDOA <- 
  puramona %>% 
  ungroup() %>% 
  filter(Site == "DO" & Sampling == "A") %>% 
  dplyr::select(Tree,Treatment, Morphospecies, Abundance) %>%
  group_by(Tree,Treatment, Morphospecies) %>% 
  summarise_all(funs(sum)) %>% 
  spread(key = Morphospecies, Abundance, fill =0)
ricamonaDOA <- data.frame(ricamonaDOA, row.names = ricamonaDOA$Tree)
str(ricamonaDOA)
##Dist
chaoDOA <- vegdist(ricamonaDOA[,3:315], method = "chao")
##NMDS
ricamonaDOA.NMDS <- 
  metaMDS(chaoDOB, k = 2, trymax = 20, 
          engine = c("monoMDS", "isoMDS"), autotransform =F, plot = F)
##extracting and plotting
datascores <- as.data.frame(scores(ricamonaDOA.NMDS))
datascores$treatment <- ricamonaDOA$Treatment
plot(datascores$NMDS2 ~ datascores$NMDS1, pch = 16, cex =2,
     col= c('yellow','blue','red')[datascores$treatment], xlim =c(-1,1), 
     ylim =c(-0.5,0.5),
     xlab = "", ylab = "")
legend ("bottomleft", legend ="Stress = 0.268")
##ER
ricamonaERA <- 
  puramona %>% 
  ungroup() %>% 
  filter(Site == "ER" & Sampling == "A") %>% 
  dplyr::select(Tree,Treatment, Morphospecies, Abundance) %>%
  group_by(Tree,Treatment, Morphospecies) %>% 
  summarise_all(funs(sum)) %>% 
  spread(key = Morphospecies, Abundance, fill =0)
ricamonaERA <- data.frame(ricamonaERA, row.names = ricamonaERA$Tree)
str(ricamonaERA)
##Dist
chaoERA <- vegdist(ricamonaERA[,3:429], method = "chao")
##NMDS
ricamonaERA.NMDS <- 
  metaMDS(chaoERB, k = 2, trymax = 20, 
          engine = c("monoMDS", "isoMDS"), autotransform =F, plot = F)
##extracting and plotting
datascores <- as.data.frame(scores(ricamonaERA.NMDS))
datascores$treatment <- ricamonaERA$Treatment
plot(datascores$NMDS2 ~ datascores$NMDS1, pch = 16, cex =2,
     col= c('yellow','blue','red')[datascores$treatment], xlim =c(-1,1), 
     ylim =c(-0.5,0.5),
     xlab = "", ylab = "")
legend ("bottomleft", legend = "Stress =0.232")

##Close and reset par
dev.off()
par (mfrow = c(1,1))



































# Lamona NMDS with tree ID ------------------------------------------------

#Overall
##CP
plot(ricamonaCP.NMDS, type ="n", xlim =c(-1,1), ylim =c(-0.5,0.5))
orditorp(ricamonaCP.NMDS,display="sites",cex=0.5, pch = NA, air = 0.5)
##DO
plot(ricamonaDO.NMDS, type ="n", xlim =c(-1,1), ylim =c(-0.5,0.5))
orditorp(ricamonaDO.NMDS,display="sites",cex=0.5, pch = NA, air = 0.5)
##ER
plot(ricamonaER.NMDS, type ="n", xlim =c(-1,1), ylim =c(-0.5,0.5))
orditorp(ricamonaER.NMDS,display="sites",cex=0.5, pch = NA, air = 0.5)

#Before
##CP
plot(ricamonaCPB.NMDS, type ="n", xlim =c(-1,1), ylim =c(-0.5,0.5))
orditorp(ricamonaCPB.NMDS,display="sites",cex=0.5, pch = NA, air = 0.5)
##DO
plot(ricamonaDOB.NMDS, type ="n", xlim =c(-1,1), ylim =c(-0.5,0.5))
orditorp(ricamonaDOB.NMDS,display="sites",cex=0.5, pch = NA, air = 0.5)
##ER
plot(ricamonaERB.NMDS, type ="n", xlim =c(-1,1), ylim =c(-0.5,0.5))
orditorp(ricamonaERB.NMDS,display="sites",cex=0.5, pch = NA, air = 0.5)

#After
##CP
plot(ricamonaCPA.NMDS, type ="n", xlim =c(-1,1), ylim =c(-0.5,0.5))
orditorp(ricamonaCPB.NMDS,display="sites",cex=0.5, pch = NA, air = 0.5)
##DO
plot(ricamonaDOA.NMDS, type ="n", xlim =c(-1,1), ylim =c(-0.5,0.5))
orditorp(ricamonaDOB.NMDS,display="sites",cex=0.5, pch = NA, air = 0.5)
##ER
plot(ricamonaERA.NMDS, type ="n", xlim =c(-1,1), ylim =c(-0.5,0.5))
orditorp(ricamonaERB.NMDS,display="sites",cex=0.5, pch = NA, air = 0.5)

# Nonestbromzy NMDS  DOESNT WORK DATA TOO SPREAD---------------------------------------------------
pdf("nonestbrom_NMDS.pdf", height = 25, width = 30)
par(mfrow = c(3,3), mar = c(5.5, 5.1, 4.1, 2.1))
#Before/After in each site
##CP
ricabromCP <- 
  nonestbromzy %>% 
  ungroup() %>% 
  filter(Site == "CP") %>% 
  unite(allbroms, Tree, Bromeliad, remove = F) %>% 
  dplyr::select(Sampling, allbroms, Morphospecies, Abundance) %>% 
  spread(key = Morphospecies, Abundance, fill = 0)
##Dist
chaoCP <- vegdist(ricabromCP[,3:165], method = "horn")
##NMDS
ricabromCP.NMDS <- 
  metaMDS(chaoCP, k = 2, trymax = 20, 
          engine = "monoMDS", autotransform =F, plot = F)
##extracting and plotting
datascores <- as.data.frame(scores(ricabromCP.NMDS))
datascores$sampling <- ricabromCP$Sampling

plot(datascores$NMDS2 ~ datascores$NMDS1, pch = c(16,17)[datascores$sampling], cex =2) 
, xlim =c(-1,1), ylim =c(-0.5,0.5),
xlab = "", ylab = "Overall", main = "CP", cex.main = 2, cex.lab =2)
legend ("topleft", legend = c("w", "wo", "wr"), 
        pch = 15, pt.bg = c('yellow','blue','red'), col = c('yellow','blue','red'))
legend("bottomleft", legend = "Stress = 0.265")


##Difference overall
ricabromtodo <- 
  nonestbromzy %>% 
  ungroup() %>% 
  unite(allbroms, Site, Tree, Bromeliad, remove = F) %>% 
  dplyr::select(Sampling, allbroms, Morphospecies, Abundance) %>% 
  spread(key = Morphospecies, Abundance, fill = 0)
##Dist
chaotodo <- vegdist(ricabromtodo[,3:313], method = "horn")
##NMDS
ricabromtodo.NMDS <- 
  metaMDS(chaoCP, k = 2, trymax = 20, 
          engine = "isoMDS", autotransform =F, plot = T)
##extracting and plotting
datascores <- as.data.frame(scores(ricabromCP.NMDS))
datascores$sampling <- ricabromCP$Sampling

plot(datascores$NMDS2 ~ datascores$NMDS1, pch = c(16,17)[datascores$sampling], cex =2) 
, xlim =c(-1,1), ylim =c(-0.5,0.5),
xlab = "", ylab = "Overall", main = "CP", cex.main = 2, cex.lab =2)
legend ("topleft", legend = c("w", "wo", "wr"), 
        pch = 15, pt.bg = c('yellow','blue','red'), col = c('yellow','blue','red'))
legend("bottomleft", legend = "Stress = 0.265")

##Before/After/ overall





# Bromzy NMDS DOESNT WORK DATA TOO SPREAD--------------------------------------------------
ricabromtodo <- 
  purabromzy %>% 
  ungroup() %>% 
  unite(allbroms, Site, Tree, Bromeliad, remove = F) %>% 
  dplyr::select(Sampling, Site, allbroms, Morphospecies, Abundance) %>% 
  spread(key = Morphospecies, Abundance, fill = 0)
##Remove duplicate rows

##Dist
chaotodo <- vegdist(ricabromtodo[,4:317], method = "horn")
##NMDS
ricabromtodo.NMDS <- 
  metaMDS(chaotodo, k = 2, trymax = 20, 
          engine = "monoMDS", autotransform =F, plot = F)


# Bromzynonest inside plotting ------------------------------------------------------
#Overall
##Abundance
sizetest <- 
  nonestbromzy %>% 
  ungroup() %>% 
  dplyr::select(Site, alltrees, Bromeliad, Abundance) %>% 
  group_by(Site, alltrees, Bromeliad) %>% 
  summarise_all(funs(sum))
##Richness
ricperbrom <- 
  nonestbromzy %>% 
  ungroup() %>% 
  dplyr::select(Site, alltrees, Bromeliad, Morphospecies)
ricperbrom <- unique(ricperbrom)
ricperbrom$Morphospecies <- 1
ricperbrom <- 
  ricperbrom %>% 
  group_by(Site, alltrees, Bromeliad) %>% 
  summarise_all(funs(sum))
sizetest <- 
  sizetest %>% 
  left_join(ricperbrom)
sizetest$alltrees <- as.factor(sizetest$alltrees)
##Now add brom leaf length as proxy for size
bromsize <- 
  distance %>% 
  unite(alltrees, Site, Tree, sep ="_", remove =F) %>% 
  dplyr::select(Site, alltrees, Bromeliad_., Longest_leaf_length)
bromsize$alltrees <- as.factor(bromsize$alltrees) 
bromsize <-  unique(bromsize)
###make names the same and join
colnames(sizetest) <- c("Site", "alltrees", "Bromeliad", "Abundance", "Richness")
colnames(bromsize) <- c("Site", "alltrees", "Bromeliad", "bigleaf")
sizetest <- 
  bromsize %>% 
  left_join(sizetest)
str(sizetest)
sizetest$alltrees <- as.factor(sizetest$alltrees)
sizetest$Bromeliad <- as.factor(sizetest$Bromeliad)
str(sizetest)
sizetest <- 
  sizetest %>% 
  unite(bromname, alltrees, Bromeliad, sep="_", remove =F) %>% 
  dplyr::select(-Bromeliad)
###adding empty broms and removing the fallen one
sizetest$Abundance[is.na(sizetest$Abundance)] <- 0
sizetest$Richness[is.na(sizetest$Richness)] <- 0
sizetest <- 
  sizetest %>% 
  filter(bromname != "ER_F14_a")
##Plotamos pues
plot(sizetest$Abundance ~ sizetest$bigleaf)
plot(sizetest$Richness ~ sizetest$bigleaf)
plot(sizetest$Richness ~ sizetest$Abundance)

#Just predators
##Abundance
predtest <- 
  nonestbromzy %>% 
  filter(Diet == "pred") %>% 
  ungroup() %>% 
  dplyr::select(Site, alltrees, Bromeliad, Abundance) %>% 
  group_by(Site, alltrees, Bromeliad) %>% 
  summarise_all(funs(sum))
##Richness
predricperbrom <- 
  nonestbromzy %>% 
  filter(Diet == "pred") %>%
  ungroup() %>% 
  dplyr::select(Site, alltrees, Bromeliad, Morphospecies)
predricperbrom <- unique(predricperbrom)
predricperbrom$Morphospecies <- 1
predricperbrom <- 
  predricperbrom %>% 
  group_by(Site, alltrees, Bromeliad) %>% 
  summarise_all(funs(sum))
predtest <- 
  predtest %>% 
  left_join(predricperbrom)
predtest$alltrees <- as.factor(predtest$alltrees)
##Now add brom leaf length as proxy for size
bromsize <- 
  distance %>% 
  unite(alltrees, Site, Tree, sep ="_", remove =F) %>% 
  dplyr::select(Site, alltrees, Bromeliad_., Longest_leaf_length)
bromsize$alltrees <- as.factor(bromsize$alltrees) 
bromsize <-  unique(bromsize)
###make names the same and join
colnames(predtest) <- c("Site", "alltrees", "Bromeliad", "Abundance", "Richness")
colnames(bromsize) <- c("Site", "alltrees", "Bromeliad", "bigleaf")
predtest <- 
  bromsize %>% 
  left_join(predtest)
str(predtest)
predtest$alltrees <- as.factor(predtest$alltrees)
predtest$Bromeliad <- as.factor(predtest$Bromeliad)
str(predtest)
predtest <- 
  predtest %>% 
  unite(bromname, alltrees, Bromeliad, sep="_", remove =F) %>% 
  dplyr::select(-Bromeliad)
###adding empty broms and removing the fallen one
predtest$Abundance[is.na(predtest$Abundance)] <- 0
predtest$Richness[is.na(predtest$Richness)] <- 0
predtest <- 
  predtest %>% 
  filter(bromname != "ER_F14_a")
##Plotamos pues
plot(predtest$Abundance ~ predtest$bigleaf)
plot(predtest$Richness ~ predtest$bigleaf)
plot(predtest$Richness ~ predtest$Abundance)

##Hebivores
##Abundance
herbtest <- 
  nonestbromzy %>% 
  filter(Diet == "herb") %>% 
  ungroup() %>% 
  dplyr::select(Site, alltrees, Bromeliad, Abundance) %>% 
  group_by(Site, alltrees, Bromeliad) %>% 
  summarise_all(funs(sum))
##Richness
herbricperbrom <- 
  nonestbromzy %>% 
  filter(Diet == "herb") %>%
  ungroup() %>% 
  dplyr::select(Site, alltrees, Bromeliad, Morphospecies)
herbricperbrom <- unique(herbricperbrom)
herbricperbrom$Morphospecies <- 1
herbricperbrom <- 
  herbricperbrom %>% 
  group_by(Site, alltrees, Bromeliad) %>% 
  summarise_all(funs(sum))
herbtest <- 
  herbtest %>% 
  left_join(herbricperbrom)
herbtest$alltrees <- as.factor(herbtest$alltrees)
##Now add brom leaf length as proxy for size
bromsize <- 
  distance %>% 
  unite(alltrees, Site, Tree, sep ="_", remove =F) %>% 
  dplyr::select(Site, alltrees, Bromeliad_., Longest_leaf_length)
bromsize$alltrees <- as.factor(bromsize$alltrees) 
bromsize <-  unique(bromsize)
###make names the same and join
colnames(herbtest) <- c("Site", "alltrees", "Bromeliad", "Abundance", "Richness")
colnames(bromsize) <- c("Site", "alltrees", "Bromeliad", "bigleaf")
herbtest <- 
  bromsize %>% 
  left_join(herbtest)
str(herbtest)
herbtest$alltrees <- as.factor(herbtest$alltrees)
herbtest$Bromeliad <- as.factor(herbtest$Bromeliad)
str(herbtest)
herbtest <- 
  herbtest %>% 
  unite(bromname, alltrees, Bromeliad, sep="_", remove =F) %>% 
  dplyr::select(-Bromeliad)
###adding empty broms and removing the fallen one
herbtest$Abundance[is.na(herbtest$Abundance)] <- 0
herbtest$Richness[is.na(herbtest$Richness)] <- 0
herbtest <- 
  herbtest %>% 
  filter(bromname != "ER_F14_a")
##Plotamos pues
plot(herbtest$Abundance ~ herbtest$bigleaf)
plot(herbtest$Richness ~ herbtest$bigleaf)
plot(herbtest$Richness ~ herbtest$Abundance)

# Bromzynonest inside testing --------------------------------------------------------

#Size vs richness and abundance overall
##How are the two correlated
plot(sizetest$Richness ~ sizetest$Abundance)
cor.test(sizetest$Richness, sizetest$Abundance)
##Models
bromsizemodel_1 <- 
  glmer.nb(Abundance ~ log(bigleaf) +(1|Site/alltrees),
          data = sizetest)
plot(bromsizemodel_1)
qqnorm(residuals(bromsizemodel_1))
qqline(residuals(bromsizemodel_1), col = "red")
summary(bromsizemodel_1)
Anova(bromsizemodel_1)
getME(bromsizemodel_1, "glmer.nb.theta")

bromsizemodel_2 <- 
  glmer.nb(Richness ~ log(bigleaf) + (1|Site/alltrees),
        data = sizetest)
plot(bromsizemodel_2)
qqnorm(residuals(bromsizemodel_2))
qqline(residuals(bromsizemodel_2), col = "red")
summary(bromsizemodel_2)
Anova(bromsizemodel_2)
#Size vs richness and abundance inside predator
bromsizemodel_3 <- 
  glmer.nb(Abundance ~ log(bigleaf) + (1|Site/alltrees), data = predtest)
plot(bromsizemodel_3)
qqnorm(residuals(bromsizemodel_3))
qqline(residuals(bromsizemodel_3), col = "red")
summary(bromsizemodel_3)
Anova(bromsizemodel_3)
bromsizemodel_4 <- 
  glmer.nb(Richness ~ log(bigleaf) + (1|Site/alltrees), data = predtest)
plot(bromsizemodel_4)
qqnorm(residuals(bromsizemodel_4))
qqline(residuals(bromsizemodel_4), col = "red")
summary(bromsizemodel_4)
Anova(bromsizemodel_4)
#Size vs richness and abundance inside herbivore
bromsizemodel_5 <- 
  glmer.nb(Abundance ~ log(bigleaf) + (1|Site/alltrees), data = herbtest)
plot(bromsizemodel_5)
qqnorm(residuals(bromsizemodel_5))
qqline(residuals(bromsizemodel_5), col = "red")
summary(bromsizemodel_5)
Anova(bromsizemodel_5)

bromsizemodel_6 <- 
  glmer.nb(Richness ~ log(bigleaf) + (1|Site/alltrees), data = herbtest)
plot(bromsizemodel_6)
qqnorm(residuals(bromsizemodel_6))
qqline(residuals(bromsizemodel_6), col = "red")
summary(bromsizemodel_6)
Anova(bromsizemodel_6)




##Now plot with line
##Create PDF
pdf("richness_abundance_broms.pdf",height = 10,width = 10)
##set par
par(mfrow = c(2,1))
plot(sizetest$Abundance ~ sizetest$bigleaf, pch = 20, 
     ylab = "Arthropod abundance", xlab = "Largest leaf length (cm)")
coef <- fixef(bromsizemodel_1)
abline(coef = coef, col = "red", lwd = 2)
plot(sizetest$Richness ~ sizetest$biglea, pch = 20, 
     ylab = "Arthropod richness", xlab = "Largest leaf length (cm)")
coef <- fixef(bromsizemodel_2)
abline(coef = coef, col = "red", lwd = 2)
##close dev.off()
dev.off()
##Bring par back
par(mfrow = (c(1,1)))
##Now plot with line
##Create PDF
pdf("richness_abundance_broms.pdf",height = 10,width = 10)
##set par
par(mfrow = c(2,1))
plot(predtest$Abundance ~ predtest$bigleaf, pch = 20, 
     ylab = "Predator abundance", xlab = "Largest leaf length (cm)")
coef <- fixef(bromsizemodel_3)
abline(coef = coef, col = "red", lwd = 2)
plot(predtest$Richness ~ predtest$biglea, pch = 20, 
     ylab = "Predator richness", xlab = "Largest leaf length (cm)")
coef <- fixef(bromsizemodel_4)
abline(coef = coef, col = "red", lwd = 2)
##close dev.off()
dev.off()
##Bring par back
par(mfrow = (c(1,1)))
##Now plot with line
##Create PDF
pdf("richness_abundance_broms.pdf",height = 10,width = 10)
##set par
par(mfrow = c(2,1))
plot(herbtest$Abundance ~ herbtest$bigleaf, pch = 20, 
     ylab = "herbivore abundance", xlab = "Largest leaf length (cm)")
coef <- fixef(bromsizemodel_3)
abline(coef = coef, col = "red", lwd = 2)
plot(herbtest$Richness ~ herbtest$biglea, pch = 20, 
     ylab = "herbivore richness", xlab = "Largest leaf length (cm)")
coef <- fixef(bromsizemodel_4)
abline(coef = coef, col = "red", lwd = 2)
##close dev.off()
dev.off()
##Bring par back
par(mfrow = (c(1,1)))






# Bromzynonest inside plotting ------------------------------------------------------
#Overall
##Abundance
sizetest <- 
  nonestbromzy %>% 
  ungroup() %>% 
  dplyr::select(Site, alltrees, Bromeliad, Abundance) %>% 
  group_by(Site, alltrees, Bromeliad) %>% 
  summarise_all(funs(sum))
##Richness
ricperbrom <- 
  nonestbromzy %>% 
  ungroup() %>% 
  dplyr::select(Site, alltrees, Bromeliad, Morphospecies)
ricperbrom <- unique(ricperbrom)
ricperbrom$Morphospecies <- 1
ricperbrom <- 
  ricperbrom %>% 
  group_by(Site, alltrees, Bromeliad) %>% 
  summarise_all(funs(sum))
sizetest <- 
  sizetest %>% 
  left_join(ricperbrom)
sizetest$alltrees <- as.factor(sizetest$alltrees)
##Now add brom leaf length as proxy for size
bromsize <- 
  distance %>% 
  unite(alltrees, Site, Tree, sep ="_", remove =F) %>% 
  dplyr::select(Site, alltrees, Bromeliad_., Longest_leaf_length)
bromsize$alltrees <- as.factor(bromsize$alltrees) 
bromsize <-  unique(bromsize)
###make names the same and join
colnames(sizetest) <- c("Site", "alltrees", "Bromeliad", "Abundance", "Richness")
colnames(bromsize) <- c("Site", "alltrees", "Bromeliad", "bigleaf")
sizetest <- 
  bromsize %>% 
  left_join(sizetest)
str(sizetest)
sizetest$alltrees <- as.factor(sizetest$alltrees)
sizetest$Bromeliad <- as.factor(sizetest$Bromeliad)
str(sizetest)
sizetest <- 
  sizetest %>% 
  unite(bromname, alltrees, Bromeliad, sep="_", remove =F) %>% 
  dplyr::select(-Bromeliad)
###adding empty broms and removing the fallen one
sizetest$Abundance[is.na(sizetest$Abundance)] <- 0
sizetest$Richness[is.na(sizetest$Richness)] <- 0
sizetest <- 
  sizetest %>% 
  filter(bromname != "ER_F14_a")
##Plotamos pues
plot(sizetest$Abundance ~ sizetest$bigleaf)
plot(sizetest$Richness ~ sizetest$bigleaf)
plot(sizetest$Richness ~ sizetest$Abundance)

#Just predators
##Abundance
predtest <- 
  nonestbromzy %>% 
  filter(Diet == "pred") %>% 
  ungroup() %>% 
  dplyr::select(Site, alltrees, Bromeliad, Abundance) %>% 
  group_by(Site, alltrees, Bromeliad) %>% 
  summarise_all(funs(sum))
##Richness
predricperbrom <- 
  nonestbromzy %>% 
  filter(Diet == "pred") %>%
  ungroup() %>% 
  dplyr::select(Site, alltrees, Bromeliad, Morphospecies)
predricperbrom <- unique(predricperbrom)
predricperbrom$Morphospecies <- 1
predricperbrom <- 
  predricperbrom %>% 
  group_by(Site, alltrees, Bromeliad) %>% 
  summarise_all(funs(sum))
predtest <- 
  predtest %>% 
  left_join(predricperbrom)
predtest$alltrees <- as.factor(predtest$alltrees)
##Now add brom leaf length as proxy for size
bromsize <- 
  distance %>% 
  unite(alltrees, Site, Tree, sep ="_", remove =F) %>% 
  dplyr::select(Site, alltrees, Bromeliad_., Longest_leaf_length)
bromsize$alltrees <- as.factor(bromsize$alltrees) 
bromsize <-  unique(bromsize)
###make names the same and join
colnames(predtest) <- c("Site", "alltrees", "Bromeliad", "Abundance", "Richness")
colnames(bromsize) <- c("Site", "alltrees", "Bromeliad", "bigleaf")
predtest <- 
  bromsize %>% 
  left_join(predtest)
str(predtest)
predtest$alltrees <- as.factor(predtest$alltrees)
predtest$Bromeliad <- as.factor(predtest$Bromeliad)
str(predtest)
predtest <- 
  predtest %>% 
  unite(bromname, alltrees, Bromeliad, sep="_", remove =F) %>% 
  dplyr::select(-Bromeliad)
###adding empty broms and removing the fallen one
predtest$Abundance[is.na(predtest$Abundance)] <- 0
predtest$Richness[is.na(predtest$Richness)] <- 0
predtest <- 
  predtest %>% 
  filter(bromname != "ER_F14_a")
##Plotamos pues
plot(predtest$Abundance ~ predtest$bigleaf)
plot(predtest$Richness ~ predtest$bigleaf)
plot(predtest$Richness ~ predtest$Abundance)

##Hebivores
##Abundance
herbtest <- 
  nonestbromzy %>% 
  filter(Diet == "herb") %>% 
  ungroup() %>% 
  dplyr::select(Site, alltrees, Bromeliad, Abundance) %>% 
  group_by(Site, alltrees, Bromeliad) %>% 
  summarise_all(funs(sum))
##Richness
herbricperbrom <- 
  nonestbromzy %>% 
  filter(Diet == "herb") %>%
  ungroup() %>% 
  dplyr::select(Site, alltrees, Bromeliad, Morphospecies)
herbricperbrom <- unique(herbricperbrom)
herbricperbrom$Morphospecies <- 1
herbricperbrom <- 
  herbricperbrom %>% 
  group_by(Site, alltrees, Bromeliad) %>% 
  summarise_all(funs(sum))
herbtest <- 
  herbtest %>% 
  left_join(herbricperbrom)
herbtest$alltrees <- as.factor(herbtest$alltrees)
##Now add brom leaf length as proxy for size
bromsize <- 
  distance %>% 
  unite(alltrees, Site, Tree, sep ="_", remove =F) %>% 
  dplyr::select(Site, alltrees, Bromeliad_., Longest_leaf_length)
bromsize$alltrees <- as.factor(bromsize$alltrees) 
bromsize <-  unique(bromsize)
###make names the same and join
colnames(herbtest) <- c("Site", "alltrees", "Bromeliad", "Abundance", "Richness")
colnames(bromsize) <- c("Site", "alltrees", "Bromeliad", "bigleaf")
herbtest <- 
  bromsize %>% 
  left_join(herbtest)
str(herbtest)
herbtest$alltrees <- as.factor(herbtest$alltrees)
herbtest$Bromeliad <- as.factor(herbtest$Bromeliad)
str(herbtest)
herbtest <- 
  herbtest %>% 
  unite(bromname, alltrees, Bromeliad, sep="_", remove =F) %>% 
  dplyr::select(-Bromeliad)
###adding empty broms and removing the fallen one
herbtest$Abundance[is.na(herbtest$Abundance)] <- 0
herbtest$Richness[is.na(herbtest$Richness)] <- 0
herbtest <- 
  herbtest %>% 
  filter(bromname != "ER_F14_a")
##Plotamos pues
plot(herbtest$Abundance ~ herbtest$bigleaf)
plot(herbtest$Richness ~ herbtest$bigleaf)
plot(herbtest$Richness ~ herbtest$Abundance)

# Bromzynonest inside testing --------------------------------------------------------

#Size vs richness and abundance overall
##How are the two correlated
plot(sizetest$Richness ~ sizetest$Abundance)
cor.test(sizetest$Richness, sizetest$Abundance)
##Models
bromsizemodel_1 <- 
  glmer.nb(Abundance ~ log(bigleaf) +(1|Site/alltrees),
           data = sizetest)
plot(bromsizemodel_1)
qqnorm(residuals(bromsizemodel_1))
qqline(residuals(bromsizemodel_1), col = "red")
summary(bromsizemodel_1)
Anova(bromsizemodel_1)
getME(bromsizemodel_1, "glmer.nb.theta")

bromsizemodel_2 <- 
  glmer.nb(Richness ~ log(bigleaf) + (1|Site/alltrees),
           data = sizetest)
plot(bromsizemodel_2)
qqnorm(residuals(bromsizemodel_2))
qqline(residuals(bromsizemodel_2), col = "red")
summary(bromsizemodel_2)
Anova(bromsizemodel_2)
#Size vs richness and abundance inside predator
bromsizemodel_3 <- 
  glmer.nb(Abundance ~ log(bigleaf) + (1|Site/alltrees), data = predtest)
plot(bromsizemodel_3)
qqnorm(residuals(bromsizemodel_3))
qqline(residuals(bromsizemodel_3), col = "red")
summary(bromsizemodel_3)
Anova(bromsizemodel_3)
bromsizemodel_4 <- 
  glmer.nb(Richness ~ log(bigleaf) + (1|Site/alltrees), data = predtest)
plot(bromsizemodel_4)
qqnorm(residuals(bromsizemodel_4))
qqline(residuals(bromsizemodel_4), col = "red")
summary(bromsizemodel_4)
Anova(bromsizemodel_4)
#Size vs richness and abundance inside herbivore
bromsizemodel_5 <- 
  glmer.nb(Abundance ~ log(bigleaf) + (1|Site/alltrees), data = herbtest)
plot(bromsizemodel_5)
qqnorm(residuals(bromsizemodel_5))
qqline(residuals(bromsizemodel_5), col = "red")
summary(bromsizemodel_5)
Anova(bromsizemodel_5)

bromsizemodel_6 <- 
  glmer.nb(Richness ~ log(bigleaf) + (1|Site/alltrees), data = herbtest)
plot(bromsizemodel_6)
qqnorm(residuals(bromsizemodel_6))
qqline(residuals(bromsizemodel_6), col = "red")
summary(bromsizemodel_6)
Anova(bromsizemodel_6)




##Now plot with line
##Create PDF
pdf("richness_abundance_broms.pdf",height = 10,width = 10)
##set par
par(mfrow = c(2,1))
plot(sizetest$Abundance ~ sizetest$bigleaf, pch = 20, 
     ylab = "Arthropod abundance", xlab = "Largest leaf length (cm)")
coef <- fixef(bromsizemodel_1)
abline(coef = coef, col = "red", lwd = 2)
plot(sizetest$Richness ~ sizetest$biglea, pch = 20, 
     ylab = "Arthropod richness", xlab = "Largest leaf length (cm)")
coef <- fixef(bromsizemodel_2)
abline(coef = coef, col = "red", lwd = 2)
##close dev.off()
dev.off()
##Bring par back
par(mfrow = (c(1,1)))
##Now plot with line
##Create PDF
pdf("richness_abundance_broms.pdf",height = 10,width = 10)
##set par
par(mfrow = c(2,1))
plot(predtest$Abundance ~ predtest$bigleaf, pch = 20, 
     ylab = "Predator abundance", xlab = "Largest leaf length (cm)")
coef <- fixef(bromsizemodel_3)
abline(coef = coef, col = "red", lwd = 2)
plot(predtest$Richness ~ predtest$biglea, pch = 20, 
     ylab = "Predator richness", xlab = "Largest leaf length (cm)")
coef <- fixef(bromsizemodel_4)
abline(coef = coef, col = "red", lwd = 2)
##close dev.off()
dev.off()
##Bring par back
par(mfrow = (c(1,1)))
##Now plot with line
##Create PDF
pdf("richness_abundance_broms.pdf",height = 10,width = 10)
##set par
par(mfrow = c(2,1))
plot(herbtest$Abundance ~ herbtest$bigleaf, pch = 20, 
     ylab = "herbivore abundance", xlab = "Largest leaf length (cm)")
coef <- fixef(bromsizemodel_3)
abline(coef = coef, col = "red", lwd = 2)
plot(herbtest$Richness ~ herbtest$biglea, pch = 20, 
     ylab = "herbivore richness", xlab = "Largest leaf length (cm)")
coef <- fixef(bromsizemodel_4)
abline(coef = coef, col = "red", lwd = 2)
##close dev.off()
dev.off()
##Bring par back
par(mfrow = (c(1,1)))








