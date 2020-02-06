#Data preparation

#Load packages
##Data tidying
library(plyr)
library(dplyr)
library(tidyr)
##Modelling
library(lme4)
library(afex)
library(DHARMa)
library(car)
library(MASS)
##Visualising
library(visreg)
library(effects)
library(ggeffects)
library(ggplot2)
library(gridExtra)
library(gridGraphics)
##Diversity
library(vegan)

#Functions
##not in
'%notin%' <- 
  Negate('%in%')

#Load and check data
distance <- # Please download data from repository
  read.csv("Data/distance.csv", stringsAsFactors = F)
str(distance)
damage <- # Please download data from repository
  read.csv("Data/damage.csv", stringsAsFactors = F)
str(damage)
vacuum <- # Please download data from repository
  read.csv("Data/vacuum.csv", stringsAsFactors = F)
str(vacuum)
dissection <- # Please download data from repository
  read.csv("Data/dissection.csv", stringsAsFactors = F)
str(dissection)


# Distance and index--------------------------------------------------
#Tidying 
str(distance)
##Remove the extra columns
distance <- 
  distance[,1:10]
str(distance)

##Add missing bromeliad
si <- 
  c("ER", "ER", "ER", "ER")
sa <- 
  c("B", "B", "B", "B")
da <- 
  c("02.05.2017", "02.05.2017", "02.05.2017", "02.05.2017")
tree <- 
  c("B3", "B3", "B3", "B3")
trea <- 
  c("wr", "wr", "wr", "wr")
sq <- 
  c("i", "ii", "iii", "iv")
br <- 
  c("d", "d", "d", "d")
dis <- 
  c(NA, NA, NA, NA)
dia <- 
  c(NA, NA, NA, NA)
lo <- 
  c(NA, NA, NA, NA)
missing_brom <- 
  data.frame(si, sa, da, tree, trea, sq, br, dis, dia, lo)
names(missing_brom) <- 
  names(distance)
distance <- 
  rbind(distance, missing_brom)

##Correct wrong  quadrat distance
distance$Square[distance$Site == "DO" 
                & distance$Tree == "F23"
                & distance$Bromeliad == "b"
                &distance$Distance == 167] <- 
  "ii"
distance$Square[distance$Site == "ER" 
                & distance$Tree == "M12"
                & distance$Bromeliad == "d"
                &distance$Distance == 201.9] <- 
  "iii"

#Add new variables
##Height and volume
distance <- 
  distance %>% 
  unite(alltrees, Site, Tree, sep = "_", remove = F) %>% 
  unite(quadrats, alltrees, Square, sep = "_", remove = F) %>% 
  mutate(height = (Longest_leaf_length^2- (Diameter/2)^2)^0.5) %>% 
  mutate(volume = pi*((Diameter/2)^2)*height/3) 
##Proximity index based on volume
calc_index <- 
  distance %>% 
  mutate(largeleaf = Longest_leaf_length/Distance) %>% 
  dplyr::select(alltrees, quadrats, largeleaf) %>% 
  group_by(alltrees, quadrats) %>% 
  summarise_all(funs(sum))

###Bind and change to appropriate names
distance <- 
  distance %>%
  left_join(calc_index) %>% 
  rename(Bromeliad = Bromeliad_.) %>% 
  rename(treetype = Treatment)









# Damage and bromeliad parameters------------------------------------------------------
#Tidying
str(damage)
##Coerce leaf ID into character
damage$Leaf <- 
  as.character(damage$Leaf)
str(damage)

#Tree mistakes
###One extra tree
damage$Tree[damage$Tree == "Al1"] <- 
  "AI1"
###Two trees with wrong name
levels(damage$Tree) <- 
  c(levels(damage$Tree), "J13", "J19")
damage$Tree[damage$Tree == "J15"] <- 
  "J13"
damage$Tree[damage$Tree == "J21"] <- 
  "J19"
damage$Tree <- droplevels(damage$Tree)

###Some trees with more than one treatment
damage$Treatment[damage$Tree == "C3"] <- 
  "w"
damage$Treatment[damage$Site == "DO" 
                 & damage$Tree == "G6"] <- 
  "wr"
damage$Treatment[damage$Site == "CP" 
                 & damage$Tree == "G6"] <- 
  "w"

#Bring in new values
##Percentages
leafdamage <- 
  damage %>% 
  rename(actual = Actual_area) %>% 
  rename(original = Original_area) %>% 
  mutate(okarea = actual - Miner - Hemiptera) %>% 
  mutate(propdamage = 1- okarea/original) %>% 
  dplyr::select(-Analyst, -Day, -Miner, -Hemiptera) %>% 
  unite(alltrees, Site, Tree, sep = "_", remove = F) %>% 
  unite(quadrats, alltrees, Square, sep = "_", remove = F) %>% 
  rename(treetype = Treatment)

###Convert to factors
table(leafdamage$Square)
str(leafdamage)
leafdamage$alltrees <-
  as.factor(leafdamage$alltrees)
leafdamage$quadrats <-
  as.factor(leafdamage$quadrats)
leafdamage$Tree <-
  as.factor(leafdamage$Tree)
leafdamage$treetype <-
  as.factor(leafdamage$treetype)

##Bromeliad number and size
calc_bromnumber <- 
  distance %>% 
  dplyr::select(alltrees, Longest_leaf_length, Bromeliad)
calc_bromnumber <- 
  unique(calc_bromnumber)
calc_bromnumber$Bromeliad <- 
  1
calc_bromnumber <- 
  calc_bromnumber %>% 
  group_by(alltrees) %>% 
  summarise_all(funs(sum)) %>% 
  mutate(meanvolume = Longest_leaf_length/Bromeliad) %>% 
  rename(totalvolume = Longest_leaf_length) %>% 
  rename(broms = Bromeliad)


##Convert to factors
str(leafdamage)
leafdamage$alltrees <-
  as.factor(leafdamage$alltrees)

##Pool per quadrat
calc_leafdamage <- 
  na.exclude(leafdamage) %>% 
  dplyr::select(alltrees, quadrats, Sampling, propdamage) %>% 
  group_by(alltrees, quadrats, Sampling) %>% 
  summarise_all(funs(mean))

#Add bromeliead presence variable to leafdamage
leafdamage$presence <- 
  "no"
leafdamage$presence[which(leafdamage$treetype == "wr" 
                          & leafdamage$Sampling == "B")] <- 
  "yes"
leafdamage$presence[which(leafdamage$treetype == "w")] <- 
  "yes"
leafdamage$presence <- 
  as.factor(leafdamage$presence)
# Vacuum sampling ----------------------------------------------------
#Tidying
str(vacuum)
##Change tretament to tree type
vacuum <- 
  vacuum %>% 
  rename(treetype = Treatment)
##Remove extra columns
vacuum <-  
  vacuum[,1:17]
##Remove invertebrates that will be excluded from analyses (e.g. mosquitoes and blackflies harassing the researchers)
vacuum <- 
  vacuum[!vacuum$Species == "Remove",]
vacuum <- 
  vacuum[!vacuum$Species == "Remove?",]
##Contrast between feeding behaviour of cockroaches and oothecas
vacuum$Diet[which(vacuum$Subfamily == "Ootheca")] <- 
  "nada"
vacuum$Diet <- 
  droplevels(vacuum$Diet)
##Remove the recheck indication for ants to double-check
vacuum$Suborder[which(vacuum$Family == "Formicidae")] <- 
  "Apocrita"
vacuum$Suborder <- 
  droplevels(vacuum$Suborder)
##Add Acari as Order name for mites, even though it is not
levels(vacuum$Order) <- 
  c(levels(vacuum$Order), "Acari")
vacuum$Order[which(vacuum$Order == "")] <- 
  "Acari"
vacuum$Order <- 
  droplevels(vacuum$Order)
##Add 1 where I forgot to
vacuum$Abundance[which(is.na(vacuum$Abundance))] <-
  1
##Double-check
str(vacuum)
##One tree with two treatments
vacuum$treetype[which(vacuum$Tree == "D18")] <- 
  "wo"



#Combine specimens per quadrat based on morphospecies
clean_vacuum <- 
  vacuum %>% 
  dplyr::select(Order, Suborder, Family, Morphospecies, Diet, Site, Sampling, Tree, treetype, Square, Abundance) %>% 
  unite(alltrees, Site, Tree, sep ="_", remove =F) %>% 
  unite(quadrats, alltrees, Square, sep ="_", remove = F) %>% 
  group_by(Order, Suborder, Family, Morphospecies, Diet, Site, alltrees, quadrats, Sampling, Tree, treetype, Square) %>% 
  summarise_all(funs(sum)) 

#Add proximity index
clean_vacuum <- 
  clean_vacuum %>% 
  left_join(calc_index)
clean_vacuum$largeleaf[clean_vacuum$treetype == "wo"] <- 
  0
clean_vacuum$largeleaf[which(clean_vacuum$treetype == "wr" 
                         & clean_vacuum$Sampling == "A")] <- 
  0


#Empty sample vector
Site <- 
  c("CP", "CP", "CP", "CP", "CP", "DO", "DO", "DO", "DO", "DO", "DO", "DO", "DO", "DO", "DO", "DO", "ER", "ER", "ER", "ER", "ER", "ER", "ER", "ER")
Sampling <- 
  c("B", "A", "A","B", "A", "B", "B", "B", "B", "A", "A", "A", "B", "A", "B", "A","B", "B", "A", "B", "B", "A", "B", "B")
tree <- 
  c("G1", "I1", "J1", "J6", "L10", "A10", "C16", "C8", "F10", "G9", "H5", "H7", "H7", "H9", "I1", "I4", "B3", "B3", "C3", "G6", "J13", "O9", "S11", "S11")
squ <-
  c("iv", "ii", "iii", "ii", "iv", "ii", "ii", "iv", "ii", "ii", "ii", "ii", "iv", "ii", "iii", "iv", "iii", "iv", "iii", "iii", "iv", "iii", "i", "ii")
empty_samples <- 
  data.frame(cbind(Site, Sampling, tree, squ))
empty_samples <- 
  empty_samples %>% 
  unite(alltrees, Site, tree, sep= "_", remove = F) %>% 
  unite(quadrats, alltrees, squ, remove = F) %>% 
  mutate(Abundance = 0) %>% 
  dplyr::select(alltrees, quadrats, Sampling, Abundance)
str(empty_samples)


# Bromeliad dissection ----------------------------------------------------
str(dissection)
#Change treatment to tree type
dissection <- 
  dissection %>% 
  rename(treetype = Treatment)
#Remove invertebrates that will be excluded from analyses (e.g. aquatic ones)
dissection <- 
  dissection[!dissection$Species == "Remove",]
dissection <- 
  dissection[!dissection$Species == "Remove?",]
##Contrast between feeding behaviour of cockroaches and oothecas
dissection$Diet[which(dissection$Subfamily == "Ootheca")] <- 
  "nada"
dissection$Diet <- 
  droplevels(dissection$Diet)
##Additional level in nest factor
levels(dissection$Abundance)
dissection$Abundance[which(dissection$Abundance == "nest ")] <- 
  "nest"
dissection$Abundance <- 
  droplevels(dissection$Abundance)
##Add 1 where I forgot to
dissection$Abundance[dissection$Abundance == ""] <- 
  1
dissection$Abundance[dissection$Abundance == " "] <- 
  1
dissection$Abundance <- 
  droplevels(dissection$Abundance)
##Double-check
str(dissection)
levels(dissection$Tree)
levels(dissection$Tree) <- 
  c(levels(dissection$Tree), "J5")
dissection$Tree[dissection$Tree == "J5 "] <- 
  "J5"
dissection$Tree <- droplevels(dissection$Tree)
##Removing rows with individual ant counts when nest has already been counted
dissection <- 
  dissection[!(dissection$Abundance== 16 & dissection$Tree== "B3" & dissection$Morphospecies =="Ant_A"),]
dissection <- 
  dissection[!(dissection$Abundance== 1 & dissection$Tree== "E8" & dissection$Morphospecies =="Ant_X"),]
dissection <- 
  dissection[!(dissection$Abundance== 1 & dissection$Tree== "O18" & dissection$Morphospecies =="Ant_X"),]

###Create vector for empty bromeliads
####F14a NA because fell from tree
sit <- 
  c("DO", "ER", "CP", "CP", "DO")
sam <- 
  c("B", "A", "B", "B", "B")
tree <-
  c("E11", "F14", "J1", "J2", "D7") 
trea <- 
  c("wr", "w", "wr", "wr", "wr")
brom <- 
  c("e", "a", "b", "c", "a")
div <- 
  c(0, NA, 0, 0, 0)
empty_broms <-
  rbind(sit, sam, tree, trea, brom, div)

#Final frame
##Add nest category
###Create column and add approximate nest size levels
dissection$nestcat <- 
  NA
levels(dissection$Abundance) <- 
  c(levels(dissection$Abundance), 0)
dissection$Abundance[dissection$Abundance == "nest "] <- 
  "nest"
levels(dissection$Abundance) <-  
  c(levels(dissection$Abundance), 50, 100, 200, 1000)
###Approximate nest size
dissection$Abundance[which(dissection$Abundance == "nest" 
                       & dissection$Morphospecies == "Ant_X")] <- 
  "50"
dissection$Abundance[which(dissection$Abundance == "nest" 
                       & dissection$Morphospecies == "Ant_E")] <- 
  "50"
dissection$Abundance[which(dissection$Abundance == "nest" 
                       & dissection$Morphospecies == "Ant_AA")] <- 
  "50"
dissection$Abundance[which(dissection$Abundance == "nest" 
                       & dissection$Morphospecies == "Ant_BF")] <- 
  "100"
dissection$Abundance[which(dissection$Abundance == "nest" 
                       & dissection$Morphospecies == "Ant_AK")] <- 
  "50"
dissection$Abundance[which(dissection$Abundance == "nest" 
                       & dissection$Morphospecies == "Ant_AB")] <- 
  "100"
dissection$Abundance[which(dissection$Abundance == "nest" 
                       & dissection$Morphospecies == "Ant_AS")] <- 
  "100"
dissection$Abundance[which(dissection$Abundance == "nest" 
                       & dissection$Morphospecies == "Ant_H")] <- 
  "100"
dissection$Abundance[which(dissection$Abundance == "nest" 
                       & dissection$Morphospecies == "Ant_F")] <- 
  "100"
dissection$Abundance[which(dissection$Abundance == "nest" 
                       & dissection$Morphospecies == "Ant_AH")] <- 
  "100"
dissection$Abundance[which(dissection$Abundance == "nest" 
                       & dissection$Morphospecies == "Ant_BD")] <- 
  "100"
dissection$Abundance[which(dissection$Abundance == "nest" 
                       & dissection$Morphospecies == "Ant_A")] <- 
  "200"
dissection$Abundance[which(dissection$Abundance == "nest" 
                       & dissection$Morphospecies == "Ant_G")] <- 
  "200"
dissection$Abundance[which(dissection$Abundance == "nest" 
                       & dissection$Morphospecies == "Ant_Z")] <- 
  "200"
dissection$Abundance[which(dissection$Abundance == "100" 
                       & dissection$Morphospecies == "Ant_AH" 
                       & dissection$Tree =="G11" 
                       & dissection$Bromeliad == "c")] <- 
  "1000"
###Add nest category
dissection$nestcat[which(dissection$Abundance == "50")] <- 
  "A"
dissection$nestcat[which(dissection$Abundance == "100")] <- 
  "B"
dissection$nestcat[which(dissection$Abundance == "200")] <- 
  "C"
dissection$nestcat[which(dissection$Abundance == "1000")] <- 
  "W"
##Finish and double-check
dissection$Abundance <-  
  as.numeric(as.character(dissection$Abundance))
dissection$nestcat <-  
  as.factor(dissection$nestcat)
str(dissection)
##Combine frames
clean_dissection <- 
  dissection %>% 
  unite(alltrees, Site, Tree, sep ="_", remove =F) %>% 
  dplyr::select(Order, Suborder, Family, Site, Sampling, alltrees, Tree, treetype, Bromeliad, nestcat,Morphospecies, Diet, Abundance) %>% 
  group_by(Order, Suborder, Family, Site, Sampling, alltrees, Tree, treetype, Bromeliad, nestcat, Morphospecies, Diet) %>% 
  summarise_all(funs(sum))
##Error in original species name
clean_dissection$Abundance[clean_dissection$Abundance == 600] <- 
  400

##Add functional groups (hereafter 'kinds')
###Create a column for kind
clean_dissection$kind <- 
  NA
###Add categories, based on taxonomy/behavior
clean_dissection$kind <- 
  ifelse(
    ##Herbivores
    clean_dissection$Order == "Gastropoda" & 
      clean_dissection$Diet == "herb",  "snail",
    ifelse(
      clean_dissection$Order == "Lepidoptera"& 
        clean_dissection$Diet == "herb",  "lepi",
      ifelse(
        clean_dissection$Order == "Coleoptera" &
          clean_dissection$Diet %in% c("herb", "poll"),  "herbeetle",
        ifelse(
          clean_dissection$Morphospecies %in% c("Hemiptera_BI", "Hemiptera_BP", "Homoptera_CF","Homoptera_CG",
                                          "Homoptera_CJ", "Hemiptera_F"),"jump",
          ifelse(
            clean_dissection$Family == "Aphididae", "scaleaphid",
            ifelse(
              clean_dissection$Morphospecies %in% c("Scale_K", "Scale_I", "Scale_H", "Scale_G", "Scale_E", 
                                              "Scale_D", "Scale_C", "Scale_B", "Scale_A", "Aphid_A"),  "scaleaphid",
              ifelse(
                clean_dissection$Suborder == "Auchenorrhyncha",  "jump",
                ifelse(
                  clean_dissection$Suborder == "Heteroptera" &
                    clean_dissection$Diet == "herb",  "heteroherb",
                  ifelse(
                    clean_dissection$Order == "Orthoptera" &
                      clean_dissection$Diet == "herb",  "ortho",
                    ##Predators
                    ifelse(
                      clean_dissection$Family =="Formicidae",  "ants",
                      ifelse(
                        clean_dissection$Family == "Vespidae",  "wasps",
                        ifelse(
                          clean_dissection$Order == "Blattodea" &
                            clean_dissection$Diet =="scav",  "roaches",
                          ifelse(
                            clean_dissection$Order == "Neuroptera",  "lacewings",
                            ifelse(
                              clean_dissection$Order == "Coleoptera" &
                                clean_dissection$Diet == "pred",  "predbeetle",
                              ifelse(
                                clean_dissection$Suborder == "Heteroptera" &
                                  clean_dissection$Diet == "pred",  "heteropred",
                                ifelse(
                                  clean_dissection$Order == "Opiliones",  "opilio",
                                  ifelse(
                                    clean_dissection$Order == "Diptera" &
                                      clean_dissection$Diet == "pred",  "predflies",
                                    ifelse(
                                      clean_dissection$Order == "Diptera" &
                                        clean_dissection$Diet == "herb",  "herbflies",
                                      ifelse(
                                        clean_dissection$Order == "Dermaptera", "earwig",
                                        ifelse(
                                          clean_dissection$Order == "Mantodea", "mantid",
                                          ifelse(
                                            clean_dissection$Order == "Araneae" &
                                              clean_dissection$Morphospecies %in% c("Spider_A", "Spider_X", "Spider_Z", "Spider_AC", "Spider_AD", "Spider_AL",
                                                                              "Spider_AU", "Spider_AW", "Spider_AZ", "Spider_BA", "Spider_BF", "Spider_BO",
                                                                              "Spider_BG", "Spider_BR", "Spider_BU", "Spider_BV", "Spider_BW", "Spider_G",
                                                                              "Spider_BZ", "Spider_CI", "Spider_CJ", "Spider_CQ", "Spider_CW", "Spider_DD",
                                                                              "Spider_DI", "Spider_DO", "Spider_DQ", "Spider_DU", "Spider_DV", "Spider_I",
                                                                              "Spider_DY", "Spider_EL", "Spider_EQ", "Spider_EM", "Spider_EY", "Spider_S",
                                                                              "Spider_FK", "Spider_FM", "Spider_GB"),  "webspids", 
                                            ifelse(
                                              clean_dissection$Order == "Araneae" &
                                                clean_dissection$Morphospecies %notin% c("Spider_A", "Spider_X", "Spider_Z", "Spider_AC", "Spider_AD", "Spider_AL",
                                                                                   "Spider_AU", "Spider_AW", "Spider_AZ", "Spider_BA", "Spider_BF", "Spider_BO",
                                                                                   "Spider_BG", "Spider_BR", "Spider_BU", "Spider_BV", "Spider_BW", "Spider_G",
                                                                                   "Spider_BZ", "Spider_CI", "Spider_CJ", "Spider_CQ", "Spider_CW", "Spider_DD",
                                                                                   "Spider_DI", "Spider_DO", "Spider_DQ", "Spider_DU", "Spider_DV", "Spider_I",
                                                                                   "Spider_DY", "Spider_EL", "Spider_EQ", "Spider_EM", "Spider_EY", "Spider_S",
                                                                                   "Spider_FK", "Spider_FM", "Spider_GB"), "huntspids", 
                                              ifelse(
                                                clean_dissection$Diet =="para", "para", 
                                                ifelse(
                                                  clean_dissection$Suborder != "", as.character(clean_dissection$Suborder), as.character(clean_dissection$Order)
                                                ))))))))))))))))))))))))

# Kinds of herbivores and predators, and other arthropods ---------------------------------------
#Create a column for kind
clean_vacuum$kind <- 
  NA
##Add categories, based on taxonomy/behavior
clean_vacuum$kind <- 
  ifelse(
    ##Herbivores
    clean_vacuum$Order == "Gastropoda" & 
      clean_vacuum$Diet == "herb",  "snail",
    ifelse(
      clean_vacuum$Order == "Lepidoptera"& 
        clean_vacuum$Diet == "herb",  "lepi",
      ifelse(
        clean_vacuum$Order == "Coleoptera" &
          clean_vacuum$Diet %in% c("herb", "poll"),  "herbeetle",
        ifelse(
          clean_vacuum$Morphospecies %in% c("Hemiptera_BI", "Hemiptera_BP", "Homoptera_CF","Homoptera_CG",
                                        "Homoptera_CJ", "Hemiptera_F"),"jump",
          ifelse(
            clean_vacuum$Family == "Aphididae", "scaleaphid",
            ifelse(
              clean_vacuum$Morphospecies %in% c("Scale_K", "Scale_I", "Scale_H", "Scale_G", "Scale_E", 
                                            "Scale_D", "Scale_C", "Scale_B", "Scale_A", "Aphid_A"),  "scaleaphid",
              ifelse(
                clean_vacuum$Suborder == "Auchenorrhyncha",  "jump",
                ifelse(
                  clean_vacuum$Suborder == "Heteroptera" &
                    clean_vacuum$Diet == "herb",  "heteroherb",
                  ifelse(
                    clean_vacuum$Order == "Orthoptera" &
                      clean_vacuum$Diet == "herb",  "ortho",
                    ##Predators
                    ifelse(
                      clean_vacuum$Family =="Formicidae",  "ants",
                      ifelse(
                        clean_vacuum$Family == "Vespidae",  "wasps",
                        ifelse(
                          clean_vacuum$Order == "Blattodea" &
                            clean_vacuum$Diet =="scav",  "roaches",
                          ifelse(
                            clean_vacuum$Order == "Neuroptera",  "lacewings",
                            ifelse(
                              clean_vacuum$Order == "Coleoptera" &
                                clean_vacuum$Diet == "pred",  "predbeetle",
                              ifelse(
                                clean_vacuum$Suborder == "Heteroptera" &
                                  clean_vacuum$Diet == "pred",  "heteropred",
                                ifelse(
                                  clean_vacuum$Order == "Opiliones",  "opilio",
                                  ifelse(
                                    clean_vacuum$Order == "Diptera" &
                                      clean_vacuum$Diet == "pred",  "predflies",
                                    ifelse(
                                      clean_vacuum$Order == "Diptera" &
                                        clean_vacuum$Diet == "herb",  "herbflies",
                                      ifelse(
                                        clean_vacuum$Order == "Dermaptera", "earwig",
                                        ifelse(
                                          clean_vacuum$Order == "Mantodea", "mantid",
                                          ifelse(
                                            clean_vacuum$Order == "Araneae" &
                                              clean_vacuum$Morphospecies %in% c("Spider_A", "Spider_X", "Spider_Z", "Spider_AC", "Spider_AD", "Spider_AL",
                                                                            "Spider_AU", "Spider_AW", "Spider_AZ", "Spider_BA", "Spider_BF", "Spider_BO",
                                                                            "Spider_BG", "Spider_BR", "Spider_BU", "Spider_BV", "Spider_BW", "Spider_G",
                                                                            "Spider_BZ", "Spider_CI", "Spider_CJ", "Spider_CQ", "Spider_CW", "Spider_DD",
                                                                            "Spider_DI", "Spider_DO", "Spider_DQ", "Spider_DU", "Spider_DV", "Spider_I",
                                                                            "Spider_DY", "Spider_EL", "Spider_EQ", "Spider_EM", "Spider_EY", "Spider_S",
                                                                            "Spider_FK", "Spider_FM", "Spider_GB", "Spider_L"),  "webspids", 
                                            ifelse(
                                              clean_vacuum$Order == "Araneae" &
                                                clean_vacuum$Morphospecies %notin% c("Spider_A", "Spider_X", "Spider_Z", "Spider_AC", "Spider_AD", "Spider_AL",
                                                                                 "Spider_AU", "Spider_AW", "Spider_AZ", "Spider_BA", "Spider_BF", "Spider_BO",
                                                                                 "Spider_BG", "Spider_BR", "Spider_BU", "Spider_BV", "Spider_BW", "Spider_G",
                                                                                 "Spider_BZ", "Spider_CI", "Spider_CJ", "Spider_CQ", "Spider_CW", "Spider_DD",
                                                                                 "Spider_DI", "Spider_DO", "Spider_DQ", "Spider_DU", "Spider_DV", "Spider_I",
                                                                                 "Spider_DY", "Spider_EL", "Spider_EQ", "Spider_EM", "Spider_EY", "Spider_S",
                                                                                 "Spider_FK", "Spider_FM", "Spider_GB", "Spider_L"), "huntspids", 
                                              ifelse(
                                                clean_vacuum$Diet =="para", "para", 
                                                ifelse(
                                                  clean_vacuum$Suborder != "", as.character(clean_vacuum$Suborder), as.character(clean_vacuum$Order)
                                                ))))))))))))))))))))))))




# Predators and herbivores by quadrat -------------------------------------
#Reduce per quadrat and combining everything
calc_quadrats <- 
  leafdamage %>% 
  dplyr::select(Site, alltrees, quadrats, treetype, Sampling) %>% 
  unique()
##Add presence column
calc_quadrats$presence <- 
  "no"
calc_quadrats$presence[which(calc_quadrats$treetype == "wr" 
                             & calc_quadrats$Sampling == "B")] <- 
  "yes"
calc_quadrats$presence[which(calc_quadrats$treetype == "w")] <- 
  "yes"


#Build frame with everything
calc_dichos <- 
  clean_vacuum %>% 
  ungroup() %>% 
  dplyr::select(alltrees, quadrats, Sampling, Abundance) %>% 
  group_by(alltrees, quadrats, Sampling) %>% 
  summarise_all(funs(sum)) %>% 
  rename(todo = Abundance)

#Isolate Asian citrus psyllid (only identified Liviidae)
calc_psyllid <- 
  clean_vacuum %>% 
  ungroup() %>% 
  filter(Family == "Liviidae") %>% 
  dplyr::select(quadrats, Sampling, Abundance) %>% 
  rename(psyllid = Abundance)

#All predators, parasitoids and herbivores
calc_predherb <- 
  clean_vacuum %>% 
  ungroup() %>% 
  filter(Diet %in% c("pred", "scav", "omni", "para", "herb")) %>% 
  ##Removing irrelevant families
  filter(Family %notin% c("Gryllidae", "Anthicidae", "Aderidae", "Tenebrionidae")) %>%
  dplyr::select(alltrees, quadrats, Diet, Sampling, Abundance) %>% 
  group_by(alltrees,quadrats, Diet, Sampling) %>%
  summarise_all(funs(sum)) %>% 
  spread(key=Diet, Abundance, fill =0) %>% 
  ##grouping potential preds
  mutate(predpara = pred + scav + omni + para) %>% 
  mutate(preds = pred + scav + omni) %>% 
  dplyr::select(-pred, -scav, -omni)


#Contrast between bromeliad predators, tree and mobile predators
calc_bromstuff <- 
  clean_dissection %>% 
  ungroup() %>% 
  ###Filter tourist taxa, based on observation and published literature
  ###Mantids
  filter(Order != "Mantodea") %>% 
  ###Flies
  filter(Order != "Diptera") %>% 
  ###Social wasps
  filter(Family != "Vespidae") %>% 
  ###One parasitoid
  filter(Morphospecies != "Wasp_N") %>% 
  ##Neuroptera larvae
  filter(Order != "Neuroptera") %>% 
  ###Scales, likely associated with ants, but we found only 4
  filter(Suborder != "Sternorrhyncha") %>% 
  ##Spider CJ
  filter(Morphospecies != "Spider_CJ") %>% 
  ###Bees
  filter(Morphospecies != "Bee_B") %>% 
  ###Vagrant ants, not flying ants, because likely to look for nesting ground there
  filter(Morphospecies %notin% c("Ant_AJ", "Ant_AL", "Ant_BA", "Ant_BE",
                                 "Ant_C", "Ant_J", "Ant_K", "Ant_L")) %>% 
  dplyr::select(Morphospecies) %>% 
  unique() %>% 
  mutate(where = "brom")
clean_vacuum <- 
  clean_vacuum %>% 
  left_join(calc_bromstuff)
##Add mobile and tree predators
clean_vacuum$where <- 
  ifelse(
    is.na(clean_vacuum$where) & clean_vacuum$Diet %in% c("pred","omni", "scav", "para") & 
      clean_vacuum$Family %in% c("Coccinellidae", "Lampyridae", "Cleridae", "Geocoridae", "Nabidae", "Vespidae") | 
      is.na(clean_vacuum$where) & clean_vacuum$Order == "Mantodea" | 
      is.na(clean_vacuum$where) & clean_vacuum$Order == "Neuroptera" & clean_vacuum$Morphospecies != "Neurolarva_A" |
      is.na(clean_vacuum$where) & clean_vacuum$kind %in% c("predflies", "para"), "mobi",
    ifelse(
      is.na(clean_vacuum$where) & clean_vacuum$Diet %in% c("pred","omni", "scav", "para") &
        clean_vacuum$Family %notin% c("Gryllidae", "Anthicidae", "Aderidae", "Tenebrionidae","Coccinellidae", 
                                  "Lampyridae", "Cleridae", "Geocoridae", "Nabidae", "Vespidae") & 
        clean_vacuum$Order %notin% c("Mantodea", "Neuroptera") & clean_vacuum$kind %notin% c("predflies", "para") | 
        is.na(clean_vacuum$where) & clean_vacuum$Morphospecies == "Neurolarva_A", "tree", clean_vacuum$where
    ))
clean_vacuum$where[is.na(clean_vacuum$where)] <- 
  "tbd"

##Bind to vacuum samples and make small dataframe to see which are found in trees
calc_dissectiontree <- 
  clean_vacuum %>% 
  ungroup() %>% 
  filter(where == "brom") %>% 
  dplyr::select(Order, Family, Morphospecies, Abundance) %>% 
  group_by(Order, Family, Morphospecies) %>% 
  summarise_all(funs(sum))
##Now make dataframes
###Bromeliad-associated predators
calc_brompred <- 
  clean_vacuum %>% 
  ungroup() %>% 
  filter(where == "brom") %>% 
  filter(Diet %in% c("pred","omni","scav")) %>% 
  filter(Family %notin% c("Gryllidae", "Anthicidae", "Aderidae", "Tenebrionidae")) %>% 
  dplyr::select(Sampling, alltrees, quadrats,Abundance) %>% 
  group_by(Sampling, alltrees, quadrats) %>% 
  summarise_all(funs(sum)) %>% 
  rename(brompred = Abundance)

###Aerial predators
calc_mobipred <- 
  clean_vacuum %>% 
  ungroup() %>% 
  filter(where == "mobi" & Diet != "para") %>% 
  dplyr::select(Sampling, alltrees, quadrats,Abundance) %>% 
  group_by(Sampling, alltrees, quadrats) %>% 
  summarise_all(funs(sum)) %>% 
  rename(mobipred = Abundance)

###Tree-associated predators
calc_arbopred <- 
  clean_vacuum %>% 
  ungroup() %>% 
  filter(where == "tree") %>% 
  dplyr::select(Sampling, alltrees, quadrats,Abundance) %>% 
  group_by(Sampling, alltrees, quadrats) %>% 
  summarise_all(funs(sum)) %>% 
  rename(arbopred = Abundance)

##Divide bromeliad-associated predators divided into kinds
calc_brompred_kinds <- 
  clean_vacuum %>% 
  ungroup() %>% 
  filter(where == "brom") %>% 
  ##not including low abundance kinds webspids and heteropreds
  filter(kind %in% c("predbeetle", "huntspids", "predflies","roaches", "opilio", "lacewings", "ants", "para")) %>% 
  dplyr::select(kind, Site, alltrees, quadrats, Sampling, treetype, Abundance) %>% 
  group_by(kind, Site, alltrees, quadrats, Sampling, treetype) %>% 
  summarise_all(funs(sum)) %>% 
  spread(key = kind, Abundance, fill =0) %>% 
  rename(bromants = ants,
         bromhuntspids = huntspids,
         bromopilio = opilio,
         brompredbeetle = predbeetle,
         bromroaches = roaches)

#Now make a clean data frame to bind anywhere
##Make sure that the rows to put NAs in belong to alltrees = CP_A2
##this tree was not vacuumed, but we have leaf damage for it
calc_kind <- 
  clean_vacuum %>% 
  ungroup() %>% 
  dplyr::select(Sampling, alltrees, quadrats, kind, Abundance) %>% 
  group_by(Sampling, alltrees, quadrats, kind) %>% 
  summarise_all(funs(sum)) %>% 
  spread(key= kind, Abundance, fill = 0)

##Bind all abundances together
calc_dichos <- 
  calc_quadrats[, c(2,3,5)] %>% 
  filter(alltrees != "CP_A2") %>% 
  left_join(calc_dichos) %>% 
  left_join(calc_predherb) %>% 
  left_join(calc_brompred) %>% 
  left_join(calc_mobipred) %>% 
  left_join(calc_arbopred) %>% 
  left_join(calc_kind) %>% 
  left_join(calc_psyllid) %>% 
  left_join(calc_brompred_kinds) %>% 
  ##Reorder
  dplyr::select(alltrees:arbopred,  herbeetle, herbflies, heteroherb, jump, lepi, 
                ortho, psyllid, scaleaphid, snail, ants, earwig, heteropred, 
                huntspids, lacewings, mantid, predbeetle, predflies, opilio,
                roaches, wasps,  webspids, para, bromants:bromroaches)
calc_dichos[,4:37][is.na(calc_dichos[,4:37])] <- 
  0
##Check column sums and remove the rare ones
colSums(calc_dichos[,7:37])
##earwigs, mantids, wasps with less than 10 in abundance
calc_dichos <- 
  calc_dichos %>% 
  dplyr::select(-earwig, -mantid, -wasps, -herbflies)




# Combine frames---------------------------
pooldamage <- 
  calc_quadrats %>% 
  left_join(calc_leafdamage) %>% 
  left_join(calc_index) %>% 
  left_join(calc_dichos)
str(pooldamage)
#Change proximity index to 0 post-removal
pooldamage$largeleaf[pooldamage$presence == "no"] <-
  0

# Transformation and centering of independent variable - quadrat level -------------------------------------------------
# Poolcenter
poolcenter <- 
  na.omit(pooldamage) %>% 
  dplyr::select( -predpara) %>% 
  ##Add phloem suckers and leaf chewers
  mutate(phloem = heteroherb + jump + scaleaphid,
         chewer = herbeetle + lepi + ortho + snail) %>% 
  ##Square root future predictor values
  mutate(largeleaf = sqrt(largeleaf),
         predcenter = preds^(1/3),
         bromcenter = brompred^(1/3),
         mobicenter = mobipred^(1/3),
         arbocenter = arbopred^(1/3),
         paracenter = para^(1/3),
         antscenter = sqrt(ants),
         heteropredcenter = sqrt(heteropred),
         huntspidscenter = sqrt(huntspids),
         lacewingscenter = sqrt(lacewings),
         predbeetlecenter = sqrt(predbeetle),
         predfliescenter = sqrt(predflies),
         opiliocenter = sqrt(opilio),
         roachescenter = sqrt(roaches),
         webspidscenter = sqrt(webspids),
         herbcenter = sqrt(herb),
         phloemcenter = sqrt(phloem),
         chewercenter = sqrt(chewer),
         bromant_center = sqrt(bromants),
         bromhuntspids_center = sqrt(bromhuntspids),
         bromopilio_center = sqrt(bromopilio),
         brompredbeetle_center = sqrt(brompredbeetle),
         bromroaches_center = sqrt(bromroaches)) %>% 
  ##Center around site means
  group_by(Site) %>% 
  mutate(largeleaf= largeleaf - mean(largeleaf),
         herbcenter = herbcenter - mean(herbcenter),
         phloemcenter = phloemcenter - mean(phloemcenter),
         chewercenter = chewercenter - mean(chewercenter),
         predcenter = predcenter - mean(predcenter),
         paracenter = paracenter - mean(paracenter),
         bromcenter = bromcenter - mean(bromcenter),
         arbocenter = arbocenter - mean(arbocenter),
         mobicenter = mobicenter - mean(mobicenter),
         antscenter = antscenter - mean(antscenter),
         heteropredcenter = heteropredcenter - mean(heteropredcenter),
         huntspidscenter = huntspidscenter - mean(huntspidscenter),
         lacewingscenter = lacewingscenter - mean(lacewingscenter),
         predbeetlecenter = predbeetlecenter - mean(predbeetlecenter),
         predfliescenter = predfliescenter - mean(predfliescenter),
         opiliocenter = opiliocenter - mean(opiliocenter),
         roachescenter = roachescenter - mean(roachescenter),
         webspidscenter = webspidscenter - mean(webspidscenter),
         bromant_center = bromant_center - mean(bromant_center),
         bromhuntspids_center = bromhuntspids_center - mean(bromhuntspids_center),
         bromopilio_center = bromopilio_center - mean(bromopilio_center),
         brompredbeetle_center = brompredbeetle_center - mean(brompredbeetle_center),
         bromroaches_center = bromroaches_center - mean(bromroaches_center))



# Transformation and centering of independent variable - tree level -------------------------------------------------
#Poolcenter
poolcenter_tree <- 
  na.omit(pooldamage) %>% 
  dplyr::select(-quadrats, -predpara) %>% 
  group_by(Site, alltrees, treetype, Sampling, presence) %>% 
  summarise_all(funs(sum)) %>% 
  ungroup() %>% 
  ##Add phloem suckers and leaf chewers
  mutate(phloem = heteroherb + jump + scaleaphid,
         chewer = herbeetle + lepi + ortho + snail) %>% 
  ##Square root future predictor values
  mutate(largeleaf = sqrt(largeleaf),
         predcenter = preds^(1/3),
         bromcenter = brompred^(1/3),
         mobicenter = mobipred^(1/3),
         arbocenter = arbopred^(1/3),
         paracenter = para^(1/3),
         antscenter = sqrt(ants),
         heteropredcenter = sqrt(heteropred),
         huntspidscenter = sqrt(huntspids),
         lacewingscenter = sqrt(lacewings),
         predbeetlecenter = sqrt(predbeetle),
         predfliescenter = sqrt(predflies),
         opiliocenter = sqrt(opilio),
         roachescenter = sqrt(roaches),
         webspidscenter = sqrt(webspids),
         herbcenter = sqrt(herb),
         phloemcenter = sqrt(phloem),
         chewercenter = sqrt(chewer),
         bromant_center = sqrt(bromants),
         bromhuntspids_center = sqrt(bromhuntspids),
         bromopilio_center = sqrt(bromopilio),
         brompredbeetle_center = sqrt(brompredbeetle),
         bromroaches_center = sqrt(bromroaches)) %>% 
  ##Center around site means
  group_by(Site) %>% 
  mutate(largeleaf= largeleaf - mean(largeleaf),
         herbcenter = herbcenter - mean(herbcenter),
         phloemcenter = phloemcenter - mean(phloemcenter),
         chewercenter = chewercenter - mean(chewercenter),
         predcenter = predcenter - mean(predcenter),
         paracenter = paracenter - mean(paracenter),
         bromcenter = bromcenter - mean(bromcenter),
         arbocenter = arbocenter - mean(arbocenter),
         mobicenter = mobicenter - mean(mobicenter),
         antscenter = antscenter - mean(antscenter),
         heteropredcenter = heteropredcenter - mean(heteropredcenter),
         huntspidscenter = huntspidscenter - mean(huntspidscenter),
         lacewingscenter = lacewingscenter - mean(lacewingscenter),
         predbeetlecenter = predbeetlecenter - mean(predbeetlecenter),
         predfliescenter = predfliescenter - mean(predfliescenter),
         opiliocenter = opiliocenter - mean(opiliocenter),
         roachescenter = roachescenter - mean(roachescenter),
         webspidscenter = webspidscenter - mean(webspidscenter),
         bromant_center = bromant_center - mean(bromant_center),
         bromhuntspids_center = bromhuntspids_center - mean(bromhuntspids_center),
         bromopilio_center = bromopilio_center - mean(bromopilio_center),
         brompredbeetle_center = brompredbeetle_center - mean(brompredbeetle_center),
         bromroaches_center = bromroaches_center - mean(bromroaches_center))




# Spread transformed and centered data - tree level -----------------------------------------------------
#Herbivores and predators
spread_predherb <- 
  clean_vacuum %>% 
  ungroup() %>% 
  filter(kind %in% c("herbeetle", "heteroherb", "jump", "lepi", 
                     "ortho", "psyllid", "scaleaphid", "snail", "ants", "heteropred", 
                     "huntspids", "lacewings", "predbeetle", "predflies", "opilio",
                     "roaches", "webspids")) %>% 
  dplyr::select(kind, Site, alltrees, Sampling, treetype, Abundance) %>%
  group_by(kind, Site, alltrees, Sampling, treetype) %>% 
  summarise_all(funs(sum)) %>% 
  spread(key = kind, Abundance, fill =0)
colSums(spread_predherb[,5:20]) 
##Bind proximity index
spread_predherb <- 
  spread_predherb[,1:4] %>% 
  left_join(poolcenter_tree[,c(2,4,7)]) %>% 
  left_join(spread_predherb)
##Double-check
spread_predherb <- 
  na.omit(spread_predherb)

#Predators
spread_pred <-
  clean_vacuum %>% 
  ungroup() %>% 
  filter(kind %in% c("predbeetle", "webspids", "huntspids", "heteropred", "predflies","roaches", "opilio", "lacewings", "ants")) %>% 
  dplyr::select(kind, Site, alltrees,Sampling, treetype, Abundance) %>% 
  group_by(kind, Site, alltrees, Sampling, treetype) %>% 
  summarise_all(funs(sum)) %>% 
  spread(key = kind, Abundance, fill =0)
colSums(spread_pred[,5:13])
##Bind proximity index
spread_pred <- 
  spread_pred[,1:4] %>% 
  left_join(poolcenter_tree[,c(2,4,7)]) %>% 
  left_join(spread_pred)
##Double-check
spread_pred <- 
  na.omit(spread_pred)

#Bromeliad-associated predators
spread_brompred <- 
  clean_vacuum %>% 
  ungroup() %>% 
  filter(where == "brom") %>% 
  ##not including low abundance kinds webspids and heteropreds
  filter(kind %in% c("predbeetle", "huntspids", "predflies","roaches", "opilio", "lacewings", "ants", "para")) %>% 
  dplyr::select(kind, Site, alltrees, Sampling, treetype, Abundance) %>% 
  group_by(kind, Site, alltrees, Sampling, treetype) %>% 
  summarise_all(funs(sum)) %>% 
  spread(key = kind, Abundance, fill =0)
##Bind proximity index and abundance of other predators
spread_brompred <- 
  spread_brompred[,1:4] %>% 
  left_join(poolcenter_tree[,c(2,4,7,41:43)]) %>% 
  left_join(spread_brompred)
##Double-check
spread_brompred <- 
  na.omit(spread_brompred)
colSums(spread_brompred[,6:13])

#Aerial predators
spread_mobipred <- 
  clean_vacuum %>% 
  ungroup() %>% 
  filter(where == "mobi") %>% 
  ##not including low abundance kind opilio
  filter(kind %in% c("predbeetle", "webspids", "huntspids", "heteropred", "predflies","roaches", "lacewings", "ants")) %>% 
  dplyr::select(kind, Site, alltrees, Sampling, treetype, Abundance) %>% 
  group_by(kind, Site, alltrees, Sampling, treetype) %>% 
  summarise_all(funs(sum)) %>% 
  spread(key = kind, Abundance, fill =0) 
##Bind proximity index and abundance of other predators
spread_mobipred <- 
  spread_mobipred[,1:4] %>% 
  left_join(poolcenter_tree[,c(2,4,7,40,42,43)]) %>% 
  left_join(spread_mobipred)
##Double-check
spread_mobipred <- 
  na.omit(spread_mobipred)
colSums(spread_mobipred[,6:12])

#Tree-associated predators
spread_arbopred <- 
  clean_vacuum %>% 
  ungroup() %>% 
  filter(where == "tree") %>% 
  ##not including low abundance kind opilio and predatory beetle (like carabids)
  filter(kind %in% c("para", "webspids", "huntspids", "heteropred", "predflies","roaches", "lacewings", "ants")) %>% 
  dplyr::select(kind, Site, alltrees, Sampling, treetype, Abundance) %>% 
  group_by(kind, Site, alltrees, Sampling, treetype) %>% 
  summarise_all(funs(sum)) %>% 
  spread(key = kind, Abundance, fill =0) 
##Bind proximity index and abundance of other predators
spread_arbopred <- 
  spread_arbopred[,1:4] %>% 
  left_join(poolcenter_tree[,c(2,4,7,40,41,43)]) %>% 
  left_join(spread_arbopred)
##Double-check
spread_arbopred <- 
  na.omit(spread_arbopred)
colSums(spread_arbopred[,6:14])

#Herbivores
spread_herb <- 
  clean_vacuum %>% 
  ungroup() %>% 
  filter(kind %in% c("herbeetle", "heteroherb", "jump", "lepi", 
                     "ortho", "psyllid", "scaleaphid", "snail")) %>% 
  dplyr::select(kind, Site, alltrees, Sampling, treetype, Abundance) %>% 
  group_by(kind, Site, alltrees, Sampling, treetype) %>% 
  summarise_all(funs(sum)) %>% 
  spread(key = kind, Abundance, fill =0) 
##Bind proximity index and abundance of predators
spread_herb <- 
  spread_herb[,1:4] %>% 
  left_join(poolcenter_tree[,c(2,4,7,39:43)]) %>% 
  left_join(spread_herb)
##Double-check
spread_herb <- 
  na.omit(spread_herb)
colSums(spread_herb[,6:17])





# Merge tree and bromeliad data -----------------------------------------
#Merge the two datasets and assign them to two different locations
bromtree_comparison <- 
  clean_vacuum %>% 
  ungroup %>% 
  dplyr::select(Site, kind, Sampling, alltrees, Abundance) %>% 
  group_by(Site, kind, Sampling, alltrees) %>% 
  summarise_all(funs(sum)) %>% 
  mutate(loc = "tree") %>% 
  bind_rows(clean_dissection %>% 
              ungroup %>% 
              dplyr::select(Site, kind, Sampling, alltrees, Abundance) %>% 
              group_by(Site,kind, Sampling, alltrees) %>% 
              summarise_all(funs(sum)) %>% 
              mutate(loc = "brom")) %>% 
  spread(key= kind, 
         Abundance, 
         fill = 0) %>% 
  ##Unique name for location,and sampling period of each sample
  unite(where, alltrees, loc, Sampling, sep= "_", remove = F) %>% 
  ##Remove the four outlying trees with tick nests
  filter(where %notin% c("ER_U1_tree_B", "ER_P5_tree_B",
                         "ER_O9_tree_B", "ER_AI1_tree_B")) %>% 
  ##Rename Ootheca column by Ootheca instead of Blattodea
  rename(ootheca = Blattodea)

#Make a dataframe with rocation as row names
bromtree_comparison <- 
  data.frame(bromtree_comparison,
             row.names = bromtree_comparison$where)
#Split in two separate frames for cca
whereloc <- 
  bromtree_comparison[,1:5]
bromtree_comparison <- 
  data.frame(bromtree_comparison[,6:48])
#Remove kinds with less than 10
bromtree_comparison <- 
  bromtree_comparison[,colSums(bromtree_comparison) > 10]
#Hellinger transformation
bromtree_comparison <- 
  data.frame(decostand(bromtree_comparison, 
                       "hellinger"))
