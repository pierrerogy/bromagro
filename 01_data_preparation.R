#Data preparation

#Load packages
##Data tidying
library(plyr)
library(dplyr)
library(tidyr)
##Modelling
library(lme4)
library(effects)
library(ggeffects)
library(ggplot2)
library(afex)
library(DHARMa)
library(visreg)
library(car)
library(gridExtra)
library(MASS)
##Diversity
library(vegan)
library(ade4)
library(corrplot)
library(BiodiversityR)

#Functions
##not in
'%notin%' <- 
  Negate('%in%')

#Load and check data
distance <- read.csv("C://Users/pierr/OneDrive/Projects/bromagro/Data/Distance.csv",
                     sep = ";")
str(distance)
damage <- read.csv("C://Users/pierr/OneDrive/Projects/bromagro/Data/LeafDamage.csv",
                   sep = ";")
str(damage)
lamona <- read.csv("C://Users/pierr/OneDrive/Projects/bromagro/Data/LaMona.csv",
                   sep = ";")
str(lamona)
bromzy <- read.csv("C://Users/pierr/OneDrive/Projects/bromagro/Data/Bromzy.csv", 
                   sep = ";")
str(bromzy)


#Distance and index--------------------------------------------------
#Tidying and assertions
str(distance)
##Remove the weird column
distance <- 
  distance[,1:10]
str(distance)

##Missing bromeliad.
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
missing_tree <- 
  data.frame(si, sa, da, tree, trea, sq, br, dis, dia, lo)
names(missing_tree) <- 
  names(distance)
distance <- 
  rbind(distance, missing_tree)

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
##height and volume
distance <- 
  distance %>% 
  unite(alltrees, Site, Tree, sep = "_", remove = F) %>% 
  unite(quadrats, alltrees, Square, sep = "_", remove = F) %>% 
  mutate(height = (Longest_leaf_length^2- (Diameter/2)^2)^0.5) %>% 
  mutate(volume = pi*((Diameter/2)^2)*height/3) 
##density index
###Create
calc_index <- 
  distance %>% 
  mutate(largeleaf = Longest_leaf_length/Distance) %>% 
  dplyr::select(alltrees, quadrats, largeleaf) %>% 
  group_by(alltrees, quadrats) %>% 
  summarise_all(funs(sum))

###Bind and tweak
distance <- 
  distance %>%
  left_join(calc_index) %>% 
  rename(Bromeliad = Bromeliad_.)









#Damage and bromeliad parameters------------------------------------------------------
#Tidying & Assertions
str(damage)
##Coerce variables into their actual category
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

#Bringing in new values
##Percentages
leafdamage <- 
  damage %>% 
  rename(actual = Actual_area) %>% 
  rename(original = Original_area) %>% 
  mutate(okarea = actual - Miner - Hemiptera_scraper) %>% 
  mutate(propdamage = 1- okarea/original) %>% 
  dplyr::select(-Analyst, -Day, -Miner, -Hemiptera_scraper) %>% 
  unite(alltrees, Site, Tree, sep = "_", remove = F) %>% 
  unite(quadrats, alltrees, Square, sep = "_", remove = F)
 
###Checking
table(leafdamage$Square)
str(leafdamage)
leafdamage$alltrees <-
  as.factor(leafdamage$alltrees)
leafdamage$quadrats <-
  as.factor(leafdamage$quadrats)
leafdamage$Tree <-
  as.factor(leafdamage$Tree)
leafdamage$Treatment <-
  as.factor(leafdamage$Treatment)

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

  
##Rechecking everything
str(leafdamage)
leafdamage$alltrees <-
  as.factor(leafdamage$alltrees)

##pool per quadrat
calc_leafdamage <- 
  na.exclude(leafdamage) %>% 
  dplyr::select(alltrees, quadrats, Sampling, propdamage) %>% 
  group_by(alltrees, quadrats, Sampling) %>% 
  summarise_all(funs(mean))

#Adding ipresence to leafdamage
leafdamage$presence <- 
  "no"
leafdamage$presence[which(leafdamage$Treatment == "wr" 
                        & leafdamage$Sampling == "B")] <- 
  "yes"
leafdamage$presence[which(leafdamage$Treatment == "w")] <- 
  "yes"
leafdamage$presence <- 
  as.factor(leafdamage$presence)
#Vacuum sampling ----------------------------------------------------
#Tidying
str(lamona)
##Removing weird column
lamona <-  
  lamona[,1:17]
##Remove things that have nothing to do with the analysis, eg mozzies, blackflies....
lamona <- 
  lamona[!lamona$Species == "Remove",]
lamona <- 
  lamona[!lamona$Species == "Remove?",]
##Just add nada for ootheca feeding behaviour
lamona$Diet[which(lamona$Subfamily == "Ootheca")] <- 
  "nada"
lamona$Diet <- 
  droplevels(lamona$Diet)
##Remove the recheck indication for ants to double-check
lamona$Suborder[which(lamona$Family == "Formicidae")] <- 
  "Apocrita"
lamona$Suborder <- 
  droplevels(lamona$Suborder)
##Putting Acari as Order name for mites, even though it is not
levels(lamona$Order) <- 
  c(levels(lamona$Order), "Acari")
lamona$Order[which(lamona$Order == "")] <- 
  "Acari"
lamona$Order <- 
  droplevels(lamona$Order)
##Add 1 where I forgot to
lamona$Abundance[which(is.na(lamona$Abundance))] <-
  1
##recheck
str(lamona)
##One tree with two treatments
lamona$Treatment[which(lamona$Tree == "D18")] <- 
  "wo"


#Combine specimens per quadrat based on morphospecies
puramona <- 
  lamona %>% 
  dplyr::select(Order, Suborder, Family, Morphospecies, Diet, Site, Sampling, Tree, Treatment, Square, Abundance) %>% 
  unite(alltrees, Site, Tree, sep ="_", remove =F) %>% 
  unite(quadrats, alltrees, Square, sep ="_", remove = F) %>% 
  group_by(Order, Suborder, Family, Morphospecies, Diet, Site, alltrees, quadrats, Sampling, Tree, Treatment, Square) %>% 
  summarise_all(funs(sum)) 

#Add proximity index
puramona <- 
  puramona %>% 
  left_join(calc_index)
puramona$largeleaf[puramona$Treatment == "wo"] <- 
  0
puramona$largeleaf[which(puramona$Treatment == "wr" 
                       & puramona$Sampling == "A")] <- 
  0


#Missing sample vector
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


#Bromeliad dissection ----------------------------------------------------
str(bromzy)
#Remove things that have nothing to do with the analysis, eg aquatic stuff
bromzy <- 
  bromzy[!bromzy$Species == "Remove",]
bromzy <- 
  bromzy[!bromzy$Species == "Remove?",]
##Just add nada for ootheca feeding behaviour
bromzy$Diet[which(bromzy$Subfamily == "Ootheca")] <- 
  "nada"
bromzy$Diet <- 
  droplevels(bromzy$Diet)
##Additional level in nest factor
levels(bromzy$Abundance)
bromzy$Abundance[which(bromzy$Abundance == "nest ")] <- 
  "nest"
bromzy$Abundance <- 
  droplevels(bromzy$Abundance)
##Add 1 where I forgot to
bromzy$Abundance[bromzy$Abundance == ""] <- 
  1
bromzy$Abundance[bromzy$Abundance == " "] <- 
  1
bromzy$Abundance <- 
  droplevels(bromzy$Abundance)
##recheck
str(bromzy)
levels(bromzy$Tree)
levels(bromzy$Tree) <- 
  c(levels(bromzy$Tree), "J5")
bromzy$Tree[bromzy$Tree == "J5 "] <- 
  "J5"
bromzy$Tree <- droplevels(bromzy$Tree)
##Removing rows with individual ant counts when nest has already been counted
###probably a morphospecies oversplit
bromzy <- 
  bromzy[!(bromzy$Abundance== 16 & bromzy$Tree== "B3" & bromzy$Morphospecies =="Ant_A"),]
bromzy <- 
  bromzy[!(bromzy$Abundance== 1 & bromzy$Tree== "E8" & bromzy$Morphospecies =="Ant_X"),]
bromzy <- 
  bromzy[!(bromzy$Abundance== 1 & bromzy$Tree== "O18" & bromzy$Morphospecies =="Ant_X"),]


###Creating frame for empty ones
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
bromzy$nestcat <- 
  NA
levels(bromzy$Abundance) <- 
  c(levels(bromzy$Abundance), 0)
bromzy$Abundance[bromzy$Abundance == "nest "] <- 
  "nest"
levels(bromzy$Abundance) <-  
  c(levels(bromzy$Abundance), 50, 100, 200, 1000)
###Approximate nest size
bromzy$Abundance[which(bromzy$Abundance == "nest" 
                       & bromzy$Morphospecies == "Ant_X")] <- 
  "50"
bromzy$Abundance[which(bromzy$Abundance == "nest" 
                       & bromzy$Morphospecies == "Ant_E")] <- 
  "50"
bromzy$Abundance[which(bromzy$Abundance == "nest" 
                       & bromzy$Morphospecies == "Ant_AA")] <- 
  "50"
bromzy$Abundance[which(bromzy$Abundance == "nest" 
                       & bromzy$Morphospecies == "Ant_BF")] <- 
  "100"
bromzy$Abundance[which(bromzy$Abundance == "nest" 
                       & bromzy$Morphospecies == "Ant_AK")] <- 
  "50"
bromzy$Abundance[which(bromzy$Abundance == "nest" 
                       & bromzy$Morphospecies == "Ant_AB")] <- 
  "100"
bromzy$Abundance[which(bromzy$Abundance == "nest" 
                       & bromzy$Morphospecies == "Ant_AS")] <- 
  "100"
bromzy$Abundance[which(bromzy$Abundance == "nest" 
                       & bromzy$Morphospecies == "Ant_H")] <- 
  "100"
bromzy$Abundance[which(bromzy$Abundance == "nest" 
                       & bromzy$Morphospecies == "Ant_F")] <- 
  "100"
bromzy$Abundance[which(bromzy$Abundance == "nest" 
                       & bromzy$Morphospecies == "Ant_AH")] <- 
  "100"
bromzy$Abundance[which(bromzy$Abundance == "nest" 
                       & bromzy$Morphospecies == "Ant_BD")] <- 
  "100"
bromzy$Abundance[which(bromzy$Abundance == "nest" 
                       & bromzy$Morphospecies == "Ant_A")] <- 
  "200"
bromzy$Abundance[which(bromzy$Abundance == "nest" 
                       & bromzy$Morphospecies == "Ant_G")] <- 
  "200"
bromzy$Abundance[which(bromzy$Abundance == "nest" 
                       & bromzy$Morphospecies == "Ant_Z")] <- 
  "200"
bromzy$Abundance[which(bromzy$Abundance == "100" 
                       & bromzy$Morphospecies == "Ant_AH" 
                       & bromzy$Tree =="G11" 
                       & bromzy$Bromeliad == "c")] <- 
  "1000"
###add nest category
bromzy$nestcat[which(bromzy$Abundance == "50")] <- 
  "A"
bromzy$nestcat[which(bromzy$Abundance == "100")] <- 
  "B"
bromzy$nestcat[which(bromzy$Abundance == "200")] <- 
  "C"
bromzy$nestcat[which(bromzy$Abundance == "1000")] <- 
  "W"
##finishing it
bromzy$Abundance <-  
  as.numeric(as.character(bromzy$Abundance))
bromzy$nestcat <-  
  as.factor(bromzy$nestcat)
str(bromzy)
##Frame and correct alter ego nests
purabromzy <- 
  bromzy %>% 
  unite(alltrees, Site, Tree, sep ="_", remove =F) %>% 
  dplyr::select(Order, Suborder, Family, Site, Sampling, alltrees, Tree, Treatment, Bromeliad, nestcat,Morphospecies, Diet, Abundance) %>% 
  group_by(Order, Suborder, Family, Site, Sampling, alltrees, Tree, Treatment, Bromeliad, nestcat, Morphospecies, Diet) %>% 
  summarise_all(funs(sum))
purabromzy$Abundance[purabromzy$Abundance == 600] <- 
  400

##add kinds
###Create a column for kind
purabromzy$kind <- 
  NA
###Add categories, based on taxonomy/behavior
purabromzy$kind <- 
  ifelse(
    ##herbivores
    purabromzy$Order == "Gastropoda" & 
      purabromzy$Diet == "herb",  "snail",
    ifelse(
      purabromzy$Order == "Lepidoptera"& 
        purabromzy$Diet == "herb",  "lepi",
      ifelse(
        purabromzy$Order == "Coleoptera" &
          purabromzy$Diet %in% c("herb", "poll"),  "herbeetle",
        ifelse(
          purabromzy$Morphospecies %in% c("Hemiptera_BI", "Hemiptera_BP", "Homoptera_CF","Homoptera_CG",
                                        "Homoptera_CJ", "Hemiptera_F"),"jump",
          ifelse(
            purabromzy$Family == "Aphididae", "scaleaphid",
            ifelse(
              purabromzy$Morphospecies %in% c("Scale_K", "Scale_I", "Scale_H", "Scale_G", "Scale_E", 
                                            "Scale_D", "Scale_C", "Scale_B", "Scale_A", "Aphid_A"),  "scaleaphid",
              ifelse(
                purabromzy$Suborder == "Auchenorrhyncha",  "jump",
                ifelse(
                  purabromzy$Suborder == "Heteroptera" &
                    purabromzy$Diet == "herb",  "heteroherb",
                  ifelse(
                    purabromzy$Order == "Orthoptera" &
                      purabromzy$Diet == "herb",  "ortho",
                    ##predators
                    ifelse(
                      purabromzy$Family =="Formicidae",  "ants",
                      ifelse(
                        purabromzy$Family == "Vespidae",  "wasps",
                        ifelse(
                          purabromzy$Order == "Blattodea" &
                            purabromzy$Diet =="scav",  "roaches",
                          ifelse(
                            purabromzy$Order == "Neuroptera",  "lacewings",
                            ifelse(
                              purabromzy$Order == "Coleoptera" &
                                purabromzy$Diet == "pred",  "predbeetle",
                              ifelse(
                                purabromzy$Suborder == "Heteroptera" &
                                  purabromzy$Diet == "pred",  "heteropred",
                                ifelse(
                                  purabromzy$Order == "Opiliones",  "opilio",
                                  ifelse(
                                    purabromzy$Order == "Diptera" &
                                      purabromzy$Diet == "pred",  "predflies",
                                    ifelse(
                                      purabromzy$Order == "Diptera" &
                                        purabromzy$Diet == "herb",  "herbflies",
                                      ifelse(
                                        purabromzy$Order == "Dermaptera", "earwig",
                                        ifelse(
                                          purabromzy$Order == "Mantodea", "mantid",
                                          ifelse(
                                            purabromzy$Order == "Araneae" &
                                              purabromzy$Morphospecies %in% c("Spider_A", "Spider_X", "Spider_Z", "Spider_AC", "Spider_AD", "Spider_AL",
                                                                            "Spider_AU", "Spider_AW", "Spider_AZ", "Spider_BA", "Spider_BF", "Spider_BO",
                                                                            "Spider_BG", "Spider_BR", "Spider_BU", "Spider_BV", "Spider_BW", "Spider_G",
                                                                            "Spider_BZ", "Spider_CI", "Spider_CJ", "Spider_CQ", "Spider_CW", "Spider_DD",
                                                                            "Spider_DI", "Spider_DO", "Spider_DQ", "Spider_DU", "Spider_DV", "Spider_I",
                                                                            "Spider_DY", "Spider_EL", "Spider_EQ", "Spider_EM", "Spider_EY", "Spider_S",
                                                                            "Spider_FK", "Spider_FM", "Spider_GB"),  "webspids", 
                                            ifelse(
                                              purabromzy$Order == "Araneae" &
                                                purabromzy$Morphospecies %notin% c("Spider_A", "Spider_X", "Spider_Z", "Spider_AC", "Spider_AD", "Spider_AL",
                                                                                 "Spider_AU", "Spider_AW", "Spider_AZ", "Spider_BA", "Spider_BF", "Spider_BO",
                                                                                 "Spider_BG", "Spider_BR", "Spider_BU", "Spider_BV", "Spider_BW", "Spider_G",
                                                                                 "Spider_BZ", "Spider_CI", "Spider_CJ", "Spider_CQ", "Spider_CW", "Spider_DD",
                                                                                 "Spider_DI", "Spider_DO", "Spider_DQ", "Spider_DU", "Spider_DV", "Spider_I",
                                                                                 "Spider_DY", "Spider_EL", "Spider_EQ", "Spider_EM", "Spider_EY", "Spider_S",
                                                                                 "Spider_FK", "Spider_FM", "Spider_GB"), "huntspids", 
                                              ifelse(
                                                purabromzy$Diet =="para", "para", 
                                                ifelse(
                                                  purabromzy$Suborder != "", as.character(purabromzy$Suborder), as.character(purabromzy$Order)
                                                ))))))))))))))))))))))))



#Bromeliad predator indices--------------------------
#Per bromeliad
##without ants
calc_noant <- 
  purabromzy %>% 
  ungroup() %>% 
  filter(Diet %in% c("pred", "scav", "omni")) %>% 
  ###Removing irrelevant families
  filter(Family %notin% c("Formicidae", "Gryllidae", "Anthicidae", "Aderidae", "Tenebrionidae")) %>% 
  dplyr::select(Site, Tree, Bromeliad, Abundance) %>%
  group_by(Site, Tree, Bromeliad) %>%
  summarise_all(funs(sum)) %>% 
  rename(noantabund = Abundance)
##Ant nests
calc_nest <- 
  purabromzy %>% 
  ungroup() %>% 
  filter(Abundance >20) %>%
  dplyr::select(Site, Tree, Bromeliad, Abundance) %>%
  group_by(Site, Tree, Bromeliad) %>%
  summarise_all(funs(sum)) %>% 
  rename(nestabund = Abundance)

#Per tree
calc_noantabund <- 
  purabromzy %>% 
  ungroup() %>% 
  filter(Diet %in% c("pred", "scav", "omni")) %>% 
  ###Removing irrelevant families
  filter(Family %notin% c("Formicidae", "Gryllidae", "Anthicidae", "Aderidae", "Tenebrionidae")) %>% 
  dplyr::select(Site, Tree, Abundance) %>%
  group_by(Site, Tree) %>%
  summarise_all(funs(sum)) %>% 
  rename(treenoantabund = Abundance)

calc_nestabund <- 
  purabromzy %>% 
  ungroup() %>% 
  filter(Abundance >20) %>%
  dplyr::select(Site, Tree, Abundance) %>%
  group_by(Site, Tree) %>%
  summarise_all(funs(sum)) %>% 
  rename(treenestabund = Abundance)

#Create dataframe and add up the two
calc_insidepred <-
  distance %>% 
  left_join(calc_noant) %>% 
  left_join(calc_noantabund) %>% 
  left_join(calc_nest) %>% 
  left_join(calc_nestabund)
##Removing NAs
calc_insidepred[,16:19][is.na(calc_insidepred[,16:19])]   <- 
  0

##Get back to it
calc_insidepred <- 
  calc_insidepred %>% 
  ungroup() %>% 
  mutate(predabund = noantabund + nestabund) %>% 
  mutate(treepredabund = treenoantabund + treenestabund) %>% 
  mutate(noantindex = noantabund/Distance) %>% 
  mutate(nestindex = nestabund/Distance) %>% 
  mutate(predindex = predabund/Distance) %>%   
  dplyr::select(alltrees, quadrats, treepredabund, treenoantabund, treenestabund, noantindex, nestindex, predindex) %>% 
  group_by(alltrees, quadrats, treepredabund, treenoantabund, treenestabund) %>% 
  summarise_all(funs(sum))

#Kinds of herbivores and predators, and other arthropods ---------------------------------------
#Create a column for kind
puramona$kind <- 
  NA
##Add categories, based on taxonomy/behavior
puramona$kind <- 
  ifelse(
    ##herbivores
    puramona$Order == "Gastropoda" & 
      puramona$Diet == "herb",  "snail",
    ifelse(
      puramona$Order == "Lepidoptera"& 
        puramona$Diet == "herb",  "lepi",
      ifelse(
        puramona$Order == "Coleoptera" &
          puramona$Diet %in% c("herb", "poll"),  "herbeetle",
        ifelse(
          puramona$Morphospecies %in% c("Hemiptera_BI", "Hemiptera_BP", "Homoptera_CF","Homoptera_CG",
                                        "Homoptera_CJ", "Hemiptera_F"),"jump",
          ifelse(
            puramona$Family == "Aphididae", "scaleaphid",
            ifelse(
              puramona$Morphospecies %in% c("Scale_K", "Scale_I", "Scale_H", "Scale_G", "Scale_E", 
                                            "Scale_D", "Scale_C", "Scale_B", "Scale_A", "Aphid_A"),  "scaleaphid",
              ifelse(
                puramona$Suborder == "Auchenorrhyncha",  "jump",
                ifelse(
                  puramona$Suborder == "Heteroptera" &
                    puramona$Diet == "herb",  "heteroherb",
                  ifelse(
                    puramona$Order == "Orthoptera" &
                      puramona$Diet == "herb",  "ortho",
                    ##predators
                    ifelse(
                      puramona$Family =="Formicidae",  "ants",
                      ifelse(
                        puramona$Family == "Vespidae",  "wasps",
                        ifelse(
                          puramona$Order == "Blattodea" &
                            puramona$Diet =="scav",  "roaches",
                          ifelse(
                            puramona$Order == "Neuroptera",  "lacewings",
                            ifelse(
                              puramona$Order == "Coleoptera" &
                                puramona$Diet == "pred",  "predbeetle",
                              ifelse(
                                puramona$Suborder == "Heteroptera" &
                                  puramona$Diet == "pred",  "heteropred",
                                ifelse(
                                  puramona$Order == "Opiliones",  "opilio",
                                  ifelse(
                                    puramona$Order == "Diptera" &
                                      puramona$Diet == "pred",  "predflies",
                                    ifelse(
                                      puramona$Order == "Diptera" &
                                        puramona$Diet == "herb",  "herbflies",
                                      ifelse(
                                        puramona$Order == "Dermaptera", "earwig",
                                        ifelse(
                                          puramona$Order == "Mantodea", "mantid",
                                          ifelse(
                                            puramona$Order == "Araneae" &
                                              puramona$Morphospecies %in% c("Spider_A", "Spider_X", "Spider_Z", "Spider_AC", "Spider_AD", "Spider_AL",
                                                                            "Spider_AU", "Spider_AW", "Spider_AZ", "Spider_BA", "Spider_BF", "Spider_BO",
                                                                            "Spider_BG", "Spider_BR", "Spider_BU", "Spider_BV", "Spider_BW", "Spider_G",
                                                                            "Spider_BZ", "Spider_CI", "Spider_CJ", "Spider_CQ", "Spider_CW", "Spider_DD",
                                                                            "Spider_DI", "Spider_DO", "Spider_DQ", "Spider_DU", "Spider_DV", "Spider_I",
                                                                            "Spider_DY", "Spider_EL", "Spider_EQ", "Spider_EM", "Spider_EY", "Spider_S",
                                                                            "Spider_FK", "Spider_FM", "Spider_GB"),  "webspids", 
                                            ifelse(
                                              puramona$Order == "Araneae" &
                                                puramona$Morphospecies %notin% c("Spider_A", "Spider_X", "Spider_Z", "Spider_AC", "Spider_AD", "Spider_AL",
                                                                                 "Spider_AU", "Spider_AW", "Spider_AZ", "Spider_BA", "Spider_BF", "Spider_BO",
                                                                                 "Spider_BG", "Spider_BR", "Spider_BU", "Spider_BV", "Spider_BW", "Spider_G",
                                                                                 "Spider_BZ", "Spider_CI", "Spider_CJ", "Spider_CQ", "Spider_CW", "Spider_DD",
                                                                                 "Spider_DI", "Spider_DO", "Spider_DQ", "Spider_DU", "Spider_DV", "Spider_I",
                                                                                 "Spider_DY", "Spider_EL", "Spider_EQ", "Spider_EM", "Spider_EY", "Spider_S",
                                                                                 "Spider_FK", "Spider_FM", "Spider_GB"), "huntspids", 
                                              ifelse(
                                                puramona$Diet =="para", "para", 
                                                ifelse(
                                                  puramona$Suborder != "", as.character(puramona$Suborder), as.character(puramona$Order)
                                                ))))))))))))))))))))))))




#Predators and herbivores by quadrat -------------------------------------
#Reduce per quadrat and combining everything
calc_quadrats <- 
  leafdamage %>% 
  dplyr::select(Site, alltrees, quadrats, Treatment, Sampling) %>% 
  unique()
##adding presence column
##Presence/absence
calc_quadrats$presence <- 
  "no"
calc_quadrats$presence[which(calc_quadrats$Treatment == "wr" 
                             & calc_quadrats$Sampling == "B")] <- 
  "yes"
calc_quadrats$presence[which(calc_quadrats$Treatment == "w")] <- 
  "yes"


#Make something with everything
calc_dichos <- 
  puramona %>% 
  ungroup() %>% 
  dplyr::select(alltrees, quadrats, Sampling, Abundance) %>% 
  group_by(alltrees, quadrats, Sampling) %>% 
  summarise_all(funs(sum)) %>% 
  rename(todo = Abundance)
  
#Get the psyllids out
calc_psyllid <- 
  puramona %>% 
  ungroup() %>% 
  filter(Family == "Liviidae") %>% 
  dplyr::select(quadrats, Sampling, Abundance) %>% 
  rename(psyllid = Abundance)

#All preds, parasitoids and herbivores
calc_predherb <- 
  puramona %>% 
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
##Did not check for bromeliad non-predatory occupants
##First check what is inside
calc_bromstuff <- 
  purabromzy %>% 
  ungroup() %>% 
  ###Filter tourist taxa, based on observation
  ###flies
  filter(Order != "Diptera") %>% 
  ###social wasps
  filter(Family != "Vespidae") %>% 
  ###one parasitoid
  filter(Morphospecies != "Wasp_N") %>% 
  ##neuroptera larvae
  filter(Order != "Neuroptera") %>% 
  ###scales, likely associated with ants, but we found only 4
  filter(Suborder != "Sternorrhyncha") %>% 
  ##Spider CJ
  filter(Morphospecies != "Spider_CJ") %>% 
  ###bees
  filter(Morphospecies != "Bee_B") %>% 
  ###Vagrant ants, not flying ants, because likely to look for nesting ground there
  filter(Morphospecies %notin% c("Ant_AJ", "Ant_AL", "Ant_BA", "Ant_BE",
                                 "Ant_C", "Ant_J", "Ant_K", "Ant_L")) %>% 
  dplyr::select(Morphospecies) %>% 
  unique() %>% 
  mutate(where = "brom")
puramona <- 
  puramona %>% 
  left_join(calc_bromstuff)
##Add mobile and tree predators
puramona$where <- 
  ifelse(
    is.na(puramona$where) & puramona$Diet %in% c("pred","omni", "scav", "para") & 
      puramona$Family %in% c("Coccinellidae", "Lampyridae", "Cleridae", "Geocoridae", "Nabidae", "Vespidae") | 
      is.na(puramona$where) & puramona$Order == "Mantodea" | 
      is.na(puramona$where) & puramona$Order == "Neuroptera" & puramona$Morphospecies != "Neurolarva_A" |
      is.na(puramona$where) & puramona$kind %in% c("predflies", "para"), "mobi",
    ifelse(
      is.na(puramona$where) & puramona$Diet %in% c("pred","omni", "scav", "para") &
        puramona$Family %notin% c("Gryllidae", "Anthicidae", "Aderidae", "Tenebrionidae","Coccinellidae", 
                                  "Lampyridae", "Cleridae", "Geocoridae", "Nabidae", "Vespidae") & 
        puramona$Order %notin% c("Mantodea", "Neuroptera") & puramona$kind %notin% c("predflies", "para") | 
        is.na(puramona$where) & puramona$Morphospecies == "Neurolarva_A", "tree", puramona$where
    ))
puramona$where[is.na(puramona$where)] <- 
  "tbd"

##Bind to vacuum samples and make small dataframe to see which are found in trees
calc_bromzytree <- 
  puramona %>% 
  ungroup() %>% 
  filter(where == "brom") %>% 
  dplyr::select(Order, Family, Morphospecies, Abundance) %>% 
  group_by(Order, Family, Morphospecies) %>% 
  summarise_all(funs(sum))
##now make dataframes
###bromeliads
calc_brompred <- 
  puramona %>% 
  ungroup() %>% 
  filter(where == "brom") %>% 
  filter(Diet %in% c("pred","omni","scav")) %>% 
  filter(Family %notin% c("Gryllidae", "Anthicidae", "Aderidae", "Tenebrionidae")) %>% 
  dplyr::select(Sampling, alltrees, quadrats,Abundance) %>% 
  group_by(Sampling, alltrees, quadrats) %>% 
  summarise_all(funs(sum)) %>% 
  rename(bromypred = Abundance)
###without ants
calc_bromantless <- 
  puramona %>% 
  ungroup() %>% 
  filter(where == "brom") %>% 
  filter(Diet %in% c("pred","omni","scav")) %>% 
  filter(Family %notin% c("Gryllidae", "Anthicidae", "Aderidae", "Tenebrionidae", "Formicidae")) %>% 
  dplyr::select(Sampling, alltrees, quadrats,Abundance) %>% 
  group_by(Sampling, alltrees, quadrats) %>% 
  summarise_all(funs(sum)) %>% 
  rename(bromantless = Abundance)
calc_brompred <- 
  calc_brompred %>% 
  left_join(calc_bromantless)
is.na(calc_brompred$bromantless) <- 
  0
###mobile
calc_mobipred <- 
  puramona %>% 
  ungroup() %>% 
  filter(where == "mobi" & Diet != "para") %>% 
  dplyr::select(Sampling, alltrees, quadrats,Abundance) %>% 
  group_by(Sampling, alltrees, quadrats) %>% 
  summarise_all(funs(sum)) %>% 
  rename(mobipred = Abundance)

###tree
calc_arbopred <- 
  puramona %>% 
  ungroup() %>% 
  filter(where == "tree") %>% 
  dplyr::select(Sampling, alltrees, quadrats,Abundance) %>% 
  group_by(Sampling, alltrees, quadrats) %>% 
  summarise_all(funs(sum)) %>% 
  rename(arbopred = Abundance)

#Now make a clean data frame to bind anywhere
##Make sure that the rows to put NAs in belong to alltrees = CP_A2
##this tree was not vacuumed, but we have leaf damage for it
calc_kind <- 
  puramona %>% 
  ungroup() %>% 
  dplyr::select(Sampling, alltrees, quadrats, kind, Abundance) %>% 
  group_by(Sampling, alltrees, quadrats, kind) %>% 
  summarise_all(funs(sum)) %>% 
  spread(key= kind, Abundance, fill = 0)

##bind all abundances together
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
  mutate(herbsnailless = herb-snail) %>% 
  ##reordering
  dplyr::select(alltrees:arbopred, herbsnailless,  herbeetle, herbflies, heteroherb, jump, lepi, 
                ortho, psyllid, scaleaphid, snail, ants, earwig, heteropred, 
                huntspids, lacewings, mantid, predbeetle, predflies, opilio,
                roaches, wasps,  webspids, para)
calc_dichos[,4:34][is.na(calc_dichos[,4:34])] <- 
  0
##Check column sums and remove the rare ones
colSums(calc_dichos[,7:34])
##earwigs, mantids, wasps with less than 10 in abundance
calc_dichos <- 
  calc_dichos %>% 
  dplyr::select(-earwig, -mantid, -wasps, -herbflies)



#Checking correlations between bromeliad parameters ----------------------

#Merging all variables
correl_bromvars<- 
  calc_quadrats %>% 
  left_join(calc_bromnumber) %>% 
  left_join(calc_index) %>% 
  left_join(calc_insidepred) %>% 
  unique()
##Could not figure easier way
correl_bromvars$nestindex[correl_bromvars$presence == "no"] <-
  0
correl_bromvars$noantindex[correl_bromvars$presence == "no"] <-
  0
correl_bromvars$predindex[correl_bromvars$presence == "no"] <-
  0
correl_bromvars$treenestabund[correl_bromvars$presence == "no"] <-
  0
correl_bromvars$treenoantabund[correl_bromvars$presence == "no"] <-
  0
correl_bromvars$treepredabund[correl_bromvars$presence == "no"] <-
  0
correl_bromvars$broms[which(correl_bromvars$presence == "no")] <- 
  0
correl_bromvars$meanvolume[correl_bromvars$presence == "no"] <- 
  0
correl_bromvars$totalvolume[correl_bromvars$presence == "no"] <- 
  0
correl_bromvars$largeleaf[which(correl_bromvars$presence == "no")] <- 
  0 
correl_bromvars <- 
  correl_bromvars %>% 
  filter(presence == "yes") %>% 
  dplyr::select(-Site:-presence, -meanvolume) %>% 
  na.omit()

##PCA to vixualize uncorrelated variables, or relatively so
brompca <- dudi.pca(correl_bromvars,
                    scannf = FALSE, 
                    nf = 2)
brompca$eig/sum(brompca$eig)
s.corcircle(brompca$co, 
            clabel = 0.5)
##quick test
cor.test(correl_bromvars$largeleaf,
         correl_bromvars$predindex)

##Make a simplified frame with largeleaf and predindex
calc_bromvars <- 
  calc_quadrats %>% 
  left_join(calc_index) %>% 
  left_join(calc_insidepred) %>% 
  dplyr::select(Site:presence, largeleaf, predindex)
calc_bromvars$largeleaf[calc_bromvars$presence == "no"] <-
  0
calc_bromvars$predindex[calc_bromvars$presence == "no"] <-
  0


#Binding to frames---------------------------
#Pooling leaf damage by quadrat
pooldamage <- 
  calc_quadrats %>% 
  left_join(calc_leafdamage) %>% 
  left_join(calc_bromvars[,c(3,5,7,8)]) %>% 
  left_join(calc_dichos)
str(pooldamage)
##For the NA-intolerant adonis
pooldamage_noNAS <- 
  na.omit(pooldamage)

#Leaf damage
leafdamage<- 
  leafdamage %>% 
  left_join(calc_bromvars[,c(3,5,7,8)]) %>% 
  left_join(calc_dichos)

#Vacuum samples
puramona <- 
  puramona %>% 
  left_join(calc_bromvars[,c(3,5,7,8)])
  
#Re-centering data based on site means -----------------------------------
poolcenter <- 
  na.omit(pooldamage) %>% 
  group_by(Site) %>% 
  mutate(largeleaf= largeleaf - mean(largeleaf),
         predindex = predindex - mean(predindex),
         herbcenter = herb - mean(herb),
         herbsnaillesscenter = (herb-snail) - mean(herb-snail),
         predparacenter = predpara - mean(predpara),
         predcenter = preds - mean(preds),
         paracenter = para - mean(para),
         bromcenter = bromypred - mean(bromypred),
         bromantlesscenter = bromantless - mean(bromantless),
         arbocenter = arbopred - mean(arbopred),
         mobicenter = mobipred - mean(mobipred))

leafcenter <- 
  na.omit(leafdamage) %>% 
  group_by(Site) %>% 
  mutate(largeleaf= largeleaf - mean(largeleaf),
         predindex = predindex - mean(predindex)) %>% 
  group_by(Site) %>% 
  mutate(herbcenter= herb - mean(herb),
         herbsnaillesscenter = herbsnailless - mean(herbsnailless))

monacentral <- 
  na.omit(puramona) %>% 
  group_by(Site) %>% 
  mutate(largeleaf= largeleaf - mean(largeleaf),
         predindex = predindex - mean(predindex))
#Spreading with centered data -----------------------------------------------------
#Herbivores and predators
##with parasitoids
spread_predparaherb <- 
  monacentral %>% 
  ungroup() %>% 
  filter(kind %in% c("herbeetle", "heteroherb", "jump", "lepi", 
                     "ortho", "psyllid", "scaleaphid", "snail", "ants", "heteropred", 
                     "huntspids", "lacewings", "predbeetle", "predflies", "opilio",
                     "roaches", "webspids", "para")) %>% 
  dplyr::select(kind, Site, alltrees, quadrats, Sampling, Treatment, largeleaf, predindex, Abundance) %>% 
  group_by(kind, Site, alltrees, quadrats, Sampling, Treatment, largeleaf, predindex) %>% 
  summarise_all(funs(sum)) %>% 
  spread(key = kind, Abundance, fill =0)
colSums(spread_predparaherb[,8:24]) 
##without parasitoids
spread_predherb <- 
    monacentral %>% 
      ungroup() %>% 
      filter(kind %in% c("herbeetle", "heteroherb", "jump", "lepi", 
                            "ortho", "psyllid", "scaleaphid", "snail", "ants", "heteropred", 
                            "huntspids", "lacewings", "predbeetle", "predflies", "opilio",
                            "roaches", "webspids")) %>% 
      dplyr::select(kind, Site, alltrees, quadrats, Sampling, Treatment, largeleaf, predindex, Abundance) %>% 
      group_by(kind, Site, alltrees, quadrats, Sampling, Treatment, largeleaf, predindex) %>% 
      summarise_all(funs(sum)) %>% 
      spread(key = kind, Abundance, fill =0)
colSums(spread_predherb[,8:23]) 

#Predators
##with parasitoids
spread_predpara <-
      monacentral %>% 
      ungroup() %>% 
      filter(kind %in% c("predbeetle", "webspids", "huntspids", "heteropred", "predflies","roaches", "opilio", "lacewings", "ants", "para")) %>% 
        dplyr::select(kind, Site, alltrees, quadrats, Sampling, Treatment, largeleaf, predindex, Abundance) %>% 
        group_by(kind, Site, alltrees, quadrats, Sampling, Treatment, largeleaf, predindex) %>% 
        summarise_all(funs(sum)) %>% 
      spread(key = kind, Abundance, fill =0)
colSums(spread_predpara[,8:17])
##without parasitoids
spread_pred <-
    monacentral %>% 
      ungroup() %>% 
      filter(kind %in% c("predbeetle", "webspids", "huntspids", "heteropred", "predflies","roaches", "opilio", "lacewings", "ants")) %>% 
      dplyr::select(kind, Site, alltrees, quadrats, Sampling, Treatment, largeleaf, predindex, Abundance) %>% 
      group_by(kind, Site, alltrees, quadrats, Sampling, Treatment, largeleaf, predindex) %>% 
      summarise_all(funs(sum)) %>% 
      spread(key = kind, Abundance, fill =0)
colSums(spread_pred[,8:16])

#Bromeliad predators
spread_brompred <- 
    monacentral %>% 
      ungroup() %>% 
      filter(where == "brom") %>% 
      ##not including low abundance kinds webspids and heteropreds
      filter(kind %in% c("predbeetle", "huntspids", "predflies","roaches", "opilio", "lacewings", "ants", "para")) %>% 
      dplyr::select(kind, Site, alltrees, quadrats, Sampling, Treatment, largeleaf, predindex, Abundance) %>% 
      group_by(kind, Site, alltrees, quadrats, Sampling, Treatment, largeleaf, predindex) %>% 
      summarise_all(funs(sum)) %>% 
      spread(key = kind, Abundance, fill =0) %>% 
      ##attaching predator abundance
      left_join(poolcenter[,c(3,5,40:45)])
spread_brompred <- 
  na.omit(spread_brompred)

#Mobile predators
spread_mobipred <- 
  monacentral %>% 
  ungroup() %>% 
  filter(where == "mobi") %>% 
  ##not including low abundance kind opilio
  filter(kind %in% c("predbeetle", "webspids", "huntspids", "heteropred", "predflies","roaches", "lacewings", "ants")) %>% 
  dplyr::select(kind, Site, alltrees, quadrats, Sampling, Treatment, largeleaf, predindex, Abundance) %>% 
  group_by(kind, Site, alltrees, quadrats, Sampling, Treatment, largeleaf, predindex) %>% 
  summarise_all(funs(sum)) %>% 
  spread(key = kind, Abundance, fill =0) %>% 
  ##attaching predator abundance
  left_join(poolcenter[,c(3,5,40:45)])
spread_mobipred <- 
  na.omit(spread_mobipred)
colSums(spread_mobipred[,8:17])
#Tree predators
spread_arbopred <- 
    monacentral %>% 
      ungroup() %>% 
      filter(where == "tree") %>% 
      ##not including low abundance kind opilio and predatory beetle (like carabids)
      filter(kind %in% c("para", "webspids", "huntspids", "heteropred", "predflies","roaches", "lacewings", "ants")) %>% 
      dplyr::select(kind, Site, alltrees, quadrats, Sampling, Treatment, largeleaf, predindex, Abundance) %>% 
      group_by(kind, Site, alltrees, quadrats, Sampling, Treatment, largeleaf, predindex) %>% 
      summarise_all(funs(sum)) %>% 
      spread(key = kind, Abundance, fill =0) %>% 
      ##attaching predator abundance
      left_join(poolcenter[,c(3,5,40:45)])
colSums(spread_arbopred[,8:19])
spread_arbopred <- 
  na.omit(spread_arbopred)

#Herbivores
spread_herb <- 
    monacentral %>% 
      ungroup() %>% 
      filter(kind %in% c("herbeetle", "heteroherb", "jump", "lepi", 
                         "ortho", "psyllid", "scaleaphid", "snail")) %>% 
      dplyr::select(kind, Site, alltrees, quadrats, Sampling, Treatment, largeleaf, predindex, Abundance) %>% 
      group_by(kind, Site, alltrees, quadrats, Sampling, Treatment, largeleaf, predindex) %>% 
      summarise_all(funs(sum)) %>% 
      spread(key = kind, Abundance, fill =0) %>% 
      ##attaching predator abundance
      left_join(poolcenter[,c(3,5,40:45)])
spread_herb <- 
  na.omit(spread_herb)




#Adding kinds of bromeliad predators ------------------------
predcenter<- 
  poolcenter%>% 
  dplyr::select(-presence, -largeleaf:-arbopred, -ants:-mobicenter) %>% 
  left_join(spread_brompred[,c(3,4, 8:12)]) %>% 
  rename(bromants = ants) %>% 
  rename(bromhuntspids = huntspids) %>% 
  rename(bromopilio = opilio) %>% 
  rename(brompredbeetle = predbeetle) %>% 
  rename(bromroaches = roaches) %>% 
  left_join(poolcenter[, c(3,5, 28:43)])
predcenter[is.na(predcenter)] <- 
  0

predcenter <- 
  predcenter %>% 
  group_by(Site) %>% 
  mutate(bromant_center = bromants - mean(bromants),
         bromhuntspids_center = bromhuntspids - mean(bromhuntspids),
         bromopilio_center = bromopilio- mean(bromopilio),
         brompredbeetle_center = brompredbeetle- mean(brompredbeetle),
         bromroaches_center = bromroaches - mean(bromroaches),
         ants_center = ants- mean(ants),
         heteropred_center = heteropred- mean(heteropred),
         huntspids_center = huntspids- mean(huntspids),
         lacewings_center = lacewings- mean(lacewings),
         predbeetle_center = predbeetle- mean(predbeetle),
         opilio_center = opilio- mean(opilio),
         roaches_center = roaches- mean(roaches),
         webspids_center = webspids- mean(webspids)) %>% 
  left_join(poolcenter[,c(3,5,8,9)]) %>% 
  left_join(calc_dichos[,c(2,3,5,8)])
  







#Merging tree and bromeliad data -----------------------------------------
bromtree_comparison <- 
  puramona %>% 
  ungroup %>% 
  dplyr::select(Site, kind, Sampling, alltrees, Abundance) %>% 
  group_by(Site, kind, Sampling, alltrees) %>% 
  summarise_all(funs(sum)) %>% 
  mutate(loc = "tree") %>% 
  bind_rows(purabromzy %>% 
              ungroup %>% 
              dplyr::select(Site, kind, Sampling, alltrees, Abundance) %>% 
              group_by(Site,kind, Sampling, alltrees) %>% 
              summarise_all(funs(sum)) %>% 
              mutate(loc = "brom")) %>% 
  spread(key= kind, 
         Abundance, 
         fill = 0) %>% 
  unite(where, alltrees, loc, Sampling, sep= "_", remove = F)
bromtree_comparison <- 
  data.frame(bromtree_comparison,
             row.names = bromtree_comparison$where)
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










  
  

