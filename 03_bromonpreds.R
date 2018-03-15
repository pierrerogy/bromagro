#Predators

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

# Largeleaf adonis per site on species----------------------------------------------------------
##Species CPB
allspCPB <- 
  na.omit(puramona %>% 
            ungroup() %>%  
            filter(Site =="CP" & Sampling =="B") %>% 
            dplyr::select(Morphospecies, alltrees, quadrats, Treatment, largeleaf, Abundance) %>% 
            group_by(Morphospecies, alltrees, quadrats, Treatment, largeleaf) %>% 
            summarise_all(funs(sum)) %>% 
            spread(key = Morphospecies, Abundance, fill =0))
allspCPB_adonis <- 
  adonis(allspCPB[,5:404]~
           largeleaf, 
         permutations = 999,
         strata = allspCPB$alltrees,
         method ="chao",
         data = allspCPB)
##Species CPA
allspCPA <- 
  na.omit(puramona %>% 
            ungroup() %>%  
            filter(Site =="CP" & Sampling =="A") %>% 
            dplyr::select(Morphospecies, alltrees, quadrats, Treatment, largeleaf, Abundance) %>% 
            group_by(Morphospecies, alltrees, quadrats, Treatment, largeleaf) %>% 
            summarise_all(funs(sum)) %>% 
            spread(key = Morphospecies, Abundance, fill =0))
allspCPA_adonis <- 
  adonis(allspCPA[,5:592]~
           largeleaf, 
         permutations = 999,
         strata = allspCPA$alltrees,
         method ="chao",
         data = allspCPA)
##Species CP
allspCP <- 
  na.omit(puramona %>% 
            ungroup() %>%  
            filter(Site =="CP") %>% 
            dplyr::select(Morphospecies, Sampling, alltrees, quadrats, Treatment, largeleaf, Abundance) %>% 
            group_by(Morphospecies, Sampling, alltrees, quadrats, Treatment, largeleaf) %>% 
            summarise_all(funs(sum)) %>% 
            spread(key = Morphospecies, Abundance, fill =0))
allspCP_adonis <- 
  adonis(allspCP[,6:800]~
           largeleaf*Sampling, 
         permutations = 999,
         strata = allspCP$alltrees,
         method ="chao",
         data = allspCP)

##Species DOB
allspDOB <- 
  na.omit(puramona %>% 
            ungroup() %>%  
            filter(Site =="DO" & Sampling =="B") %>% 
            dplyr::select(Morphospecies, alltrees, quadrats, Treatment, largeleaf, Abundance) %>% 
            group_by(Morphospecies, alltrees, quadrats, Treatment, largeleaf) %>% 
            summarise_all(funs(sum)) %>% 
            spread(key = Morphospecies, Abundance, fill =0))
allspDOB_adonis <- 
  adonis(allspDOB[,5:304]~
           largeleaf, 
         permutations = 999,
         strata = allspDOB$alltrees,
         method ="chao",
         data = allspDOB)
##Species DOA
allspDOA <- 
  na.omit(puramona %>% 
            ungroup() %>%  
            filter(Site =="DO" & Sampling =="A") %>% 
            dplyr::select(Morphospecies, alltrees, quadrats, Treatment, largeleaf, Abundance) %>% 
            group_by(Morphospecies, alltrees, quadrats, Treatment, largeleaf) %>% 
            summarise_all(funs(sum)) %>% 
            spread(key = Morphospecies, Abundance, fill =0))
allspDOA_adonis <- 
  adonis(allspDOA[,5:317]~
           largeleaf, 
         permutations = 999,
         strata = allspDOA$alltrees,
         method ="chao",
         data = allspDOA)
##Species DO
allspDO <- 
  na.omit(puramona %>% 
            ungroup() %>%  
            filter(Site =="DO") %>% 
            dplyr::select(Morphospecies, Sampling, alltrees, quadrats, Treatment, largeleaf, Abundance) %>% 
            group_by(Morphospecies, Sampling, alltrees, quadrats, Treatment, largeleaf) %>% 
            summarise_all(funs(sum)) %>% 
            spread(key = Morphospecies, Abundance, fill =0))
allspDO_adonis <- 
  adonis(allspDO[,6:506]~
           largeleaf*Sampling, 
         permutations = 999,
         strata = allspDO$alltrees,
         method ="chao",
         data = allspDO)


##Species ERB
allspERB <- 
  na.omit(puramona %>% 
            ungroup() %>%  
            filter(Site =="ER" & Sampling =="B") %>% 
            dplyr::select(Morphospecies, alltrees, quadrats, Treatment, largeleaf, Abundance) %>% 
            group_by(Morphospecies, alltrees, quadrats, Treatment, largeleaf) %>% 
            summarise_all(funs(sum)) %>% 
            spread(key = Morphospecies, Abundance, fill =0))
allspERB_adonis <- 
  adonis(allspERB[,5:315]~
           largeleaf, 
         permutations = 999,
         strata = allspERB$alltrees,
         method ="chao",
         data = allspERB)
##Species ERA
allspERA <- 
  na.omit(puramona %>% 
            ungroup() %>%  
            filter(Site =="ER" & Sampling =="A") %>% 
            dplyr::select(Morphospecies, alltrees, quadrats, Treatment, largeleaf, Abundance) %>% 
            group_by(Morphospecies, alltrees, quadrats, Treatment, largeleaf) %>% 
            summarise_all(funs(sum)) %>% 
            spread(key = Morphospecies, Abundance, fill =0))
allspERA_adonis <- 
  adonis(allspERA[,5:431]~
           largeleaf, 
         permutations = 999,
         strata = allspERA$alltrees,
         method ="chao",
         data = allspERA)
##Species ER
allspER <- 
  na.omit(puramona %>% 
            ungroup() %>%  
            filter(Site =="ER") %>% 
            dplyr::select(Morphospecies, Sampling, alltrees, quadrats, Treatment, largeleaf, Abundance) %>% 
            group_by(Morphospecies, Sampling, alltrees, quadrats, Treatment, largeleaf) %>% 
            summarise_all(funs(sum)) %>% 
            spread(key = Morphospecies, Abundance, fill =0))
allspER_adonis <- 
  adonis(allspER[,6:613]~
           largeleaf*Sampling, 
         permutations = 999,
         strata = allspER$alltrees,
         method ="chao",
         data = allspER)

# Largeleaf adonis per site on diet-------------------------------------------------
##Diet CPB
alldieCPB <- 
  na.omit(puramona %>% 
            ungroup() %>%  
            filter(Site =="CP" & Sampling =="B") %>% 
            dplyr::select(Diet, alltrees, quadrats, Treatment, largeleaf, Abundance) %>% 
            group_by(Diet, alltrees, quadrats, Treatment, largeleaf) %>% 
            summarise_all(funs(sum)) %>% 
            spread(key = Diet, Abundance, fill =0))
alldieCPB_adonis <- 
  adonis(alldieCPB[,5:20]~
           largeleaf, 
         permutations = 999,
         strata = alldieCPB$alltrees,
         method ="chao",
         data = alldieCPB)
##Diet CPa
alldieCPA <- 
  na.omit(puramona %>% 
            ungroup() %>%  
            filter(Site =="CP" & Sampling =="A") %>% 
            dplyr::select(Diet, alltrees, quadrats, Treatment, largeleaf, Abundance) %>% 
            group_by(Diet, alltrees, quadrats, Treatment, largeleaf) %>% 
            summarise_all(funs(sum)) %>% 
            spread(key = Diet, Abundance, fill =0))
alldieCPA_adonis <- 
  adonis(alldieCPA[,5:20]~
           largeleaf, 
         permutations = 999,
         strata = alldieCPA$alltrees,
         method ="chao",
         data = alldieCPA)
##Diet CP
alldieCP <- 
  na.omit(puramona %>% 
            ungroup() %>%  
            filter(Site =="CP") %>% 
            dplyr::select(Diet, Sampling, alltrees, quadrats, Treatment, largeleaf, Abundance) %>% 
            group_by(Diet, Sampling, alltrees, quadrats, Treatment, largeleaf) %>% 
            summarise_all(funs(sum)) %>% 
            spread(key = Diet, Abundance, fill =0))
alldieCP_adonis <- 
  adonis(alldieCP[,6:21]~
           largeleaf*Sampling, 
         permutations = 999,
         strata = alldieCP$alltrees,
         method ="chao",
         data = alldieCP)

##Diet DOB
alldieDOB <- 
  na.omit(puramona %>% 
            ungroup() %>%  
            filter(Site =="DO" & Sampling =="B") %>% 
            dplyr::select(Diet, alltrees, quadrats, Treatment, largeleaf, Abundance) %>% 
            group_by(Diet, alltrees, quadrats, Treatment, largeleaf) %>% 
            summarise_all(funs(sum)) %>% 
            spread(key = Diet, Abundance, fill =0))
alldieDOB_adonis <- 
  adonis(alldieDOB[,5:20]~
           largeleaf, 
         permutations = 999,
         strata = alldieDOB$alltrees,
         method ="chao",
         data = alldieDOB)
##Diet DOA
alldieDOA <- 
  na.omit(puramona %>% 
            ungroup() %>%  
            filter(Site =="DO" & Sampling =="A") %>% 
            dplyr::select(Diet, alltrees, quadrats, Treatment, largeleaf, Abundance) %>% 
            group_by(Diet, alltrees, quadrats, Treatment, largeleaf) %>% 
            summarise_all(funs(sum)) %>% 
            spread(key = Diet, Abundance, fill =0))
alldieDOA_adonis <- 
  adonis(alldieDOA[,5:18]~
           largeleaf, 
         permutations = 999,
         strata = alldieDOA$alltrees,
         method ="chao",
         data = alldieDOA)
##Diet DO
alldieDO <- 
  na.omit(puramona %>% 
            ungroup() %>%  
            filter(Site =="DO") %>% 
            dplyr::select(Diet, Sampling, alltrees, quadrats, Treatment, largeleaf, Abundance) %>% 
            group_by(Diet, Sampling, alltrees, quadrats, Treatment, largeleaf) %>% 
            summarise_all(funs(sum)) %>% 
            spread(key = Diet, Abundance, fill =0))
alldieDO_adonis <- 
  adonis(alldieDO[,6:21]~
           largeleaf*Sampling, 
         permutations = 999,
         strata = alldieDO$alltrees,
         method ="chao",
         data = alldieDO)


##Diet ERB
alldieERB <- 
  na.omit(puramona %>% 
            ungroup() %>%  
            filter(Site =="ER" & Sampling =="B") %>% 
            dplyr::select(Diet, alltrees, quadrats, Treatment, largeleaf, Abundance) %>% 
            group_by(Diet, alltrees, quadrats, Treatment, largeleaf) %>% 
            summarise_all(funs(sum)) %>% 
            spread(key = Diet, Abundance, fill =0))
alldieERB_adonis <- 
  adonis(alldieERB[,5:19]~
           largeleaf, 
         permutations = 999,
         strata = alldieERB$alltrees,
         method ="chao",
         data = alldieERB)
##Diet ERA
alldieERA <- 
  na.omit(puramona %>% 
            ungroup() %>%  
            filter(Site =="ER" & Sampling =="A") %>% 
            dplyr::select(Diet, alltrees, quadrats, Treatment, largeleaf, Abundance) %>% 
            group_by(Diet, alltrees, quadrats, Treatment, largeleaf) %>% 
            summarise_all(funs(sum)) %>% 
            spread(key = Diet, Abundance, fill =0))
alldieERA_adonis <- 
  adonis(alldieERA[,5:18]~
           largeleaf, 
         permutations = 999,
         strata = alldieERA$alltrees,
         method ="chao",
         data = alldieERA)

##Diet ER
alldieER <- 
  na.omit(puramona %>% 
            ungroup() %>%  
            filter(Site =="ER") %>% 
            dplyr::select(Diet, Sampling, alltrees, quadrats, Treatment, largeleaf, Abundance) %>% 
            group_by(Diet, Sampling, alltrees, quadrats, Treatment, largeleaf) %>% 
            summarise_all(funs(sum)) %>% 
            spread(key = Diet, Abundance, fill =0))
alldieER_adonis <- 
  adonis(alldieER[,6:21]~
           largeleaf*Sampling, 
         permutations = 999,
         strata = alldieER$alltrees,
         method ="chao",
         data = alldieER)









# Largeleaf adonis per site on main diets-------------------------------------------------
##Diet CPB
alldieCPB <- 
  puramona %>% 
  ungroup() %>% 
  dplyr::filter(Site =="CP" & Sampling =="B" & Diet != "xylo" & Diet != "nect" & Diet != "poll" & Diet != "klep" & Diet != "poll/nect" & Diet != "gran") %>% 
  dplyr::select(Diet, alltrees, quadrats, Treatment, largeleaf, Abundance) %>%
  group_by(Diet, alltrees, quadrats, Treatment, largeleaf) %>%
  summarise_all(funs(sum)) %>%
  spread(key = Diet, Abundance, fill =0)
alldieCPB_adonis <- 
  adonis(alldieCPB[,5:14]~
           largeleaf, 
         permutations = 999,
         strata = alldieCPB$alltrees,
         method ="chao",
         data = alldieCPB)
##Diet CPA
alldieCPA <- 
  na.omit(puramona %>% 
            ungroup() %>%  
            filter(Site =="CP" & Sampling =="A"& Diet != "xylo" & Diet != "nect" & Diet != "poll" & Diet != "klep" & Diet != "poll/nect" & Diet != "gran") %>% 
            dplyr::select(Diet, alltrees, quadrats, Treatment, largeleaf, Abundance) %>% 
            group_by(Diet, alltrees, quadrats, Treatment, largeleaf) %>% 
            summarise_all(funs(sum)) %>% 
            spread(key = Diet, Abundance, fill =0))
alldieCPA_adonis <- 
  adonis(alldieCPA[,5:14]~
           largeleaf, 
         permutations = 999,
         strata = alldieCPA$alltrees,
         method ="chao",
         data = alldieCPA)
##Diet CP
alldieCP <- 
  na.omit(puramona %>% 
            ungroup() %>%  
            filter(Site =="CP"& Diet != "xylo" & Diet != "nect" & Diet != "poll" & Diet != "klep" & Diet != "poll/nect" & Diet != "gran") %>% 
            dplyr::select(Diet, Sampling, alltrees, quadrats, Treatment, largeleaf, Abundance) %>% 
            group_by(Diet, Sampling, alltrees, quadrats, Treatment, largeleaf) %>% 
            summarise_all(funs(sum)) %>% 
            spread(key = Diet, Abundance, fill =0))
alldieCP_adonis <- 
  adonis(alldieCP[,6:15]~
           largeleaf*Sampling, 
         permutations = 999,
         strata = alldieCP$alltrees,
         method ="chao",
         data = alldieCP)

##Diet DOB
alldieDOB <- 
  na.omit(puramona %>% 
            ungroup() %>%  
            filter(Site =="DO" & Sampling =="B"& Diet != "xylo" & Diet != "nect" & Diet != "poll" & Diet != "klep" & Diet != "poll/nect" & Diet != "gran") %>% 
            dplyr::select(Diet, alltrees, quadrats, Treatment, largeleaf, Abundance) %>% 
            group_by(Diet, alltrees, quadrats, Treatment, largeleaf) %>% 
            summarise_all(funs(sum)) %>% 
            spread(key = Diet, Abundance, fill =0))
alldieDOB_adonis <- 
  adonis(alldieDOB[,5:14]~
           largeleaf, 
         permutations = 999,
         strata = alldieDOB$alltrees,
         method ="chao",
         data = alldieDOB)
##Diet DOA
alldieDOA <- 
  na.omit(puramona %>% 
            ungroup() %>%  
            filter(Site =="DO" & Sampling =="A"& Diet != "xylo" & Diet != "nect" & Diet != "poll" & Diet != "klep" & Diet != "poll/nect" & Diet != "gran") %>% 
            dplyr::select(Diet, alltrees, quadrats, Treatment, largeleaf, Abundance) %>% 
            group_by(Diet, alltrees, quadrats, Treatment, largeleaf) %>% 
            summarise_all(funs(sum)) %>% 
            spread(key = Diet, Abundance, fill =0))
alldieDOA_adonis <- 
  adonis(alldieDOA[,5:14]~
           largeleaf, 
         permutations = 999,
         strata = alldieDOA$alltrees,
         method ="chao",
         data = alldieDOA)
##Diet DO
alldieDO <- 
  na.omit(puramona %>% 
            ungroup() %>%  
            filter(Site =="DO"& Diet != "xylo" & Diet != "nect" & Diet != "poll" & Diet != "klep" & Diet != "poll/nect" & Diet != "gran") %>% 
            dplyr::select(Diet, Sampling, alltrees, quadrats, Treatment, largeleaf, Abundance) %>% 
            group_by(Diet, Sampling, alltrees, quadrats, Treatment, largeleaf) %>% 
            summarise_all(funs(sum)) %>% 
            spread(key = Diet, Abundance, fill =0))
alldieDO_adonis <- 
  adonis(alldieDO[,6:15]~
           largeleaf*Sampling, 
         permutations = 999,
         strata = alldieDO$alltrees,
         method ="chao",
         data = alldieDO)


##Diet ERB
alldieERB <- 
  na.omit(puramona %>% 
            ungroup() %>%  
            filter(Site =="ER" & Sampling =="B"& Diet != "xylo" & Diet != "nect" & Diet != "poll" & Diet != "klep" & Diet != "poll/nect" & Diet != "gran") %>% 
            dplyr::select(Diet, alltrees, quadrats, Treatment, largeleaf, Abundance) %>% 
            group_by(Diet, alltrees, quadrats, Treatment, largeleaf) %>% 
            summarise_all(funs(sum)) %>% 
            spread(key = Diet, Abundance, fill =0))
alldieERB_adonis <- 
  adonis(alldieERB[,5:14]~
           largeleaf, 
         permutations = 999,
         strata = alldieERB$alltrees,
         method ="chao",
         data = alldieERB)
##Diet ERA
alldieERA <- 
  na.omit(puramona %>% 
            ungroup() %>%  
            filter(Site =="ER" & Sampling =="A"& Diet != "xylo" & Diet != "nect" & Diet != "poll" & Diet != "klep" & Diet != "poll/nect" & Diet != "gran") %>% 
            dplyr::select(Diet, alltrees, quadrats, Treatment, largeleaf, Abundance) %>% 
            group_by(Diet, alltrees, quadrats, Treatment, largeleaf) %>% 
            summarise_all(funs(sum)) %>% 
            spread(key = Diet, Abundance, fill =0))
alldieERA_adonis <- 
  adonis(alldieERA[,5:14]~
           largeleaf, 
         permutations = 999,
         strata = alldieERA$alltrees,
         method ="chao",
         data = alldieERA)

##Diet ER
alldieER <- 
  na.omit(puramona %>% 
            ungroup() %>%  
            filter(Site =="ER"& Diet != "xylo" & Diet != "nect" & Diet != "poll" & Diet != "klep" & Diet != "poll/nect" & Diet != "gran") %>% 
            dplyr::select(Diet, Sampling, alltrees, quadrats, Treatment, largeleaf, Abundance) %>% 
            group_by(Diet, Sampling, alltrees, quadrats, Treatment, largeleaf) %>% 
            summarise_all(funs(sum)) %>% 
            spread(key = Diet, Abundance, fill =0))
alldieER_adonis <- 
  adonis(alldieER[,6:15]~
           largeleaf*Sampling, 
         permutations = 999,
         strata = alldieER$alltrees,
         method ="chao",
         data = alldieER)










# Largeleaf adonis per site on preds----------------------------------------------------------
##Species CPB
predCPB <- 
  na.omit(puramona %>% 
            ungroup() %>%  
            filter(Site =="CP" & Sampling =="B") %>% 
            filter(Diet == "pred") %>% 
            dplyr::select(Morphospecies, alltrees, quadrats, Treatment, largeleaf, Abundance) %>% 
            group_by(Morphospecies, alltrees, quadrats, Treatment, largeleaf) %>% 
            summarise_all(funs(sum)) %>% 
            spread(key = Morphospecies, Abundance, fill =0))
predCPB_adonis <- 
  adonis(predCPB[,5:121]~
           largeleaf, 
         permutations = 999,
         strata = predCPB$alltrees,
         method ="chao",
         data = predCPB)
##Species CPA
predCPA <- 
  na.omit(puramona %>% 
            ungroup() %>%  
            filter(Site =="CP" & Sampling =="A") %>% 
            filter(Diet == "pred") %>% 
            dplyr::select(Morphospecies, alltrees, quadrats, Treatment, largeleaf, Abundance) %>% 
            group_by(Morphospecies, alltrees, quadrats, Treatment, largeleaf) %>% 
            summarise_all(funs(sum)) %>% 
            spread(key = Morphospecies, Abundance, fill =0))
predCPA_adonis <- 
  adonis(predCPA[,5:162]~
           largeleaf, 
         permutations = 999,
         strata = predCPA$alltrees,
         method ="chao",
         data = predCPA)
##Species CP
predCP <- 
  na.omit(puramona %>% 
            ungroup() %>%  
            filter(Site =="CP") %>% 
            filter(Diet == "pred") %>% 
            dplyr::select(Morphospecies, Sampling, alltrees, quadrats, Treatment, largeleaf, Abundance) %>% 
            group_by(Morphospecies, Sampling, alltrees, quadrats, Treatment, largeleaf) %>% 
            summarise_all(funs(sum)) %>% 
            spread(key = Morphospecies, Abundance, fill =0))
predCP_adonis <- 
  adonis(predCP[,6:222]~
           largeleaf*Sampling, 
         permutations = 999,
         strata = predCP$alltrees,
         method ="chao",
         data = predCP)

##Species DOB
predDOB <- 
  na.omit(puramona %>% 
            ungroup() %>%  
            filter(Site =="DO" & Sampling =="B") %>% 
            filter(Diet == "pred") %>% 
            dplyr::select(Morphospecies, alltrees, quadrats, Treatment, largeleaf, Abundance) %>% 
            group_by(Morphospecies, alltrees, quadrats, Treatment, largeleaf) %>% 
            summarise_all(funs(sum)) %>% 
            spread(key = Morphospecies, Abundance, fill =0))
predDOB_adonis <- 
  adonis(predDOB[,5:105]~
           largeleaf, 
         permutations = 999,
         strata = predDOB$alltrees,
         method ="chao",
         data = predDOB)
##Species DOA
predDOA <- 
  na.omit(puramona %>% 
            ungroup() %>%  
            filter(Site =="DO" & Sampling =="A") %>%
            filter(Diet == "pred") %>% 
            dplyr::select(Morphospecies, alltrees, quadrats, Treatment, largeleaf, Abundance) %>% 
            group_by(Morphospecies, alltrees, quadrats, Treatment, largeleaf) %>% 
            summarise_all(funs(sum)) %>% 
            spread(key = Morphospecies, Abundance, fill =0))
predDOA_adonis <- 
  adonis(predDOA[,5:127]~
           largeleaf, 
         permutations = 999,
         strata = predDOA$alltrees,
         method ="chao",
         data = predDOA)
##Species DO
predDO <- 
  na.omit(puramona %>% 
            ungroup() %>%  
            filter(Site =="DO") %>% 
            filter(Diet == "pred") %>% 
            dplyr::select(Morphospecies, Sampling, alltrees, quadrats, Treatment, largeleaf, Abundance) %>% 
            group_by(Morphospecies, Sampling, alltrees, quadrats, Treatment, largeleaf) %>% 
            summarise_all(funs(sum)) %>% 
            spread(key = Morphospecies, Abundance, fill =0))
predDO_adonis <- 
  adonis(predDO[,6:184]~
           largeleaf*Sampling, 
         permutations = 999,
         strata = predDO$alltrees,
         method ="chao",
         data = predDO)


##Species ERB
predERB <- 
  na.omit(puramona %>% 
            ungroup() %>%  
            filter(Site =="ER" & Sampling =="B") %>% 
            filter(Diet == "pred") %>% 
            dplyr::select(Morphospecies, alltrees, quadrats, Treatment, largeleaf, Abundance) %>% 
            group_by(Morphospecies, alltrees, quadrats, Treatment, largeleaf) %>% 
            summarise_all(funs(sum)) %>% 
            spread(key = Morphospecies, Abundance, fill =0))
predERB_adonis <- 
  adonis(predERB[,5:101]~
           largeleaf, 
         permutations = 999,
         strata = predERB$alltrees,
         method ="chao",
         data = predERB)
##Species ERA
predERA <- 
  na.omit(puramona %>% 
            ungroup() %>%  
            filter(Site =="ER" & Sampling =="A") %>% 
            filter(Diet == "pred") %>% 
            dplyr::select(Morphospecies, alltrees, quadrats, Treatment, largeleaf, Abundance) %>% 
            group_by(Morphospecies, alltrees, quadrats, Treatment, largeleaf) %>% 
            summarise_all(funs(sum)) %>% 
            spread(key = Morphospecies, Abundance, fill =0))
predERA_adonis <- 
  adonis(predERA[,5:153]~
           largeleaf, 
         permutations = 999,
         strata = predERA$alltrees,
         method ="chao",
         data = predERA)
##Species ER
predER <- 
  na.omit(puramona %>% 
            ungroup() %>%  
            filter(Site =="ER") %>% 
            filter(Diet == "pred") %>% 
            dplyr::select(Morphospecies, Sampling, alltrees, quadrats, Treatment, largeleaf, Abundance) %>% 
            group_by(Morphospecies, Sampling, alltrees, quadrats, Treatment, largeleaf) %>% 
            summarise_all(funs(sum)) %>% 
            spread(key = Morphospecies, Abundance, fill =0))
predER_adonis <- 
  adonis(predER[,6:200]~
           largeleaf*Sampling, 
         permutations = 999,
         strata = predER$alltrees,
         method ="chao",
         data = predER)


# Largeleaf overall???? -------------------------------------------------------

#On Species
perm <- 
  how(nperm=50)
setBlocks(perm)<- 
  with(puramona_spread_species, 
       Site,
       nested.blocks =F)
adoleaf_overall_sp <- 
  adonis(puramona_spread_species[,7:1209]~
           largeleaf*Sampling, 
         permutations = 999,
         strata = puramona_spread_species$alltrees,
         method ="chao",
         data = puramona_spread_species
  )

#On diet  
perm <- 
  how(nperm=999)
setBlocks(perm)<- 
  with(puramona_spread_diet, 
       quadrats %in% alltrees %in% Site,
       nested.blocks =T)
adoleaf_overall_diet <- 
  adonis(puramona_spread_diet[,7:22]~
           largeleaf*Sampling, 
         permutations = perm,
         method ="chao",
         data = puramona_spread_species)





# nestindex adonis per site on species----------------------------------------------------------
##Species CPB
allspCPB <- 
  na.omit(puramona %>% 
            ungroup() %>%  
            filter(Site =="CP" & Sampling =="B") %>% 
            dplyr::select(Morphospecies, alltrees, quadrats, Treatment, nestindex, Abundance) %>% 
            group_by(Morphospecies, alltrees, quadrats, Treatment, nestindex) %>% 
            summarise_all(funs(sum)) %>% 
            spread(key = Morphospecies, Abundance, fill =0))
allspCPB_adonis <- 
  adonis(allspCPB[,5:404]~
           nestindex, 
         permutations = 999,
         strata = allspCPB$alltrees,
         method ="chao",
         data = allspCPB)
##Species CPA
allspCPA <- 
  na.omit(puramona %>% 
            ungroup() %>%  
            filter(Site =="CP" & Sampling =="A") %>% 
            dplyr::select(Morphospecies, alltrees, quadrats, Treatment, nestindex, Abundance) %>% 
            group_by(Morphospecies, alltrees, quadrats, Treatment, nestindex) %>% 
            summarise_all(funs(sum)) %>% 
            spread(key = Morphospecies, Abundance, fill =0))
allspCPA_adonis <- 
  adonis(allspCPA[,5:592]~
           nestindex, 
         permutations = 999,
         strata = allspCPA$alltrees,
         method ="chao",
         data = allspCPA)
##Species CP
allspCP <- 
  na.omit(puramona %>% 
            ungroup() %>%  
            filter(Site =="CP") %>% 
            dplyr::select(Morphospecies, Sampling, alltrees, quadrats, Treatment, nestindex, Abundance) %>% 
            group_by(Morphospecies, Sampling, alltrees, quadrats, Treatment, nestindex) %>% 
            summarise_all(funs(sum)) %>% 
            spread(key = Morphospecies, Abundance, fill =0))
allspCP_adonis <- 
  adonis(allspCP[,6:800]~
           nestindex*Sampling, 
         permutations = 999,
         strata = allspCP$alltrees,
         method ="chao",
         data = allspCP)

##Species DOB
allspDOB <- 
  na.omit(puramona %>% 
            ungroup() %>%  
            filter(Site =="DO" & Sampling =="B") %>% 
            dplyr::select(Morphospecies, alltrees, quadrats, Treatment, nestindex, Abundance) %>% 
            group_by(Morphospecies, alltrees, quadrats, Treatment, nestindex) %>% 
            summarise_all(funs(sum)) %>% 
            spread(key = Morphospecies, Abundance, fill =0))
allspDOB_adonis <- 
  adonis(allspDOB[,5:304]~
           nestindex, 
         permutations = 999,
         strata = allspDOB$alltrees,
         method ="chao",
         data = allspDOB)
##Species DOA
allspDOA <- 
  na.omit(puramona %>% 
            ungroup() %>%  
            filter(Site =="DO" & Sampling =="A") %>% 
            dplyr::select(Morphospecies, alltrees, quadrats, Treatment, nestindex, Abundance) %>% 
            group_by(Morphospecies, alltrees, quadrats, Treatment, nestindex) %>% 
            summarise_all(funs(sum)) %>% 
            spread(key = Morphospecies, Abundance, fill =0))
allspDOA_adonis <- 
  adonis(allspDOA[,5:317]~
           nestindex, 
         permutations = 999,
         strata = allspDOA$alltrees,
         method ="chao",
         data = allspDOA)
##Species DO
allspDO <- 
  na.omit(puramona %>% 
            ungroup() %>%  
            filter(Site =="DO") %>% 
            dplyr::select(Morphospecies, Sampling, alltrees, quadrats, Treatment, nestindex, Abundance) %>% 
            group_by(Morphospecies, Sampling, alltrees, quadrats, Treatment, nestindex) %>% 
            summarise_all(funs(sum)) %>% 
            spread(key = Morphospecies, Abundance, fill =0))
allspDO_adonis <- 
  adonis(allspDO[,6:506]~
           nestindex*Sampling, 
         permutations = 999,
         strata = allspDO$alltrees,
         method ="chao",
         data = allspDO)


##Species ERB
allspERB <- 
  na.omit(puramona %>% 
            ungroup() %>%  
            filter(Site =="ER" & Sampling =="B") %>% 
            dplyr::select(Morphospecies, alltrees, quadrats, Treatment, nestindex, Abundance) %>% 
            group_by(Morphospecies, alltrees, quadrats, Treatment, nestindex) %>% 
            summarise_all(funs(sum)) %>% 
            spread(key = Morphospecies, Abundance, fill =0))
allspERB_adonis <- 
  adonis(allspERB[,5:315]~
           nestindex, 
         permutations = 999,
         strata = allspERB$alltrees,
         method ="chao",
         data = allspERB)
##Species ERA
allspERA <- 
  na.omit(puramona %>% 
            ungroup() %>%  
            filter(Site =="ER" & Sampling =="A") %>% 
            dplyr::select(Morphospecies, alltrees, quadrats, Treatment, nestindex, Abundance) %>% 
            group_by(Morphospecies, alltrees, quadrats, Treatment, nestindex) %>% 
            summarise_all(funs(sum)) %>% 
            spread(key = Morphospecies, Abundance, fill =0))
allspERA_adonis <- 
  adonis(allspERA[,5:431]~
           nestindex, 
         permutations = 999,
         strata = allspERA$alltrees,
         method ="chao",
         data = allspERA)
##Species ER
allspER <- 
  na.omit(puramona %>% 
            ungroup() %>%  
            filter(Site =="ER") %>% 
            dplyr::select(Morphospecies, Sampling, alltrees, quadrats, Treatment, nestindex, Abundance) %>% 
            group_by(Morphospecies, Sampling, alltrees, quadrats, Treatment, nestindex) %>% 
            summarise_all(funs(sum)) %>% 
            spread(key = Morphospecies, Abundance, fill =0))
allspER_adonis <- 
  adonis(allspER[,6:613]~
           nestindex*Sampling, 
         permutations = 999,
         strata = allspER$alltrees,
         method ="chao",
         data = allspER)

# nestindex adonis per site on diet-------------------------------------------------
##Diet CPB
alldieCPB <- 
  na.omit(puramona %>% 
            ungroup() %>%  
            filter(Site =="CP" & Sampling =="B") %>% 
            dplyr::select(Diet, alltrees, quadrats, Treatment, nestindex, Abundance) %>% 
            group_by(Diet, alltrees, quadrats, Treatment, nestindex) %>% 
            summarise_all(funs(sum)) %>% 
            spread(key = Diet, Abundance, fill =0))
alldieCPB_adonis <- 
  adonis(alldieCPB[,5:20]~
           nestindex, 
         permutations = 999,
         strata = alldieCPB$alltrees,
         method ="chao",
         data = alldieCPB)
##Diet CPa
alldieCPA <- 
  na.omit(puramona %>% 
            ungroup() %>%  
            filter(Site =="CP" & Sampling =="A") %>% 
            dplyr::select(Diet, alltrees, quadrats, Treatment, nestindex, Abundance) %>% 
            group_by(Diet, alltrees, quadrats, Treatment, nestindex) %>% 
            summarise_all(funs(sum)) %>% 
            spread(key = Diet, Abundance, fill =0))
alldieCPA_adonis <- 
  adonis(alldieCPA[,5:20]~
           nestindex, 
         permutations = 999,
         strata = alldieCPA$alltrees,
         method ="chao",
         data = alldieCPA)
##Diet CP
alldieCP <- 
  na.omit(puramona %>% 
            ungroup() %>%  
            filter(Site =="CP") %>% 
            dplyr::select(Diet, Sampling, alltrees, quadrats, Treatment, nestindex, Abundance) %>% 
            group_by(Diet, Sampling, alltrees, quadrats, Treatment, nestindex) %>% 
            summarise_all(funs(sum)) %>% 
            spread(key = Diet, Abundance, fill =0))
alldieCP_adonis <- 
  adonis(alldieCP[,6:21]~
           nestindex*Sampling, 
         permutations = 999,
         strata = alldieCP$alltrees,
         method ="chao",
         data = alldieCP)

##Diet DOB
alldieDOB <- 
  na.omit(puramona %>% 
            ungroup() %>%  
            filter(Site =="DO" & Sampling =="B") %>% 
            dplyr::select(Diet, alltrees, quadrats, Treatment, nestindex, Abundance) %>% 
            group_by(Diet, alltrees, quadrats, Treatment, nestindex) %>% 
            summarise_all(funs(sum)) %>% 
            spread(key = Diet, Abundance, fill =0))
alldieDOB_adonis <- 
  adonis(alldieDOB[,5:20]~
           nestindex, 
         permutations = 999,
         strata = alldieDOB$alltrees,
         method ="chao",
         data = alldieDOB)
##Diet DOA
alldieDOA <- 
  na.omit(puramona %>% 
            ungroup() %>%  
            filter(Site =="DO" & Sampling =="A") %>% 
            dplyr::select(Diet, alltrees, quadrats, Treatment, nestindex, Abundance) %>% 
            group_by(Diet, alltrees, quadrats, Treatment, nestindex) %>% 
            summarise_all(funs(sum)) %>% 
            spread(key = Diet, Abundance, fill =0))
alldieDOA_adonis <- 
  adonis(alldieDOA[,5:18]~
           nestindex, 
         permutations = 999,
         strata = alldieDOA$alltrees,
         method ="chao",
         data = alldieDOA)
##Diet DO
alldieDO <- 
  na.omit(puramona %>% 
            ungroup() %>%  
            filter(Site =="DO") %>% 
            dplyr::select(Diet, Sampling, alltrees, quadrats, Treatment, nestindex, Abundance) %>% 
            group_by(Diet, Sampling, alltrees, quadrats, Treatment, nestindex) %>% 
            summarise_all(funs(sum)) %>% 
            spread(key = Diet, Abundance, fill =0))
alldieDO_adonis <- 
  adonis(alldieDO[,6:21]~
           nestindex*Sampling, 
         permutations = 999,
         strata = alldieDO$alltrees,
         method ="chao",
         data = alldieDO)


##Diet ERB
alldieERB <- 
  na.omit(puramona %>% 
            ungroup() %>%  
            filter(Site =="ER" & Sampling =="B") %>% 
            dplyr::select(Diet, alltrees, quadrats, Treatment, nestindex, Abundance) %>% 
            group_by(Diet, alltrees, quadrats, Treatment, nestindex) %>% 
            summarise_all(funs(sum)) %>% 
            spread(key = Diet, Abundance, fill =0))
alldieERB_adonis <- 
  adonis(alldieERB[,5:19]~
           nestindex, 
         permutations = 999,
         strata = alldieERB$alltrees,
         method ="chao",
         data = alldieERB)
##Diet ERA
alldieERA <- 
  na.omit(puramona %>% 
            ungroup() %>%  
            filter(Site =="ER" & Sampling =="A") %>% 
            dplyr::select(Diet, alltrees, quadrats, Treatment, nestindex, Abundance) %>% 
            group_by(Diet, alltrees, quadrats, Treatment, nestindex) %>% 
            summarise_all(funs(sum)) %>% 
            spread(key = Diet, Abundance, fill =0))
alldieERA_adonis <- 
  adonis(alldieERA[,5:18]~
           nestindex, 
         permutations = 999,
         strata = alldieERA$alltrees,
         method ="chao",
         data = alldieERA)

##Diet ER
alldieER <- 
  na.omit(puramona %>% 
            ungroup() %>%  
            filter(Site =="ER") %>% 
            dplyr::select(Diet, Sampling, alltrees, quadrats, Treatment, nestindex, Abundance) %>% 
            group_by(Diet, Sampling, alltrees, quadrats, Treatment, nestindex) %>% 
            summarise_all(funs(sum)) %>% 
            spread(key = Diet, Abundance, fill =0))
alldieER_adonis <- 
  adonis(alldieER[,6:21]~
           nestindex*Sampling, 
         permutations = 999,
         strata = alldieER$alltrees,
         method ="chao",
         data = alldieER)









# nestindex adonis per site on main diets-------------------------------------------------
##Diet CPB
alldieCPB <- 
  na.omit(puramona %>% 
  ungroup() %>% 
  dplyr::filter(Site =="CP" & Sampling =="B" & Diet != "xylo" & Diet != "nect" & Diet != "poll" & Diet != "klep" & Diet != "poll/nect" & Diet != "gran") %>% 
  dplyr::select(Diet, alltrees, quadrats, Treatment, nestindex, Abundance) %>%
  group_by(Diet, alltrees, quadrats, Treatment, nestindex) %>%
  summarise_all(funs(sum)) %>%
  spread(key = Diet, Abundance, fill =0))
alldieCPB_adonis <- 
  adonis(alldieCPB[,5:14]~
           nestindex, 
         permutations = 999,
         strata = alldieCPB$alltrees,
         method ="chao",
         data = alldieCPB)
##Diet CPA
alldieCPA <- 
  na.omit(puramona %>% 
            ungroup() %>%  
            filter(Site =="CP" & Sampling =="A"& Diet != "xylo" & Diet != "nect" & Diet != "poll" & Diet != "klep" & Diet != "poll/nect" & Diet != "gran") %>% 
            dplyr::select(Diet, alltrees, quadrats, Treatment, nestindex, Abundance) %>% 
            group_by(Diet, alltrees, quadrats, Treatment, nestindex) %>% 
            summarise_all(funs(sum)) %>% 
            spread(key = Diet, Abundance, fill =0))
alldieCPA_adonis <- 
  adonis(alldieCPA[,5:14]~
           nestindex, 
         permutations = 999,
         strata = alldieCPA$alltrees,
         method ="chao",
         data = alldieCPA)
##Diet CP
alldieCP <- 
  na.omit(puramona %>% 
            ungroup() %>%  
            filter(Site =="CP"& Diet != "xylo" & Diet != "nect" & Diet != "poll" & Diet != "klep" & Diet != "poll/nect" & Diet != "gran") %>% 
            dplyr::select(Diet, Sampling, alltrees, quadrats, Treatment, nestindex, Abundance) %>% 
            group_by(Diet, Sampling, alltrees, quadrats, Treatment, nestindex) %>% 
            summarise_all(funs(sum)) %>% 
            spread(key = Diet, Abundance, fill =0))
alldieCP_adonis <- 
  adonis(alldieCP[,6:15]~
           nestindex*Sampling, 
         permutations = 999,
         strata = alldieCP$alltrees,
         method ="chao",
         data = alldieCP)

##Diet DOB
alldieDOB <- 
  na.omit(puramona %>% 
            ungroup() %>%  
            filter(Site =="DO" & Sampling =="B"& Diet != "xylo" & Diet != "nect" & Diet != "poll" & Diet != "klep" & Diet != "poll/nect" & Diet != "gran") %>% 
            dplyr::select(Diet, alltrees, quadrats, Treatment, nestindex, Abundance) %>% 
            group_by(Diet, alltrees, quadrats, Treatment, nestindex) %>% 
            summarise_all(funs(sum)) %>% 
            spread(key = Diet, Abundance, fill =0))
alldieDOB_adonis <- 
  adonis(alldieDOB[,5:14]~
           nestindex, 
         permutations = 999,
         strata = alldieDOB$alltrees,
         method ="chao",
         data = alldieDOB)
##Diet DOA
alldieDOA <- 
  na.omit(puramona %>% 
            ungroup() %>%  
            filter(Site =="DO" & Sampling =="A"& Diet != "xylo" & Diet != "nect" & Diet != "poll" & Diet != "klep" & Diet != "poll/nect" & Diet != "gran") %>% 
            dplyr::select(Diet, alltrees, quadrats, Treatment, nestindex, Abundance) %>% 
            group_by(Diet, alltrees, quadrats, Treatment, nestindex) %>% 
            summarise_all(funs(sum)) %>% 
            spread(key = Diet, Abundance, fill =0))
alldieDOA_adonis <- 
  adonis(alldieDOA[,5:14]~
           nestindex, 
         permutations = 999,
         strata = alldieDOA$alltrees,
         method ="chao",
         data = alldieDOA)
##Diet DO
alldieDO <- 
  na.omit(puramona %>% 
            ungroup() %>%  
            filter(Site =="DO"& Diet != "xylo" & Diet != "nect" & Diet != "poll" & Diet != "klep" & Diet != "poll/nect" & Diet != "gran") %>% 
            dplyr::select(Diet, Sampling, alltrees, quadrats, Treatment, nestindex, Abundance) %>% 
            group_by(Diet, Sampling, alltrees, quadrats, Treatment, nestindex) %>% 
            summarise_all(funs(sum)) %>% 
            spread(key = Diet, Abundance, fill =0))
alldieDO_adonis <- 
  adonis(alldieDO[,6:15]~
           nestindex*Sampling, 
         permutations = 999,
         strata = alldieDO$alltrees,
         method ="chao",
         data = alldieDO)


##Diet ERB
alldieERB <- 
  na.omit(puramona %>% 
            ungroup() %>%  
            filter(Site =="ER" & Sampling =="B"& Diet != "xylo" & Diet != "nect" & Diet != "poll" & Diet != "klep" & Diet != "poll/nect" & Diet != "gran") %>% 
            dplyr::select(Diet, alltrees, quadrats, Treatment, nestindex, Abundance) %>% 
            group_by(Diet, alltrees, quadrats, Treatment, nestindex) %>% 
            summarise_all(funs(sum)) %>% 
            spread(key = Diet, Abundance, fill =0))
alldieERB_adonis <- 
  adonis(alldieERB[,5:14]~
           nestindex, 
         permutations = 999,
         strata = alldieERB$alltrees,
         method ="chao",
         data = alldieERB)
##Diet ERA
alldieERA <- 
  na.omit(puramona %>% 
            ungroup() %>%  
            filter(Site =="ER" & Sampling =="A"& Diet != "xylo" & Diet != "nect" & Diet != "poll" & Diet != "klep" & Diet != "poll/nect" & Diet != "gran") %>% 
            dplyr::select(Diet, alltrees, quadrats, Treatment, nestindex, Abundance) %>% 
            group_by(Diet, alltrees, quadrats, Treatment, nestindex) %>% 
            summarise_all(funs(sum)) %>% 
            spread(key = Diet, Abundance, fill =0))
alldieERA_adonis <- 
  adonis(alldieERA[,5:14]~
           nestindex, 
         permutations = 999,
         strata = alldieERA$alltrees,
         method ="chao",
         data = alldieERA)

##Diet ER
alldieER <- 
  na.omit(puramona %>% 
            ungroup() %>%  
            filter(Site =="ER"& Diet != "xylo" & Diet != "nect" & Diet != "poll" & Diet != "klep" & Diet != "poll/nect" & Diet != "gran") %>% 
            dplyr::select(Diet, Sampling, alltrees, quadrats, Treatment, nestindex, Abundance) %>% 
            group_by(Diet, Sampling, alltrees, quadrats, Treatment, nestindex) %>% 
            summarise_all(funs(sum)) %>% 
            spread(key = Diet, Abundance, fill =0))
alldieER_adonis <- 
  adonis(alldieER[,6:15]~
           nestindex*Sampling, 
         permutations = 999,
         strata = alldieER$alltrees,
         method ="chao",
         data = alldieER)










# nestindex adonis per site on preds----------------------------------------------------------
##Species CPB
predCPB <- 
  na.omit(puramona %>% 
            ungroup() %>%  
            filter(Site =="CP" & Sampling =="B") %>% 
            filter(Diet == "pred") %>% 
            dplyr::select(Morphospecies, alltrees, quadrats, Treatment, nestindex, Abundance) %>% 
            group_by(Morphospecies, alltrees, quadrats, Treatment, nestindex) %>% 
            summarise_all(funs(sum)) %>% 
            spread(key = Morphospecies, Abundance, fill =0))
predCPB_adonis <- 
  adonis(predCPB[,5:121]~
           nestindex, 
         permutations = 999,
         strata = predCPB$alltrees,
         method ="chao",
         data = predCPB)
##Species CPA
predCPA <- 
  na.omit(puramona %>% 
            ungroup() %>%  
            filter(Site =="CP" & Sampling =="A") %>% 
            filter(Diet == "pred") %>% 
            dplyr::select(Morphospecies, alltrees, quadrats, Treatment, nestindex, Abundance) %>% 
            group_by(Morphospecies, alltrees, quadrats, Treatment, nestindex) %>% 
            summarise_all(funs(sum)) %>% 
            spread(key = Morphospecies, Abundance, fill =0))
predCPA_adonis <- 
  adonis(predCPA[,5:162]~
           nestindex, 
         permutations = 999,
         strata = predCPA$alltrees,
         method ="chao",
         data = predCPA)
##Species CP
predCP <- 
  na.omit(puramona %>% 
            ungroup() %>%  
            filter(Site =="CP") %>% 
            filter(Diet == "pred") %>% 
            dplyr::select(Morphospecies, Sampling, alltrees, quadrats, Treatment, nestindex, Abundance) %>% 
            group_by(Morphospecies, Sampling, alltrees, quadrats, Treatment, nestindex) %>% 
            summarise_all(funs(sum)) %>% 
            spread(key = Morphospecies, Abundance, fill =0))
predCP_adonis <- 
  adonis(predCP[,6:222]~
           nestindex*Sampling, 
         permutations = 999,
         strata = predCP$alltrees,
         method ="chao",
         data = predCP)

##Species DOB
predDOB <- 
  na.omit(puramona %>% 
            ungroup() %>%  
            filter(Site =="DO" & Sampling =="B") %>% 
            filter(Diet == "pred") %>% 
            dplyr::select(Morphospecies, alltrees, quadrats, Treatment, nestindex, Abundance) %>% 
            group_by(Morphospecies, alltrees, quadrats, Treatment, nestindex) %>% 
            summarise_all(funs(sum)) %>% 
            spread(key = Morphospecies, Abundance, fill =0))
predDOB_adonis <- 
  adonis(predDOB[,5:105]~
           nestindex, 
         permutations = 999,
         strata = predDOB$alltrees,
         method ="chao",
         data = predDOB)
##Species DOA
predDOA <- 
  na.omit(puramona %>% 
            ungroup() %>%  
            filter(Site =="DO" & Sampling =="A") %>%
            filter(Diet == "pred") %>% 
            dplyr::select(Morphospecies, alltrees, quadrats, Treatment, nestindex, Abundance) %>% 
            group_by(Morphospecies, alltrees, quadrats, Treatment, nestindex) %>% 
            summarise_all(funs(sum)) %>% 
            spread(key = Morphospecies, Abundance, fill =0))
predDOA_adonis <- 
  adonis(predDOA[,5:127]~
           nestindex, 
         permutations = 999,
         strata = predDOA$alltrees,
         method ="chao",
         data = predDOA)
##Species DO
predDO <- 
  na.omit(puramona %>% 
            ungroup() %>%  
            filter(Site =="DO") %>% 
            filter(Diet == "pred") %>% 
            dplyr::select(Morphospecies, Sampling, alltrees, quadrats, Treatment, nestindex, Abundance) %>% 
            group_by(Morphospecies, Sampling, alltrees, quadrats, Treatment, nestindex) %>% 
            summarise_all(funs(sum)) %>% 
            spread(key = Morphospecies, Abundance, fill =0))
predDO_adonis <- 
  adonis(predDO[,6:184]~
           nestindex*Sampling, 
         permutations = 999,
         strata = predDO$alltrees,
         method ="chao",
         data = predDO)


##Species ERB
predERB <- 
  na.omit(puramona %>% 
            ungroup() %>%  
            filter(Site =="ER" & Sampling =="B") %>% 
            filter(Diet == "pred") %>% 
            dplyr::select(Morphospecies, alltrees, quadrats, Treatment, nestindex, Abundance) %>% 
            group_by(Morphospecies, alltrees, quadrats, Treatment, nestindex) %>% 
            summarise_all(funs(sum)) %>% 
            spread(key = Morphospecies, Abundance, fill =0))
predERB_adonis <- 
  adonis(predERB[,5:101]~
           nestindex, 
         permutations = 999,
         strata = predERB$alltrees,
         method ="chao",
         data = predERB)
##Species ERA
predERA <- 
  na.omit(puramona %>% 
            ungroup() %>%  
            filter(Site =="ER" & Sampling =="A") %>% 
            filter(Diet == "pred") %>% 
            dplyr::select(Morphospecies, alltrees, quadrats, Treatment, nestindex, Abundance) %>% 
            group_by(Morphospecies, alltrees, quadrats, Treatment, nestindex) %>% 
            summarise_all(funs(sum)) %>% 
            spread(key = Morphospecies, Abundance, fill =0))
predERA_adonis <- 
  adonis(predERA[,5:153]~
           nestindex, 
         permutations = 999,
         strata = predERA$alltrees,
         method ="chao",
         data = predERA)
##Species ER
predER <- 
  na.omit(puramona %>% 
            ungroup() %>%  
            filter(Site =="ER") %>% 
            filter(Diet == "pred") %>% 
            dplyr::select(Morphospecies, Sampling, alltrees, quadrats, Treatment, nestindex, Abundance) %>% 
            group_by(Morphospecies, Sampling, alltrees, quadrats, Treatment, nestindex) %>% 
            summarise_all(funs(sum)) %>% 
            spread(key = Morphospecies, Abundance, fill =0))
predER_adonis <- 
  adonis(predER[,6:200]~
           nestindex*Sampling, 
         permutations = 999,
         strata = predER$alltrees,
         method ="chao",
         data = predER)



# nestabund adonis per site on species----------------------------------------------------------
##Species CPB
allspCPB <- 
  na.omit(puramona %>% 
            ungroup() %>%  
            filter(Site =="CP" & Sampling =="B") %>% 
            dplyr::select(Morphospecies, alltrees, quadrats, Treatment, nestabund, Abundance) %>% 
            group_by(Morphospecies, alltrees, quadrats, Treatment, nestabund) %>% 
            summarise_all(funs(sum)) %>% 
            spread(key = Morphospecies, Abundance, fill =0))
allspCPB_adonis <- 
  adonis(allspCPB[,5:404]~
           nestabund, 
         permutations = 999,
         strata = allspCPB$alltrees,
         method ="chao",
         data = allspCPB)
##Species CPA
allspCPA <- 
  na.omit(puramona %>% 
            ungroup() %>%  
            filter(Site =="CP" & Sampling =="A") %>% 
            dplyr::select(Morphospecies, alltrees, quadrats, Treatment, nestabund, Abundance) %>% 
            group_by(Morphospecies, alltrees, quadrats, Treatment, nestabund) %>% 
            summarise_all(funs(sum)) %>% 
            spread(key = Morphospecies, Abundance, fill =0))
allspCPA_adonis <- 
  adonis(allspCPA[,5:592]~
           nestabund, 
         permutations = 999,
         strata = allspCPA$alltrees,
         method ="chao",
         data = allspCPA)
##Species CP
allspCP <- 
  na.omit(puramona %>% 
            ungroup() %>%  
            filter(Site =="CP") %>% 
            dplyr::select(Morphospecies, Sampling, alltrees, quadrats, Treatment, nestabund, Abundance) %>% 
            group_by(Morphospecies, Sampling, alltrees, quadrats, Treatment, nestabund) %>% 
            summarise_all(funs(sum)) %>% 
            spread(key = Morphospecies, Abundance, fill =0))
allspCP_adonis <- 
  adonis(allspCP[,6:800]~
           nestabund*Sampling, 
         permutations = 999,
         strata = allspCP$alltrees,
         method ="chao",
         data = allspCP)

##Species DOB
allspDOB <- 
  na.omit(puramona %>% 
            ungroup() %>%  
            filter(Site =="DO" & Sampling =="B") %>% 
            dplyr::select(Morphospecies, alltrees, quadrats, Treatment, nestabund, Abundance) %>% 
            group_by(Morphospecies, alltrees, quadrats, Treatment, nestabund) %>% 
            summarise_all(funs(sum)) %>% 
            spread(key = Morphospecies, Abundance, fill =0))
allspDOB_adonis <- 
  adonis(allspDOB[,5:304]~
           nestabund, 
         permutations = 999,
         strata = allspDOB$alltrees,
         method ="chao",
         data = allspDOB)
##Species DOA
allspDOA <- 
  na.omit(puramona %>% 
            ungroup() %>%  
            filter(Site =="DO" & Sampling =="A") %>% 
            dplyr::select(Morphospecies, alltrees, quadrats, Treatment, nestabund, Abundance) %>% 
            group_by(Morphospecies, alltrees, quadrats, Treatment, nestabund) %>% 
            summarise_all(funs(sum)) %>% 
            spread(key = Morphospecies, Abundance, fill =0))
allspDOA_adonis <- 
  adonis(allspDOA[,5:317]~
           nestabund, 
         permutations = 999,
         strata = allspDOA$alltrees,
         method ="chao",
         data = allspDOA)
##Species DO
allspDO <- 
  na.omit(puramona %>% 
            ungroup() %>%  
            filter(Site =="DO") %>% 
            dplyr::select(Morphospecies, Sampling, alltrees, quadrats, Treatment, nestabund, Abundance) %>% 
            group_by(Morphospecies, Sampling, alltrees, quadrats, Treatment, nestabund) %>% 
            summarise_all(funs(sum)) %>% 
            spread(key = Morphospecies, Abundance, fill =0))
allspDO_adonis <- 
  adonis(allspDO[,6:506]~
           nestabund*Sampling, 
         permutations = 999,
         strata = allspDO$alltrees,
         method ="chao",
         data = allspDO)


##Species ERB
allspERB <- 
  na.omit(puramona %>% 
            ungroup() %>%  
            filter(Site =="ER" & Sampling =="B") %>% 
            dplyr::select(Morphospecies, alltrees, quadrats, Treatment, nestabund, Abundance) %>% 
            group_by(Morphospecies, alltrees, quadrats, Treatment, nestabund) %>% 
            summarise_all(funs(sum)) %>% 
            spread(key = Morphospecies, Abundance, fill =0))
allspERB_adonis <- 
  adonis(allspERB[,5:315]~
           nestabund, 
         permutations = 999,
         strata = allspERB$alltrees,
         method ="chao",
         data = allspERB)
##Species ERA
allspERA <- 
  na.omit(puramona %>% 
            ungroup() %>%  
            filter(Site =="ER" & Sampling =="A") %>% 
            dplyr::select(Morphospecies, alltrees, quadrats, Treatment, nestabund, Abundance) %>% 
            group_by(Morphospecies, alltrees, quadrats, Treatment, nestabund) %>% 
            summarise_all(funs(sum)) %>% 
            spread(key = Morphospecies, Abundance, fill =0))
allspERA_adonis <- 
  adonis(allspERA[,5:431]~
           nestabund, 
         permutations = 999,
         strata = allspERA$alltrees,
         method ="chao",
         data = allspERA)
##Species ER
allspER <- 
  na.omit(puramona %>% 
            ungroup() %>%  
            filter(Site =="ER") %>% 
            dplyr::select(Morphospecies, Sampling, alltrees, quadrats, Treatment, nestabund, Abundance) %>% 
            group_by(Morphospecies, Sampling, alltrees, quadrats, Treatment, nestabund) %>% 
            summarise_all(funs(sum)) %>% 
            spread(key = Morphospecies, Abundance, fill =0))
allspER_adonis <- 
  adonis(allspER[,6:613]~
           nestabund*Sampling, 
         permutations = 999,
         strata = allspER$alltrees,
         method ="chao",
         data = allspER)

# nestabund adonis per site on diet-------------------------------------------------
##Diet CPB
alldieCPB <- 
  na.omit(puramona %>% 
            ungroup() %>%  
            filter(Site =="CP" & Sampling =="B") %>% 
            dplyr::select(Diet, alltrees, quadrats, Treatment, nestabund, Abundance) %>% 
            group_by(Diet, alltrees, quadrats, Treatment, nestabund) %>% 
            summarise_all(funs(sum)) %>% 
            spread(key = Diet, Abundance, fill =0))
alldieCPB_adonis <- 
  adonis(alldieCPB[,5:20]~
           nestabund, 
         permutations = 999,
         strata = alldieCPB$alltrees,
         method ="chao",
         data = alldieCPB)
##Diet CPa
alldieCPA <- 
  na.omit(puramona %>% 
            ungroup() %>%  
            filter(Site =="CP" & Sampling =="A") %>% 
            dplyr::select(Diet, alltrees, quadrats, Treatment, nestabund, Abundance) %>% 
            group_by(Diet, alltrees, quadrats, Treatment, nestabund) %>% 
            summarise_all(funs(sum)) %>% 
            spread(key = Diet, Abundance, fill =0))
alldieCPA_adonis <- 
  adonis(alldieCPA[,5:20]~
           nestabund, 
         permutations = 999,
         strata = alldieCPA$alltrees,
         method ="chao",
         data = alldieCPA)
##Diet CP
alldieCP <- 
  na.omit(puramona %>% 
            ungroup() %>%  
            filter(Site =="CP") %>% 
            dplyr::select(Diet, Sampling, alltrees, quadrats, Treatment, nestabund, Abundance) %>% 
            group_by(Diet, Sampling, alltrees, quadrats, Treatment, nestabund) %>% 
            summarise_all(funs(sum)) %>% 
            spread(key = Diet, Abundance, fill =0))
alldieCP_adonis <- 
  adonis(alldieCP[,6:21]~
           nestabund*Sampling, 
         permutations = 999,
         strata = alldieCP$alltrees,
         method ="chao",
         data = alldieCP)

##Diet DOB
alldieDOB <- 
  na.omit(puramona %>% 
            ungroup() %>%  
            filter(Site =="DO" & Sampling =="B") %>% 
            dplyr::select(Diet, alltrees, quadrats, Treatment, nestabund, Abundance) %>% 
            group_by(Diet, alltrees, quadrats, Treatment, nestabund) %>% 
            summarise_all(funs(sum)) %>% 
            spread(key = Diet, Abundance, fill =0))
alldieDOB_adonis <- 
  adonis(alldieDOB[,5:20]~
           nestabund, 
         permutations = 999,
         strata = alldieDOB$alltrees,
         method ="chao",
         data = alldieDOB)
##Diet DOA
alldieDOA <- 
  na.omit(puramona %>% 
            ungroup() %>%  
            filter(Site =="DO" & Sampling =="A") %>% 
            dplyr::select(Diet, alltrees, quadrats, Treatment, nestabund, Abundance) %>% 
            group_by(Diet, alltrees, quadrats, Treatment, nestabund) %>% 
            summarise_all(funs(sum)) %>% 
            spread(key = Diet, Abundance, fill =0))
alldieDOA_adonis <- 
  adonis(alldieDOA[,5:18]~
           nestabund, 
         permutations = 999,
         strata = alldieDOA$alltrees,
         method ="chao",
         data = alldieDOA)
##Diet DO
alldieDO <- 
  na.omit(puramona %>% 
            ungroup() %>%  
            filter(Site =="DO") %>% 
            dplyr::select(Diet, Sampling, alltrees, quadrats, Treatment, nestabund, Abundance) %>% 
            group_by(Diet, Sampling, alltrees, quadrats, Treatment, nestabund) %>% 
            summarise_all(funs(sum)) %>% 
            spread(key = Diet, Abundance, fill =0))
alldieDO_adonis <- 
  adonis(alldieDO[,6:21]~
           nestabund*Sampling, 
         permutations = 999,
         strata = alldieDO$alltrees,
         method ="chao",
         data = alldieDO)


##Diet ERB
alldieERB <- 
  na.omit(puramona %>% 
            ungroup() %>%  
            filter(Site =="ER" & Sampling =="B") %>% 
            dplyr::select(Diet, alltrees, quadrats, Treatment, nestabund, Abundance) %>% 
            group_by(Diet, alltrees, quadrats, Treatment, nestabund) %>% 
            summarise_all(funs(sum)) %>% 
            spread(key = Diet, Abundance, fill =0))
alldieERB_adonis <- 
  adonis(alldieERB[,5:19]~
           nestabund, 
         permutations = 999,
         strata = alldieERB$alltrees,
         method ="chao",
         data = alldieERB)
##Diet ERA
alldieERA <- 
  na.omit(puramona %>% 
            ungroup() %>%  
            filter(Site =="ER" & Sampling =="A") %>% 
            dplyr::select(Diet, alltrees, quadrats, Treatment, nestabund, Abundance) %>% 
            group_by(Diet, alltrees, quadrats, Treatment, nestabund) %>% 
            summarise_all(funs(sum)) %>% 
            spread(key = Diet, Abundance, fill =0))
alldieERA_adonis <- 
  adonis(alldieERA[,5:18]~
           nestabund, 
         permutations = 999,
         strata = alldieERA$alltrees,
         method ="chao",
         data = alldieERA)

##Diet ER
alldieER <- 
  na.omit(puramona %>% 
            ungroup() %>%  
            filter(Site =="ER") %>% 
            dplyr::select(Diet, Sampling, alltrees, quadrats, Treatment, nestabund, Abundance) %>% 
            group_by(Diet, Sampling, alltrees, quadrats, Treatment, nestabund) %>% 
            summarise_all(funs(sum)) %>% 
            spread(key = Diet, Abundance, fill =0))
alldieER_adonis <- 
  adonis(alldieER[,6:21]~
           nestabund*Sampling, 
         permutations = 999,
         strata = alldieER$alltrees,
         method ="chao",
         data = alldieER)









# nestabund adonis per site on main diets-------------------------------------------------
##Diet CPB
alldieCPB <- 
  na.omit(puramona %>% 
            ungroup() %>% 
            dplyr::filter(Site =="CP" & Sampling =="B" & Diet != "xylo" & Diet != "nect" & Diet != "poll" & Diet != "klep" & Diet != "poll/nect" & Diet != "gran") %>% 
            dplyr::select(Diet, alltrees, quadrats, Treatment, nestabund, Abundance) %>%
            group_by(Diet, alltrees, quadrats, Treatment, nestabund) %>%
            summarise_all(funs(sum)) %>%
            spread(key = Diet, Abundance, fill =0))
alldieCPB_adonis <- 
  adonis(alldieCPB[,5:14]~
           nestabund, 
         permutations = 999,
         strata = alldieCPB$alltrees,
         method ="chao",
         data = alldieCPB)
##Diet CPA
alldieCPA <- 
  na.omit(puramona %>% 
            ungroup() %>%  
            filter(Site =="CP" & Sampling =="A"& Diet != "xylo" & Diet != "nect" & Diet != "poll" & Diet != "klep" & Diet != "poll/nect" & Diet != "gran") %>% 
            dplyr::select(Diet, alltrees, quadrats, Treatment, nestabund, Abundance) %>% 
            group_by(Diet, alltrees, quadrats, Treatment, nestabund) %>% 
            summarise_all(funs(sum)) %>% 
            spread(key = Diet, Abundance, fill =0))
alldieCPA_adonis <- 
  adonis(alldieCPA[,5:14]~
           nestabund, 
         permutations = 999,
         strata = alldieCPA$alltrees,
         method ="chao",
         data = alldieCPA)
##Diet CP
alldieCP <- 
  na.omit(puramona %>% 
            ungroup() %>%  
            filter(Site =="CP"& Diet != "xylo" & Diet != "nect" & Diet != "poll" & Diet != "klep" & Diet != "poll/nect" & Diet != "gran") %>% 
            dplyr::select(Diet, Sampling, alltrees, quadrats, Treatment, nestabund, Abundance) %>% 
            group_by(Diet, Sampling, alltrees, quadrats, Treatment, nestabund) %>% 
            summarise_all(funs(sum)) %>% 
            spread(key = Diet, Abundance, fill =0))
alldieCP_adonis <- 
  adonis(alldieCP[,6:15]~
           nestabund*Sampling, 
         permutations = 999,
         strata = alldieCP$alltrees,
         method ="chao",
         data = alldieCP)

##Diet DOB
alldieDOB <- 
  na.omit(puramona %>% 
            ungroup() %>%  
            filter(Site =="DO" & Sampling =="B"& Diet != "xylo" & Diet != "nect" & Diet != "poll" & Diet != "klep" & Diet != "poll/nect" & Diet != "gran") %>% 
            dplyr::select(Diet, alltrees, quadrats, Treatment, nestabund, Abundance) %>% 
            group_by(Diet, alltrees, quadrats, Treatment, nestabund) %>% 
            summarise_all(funs(sum)) %>% 
            spread(key = Diet, Abundance, fill =0))
alldieDOB_adonis <- 
  adonis(alldieDOB[,5:14]~
           nestabund, 
         permutations = 999,
         strata = alldieDOB$alltrees,
         method ="chao",
         data = alldieDOB)
##Diet DOA
alldieDOA <- 
  na.omit(puramona %>% 
            ungroup() %>%  
            filter(Site =="DO" & Sampling =="A"& Diet != "xylo" & Diet != "nect" & Diet != "poll" & Diet != "klep" & Diet != "poll/nect" & Diet != "gran") %>% 
            dplyr::select(Diet, alltrees, quadrats, Treatment, nestabund, Abundance) %>% 
            group_by(Diet, alltrees, quadrats, Treatment, nestabund) %>% 
            summarise_all(funs(sum)) %>% 
            spread(key = Diet, Abundance, fill =0))
alldieDOA_adonis <- 
  adonis(alldieDOA[,5:14]~
           nestabund, 
         permutations = 999,
         strata = alldieDOA$alltrees,
         method ="chao",
         data = alldieDOA)
##Diet DO
alldieDO <- 
  na.omit(puramona %>% 
            ungroup() %>%  
            filter(Site =="DO"& Diet != "xylo" & Diet != "nect" & Diet != "poll" & Diet != "klep" & Diet != "poll/nect" & Diet != "gran") %>% 
            dplyr::select(Diet, Sampling, alltrees, quadrats, Treatment, nestabund, Abundance) %>% 
            group_by(Diet, Sampling, alltrees, quadrats, Treatment, nestabund) %>% 
            summarise_all(funs(sum)) %>% 
            spread(key = Diet, Abundance, fill =0))
alldieDO_adonis <- 
  adonis(alldieDO[,6:15]~
           nestabund*Sampling, 
         permutations = 999,
         strata = alldieDO$alltrees,
         method ="chao",
         data = alldieDO)


##Diet ERB
alldieERB <- 
  na.omit(puramona %>% 
            ungroup() %>%  
            filter(Site =="ER" & Sampling =="B"& Diet != "xylo" & Diet != "nect" & Diet != "poll" & Diet != "klep" & Diet != "poll/nect" & Diet != "gran") %>% 
            dplyr::select(Diet, alltrees, quadrats, Treatment, nestabund, Abundance) %>% 
            group_by(Diet, alltrees, quadrats, Treatment, nestabund) %>% 
            summarise_all(funs(sum)) %>% 
            spread(key = Diet, Abundance, fill =0))
alldieERB_adonis <- 
  adonis(alldieERB[,5:14]~
           nestabund, 
         permutations = 999,
         strata = alldieERB$alltrees,
         method ="chao",
         data = alldieERB)
##Diet ERA
alldieERA <- 
  na.omit(puramona %>% 
            ungroup() %>%  
            filter(Site =="ER" & Sampling =="A"& Diet != "xylo" & Diet != "nect" & Diet != "poll" & Diet != "klep" & Diet != "poll/nect" & Diet != "gran") %>% 
            dplyr::select(Diet, alltrees, quadrats, Treatment, nestabund, Abundance) %>% 
            group_by(Diet, alltrees, quadrats, Treatment, nestabund) %>% 
            summarise_all(funs(sum)) %>% 
            spread(key = Diet, Abundance, fill =0))
alldieERA_adonis <- 
  adonis(alldieERA[,5:14]~
           nestabund, 
         permutations = 999,
         strata = alldieERA$alltrees,
         method ="chao",
         data = alldieERA)

##Diet ER
alldieER <- 
  na.omit(puramona %>% 
            ungroup() %>%  
            filter(Site =="ER"& Diet != "xylo" & Diet != "nect" & Diet != "poll" & Diet != "klep" & Diet != "poll/nect" & Diet != "gran") %>% 
            dplyr::select(Diet, Sampling, alltrees, quadrats, Treatment, nestabund, Abundance) %>% 
            group_by(Diet, Sampling, alltrees, quadrats, Treatment, nestabund) %>% 
            summarise_all(funs(sum)) %>% 
            spread(key = Diet, Abundance, fill =0))
alldieER_adonis <- 
  adonis(alldieER[,6:15]~
           nestabund*Sampling, 
         permutations = 999,
         strata = alldieER$alltrees,
         method ="chao",
         data = alldieER)










# nestabund adonis per site on preds----------------------------------------------------------
##Species CPB
predCPB <- 
  na.omit(puramona %>% 
            ungroup() %>%  
            filter(Site =="CP" & Sampling =="B") %>% 
            filter(Diet == "pred") %>% 
            dplyr::select(Morphospecies, alltrees, quadrats, Treatment, nestabund, Abundance) %>% 
            group_by(Morphospecies, alltrees, quadrats, Treatment, nestabund) %>% 
            summarise_all(funs(sum)) %>% 
            spread(key = Morphospecies, Abundance, fill =0))
predCPB_adonis <- 
  adonis(predCPB[,5:121]~
           nestabund, 
         permutations = 999,
         strata = predCPB$alltrees,
         method ="chao",
         data = predCPB)
##Species CPA
predCPA <- 
  na.omit(puramona %>% 
            ungroup() %>%  
            filter(Site =="CP" & Sampling =="A") %>% 
            filter(Diet == "pred") %>% 
            dplyr::select(Morphospecies, alltrees, quadrats, Treatment, nestabund, Abundance) %>% 
            group_by(Morphospecies, alltrees, quadrats, Treatment, nestabund) %>% 
            summarise_all(funs(sum)) %>% 
            spread(key = Morphospecies, Abundance, fill =0))
predCPA_adonis <- 
  adonis(predCPA[,5:162]~
           nestabund, 
         permutations = 999,
         strata = predCPA$alltrees,
         method ="chao",
         data = predCPA)
##Species CP
predCP <- 
  na.omit(puramona %>% 
            ungroup() %>%  
            filter(Site =="CP") %>% 
            filter(Diet == "pred") %>% 
            dplyr::select(Morphospecies, Sampling, alltrees, quadrats, Treatment, nestabund, Abundance) %>% 
            group_by(Morphospecies, Sampling, alltrees, quadrats, Treatment, nestabund) %>% 
            summarise_all(funs(sum)) %>% 
            spread(key = Morphospecies, Abundance, fill =0))
predCP_adonis <- 
  adonis(predCP[,6:222]~
           nestabund*Sampling, 
         permutations = 999,
         strata = predCP$alltrees,
         method ="chao",
         data = predCP)

##Species DOB
predDOB <- 
  na.omit(puramona %>% 
            ungroup() %>%  
            filter(Site =="DO" & Sampling =="B") %>% 
            filter(Diet == "pred") %>% 
            dplyr::select(Morphospecies, alltrees, quadrats, Treatment, nestabund, Abundance) %>% 
            group_by(Morphospecies, alltrees, quadrats, Treatment, nestabund) %>% 
            summarise_all(funs(sum)) %>% 
            spread(key = Morphospecies, Abundance, fill =0))
predDOB_adonis <- 
  adonis(predDOB[,5:105]~
           nestabund, 
         permutations = 999,
         strata = predDOB$alltrees,
         method ="chao",
         data = predDOB)
##Species DOA
predDOA <- 
  na.omit(puramona %>% 
            ungroup() %>%  
            filter(Site =="DO" & Sampling =="A") %>%
            filter(Diet == "pred") %>% 
            dplyr::select(Morphospecies, alltrees, quadrats, Treatment, nestabund, Abundance) %>% 
            group_by(Morphospecies, alltrees, quadrats, Treatment, nestabund) %>% 
            summarise_all(funs(sum)) %>% 
            spread(key = Morphospecies, Abundance, fill =0))
predDOA_adonis <- 
  adonis(predDOA[,5:127]~
           nestabund, 
         permutations = 999,
         strata = predDOA$alltrees,
         method ="chao",
         data = predDOA)
##Species DO
predDO <- 
  na.omit(puramona %>% 
            ungroup() %>%  
            filter(Site =="DO") %>% 
            filter(Diet == "pred") %>% 
            dplyr::select(Morphospecies, Sampling, alltrees, quadrats, Treatment, nestabund, Abundance) %>% 
            group_by(Morphospecies, Sampling, alltrees, quadrats, Treatment, nestabund) %>% 
            summarise_all(funs(sum)) %>% 
            spread(key = Morphospecies, Abundance, fill =0))
predDO_adonis <- 
  adonis(predDO[,6:184]~
           nestabund*Sampling, 
         permutations = 999,
         strata = predDO$alltrees,
         method ="chao",
         data = predDO)


##Species ERB
predERB <- 
  na.omit(puramona %>% 
            ungroup() %>%  
            filter(Site =="ER" & Sampling =="B") %>% 
            filter(Diet == "pred") %>% 
            dplyr::select(Morphospecies, alltrees, quadrats, Treatment, nestabund, Abundance) %>% 
            group_by(Morphospecies, alltrees, quadrats, Treatment, nestabund) %>% 
            summarise_all(funs(sum)) %>% 
            spread(key = Morphospecies, Abundance, fill =0))
predERB_adonis <- 
  adonis(predERB[,5:101]~
           nestabund, 
         permutations = 999,
         strata = predERB$alltrees,
         method ="chao",
         data = predERB)
##Species ERA
predERA <- 
  na.omit(puramona %>% 
            ungroup() %>%  
            filter(Site =="ER" & Sampling =="A") %>% 
            filter(Diet == "pred") %>% 
            dplyr::select(Morphospecies, alltrees, quadrats, Treatment, nestabund, Abundance) %>% 
            group_by(Morphospecies, alltrees, quadrats, Treatment, nestabund) %>% 
            summarise_all(funs(sum)) %>% 
            spread(key = Morphospecies, Abundance, fill =0))
predERA_adonis <- 
  adonis(predERA[,5:153]~
           nestabund, 
         permutations = 999,
         strata = predERA$alltrees,
         method ="chao",
         data = predERA)
##Species ER
predER <- 
  na.omit(puramona %>% 
            ungroup() %>%  
            filter(Site =="ER") %>% 
            filter(Diet == "pred") %>% 
            dplyr::select(Morphospecies, Sampling, alltrees, quadrats, Treatment, nestabund, Abundance) %>% 
            group_by(Morphospecies, Sampling, alltrees, quadrats, Treatment, nestabund) %>% 
            summarise_all(funs(sum)) %>% 
            spread(key = Morphospecies, Abundance, fill =0))
predER_adonis <- 
  adonis(predER[,6:200]~
           nestabund*Sampling, 
         permutations = 999,
         strata = predER$alltrees,
         method ="chao",
         data = predER)



# Data glmer --------------------------------------------------------------------
predherb <- 
  puramona %>% 
  ungroup() %>% 
  filter(Diet == c("pred", "herb")) %>% 
  dplyr::select(Site, alltrees, quadrats, Treatment, Diet, Sampling, Abundance) %>% 
  group_by(Site, alltrees,quadrats, Treatment,Diet, Sampling) %>%
  summarise_all(funs(sum)) %>% 
  spread(key=Diet, Abundance, fill =0) 
herbcalc <- 
  predherb %>%
  ungroup() %>% 
  dplyr::select(alltrees, Sampling, herb) %>% 
  group_by(alltrees, Sampling) %>% 
  summarise_all(funs(sum)) %>% 
  rename(herbpertree=herb)
predherb <- 
  predherb %>% 
  left_join(herbcalc)
predbcalc <- 
  predherb %>%
  ungroup() %>% 
  dplyr::select(alltrees, Sampling, pred) %>% 
  group_by(alltrees, Sampling) %>% 
  summarise_all(funs(sum)) %>% 
  rename(predpertree=pred)
predherb <- 
  predherb %>% 
  left_join(predbcalc)
predherb <- 
  poolmodel %>% 
  left_join(predherb)
predherb[is.na(predherb)] <- 
  0
##Remove tree that was not sampled
predherb <- 
  predherb %>% 
  filter(alltrees != "CP_A2")
  
# glmer largeleaf on predator abundance ------------------------------------------------------
#largeleaf
plot(predherb$pred ~ predherb$largeleaf)
brompred_model <- 
  glmer(pred ~ 
          largeleaf*Sampling + (1|Site/alltrees/quadrats),
        family= "poisson"(link ="log"), 
        data =predherb)
plot(brompred_model)
qqnorm(residuals(brompred_model))
qqline(residuals(brompred_model), col ="red")
summary(brompred_model)
Anova(brompred_model)
visreg(brompred_model)
rsquared(brompred_model)





