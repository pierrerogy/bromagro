
# glmer with largeleaf ----------------------------------------------------
bromherb_model <- 
  glmer(herb ~ 
          largeleaf*Sampling + (1|Site/alltrees/quadrats),
        family= "poisson"(link ="log"), 
        data =predherb)
plot(bromherb_model)
qqnorm(residuals(bromherb_model))
qqline(residuals(bromherb_model), col ="red")
summary(bromherb_model)
Anova(bromherb_model)
visreg(bromherb_model)
rsquared(bromherb_model)



# Largeleaf adonis per site on herbs----------------------------------------------------------
##Species CPB
herbCPB <- 
  na.omit(puramona %>% 
            ungroup() %>%  
            filter(Site =="CP" & Sampling =="B") %>% 
            filter(Diet == "herb") %>% 
            dplyr::select(Morphospecies, alltrees, quadrats, Treatment, largeleaf, Abundance) %>% 
            group_by(Morphospecies, alltrees, quadrats, Treatment, largeleaf) %>% 
            summarise_all(funs(sum)) %>% 
            spread(key = Morphospecies, Abundance, fill =0))
herbCPB_adonis <- 
  adonis(herbCPB[,5:91]~
           largeleaf, 
         permutations = 999,
         strata = herbCPB$alltrees,
         method ="chao",
         data = herbCPB)
##Species CPA
herbCPA <- 
  na.omit(puramona %>% 
            ungroup() %>%  
            filter(Site =="CP" & Sampling =="A") %>% 
            filter(Diet == "herb") %>% 
            dplyr::select(Morphospecies, alltrees, quadrats, Treatment, largeleaf, Abundance) %>% 
            group_by(Morphospecies, alltrees, quadrats, Treatment, largeleaf) %>% 
            summarise_all(funs(sum)) %>% 
            spread(key = Morphospecies, Abundance, fill =0))
herbCPA_adonis <- 
  adonis(herbCPA[,5:135]~
           largeleaf, 
         permutations = 999,
         strata = herbCPA$alltrees,
         method ="chao",
         data = herbCPA)
##Species CP
herbCP <- 
  na.omit(puramona %>% 
            ungroup() %>%  
            filter(Site =="CP") %>% 
            filter(Diet == "herb") %>% 
            dplyr::select(Morphospecies, Sampling, alltrees, quadrats, Treatment, largeleaf, Abundance) %>% 
            group_by(Morphospecies, Sampling, alltrees, quadrats, Treatment, largeleaf) %>% 
            summarise_all(funs(sum)) %>% 
            spread(key = Morphospecies, Abundance, fill =0))
herbCP_adonis <- 
  adonis(herbCP[,6:181]~
           largeleaf*Sampling, 
         permutations = 999,
         strata = herbCP$alltrees,
         method ="chao",
         data = herbCP)

##Species DOB
herbDOB <- 
  na.omit(puramona %>% 
            ungroup() %>%  
            filter(Site =="DO" & Sampling =="B") %>% 
            filter(Diet == "herb") %>% 
            dplyr::select(Morphospecies, alltrees, quadrats, Treatment, largeleaf, Abundance) %>% 
            group_by(Morphospecies, alltrees, quadrats, Treatment, largeleaf) %>% 
            summarise_all(funs(sum)) %>% 
            spread(key = Morphospecies, Abundance, fill =0))
herbDOB_adonis <- 
  adonis(herbDOB[,5:71]~
           largeleaf, 
         permutations = 999,
         strata = herbDOB$alltrees,
         method ="chao",
         data = herbDOB)
##Species DOA
herbDOA <- 
  na.omit(puramona %>% 
            ungroup() %>%  
            filter(Site =="DO" & Sampling =="A") %>%
            filter(Diet == "herb") %>% 
            dplyr::select(Morphospecies, alltrees, quadrats, Treatment, largeleaf, Abundance) %>% 
            group_by(Morphospecies, alltrees, quadrats, Treatment, largeleaf) %>% 
            summarise_all(funs(sum)) %>% 
            spread(key = Morphospecies, Abundance, fill =0))
herbDOA_adonis <- 
  adonis(herbDOA[,5:64]~
           largeleaf, 
         permutations = 999,
         strata = herbDOA$alltrees,
         method ="chao",
         data = herbDOA)
##Species DO
herbDO <- 
  na.omit(puramona %>% 
            ungroup() %>%  
            filter(Site =="DO") %>% 
            filter(Diet == "herb") %>% 
            dplyr::select(Morphospecies, Sampling, alltrees, quadrats, Treatment, largeleaf, Abundance) %>% 
            group_by(Morphospecies, Sampling, alltrees, quadrats, Treatment, largeleaf) %>% 
            summarise_all(funs(sum)) %>% 
            spread(key = Morphospecies, Abundance, fill =0))
herbDO_adonis <- 
  adonis(herbDO[,6:111]~
           largeleaf*Sampling, 
         permutations = 999,
         strata = herbDO$alltrees,
         method ="chao",
         data = herbDO)


##Species ERB
herbERB <- 
  na.omit(puramona %>% 
            ungroup() %>%  
            filter(Site =="ER" & Sampling =="B") %>% 
            filter(Diet == "herb") %>% 
            dplyr::select(Morphospecies, alltrees, quadrats, Treatment, largeleaf, Abundance) %>% 
            group_by(Morphospecies, alltrees, quadrats, Treatment, largeleaf) %>% 
            summarise_all(funs(sum)) %>% 
            spread(key = Morphospecies, Abundance, fill =0))
herbERB_adonis <- 
  adonis(herbERB[,5:68]~
           largeleaf, 
         permutations = 999,
         strata = herbERB$alltrees,
         method ="chao",
         data = herbERB)
##Species ERA
herbERA <- 
  na.omit(puramona %>% 
            ungroup() %>%  
            filter(Site =="ER" & Sampling =="A") %>% 
            filter(Diet == "herb") %>% 
            dplyr::select(Morphospecies, alltrees, quadrats, Treatment, largeleaf, Abundance) %>% 
            group_by(Morphospecies, alltrees, quadrats, Treatment, largeleaf) %>% 
            summarise_all(funs(sum)) %>% 
            spread(key = Morphospecies, Abundance, fill =0))
herbERA_adonis <- 
  adonis(herbERA[,5:68]~
           largeleaf, 
         permutations = 999,
         strata = herbERA$alltrees,
         method ="chao",
         data = herbERA)
##Species ER
herbER <- 
  na.omit(puramona %>% 
            ungroup() %>%  
            filter(Site =="ER") %>% 
            filter(Diet == "herb") %>% 
            dplyr::select(Morphospecies, Sampling, alltrees, quadrats, Treatment, largeleaf, Abundance) %>% 
            group_by(Morphospecies, Sampling, alltrees, quadrats, Treatment, largeleaf) %>% 
            summarise_all(funs(sum)) %>% 
            spread(key = Morphospecies, Abundance, fill =0))
herbER_adonis <- 
  adonis(herbER[,6:118]~
           largeleaf*Sampling, 
         permutations = 999,
         strata = herbER$alltrees,
         method ="chao",
         data = herbER)



# nestindex adonis per site on herbs----------------------------------------------------------
##Species CPB
herbCPB <- 
  na.omit(puramona %>% 
            ungroup() %>%  
            filter(Site =="CP" & Sampling =="B") %>% 
            filter(Diet == "herb") %>% 
            dplyr::select(Morphospecies, alltrees, quadrats, Treatment, nestindex, Abundance) %>% 
            group_by(Morphospecies, alltrees, quadrats, Treatment, nestindex) %>% 
            summarise_all(funs(sum)) %>% 
            spread(key = Morphospecies, Abundance, fill =0))
herbCPB_adonis <- 
  adonis(herbCPB[,5:91]~
           nestindex, 
         permutations = 999,
         strata = herbCPB$alltrees,
         method ="chao",
         data = herbCPB)
##Species CPA
herbCPA <- 
  na.omit(puramona %>% 
            ungroup() %>%  
            filter(Site =="CP" & Sampling =="A") %>% 
            filter(Diet == "herb") %>% 
            dplyr::select(Morphospecies, alltrees, quadrats, Treatment, nestindex, Abundance) %>% 
            group_by(Morphospecies, alltrees, quadrats, Treatment, nestindex) %>% 
            summarise_all(funs(sum)) %>% 
            spread(key = Morphospecies, Abundance, fill =0))
herbCPA_adonis <- 
  adonis(herbCPA[,5:135]~
           nestindex, 
         permutations = 999,
         strata = herbCPA$alltrees,
         method ="chao",
         data = herbCPA)
##Species CP
herbCP <- 
  na.omit(puramona %>% 
            ungroup() %>%  
            filter(Site =="CP") %>% 
            filter(Diet == "herb") %>% 
            dplyr::select(Morphospecies, Sampling, alltrees, quadrats, Treatment, nestindex, Abundance) %>% 
            group_by(Morphospecies, Sampling, alltrees, quadrats, Treatment, nestindex) %>% 
            summarise_all(funs(sum)) %>% 
            spread(key = Morphospecies, Abundance, fill =0))
herbCP_adonis <- 
  adonis(herbCP[,6:181]~
           nestindex*Sampling, 
         permutations = 999,
         strata = herbCP$alltrees,
         method ="chao",
         data = herbCP)

##Species DOB
herbDOB <- 
  na.omit(puramona %>% 
            ungroup() %>%  
            filter(Site =="DO" & Sampling =="B") %>% 
            filter(Diet == "herb") %>% 
            dplyr::select(Morphospecies, alltrees, quadrats, Treatment, nestindex, Abundance) %>% 
            group_by(Morphospecies, alltrees, quadrats, Treatment, nestindex) %>% 
            summarise_all(funs(sum)) %>% 
            spread(key = Morphospecies, Abundance, fill =0))
herbDOB_adonis <- 
  adonis(herbDOB[,5:71]~
           nestindex, 
         permutations = 999,
         strata = herbDOB$alltrees,
         method ="chao",
         data = herbDOB)
##Species DOA
herbDOA <- 
  na.omit(puramona %>% 
            ungroup() %>%  
            filter(Site =="DO" & Sampling =="A") %>%
            filter(Diet == "herb") %>% 
            dplyr::select(Morphospecies, alltrees, quadrats, Treatment, nestindex, Abundance) %>% 
            group_by(Morphospecies, alltrees, quadrats, Treatment, nestindex) %>% 
            summarise_all(funs(sum)) %>% 
            spread(key = Morphospecies, Abundance, fill =0))
herbDOA_adonis <- 
  adonis(herbDOA[,5:64]~
           nestindex, 
         permutations = 999,
         strata = herbDOA$alltrees,
         method ="chao",
         data = herbDOA)
##Species DO
herbDO <- 
  na.omit(puramona %>% 
            ungroup() %>%  
            filter(Site =="DO") %>% 
            filter(Diet == "herb") %>% 
            dplyr::select(Morphospecies, Sampling, alltrees, quadrats, Treatment, nestindex, Abundance) %>% 
            group_by(Morphospecies, Sampling, alltrees, quadrats, Treatment, nestindex) %>% 
            summarise_all(funs(sum)) %>% 
            spread(key = Morphospecies, Abundance, fill =0))
herbDO_adonis <- 
  adonis(herbDO[,6:111]~
           nestindex*Sampling, 
         permutations = 999,
         strata = herbDO$alltrees,
         method ="chao",
         data = herbDO)


##Species ERB
herbERB <- 
  na.omit(puramona %>% 
            ungroup() %>%  
            filter(Site =="ER" & Sampling =="B") %>% 
            filter(Diet == "herb") %>% 
            dplyr::select(Morphospecies, alltrees, quadrats, Treatment, nestindex, Abundance) %>% 
            group_by(Morphospecies, alltrees, quadrats, Treatment, nestindex) %>% 
            summarise_all(funs(sum)) %>% 
            spread(key = Morphospecies, Abundance, fill =0))
herbERB_adonis <- 
  adonis(herbERB[,5:68]~
           nestindex, 
         permutations = 999,
         strata = herbERB$alltrees,
         method ="chao",
         data = herbERB)
##Species ERA
herbERA <- 
  na.omit(puramona %>% 
            ungroup() %>%  
            filter(Site =="ER" & Sampling =="A") %>% 
            filter(Diet == "herb") %>% 
            dplyr::select(Morphospecies, alltrees, quadrats, Treatment, nestindex, Abundance) %>% 
            group_by(Morphospecies, alltrees, quadrats, Treatment, nestindex) %>% 
            summarise_all(funs(sum)) %>% 
            spread(key = Morphospecies, Abundance, fill =0))
herbERA_adonis <- 
  adonis(herbERA[,5:68]~
           nestindex, 
         permutations = 999,
         strata = herbERA$alltrees,
         method ="chao",
         data = herbERA)
##Species ER
herbER <- 
  na.omit(puramona %>% 
            ungroup() %>%  
            filter(Site =="ER") %>% 
            filter(Diet == "herb") %>% 
            dplyr::select(Morphospecies, Sampling, alltrees, quadrats, Treatment, nestindex, Abundance) %>% 
            group_by(Morphospecies, Sampling, alltrees, quadrats, Treatment, nestindex) %>% 
            summarise_all(funs(sum)) %>% 
            spread(key = Morphospecies, Abundance, fill =0))
herbER_adonis <- 
  adonis(herbER[,6:118]~
           nestindex*Sampling, 
         permutations = 999,
         strata = herbER$alltrees,
         method ="chao",
         data = herbER)



