
# Models on herbivores/predators  ------------------------------------------
#CP
##Predators
obsmodelcp_pred <- 
  glmer.nb(number ~ 
          Bromeliads+ 
            (1|block/alltrees),
        control =glmerControl(optimizer = "bobyqa"),
        data =obspred %>% filter(Site == "CP"))
simulationOutput <- 
  simulateResiduals(fittedModel =obsmodelcp_pred, 
                    n = 2000)
plot(simulationOutput,
     asFactor =T,
     quantreg = F)
summary(obsmodelcp_pred)
obstestcp_pred <- 
  mixed(number ~ 
          Bromeliads +
          (1|block/alltrees),
        data =obspred %>% filter(Site == "CP"),
        family = "negative.binomial"(theta = getME(obsmodelcp_pred,
                                                   "glmer.nb.theta")),
        control =glmerControl(optimizer = "bobyqa"),
        method = "LRT")$anova_table
visreg(obsmodelcp_pred, 
       "Bromeliads")
##Bromeliad-associated predators
obsmodelcp_brompred <-
  glmer(brompred ~ 
          Bromeliads +
          (1|block/alltrees),
        family = "poisson"(link ="log"),
        data =obsdietcp)
simulationOutput <- 
  simulateResiduals(fittedModel =obsmodelcp_brompred, 
                    n = 2000,
                    rank = T)
plot(simulationOutput,
     asFactor =T,
     quantreg = F)
summary(obsmodelcp_brompred)
obstestcp_brompred <- 
  mixed(brompred ~ 
          Bromeliads +
          (1|block/alltrees),
        data =obsdietcp,
        family = "poisson"(link ="log"),
        method = "LRT")$anova_table
visreg(obsmodelcp_brompred, 
       "Bromeliads")
##Herbivores
obsmodelcp_herb <- 
  glmer(herb ~ 
          Bromeliads +
          (1|block/alltrees),
          family ="poisson"(link ="inverse"),
        data =obsdietcp %>%
          filter(herb <200))
simulationOutput <- 
  simulateResiduals(fittedModel =obsmodelcp_herb, 
                    n = 2000)
plot(simulationOutput,
     asFactor =T,
     quantreg = F)
summary(obsmodelcp_herb)
obstestcp_herb <- 
  mixed(herb ~ 
          Bromeliads +
          (1|block/alltrees),
        family = "poisson"(link ="inverse"),
        data =obsdietcp %>% 
          filter(herb <200),
        method = "LRT")$anova_table
visreg(obsmodelcp_herb, 
       "Bromeliads")

#DO
##Predators
obsmodeldo_pred <- 
  glm.nb(number ~ 
        Bromeliads,
        data =obspred %>% filter(Site =="DO"))
simulationOutput <- 
  simulateResiduals(fittedModel =obsmodeldo_pred, 
                    n = 2000)
plot(simulationOutput,
     asFactor =T,
     quantreg = F)
summary(obsmodeldo_pred)
Anova(obsmodeldo_pred)
visreg(obsmodeldo_pred, 
       "Bromeliads")
##Bromeliad-associated predators
obsmodeldo_brompred <-
  glm.nb(brompred ~ 
             Bromeliads, 
           data = obsdietdo)
simulationOutput <- 
  simulateResiduals(fittedModel =obsmodeldo_brompred, 
                    n = 2000)
plot(simulationOutput,
     asFactor =T,
     quantreg = F)
summary(obsmodeldo_brompred)
Anova(obsmodeldo_brompred)
visreg(obsmodeldo_brompred, 
       "Bromeliads")
##Herbivores
obsmodeldo_herb <- 
  glm.nb(herb ~ 
        Bromeliads,
        data =obsdietdo)
simulationOutput <- 
  simulateResiduals(fittedModel =obsmodeldo_herb, 
                    n = 2000)
plot(simulationOutput,
     asFactor =T,
     quantreg = F)
summary(obsmodeldo_herb)
Anova(obsmodeldo_herb)
visreg(obsmodeldo_herb, 
       "Bromeliads")

# Models on leaf chewers and phloem feeders ------------------------------------------
#CP
##Leaf chewers
###Model
obsmodelcp_chewer <- 
  glmer(chewer ~ 
          Bromeliads +
          (1|block/alltrees),
        family ="poisson"(link ="sqrt"),
        data =obsdietcp_rev)
###Assumptions
simulationOutput <- 
  simulateResiduals(fittedModel =obsmodelcp_chewer, 
                    n = 2000)
plot(simulationOutput,
     asFactor =T,
     quantreg = F)
###Summary and tests
summary(obsmodelcp_chewer)
obstestcp_chewer <- 
  mixed(chewer~ 
          Bromeliads +
          (1|block/alltrees),
        family = "poisson"(link ="sqrt"),
        data =obsdietcp_rev,
        method = "LRT")$anova_table
###Plot
visreg(obsmodelcp_chewer, 
       "Bromeliads")

##Phloem feeders
###Model
obsmodelcp_phloem <- 
  glmer.nb(phloem ~ 
          Bromeliads +
          (1|block/alltrees),
        data =obsdietcp_rev%>%
          filter(phloem <200))
###Assumptions
simulationOutput <- 
  simulateResiduals(fittedModel =obsmodelcp_phloem, 
                    n = 2000)
plot(simulationOutput,
     asFactor =T,
     quantreg = F)
###Summary and tests
summary(obsmodelcp_phloem)
obstestcp_phloem <- 
  mixed(phloem~ 
          Bromeliads +
          (1|block/alltrees),
        family = "negative.binomial"(theta = getME(obsmodelcp_phloem,
                                                   "glmer.nb.theta")),
        data =obsdietcp_rev,
        method = "LRT")$anova_table
###Plot
visreg(obsmodelcp_phloem, 
       "Bromeliads")


#DO
##Leaf chewers
###Model
obsmodeldo_chewer <- 
  glm.nb(chewer ~ 
           Bromeliads,
        data =obsdietdo_rev)
###Assumptions
simulationOutput <- 
  simulateResiduals(fittedModel =obsmodeldo_chewer, 
                    n = 2000)
plot(simulationOutput,
     asFactor =T,
     quantreg = F)
###Summary and tests
summary(obsmodeldo_chewer)
Anova(obsmodeldo_chewer)
###Plot
visreg(obsmodeldo_chewer, 
       "Bromeliads")

##Phloem feeders
###Model
obsmodeldo_phloem <- 
  glm.nb(phloem ~ 
           Bromeliads,
         data =obsdietdo_rev)
###Assumptions
simulationOutput <- 
  simulateResiduals(fittedModel =obsmodeldo_phloem, 
                    n = 2000)
plot(simulationOutput,
     asFactor =T,
     quantreg = F)
###Summary and tests
summary(obsmodeldo_phloem)
Anova(obsmodeldo_phloem)
###Plot
visreg(obsmodeldo_phloem, 
       "Bromeliads")

# Models interactions -----------------------------------------------------
#CP
##Negative
intermodel_cpneg <-
  glmer(neg ~ 
          Bromeliads*todo+ 
          (1|block/alltrees),
        family = "poisson"(link = "log"),
        control = glmerControl(optimizer = "bobyqa"),
        data = spread_interactioncp)
simulationOutput <- 
  simulateResiduals(fittedModel =intermodel_cpneg, 
                    n = 2000,
                    rank = T)
plot(simulationOutput, 
     asFactor =F,
     quantreg = F,
     rank = T)
summary(intermodel_cpneg)
intertest_cpneg <- 
  mixed(neg ~ 
          Bromeliads*todo+ 
          (1|block/alltrees),
        family = "poisson"(link = "log"),
        control = glmerControl(optimizer = "bobyqa"),
        data = spread_interactioncp,
        method = "LRT")$anova_table
visreg(intermodel_cpneg, 
       "todo", 
       by="Bromeliads")
##positive
intermodel_cppos <-
  glmer.nb(pos ~ 
          Bromeliads*todo+ 
          (1|block/alltrees),
        family = "poisson"(link = "log"),
        data = spread_interactioncp)
simulationOutput <- 
  simulateResiduals(fittedModel =intermodel_cppos, 
                    n = 2000,
                    rank = T)
plot(simulationOutput, 
     asFactor =F,
     quantreg = F,
     rank = T)
summary(intermodel_cppos)
intertest_cppos <- 
  mixed(pos ~ 
          Bromeliads*todo+ 
          (1|block/alltrees),
        family = "negative.binomial"(theta = getME(intermodel_cppos,
                                                   "glmer.nb.theta")),
        data = spread_interactioncp,
        method = "LRT")$anova_table
visreg(intermodel_cppos, 
       "todo", 
       by="Bromeliads")
#DO
##Negative
intermodel_doneg <-
  glm.nb(neg ~ 
          Bromeliads*todo,
        data = spread_interactiondo)
simulationOutput <- 
  simulateResiduals(fittedModel =intermodel_doneg, 
                    n = 2000,
                    rank = T)
plotSimulatedResiduals(simulationOutput = simulationOutput, 
                       asFactor =F,
                       quantreg = F,
                       rank = T)
testZeroInflation(simulationOutput)
summary(intermodel_doneg)
Anova(intermodel_doneg)
visreg(intermodel_doneg, 
       "todo", 
       by="Bromeliads")
##positive
intermodel_dopos <-
  glm.nb(sqrt(pos) ~ 
          Bromeliads*todo,
        data = spread_interactiondo)
simulationOutput <- 
  simulateResiduals(fittedModel =intermodel_dopos, 
                    n = 2000,
                    rank = T)
plotSimulatedResiduals(simulationOutput = simulationOutput, 
                       asFactor =F,
                       quantreg = F,
                       rank = T)
summary(intermodel_dopos)
Anova(intermodel_dopos)
visreg(intermodel_dopos, 
       "todo", 
       by="Bromeliads")
# Adonis diet/kinds CP---------------------------------------------------------
#Diet with all herbivores together
##Bromeliads
obsadoniscp_bromdiet <- 
  adonis(obsdietcp[,5:14] ~
           Bromeliads,
         method = "bray",
         permutations = 2000,
         strata = obsdietcp$block,
         data = obsdietcp)
##Bromeliad predators
obsadoniscp_brompreddiet <- 
  adonis(obsdietcp_nobrompreds[,5:14] ~
           brompred,
         method = "bray",
         permutations = 2000,
         strata = obsdietcp_nobrompreds$block,
         data = obsdietcp_nobrompreds)

#Diet with herbivores split between functional groups
##Bromeliads
obsadoniscp_bromdiet_rev <- 
  adonis(obsdietcp_rev[,5:15] ~
           Bromeliads,
         method = "bray",
         permutations = 2000,
         strata = obsdietcp_rev$block,
         data = obsdietcp_rev)
##Bromeliad-associated predators
obsadoniscp_brompreddiet_rev <- 
  adonis(obsdietcp_nobrompreds[,5:15] ~
           brompred,
         method = "bray",
         permutations = 2000,
         strata = obsdietcp_nobrompreds_rev$block,
         data = obsdietcp_nobrompreds_rev)


#Kinds
##Bromeliads
obsadoniscp_bromkind <- 
  adonis(obskindcp[,5:31] ~
           Bromeliads,
         method = "bray",
         permutations = 2000,
         strata = obskindcp$block,
         data = obskindcp)
##Bromeliad predators
obsadoniscp_brompredkind <- 
  adonis(obskindcp_nobrompreds[,5:31] ~
           brompred,
         method = "bray",
         permutations = 2000,
         strata = obskindcp_nobrompreds$block,
         data = obskindcp_nobrompreds)

#Restricted kinds
##Bromeliads
obsadoniscp_bromkind_sub <- 
  adonis(obskindcp_sub[,5:20] ~
           Bromeliads,
         method = "bray",
         permutations = 2000,
         strata = obskindcp_sub$block,
         data = obskindcp_sub)
##Bromeliad predators
obsadoniscp_brompredkind_sub <- 
  adonis(obskindcp_sub_nobrompreds[,5:20] ~
           brompred,
         method = "bray",
         permutations = 2000,
         strata = obskindcp_sub_nobrompreds$block,
         data = obskindcp_sub_nobrompreds)

# Adonis diet/kinds DO ---------------------------------------------------------
#Diet with all herbivores together
##Bromeliads
obsadonisdo_bromdiet <- 
  adonis(obsdietdo[,4:9] ~
           Bromeliads,
         method = "bray",
         permutations = 2000,
         data = obsdietdo)
##Bromeliad predators
obsadonisdo_brompreddiet <- 
  adonis(obsdietdo_nobrompreds[,4:9] ~
           brompred,
         method = "bray",
         permutations = 2000,
         data = obsdietdo_nobrompreds)

#Diet with herbivores split between functional groups
##Bromeliads
obsadonisdo_bromdiet_rev <- 
  adonis(obsdietdo_rev[,4:11] ~
           Bromeliads,
         method = "bray",
         permutations = 2000,
         data = obsdietdo_rev)
##Bromeliad predators
obsadonisdo_brompreddiet_rev <- 
  adonis(obsdietdo_nobrompreds_rev[,4:11] ~
           brompred,
         method = "bray",
         permutations = 2000,
         data = obsdietdo_nobrompreds_rev)




#Kinds
##Bromeliads
obsadonisdo_bromkind <- 
  adonis(obskinddo[,4:26] ~
           Bromeliads,
         method = "bray",
         permutations = 2000,
         data = obskinddo)
##Bromeliad predators
obsadonisdo_brompredkind <- 
  adonis(obskinddo_nobrompreds[,4:26] ~
           brompred,
         method = "bray",
         permutations = 2000,
         data = obskinddo_nobrompreds)

#Restricted kinds
##Bromeliads
obsadonisdo_bromkind_sub <- 
  adonis(obskinddo_sub[,4:18] ~
           Bromeliads,
         method = "bray",
         permutations = 2000,
         data = obskinddo_sub)
##Bromeliad predators
obsadonisdo_brompredkind_sub <- 
  adonis(obskinddo_sub_nobrompreds[,4:18] ~
           brompred,
         method = "bray",
         permutations = 2000,
         data = obskinddo_sub_nobrompreds)


# Adonis behaviour with all herbivores together--------------------------------------------------------
#CP
##Duration
###Bromeliads
obsadonis_behavdurcp_brom <- 
  adonis(spread_behavdurcp[,5:11] ~
           Bromeliads,
         method = "euclidean",
         permutations = 2000,
         strata = spread_behavdurcp$block,
         data = spread_behavdurcp)
###Bromeliad-associated predators
obsadonis_behavdurcp_brompred <- 
  adonis(spread_behavdurcp_nobrompreds[,5:11] ~
          brompred,
         method = "euclidean",
         permutations = 2000,
         strata = spread_behavdurcp_nobrompreds$block,
         data = spread_behavdurcp_nobrompreds)
##Frequency
###Bromeliads
obsadonis_behavfreqcp_brom <- 
  adonis(spread_behavfreqcp[,5:11] ~
           Bromeliads,
         method = "bray",
         permutations = 2000,
         strata = spread_behavfreqcp$block,
         data = spread_behavfreqcp)
###Bromeliad-associated predators
obsadonis_behavfreqcp_brompred <- 
  adonis(spread_behavfreqcp_nobrompreds[,5:11] ~
           brompred,
         method = "bray",
         permutations = 2000,
         strata = spread_behavfreqcp_nobrompreds$block,
         data = spread_behavfreqcp_nobrompreds)

#DO
##Duration
###Bromeliads
obsadonis_behavdurdo_brom <- 
  adonis(spread_behavdurdo[,5:14] ~
           Bromeliads,
         method = "euclidean",
         permutations = 2000,
         strata = spread_behavdurdo$block,
         data = spread_behavdurdo)
###Bromeliad-associated predators
obsadonis_behavdurdo_brompred <- 
  adonis(spread_behavdurdo_nobrompreds[,5:14] ~
           brompred,
         method = "euclidean",
         permutations = 2000,
         strata = spread_behavdurdo_nobrompreds$block,
         data = spread_behavdurdo_nobrompreds)
##Frequency
##Bromeliads
obsadonis_behavfreqdo_brom <- 
  adonis(spread_behavfreqdo[,5:14] ~
           Bromeliads,
         method = "bray",
         permutations = 2000,
         strata = spread_behavfreqdo$block,
         data = spread_behavfreqdo)
###Bromeliad-associated predators
obsadonis_behavfreqdo_brompred <- 
  adonis(spread_behavfreqdo_nobrompreds[,5:14] ~
           brompred,
         method = "bray",
         permutations = 2000,
         strata = spread_behavfreqdo_nobrompreds$block,
         data = spread_behavfreqdo_nobrompreds)
# Adonis behaviour with leaf chewers and phloem feeders--------------------------------------------------------
#CP
##Duration
###Bromeliads
obsadonis_behavdurcp_brom_rev <- 
  adonis(spread_behavdurcp_rev[,5:14] ~
           Bromeliads,
         method = "euclidean",
         permutations = 2000,
         strata = spread_behavdurcp_rev$block,
         data = spread_behavdurcp_rev)
###Bromeliad-associated predators
obsadonis_behavdurcp_brompred_rev <- 
  adonis(spread_behavdurcp_nobrompreds_rev[,5:14] ~
           brompred,
         method = "euclidean",
         permutations = 2000,
         strata = spread_behavdurcp_nobrompreds_rev$block,
         data = spread_behavdurcp_nobrompreds_rev)
##Frequency
###Bromeliads
obsadonis_behavfreqcp_brom_rev<- 
  adonis(spread_behavfreqcp_rev[,5:14] ~
           Bromeliads,
         method = "bray",
         permutations = 2000,
         strata = spread_behavfreqcp$block,
         data = spread_behavfreqcp)
###Bromeliad-associated predators
obsadonis_behavfreqcp_brompred_rev <- 
  adonis(spread_behavfreqcp_nobrompreds_rev[,5:14] ~
           brompred,
         method = "bray",
         permutations = 2000,
         strata = spread_behavfreqcp_nobrompreds_rev$block,
         data = spread_behavfreqcp_nobrompreds_rev)

#DO
##Duration
###Bromeliads
obsadonis_behavdurdo_brom_rev <- 
  adonis(spread_behavdurdo_rev[,5:17] ~
           Bromeliads,
         method = "euclidean",
         permutations = 2000,
         strata = spread_behavdurdo_rev$block,
         data = spread_behavdurdo_rev)
###Bromeliad-associated predators
obsadonis_behavdurdo_brompred_rev <- 
  adonis(spread_behavdurdo_nobrompreds_rev[,5:17] ~
           brompred,
         method = "euclidean",
         permutations = 2000,
         strata = spread_behavdurdo_nobrompreds_rev$block,
         data = spread_behavdurdo_nobrompreds_rev)
##Frequency
##Bromeliads
obsadonis_behavfreqdo_brom_rev <- 
  adonis(spread_behavfreqdo_rev[,5:17] ~
           Bromeliads,
         method = "bray",
         permutations = 2000,
         strata = spread_behavfreqdo_rev$block,
         data = spread_behavfreqdo_rev)
###Bromeliad-associated predators
obsadonis_behavfreqdo_brompred_rev <- 
  adonis(spread_behavfreqdo_nobrompreds_rev[,5:17] ~
           brompred,
         method = "bray",
         permutations = 2000,
         strata = spread_behavfreqdo_nobrompreds_rev$block,
         data = spread_behavfreqdo_nobrompreds_rev)

# Night and Day models -----------------------------------------------------------
#Predators
noctmodel_pred <- 
  glmer.nb(number ~ 
          Bromeliads*Time+ 
          (1|block/alltrees),
          control = glmerControl(optimizer = "bobyqa"),
          data =noctobspred)
simulationOutput <- 
  simulateResiduals(fittedModel =noctmodel_pred, 
                    n = 2000)
plot(simulationOutput, 
     asFactor =T,
     quantreg = F)
summary(noctmodel_pred)
noctest_pred <- 
  mixed(number ~ 
          Bromeliads*Time +
          (1|block/alltrees),
        data =noctobspred,
        family = "negative.binomial"(theta = getME(noctmodel_pred,
                                     "glmer.nb.theta")),
        control = glmerControl(optimizer = "bobyqa"),
        method = "LRT")$anova_table
visreg(noctmodel_pred,
       "Bromeliads",
       by = "Time")

#Bromeliad  predators
noctmodel_brompred <- 
  glmer(brompred ~ 
          Bromeliads*Time+ 
          (1|block/alltrees),
        family = "poisson"(link ="log"),
        data =noctdiet)
simulationOutput <- 
  simulateResiduals(fittedModel =noctmodel_brompred, 
                    n = 2000)
plot(simulationOutput, 
     asFactor =T,
     quantreg = F)
summary(noctmodel_brompred)
noctest_brompred <- 
  mixed(brompred ~ 
          Bromeliads*Time +
          (1|block/alltrees),
        data =noctdiet,
        family = "poisson"(link ="log"),
        method = "LRT")$anova_table
visreg(noctmodel_brompred, 
       "Bromeliads",
       by = "Time")

#Herbivores
noctmodel_herb <- 
  glmer.nb(herb ~ 
             Bromeliads*Time+ 
             (1|block/alltrees),
           control = glmerControl(optimizer = "bobyqa"),
           data =noctdiet %>% 
             filter(herb < 200))
simulationOutput <- 
  simulateResiduals(fittedModel =noctmodel_herb, 
                    n = 2000)
plot(simulationOutput, 
     asFactor =T,
     quantreg = F)
summary(noctmodel_herb)
noctest_herb <- 
  mixed(herb~ 
          Bromeliads*Time +
          (1|block/alltrees),
        data =noctdiet%>% 
          filter(herb < 200),
        family = "negative.binomial"(theta = getME(noctmodel_herb,
                                                   "glmer.nb.theta")),
        control = glmerControl(optimizer = "bobyqa"),
        method = "LRT")$anova_table
visreg(noctmodel_herb, 
       "Time",
       by = "Bromeliads")

#Leaf chewers
##Model
noctmodel_chewer <- 
  glmer.nb(chewer ~ 
             Bromeliads*Time+ 
             (1|block/alltrees),
           data =noctdiet_rev)
##Assumptions
simulationOutput <- 
  simulateResiduals(fittedModel =noctmodel_chewer, 
                    n = 2000)
plot(simulationOutput, 
     asFactor =T,
     quantreg = F)
##Summary and tests
summary(noctmodel_chewer)
noctest_chewer <- 
  mixed(chewer~ 
          Bromeliads*Time +
          (1|block/alltrees),
        data =noctdiet_rev,
        family = "negative.binomial"(theta = getME(noctmodel_chewer,
                                                   "glmer.nb.theta")),
        method = "LRT")$anova_table
##Plot
visreg(noctmodel_chewer, 
       "Time", by = "Bromeliads")

#Phloem feeders
##Model
noctmodel_phloem <- 
  glmer.nb(phloem ~ 
             Bromeliads*Time+ 
             (1|block/alltrees),
           data =noctdiet_rev %>% 
             filter(phloem<200))
##Assumptions
simulationOutput <- 
  simulateResiduals(fittedModel =noctmodel_phloem, 
                    n = 2000)
plot(simulationOutput, 
     asFactor =T,
     quantreg = F)
##Summary and tests
summary(noctmodel_phloem)
noctest_phloem <- 
  mixed(phloem~ 
          Bromeliads*Time +
          (1|block/alltrees),
        data =noctdiet_rev %>% 
          filter(phloem <200),
        family = "negative.binomial"(theta = getME(noctmodel_phloem,
                                                   "glmer.nb.theta")),
        method = "LRT")$anova_table
##Plot
visreg(noctmodel_phloem, 
       "Time", by = "Bromeliads")

# Night and Day adonises with all herbivores --------------------------------------------------
#Diet
##Bromeliads
noctadonis_bromdiet <- 
  adonis(noctdiet[,6:15] ~
           Bromeliads*Time,
         method = "bray",
         strata = noctdiet$block,
         permutations = 2000,
         data = noctdiet)
##Bromeliad-associated predators
noctadonis_brompreddiet <- 
  adonis(noctdiet_nobrompreds[,6:15] ~
           brompred*Time,
         method = "bray",
         strata = noctdiet_nobrompreds$block,
         permutations = 2000,
         data = noctdiet_nobrompreds)

#Kinds
##Bromeliads
noctadonis_bromkind <- 
  adonis(noctkind[,6:32] ~
           Bromeliads*Time,
         method = "bray",
         strata = noctkind$block,
         permutations = 2000,
         data = noctkind)
##Bromeliad-associated predators
noctadonis_brompredkind <- 
  adonis(noctkind_nobrompreds[,6:33] ~
           brompred*Time,
         method = "bray",
         strata = noctkind_nobrompreds$block,
         permutations = 2000,
         data = noctkind_nobrompreds)


#Restricted kinds
##Bromeliads
noctadonis_bromkind_sub <- 
  adonis(noctkind_sub[,6:21] ~
           Bromeliads*Time,
         method = "bray",
         strata = noctkind_sub$block,
         permutations = 2000,
         data = noctkind_sub)
##Bromeliad-associated predators
noctadonis_brompredkind_sub <- 
  adonis(noctkind_sub_nobrompreds[,6:21] ~
           brompred*Time,
         method = "bray",
         strata = noctkind_sub_nobrompreds$block,
         permutations = 2000,
         data = noctkind_sub_nobrompreds)

#Behaviour duration
##Bromeliads
noctadonis_brombehavdur <- 
  adonis(spread_noctbehavdur[,6:12] ~
           Bromeliads*Time,
         method = "euclidean",
         strata = spread_noctbehavdur$block,
         permutations = 2000,
         data = spread_noctbehavdur)
##Bromeliad-associated predators
noctadonis_brompredbehavdur <- 
  adonis(spread_noctbehavdur_nobrompreds[,6:12] ~
           brompred*Time,
         method = "euclidean",
         strata = spread_noctbehavdur_nobrompreds$block,
         permutations = 2000,
         data = spread_noctbehavdur_nobrompreds)

#Behaviour frequency
##Bromeliads
noctadonis_brombehavfreq <- 
  adonis(spread_noctbehavfreq[,6:12] ~
           Bromeliads*Time,
         method = "bray",
         strata = spread_noctbehavfreq$block,
         permutations = 2000,
         data = spread_noctbehavfreq)
##Bromeliad-associated predators
noctadonis_brompredbehavfreq <- 
  adonis(spread_noctbehavfreq_nobrompreds[,6:12] ~
           brompred*Time,
         method = "bray",
         strata = spread_noctbehavfreq_nobrompreds$block,
         permutations = 2000,
         data = spread_noctbehavfreq_nobrompreds)


# Night and Day adonises with leaf chewers and phloem feeders --------------------------------------------------
#Dietary groups
##Bromeliads
noctadonis_bromdiet_rev <- 
  adonis(noctdiet_rev[,6:16] ~
           Bromeliads*Time,
         method = "bray",
         strata = noctdiet_rev$block,
         permutations = 2000,
         data = noctdiet_rev)
##Bromeliad-associated predators
noctadonis_brompreddiet_rev <- 
  adonis(noctdiet_nobrompreds_rev[,6:16] ~
           brompred*Time,
         method = "bray",
         strata = noctdiet_nobrompreds_rev$block,
         permutations = 2000,
         data = noctdiet_nobrompreds_rev)

#Behaviour duration
##Bromeliads
noctadonis_brombehavdur_rev <- 
  adonis(spread_noctbehavdur_rev[,6:15] ~
           Bromeliads*Time,
         method = "euclidean",
         strata = spread_noctbehavdur_rev$block,
         permutations = 2000,
         data = spread_noctbehavdur_rev)
##Bromeliad-associated predators
noctadonis_brompredbehavdur_rev <- 
  adonis(spread_noctbehavdur_nobrompreds_rev[,6:15] ~
           brompred*Time,
         method = "euclidean",
         strata = spread_noctbehavdur_nobrompreds_rev$block,
         permutations = 2000,
         data = spread_noctbehavdur_nobrompreds_rev)

#Behaviour frequency
##Bromeliads
noctadonis_brombehavfreq_rev <- 
  adonis(spread_noctbehavfreq_rev[,6:15] ~
           Bromeliads*Time,
         method = "bray",
         strata = spread_noctbehavfreq_rev$block,
         permutations = 2000,
         data = spread_noctbehavfreq_rev)
##Bromeliad-associated predators
noctadonis_brompredbehavfreq_rev <- 
  adonis(spread_noctbehavfreq_nobrompreds_rev[,6:15] ~
           brompred*Time,
         method = "bray",
         strata = spread_noctbehavfreq_nobrompreds_rev$block,
         permutations = 2000,
         data = spread_noctbehavfreq_nobrompreds_rev)


# Night and Day interactions -----------------------------------------------
##Negative
noctintermodel_neg <-
  glmer(neg ~ 
          Bromeliads*Time*todo+ 
          (1|block/alltrees),
        family = "poisson"(link = "log"),
        control = glmerControl(optimizer = "bobyqa"),
        data = spread_noctinteraction)
simulationOutput <- 
  simulateResiduals(fittedModel =noctintermodel_neg, 
                    n = 2000,
                    rank = T)
plot(simulationOutput, 
     asFactor =F,
     quantreg = F,
     rank = T)
summary(noctintermodel_neg)
noctintertest_neg <- 
  mixed(neg ~ 
          Bromeliads*Time*todo+ 
          (1|block/alltrees),
        family = "poisson"(link = "log"),
        control = glmerControl(optimizer = "bobyqa"),
        data = spread_noctinteraction,
        method = "LRT")$anova_table
visreg(noctintermodel_neg, 
       "Bromeliads", 
       by="Time")
##positive
noctintermodel_pos <-
  glmer(pos ~ 
          Bromeliads*Time*todo+ 
          (1|block/alltrees),
        family = "poisson"(link = "sqrt"),
        data = spread_noctinteraction)
simulationOutput <- 
  simulateResiduals(fittedModel =noctintermodel_pos, 
                    n = 2000,
                    rank = T)
plot(simulationOutput, 
     asFactor =F,
     quantreg = F,
     rank = T)
summary(noctintermodel_pos)
noctintertest_pos <- 
  mixed(pos ~ 
          Bromeliads*Time*todo+ 
          (1|block/alltrees),
        family = "poisson"(link = "sqrt"),
        data = spread_noctinteraction,
        method = "LRT")$anova_table
visreg(noctintermodel_pos, 
       "Time", 
       by="Bromeliads")

# Model plots -------------------------------------------------------------------
#Overall pred
obseffectcp_pred <- 
  ggeffect(obsmodelcp_pred,
           type = "re",
           x.as.factor = T,
           terms = c("Bromeliads"),
           ci.lvl = 0.95)
obsplotcp_pred <- 
  ggplot(obseffectcp_pred, 
         aes(x=x, 
             y=predicted, 
             colour= group)) + 
  geom_errorbar(aes(ymin=conf.low, 
                    ymax=conf.high), 
                width=0.1,
                lwd = 2,
                position = position_dodge(0.3)) +
  geom_point(position = position_dodge(0.3), 
             lwd =6) +
  ggtitle("") + 
  xlab("Bromeliads") +
  scale_x_discrete(limit = c("absent", "present"),
                   labels = c("Absent", "Present"),
                   expand = expand_scale(add = c(0.6)))+
  ylab("Predator abundance") +
  scale_color_manual(name = "",
                     labels = c(""),
                     values = c("black")) +
  theme(legend.position = "",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))

#Day/Night pred
nocteffect_pred <- 
  ggeffect(noctmodel_pred,
           type = "re",
           x.as.factor = T,
           terms = c("Bromeliads", "Time"),
           ci.lvl = 0.95)
noctplot_pred <- 
  ggplot(nocteffect_pred, 
         aes(x=x, 
             y=predicted, 
             colour=group)) + 
  geom_errorbar(aes(ymin=conf.low, 
                    ymax=conf.high), 
                width=0.1,
                lwd = 2,
                position = position_dodge(0.3)) +
  geom_point(position = position_dodge(0.3), 
             lwd =6) +
  ggtitle("") + 
  xlab("Bromeliads") +
  scale_x_discrete(limit = c("absent", "present"),
                   labels = c("Absent", "Present"),
                   expand = expand_scale(add = c(0.6)))+
  ylab("Predator abundance") +
  scale_color_manual(name = "Time of observation",
                     labels = c("Day", "Night"),
                     values = c("grey", "grey40")) +
  theme(legend.position = c(0.82,0.9),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))

#Overall brompred
obseffectcp_brompred <- 
  ggeffect(obsmodelcp_brompred,
           type = "re",
           x.as.factor = T,
           terms = c("Bromeliads"),
           ci.lvl = 0.95)
obsplotcp_brompred <- 
  ggplot(obseffectcp_brompred, 
         aes(x=x, 
             y=predicted, 
             colour=group)) + 
  geom_errorbar(aes(ymin=conf.low, 
                    ymax=conf.high), 
                width=0.1,
                lwd = 2,
                position = position_dodge(0.3)) +
  geom_point(position = position_dodge(0.3), 
             lwd =6) +
  ggtitle("") + 
  xlab("Bromeliads") +
  scale_x_discrete(limit = c("absent", "present"),
                   labels = c("Absent", "Present"),
                   expand = expand_scale(add = c(0.6)))+
  ylab("Bromeliad-associated predator abundance") +
  scale_color_manual(name = "",
                     labels = "",
                     values = c("black")) +
  theme(legend.position = "",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))


#Day/night brompred
nocteffect_brompred <- 
  ggeffect(noctmodel_brompred,
           type = "re",
           x.as.factor = T,
           terms = c("Bromeliads", "Time"),
           ci.lvl = 0.95)
noctplot_brompred <- 
  ggplot(nocteffect_brompred, 
         aes(x=x, 
             y=predicted, 
             colour=group)) + 
  geom_errorbar(aes(ymin=conf.low, 
                    ymax=conf.high), 
                width=0.1,
                lwd = 2,
                position = position_dodge(0.3)) +
  geom_point(position = position_dodge(0.3), 
             lwd =6) +
  ggtitle("") + 
  xlab("Bromeliads") +
  scale_x_discrete(limit = c("absent", "present"),
                   labels = c("Absent", "Present"),
                   expand = expand_scale(add = c(0.6)))+
  ylab("Bromeliad-associated predator abundance") +
  scale_color_manual(name = "Time of observation",
                     labels = c("Day", "Night"),
                     values = c("grey", "grey40")) +
  theme(legend.position = c(0.9,0.9),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))

#Panel plot
tiff("obspredbrompred_overall_time.tiff",
    width = 8,
    height= 8,
    unit = "in",
    res = 200)

gridExtra::grid.arrange(grobs = list(obsplotcp_pred +
                                       ggtitle("(a)") + 
                                       theme(axis.title.y=element_text(size = rel(1.3)),
                                             axis.text.y=element_text(size = rel(1.5)),
                                             axis.title.x=element_blank(),
                                             axis.text.x=element_blank(),
                                             axis.ticks.x=element_blank()),
                                     noctplot_pred +
                                       ggtitle("(b)") +
                                       theme(legend.position = c(0.8,0.95),
                                             legend.title = element_text(size = rel(1.1)),
                                             legend.text = element_text(size = rel(1.1)),
                                             axis.title.x=element_blank(),
                                             axis.text.y=element_text(size = rel(1.5)),
                                             axis.text.x=element_blank(),
                                             axis.ticks.x=element_blank(),
                                             axis.title.y=element_blank()),
                                     obsplotcp_brompred +
                                       ggtitle("(c)")+
                                       scale_y_continuous(breaks = c(1, 2),
                                                          limits = c(0.9, 2.3)) +
                                       theme(axis.title.y=element_text(size = rel(1.3)),
                                             axis.text.y=element_text(size = rel(1.5)),
                                             axis.title.x=element_text(size = rel(1.5)),
                                             axis.text.x=element_text(size = rel(1.5))),
                                     noctplot_brompred +
                                       ggtitle("(d)") +
                                       scale_y_continuous(breaks = c(0, 1),
                                                          limits = c(0, 1.4)) +
                                       theme(legend.position = "",
                                             axis.text.y=element_text(size = rel(1.5)),
                                             axis.ticks.x=element_blank(),
                                             axis.title.y=element_blank(),
                                             axis.title.x=element_text(size = rel(1.5)),
                                             axis.text.x=element_text(size = rel(1.5)))
                                     ),
                        ncol =2)
dev.off()

#Positive interactions model
noctintereffect_pos <- 
  ggpredict(noctintermodel_pos,
            terms = c("Bromeliads", "Time", "todo"),
            swap.pred =F,
            x.as.factor = T,
            ci.level = 0.95)
pdf("noctinteraction_pos.pdf",
    height= 5,
    width = 5)
ggplot(noctintereffect_pos,
       ci =T) + 
  aes(x=as.numeric(paste(noctintereffect_pos$facet)),
      y=predicted,
      fill=interaction(x, group),
      linetype=interaction(x, group)) + 
  geom_ribbon(aes(ymin=conf.low,
                  ymax=conf.high),
              linetype=0) +
  geom_line(aes(colour = "black")) +
  ggtitle("") + 
  guides(color = F) +
  xlab("Number of observed specimens") +
  ##make all lines black and remove resulting key
  scale_colour_manual("",
                      labels = NULL,
                      values = c("black")) +
  ###To combine legends, add sme title and labels, otherwise there are two different ones
  scale_fill_manual("",
                     labels = c("Bromeliads present, Day",
                                "Bromeliads absent, Day", 
                                "Bromeliads present, Night",
                                "Bromeliads absent, Night"),
                     values = c(alpha("grey", 0.8), alpha("grey40", 0.8), 
                                alpha("grey", 0.8), alpha("grey40", 0.8))) +
  scale_linetype_manual("", 
                        labels = c("Bromeliads present, Day",
                                   "Bromeliads absent, Day", 
                                   "Bromeliads present, Night",
                                   "Bromeliads absent, Night"),
                        values=c(1,1,2,2)) +
  ylab("Number of positive interactions") +
  ylim(0, 200) +
  theme(legend.position = c(0.3,0.9),
             panel.grid.major = element_blank(), 
             panel.grid.minor = element_blank(),
             panel.background = element_blank(), 
             panel.border = element_blank(),
             axis.line = element_line(colour = "black"))
dev.off()

#Gradual plotting for defense
pdf("noctinteraction_pos_day.pdf",
    height= 5,
    width = 5)
ggplot(noctintereffect_pos %>% 
         filter(group =="Day"),
       ci =T) + 
  aes(x=as.numeric(paste(facet)),
      y=predicted,
      fill=interaction(x, 
                       group)) + 
  geom_ribbon(aes(ymin=conf.low,
                  ymax=conf.high),
              linetype=0) +
  geom_line(aes(colour = x)) +
  ggtitle("") + 
  xlab("Number of observed specimens") +
  scale_colour_manual(name = NULL,
                      labels = NULL,
                      values = c("saddlebrown", "darkgreen")) +
  guides(colour = F) +
  scale_fill_manual(name = NULL,
                    labels = c("Bromeliads, Day", 
                               "No bromeliads, Day"),
                    values = c(present.Day = alpha("saddlebrown", 0.3),
                               absent.Day = alpha("darkgreen", 0.3))) +
  ylab("Number of positive interactions") +
  ylim(0, 200) +
  theme(legend.position = c(0.2,0.9),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))
dev.off()
# Overall PCoA with plot all herbivores--------------------------------------------------------------
pdf("Observationcp_pcoa.pdf",
    width = 5,
    height = 15)
par(mfrow= c(3,1))
#CP bromeliad dietary groups
dietcp_pcoa <- 
  cmdscale(vegdist(obsdietcp[,5:14],
                           distance = "bray"),
                   add = T,
                   k=2,
                   eig = T)
dietcp_pcoa <- 
  add.spec.scores(dietcp_pcoa,
                  obsdietcp[,5:14],
                  method="wa.scores",
                  multi=1,
                  Rscale=F)
col <-
  ifelse(obsdietcp$Bromeliads == "present",
         "saddlebrown",
         "darkgreen")
ordiplot(dietcp_pcoa,
         type="n")
title("a",
      adj = 0)
points(dietcp_pcoa$points,
       pch = 16,
       col=col)
legend ("topright", 
        legend = c("Bromeliads present",
                   "Bromeliads absent"), 
        pch = 15, 
        pt.bg = c("saddlebrown", "darkgreen"), 
        col = c("saddlebrown", "darkgreen"))
ordihull(dietcp_pcoa,
         groups = obsdietcp$Bromeliads,
         show.groups = "present",
         draw = "polygon",
         border =  "transparent",
         col = "saddlebrown")
ordihull(dietcp_pcoa,
         groups = obsdietcp$Bromeliads,
         show.groups = "absent",
         draw = "polygon",
         border =  "transparent",
         col = "darkgreen")
orditorp(dietcp_pcoa$cproj,
         display = "species",
         pcex = 0,
         cex = 1,
         labels = c("Detritivores", "Herbivores", NA, "Non-feeders",
                    "Omnivores", "Parasitoids", NA, "Predators",
                    "Scavengers", "Unknown"),
         air = 1)

#CP bromeliad- restricted functional groups
kindcp_sub_pcoa <- 
  cmdscale(vegdist(obskindcp_sub[,5:20],
                           method ="bray"), 
            k=2,
            eig =T)
kindcp_sub_pcoa <- 
  add.spec.scores(kindcp_sub_pcoa,
                  obskindcp_sub[,5:20],
                  method="wa.scores",
                  multi=1,
                  Rscale=F)
##plot
ordiplot(kindcp_sub_pcoa,
         type ="n")
title("b",
      adj = 0)
col <- 
  ifelse(obskindcp_sub$Bromeliads == "present",
         "saddlebrown", 
         "darkgreen")
points(kindcp_sub_pcoa$points,
       pch = 16,
       col=col)
ordihull(kindcp_sub_pcoa,
         groups = obskindcp_sub$Bromeliads,
         show.groups = "present",
         draw = "polygon",
         border =  "transparent",
         col = "saddlebrown")
ordihull(kindcp_sub_pcoa,
         groups = obskindcp_sub$Bromeliads,
         show.groups = "absent",
         draw = "polygon",
         border =  "transparent",
         col = "darkgreen")
orditorp(kindcp_sub_pcoa$cproj,
         display = "species",
         pcex = 0,
         cex = 1,
         labels = c("Ants", "Herbivorous beetles", NA, NA, "Hunting spiders", 
                    "Hoppers", NA, NA, "Orthopterans", "Parasitoids",
                    NA, NA, "Cockroaches", "Scales/Aphids", "Snails",
                    NA),
         air = 0.1)

#CP bromeliad predators-behaviour frequency
behavfreqcp_nobrompreds_pcoa <- 
  cmdscale(vegdist(spread_behavfreqcp_nobrompreds[,5:11],
                   method ="bray"), 
           k=2,
           add=T,
           eig =T)
behavfreqcp_nobrompreds_pcoa <- 
  add.spec.scores(behavfreqcp_nobrompreds_pcoa,
                  spread_behavfreqcp_nobrompreds[,5:11],
                  method="wa.scores",
                  multi=1,
                  Rscale=F)

ordiplot(behavfreqcp_nobrompreds_pcoa,
         type ="n")
title("c",
      adj = 0)
col <- 
  ifelse(spread_behavfreqcp_nobrompreds$Bromeliads == "present",
         "saddlebrown", 
         "darkgreen")
points(behavfreqcp_nobrompreds_pcoa$points,
       pch = 16,
       col=col)
ordihull(behavfreqcp_nobrompreds_pcoa,
         groups = spread_behavfreqcp_nobrompreds$Bromeliads,
         show.groups = "present",
         draw = "polygon",
         border =  "transparent",
         col = "saddlebrown")
ordihull(behavfreqcp_nobrompreds_pcoa,
         groups = spread_behavfreqcp_nobrompreds$Bromeliads,
         show.groups = "absent",
         draw = "polygon",
         border =  "transparent",
         col = "darkgreen")
brompredvector <- 
  envfit(behavfreqcp_nobrompreds_pcoa$points,
         spread_behavfreqcp_nobrompreds$brompred,
         permutations = 2000)
plot(brompredvector,
     col ="black",
     labels = "Brom. predators",
     lwd = 2,
     cex= 1)

orditorp(behavfreqcp_nobrompreds_pcoa$cproj,
         display = "species",
         pcex = 0,
         cex = 1,
         labels = c("Herbivory", "Mobile", "Predation/Parasitism", "Reproduction",
                    "Stationary", "Tending", "Transporting"),
         air = 0.01)

dev.off()
par(mfrow = c(1,1))
# Overall PCoA with plot herbivores split--------------------------------------------------------------
#PDF
tiff("Observationcp_pcoa_split.tiff",
    width = 5,
    height = 14,
    unit ="in",
    res = 200)
par(mfrow= c(3,1))

#CP bromeliad dietary groups
dietcp_pcoa_rev <- 
  cmdscale(vegdist(obsdietcp_rev[,5:15],
                   distance = "bray"),
           add = T,
           k=2,
           eig = T)
dietcp_pcoa_rev <- 
  add.spec.scores(dietcp_pcoa_rev,
                  obsdietcp_rev[,5:15],
                  method="wa.scores",
                  multi=1,
                  Rscale=F)
col <-
  ifelse(obsdietcp_rev$Bromeliads == "present",
         "grey",
         "grey40")
pch <-
  ifelse(obsdietcp_rev$Bromeliads == "present",
         1,
         16)
ordiplot(dietcp_pcoa_rev,
         type="n",
         cex.axis = 1.5,
         cex.lab = 1.5,
         xlab = "Dimension 1",
         ylab = "Dimension 2")
title("(a)",
      adj = 0,
      cex.main = 2)
points(dietcp_pcoa_rev$points,
       pch = pch,
       col=col)
ordihull(dietcp_pcoa_rev,
         groups = obsdietcp_rev$Bromeliads,
         show.groups = "present",
         draw = "polygon",
         border =  "transparent",
         col = "grey")
ordihull(dietcp_pcoa_rev,
         groups = obsdietcp_rev$Bromeliads,
         show.groups = "absent",
         draw = "polygon",
         border =  "transparent",
         col = "grey40")
orditorp(dietcp_pcoa_rev$cproj,
         display = "species",
         pcex = 0,
         cex = 1.5,
         air =0.01,
         labels = c("Chw", "Det", NA, "Non",
                    "Omn", "Par","Phl", NA, "Prd",
                    "Scv", "Unk"))
legend("topright", 
       col = c("grey", "grey40"), 
       pch = c(1,16),
       cex = 1.5,
       legend = c("Present", "Absent"),
       title = "Bromeliads")

#CP bromeliad- restricted functional groups
kindcp_sub_pcoa <- 
  cmdscale(vegdist(obskindcp_sub[,5:20],
                   method ="bray"), 
           k=2,
           eig =T)
kindcp_sub_pcoa <- 
  add.spec.scores(kindcp_sub_pcoa,
                  obskindcp_sub[,5:20],
                  method="wa.scores",
                  multi=1,
                  Rscale=F)
##plot
ordiplot(kindcp_sub_pcoa,
         type ="n",
         cex.axis = 1.5,
         cex.lab = 1.5,
         xlab = "Dimension 1",
         ylab = "Dimension 2")
title("(b)",
      adj = 0,
      cex.main = 2)
col <- 
  ifelse(obskindcp_sub$Bromeliads == "present",
         "grey", 
         "grey40")
pch <- 
  ifelse(obskindcp_sub$Bromeliads == "present",
         1, 
         16)
points(kindcp_sub_pcoa$points,
       pch = pch,
       col=col)
ordihull(kindcp_sub_pcoa,
         groups = obskindcp_sub$Bromeliads,
         show.groups = "present",
         draw = "polygon",
         border =  "transparent",
         col = "grey")
ordihull(kindcp_sub_pcoa,
         groups = obskindcp_sub$Bromeliads,
         show.groups = "absent",
         draw = "polygon",
         border =  "transparent",
         col = "grey40")
orditorp(kindcp_sub_pcoa$cproj,
         display = "species",
         pcex = 0,
         cex = 1.5,
         labels = c("Ant", "Hbt", NA, NA, "Hsp", 
                    "Hop", NA, NA, NA, "Par",
                    NA, NA, "Coc", "Dew", "Snl",
                    NA),
         air = 0.1)


text(x =-0.1339375,
    y = 0.05,
    labels = "Ort",
    cex = 1.5)

#CP bromeliad predators-behaviour frequency
behavfreqcp_nobrompreds_pcoa_rev <- 
  cmdscale(vegdist(spread_behavfreqcp_nobrompreds_rev[,5:14],
                   method ="bray"), 
           k=2,
           add=T,
           eig =T)
behavfreqcp_nobrompreds_pcoa_rev <- 
  add.spec.scores(behavfreqcp_nobrompreds_pcoa_rev,
                  spread_behavfreqcp_nobrompreds_rev[,5:14],
                  method="wa.scores",
                  multi=1,
                  Rscale=F)

ordiplot(behavfreqcp_nobrompreds_pcoa_rev,
         type ="n",
         cex.axis = 1.5,
         cex.lab = 1.5,
         xlab = "Dimension 1",
         ylab = "Dimension 2")
title("(c)",
      adj = 0,
      cex.main = 2)
col <- 
  ifelse(spread_behavfreqcp_nobrompreds_rev$Bromeliads == "present",
         "grey", 
         "grey40")
pch <- 
  ifelse(spread_behavfreqcp_nobrompreds_rev$Bromeliads == "present",
         1, 
         16)
points(behavfreqcp_nobrompreds_pcoa_rev$points,
       pch =pch,
       col=col)
ordihull(behavfreqcp_nobrompreds_pcoa_rev,
         groups = spread_behavfreqcp_nobrompreds_rev$Bromeliads,
         show.groups = "present",
         draw = "polygon",
         border =  "transparent",
         col = "grey")
ordihull(behavfreqcp_nobrompreds_pcoa_rev,
         groups = spread_behavfreqcp_nobrompreds_rev$Bromeliads,
         show.groups = "absent",
         draw = "polygon",
         border =  "transparent",
         col = "grey40")
brompredvector_rev <- 
  envfit(behavfreqcp_nobrompreds_pcoa_rev$points,
         spread_behavfreqcp_nobrompreds_rev$brompred,
         permutations = 2000)
plot(brompredvector_rev,
     col ="black",
     labels = "",
     lwd = 2,
     cex= 1.3)
text(x = -0.65,
     y = -0.45,
     labels = "Brom. predators",
     cex = 1.5)

orditorp(behavfreqcp_nobrompreds_pcoa_rev$cproj,
         display = "species",
         pcex = 0,
         cex = 1.5,
         labels = c(NA, "Chew", NA, "Mob", "Phl",
                    "PPr", "Rep", "Sta", "Ten", 
                    "Tra"),
         air = 0.01)



dev.off()



# Night and Day PCoA with all herbivores-------------------------------------------------------
pdf("obs_noctpcoa.pdf",
    width =5,
    height = 15)
par(mfrow=c(3,1))
#Diet
noctdiet_pcoa <- 
  cmdscale(vegdist(noctdiet[,6:15],
                   distance = "bray"),
           k=2,
           eig = T)
noctdiet_pcoa <- 
  add.spec.scores(noctdiet_pcoa,
                  noctdiet[,6:15],
                  method="wa.scores",
                  multi=1,
                  Rscale=F)
##plot
col <- 
  ifelse(noctdiet$Bromeliads == "present",
         "saddlebrown", 
         "darkgreen")
pch <-
  ifelse(noctdiet$Time == "Day",
         18, 
         17)

ordiplot(noctdiet_pcoa,
         type = "n")
title("a",
      adj =0)
points(noctdiet_pcoa$points,
       pch = pch,
       col=col)
orditorp(noctdiet_pcoa$cproj,
         display = "species",
         pcex = 0,
         cex = 1,
         labels = c("Detritivores", "Herbivores", NA, "Non-feeders",
                    "Omnivores", "Parasitoids", NA, "Predators",
                    "Scavengers", "Unknown"),
         air = 0.1)

legend ("topright", 
        legend = c("Bromeliads present",
                   "Bromeliads absent"), 
        pch = 15, 
        pt.bg = c("saddlebrown", "darkgreen"), 
        col = c("saddlebrown", "darkgreen"))
legend ("topleft", 
        legend = c("Day",
                   "Night"), 
        pch = c(18,17), 
        pt.bg = "black", 
        col = "black")

#restricted kinds
noctkind_sub_pcoa <- 
  cmdscale(vegdist(noctkind_sub[,6:21],
                           distance = "bray"),
                   k=2,
                   eig = T)
noctkind_sub_pcoa <- 
  add.spec.scores(noctkind_sub_pcoa,
                  noctkind_sub[,6:21],
                  method="wa.scores",
                  multi=1,
                  Rscale=F)

##plot
col <- 
  ifelse(noctkind_sub$Bromeliads == "present",
         "saddlebrown", 
         "darkgreen")
pch <-
  ifelse(noctkind_sub$Time == "Day",
         18, 
         17)

ordiplot(noctkind_sub_pcoa,
         type = "n")
title("b",
      adj =0)
points(noctkind_sub_pcoa$points,
       pch = pch,
       col=col)
orditorp(noctkind_sub_pcoa$cproj,
         display = "species",
         pcex = 0,
         cex = 0.9,
         labels = c("Ants", "Herbivorous beetles", NA, NA, "Hunting spiders", 
                    "Hoppers", NA, NA, "Orthopterans", "Parasitoids",
                    NA, NA, "Cockroaches", "Scales/Aphids", "Snails",
                    NA),
         air = 1)


#Behaviour frequency
noctbehavfreq_pcoa <- 
  cmdscale(vegdist(spread_noctbehavfreq[,6:12],
                           distance = "bray"),
           k=2,
           eig = T)
noctbehavfreq_pcoa <- 
  add.spec.scores(noctbehavfreq_pcoa,
                  spread_noctbehavfreq[,6:12],
                  method="wa.scores",
                  multi=1,
                  Rscale=F)

##plot
col <- 
  ifelse(spread_noctbehavfreq$Bromeliads == "present",
         "saddlebrown", 
         "darkgreen")
pch <-
  ifelse(spread_noctbehavfreq$Time == "Day",
         18, 
         17)

ordiplot(noctbehavfreq_pcoa,
         type = "n")
title("c",
      adj =0)

points(noctbehavfreq_pcoa$points,
       pch = pch,
       col=col)
orditorp(noctbehavfreq_pcoa$cproj,
         display = "species",
         pcex = 0,
         cex = 1,
         labels = c("Herbivory", "Mobile", "Predation/Parasitism", "Reproduction",
                    "Stationary", "Tending", "Transporting"),
         air = 0.01)


dev.off()
par(mfrow = c(1,1))

# Night and Day PCoA with all herbivores split-------------------------------------------------------
pdf("obs_noctpcoa.pdf",
    width =5,
    height = 15)
par(mfrow=c(3,1))
#Diet
noctdiet_pcoa_rev <- 
  cmdscale(vegdist(noctdiet_rev[,6:16],
                   distance = "bray"),
           k=2,
           eig = T)
noctdiet_pcoa_rev <- 
  add.spec.scores(noctdiet_pcoa_rev,
                  noctdiet_rev[,6:16],
                  method="wa.scores",
                  multi=1,
                  Rscale=F)
##plot
col <- 
  ifelse(noctdiet_rev$Bromeliads == "present",
         "saddlebrown", 
         "darkgreen")
pch <-
  ifelse(noctdiet_rev$Time == "Day",
         18, 
         17)

ordiplot(noctdiet_pcoa_rev,
         type = "n")
title("a",
      adj =0)
points(noctdiet_pcoa_rev$points,
       pch = pch,
       col=col)
orditorp(noctdiet_pcoa_rev$cproj,
         display = "species",
         pcex = 0,
         cex = 1,
         labels = c("Leaf chewers", "Detritivores", NA, "Non-feeders",
                    "Omnivores", "Parasitoids","Phloem feeders", NA, "Predators",
                    "Scavengers", "Unknown"),
         air = 0.1)

legend ("topright", 
        legend = c("Bromeliads present",
                   "Bromeliads absent"), 
        pch = 15, 
        pt.bg = c("saddlebrown", "darkgreen"), 
        col = c("saddlebrown", "darkgreen"))
legend ("topleft", 
        legend = c("Day",
                   "Night"), 
        pch = c(18,17), 
        pt.bg = "black", 
        col = "black")

#restricted kinds
noctkind_sub_pcoa <- 
  cmdscale(vegdist(noctkind_sub[,6:21],
                   distance = "bray"),
           k=2,
           eig = T)
noctkind_sub_pcoa <- 
  add.spec.scores(noctkind_sub_pcoa,
                  noctkind_sub[,6:21],
                  method="wa.scores",
                  multi=1,
                  Rscale=F)

##plot
col <- 
  ifelse(noctkind_sub$Bromeliads == "present",
         "saddlebrown", 
         "darkgreen")
pch <-
  ifelse(noctkind_sub$Time == "Day",
         18, 
         17)

ordiplot(noctkind_sub_pcoa,
         type = "n")
title("b",
      adj =0)
points(noctkind_sub_pcoa$points,
       pch = pch,
       col=col)
orditorp(noctkind_sub_pcoa$cproj,
         display = "species",
         pcex = 0,
         cex = 0.9,
         labels = c("Ants", "Herbivorous beetles", NA, NA, "Hunting spiders", 
                    "Hoppers", NA, NA, "Orthopterans", "Parasitoids",
                    NA, NA, "Cockroaches", "Scales/Aphids", "Snails",
                    NA),
         air = 1)


#Behaviour frequency
noctbehavfreq_pcoa_rev <- 
  cmdscale(vegdist(spread_noctbehavfreq_rev[,6:15],
                   distance = "bray"),
           k=2,
           eig = T)
noctbehavfreq_pcoa_rev <- 
  add.spec.scores(noctbehavfreq_pcoa_rev,
                  spread_noctbehavfreq_rev[,6:15],
                  method="wa.scores",
                  multi=1,
                  Rscale=F)

##plot
col <- 
  ifelse(spread_noctbehavfreq_rev$Bromeliads == "present",
         "saddlebrown", 
         "darkgreen")
pch <-
  ifelse(spread_noctbehavfreq_rev$Time == "Day",
         18, 
         17)

ordiplot(noctbehavfreq_pcoa_rev,
         type = "n")
title("c",
      adj =0)

points(noctbehavfreq_pcoa_rev$points,
       pch = pch,
       col=col)
orditorp(noctbehavfreq_pcoa_rev$cproj,
         display = "species",
         pcex = 0,
         cex = 1,
         labels = c("Detritivory", "Leaf chewing", NA, "Mobile", "Phloem sucking",
                    "Predation/Parasitism", "Reproduction", "Stationary", 
                    "Tending", "Transporting"),
         air = 0.01)


dev.off()
par(mfrow = c(1,1))


# Adonis frames -----------------------------------------------------------
#Diet and kind
##CP
write.csv(
  rbind(obsadoniscp_bromdiet$aov.tab,
        obsadoniscp_brompreddiet$aov.tab,
        obsadoniscp_bromkind$aov.tab,
        obsadoniscp_brompredkind$aov.tab,
        obsadoniscp_bromkind_sub$aov.tab,
        obsadoniscp_brompredkind_sub$aov.tab),
  "cpdietkind_adonis.csv")
##DO
write.csv(
  rbind(obsadonisdo_bromdiet$aov.tab,
        obsadonisdo_brompreddiet$aov.tab,
        obsadonisdo_bromkind$aov.tab,
        obsadonisdo_brompredkind$aov.tab,
        obsadonisdo_bromkind_sub$aov.tab,
        obsadonisdo_brompredkind_sub$aov.tab),
  "dodietkind_adonis.csv")

#Behaviour
write.csv(
  rbind(obsadonis_behavdurcp_brom$aov.tab,
        obsadonis_behavdurcp_brompred$aov.tab,
        obsadonis_behavfreqcp_brom$aov.tab,
        obsadonis_behavfreqcp_brompred$aov.tab,
        obsadonis_behavdurdo_brom$aov.tab,
        obsadonis_behavdurdo_brompred$aov.tab,
        obsadonis_behavfreqdo_brom$aov.tab,
        obsadonis_behavfreqdo_brompred$aov.tab),
  "cpdobehavdurfreq_adonis.csv")

#Night and Day
write.csv(
  rbind(noctadonis_bromdiet$aov.tab,
        noctadonis_brompreddiet$aov.tab,
        noctadonis_bromkind$aov.tab,
        noctadonis_brompredkind$aov.tab,
        noctadonis_bromkind_sub$aov.tab,
        noctadonis_brompredkind_sub$aov.tab,
        noctadonis_brombehavdur$aov.tab, 
        noctadonis_brompredbehavdur$aov.tab, 
        noctadonis_brombehavfreq$aov.tab,
        noctadonis_brompredbehavfreq$aov.tab),
  "noctadonis.csv")






# Bar graphs --------------------------------------------------------------
#Night/Day Dietary
##Data
###Prepare
noctdiet_bar <- 
  observation_rev %>% 
  ungroup() %>% 
  filter(Site == "CP" & 
           block %in% c("A", "B", "C")) %>% 
  dplyr::select(alltrees, block, rep, Observer, obs, Bromeliads, Time, Diet, Specimen,number) %>% 
  group_by(alltrees, block, rep, Observer, obs, Bromeliads, Time, Diet, Specimen) %>% 
  summarise_all(funs(mean)) %>% 
  ungroup() %>% 
  dplyr::select(-Specimen, -Observer, -obs) %>% 
  group_by(alltrees, block, rep, Bromeliads, Time, Diet) %>%
  summarise_all(funs(sum)) %>% 
  unite(treerep, alltrees, rep, sep ="_") %>% 
  filter(treerep != "CP_F4_1") %>% 
  filter(Diet %notin% c("poll", "gran", "xylo"))
###Get mean and sd with aggregate()
noctdiet_bar <- 
  aggregate(noctdiet_bar$number , list(noctdiet_bar$Diet,
                                     noctdiet_bar$Time,
                                     noctdiet_bar$Bromeliads), 
            drop = F,
            mean) %>% 
  rename(mean=x) %>% 
  left_join(aggregate(noctdiet_bar$number , list(noctdiet_bar$Diet,
                                                 noctdiet_bar$Time,
                                                 noctdiet_bar$Bromeliads), 
                      drop = F,
                      sd)) %>% 
  rename(Diet=Group.1,
         Time=Group.2,
         Bromeliads=Group.3,
         sd=x)
###Replace NAs with 0
noctdiet_bar[is.na(noctdiet_bar)] <-0
###Convert standard deviation to standard error
noctdiet_bar$sd <- 
  noctdiet_bar$sd/sqrt(69)
noctdiet_bar <- 
  noctdiet_bar %>% 
  rename(se=sd)
##Day plot
barplot_dietday <- 
  ggplot(data=noctdiet_bar %>% filter(Time == "Day"), 
       aes(x=factor(Diet, levels = c("phloem", "chewer", "miner", "pred", "scav", "omni",
                                     "para","detr", "myco","poll/nect", "nada",
                                     "unkn")), 
           y=mean, 
           fill = Bromeliads)) +
  geom_bar(stat="identity",  position=position_dodge()) +
  geom_errorbar(aes(ymin = mean-se, ymax = mean +se), 
                width = 0.2,
                position = position_dodge(0.9)) +
  ggtitle("(a)") +
  ylab("Mean abundance") +
  ylim(-5,40) +
  xlab("") +
  scale_fill_manual(name = "Bromeliads",
                     labels = c("Present", "Absent"), 
                     values = c("grey", "grey40")) +
  scale_x_discrete(breaks = c("phloem", "chewer", "miner", "pred", "scav", "omni",
                              "para","detr", "myco", "poll/nect", "nada",
                              "unkn"),
                   labels = c("Phl", "Chw", "Min", "Prd", "Scv", "Omn",
                              "Par","Det", "Myc", "Pol", "Non",
                              "Unk")) +
  theme(legend.position = c(0.9,0.9),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))
##Night plot
barplot_dietnight <- 
  ggplot(data=noctdiet_bar %>% filter(Time == "Night"), 
       aes(x=factor(Diet, levels = c("phloem", "chewer", "miner", "pred", "scav", "omni",
                                     "para","detr", "myco", "poll/nect", "nada",
                                     "unkn")), 
           y=mean, 
           fill = Bromeliads)) +
  geom_bar(stat="identity",  position=position_dodge()) +
  geom_errorbar(aes(ymin = mean-se, ymax = mean +se), 
                width = 0.2,
                position = position_dodge(0.9)) +
  ggtitle("(d)") +
  ylab("Mean abundance") +
  xlab("") +
  ylim(-5,40) +
  scale_fill_manual(name = "Bromeliads",
                    labels = c("Present", "Absent"), 
                    values = c("grey", "grey40")) +
  scale_x_discrete(breaks = c("phloem", "chewer", "miner", "pred", "scav", "omni",
                              "para","detr", "myco", "poll/nect", "nada",
                              "unkn"),
                   labels = c("Phl", "Chw", "Min", "Prd", "Scv", "Omn",
                              "Par","Det", "Myc", "Pol", "Non",
                              "Unk")) +
  theme(legend.position = c(0.9,0.9),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

#Kinds
##Data
###Prepare
noctkind_sub_bar <- 
  observation %>% 
  ungroup() %>% 
  filter(Site == "CP" & 
           block %in% c("A", "B", "C")) %>% 
  filter(kind %in% c("herbeetle", "heteroherb", "jump", "lepi", "miner",
                     "ortho", "psyllid", "scaleaphid", "snail", "ants", "heteropred", 
                     "huntspids", "lacewings", "predbeetle", "predflies", "opilio",
                     "roaches", "webspids", "para")) %>% 
  dplyr::select(alltrees, block, rep, Observer, obs, Bromeliads, Time, kind, Specimen,number) %>% 
  group_by(alltrees, block, rep, Observer, obs, Bromeliads, Time, kind, Specimen) %>% 
  summarise_all(funs(mean)) %>% 
  ungroup() %>% 
  dplyr::select(-Specimen, -Observer, -obs) %>% 
  group_by(alltrees, block, rep, Bromeliads, Time, kind) %>%
  summarise_all(funs(sum))%>% 
  unite(treerep, alltrees, rep, sep ="_") %>% 
  filter(treerep != "CP_F4_1")
###Get mean and sd with aggregate()
noctkind_sub_bar <- 
  aggregate(noctkind_sub_bar$number , list(noctkind_sub_bar$kind,
                                       noctkind_sub_bar$Time,
                                       noctkind_sub_bar$Bromeliads), 
            drop = F,
            mean) %>% 
  rename(mean=x) %>% 
  left_join(aggregate(noctkind_sub_bar$number , list(noctkind_sub_bar$kind,
                                                 noctkind_sub_bar$Time,
                                                 noctkind_sub_bar$Bromeliads), 
                      drop = F,
                      sd)) %>% 
  rename(kind=Group.1,
         Time=Group.2,
         Bromeliads=Group.3,
         sd=x)
###Replace NAs with 0
noctkind_sub_bar[is.na(noctkind_sub_bar)] <- 
  0
###Convert standard deviation to standard error
noctkind_sub_bar$sd <- 
  noctkind_sub_bar$sd/sqrt(69)
noctkind_sub_bar <- 
  noctkind_sub_bar %>% 
  rename(se=sd)
##Day plot
barplot_kindday <- 
  ggplot(data=noctkind_sub_bar %>% filter(Time == "Day"), 
       aes(x=factor(kind, levels = c("herbeetle", "heteroherb", "jump", "lepi", "miner",
                                     "ortho", "psyllid", "scaleaphid", "snail", "ants", "predbeetle", 
                                     "heteropred","predflies","lacewings", "huntspids", "webspids", 
                                     "opilio","roaches", "para")), 
           y=mean, 
           fill = Bromeliads)) +
  geom_bar(stat="identity",  position=position_dodge()) +
  geom_errorbar(aes(ymin = mean-se, ymax = mean +se), 
                width = 0.2,
                position = position_dodge(0.9)) +
  ggtitle("(b)") +
  ylab("Mean abundance") +
  xlab("") +
  scale_fill_manual(name = "Bromeliads",
                    labels = c("Present", "Absent"), 
                    values = c("grey", "grey40")) +
  scale_x_discrete(breaks =  c("herbeetle", "heteroherb", "jump", "lepi", "miner",
                               "ortho", "psyllid", "scaleaphid", "snail", "ants", "predbeetle", 
                               "heteropred","predflies","lacewings", "huntspids", "webspids", 
                               "opilio","roaches", "para"),
                   labels =  c("Hbt", "Hbu", "Hop", "Lep", "Min",
                               "Ort", "Psy", "Dew", "Snl", "Ant", "Pbt", 
                               "Pbu", "Pfl", "Lac", "Hsp", "Wsp", 
                               "Opl","Coc", "Par")) +
  scale_y_continuous(breaks = c(0, 4, 8),
                     limits = c(0, 8.5)) +
  theme(legend.position = c(0.9,0.9),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))
##Night plot
barplot_kindnight <- 
  ggplot(data=noctkind_sub_bar %>% filter(Time == "Night"), 
       aes(x=factor(kind, levels = c("herbeetle", "heteroherb", "jump", "lepi", "miner",
                                     "ortho", "psyllid", "scaleaphid", "snail", "ants", "predbeetle", 
                                     "heteropred","predflies","lacewings", "huntspids", "webspids", 
                                     "opilio","roaches", "para")), 
           y=mean, 
           fill = Bromeliads)) +
  geom_bar(stat="identity",  position=position_dodge()) +
  geom_errorbar(aes(ymin = mean-se, ymax = mean +se), 
                width = 0.2,
                position = position_dodge(0.9)) +
  ggtitle("(e)") +
  ylab("Mean abundance") +
  scale_y_continuous(breaks = c(0, 4, 8),
                     limits = c(0, 8.5)) +
  xlab("") +
  scale_fill_manual(name = "Bromeliads",
                    labels = c("Present", "Absent"), 
                    values = c("grey", "grey40")) +
  scale_x_discrete(breaks =  c("herbeetle", "heteroherb", "jump", "lepi", "miner",
                               "ortho", "psyllid", "scaleaphid", "snail", "ants", "predbeetle", 
                               "heteropred","predflies","lacewings", "huntspids", "webspids", 
                               "opilio","roaches", "para"),
                   labels =  c("Hbt", "Hbu", "Hop", "Lep", "Min",
                               "Ort", "Psy", "Dew", "Snl", "Ant", "Pbt", 
                               "Pbu", "Pfl", "Lac", "Hsp", "Wsp", 
                               "Opl","Coc", "Par"))  +
  theme(legend.position = c(0.9,0.9),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

#Behaviour frequency
##Data
###Prepare
noctbehavfreq_bar <- 
  observation_rev %>% 
  ungroup() %>% 
  filter(Site == "CP" 
         & block %in% c("A", "B", "C")) %>% 
  dplyr::select(alltrees, block, Bromeliads, rep, Observer, obs, Time, Specimen, behaviour, number) %>% 
  group_by(alltrees, block, Bromeliads, rep, Observer, obs, Time, Specimen, behaviour) %>% 
  summarise_all(funs(mean)) %>%
  ungroup() %>% 
  dplyr::select(-Specimen, -Observer, -obs) %>% 
  group_by(alltrees, block, Bromeliads, rep, Time, behaviour) %>%
  summarise_all(funs(sum)) %>% 
  unite(treerep, alltrees, rep, sep ="_") %>% 
  filter(treerep != "CP_F4_1")
###Get mean and sd with aggregate()
noctbehavfreq_bar <- 
  aggregate(noctbehavfreq_bar$number , list(noctbehavfreq_bar$behaviour,
                                           noctbehavfreq_bar$Time,
                                           noctbehavfreq_bar$Bromeliads), 
            drop = F,
            mean) %>% 
  rename(mean=x) %>% 
  left_join(aggregate(noctbehavfreq_bar$number , list(noctbehavfreq_bar$behaviour,
                                                     noctbehavfreq_bar$Time,
                                                     noctbehavfreq_bar$Bromeliads), 
                      drop = F,
                      sd)) %>% 
  rename(behaviour=Group.1,
         Time=Group.2,
         Bromeliads=Group.3,
         sd=x)
###Replace NAs with 0
noctbehavfreq_bar[is.na(noctbehavfreq_bar)] <- 
  0
###Convert standard deviation to standard error
noctbehavfreq_bar$sd <- 
  noctbehavfreq_bar$sd/sqrt(69)
noctbehavfreq_bar <- 
  noctbehavfreq_bar %>% 
  rename(se=sd)
##Day plot
barplot_behavfreqday <- 
  ggplot(data=noctbehavfreq_bar %>% filter(Time == "Day"), 
         aes(x=factor(behaviour, levels = c("phloem_sucking", "leaf_chewing", "mining",
                                       "predpara", "detr_feeding","tending", 
                                       "mobile", "stationary","transporting", "reproduction")), 
             y=mean, 
             fill = Bromeliads)) +
  geom_bar(stat="identity",  position=position_dodge()) +
  geom_errorbar(aes(ymin = mean-se, ymax = mean +se), 
                width = 0.2,
                position = position_dodge(0.9)) +
  ggtitle("(c)") +
  ylab("Mean abundance") +
  xlab("") +
  scale_y_continuous(breaks = c(0, 4, 8, 12),
                     limits = c(0, 12)) + 
  scale_fill_manual(name = "Bromeliads",
                    labels = c("Present", "Absent"), 
                    values = c("grey", "grey40")) +
  scale_x_discrete(breaks =  c("phloem_sucking", "leaf_chewing", "mining",
                               "predpara", "detr_feeding","tending", 
                               "mobile", "stationary","transporting", "reproduction"),
                   labels =  c("Phl", "Chw", "Min",
                               "PPr", "Det","Ten", 
                               "Mob", "Sta","Trn", "Rep")) +
  theme(legend.position = c(0.9,0.9),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))
##Night plot
barplot_behavfreqnight <- 
  ggplot(data=noctbehavfreq_bar %>% filter(Time == "Night"), 
         aes(x=factor(behaviour, levels = c("phloem_sucking", "leaf_chewing", "mining",
                                            "predpara", "detr_feeding","tending", 
                                            "mobile", "stationary","transporting", "reproduction")), 
             y=mean, 
             fill = Bromeliads)) +
  geom_bar(stat="identity",  position=position_dodge()) +
  geom_errorbar(aes(ymin = mean-se, ymax = mean +se), 
                width = 0.2,
                position = position_dodge(0.9)) +
  ggtitle("(f)") +
  ylab("Mean abundance") +
  scale_y_continuous(breaks = c(0, 4, 8, 12),
                     limits = c(0, 12)) + 
  xlab("") +
  scale_fill_manual(name = "Bromeliads",
                    labels = c("Present", "Absent"), 
                    values = c("grey", "grey40")) +
  scale_x_discrete(breaks =  c("phloem_sucking", "leaf_chewing", "mining",
                               "predpara", "detr_feeding","tending", 
                               "mobile", "stationary","transporting", "reproduction"),
                   labels =  c("Phl", "Chw", "Min",
                               "PPr", "Det","Ten", 
                               "Mob", "Sta","Trn", "Rep")) +
  theme(legend.position = c(0.9,0.9),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))
##V2
tiff("barplot_behavioural_high.tiff",
     width = 20,
     height = 30,
     unit = "in",
     res = 100)
gridExtra::grid.arrange(grobs = list(barplot_dietday +
                                       ylab("Functional group abundance") +
                                       ggtitle("(a)") +
                                       ylim(0,20) +
                                       theme(legend.position = "none",
                                             axis.text.x = element_text(size = rel(2.5)),
                                             axis.text.y = element_text(size = rel(3)),
                                             axis.title.y = element_text(size = rel(3)),
                                             plot.title = element_text(size = rel(3))),
                                     barplot_dietnight +
                                       ggtitle("(b)") +
                                       ylim(0,20) +
                                       theme(legend.position = c(0.9,0.9),
                                             legend.title = element_text(size=rel(2.5)), 
                                             legend.text = element_text(size=rel(2.5)),
                                             axis.text.x = element_text(size = rel(2.5)),
                                             axis.text.y = element_blank(),
                                             axis.title.y = element_blank(),
                                             plot.title = element_text(size = rel(3))),
                                     barplot_kindday +
                                       ggtitle("(c)") +
                                       ylab("Taxonomic group abundance") +
                                       theme(legend.position = "none",
                                             axis.text.x = element_text(size = rel(2.3)),
                                             axis.text.y = element_text(size = rel(3)),
                                             axis.title.y = element_text(size = rel(3)),
                                             plot.title = element_text(size = rel(3))),
                                     barplot_kindnight +
                                       ylab("") +
                                       ggtitle("(d)") +
                                       theme(legend.position = "none",
                                             axis.text.x = element_text(size = rel(2.3)),
                                             axis.text.y =element_blank(),
                                             axis.title.y = element_blank(),
                                             plot.title = element_text(size = rel(3))),
                                     barplot_behavfreqday +
                                       ggtitle("(e)") +
                                       ylab("Behaviour frequency") +
                                       xlab("Diurnal observations") +
                                       theme(legend.position = "none",
                                             axis.text.x = element_text(size = rel(2.5)),
                                             axis.title.x = element_text(size = rel(3)),
                                             axis.text.y = element_text(size = rel(3)),
                                             axis.title.y = element_text(size = rel(3)),
                                             plot.title = element_text(size = rel(3))),
                                     barplot_behavfreqnight +
                                       ylab("") +
                                       ggtitle("(f)") +
                                       xlab("Nocturnal observations") +
                                       theme(legend.position = "none",
                                             axis.text.x = element_text(size = rel(2.5)),
                                             axis.title.x = element_text(size = rel(3)),
                                             axis.text.y = element_blank(),
                                             axis.title.y = element_blank(),
                                             plot.title = element_text(size = rel(3)))),
                        ncol = 2)
dev.off()



# Species accumulation curves ---------------------------------------------
# Data preparation
## CP
accumulation_CP <- 
  observation %>%
    filter(Site == "CP" & 
            block %in% c("A", "B", "C")) %>% 
    dplyr::select(alltrees, block, rep, Observer, obs, Bromeliads, cleanspec, Specimen,number) %>% 
    group_by(alltrees, block, rep, Observer, obs, Bromeliads, cleanspec, Specimen) %>%
    summarise_all(funs(mean)) %>%  
    spread(key = cleanspec, number, fill =0) %>% 
    ungroup()
## DO
accumulation_DO <- 
  observation %>%
  filter(Site == "DO") %>% 
  dplyr::select(alltrees, block, rep, Observer, obs, Bromeliads, cleanspec, Specimen,number) %>% 
  group_by(alltrees, block, rep, Observer, obs, Bromeliads, cleanspec, Specimen) %>% 
  summarise_all(funs(mean)) %>% 
  spread(key = cleanspec, number, fill =0) %>% 
  ungroup()

# Curves
## Computation
accumulation_curve_CP <- 
  specaccum(accumulation_CP[,8:231], 
          method = "random",
          permutations = 2000)
accumulation_curve_DO <- 
  specaccum(accumulation_DO[,8:209], 
            method = "random",
            permutations = 2000)
## Plotting
tiff("accumuation_curves.tiff",
     width = 5,
     height = 5,
     unit = "in",
     res = 100)
plot(accumulation_curve_CP,
     col = "black",
     ci.col = scales::alpha("grey50", 0.4),
     ci.type = "polygon",
     ci.lty = 0,
     xlab = "Number of samples",
     ylab = "Cumulative number of species")
plot(accumulation_curve_DO,
     add = T,
     lty = "dashed",
     col = "black",
     ci.col = scales::alpha("grey50", 0.4),
     ci.type = "polygon",
     ci.lty = 0)
legend("topleft", 
       legend=c("Site CP", "Site DO"),
       col=c("black", "black"), 
       lty=c("solid", "dashed"), 
       cex=0.8)
dev.off()
