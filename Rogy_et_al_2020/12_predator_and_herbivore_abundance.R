#Univariate analyses on predator, herbivore and herbivore functional group abundances

# Models on herbivores/predators  ------------------------------------------
#CP
##Predators
###Model
obsmodelcp_pred <- 
  glmer.nb(number ~ 
             Bromeliads+ 
             (1|block/alltrees),
           control =glmerControl(optimizer = "bobyqa"),
           data =obspred %>% filter(Site == "CP"))
###Assumptions
simulationOutput <- 
  simulateResiduals(fittedModel =obsmodelcp_pred, 
                    n = 2000)
plot(simulationOutput,
     asFactor =T,
     quantreg = F)
###Summary and test
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
###Model
obsmodelcp_brompred <-
  glmer(brompred ~ 
          Bromeliads +
          (1|block/alltrees),
        family = "poisson"(link ="log"),
        data =obsdietcp)
###Assumptions
simulationOutput <- 
  simulateResiduals(fittedModel =obsmodelcp_brompred, 
                    n = 2000,
                    rank = T)
plot(simulationOutput,
     asFactor =T,
     quantreg = F)
###Summary and test
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
###Model
obsmodelcp_herb <- 
  glmer(herb ~ 
          Bromeliads +
          (1|block/alltrees),
        family ="poisson"(link ="inverse"),
        data =obsdietcp %>%
          filter(herb <200))
###Assumptions
simulationOutput <- 
  simulateResiduals(fittedModel =obsmodelcp_herb, 
                    n = 2000)
plot(simulationOutput,
     asFactor =T,
     quantreg = F)
###Summary and test
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
###Model
obsmodeldo_pred <- 
  glm.nb(number ~ 
           Bromeliads,
         data =obspred %>% filter(Site =="DO"))
###Assumptions
simulationOutput <- 
  simulateResiduals(fittedModel =obsmodeldo_pred, 
                    n = 2000)
plot(simulationOutput,
     asFactor =T,
     quantreg = F)
###Summary and test
summary(obsmodeldo_pred)
Anova(obsmodeldo_pred)
visreg(obsmodeldo_pred, 
       "Bromeliads")

##Bromeliad-associated predators
###Model
obsmodeldo_brompred <-
  glm.nb(brompred ~ 
           Bromeliads, 
         data = obsdietdo)
###Assumptions
simulationOutput <- 
  simulateResiduals(fittedModel =obsmodeldo_brompred, 
                    n = 2000)
plot(simulationOutput,
     asFactor =T,
     quantreg = F)
###Summary and tests
summary(obsmodeldo_brompred)
Anova(obsmodeldo_brompred)
visreg(obsmodeldo_brompred, 
       "Bromeliads")

##Herbivores
###Models
obsmodeldo_herb <- 
  glm.nb(herb ~ 
           Bromeliads,
         data =obsdietdo)
###Assumptions
simulationOutput <- 
  simulateResiduals(fittedModel =obsmodeldo_herb, 
                    n = 2000)
plot(simulationOutput,
     asFactor =T,
     quantreg = F)
###Summary and test
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


# Diel models -----------------------------------------------------------
##Predators
###Model
noctmodel_pred <- 
  glmer.nb(number ~ 
             Bromeliads*Time+ 
             (1|block/alltrees),
           control = glmerControl(optimizer = "bobyqa"),
           data =noctobspred)
###Assumptions
simulationOutput <- 
  simulateResiduals(fittedModel =noctmodel_pred, 
                    n = 2000)
plot(simulationOutput, 
     asFactor =T,
     quantreg = F)
###Summary and test
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

##Bromeliad-associated  predators
###Model
noctmodel_brompred <- 
  glmer(brompred ~ 
          Bromeliads*Time+ 
          (1|block/alltrees),
        family = "poisson"(link ="log"),
        data =noctdiet)
###Assumptions
simulationOutput <- 
  simulateResiduals(fittedModel =noctmodel_brompred, 
                    n = 2000)
plot(simulationOutput, 
     asFactor =T,
     quantreg = F)
###Summary and test
summary(noctmodel_brompred)
noctest_brompred <- 
  mixed(brompred ~ 
          Bromeliads*Time +
          (1|block/alltrees),
        data =noctdiet,
        family = "poisson"(link ="log"),
        method = "LRT")$anova_table

##Herbivores
###Model
noctmodel_herb <- 
  glmer.nb(herb ~ 
             Bromeliads*Time+ 
             (1|block/alltrees),
           control = glmerControl(optimizer = "bobyqa"),
           data =noctdiet %>% 
             filter(herb < 200))
###Assumptions
simulationOutput <- 
  simulateResiduals(fittedModel =noctmodel_herb, 
                    n = 2000)
plot(simulationOutput, 
     asFactor =T,
     quantreg = F)
###Summary and test
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

##Leaf chewers
###Model
noctmodel_chewer <- 
  glmer.nb(chewer ~ 
             Bromeliads*Time+ 
             (1|block/alltrees),
           data =noctdiet_rev)
###Assumptions
simulationOutput <- 
  simulateResiduals(fittedModel =noctmodel_chewer, 
                    n = 2000)
plot(simulationOutput, 
     asFactor =T,
     quantreg = F)
###Summary and test
summary(noctmodel_chewer)
noctest_chewer <- 
  mixed(chewer~ 
          Bromeliads*Time +
          (1|block/alltrees),
        data =noctdiet_rev,
        family = "negative.binomial"(theta = getME(noctmodel_chewer,
                                                   "glmer.nb.theta")),
        method = "LRT")$anova_table

##Phloem feeders
###Model
noctmodel_phloem <- 
  glmer.nb(phloem ~ 
             Bromeliads*Time+ 
             (1|block/alltrees),
           data =noctdiet_rev %>% 
             filter(phloem<200))
###Assumptions
simulationOutput <- 
  simulateResiduals(fittedModel =noctmodel_phloem, 
                    n = 2000)
plot(simulationOutput, 
     asFactor =T,
     quantreg = F)
###Summary and test
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

