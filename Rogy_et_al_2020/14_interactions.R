#Univariate analyses on interspecific interactions

# Overall -----------------------------------------------------
#CP
##Negative interspecific interactions
###Model
intermodel_cpneg <-
  glmer(neg ~ 
          Bromeliads*todo+ 
          (1|block/alltrees),
        family = "poisson"(link = "log"),
        control = glmerControl(optimizer = "bobyqa"),
        data = spread_interactioncp)
###Assumptions
simulationOutput <- 
  simulateResiduals(fittedModel =intermodel_cpneg, 
                    n = 2000,
                    rank = T)
plot(simulationOutput, 
     asFactor =F,
     quantreg = F,
     rank = T)
###Summary and test
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

##Positive interspecific interactions
###Model
intermodel_cppos <-
  glmer.nb(pos ~ 
             Bromeliads*todo+ 
             (1|block/alltrees),
           family = "poisson"(link = "log"),
           data = spread_interactioncp)
###Assumptions
simulationOutput <- 
  simulateResiduals(fittedModel =intermodel_cppos, 
                    n = 2000,
                    rank = T)
plot(simulationOutput, 
     asFactor =F,
     quantreg = F,
     rank = T)
###Summary and test
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
##Negative interspecific interactions
###Model
intermodel_doneg <-
  glm.nb(neg ~ 
           Bromeliads*todo,
         data = spread_interactiondo)
###Assumptions
simulationOutput <- 
  simulateResiduals(fittedModel =intermodel_doneg, 
                    n = 2000,
                    rank = T)
plotSimulatedResiduals(simulationOutput = simulationOutput, 
                       asFactor =F,
                       quantreg = F,
                       rank = T)
testZeroInflation(simulationOutput)
###Summary and test
summary(intermodel_doneg)
Anova(intermodel_doneg)
visreg(intermodel_doneg, 
       "todo", 
       by="Bromeliads")

##Positive interspecific interactions
###Model
intermodel_dopos <-
  glm.nb(sqrt(pos) ~ 
           Bromeliads*todo,
         data = spread_interactiondo)
###Assumptions
simulationOutput <- 
  simulateResiduals(fittedModel =intermodel_dopos, 
                    n = 2000,
                    rank = T)
plotSimulatedResiduals(simulationOutput = simulationOutput, 
                       asFactor =F,
                       quantreg = F,
                       rank = T)
###Summary and test
summary(intermodel_dopos)
Anova(intermodel_dopos)
visreg(intermodel_dopos, 
       "todo", 
       by="Bromeliads")


# Diel -----------------------------------------------
##Negative interspecific interactions
###Model
noctintermodel_neg <-
  glmer(neg ~ 
          Bromeliads*Time*todo+ 
          (1|block/alltrees),
        family = "poisson"(link = "log"),
        control = glmerControl(optimizer = "bobyqa"),
        data = spread_noctinteraction)
###Assumptions
simulationOutput <- 
  simulateResiduals(fittedModel =noctintermodel_neg, 
                    n = 2000,
                    rank = T)
plot(simulationOutput, 
     asFactor =F,
     quantreg = F,
     rank = T)
###Summary and test
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

##Positive intrespecific interactions
###Model
noctintermodel_pos <-
  glmer(pos ~ 
          Bromeliads*Time*todo+ 
          (1|block/alltrees),
        family = "poisson"(link = "sqrt"),
        data = spread_noctinteraction)
###Assumptions
simulationOutput <- 
  simulateResiduals(fittedModel =noctintermodel_pos, 
                    n = 2000,
                    rank = T)
plot(simulationOutput, 
     asFactor =F,
     quantreg = F,
     rank = T)
###Summary and test
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
