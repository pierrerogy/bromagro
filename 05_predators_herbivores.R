#Predator effects on herbivore
# All herbivore models ------------------------------------------------------------------
#All predators
##Model
predherbmodel_pred <- 
  glmer(herb ~
          predcenter*Sampling +
          (1|Site/alltrees/quadrats),
        family= "poisson"(link = "log"),
        data = poolcenter)
##Assumptions
simulationOutput <- 
  simulateResiduals(fittedModel =predherbmodel_pred, 
                    n = 2000,
                    rank = T)
plot(simulationOutput,
     quantreg = F,
     rank = T)
##Summary and tests
summary(predherbmodel_pred)
predherbtest_pred <- 
  mixed(herb~ 
          predcenter*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="log"),
        type = afex_options(type = "2"),
        data = poolcenter,
        method = "LRT")$anova_table
##Plot
###visreg
visreg(predherbmodel_pred,
       "predcenter", by = "Sampling")
###ggeffects
####Data
predherbeffect_pred <- 
  ggeffect(predherbmodel_pred,
           terms = c("predcenter", "Sampling"),
           swap.pred = T,
           type = "re",
           ci.level = 0.95)
predherbeffect_pred$group <- 
  factor(predherbeffect_pred$group, levels = c("B", "A"))
col <- 
  ifelse(poolcenter$Sampling == "B",
         "goldenrod4", 
         "goldenrod")
predherbeffect_pred$conf.low <- 
  predherbeffect_pred$conf.low +1
predherbeffect_pred$conf.high <- 
  predherbeffect_pred$conf.high +1
predherbeffect_pred$predicted <- 
  predherbeffect_pred$predicted +1
####Plot
predherbplot_pred <- 
  plot(predherbeffect_pred,
       ci = T) + 
  geom_point(data = poolcenter,
             mapping = aes(x = predcenter, y = jitter(herb+1, 2)), 
             colour = col, 
             fill = col) +
  ggtitle("") + 
  xlab("Predator abundance") +
  ylab("Herbivore abundance") +
  scale_y_continuous(trans = "log",
                     limits = c(0.6, 40),
                     breaks = c(1, 5, 20)) +
  scale_color_manual(name ="Sampling period",
                     labels = c("Before", "After"), 
                     values = c("goldenrod4", "goldenrod")) +
  theme(legend.position = c(0.8 ,0.9),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

#Bromeliad-associated predators
##Model
predherbmodel_brompreds <- 
  glmer(herb ~
          bromcenter*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="log"),
        data = poolcenter)
##Assumptions
simulationOutput <- 
  simulateResiduals(fittedModel =predherbmodel_brompreds, 
                    n = 2000,
                    rank = T)
plot(simulationOutput,
     quantreg = F,
     rank = T)
##Summary and tests
summary(predherbmodel_brompreds)
predherbtest_brompreds <- 
  mixed(herb~ 
          bromcenter*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="log"),
        type = afex_options(type = "2"),
        data = poolcenter,
        method = "LRT")$anova_table
##Plot
###visreg
visreg(predherbmodel_brompreds,
       "bromcenter", by = "Sampling")
###ggeffects
####Data
predherbeffect_brompreds <- 
  ggeffect(predherbmodel_brompreds,
           terms = c("bromcenter", "Sampling"),
           swap.pred = T,
           type = "re",
           ci.level = 0.95)
predherbeffect_brompreds$group <- 
  factor(predherbeffect_brompreds$group, levels = c("B", "A"))
col <- 
  ifelse(poolcenter$Sampling == "B",
         "goldenrod4", 
         "goldenrod")
predherbeffect_brompreds$conf.low <- 
  predherbeffect_brompreds$conf.low +1
predherbeffect_brompreds$conf.high <- 
  predherbeffect_brompreds$conf.high +1
predherbeffect_brompreds$predicted <- 
  predherbeffect_brompreds$predicted +1
####Plot
predherbplot_brompreds <- 
  plot(predherbeffect_brompreds,
       ci = T) + 
  geom_point(data = poolcenter,
             mapping = aes(x = bromcenter, y = jitter(herb+1, 2)), 
             colour = col, 
             fill = col) +
  ggtitle("") + 
  xlab("Bromeliad-associated predator abundance") +
  ylab("Herbivore abundance") +
  scale_y_continuous(trans = "log",
                     limits = c(0.6, 40),
                     breaks = c(1, 5, 20)) +
  scale_color_manual(name = "Sampling period",
                     labels = c("Before", "After"), 
                     values = c("goldenrod4", "goldenrod")) +
  theme(legend.position = c(0.8,0.9),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))


#Tree-associated predators
##Model
predherbmodel_treepreds <- 
  glmer(herb ~
          arbocenter*Sampling +
          (1|Site/alltrees/quadrats),
        family = "poisson"(link ="log"),
        data = poolcenter)
##Assumptions
simulationOutput <- 
  simulateResiduals(fittedModel =predherbmodel_treepreds, 
                    n = 2000,
                    rank = T)
plot(simulationOutput,
     quantreg = F,
     rank = T)
##Summary and tests
summary(predherbmodel_treepreds)
predherbtest_treepreds <- 
  mixed(herb~ 
          arbocenter*Sampling + 
          (1|Site/alltrees/quadrats),
        family = "poisson"(link ="log"),
        type = afex_options(type = "2"),
        data = poolcenter,
        method = "LRT")$anova_table
##Plot
###visreg
visreg(predherbmodel_treepreds,
       "arbocenter", by = "Sampling")
###ggeffects
####Data
predherbeffect_treepreds <- 
  ggeffect(predherbmodel_treepreds,
           terms = c("arbocenter", "Sampling"),
           swap.pred = T,
           type = "re",
           ci.level = 0.95)
predherbeffect_treepreds$group <- 
  factor(predherbeffect_treepreds$group, levels = c("B", "A"))
col <- 
  ifelse(poolcenter$Sampling == "B",
         "goldenrod4", 
         "goldenrod")
predherbeffect_treepreds$conf.low <- 
  predherbeffect_treepreds$conf.low +1
predherbeffect_treepreds$conf.high <- 
  predherbeffect_treepreds$conf.high +1
predherbeffect_treepreds$predicted <- 
  predherbeffect_treepreds$predicted +1
####Plot
predherbplot_treepreds <- 
  plot(predherbeffect_treepreds,
       ci = T) + 
  geom_point(data = poolcenter,
             mapping = aes(x = arbocenter, y = jitter(herb+1, 2)), 
             colour = col, 
             fill = col) +
  ggtitle("") + 
  xlab("Tree-associated predator abundance") +
  ylab("Herbivore abundance") +
  scale_y_continuous(trans = "log",
                     limits = c(0.6, 40),
                     breaks = c(1, 5, 20)) +
  scale_color_manual(name ="Sampling period",
                     labels = c("Before", "After"), 
                     values = c("goldenrod4", "goldenrod")) +
  theme(legend.position = c(0.8 ,0.9),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

#Mobile predators
##Model
predherbmodel_mobipreds <- 
  glmer(herb ~
          mobicenter*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="log"),
        data = poolcenter)
##Assumptions
simulationOutput <- 
  simulateResiduals(fittedModel =predherbmodel_mobipreds, 
                    n = 2000,
                    rank = T)
plot(simulationOutput,
     quantreg = F,
     rank = T)
##Summary and tests
summary(predherbmodel_mobipreds)
predherbtest_mobipreds <- 
  mixed(herb~ 
          mobicenter*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="sqrt"),
        type = afex_options(type = "2"),
        data = poolcenter,
        method = "LRT")$anova_table
##Plot
###visreg
visreg(predherbmodel_mobipreds,
       "mobicenter", by = "Sampling")
###ggeffects
####Data
predherbeffect_mobipreds <- 
  ggeffect(predherbmodel_mobipreds,
           terms = c("mobicenter", "Sampling"),
           swap.pred = T,
           type = "re",
           ci.level = 0.95)
predherbeffect_mobipreds$group <- 
  factor(predherbeffect_mobipreds$group, levels = c("B", "A"))
col <- 
  ifelse(poolcenter$Sampling == "B",
         "goldenrod4", 
         "goldenrod")
predherbeffect_mobipreds$conf.low <- 
  predherbeffect_mobipreds$conf.low +1
predherbeffect_mobipreds$conf.high <- 
  predherbeffect_mobipreds$conf.high +1
predherbeffect_mobipreds$predicted <- 
  predherbeffect_mobipreds$predicted +1
####Plot
predherbplot_mobipreds <- 
  plot(predherbeffect_mobipreds,
       ci = T) + 
  geom_point(data = poolcenter,
             mapping = aes(x = mobicenter, y = jitter(herb+1, 2)), 
             colour = col, 
             fill = col) +
  ggtitle("") + 
  xlab("Aerial predator abundance") +
  ylab("Herbivore abundance") +
  scale_y_continuous(trans = "log",
                     limits = c(0.6, 40),
                     breaks = c(1, 5, 20)) +
  scale_color_manual(name ="Sampling period",
                     labels = c("Before", "After"), 
                     values = c("goldenrod4", "goldenrod")) +
  theme(legend.position = c(0.8 ,0.9),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

#Parasitoids
##Model
predherbmodel_para <- 
  glmer(herb~
          paracenter*Sampling +
          (1|Site/alltrees/quadrats),
        family = "poisson"(link="log"),
        data = poolcenter)
##Assumptions
simulationOutput <- 
  simulateResiduals(fittedModel =predherbmodel_para, 
                    n = 2000,
                    rank = T)
plot(simulationOutput,
     quantreg = F,
     rank = T)
##Summary and tests
summary(predherbmodel_para)
predherbtest_para <- 
  mixed(herb~ 
          paracenter*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="log"),
        type = afex_options(type = "2"),
        data = poolcenter,
        method = "LRT")$anova_table
##Plot
###visreg
visreg(predherbmodel_para,
       "paracenter", by = "Sampling")
###ggeffects
####Data
predherbeffect_para <- 
  ggeffect(predherbmodel_para,
           terms = c("paracenter", "Sampling"),
           swap.pred = T,
           type = "re",
           ci.level = 0.95)
predherbeffect_para$group <- 
  factor(predherbeffect_para$group, levels = c("B", "A"))
col <- 
  ifelse(poolcenter$Sampling == "B",
         "goldenrod4", 
         "goldenrod")
predherbeffect_para$conf.low <- 
  predherbeffect_para$conf.low +1
predherbeffect_para$conf.high <- 
  predherbeffect_para$conf.high +1
predherbeffect_para$predicted <- 
  predherbeffect_para$predicted +1
####Plot
predherbplot_para <- 
  plot(predherbeffect_para,
       ci = T) + 
  geom_point(data = poolcenter,
             mapping = aes(x = paracenter, y = jitter(herb+1, 2)), 
             colour = col, 
             fill = col) +
  ggtitle("") + 
  xlab("Parasitoid abundance") +
  ylab("Herbivore abundance") +
  scale_y_continuous(trans = "log",
                     limits = c(0.6, 40),
                     breaks = c(1, 5, 20)) +
  scale_color_manual(name ="Sampling period",
                     labels = c("Before", "After"), 
                     values = c("goldenrod4", "goldenrod")) +
  theme(legend.position = c(0.8 ,0.9),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

# All herbivore adonis ------------------------------------------------------------------
#All predators
herbadonis_preds <- 
  adonis(spread_herb[,11:17] ~
           predcenter*Sampling,
         method = "bray",
         permutations = 2000,
         strata = spread_herb$Site,
         data = spread_herb)


#Bromeliad-associated predators
herbadonis_brompred <- 
  adonis(spread_herb[,11:17] ~
           bromcenter*Sampling,
         method = "bray",
         permutations = 2000,
         strata = spread_herb$Site,
         data = spread_herb)  


#Tree-associated predators
herbadonis_arbopred <- 
  adonis(spread_herb[,11:17] ~
           arbocenter*Sampling,
         method = "bray",
         permutations = 2000,
         strata = spread_herb$Site,
         data = spread_herb)  

#Aerial predators
herbadonis_mobipred <- 
  adonis(spread_herb[,11:17] ~
           mobicenter*Sampling,
         method = "bray",
         permutations = 2000,
         strata = spread_herb$Site,
         data = spread_herb) 

#Parasitoids
herbadonis_para <- 
  adonis(spread_herb[,11:17] ~
           paracenter*Sampling,
         method = "bray",
         permutations = 2000,
         strata = spread_herb$Site,
         data = spread_herb) 



# Leaf chewer models ------------------------------------------------------------------
#All predators
##Model
predchewermodel_pred <- 
  glmer(chewer ~
          predcenter*Sampling +
          (1|Site/alltrees/quadrats),
        family= "poisson"(link = "log"),
        data = poolcenter)
##Assumptions
simulationOutput <- 
  simulateResiduals(fittedModel =predchewermodel_pred, 
                    n = 2000,
                    rank = T)
plot(simulationOutput,
     quantreg = F,
     rank = T)
##Summary and tests
summary(predchewermodel_pred)
predchewertest_pred <- 
  mixed(chewer~ 
          predcenter*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="log"),
        type = afex_options(type = "2"),
        data = poolcenter,
        method = "LRT")$anova_table
##Plot
###visreg
visreg(predchewermodel_pred,
       "predcenter", by = "Sampling")
###ggeffects
####Data
predchewereffect_pred <- 
  ggeffect(predchewermodel_pred,
           terms = c("predcenter", "Sampling"),
           swap.pred = T,
           type = "re",
           ci.level = 0.95)
predchewereffect_pred$group <- 
  factor(predchewereffect_pred$group, levels = c("B", "A"))
col <- 
  ifelse(poolcenter$Sampling == "B",
         "goldenrod4", 
         "goldenrod")
predchewereffect_pred$conf.low <- 
  predchewereffect_pred$conf.low +1
predchewereffect_pred$conf.high <- 
  predchewereffect_pred$conf.high +1
predchewereffect_pred$predicted <- 
  predchewereffect_pred$predicted +1
####Plot
predchewerplot_pred <- 
  plot(predchewereffect_pred,
       ci = T) + 
  geom_point(data = poolcenter,
             mapping = aes(x = predcenter, y = jitter(chewer+1, 2)), 
             colour = col, 
             fill = col) +
  ggtitle("") + 
  xlab("Predator abundance") +
  ylab("Leaf chewer abundance") +
  scale_y_continuous(trans = "log",
                     limits = c(0.6, 40),
                     breaks = c(1, 5, 20)) +
  scale_color_manual(name ="Sampling period",
                     labels = c("Before", "After"), 
                     values = c("goldenrod4", "goldenrod")) +
  theme(legend.position = c(0.8 ,0.9),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

#Bromeliad-associated predators
##Model
predchewermodel_brompreds <- 
  glmer(chewer ~
          bromcenter*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="log"),
        data = poolcenter)
##Assumptions
simulationOutput <- 
  simulateResiduals(fittedModel =predchewermodel_brompreds, 
                    n = 2000,
                    rank = T)
plot(simulationOutput,
     quantreg = F,
     rank = T)
##Summary and tests
summary(predchewermodel_brompreds)
predchewertest_brompreds <- 
  mixed(chewer~ 
          bromcenter*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="log"),
        type = afex_options(type = "2"),
        data = poolcenter,
        method = "LRT")$anova_table
##Plot
###visreg
visreg(predchewermodel_brompreds,
       "bromcenter", by = "Sampling")
###ggeffects
####Data
predchewereffect_brompreds <- 
  ggeffect(predchewermodel_brompreds,
           terms = c("bromcenter", "Sampling"),
           swap.pred = T,
           type = "re",
           ci.level = 0.95)
predchewereffect_brompreds$group <- 
  factor(predchewereffect_brompreds$group, levels = c("B", "A"))
col <- 
  ifelse(poolcenter$Sampling == "B",
         "goldenrod4", 
         "goldenrod")
predchewereffect_brompreds$conf.low <- 
  predchewereffect_brompreds$conf.low +1
predchewereffect_brompreds$conf.high <- 
  predchewereffect_brompreds$conf.high +1
predchewereffect_brompreds$predicted <- 
  predchewereffect_brompreds$predicted +1
####Plot
predchewerplot_brompreds <- 
  plot(predchewereffect_brompreds,
       ci = T) + 
  geom_point(data = poolcenter,
             mapping = aes(x = bromcenter, y = jitter(chewer+1, 2)), 
             colour = col, 
             fill = col) +
  ggtitle("") + 
  xlab("Bromeliad-associated predator abundance") +
  ylab("Leaf chewer abundance") +
  scale_y_continuous(trans = "log",
                     limits = c(0.6, 40),
                     breaks = c(1, 5, 20)) +
  scale_color_manual(name ="Sampling period",
                     labels = c("Before", "After"), 
                     values = c("goldenrod4", "goldenrod")) +
  theme(legend.position = c(0.8 ,0.9),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))


#Tree-associated predators
##Model
predchewermodel_treepreds <- 
  glmer(chewer ~
          arbocenter*Sampling +
          (1|Site/alltrees/quadrats),
        family = "poisson"(link ="log"),
        data = poolcenter)
##Assumptions
simulationOutput <- 
  simulateResiduals(fittedModel =predchewermodel_treepreds, 
                    n = 2000,
                    rank = T)
plot(simulationOutput,
     quantreg = F,
     rank = T)
##Summary and tests
summary(predchewermodel_treepreds)
predchewertest_treepreds <- 
  mixed(chewer~ 
          arbocenter*Sampling + 
          (1|Site/alltrees/quadrats),
        family = "poisson"(link ="log"),
        type = afex_options(type = "2"),
        data = poolcenter,
        method = "LRT")$anova_table
##Plot
###visreg
visreg(predchewermodel_treepreds,
       "arbocenter", by = "Sampling")
###ggeffects
####Data
predchewereffect_treepreds <- 
  ggeffect(predchewermodel_treepreds,
           terms = c("arbocenter", "Sampling"),
           swap.pred = T,
           type = "re",
           ci.level = 0.95)
predchewereffect_treepreds$group <- 
  factor(predchewereffect_treepreds$group, levels = c("B", "A"))
col <- 
  ifelse(poolcenter$Sampling == "B",
         "goldenrod4", 
         "goldenrod")
predchewereffect_treepreds$conf.low <- 
  predchewereffect_treepreds$conf.low +1
predchewereffect_treepreds$conf.high <- 
  predchewereffect_treepreds$conf.high +1
predchewereffect_treepreds$predicted <- 
  predchewereffect_treepreds$predicted +1
####Plot
predchewerplot_treepreds <- 
  plot(predchewereffect_treepreds,
       ci = T) + 
  geom_point(data = poolcenter,
             mapping = aes(x = arbocenter, y = jitter(chewer+1, 2)), 
             colour = col, 
             fill = col) +
  ggtitle("") + 
  xlab("Tree-associated predator abundance") +
  ylab("Leaf chewer abundance") +
  scale_y_continuous(trans = "log",
                     limits = c(0.6, 40),
                     breaks = c(1, 5, 20)) +
  scale_color_manual(name ="Sampling period",
                     labels = c("Before", "After"), 
                     values = c("goldenrod4", "goldenrod")) +
  theme(legend.position = c(0.8 ,0.9),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

#Mobile predators
##Model
predchewermodel_mobipreds <- 
  glmer(chewer ~
          mobicenter*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="log"),
        data = poolcenter)
##Assumptions
simulationOutput <- 
  simulateResiduals(fittedModel =predchewermodel_mobipreds, 
                    n = 2000,
                    rank = T)
plot(simulationOutput,
     quantreg = F,
     rank = T)
##Summary and tests
summary(predchewermodel_mobipreds)
predchewertest_mobipreds <- 
  mixed(chewer~ 
          mobicenter*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="log"),
        type = afex_options(type = "2"),
        data = poolcenter,
        method = "LRT")$anova_table
##Plot
###visreg
visreg(predchewermodel_mobipreds,
       "mobicenter", by = "Sampling")
###ggeffects
####Data
predchewereffect_mobipreds <- 
  ggeffect(predchewermodel_mobipreds,
           terms = c("mobicenter", "Sampling"),
           swap.pred = T,
           type = "re",
           ci.level = 0.95)
predchewereffect_mobipreds$group <- 
  factor(predchewereffect_mobipreds$group, levels = c("B", "A"))
col <- 
  ifelse(poolcenter$Sampling == "B",
         "goldenrod4", 
         "goldenrod")
predchewereffect_mobipreds$conf.low <- 
  predchewereffect_mobipreds$conf.low +1
predchewereffect_mobipreds$conf.high <- 
  predchewereffect_mobipreds$conf.high +1
predchewereffect_mobipreds$predicted <- 
  predchewereffect_mobipreds$predicted +1
####Plot
predchewerplot_mobipreds <- 
  plot(predchewereffect_mobipreds,
       ci = T) + 
  geom_point(data = poolcenter,
             mapping = aes(x = mobicenter, y = jitter(chewer+1, 2)), 
             colour = col, 
             fill = col) +
  ggtitle("") + 
  xlab("Aerial predator abundance") +
  ylab("Leaf chewer abundance") +
  scale_y_continuous(trans = "log",
                     limits = c(0.6, 40),
                     breaks = c(1, 5, 20)) +
  scale_color_manual(name ="Sampling period",
                     labels = c("Before", "After"), 
                     values = c("goldenrod4", "goldenrod")) +
  theme(legend.position = c(0.8 ,0.9),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

#Parasitoids
##Model
predchewermodel_para <- 
  glmer(chewer~
          paracenter*Sampling +
          (1|Site/alltrees/quadrats),
        family = "poisson"(link ="log"),
        data = poolcenter)
##Assumptions
simulationOutput <- 
  simulateResiduals(fittedModel =predchewermodel_para, 
                    n = 2000,
                    rank = T)
plot(simulationOutput,
     quantreg = F,
     rank = T)
##Summary and tests
summary(predchewermodel_para)
predchewertest_para <- 
  mixed(chewer~ 
          paracenter*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="log"),
        type = afex_options(type = "2"),
        data = poolcenter,
        method = "LRT")$anova_table
##Plot
###visreg
visreg(predchewermodel_para,
       "paracenter", by = "Sampling")
###ggeffects
####Data
predchewereffect_para <- 
  ggeffect(predchewermodel_para,
           terms = c("paracenter", "Sampling"),
           swap.pred = T,
           type = "re",
           ci.level = 0.95)
predchewereffect_para$group <- 
  factor(predchewereffect_para$group, levels = c("B", "A"))
col <- 
  ifelse(poolcenter$Sampling == "B",
         "goldenrod4", 
         "goldenrod")
predchewereffect_para$conf.low <- 
  predchewereffect_para$conf.low +1
predchewereffect_para$conf.high <- 
  predchewereffect_para$conf.high +1
predchewereffect_para$predicted <- 
  predchewereffect_para$predicted +1
####Plot
predchewerplot_para <- 
  plot(predchewereffect_para,
       ci = T) + 
  geom_point(data = poolcenter,
             mapping = aes(x = paracenter, y = jitter(chewer+1, 2)), 
             colour = col, 
             fill = col) +
  ggtitle("") + 
  xlab("Parasitoid abundance") +
  ylab("Leaf chewer abundance") +
  scale_y_continuous(trans = "log",
                     limits = c(0.6, 40),
                     breaks = c(1, 5, 20)) +
  scale_color_manual(name ="Sampling period",
                     labels = c("Before", "After"), 
                     values = c("goldenrod4", "goldenrod")) +
  theme(legend.position = c(0.8 ,0.9),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))


# Phloem sucker models ------------------------------------------------------------------
#All predators
##Model
predphloemmodel_pred <- 
  glmer(phloem ~
          predcenter*Sampling +
          (1|Site/alltrees/quadrats),
        family= "poisson"(link = "log"),
        data = poolcenter)
##Assumptions
simulationOutput <- 
  simulateResiduals(fittedModel =predphloemmodel_pred, 
                    n = 2000,
                    rank = T)
plot(simulationOutput,
     quantreg = F,
     rank = T)
##Summary and tests
summary(predphloemmodel_pred)
predphloemtest_pred <- 
  mixed(phloem~ 
          predcenter*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="log"),
        type = afex_options(type = "2"),
        data = poolcenter,
        method = "LRT")$anova_table
##Plot
###visreg
visreg(predphloemmodel_pred,
       "predcenter", by = "Sampling")
###ggeffects
####Data
predphloemeffect_pred <- 
  ggeffect(predphloemmodel_pred,
           terms = c("predcenter", "Sampling"),
           swap.pred = T,
           type = "re",
           ci.level = 0.95)
predphloemeffect_pred$group <- 
  factor(predphloemeffect_pred$group, levels = c("B", "A"))
col <- 
  ifelse(poolcenter$Sampling == "B",
         "goldenrod4", 
         "goldenrod")
predphloemeffect_pred$conf.low <- 
  predphloemeffect_pred$conf.low +1
predphloemeffect_pred$conf.high <- 
  predphloemeffect_pred$conf.high +1
predphloemeffect_pred$predicted <- 
  predphloemeffect_pred$predicted +1
####Plot
predphloemplot_pred <- 
  plot(predphloemeffect_pred,
       ci = T) + 
  geom_point(data = poolcenter,
             mapping = aes(x = predcenter, y = jitter(phloem+1, 2)), 
             colour = col, 
             fill = col) +
  ggtitle("") + 
  xlab("Predator abundance") +
  ylab("Phloem sucker abundance") +
  scale_y_continuous(trans = "log",
                     limits = c(0.6, 40),
                     breaks = c(1, 5, 20)) +
  scale_color_manual(name ="Sampling period",
                     labels = c("Before", "After"), 
                     values = c("goldenrod4", "goldenrod")) +
  theme(legend.position = c(0.8 ,0.9),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

#Bromeliad-associated predators
##Model
predphloemmodel_brompreds <- 
  glmer(phloem ~
          bromcenter*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="log"),
        data = poolcenter)
##Assumptions
simulationOutput <- 
  simulateResiduals(fittedModel =predphloemmodel_brompreds, 
                    n = 2000,
                    rank = T)
plot(simulationOutput,
     quantreg = F,
     rank = T)
##Summary and tests
summary(predphloemmodel_brompreds)
predphloemtest_brompreds <- 
  mixed(phloem~ 
          bromcenter*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="log"),
        type = afex_options(type = "2"),
        data = poolcenter,
        method = "LRT")$anova_table
##Plot
###visreg
visreg(predphloemmodel_brompreds,
       "bromcenter", by = "Sampling")
###ggeffects
####Data
predphloemeffect_brompreds <- 
  ggeffect(predphloemmodel_brompreds,
           terms = c("bromcenter", "Sampling"),
           swap.pred = T,
           type = "re",
           ci.level = 0.95)
predphloemeffect_brompreds$group <- 
  factor(predphloemeffect_brompreds$group, levels = c("B", "A"))
col <- 
  ifelse(poolcenter$Sampling == "B",
         "goldenrod4", 
         "goldenrod")
predphloemeffect_brompreds$conf.low <- 
  predphloemeffect_brompreds$conf.low +1
predphloemeffect_brompreds$conf.high <- 
  predphloemeffect_brompreds$conf.high +1
predphloemeffect_brompreds$predicted <- 
  predphloemeffect_brompreds$predicted +1
####Plot
predphloemplot_brompreds <- 
  plot(predphloemeffect_brompreds,
       ci = T) + 
  geom_point(data = poolcenter,
             mapping = aes(x = bromcenter, y = jitter(phloem+1, 2)), 
             colour = col, 
             fill = col) +
  ggtitle("") + 
  xlab("Bromeliad-associated predator abundance") +
  ylab("Phloem sucker abundance") +
  scale_y_continuous(trans = "log",
                     limits = c(0.6, 40),
                     breaks = c(1, 5, 20)) +
  scale_color_manual(name ="Sampling period",
                     labels = c("Before", "After"), 
                     values = c("goldenrod4", "goldenrod")) +
  theme(legend.position = c(0.8 ,0.9),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

#Tree-associated predators
##Model
predphloemmodel_treepreds <- 
  glmer(phloem ~
          arbocenter*Sampling +
          (1|Site/alltrees/quadrats),
        family = "poisson"(link ="log"),
        data = poolcenter)
##Assumptions
simulationOutput <- 
  simulateResiduals(fittedModel =predphloemmodel_treepreds, 
                    n = 2000,
                    rank = T)
plot(simulationOutput,
     quantreg = F,
     rank = T)
##Summary and tests
summary(predphloemmodel_treepreds)
predphloemtest_treepreds <- 
  mixed(phloem~ 
          arbocenter*Sampling + 
          (1|Site/alltrees/quadrats),
        family = "poisson"(link ="log"),
        type = afex_options(type = "2"),
        data = poolcenter,
        method = "LRT")$anova_table
##Plot
###visreg
visreg(predphloemmodel_treepreds,
       "arbocenter", by = "Sampling")
###ggeffects
####Data
predphloemeffect_treepreds <- 
  ggeffect(predphloemmodel_treepreds,
           terms = c("arbocenter", "Sampling"),
           swap.pred = T,
           type = "re",
           ci.level = 0.95)
predphloemeffect_treepreds$group <- 
  factor(predphloemeffect_treepreds$group, levels = c("B", "A"))
col <- 
  ifelse(poolcenter$Sampling == "B",
         "goldenrod4", 
         "goldenrod")
predphloemeffect_treepreds$conf.low <- 
  predphloemeffect_treepreds$conf.low +1
predphloemeffect_treepreds$conf.high <- 
  predphloemeffect_treepreds$conf.high +1
predphloemeffect_treepreds$predicted <- 
  predphloemeffect_treepreds$predicted +1
####Plot
predphloemplot_treepreds <- 
  plot(predphloemeffect_treepreds,
       ci = T) + 
  geom_point(data = poolcenter,
             mapping = aes(x = arbocenter, y = jitter(phloem+1, 2)), 
             colour = col, 
             fill = col) +
  ggtitle("") + 
  xlab("Tree-associated predator abundance") +
  ylab("Phloem sucker abundance") +
  scale_y_continuous(trans = "log",
                     limits = c(0.6, 40),
                     breaks = c(1, 5, 20)) +
  scale_color_manual(name ="Sampling period",
                     labels = c("Before", "After"), 
                     values = c("goldenrod4", "goldenrod")) +
  theme(legend.position = c(0.8 ,0.9),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

#Aerial predators
##Model
predphloemmodel_mobipreds <- 
  glmer(phloem ~
          mobicenter*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="sqrt"),
        data = poolcenter)
##Assumptions
simulationOutput <- 
  simulateResiduals(fittedModel =predphloemmodel_mobipreds, 
                    n = 2000,
                    rank = T)
plot(simulationOutput,
     quantreg = F,
     rank = T)
##Summary and tests
summary(predphloemmodel_mobipreds)
predphloemtest_mobipreds <- 
  mixed(phloem~ 
          mobicenter*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="log"),
        type = afex_options(type = "2"),
        data = poolcenter,
        method = "LRT")$anova_table
##Plot
###visreg
visreg(predphloemmodel_mobipreds,
       "mobicenter", by = "Sampling")
###ggeffects
####Data
predphloemeffect_mobipreds <- 
  ggeffect(predphloemmodel_mobipreds,
           terms = c("mobicenter", "Sampling"),
           swap.pred = T,
           type = "re",
           ci.level = 0.95)
predphloemeffect_mobipreds$group <- 
  factor(predphloemeffect_mobipreds$group, levels = c("B", "A"))
col <- 
  ifelse(poolcenter$Sampling == "B",
         "goldenrod4", 
         "goldenrod")
predphloemeffect_mobipreds$conf.low <- 
  predphloemeffect_mobipreds$conf.low +1
predphloemeffect_mobipreds$conf.high <- 
  predphloemeffect_mobipreds$conf.high +1
predphloemeffect_mobipreds$predicted <- 
  predphloemeffect_mobipreds$predicted +1
####Plot
predphloemplot_mobipreds <- 
  plot(predphloemeffect_mobipreds,
       ci = T) + 
  geom_point(data = poolcenter,
             mapping = aes(x = mobicenter, y = jitter(phloem+1, 2)), 
             colour = col, 
             fill = col) +
  ggtitle("") + 
  xlab("Aerial predator abundance") +
  ylab("Phloem sucker abundance") +
  scale_y_continuous(trans = "log",
                     limits = c(0.6, 40),
                     breaks = c(1, 5, 20)) +
  scale_color_manual(name ="Sampling period",
                     labels = c("Before", "After"), 
                     values = c("goldenrod4", "goldenrod")) +
  theme(legend.position = c(0.8 ,0.9),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

#Parasitoids
##Model
predphloemmodel_para <- 
  glmer(phloem~
          paracenter*Sampling +
          (1|Site/alltrees/quadrats),
        family = "poisson"(link ="log"),
        data = poolcenter)
##Assumptions
simulationOutput <- 
  simulateResiduals(fittedModel =predphloemmodel_para, 
                    n = 2000,
                    rank = T)
plot(simulationOutput,
     quantreg = F,
     rank = T)
##Summary and tests
summary(predphloemmodel_para)
predphloemtest_para <- 
  mixed(phloem~ 
          paracenter*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="log"),
        type = afex_options(type = "2"),
        data = poolcenter,
        method = "LRT")$anova_table
##Plot
###visreg
visreg(predphloemmodel_para,
       "paracenter", by = "Sampling")
###ggeffects
####Data
predphloemeffect_para <- 
  ggeffect(predphloemmodel_para,
           terms = c("paracenter", "Sampling"),
           swap.pred = T,
           type = "re",
           ci.level = 0.95)
predphloemeffect_para$group <- 
  factor(predphloemeffect_para$group, levels = c("B", "A"))
col <- 
  ifelse(poolcenter$Sampling == "B",
         "goldenrod4", 
         "goldenrod")
predphloemeffect_para$conf.low <- 
  predphloemeffect_para$conf.low +1
predphloemeffect_para$conf.high <- 
  predphloemeffect_para$conf.high +1
predphloemeffect_para$predicted <- 
  predphloemeffect_para$predicted +1
####Plot
predphloemplot_para <- 
  plot(predphloemeffect_para,
       ci = T) + 
  geom_point(data = poolcenter,
             mapping = aes(x = paracenter, y = jitter(phloem+1, 2)), 
             colour = col, 
             fill = col) +
  ggtitle("") + 
  xlab("Parasitoid abundance") +
  ylab("Phloem sucker abundance") +
  scale_y_continuous(trans = "log",
                     limits = c(0.6, 40),
                     breaks = c(1, 5, 20)) +
  scale_color_manual(name ="Sampling period",
                     labels = c("Before", "After"), 
                     values = c("goldenrod4", "goldenrod")) +
  theme(legend.position = c(0.8 ,0.9),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))




# Bromeliad-associated predators and honeydew producers -------------------
#Model
scaleaphidmodel_brompreds <- 
  glmer(scaleaphid ~
          bromcenter*Sampling +
          (1|Site/alltrees/quadrats),
        family= "poisson"(link = "log"),
        control=glmerControl(optimizer = "bobyqa"),
        data = poolcenter)
#Assumptions
simulationOutput <- 
  simulateResiduals(fittedModel = scaleaphidmodel_brompreds, 
                    n = 2000,
                    rank = T)
plot(simulationOutput,
     quantreg = F,
     rank = T)
#Summary and tests
summary(scaleaphidmodel_brompreds)
scaleaphidtest_brompreds <- 
  mixed(scaleaphid~ 
          bromcenter*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="log"),
        type = afex_options(type = "2"),
        control = glmerControl(optimizer = "bobyqa"),
        data = poolcenter,
        method = "LRT")$anova_table
#Plot
##visreg
visreg(scaleaphidmodel_brompreds,
       "bromcenter", by = "Sampling")
##ggeffects
###Data
scaleaphideffect_brompreds <- 
  ggeffect(scaleaphidmodel_brompreds,
           terms = c("bromcenter", "Sampling"),
           swap.pred = T,
           type = "re",
           ci.level = 0.95)
scaleaphideffect_brompreds$group <- 
  factor(scaleaphideffect_brompreds$group, levels = c("B", "A"))
col <- 
  ifelse(poolcenter$Sampling == "B",
         "goldenrod4", 
         "goldenrod")
scaleaphideffect_brompreds$conf.low <- 
  scaleaphideffect_brompreds$conf.low +1
scaleaphideffect_brompreds$conf.high <- 
  scaleaphideffect_brompreds$conf.high +1
scaleaphideffect_brompreds$predicted <- 
  scaleaphideffect_brompreds$predicted +1
####Plot
scaleaphidplot_brompreds <- 
  plot(scaleaphideffect_brompreds,
       ci = T) + 
  geom_point(data = poolcenter,
             mapping = aes(x = bromcenter, y = jitter(scaleaphid +1, 2)), 
             colour = col, 
             fill = col) +
  ggtitle("") + 
  xlab("Bromeliad-associated predator abundance") +
  ylab("Honeydew producer abundance") +
  scale_color_manual(name ="Sampling period",
                     labels = c("Before", "After"), 
                     values = c("goldenrod4", "goldenrod")) +
  scale_y_continuous(trans = "log",
                     breaks = c(1,3,7)) +
  theme(legend.position = c(0.8,0.9),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

# Models intraguild predation ---------------------------------------------------
#Bromeliad predators on parasitoids
##Model
intramodel_brompara <- 
  glmer(para ~
          bromcenter*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="log"),
        data = poolcenter)
##Assumptions
simulationOutput <- 
  simulateResiduals(fittedModel =intramodel_brompara, 
                    n = 2000,
                    rank = T)
plot(simulationOutput,
     quantreg = F,
     rank = T)
##Summary and tests
summary(intramodel_brompara)
intratest_brompara <- 
  mixed(para~ 
          bromcenter*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="log"),
        type = afex_options(type = "2"),
        data = poolcenter,
        method = "LRT")$anova_table
##Plot
###visreg
visreg(intramodel_brompara,
       "bromcenter", by = "Sampling")
###ggeffects
####Data
intraeffect_brompara <- 
  ggeffect(intramodel_brompara,
           terms = c("bromcenter", "Sampling"),
           swap.pred = T,
           type = "re",
           ci.level = 0.95)
intraeffect_brompara$group <- 
  factor(intraeffect_brompara$group, levels = c("B", "A"))
col <- 
  ifelse(poolcenter$Sampling == "B",
         "goldenrod4", 
         "goldenrod")
intraeffect_brompara$conf.low <- 
  intraeffect_brompara$conf.low +1
intraeffect_brompara$conf.high <- 
  intraeffect_brompara$conf.high +1
intraeffect_brompara$predicted <- 
  intraeffect_brompara$predicted +1
####Plot
intraplot_brompara <- 
  plot(intraeffect_brompara,
       ci = T) + 
  geom_point(data = poolcenter,
             mapping = aes(x = bromcenter, y = jitter(para +1, 2)), 
             colour = col, 
             fill = col) +
  ggtitle("") + 
  xlab("Bromeliad-associated predator abundance") +
  ylab("Parasitoid abundance") +
  scale_y_continuous(trans = "log",
                     limits = c(0.6, 30),
                     breaks = c(1, 5, 20)) +
  scale_color_manual(name = "Sampling period",
                     labels = c("Before", "After"), 
                     values = c("goldenrod4", "goldenrod")) +
  theme(legend.position = c(0.8 ,0.9),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))


#Bromeliad predators on aerial predators
##Model
intramodel_brommobipred <- 
  glmer(mobipred ~
          bromcenter*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="log"),
        data = poolcenter)
##Assumptions
simulationOutput <- 
  simulateResiduals(fittedModel =intramodel_brommobipred, 
                    n = 2000,
                    rank = T)
plot(simulationOutput,
     quantreg = F,
     rank = T)
##Summary and tests
summary(intramodel_brommobipred)
intratest_brommobipred <- 
  mixed(mobipred~ 
          bromcenter*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="log"),
        type = afex_options(type = "2"),
        data = poolcenter,
        method = "LRT")$anova_table
##Plot
###visreg
visreg(intramodel_brommobipred,
       "bromcenter", by = "Sampling")
###ggeffects
####Data
intraeffect_brommobipred <- 
  ggeffect(intramodel_brommobipred,
           terms = c("bromcenter", "Sampling"),
           swap.pred = T,
           type = "re",
           ci.level = 0.95)
intraeffect_brommobipred$group <- 
  factor(intraeffect_brommobipred$group, levels = c("B", "A"))
col <- 
  ifelse(poolcenter$Sampling == "B",
         "goldenrod4", 
         "goldenrod")
intraeffect_brommobipred$conf.low <- 
  intraeffect_brommobipred$conf.low +1
intraeffect_brommobipred$conf.high <- 
  intraeffect_brommobipred$conf.high +1
intraeffect_brommobipred$predicted <- 
  intraeffect_brommobipred$predicted +1
####Plot
intraplot_brommobipred <- 
  plot(intraeffect_brommobipred,
       ci = T) + 
  geom_point(data = poolcenter,
             mapping = aes(x = bromcenter, y = jitter(mobipred +1, 2)), 
             colour = col, 
             fill = col) +
  ggtitle("") + 
  xlab("Bromeliad-associated predator abundance") +
  ylab("Aerial predator abundance") +
  scale_y_continuous(trans = "log",
                     limits = c(0.6, 20),
                     breaks = c(1, 5, 15)) +
  scale_color_manual(name = "Sampling period",
                     labels = c("Before", "After"), 
                     values = c("goldenrod4", "goldenrod")) +
  theme(legend.position = c(0.8 ,0.9),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))


#Bromeliad predators on tree predators
##Model
intramodel_bromarbopred <- 
  glmer(arbopred~
          bromcenter*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="log"),
        control=glmerControl(optimizer = "bobyqa"),
        data = poolcenter)
##Assumptions
simulationOutput <- 
  simulateResiduals(fittedModel =intramodel_bromarbopred, 
                    n = 2000,
                    rank = T)
plot(simulationOutput,
     quantreg = F,
     rank = T)
##Summary and tests
summary(intramodel_bromarbopred)
intratest_bromarbopred <- 
  mixed(arbopred~ 
          bromcenter*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="log"),
        type = afex_options(type = "2"),
        data = poolcenter,
        method = "LRT")$anova_table
##Plot
###visreg
visreg(intramodel_bromarbopred,
       "bromcenter", by = "Sampling")
###ggeffects
####Data
intraeffect_bromarbopred <- 
  ggeffect(intramodel_bromarbopred,
           terms = c("bromcenter", "Sampling"),
           swap.pred = T,
           type = "re",
           ci.level = 0.95)
intraeffect_bromarbopred$group <- 
  factor(intraeffect_bromarbopred$group, levels = c("B", "A"))
col <- 
  ifelse(poolcenter$Sampling == "B",
         "goldenrod4", 
         "goldenrod")
intraeffect_bromarbopred$conf.low <- 
  intraeffect_bromarbopred$conf.low +1
intraeffect_bromarbopred$conf.high <- 
  intraeffect_bromarbopred$conf.high +1
intraeffect_bromarbopred$predicted <- 
  intraeffect_bromarbopred$predicted +1
####Plot
intraplot_bromarbopred <- 
  plot(intraeffect_bromarbopred,
       ci = T) + 
  geom_point(data = poolcenter,
             mapping = aes(x = bromcenter, y = jitter(arbopred +1, 2)), 
             colour = col, 
             fill = col) +
  ggtitle("") + 
  xlab("Bromeliad-associated predator abundance") +
  ylab("Tree-associated predator abundance") +
  scale_y_continuous(trans = "log",
                     limits = c(0.6, 100),
                     breaks = c(1,10,30)) +
  scale_color_manual(name = "Sampling period",
                     labels = c("Before", "After"), 
                     values = c("goldenrod4", "goldenrod")) +
  theme(legend.position = c(0.8 ,0.9),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

#Parasitoids on tree predators
##Model
intramodel_paraarbopred <- 
  glmer(arbopred ~
          paracenter*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="sqrt"),
        data = poolcenter)
##Assumptions
simulationOutput <- 
  simulateResiduals(fittedModel =intramodel_paraarbopred, 
                    n = 2000,
                    rank = T)
plot(simulationOutput,
     quantreg = F,
     rank = T)
##Summary and tests
summary(intramodel_paraarbopred)
intratest_paraarbopred <- 
  mixed(arbopred~ 
          paracenter*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="sqrt"),
        type = afex_options(type = "2"),
        data = poolcenter,
        method = "LRT")$anova_table
##Plot
###visreg
visreg(intramodel_paraarbopred,
       "paracenter", by = "Sampling")
###ggeffects
####Data
intraeffect_paraarbopred <- 
  ggeffect(intramodel_paraarbopred,
           terms = c("paracenter", "Sampling"),
           swap.pred = T,
           type = "re",
           ci.level = 0.95)
intraeffect_paraarbopred$group <- 
  factor(intraeffect_paraarbopred$group, levels = c("B", "A"))
col <- 
  ifelse(poolcenter$Sampling == "B",
         "goldenrod4", 
         "goldenrod")
intraeffect_paraarbopred$conf.low <- 
  intraeffect_paraarbopred$conf.low +1
intraeffect_paraarbopred$conf.high <- 
  intraeffect_paraarbopred$conf.high +1
intraeffect_paraarbopred$predicted <- 
  intraeffect_paraarbopred$predicted +1
####Plot
intraplot_paraarbopred <- 
  plot(intraeffect_paraarbopred,
       ci = T) + 
  geom_point(data = poolcenter,
             mapping = aes(x = paracenter, y = jitter(arbopred +1, 2)), 
             colour = col, 
             fill = col) +
  ggtitle("") + 
  xlab("Parasitoid abundance") +
  ylab("Tree-associated predator abundance") +
  scale_y_continuous(trans = "log",
                     limits = c(0.6, 100),
                     breaks = c(1,10,30)) +
  scale_color_manual(name = "Sampling period",
                     labels = c("Before", "After"), 
                     values = c("goldenrod4", "goldenrod")) +
  theme(legend.position = c(0.8 ,0.9),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

#Parasitoids on aerial predators
##Model
intramodel_paramobipred <- 
  glmer(mobipred ~
          paracenter*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="log"),
        data = poolcenter)
##Assumptions
simulationOutput <- 
  simulateResiduals(fittedModel =intramodel_paramobipred, 
                    n = 2000,
                    rank = T)
plot(simulationOutput,
     quantreg = F,
     rank = T)
##Summary and tests
summary(intramodel_paramobipred)
intratest_paramobipred <- 
  mixed(mobipred~ 
          paracenter*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="log"),
        type = afex_options(type = "2"),
        data = poolcenter,
        method = "LRT")$anova_table
##Plot
###visreg
visreg(intramodel_paramobipred,
       "paracenter", by = "Sampling")
###ggeffects
####Data
intraeffect_paramobipred <- 
  ggeffect(intramodel_paramobipred,
           terms = c("paracenter", "Sampling"),
           swap.pred = T,
           type = "re",
           ci.level = 0.95)
intraeffect_paramobipred$group <- 
  factor(intraeffect_paramobipred$group, levels = c("B", "A"))
col <- 
  ifelse(poolcenter$Sampling == "B",
         "goldenrod4", 
         "goldenrod")
intraeffect_paramobipred$conf.low <- 
  intraeffect_paramobipred$conf.low +1
intraeffect_paramobipred$conf.high <- 
  intraeffect_paramobipred$conf.high +1
intraeffect_paramobipred$predicted <- 
  intraeffect_paramobipred$predicted +1
####Plot
intraplot_paramobipred <- 
  plot(intraeffect_paramobipred,
       ci = T) + 
  geom_point(data = poolcenter,
             mapping = aes(x = paracenter, y = jitter(mobipred +1, 2)), 
             colour = col, 
             fill = col) +
  ggtitle("") + 
  xlab("Parasitoid abundance") +
  ylab("Aerial predator abundance") +
  scale_y_continuous(trans = "log",
                     limits = c(0.6, 20),
                     breaks = c(1, 5, 15)) +
  scale_color_manual(name = "Sampling period",
                     labels = c("Before", "After"), 
                     values = c("goldenrod4", "goldenrod")) +
  theme(legend.position = c(0.8 ,0.9),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))


#Aerial predators on tree predators
##Model
intramodel_mobiarbopred <- 
  glmer(arbopred ~
          mobicenter*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="log"),
        data = poolcenter)
##Assumptions
simulationOutput <- 
  simulateResiduals(fittedModel =intramodel_mobiarbopred, 
                    n = 2000,
                    rank = T)
plot(simulationOutput,
     quantreg = F,
     rank = T)
##Summary and tests
summary(intramodel_mobiarbopred)
intratest_mobiarbopred <- 
  mixed(mobipred~ 
          arbocenter*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="log"),
        type = afex_options(type = "2"),
        data = poolcenter,
        method = "LRT")$anova_table
##Plot
###visreg
visreg(intramodel_mobiarbopred,
       "mobicenter", by = "Sampling")
###ggeffects
####Data
intraeffect_mobiarbopred <- 
  ggeffect(intramodel_mobiarbopred,
           terms = c("mobicenter", "Sampling"),
           swap.pred = T,
           type = "re",
           ci.level = 0.95)
intraeffect_mobiarbopred$group <- 
  factor(intraeffect_mobiarbopred$group, levels = c("B", "A"))
col <- 
  ifelse(poolcenter$Sampling == "B",
         "goldenrod4", 
         "goldenrod")
intraeffect_mobiarbopred$conf.low <- 
  intraeffect_mobiarbopred$conf.low +1
intraeffect_mobiarbopred$conf.high <- 
  intraeffect_mobiarbopred$conf.high +1
intraeffect_mobiarbopred$predicted <- 
  intraeffect_mobiarbopred$predicted +1
####Plot
intraplot_mobiarbopred <- 
  plot(intraeffect_mobiarbopred,
       ci = T) + 
  geom_point(data = poolcenter,
             mapping = aes(x = mobicenter, y = jitter(arbopred +1, 2)), 
             colour = col, 
             fill = col) +
  ggtitle("") + 
  xlab("Aerial predator abundance") +
  ylab("Tree-associated predator abundance") +
  scale_y_continuous(trans = "log",
                     limits = c(0.6, 100),
                     breaks = c(1,10,30)) +
  scale_color_manual(name = "Sampling period",
                     labels = c("Before", "After"), 
                     values = c("goldenrod4", "goldenrod")) +
  theme(legend.position = c(0.8 ,0.9),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))


# Adonis intraguild predation---------------------------------------------
#On bromeliad-associated predators
##Tree-associated predators
intradonis_bromtree <- 
  adonis(spread_brompred[,9:13] ~
           arbocenter*Sampling,
         method = "bray",
         permutations = 2000,
         strata = spread_brompred$Site,
         data = spread_brompred)
##Aerial predators
intradonis_brommobipred <- 
  adonis(spread_brompred[,9:13] ~
           mobicenter*Sampling,
         method = "bray",
         permutations = 2000,
         strata = spread_brompred$Site,
         data = spread_brompred)
##Parasitoids
intradonis_brompara <- 
  adonis(spread_brompred[,9:13] ~
           paracenter*Sampling,
         method = "bray",
         permutations = 2000,
         strata = spread_brompred$Site,
         data = spread_brompred)

#On tree-associated predators
##Bromeliad-associated predators
intradonis_treebrom <- 
  adonis(spread_arbopred[,9:14] ~
           bromcenter*Sampling,
         method = "bray",
         permutations = 2000,
         strata = spread_arbopred$Site,
         data = spread_arbopred)
##Aerial predators
intradonis_treemobipred <- 
  adonis(spread_arbopred[,9:14] ~
           mobicenter*Sampling,
         method = "bray",
         permutations = 2000,
         strata = spread_arbopred$Site,
         data = spread_arbopred)
##Parasitoids
intradonis_treepara <- 
  adonis(spread_arbopred[,9:14] ~
           paracenter*Sampling,
         method = "bray",
         permutations = 2000,
         strata = spread_arbopred$Site,
         data = spread_arbopred)

#On aerial predators
##Tree-associated predators
intradonis_mobipredtree <- 
  adonis(spread_mobipred[,9:12] ~
           arbocenter*Sampling,
         method = "bray",
         permutations = 2000,
         strata = spread_mobipred$Site,
         data = spread_mobipred)
##Bromeliad-associated predators
intradonis_mobipredbrom <- 
  adonis(spread_mobipred[,9:12] ~
           bromcenter*Sampling,
         method = "bray",
         permutations = 2000,
         strata = spread_mobipred$Site,
         data = spread_mobipred)
##Parasitoids
intradonis_mobipredpara <- 
  adonis(spread_mobipred[,9:12] ~
           paracenter*Sampling,
         method = "bray",
         permutations = 2000,
         strata = spread_mobipred$Site,
         data = spread_mobipred)

