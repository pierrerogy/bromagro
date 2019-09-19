#Bromeliads on herbivores

# Models all herbivore abundance per quadrat ------------------------------------------
#Volume proximity index
##Model
herbmodel_largeleaf <- 
  glmer(herb ~
          largeleaf*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="log"),
        data = poolcenter)
##Assumptions
simulationOutput <- 
  simulateResiduals(fittedModel = herbmodel_largeleaf, 
                    n = 2000,
                    rank = T)
plot(simulationOutput,
     quantreg = F,
     rank = T)
##Summary and tests
summary(herbmodel_largeleaf)
herbtest_largeleaf <- 
  mixed(herb~ 
          largeleaf*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="log"),
        type = afex_options(type = "2"),
        data = poolcenter,
        method = "LRT")$anova_table
##Plot
###visreg
visreg(herbmodel_largeleaf,
       "largeleaf", by = "Sampling")
###ggeffects
####Data
herbeffect_largeleaf <- 
  ggeffect(herbmodel_largeleaf,
           terms = c("largeleaf", "Sampling"),
           swap.pred = T,
           type = "re",
           ci.level = 0.95)
herbeffect_largeleaf$group <- 
  factor(herbeffect_largeleaf$group, levels = c("B", "A"))
col <- 
  ifelse(poolcenter$Sampling == "B",
         "goldenrod4", 
         "goldenrod")
herbeffect_largeleaf$conf.low <- 
  herbeffect_largeleaf$conf.low +1
herbeffect_largeleaf$conf.high <- 
  herbeffect_largeleaf$conf.high +1
herbeffect_largeleaf$predicted <- 
  herbeffect_largeleaf$predicted +1
####Plot
herbplot_largeleaf <- 
  plot(herbeffect_largeleaf,
       ci = T) + 
  geom_point(data = poolcenter,
             mapping = aes(x = largeleaf, y = jitter(herb +1, 2)), 
             colour = col, 
             fill = col) +
  ggtitle("") + 
  xlab("Volume proximity index") +
  ylab("Herbivore abundance") +
  scale_color_manual(name ="Sampling period",
                     labels = c("Before", "After"), 
                     values = c("goldenrod4", "goldenrod")) +
  scale_y_continuous(trans = "log",
                     breaks = c(1,5,20)) +
  theme(legend.position = c(0.8,0.9),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

#Tree type - Full model
##Model
herbmodel_treetype <- 
  glmer(herb ~
          treetype*Sampling +
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="sqrt"),
        data = poolcenter)
##Assumptions
simulationOutput <- 
  simulateResiduals(fittedModel = herbmodel_treetype, 
                    n = 2000)
plot(simulationOutput, 
     asFactor = T,
     quantreg = F)
##Summary and tests
summary(herbmodel_treetype)
herbtest_treetype <- 
  mixed(herb~ 
          treetype*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="sqrt"),
        type = afex_options(type = "2"),
        data = poolcenter,
        method = "LRT")$anova_table
##Plot
###visreg
visreg(herbmodel_treetype,
       "Sampling", by = "treetype")
###ggeffects
####Data
herbeffect_treetype <- 
  ggeffect(herbmodel_treetype,
           type = "re",
           x.as.factor = T,
           terms = c("Sampling", "treetype"),
           ci.lvl = 0.95)
herbeffect_treetype$x <- 
  factor(herbeffect_treetype$x, levels = c("B", "A"))
herbeffect_treetype$group <- 
  factor(herbeffect_treetype$group, levels = c("wo", "w", "wr"))
####Plot
herbplot_treetype <- 
  ggplot(herbeffect_treetype, 
         aes(x=x, 
             y=predicted,
             group = group,
             colour = group)) + 
  geom_line(position = position_dodge(0.3),
            lwd = 2) + 
  geom_errorbar(aes(ymin=conf.low, 
                    ymax=conf.high), 
                width=0.1,
                lwd = 2,
                position = position_dodge(0.3)) +
  geom_point(position = position_dodge(0.3), 
             lwd =6) +
  ggtitle("") + 
  xlab("Sampling period") +
  scale_x_discrete(limit = c("B", "A"),
                   labels = c("Before", "After"),
                   expand = expand_scale(add = c(0.6)))+
  ylab("Herbivore abundance") +
  ylim(0.5,1.75)+
  scale_y_continuous(breaks = c(1,1.8,2.5))+
  scale_color_manual(name = "Tree type",
                     labels = c("Without", "With", "Removal"),
                     values = c("tomato4", "dodgerblue4", "black")) +
  theme(legend.position = c(0.9,0.9),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))

#Tree type - Contrast models
##Observational model
###Model
herbmodel_obs <- 
  glmer(herb ~
          treetype*Sampling +
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="sqrt"),
        data = poolcenter %>% 
          filter(treetype != "wr"))
###Assumptions
simulationOutput <- 
  simulateResiduals(fittedModel = herbmodel_obs, 
                    n = 2000)
plot(simulationOutput, 
     asFactor = T,
     quantreg = F)
###Summary and tests
summary(herbmodel_obs)
herbtest_obs <- 
  mixed(herb~ 
          treetype*Sampling +
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="sqrt"),
        type = afex_options(type = "2"),
        data = poolcenter%>% 
          filter(treetype != "wr"),
        method = "LRT")$anova_table
##Experimental model
###Model
herbmodel_exp <- 
  glmer(herb ~
          treetype*Sampling +
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="log"),
        data = poolcenter %>% 
          filter(treetype != "wo"))
###Assumptions
simulationOutput <- 
  simulateResiduals(fittedModel = herbmodel_exp, 
                    n = 2000)
plot(simulationOutput, 
     asFactor = T,
     quantreg = F)
###Summary and tests
summary(herbmodel_exp)
herbtest_exp <- 
  mixed(herb~ 
          treetype*Sampling +
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="log"),
        type = afex_options(type = "2"),
        data = poolcenter%>% 
          filter(treetype != "wo"),
        method = "LRT")$anova_table


# Models leaf chewer abundance per quadrat ------------------------------------------
#Volume proximity index
##Model
chewermodel_largeleaf <- 
  glmer(chewer ~
          largeleaf*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="log"),
        data = poolcenter)
##Assumptions
simulationOutput <- 
  simulateResiduals(fittedModel = chewermodel_largeleaf, 
                    n = 2000,
                    rank = T)
plot(simulationOutput,
     quantreg = F,
     rank = T)
##Summary and tests
summary(chewermodel_largeleaf)
chewertest_largeleaf <- 
  mixed(chewer~ 
          largeleaf*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="log"),
        type = afex_options(type = "2"),
        data = poolcenter,
        method = "LRT")$anova_table
##Plot
###visreg
visreg(chewermodel_largeleaf,
       "largeleaf", by = "Sampling")
###ggeffects
####Data
chewereffect_largeleaf <- 
  ggeffect(chewermodel_largeleaf,
           terms = c("largeleaf", "Sampling"),
           swap.pred = T,
           type = "re",
           ci.level = 0.95)
chewereffect_largeleaf$group <- 
  factor(chewereffect_largeleaf$group, levels = c("B", "A"))
col <- 
  ifelse(poolcenter$Sampling == "B",
         "goldenrod4", 
         "goldenrod")
chewereffect_largeleaf$conf.low <- 
  chewereffect_largeleaf$conf.low +1
chewereffect_largeleaf$conf.high <- 
  chewereffect_largeleaf$conf.high +1
chewereffect_largeleaf$predicted <- 
  chewereffect_largeleaf$predicted +1
####Plot
chewerplot_largeleaf <- 
  plot(chewereffect_largeleaf,
       ci = T) + 
  geom_point(data = poolcenter,
             mapping = aes(x = largeleaf, y = jitter(chewer +1, 2)), 
             colour = col, 
             fill = col) +
  ggtitle("") + 
  xlab("Bromeliad proximity index") +
  ylab("Leaf chewer abundance") +
  scale_color_manual(name ="Sampling period",
                     labels = c("Before", "After"), 
                     values = c("goldenrod4", "goldenrod")) +
  scale_y_continuous(trans = "log",
                     breaks = c(1,5,20)) +
  theme(legend.position = c(0.8,0.9),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

#Tree type - Full model
##Model
chewermodel_treetype <- 
  glmer(chewer ~
          treetype*Sampling +
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="log"),
        glmerControl(optimizer = "bobyqa"),
        data = poolcenter)
##Assumptions
simulationOutput <- 
  simulateResiduals(fittedModel = chewermodel_treetype, 
                    n = 2000)
plot(simulationOutput, 
     asFactor = T,
     quantreg = F)
##Summary and tests
summary(chewermodel_treetype)
chewertest_treetype <- 
  mixed(chewer~ 
          treetype*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="log"),
        type = afex_options(type = "2"),
        data = poolcenter,
        method = "LRT")$anova_table
##Plot
###visreg
visreg(chewermodel_treetype,
       "Sampling", by = "treetype")
###ggeffects
####Data
chewereffect_treetype <- 
  ggeffect(chewermodel_treetype,
           type = "re",
           x.as.factor = T,
           terms = c("Sampling", "treetype"),
           ci.lvl = 0.95)
chewereffect_treetype$x <- 
  factor(chewereffect_treetype$x, levels = c("B", "A"))
chewereffect_treetype$group <- 
  factor(chewereffect_treetype$group, levels = c("wo", "w", "wr"))
####Plot
chewerplot_treetype <- 
  ggplot(chewereffect_treetype, 
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
  xlab("Sampling period") +
  scale_x_discrete(limit = c("B", "A"),
                   labels = c("Before", "After"),
                   expand = expand_scale(add = c(0.6)))+
  ylab("Leaf chewer abundance") +
  ylim(0.5,1.75)+
  scale_y_continuous(breaks = c(1,1.5, 2, 2.5))+
  scale_color_manual(name = "Tree type",
                     labels = c("Without", "With", "Removal"),
                     values = c("tomato4", "dodgerblue4", "black")) +
  theme(legend.position = c(0.9,0.9),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))

#Tree type - Contrast models
##Observational model
###Model
chewermodel_obs <- 
  glmer(chewer ~
          treetype*Sampling +
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="log"),
        glmerControl(optimizer = "bobyqa"),
        data = poolcenter %>% 
          filter(treetype != "wr"))
###Assumptions
simulationOutput <- 
  simulateResiduals(fittedModel = chewermodel_obs, 
                    n = 2000)
plot(simulationOutput, 
     asFactor = T,
     quantreg = F)
###Summary and tests
summary(chewermodel_obs)
chewertest_obs <- 
  mixed(chewer~ 
          treetype*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="log"),
        type = afex_options(type = "2"),
        data = poolcenter%>% 
          filter(treetype != "wr"),
        method = "LRT")$anova_table
##Experimental model
###Model
chewermodel_exp <- 
  glmer(chewer ~
          treetype*Sampling +
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="log"),
        glmerControl(optimizer = "bobyqa"),
        data = poolcenter %>% 
          filter(treetype != "wo"))
###Assumptions
simulationOutput <- 
  simulateResiduals(fittedModel = chewermodel_exp, 
                    n = 2000)
plot(simulationOutput, 
     asFactor = T,
     quantreg = F)
###Summary and tests
summary(chewermodel_exp)
chewertest_exp <- 
  mixed(chewer~ 
          treetype*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="log"),
        type = afex_options(type = "2"),
        data = poolcenter%>% 
          filter(treetype != "wo"),
        method = "LRT")$anova_table




# Models phloem sucker abundance per quadrat ------------------------------------------
#Volume proximity index
##Model
phloemmodel_largeleaf <- 
  glmer(phloem ~
          largeleaf*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="log"),
        data = poolcenter)
##Assumptions
simulationOutput <- 
  simulateResiduals(fittedModel = phloemmodel_largeleaf, 
                    n = 2000,
                    rank = T)
plot(simulationOutput,
     quantreg = F,
     rank = T)
##Summary and tests
summary(phloemmodel_largeleaf)
phloemtest_largeleaf <- 
  mixed(phloem~ 
          largeleaf*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="log"),
        type = afex_options(type = "2"),
        data = poolcenter,
        method = "LRT")$anova_table
##Plot
###visreg
visreg(phloemmodel_largeleaf,
       "largeleaf", by = "Sampling")
###ggeffects
####Data
phloemeffect_largeleaf <- 
  ggeffect(phloemmodel_largeleaf,
           terms = c("largeleaf", "Sampling"),
           swap.pred = T,
           type = "re",
           ci.level = 0.95)
phloemeffect_largeleaf$group <- 
  factor(phloemeffect_largeleaf$group, levels = c("B", "A"))
col <- 
  ifelse(poolcenter$Sampling == "B",
         "goldenrod4", 
         "goldenrod")
phloemeffect_largeleaf$conf.low <- 
  phloemeffect_largeleaf$conf.low +1
phloemeffect_largeleaf$conf.high <- 
  phloemeffect_largeleaf$conf.high +1
phloemeffect_largeleaf$predicted <- 
  phloemeffect_largeleaf$predicted +1
####Plot
phloemplot_largeleaf <- 
  plot(phloemeffect_largeleaf,
       ci = T) + 
  geom_point(data = poolcenter,
             mapping = aes(x = largeleaf, y = jitter(phloem +1, 2)), 
             colour = col, 
             fill = col) +
  ggtitle("") + 
  xlab("Volume proximity index") +
  ylab("Leaf phloem abundance") +
  scale_color_manual(name ="Sampling period",
                     labels = c("Before", "After"), 
                     values = c("goldenrod4", "goldenrod")) +
  scale_y_continuous(trans = "log",
                     breaks = c(1,5,20)) +
  theme(legend.position = c(0.8,0.9),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

#Tree type - Full model
##Model
phloemmodel_treetype <- 
  glmer(phloem ~
          treetype*Sampling +
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="log"),
        data = poolcenter)
##Assumptions
simulationOutput <- 
  simulateResiduals(fittedModel = phloemmodel_treetype, 
                    n = 2000)
plot(simulationOutput, 
     asFactor = T,
     quantreg = F)
##Summary and tests
summary(phloemmodel_treetype)
phloemtest_treetype <- 
  mixed(phloem~ 
          treetype*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="log"),
        type = afex_options(type = "2"),
        data = poolcenter,
        method = "LRT")$anova_table
##Plot
###visreg
visreg(phloemmodel_treetype,
       "Sampling", by = "treetype")
###ggeffects
####Data
phloemeffect_treetype <- 
  ggeffect(phloemmodel_treetype,
           type = "re",
           x.as.factor = T,
           terms = c("Sampling", "treetype"),
           ci.lvl = 0.95)
phloemeffect_treetype$x <- 
  factor(phloemeffect_treetype$x, levels = c("B", "A"))
phloemeffect_treetype$group <- 
  factor(phloemeffect_treetype$group, levels = c("wo", "w", "wr"))
####Plot
phloemplot_treetype <- 
  ggplot(phloemeffect_treetype, 
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
  xlab("Sampling period") +
  scale_x_discrete(limit = c("B", "A"),
                   labels = c("Before", "After"),
                   expand = expand_scale(add = c(0.6)))+
  ylab("Leaf phloem abundance") +
  ylim(0.5,1.75)+
  scale_y_continuous(breaks = c(1,1.5, 2, 2.5))+
  scale_color_manual(name = "Tree type",
                     labels = c("Without", "With", "Removal"),
                     values = c("tomato4", "dodgerblue4", "black")) +
  theme(legend.position = c(0.9,0.9),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))

#Tree type - Contrast models
##Observational model
###Model
phloemmodel_obs <- 
  glmer(phloem ~
          treetype*Sampling +
          (1|Site/alltrees/quadrats),
        family ="poisson"(link = "log"),
        data = poolcenter %>% 
          filter(treetype != "wr"))
###Assumptions
simulationOutput <- 
  simulateResiduals(fittedModel = phloemmodel_obs, 
                    n = 2000)
plot(simulationOutput, 
     asFactor = T,
     quantreg = F)
###Summary and tests
summary(phloemmodel_obs)
phloemtest_obs <- 
  mixed(phloem~ 
          treetype*Sampling +
          (1|Site/alltrees/quadrats),
        family ="poisson"(link ="log"),
        type = afex_options(type = "2"),
        data = poolcenter %>% 
          filter(treetype != "wr"),
        method = "LRT")$anova_table
##Experimental model
###Model
phloemmodel_exp <- 
  glmer(phloem ~
          treetype*Sampling +
          (1|Site/alltrees/quadrats),
        family ="poisson"(link = "log"),
        data = poolcenter %>% 
          filter(treetype != "wo"))
###Assumptions
simulationOutput <- 
  simulateResiduals(fittedModel = phloemmodel_exp, 
                    n = 2000)
plot(simulationOutput, 
     asFactor = T,
     quantreg = F)
###Summary and tests
summary(phloemmodel_exp)
phloemtest_exp <- 
  mixed(phloem ~ 
          treetype*Sampling +
          (1|Site/alltrees/quadrats),
        family ="poisson"(link ="log"),
        type = afex_options(type = "2"),
        data = poolcenter %>% 
          filter(treetype != "wo"),
        method = "LRT")$anova_table

# Association between leaf chewers and phloem suckers ---------------------
#Model
phloemmodel_chewer <- 
  glmer(phloem ~
          chewercenter*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="log"),
        data = poolcenter)
##Assumptions
simulationOutput <- 
  simulateResiduals(fittedModel = phloemmodel_largeleaf, 
                    n = 2000,
                    rank = T)
plot(simulationOutput,
     quantreg = F,
     rank = T)
##Summary and tests
summary(phloemmodel_largeleaf)
phloemtest_chewer <- 
  mixed(phloem~ 
          chewercenter*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="log"),
        type = afex_options(type = "2"),
        data = poolcenter,
        method = "LRT")$anova_table
##Plot
###visreg
visreg(phloemmodel_chewer,
       "chewercenter", by = "Sampling")
###ggeffects
####Data
phloemeffect_chewer <- 
  ggeffect(phloemmodel_chewer,
           terms = c("chewercenter", "Sampling"),
           swap.pred = T,
           type = "re",
           ci.level = 0.95)
phloemeffect_chewer$group <- 
  factor(phloemeffect_chewer$group, levels = c("B", "A"))
col <- 
  ifelse(poolcenter$Sampling == "B",
         "goldenrod4", 
         "goldenrod")
phloemeffect_chewer$conf.low <- 
  phloemeffect_chewer$conf.low +1
phloemeffect_chewer$conf.high <- 
  phloemeffect_chewer$conf.high +1
phloemeffect_chewer$predicted <- 
  phloemeffect_chewer$predicted +1
####Plot
phloemplot_chewer <- 
  plot(phloemeffect_chewer,
       ci = T) + 
  geom_point(data = poolcenter,
             mapping = aes(x = chewercenter, y = jitter(phloem +1, 2)), 
             colour = col, 
             fill = col) +
  ggtitle("") + 
  xlab("Volume proximity index") +
  ylab("Leaf phloem abundance") +
  scale_color_manual(name ="Sampling period",
                     labels = c("Before", "After"), 
                     values = c("goldenrod4", "goldenrod")) +
  scale_y_continuous(trans = "log",
                     breaks = c(1,5,20)) +
  theme(legend.position = c(0.8,0.9),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))


# Models on kinds of herbivores -------------------------------------------
#Herbivorous beetles
##Volume proximity index
###Model
herbeetlemodel_largeleaf <- 
  glmer(herbeetle ~
          largeleaf*Sampling +
          (1|Site/alltrees/quadrats),
        family = "poisson"(link = "sqrt"),
        data = poolcenter)
###Assumptions
simulationOutput <- 
  simulateResiduals(fittedModel = herbeetlemodel_largeleaf, 
                    n = 2000,
                    rank = T)
plot(simulationOutput,
     quantreg = F,
     rank = T)
###Summary and tests
summary(herbeetlemodel_largeleaf)
herbeetletest_largeleaf <- 
  mixed(herbeetle~ 
          largeleaf*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="sqrt"),
        type = afex_options(type = "2"),
        data = poolcenter,
        method = "LRT")$anova_table
###Plot
####visreg
visreg(herbeetlemodel_largeleaf,
       "largeleaf", by = "Sampling")
###ggeffects
####Data
herbeetleeffect_largeleaf <- 
  ggeffect(herbeetlemodel_largeleaf,
           terms = c("largeleaf", "Sampling"),
           swap.pred = T,
           type = "re",
           ci.level = 0.95)
herbeetleeffect_largeleaf$group <- 
  factor(herbeetleeffect_largeleaf$group, levels = c("B", "A"))
col <- 
  ifelse(poolcenter$Sampling == "B",
         "goldenrod4", 
         "goldenrod")
herbeetleeffect_largeleaf$conf.low <- 
  herbeetleeffect_largeleaf$conf.low +1
herbeetleeffect_largeleaf$conf.high <- 
  herbeetleeffect_largeleaf$conf.high +1
herbeetleeffect_largeleaf$predicted <- 
  herbeetleeffect_largeleaf$predicted +1
####Plot
herbeetleplot_largeleaf <- 
  plot(herbeetleeffect_largeleaf,
       ci = T) + 
  geom_point(data = poolcenter,
             mapping = aes(x = largeleaf, y = jitter(herbeetle +1, 2)), 
             colour = col, 
             fill = col) +
  ggtitle("") + 
  xlab("Volume proximity index") +
  ylab("Herbivorous beetle abundance") +
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
##Tree type - Full model
###Model
herbeetlemodel_treetype <- 
  glmer.nb(herbeetle ~
             treetype*Sampling +
             (1|Site/alltrees/quadrats),
           control = glmerControl(optimizer = "bobyqa"),
           data = poolcenter)
###Assumptions
simulationOutput <- 
  simulateResiduals(fittedModel = herbeetlemodel_treetype, 
                    n = 2000)
plot(simulationOutput, 
     asFactor = T,
     quantreg = F)
###Summary and tests
summary(herbeetlemodel_treetype)
herbeetletest_treetype <- 
  mixed(herbeetle~ 
          treetype*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="negative.binomial"(theta = getME(herbeetlemodel_treetype,
                                                  "glmer.nb.theta")),
        control = glmerControl(optimizer = "bobyqa"),
        type = afex_options(type = "2"),
        data = poolcenter,
        method = "LRT")$anova_table
###Plot
####visreg
visreg(herbeetlemodel_treetype,
       "Sampling", by = "treetype")
####ggeffects
#####Data
herbeetleeffect_treetype <- 
  ggeffect(herbeetlemodel_treetype,
           type = "re",
           x.as.factor = T,
           terms = c("Sampling", "treetype"),
           ci.lvl = 0.95)
herbeetleeffect_treetype$x <- 
  factor(herbeetleeffect_treetype$x, levels = c("B", "A"))
herbeetleeffect_treetype$group <- 
  factor(herbeetleeffect_treetype$group, levels = c("wo", "w", "wr"))
#####Plot
herbeetleplot_treetype <- 
  ggplot(herbeetleeffect_treetype, 
         aes(x=x, 
             y=predicted, 
             colour=group)) + 
  geom_errorbar(aes(ymin=conf.low, 
                    ymax=conf.high), 
                width=0.1,
                lwd = 1,
                position = position_dodge(0.3)) +
  geom_point(position = position_dodge(0.3), 
             lwd =3) +
  ggtitle("") + 
  xlab("Sampling period") +
  scale_x_discrete(limit = c("B", "A"),
                   labels = c("Before", "After"),
                   expand = expand_scale(add = c(0.6)))+
  ylab("Herbivorous beetle abundance") +
  ylim(0.5,1.75)+
  scale_y_continuous(breaks = c(1,1.5, 2, 2.5))+
  scale_color_manual(name = "Tree type",
                     labels = c("Without", "With", "Removal"),
                     values = c("tomato4", "dodgerblue4", "black")) +
  theme(legend.position = "none",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))
##Tree type - Contrast models
###Observational model
####Model
herbeetlemodel_obs <- 
  glmer(herbeetle ~
          treetype*Sampling +
          (1|Site/alltrees/quadrats),
        family = "poisson"(link = "log"),
        data = poolcenter %>% 
          filter(treetype != "wr"))
####Assumptions
simulationOutput <- 
  simulateResiduals(fittedModel = herbeetlemodel_obs, 
                    n = 2000)
plot(simulationOutput, 
     asFactor = T,
     quantreg = F)
####Summary and tests
summary(herbeetlemodel_obs)
herbeetletest_obs <- 
  mixed(herbeetle~ 
          treetype*Sampling +
          (1|Site/alltrees/quadrats),
        family ="poisson"(link = "log"),
        type = afex_options(type = "2"),
        data = poolcenter%>% 
          filter(treetype != "wr"),
        method = "LRT")$anova_table
###Experimental model
####Model
herbeetlemodel_exp <- 
  glmer(herbeetle ~
             treetype*Sampling +
             (1|Site/alltrees/quadrats),
           family = "poisson"(link = "log"),
           data = poolcenter %>% 
          filter(treetype != "wo"))
####Assumptions
simulationOutput <- 
  simulateResiduals(fittedModel = herbeetlemodel_exp, 
                    n = 2000)
plot(simulationOutput, 
     asFactor = T,
     quantreg = F)
####Summary and tests
summary(herbeetlemodel_exp)
herbeetletest_exp <- 
  mixed(herbeetle~ 
          treetype*Sampling +
          (1|Site/alltrees/quadrats),
        family ="poisson"(link = "log"),
        type = afex_options(type = "2"),
        data = poolcenter%>% 
          filter(treetype != "wo"),
        method = "LRT")$anova_table


#Hoppers
##Volume proximity index
###Model
jumpmodel_largeleaf <- 
  glmer(jump ~
          largeleaf*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="log"),
        data = poolcenter)
###Assumptions
simulationOutput <- 
  simulateResiduals(fittedModel = jumpmodel_largeleaf, 
                    n = 2000,
                    rank = T)
plot(simulationOutput,
     quantreg = F,
     rank = T)
###Summary and tests
summary(jumpmodel_largeleaf)
jumptest_largeleaf <- 
  mixed(jump~ 
          largeleaf*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="log"),
        type = afex_options(type = "2"),
        data = poolcenter,
        method = "LRT")$anova_table
###Plot
####visreg
visreg(jumpmodel_largeleaf,
       "largeleaf", by = "Sampling")
####ggeffects
#####Data
jumpeffect_largeleaf <- 
  ggeffect(jumpmodel_largeleaf,
           terms = c("largeleaf", "Sampling"),
           swap.pred = T,
           type = "re",
           ci.level = 0.95)
jumpeffect_largeleaf$group <- 
  factor(jumpeffect_largeleaf$group, levels = c("B", "A"))
col <- 
  ifelse(poolcenter$Sampling == "B",
         "goldenrod4", 
         "goldenrod")
jumpeffect_largeleaf$conf.low <- 
  jumpeffect_largeleaf$conf.low +1
jumpeffect_largeleaf$conf.high <- 
  jumpeffect_largeleaf$conf.high +1
jumpeffect_largeleaf$predicted <- 
  jumpeffect_largeleaf$predicted +1
#####Plot
jumpplot_largeleaf <- 
  plot(jumpeffect_largeleaf,
       ci = T) + 
  geom_point(data = poolcenter,
             mapping = aes(x = largeleaf, y = jitter(jump +1, 2)), 
             colour = col, 
             fill = col) +
  ggtitle("") + 
  xlab("Volume proximity index") +
  ylab("Hopper abundance") +
  scale_color_manual(name ="Sampling period",
                     labels = c("Before", "After"), 
                     values = c("goldenrod4", "goldenrod")) +
  scale_y_continuous(trans = "log",
                     breaks = c(1,3,7)) +
  theme(legend.position = "none",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))
##Tree type - Full model
###Model
jumpmodel_treetype <- 
  glmer(jump ~
          treetype*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="log"),
        data = poolcenter)
###Assumptions
simulationOutput <- 
  simulateResiduals(fittedModel = jumpmodel_treetype, 
                    n = 2000)
plot(simulationOutput,
     asFactor = T,
     quantreg = F)
###Summary and tests
summary(jumpmodel_treetype)
jumptest_treetype <- 
  mixed(jump~ 
          treetype*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="log"),
        type = afex_options(type = "2"),
        data = poolcenter,
        method = "LRT")$anova_table
###Plot
####visreg
visreg(jumpmodel_treetype,
       "Sampling", by = "treetype")
####ggeffects
#####Data
jumpeffect_treetype <- 
  ggeffect(jumpmodel_treetype,
           type = "re",
           x.as.factor = T,
           terms = c("Sampling", "treetype"),
           ci.lvl = 0.95)
jumpeffect_treetype$x <- 
  factor(jumpeffect_treetype$x, levels = c("B", "A"))
jumpeffect_treetype$group <- 
  factor(jumpeffect_treetype$group, levels = c("wo", "w", "wr"))
#####Plot
jumpplot_treetype <- 
  ggplot(jumpeffect_treetype, 
         aes(x=x, 
             y=predicted, 
             colour=group)) + 
  geom_errorbar(aes(ymin=conf.low, 
                    ymax=conf.high), 
                width=0.1,
                lwd = 1,
                position = position_dodge(0.3)) +
  geom_point(position = position_dodge(0.3), 
             lwd =3) +
  ggtitle("") + 
  xlab("Sampling period") +
  scale_x_discrete(limit = c("B", "A"),
                   labels = c("Before", "After"),
                   expand = expand_scale(add = c(0.6)))+
  scale_y_continuous(breaks = c(0.3,0.6)) +
  ylab("Hopper abundance") +
  scale_color_manual(name = "Tree type",
                     labels = c("Without", "With", "Removal"),
                     values = c("tomato4", "dodgerblue4", "black")) +
  theme(legend.position = "none",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))
##Tree type - Contrast models
###Observational model
####Model
jumpmodel_obs <- 
  glmer(jump ~
          treetype*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link = "sqrt"),
        data = poolcenter %>% 
          filter(treetype != "wr"))
####Assumptions
simulationOutput <- 
  simulateResiduals(fittedModel = jumpmodel_obs, 
                    n = 2000)
plot(simulationOutput,
     asFactor = T,
     quantreg = F)
####Summary and tests
summary(jumpmodel_obs)
jumptest_obs <- 
  mixed(jump~ 
          treetype*Sampling +
          (1|Site/alltrees/quadrats),
        family ="poisson"(link = "sqrt"),
        type = afex_options(type = "2"),
        data = poolcenter %>% 
          filter(treetype != "wr"),
        method = "LRT")$anova_table
###Experimental model
####Model
jumpmodel_exp <- 
  glmer(jump ~
          treetype*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link = "sqrt"),
        data = poolcenter %>% 
          filter(treetype != "wo"))
####Assumptions
simulationOutput <- 
  simulateResiduals(fittedModel = jumpmodel_exp, 
                    n = 2000)
plot(simulationOutput,
     asFactor = T,
     quantreg = F)
####Summary and tests
summary(jumpmodel_exp)
jumptest_exp <- 
  mixed(jump~ 
          treetype*Sampling +
          (1|Site/alltrees/quadrats),
        family ="poisson"(link = "sqrt"),
        type = afex_options(type = "2"),
        data = poolcenter %>% 
          filter(treetype != "wo"),
        method = "LRT")$anova_table

#Psyllids
##Volume proximity index
###Model
psyllidmodel_largeleaf <- 
  glmer(psyllid ~
          largeleaf*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="log"),
        data = poolcenter)
###Assumptions
simulationOutput <- 
  simulateResiduals(fittedModel = psyllidmodel_largeleaf, 
                    n = 2000,
                    rank = T)
plot(simulationOutput,
     quantreg = F,
     rank = T)
###Summary and results
summary(psyllidmodel_largeleaf)
psyllidtest_largeleaf <- 
  mixed(psyllid~ 
          largeleaf*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="log"),
        type = afex_options(type = "2"),
        data = poolcenter,
        method = "LRT")$anova_table
###Plot
####visreg
visreg(psyllidmodel_largeleaf,
       "largeleaf", by = "Sampling")
###ggeffects
####Data
psyllideffect_largeleaf <- 
  ggeffect(psyllidmodel_largeleaf,
           terms = c("largeleaf", "Sampling"),
           swap.pred = T,
           type = "re",
           ci.level = 0.95)
psyllideffect_largeleaf$group <- 
  factor(psyllideffect_largeleaf$group, levels = c("B", "A"))
col <- 
  ifelse(poolcenter$Sampling == "B",
         "goldenrod4", 
         "goldenrod")
psyllideffect_largeleaf$conf.low <- 
  psyllideffect_largeleaf$conf.low +1
psyllideffect_largeleaf$conf.high <- 
  psyllideffect_largeleaf$conf.high +1
psyllideffect_largeleaf$predicted <- 
  psyllideffect_largeleaf$predicted +1
####Plot
psyllidplot_largeleaf <- 
  plot(psyllideffect_largeleaf,
       ci = T) + 
  geom_point(data = poolcenter,
             mapping = aes(x = largeleaf, y = jitter(psyllid +1, 2)), 
             colour = col, 
             fill = col) +
  ggtitle("") + 
  xlab("Volume proximity index") +
  ylab("Psyllid abundance") +
  scale_color_manual(name ="Sampling period",
                     labels = c("Before", "After"), 
                     values = c("goldenrod4", "goldenrod")) +
  scale_y_continuous(trans = "log",
                     breaks = c(1,3,7)) +
  theme(legend.position = "none",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))
##Tree type - Full model
###Model
psyllidmodel_treetype <- 
  glmer(psyllid ~
          treetype*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="log"),
        control =glmerControl(optimizer = "bobyqa"),
        data = poolcenter)
###Assumptions
simulationOutput <- 
  simulateResiduals(fittedModel = psyllidmodel_treetype, 
                    n = 2000)
plot(simulationOutput, 
     asFactor = T,
     quantreg = F)
###Sumamry and tests
summary(psyllidmodel_treetype)
psyllidtest_treetype <- 
  mixed(psyllid~ 
          treetype*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="log"),
        type = afex_options(type = "2"),
        control =glmerControl(optimizer = "bobyqa"),
        data = poolcenter,
        method = "LRT")$anova_table
###Plot
####visreg
visreg(psyllidmodel_treetype,
       "Sampling", by = "treetype")
####ggeffects
#####Data
psyllideffect_treetype <- 
  ggeffect(psyllidmodel_treetype,
           type = "re",
           x.as.factor = T,
           terms = c("Sampling", "treetype"),
           ci.lvl = 0.95)
psyllideffect_treetype$x <- 
  factor(psyllideffect_treetype$x, levels = c("B", "A"))
psyllideffect_treetype$group <- 
  factor(psyllideffect_treetype$group, levels = c("wo", "w", "wr"))
#####Plot
psyllidplot_treetype <- 
  ggplot(psyllideffect_treetype, 
         aes(x=x, 
             y=predicted, 
             colour=group)) + 
  geom_errorbar(aes(ymin=conf.low, 
                    ymax=conf.high), 
                width=0.1,
                lwd = 1,
                position = position_dodge(0.3)) +
  geom_point(position = position_dodge(0.3), 
             lwd =3) +
  ggtitle("") + 
  xlab("Sampling period") +
  scale_x_discrete(limit = c("B", "A"),
                   labels = c("Before", "After"),
                   expand = expand_scale(add = c(0.6)))+
  ylab("Psyllid abundance") +
  scale_color_manual(name = "Tree type",
                     labels = c("Without", "With", "Removal"),
                     values = c("tomato4", "dodgerblue4", "black")) +
  theme(legend.position = "none",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))
##Tree type - Contrast models
###Observational model
####Model
psyllidmodel_obs <- 
  glmer(psyllid ~
          treetype*Sampling +
          (1|Site/alltrees/quadrats),
        family = "poisson"(link = "log"),
        control = glmerControl(optimizer = "bobyqa"),
        data = poolcenter %>% 
          filter(treetype != "wr"))
####Assumptions
simulationOutput <- 
  simulateResiduals(fittedModel = psyllidmodel_obs, 
                    n = 2000)
plot(simulationOutput, 
     asFactor = T,
     quantreg = F)
####Sumamry and tests
summary(psyllidmodel_obs)
psyllidtest_obs <- 
  mixed(psyllid~ 
          treetype*Sampling +
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="log"),
        type = afex_options(type = "2"),
        control =glmerControl(optimizer = "bobyqa"),
        data = poolcenter%>% 
          filter(treetype != "wr"),
        method = "LRT")$anova_table
###Experimental model
####Model
psyllidmodel_exp <- 
  glmer(psyllid ~
          treetype*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="log"),
        data = poolcenter %>% 
          filter(treetype != "wo"))
####Assumptions
simulationOutput <- 
  simulateResiduals(fittedModel = psyllidmodel_exp, 
                    n = 2000)
plot(simulationOutput, 
     asFactor = T,
     quantreg = F)
####Sumamry and tests
summary(psyllidmodel_exp)
psyllidtest_exp <- 
  mixed(psyllid~ 
          treetype*Sampling +
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="log"),
        type = afex_options(type = "2"),
        data = poolcenter%>% 
          filter(treetype != "wo"),
        method = "LRT")$anova_table

#Honeydew producers
##Volume proximity index
###Model
scaleaphidmodel_largeleaf <- 
  glmer(scaleaphid ~
          largeleaf*Sampling +
          (1|Site/alltrees/quadrats),
        family = "poisson"(link = "sqrt"),
        data = poolcenter)
###Assumptions
simulationOutput <- 
  simulateResiduals(fittedModel = scaleaphidmodel_largeleaf, 
                    n = 2000,
                    rank = T)
plot(simulationOutput,
     quantreg = F,
     rank = T)
###Summary and tests
summary(scaleaphidmodel_largeleaf)
scaleaphidtest_largeleaf <- 
  mixed(scaleaphid~ 
          largeleaf*Sampling + 
          (1|Site/alltrees/quadrats),
        family = "poisson"(link = "sqrt"),
        type = afex_options(type = "2"),
        data = poolcenter,
        method = "LRT")$anova_table
###Plot
####visreg
visreg(scaleaphidmodel_largeleaf,
       "largeleaf", by = "Sampling")
####ggeffects
#####Data
scaleaphideffect_largeleaf <- 
  ggeffect(scaleaphidmodel_largeleaf,
           terms = c("largeleaf", "Sampling"),
           swap.pred = T,
           type = "re",
           ci.level = 0.95)
scaleaphideffect_largeleaf$group <- 
  factor(scaleaphideffect_largeleaf$group, levels = c("B", "A"))
col <- 
  ifelse(poolcenter$Sampling == "B",
         "goldenrod4", 
         "goldenrod")
scaleaphideffect_largeleaf$conf.low <- 
  scaleaphideffect_largeleaf$conf.low +1
scaleaphideffect_largeleaf$conf.high <- 
  scaleaphideffect_largeleaf$conf.high +1
scaleaphideffect_largeleaf$predicted <- 
  scaleaphideffect_largeleaf$predicted +1
#####Plot
scaleaphidplot_largeleaf <- 
  plot(scaleaphideffect_largeleaf,
       ci = T) + 
  geom_point(data = poolcenter,
             mapping = aes(x = largeleaf, y = jitter(scaleaphid +1, 2)), 
             colour = col, 
             fill = col) +
  ggtitle("") + 
  xlab("Volume proximity index") +
  ylab("Honeydew producer abundance") +
  scale_color_manual(labels = c("Before", "After"), 
                     values = c("goldenrod4", "goldenrod")) +
  scale_y_continuous(trans = "log",
                     breaks = c(1,3,7)) +
  theme(legend.position = "none",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))
##Tree type - Full model
###Model
scaleaphidmodel_treetype <- 
  glmer(scaleaphid ~
          treetype*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="log"),
        control = glmerControl(optimizer = "bobyqa"),
        data = poolcenter)
###Assumptions
simulationOutput <- 
  simulateResiduals(fittedModel = scaleaphidmodel_treetype, 
                    n = 2000)
plot(simulationOutput, 
     asFactor = T,
     quantreg = F)
###Summary and tests
summary(scaleaphidmodel_treetype)
scaleaphidtest_treetype <- 
  mixed(scaleaphid~ 
          treetype*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="log"),
        control = glmerControl(optimizer = "bobyqa"),
        type = afex_options(type = "2"),
        data = poolcenter,
        method = "LRT")$anova_table
###plot
####visreg
visreg(scaleaphidmodel_treetype,
       "Sampling", by = "treetype")
####ggeffects
#####Data
scaleaphideffect_treetype <- 
  ggeffect(scaleaphidmodel_treetype,
           type = "re",
           x.as.factor = T,
           terms = c("Sampling", "treetype"),
           ci.lvl = 0.95)
scaleaphideffect_treetype$x <- 
  factor(scaleaphideffect_treetype$x, levels = c("B", "A"))
scaleaphideffect_treetype$group <- 
  factor(scaleaphideffect_treetype$group, levels = c("wo", "w", "wr"))
#####Plot
scaleaphidplot_treetype <- 
  ggplot(scaleaphideffect_treetype, 
         aes(x=x, 
             y=predicted, 
             colour=group)) + 
  geom_errorbar(aes(ymin=conf.low, 
                    ymax=conf.high), 
                width=0.1,
                lwd = 1,
                position = position_dodge(0.3)) +
  geom_point(position = position_dodge(0.3), 
             lwd =3) +
  ggtitle("") + 
  xlab("Sampling period") +
  scale_x_discrete(limit = c("B", "A"),
                   labels = c("Before", "After"),
                   expand = expand_scale(add = c(0.6)))+
  ylab("Honeydew producer abundance") +
  scale_color_manual(name = "Tree type",
                     labels = c("Without", "With", "Removal"),
                     values = c("tomato4", "dodgerblue4", "black")) +
  theme(legend.position = "none",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))
##Tree type - Contrast models
###Observational model
####Model
scaleaphidmodel_obs <- 
  glmer(scaleaphid ~
          treetype*Sampling + 
          (1|Site/alltrees/quadrats),
        family = "poisson"(link = "log"),
        data = poolcenter %>% 
          filter(treetype != "wr"))
####Assumptions
simulationOutput <- 
  simulateResiduals(fittedModel = scaleaphidmodel_obs, 
                    n = 2000)
plot(simulationOutput, 
     asFactor = T,
     quantreg = F)
####Summary and tests
summary(scaleaphidmodel_obs)
scaleaphidtest_obs <- 
  mixed(scaleaphid~ 
          treetype*Sampling +
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="log"),
        control = glmerControl(optimizer = "bobyqa"),
        type = afex_options(type = "2"),
        data = poolcenter %>% 
          filter(treetype != "wr"),
        method = "LRT")$anova_table
###Experimental model
####Model
scaleaphidmodel_exp <- 
  glmer(scaleaphid ~
          treetype*Sampling + 
          (1|Site/alltrees/quadrats),
        family = "poisson"(link = "log"),
        control = glmerControl(optimizer = "bobyqa"),
        data = poolcenter %>% 
          filter(treetype != "wo"))
####Assumptions
simulationOutput <- 
  simulateResiduals(fittedModel = scaleaphidmodel_exp, 
                    n = 2000)
plot(simulationOutput, 
     asFactor = T,
     quantreg = F)
####Summary and tests
summary(scaleaphidmodel_exp)
scaleaphidtest_exp <- 
  mixed(scaleaphid~ 
          treetype*Sampling +
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="log"),
        control = glmerControl(optimizer = "bobyqa"),
        type = afex_options(type = "2"),
        data = poolcenter %>% 
          filter(treetype != "wo"),
        method = "LRT")$anova_table

#Snails
##Volume proximity index
###Model
snailmodel_largeleaf <- 
  glmer(snail ~
          largeleaf*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="log"),
        control = glmerControl(optimizer = "bobyqa"),
        data = poolcenter)
###Assumptions
simulationOutput <- 
  simulateResiduals(fittedModel = snailmodel_largeleaf, 
                    n = 2000,
                    rank = T)
plot(simulationOutput, 
     asFactor = F,
     quantreg = F)
###Summary and tests
summary(snailmodel_largeleaf)
snailtest_largeleaf <- 
  mixed(snail~ 
          largeleaf*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="log"),
        control = glmerControl(optimizer = "bobyqa"),
        type = afex_options(type = "2"),
        data = poolcenter,
        method = "LRT")$anova_table
###plot
####visreg
visreg(snailmodel_largeleaf,
       "largeleaf", by = "Sampling")
####ggeffects
#####Data
snaileffect_largeleaf <- 
  ggeffect(snailmodel_largeleaf,
           terms = c("largeleaf", "Sampling"),
           swap.pred = T,
           type = "re",
           ci.level = 0.95)
snaileffect_largeleaf$group <- 
  factor(snaileffect_largeleaf$group, levels = c("B", "A"))
col <- 
  ifelse(poolcenter$Sampling == "B",
         "goldenrod4", 
         "goldenrod")
snaileffect_largeleaf$conf.low <- 
  snaileffect_largeleaf$conf.low +1
snaileffect_largeleaf$conf.high <- 
  snaileffect_largeleaf$conf.high +1
snaileffect_largeleaf$predicted <- 
  snaileffect_largeleaf$predicted +1
#####Plot
snailplot_largeleaf <- 
  plot(snaileffect_largeleaf,
       ci = T) + 
  geom_point(data = poolcenter,
             mapping = aes(x = largeleaf, y = jitter(snail +1, 2)), 
             colour = col, 
             fill = col) +
  ggtitle("") + 
  xlab("Volume proximity index") +
  ylab("Honeydew producer abundance") +
  scale_color_manual(labels = c("Before", "After"), 
                     values = c("goldenrod4", "goldenrod")) +
  scale_y_continuous(trans = "log",
                     breaks = c(1,3,7)) +
  theme(legend.position = "none",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))
##Tree type - Full model
###Model
snailmodel_treetype <- 
  glmer(round(sqrt(snail)) ~
          treetype*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="log"),
        control = glmerControl(optimizer = "bobyqa"),
        data = poolcenter)
###Assumptions
simulationOutput <- 
  simulateResiduals(fittedModel = snailmodel_treetype, 
                    n = 2000)
plot(simulationOutput, 
     asFactor = T,
     quantreg = F)
###Summary and tests
summary(snailmodel_treetype)
snailtest_treetype <- 
  mixed(round(sqrt(snail))~ 
          treetype*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="log"),
        type = afex_options(type = "2"),
        control = glmerControl(optimizer = "bobyqa"),
        data = poolcenter,
        method = "LRT")$anova_table
###Plot
####visreg
visreg(snailmodel_treetype,
       "treetype", by = "Sampling")
####ggeffects
#####Data
snaileffect_treetype <- 
  ggeffect(snailmodel_treetype,
           type = "re",
           x.as.factor = T,
           terms = c("Sampling", "treetype"),
           ci.lvl = 0.95)
snaileffect_treetype$x <- 
  factor(snaileffect_treetype$x, levels = c("B", "A"))
snaileffect_treetype$group <- 
  factor(snaileffect_treetype$group, levels = c("wo", "w", "wr"))
#####Plot
snailplot_treetype <- 
  ggplot(snaileffect_treetype, 
         aes(x=x, 
             y=predicted, 
             colour=group)) + 
  geom_errorbar(aes(ymin=conf.low, 
                    ymax=conf.high), 
                width=0.1,
                lwd = 1,
                position = position_dodge(0.3)) +
  geom_point(position = position_dodge(0.3), 
             lwd =3) +
  ggtitle("") + 
  xlab("Sampling period") +
  scale_x_discrete(limit = c("B", "A"),
                   labels = c("Before", "After"),
                   expand = expand_scale(add = c(0.6)))+
  scale_color_manual(name = "Tree type",
                     labels = c("Without", "With", "Removal"),
                     values = c("tomato4", "dodgerblue4", "black")) +
  theme(legend.position = "none",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))
##Tree type - Contrast models
###Observational model
####Model
snailmodel_obs <- 
  glmer(round(sqrt(snail)) ~
          treetype*Sampling + 
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="log"),
        control = glmerControl(optimizer = "bobyqa"),
        data = poolcenter %>% 
          filter(treetype != "wr"))
###Assumptions
simulationOutput <- 
  simulateResiduals(fittedModel = snailmodel_obs, 
                    n = 2000)
plot(simulationOutput, 
     asFactor = T,
     quantreg = F)
####Summary and tests
summary(snailmodel_obs)
snailtest_obs <- 
  mixed(round(sqrt(snail))~ 
          treetype*Sampling +
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="log"),
        type = afex_options(type = "2"),
        control = glmerControl(optimizer = "bobyqa"),
        data = poolcenter%>% 
          filter(treetype != "wr"),
        method = "LRT")$anova_table
###Experimental model
####Model
snailmodel_exp <- 
  glmer(round(sqrt(snail)) ~
          treetype*Sampling + 
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="log"),
        control = glmerControl(optimizer = "bobyqa"),
        data = poolcenter %>% 
          filter(treetype != "wo"))
###Assumptions
simulationOutput <- 
  simulateResiduals(fittedModel = snailmodel_exp, 
                    n = 2000)
plot(simulationOutput, 
     asFactor = T,
     quantreg = F)
####Summary and tests
summary(snailmodel_exp)
snailtest_exp <- 
  mixed(round(sqrt(snail))~ 
          treetype*Sampling +
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="log"),
        type = afex_options(type = "2"),
        control = glmerControl(optimizer = "bobyqa"),
        data = poolcenter%>% 
          filter(treetype != "wo"),
        method = "LRT")$anova_table




# Adonis all kinds per quadrat -------------------------------
#Volume proximity index
herbadonis_largeleaf <- 
  adonis(spread_herb[,11:17] ~
           largeleaf*Sampling,
         method = "bray",
         permutations = 2000,
         strata = spread_herb$Site,
         data = spread_herb)

#Tree type - Full model
herbadonis_treetype <- 
  adonis(spread_herb[,11:17] ~
           treetype*Sampling,
         method = "bray",
         permutations = 2000,
         strata = spread_herb$Site,
         data = spread_herb)

#Tree type - Contrast models
##Observational contrast
herbadonis_obs <- 
  adonis(spread_herb[which(spread_herb$treetype != "wr" &
                                 rowSums(spread_herb[,11:17]) != 0),11:17] ~
           treetype*Sampling,
         method = "bray",
         permutations = 2000,
         strata = spread_herb$Site[which(spread_herb$treetype != "wr")],
         data = spread_herb %>% 
           filter(treetype != "wr"))
##Observational contrast
herbadonis_exp <- 
  adonis(spread_herb[which(spread_herb$treetype != "wo" &
                             rowSums(spread_herb[,11:17]) != 0),11:17] ~
           treetype*Sampling,
         method = "bray",
         permutations = 2000,
         strata = spread_herb$Site[which(spread_herb$treetype != "wo")],
         data = spread_herb %>% 
           filter(treetype != "wo"))


