#Bromeliads on herbivores

# Models all herbivore abundance per quadrat ------------------------------------------
#Volume proximity index
herbmodel_largeleaf <- 
  glmer(herb ~
          largeleaf*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="log"),
        data = predcenter)
simulationOutput <- 
  simulateResiduals(fittedModel = herbmodel_largeleaf, 
                    n = 2000,
                    rank = T)
plotSimulatedResiduals(simulationOutput = simulationOutput,
                       quantreg = F,
                       rank = T)
summary(herbmodel_largeleaf)
herbtest_largeleaf <- 
  mixed(herb~ 
          largeleaf*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="log"),
        type = afex_options(type = "2"),
        data = poolcenter,
        method = "LRT")$anova_table
##plot
###visreg
visreg(herbmodel_largeleaf,
       "largeleaf", by = "Sampling")
###ggeffect
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
         "darkorange2", 
         "dodgerblue4")
herbeffect_largeleaf$conf.low <- 
  herbeffect_largeleaf$conf.low +1
herbeffect_largeleaf$conf.high <- 
  herbeffect_largeleaf$conf.high +1
herbeffect_largeleaf$predicted <- 
  herbeffect_largeleaf$predicted +1
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
  scale_color_manual(labels = c("Before", "After"), 
                     values = c("darkorange2", "dodgerblue4")) +
  scale_y_continuous(trans = "log",
                     breaks = c(1,5,20)) +
  theme(legend.position = "none",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

#Treatment
herbmodel_treatment <- 
  glmer(herb ~
          Treatment*Sampling +
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="sqrt"),
        data = poolcenter)
simulationOutput <- 
  simulateResiduals(fittedModel = herbmodel_treatment, 
                    n = 2000)
plotSimulatedResiduals(simulationOutput = simulationOutput, 
                       asFactor = T,
                       quantreg = F)
summary(herbmodel_treatment)
herbtest_treatment <- 
  mixed(herb~ 
          Treatment*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="sqrt"),
        type = afex_options(type = "2"),
        data = poolcenter,
        method = "LRT")$anova_table
##plot
###visreg
visreg(herbmodel_treatment,
       "Sampling", by = "Treatment")
###ggeffect
herbeffect_treatment <- 
  ggeffect(herbmodel_treatment,
           type = "re",
           x.as.factor = T,
           terms = c("Sampling", "Treatment"),
           ci.lvl = 0.95)
herbeffect_treatment$x <- 
  factor(herbeffect_treatment$x, levels = c("B", "A"))
herbeffect_treatment$group <- 
  factor(herbeffect_treatment$group, levels = c("wo", "w", "wr"))
herbplot_treatment <- 
  ggplot(herbeffect_treatment, 
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
  xlab("Sampling") +
  scale_x_discrete(limit = c("B", "A"),
                   labels = c("Before", "After"),
                   expand = expand_scale(add = c(0.6)))+
  ylab("Herbivore abundance") +
  ylim(0.5,1.75)+
  scale_y_continuous(breaks = c(1,1.5, 2, 2.5))+
  scale_color_manual(name = "Treatment",
                     labels = c("Wihout", "With", "Removal"),
                     values = c("darkgreen", "saddlebrown", "ivory4")) +
  theme(legend.position = "none",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))




# Models all herbivore less snail abundance per quadrat ------------------------------------------
#Volume proximity index
herbsnaillessmodel_largeleaf <- 
  glmer(herbsnailless ~
          largeleaf*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="log"),
        data = predcenter)
simulationOutput <- 
  simulateResiduals(fittedModel = herbsnaillessmodel_largeleaf, 
                    n = 2000,
                    rank = T)
plotSimulatedResiduals(simulationOutput = simulationOutput,
                       quantreg = F,
                       rank = T)
summary(herbsnaillessmodel_largeleaf)
herbsnaillesstest_largeleaf <- 
  mixed(herbsnailless~ 
          largeleaf*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="log"),
        type = afex_options(type = "2"),
        data = predcenter,
        method = "LRT")$anova_table
##plot
###visreg
visreg(herbsnaillessmodel_largeleaf,
       "largeleaf", by = "Sampling")
###ggeffect
herbsnaillesseffect_largeleaf <- 
  ggeffect(herbsnaillessmodel_largeleaf,
           terms = c("largeleaf", "Sampling"),
           swap.pred = T,
           type = "re",
           ci.level = 0.95)
herbsnaillesseffect_largeleaf$group <- 
  factor(herbsnaillesseffect_largeleaf$group, levels = c("B", "A"))
col <- 
  ifelse(poolcenter$Sampling == "B",
         "darkorange2", 
         "dodgerblue4")
herbsnaillesseffect_largeleaf$conf.low <- 
  herbsnaillesseffect_largeleaf$conf.low +1
herbsnaillesseffect_largeleaf$conf.high <- 
  herbsnaillesseffect_largeleaf$conf.high +1
herbsnaillesseffect_largeleaf$predicted <- 
  herbsnaillesseffect_largeleaf$predicted +1
herbsnaillessplot_largeleaf <- 
  plot(herbsnaillesseffect_largeleaf,
       ci = T) + 
  geom_point(data = poolcenter,
             mapping = aes(x = largeleaf, y = jitter(herbsnailless +1, 2)), 
             colour = col, 
             fill = col) +
  ggtitle("") + 
  xlab("Volume proximity index") +
  ylab("Herbivore less snail abundance") +
  scale_color_manual(labels = c("Before", "After"), 
                     values = c("darkorange2", "dodgerblue4")) +
  scale_y_continuous(trans = "log",
                     breaks = c(1,5,20)) +
  theme(legend.position = "none",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

#Treatment
herbsnaillessmodel_treatment <- 
  glmer(herbsnailless ~
          Treatment*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="sqrt"),
        data = predcenter)
simulationOutput <- 
  simulateResiduals(fittedModel = herbsnaillessmodel_treatment, 
                    n = 2000)
plotSimulatedResiduals(simulationOutput = simulationOutput, 
                       asFactor = T,
                       quantreg = F)
summary(herbsnaillessmodel_treatment)
herbsnaillesstest_treatment <- 
  mixed(herbsnailless~ 
          Treatment*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="sqrt"),
        type = afex_options(type = "2"),
        data = predcenter,
        method = "LRT")$anova_table
visreg(herbsnaillessmodel_treatment,
       "Treatment", by = "Sampling")





# Models on kinds of herbivores -------------------------------------------
#Herbeetle
##Volume proximity index
herbeetlemodel_largeleaf <- 
  glmer(herbeetle ~
          largeleaf*Sampling +
          (1|Site/alltrees/quadrats),
        family = "poisson"(link = "log"),
        data = predcenter)
simulationOutput <- 
  simulateResiduals(fittedModel = herbeetlemodel_largeleaf, 
                    n = 2000,
                    rank = T)
plotSimulatedResiduals(simulationOutput = simulationOutput,
                       quantreg = F,
                       rank = T)
summary(herbeetlemodel_largeleaf)
herbeetletest_largeleaf <- 
  mixed(herbeetle~ 
          largeleaf*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="log"),
        type = afex_options(type = "2"),
        control = glmerControl(optimizer = "bobyqa"),
        data = predcenter,
        method = "LRT")$anova_table
###plot
####visreg
visreg(herbeetlemodel_largeleaf,
       "largeleaf", by = "Sampling")
###ggeffect
herbeetleeffect_largeleaf <- 
  ggeffect(herbeetlemodel_largeleaf,
           terms = c("largeleaf", "Sampling"),
           swap.pred = T,
           type = "re",
           ci.level = 0.95)
herbeetleeffect_largeleaf$group <- 
  factor(herbeetleeffect_largeleaf$group, levels = c("B", "A"))
col <- 
  ifelse(predcenter$Sampling == "B",
         "darkorange2", 
         "dodgerblue4")
herbeetleeffect_largeleaf$conf.low <- 
  herbeetleeffect_largeleaf$conf.low +1
herbeetleeffect_largeleaf$conf.high <- 
  herbeetleeffect_largeleaf$conf.high +1
herbeetleeffect_largeleaf$predicted <- 
  herbeetleeffect_largeleaf$predicted +1
herbeetleplot_largeleaf <- 
  plot(herbeetleeffect_largeleaf,
       ci = T) + 
  geom_point(data = predcenter,
             mapping = aes(x = largeleaf, y = jitter(herbeetle +1, 2)), 
             colour = col, 
             fill = col) +
  ggtitle("") + 
  xlab("Volume proximity index") +
  ylab("Herbivorous beetle abundance") +
  scale_color_manual(labels = c("Before", "After"), 
                     values = c("darkorange2", "dodgerblue4")) +
  scale_y_continuous(trans = "log",
                     breaks = c(1,3,7)) +
  theme(legend.position = "none",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

##Treatment
herbeetlemodel_treatment <- 
  glmer.nb(herbeetle ~
          Treatment*Sampling +
          (1|Site/alltrees/quadrats),
        data = predcenter)
simulationOutput <- 
  simulateResiduals(fittedModel = herbeetlemodel_treatment, 
                    n = 2000)
plotSimulatedResiduals(simulationOutput = simulationOutput, 
                       asFactor = T,
                       quantreg = F)
summary(herbeetlemodel_treatment)
herbeetletest_treatment <- 
  mixed(herbeetle~ 
          Treatment*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="negative.binomial"(theta = getME(herbeetlemodel_treatment,
                                    "glmer.nb.theta")),
        type = afex_options(type = "2"),
        data = predcenter,
        method = "LRT")$anova_table
##plot
###visreg
visreg(herbeetlemodel_treatment,
       "Sampling", by = "Treatment")
###ggeffect
herbeetleeffect_treatment <- 
  ggeffect(herbeetlemodel_treatment,
           type = "re",
           x.as.factor = T,
           terms = c("Sampling", "Treatment"),
           ci.lvl = 0.95)
herbeetleeffect_treatment$x <- 
  factor(herbeetleeffect_treatment$x, levels = c("B", "A"))
herbeetleeffect_treatment$group <- 
  factor(herbeetleeffect_treatment$group, levels = c("wo", "w", "wr"))
herbeetleplot_treatment <- 
  ggplot(herbeetleeffect_treatment, 
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
  xlab("Sampling") +
  scale_x_discrete(limit = c("B", "A"),
                   labels = c("Before", "After"),
                   expand = expand_scale(add = c(0.6)))+
  ylab("Herbivorous beetle abundance") +
  ylim(0.5,1.75)+
  scale_y_continuous(breaks = c(1,1.5, 2, 2.5))+
  scale_color_manual(name = "Treatment",
                     labels = c("Wihout", "With", "Removal"),
                     values = c("darkgreen", "saddlebrown", "ivory4")) +
  theme(legend.position = "none",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))

#Hoppers
##Volume proximity index
jumpmodel_largeleaf <- 
  glmer(jump ~
          largeleaf*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="sqrt"),
        data = predcenter)
simulationOutput <- 
  simulateResiduals(fittedModel = jumpmodel_largeleaf, 
                    n = 2000,
                    rank = T)
plotSimulatedResiduals(simulationOutput = simulationOutput,
                       quantreg = F,
                       rank = T)
summary(jumpmodel_largeleaf)
jumptest_largeleaf <- 
  mixed(jump~ 
          largeleaf*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="sqrt"),
        type = afex_options(type = "2"),
        data = predcenter,
        method = "LRT")$anova_table
###plot
####visreg
visreg(jumpmodel_largeleaf,
       "largeleaf", by = "Sampling")
###ggeffect
jumpeffect_largeleaf <- 
  ggeffect(jumpmodel_largeleaf,
           terms = c("largeleaf", "Sampling"),
           swap.pred = T,
           type = "re",
           ci.level = 0.95)
jumpeffect_largeleaf$group <- 
  factor(jumpeffect_largeleaf$group, levels = c("B", "A"))
col <- 
  ifelse(predcenter$Sampling == "B",
         "darkorange2", 
         "dodgerblue4")
jumpeffect_largeleaf$conf.low <- 
  jumpeffect_largeleaf$conf.low +1
jumpeffect_largeleaf$conf.high <- 
  jumpeffect_largeleaf$conf.high +1
jumpeffect_largeleaf$predicted <- 
  jumpeffect_largeleaf$predicted +1
jumpplot_largeleaf <- 
  plot(jumpeffect_largeleaf,
       ci = T) + 
  geom_point(data = predcenter,
             mapping = aes(x = largeleaf, y = jitter(jump +1, 2)), 
             colour = col, 
             fill = col) +
  ggtitle("") + 
  xlab("Volume proximity index") +
  ylab("Hopper abundance") +
  scale_color_manual(labels = c("Before", "After"), 
                     values = c("darkorange2", "dodgerblue4")) +
  scale_y_continuous(trans = "log",
                     breaks = c(1,3,7)) +
  theme(legend.position = "none",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

##Treatment
jumpmodel_treatment <- 
  glmer(jump ~
          Treatment*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="log"),
        data = predcenter)
simulationOutput <- 
  simulateResiduals(fittedModel = jumpmodel_treatment, 
                    n = 2000)
plotSimulatedResiduals(simulationOutput = simulationOutput, 
                       asFactor = T,
                       quantreg = F)
summary(jumpmodel_treatment)
jumptest_treatment <- 
  mixed(jump~ 
          Treatment*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="log"),
        type = afex_options(type = "2"),
        data = predcenter,
        method = "LRT")$anova_table
###plot
####visreg
visreg(jumpmodel_treatment,
       "Sampling", by = "Treatment")
####ggeffect
jumpeffect_treatment <- 
  ggeffect(jumpmodel_treatment,
           type = "re",
           x.as.factor = T,
           terms = c("Sampling", "Treatment"),
           ci.lvl = 0.95)
jumpeffect_treatment$x <- 
  factor(jumpeffect_treatment$x, levels = c("B", "A"))
jumpeffect_treatment$group <- 
  factor(jumpeffect_treatment$group, levels = c("wo", "w", "wr"))
jumpplot_treatment <- 
  ggplot(jumpeffect_treatment, 
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
  xlab("Sampling") +
  scale_x_discrete(limit = c("B", "A"),
                   labels = c("Before", "After"),
                   expand = expand_scale(add = c(0.6)))+
  scale_y_continuous(breaks = c(0.3,0.6)) +
  ylab("Hopper abundance") +
  scale_color_manual(name = "Treatment",
                     labels = c("Wihout", "With", "Removal"),
                     values = c("darkgreen", "saddlebrown", "ivory4")) +
  theme(legend.position = "none",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))


#Psyllids
##Volume proximity index
psyllidmodel_largeleaf <- 
  glmer(psyllid ~
          largeleaf*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="log"),
        data = predcenter)
simulationOutput <- 
  simulateResiduals(fittedModel = psyllidmodel_largeleaf, 
                    n = 2000,
                    rank = T)
plotSimulatedResiduals(simulationOutput = simulationOutput,
                       quantreg = F,
                       rank = T)
summary(psyllidmodel_largeleaf)
psyllidtest_largeleaf <- 
  mixed(psyllid~ 
          largeleaf*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="log"),
        type = afex_options(type = "2"),
        data = predcenter,
        method = "LRT")$anova_table
###plot
####visreg
visreg(psyllidmodel_largeleaf,
       "largeleaf", by = "Sampling")
###ggeffect
psyllideffect_largeleaf <- 
  ggeffect(psyllidmodel_largeleaf,
           terms = c("largeleaf", "Sampling"),
           swap.pred = T,
           type = "re",
           ci.level = 0.95)
psyllideffect_largeleaf$group <- 
  factor(psyllideffect_largeleaf$group, levels = c("B", "A"))
col <- 
  ifelse(predcenter$Sampling == "B",
         "darkorange2", 
         "dodgerblue4")
psyllideffect_largeleaf$conf.low <- 
  psyllideffect_largeleaf$conf.low +1
psyllideffect_largeleaf$conf.high <- 
  psyllideffect_largeleaf$conf.high +1
psyllideffect_largeleaf$predicted <- 
  psyllideffect_largeleaf$predicted +1
psyllidplot_largeleaf <- 
  plot(psyllideffect_largeleaf,
       ci = T) + 
  geom_point(data = predcenter,
             mapping = aes(x = largeleaf, y = jitter(psyllid +1, 2)), 
             colour = col, 
             fill = col) +
  ggtitle("") + 
  xlab("Volume proximity index") +
  ylab("Psyllid abundance") +
  scale_color_manual(labels = c("Before", "After"), 
                     values = c("darkorange2", "dodgerblue4")) +
  scale_y_continuous(trans = "log",
                     breaks = c(1,3,7)) +
  theme(legend.position = "none",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

##Treatment
psyllidmodel_treatment <- 
  glmer(psyllid ~
          Treatment*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="log"),
        control =glmerControl(optimizer = "bobyqa"),
        data = predcenter)
simulationOutput <- 
  simulateResiduals(fittedModel = psyllidmodel_treatment, 
                    n = 2000)
plotSimulatedResiduals(simulationOutput = simulationOutput, 
                       asFactor = T,
                       quantreg = F)
summary(psyllidmodel_treatment)
psyllidtest_treatment <- 
  mixed(psyllid~ 
          Treatment*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="log"),
        type = afex_options(type = "2"),
        control =glmerControl(optimizer = "bobyqa"),
        data = predcenter,
        method = "LRT")$anova_table
###plot
####visreg
visreg(psyllidmodel_treatment,
       "Sampling", by = "Treatment")
####ggeffect
psyllideffect_treatment <- 
  ggeffect(psyllidmodel_treatment,
           type = "re",
           x.as.factor = T,
           terms = c("Sampling", "Treatment"),
           ci.lvl = 0.95)
psyllideffect_treatment$x <- 
  factor(psyllideffect_treatment$x, levels = c("B", "A"))
psyllideffect_treatment$group <- 
  factor(psyllideffect_treatment$group, levels = c("wo", "w", "wr"))
psyllidplot_treatment <- 
  ggplot(psyllideffect_treatment, 
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
  xlab("Sampling") +
  scale_x_discrete(limit = c("B", "A"),
                   labels = c("Before", "After"),
                   expand = expand_scale(add = c(0.6)))+
  ylab("Psyllid abundance") +
  scale_color_manual(name = "Treatment",
                     labels = c("Wihout", "With", "Removal"),
                     values = c("darkgreen", "saddlebrown", "ivory4")) +
  theme(legend.position = "none",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))

#Honeydew producers
##Volume proximity index
scaleaphidmodel_largeleaf <- 
  glmer(scaleaphid ~
          largeleaf*Sampling +
          (1|Site/alltrees/quadrats),
          family = "poisson"(link = "sqrt"),
        data = predcenter)
simulationOutput <- 
  simulateResiduals(fittedModel = scaleaphidmodel_largeleaf, 
                    n = 2000,
                    rank = T)
plotSimulatedResiduals(simulationOutput = simulationOutput,
                       quantreg = F,
                       rank = T)
summary(scaleaphidmodel_largeleaf)
scaleaphidtest_largeleaf <- 
  mixed(scaleaphid~ 
          largeleaf*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link = "sqrt"),
        type = afex_options(type = "2"),
        data = predcenter,
        method = "LRT")$anova_table
###plot
####visreg
visreg(scaleaphidmodel_largeleaf,
       "largeleaf", by = "Sampling")
###ggeffect
scaleaphideffect_largeleaf <- 
  ggeffect(scaleaphidmodel_largeleaf,
           terms = c("largeleaf", "Sampling"),
           swap.pred = T,
           type = "re",
           ci.level = 0.95)
scaleaphideffect_largeleaf$group <- 
  factor(scaleaphideffect_largeleaf$group, levels = c("B", "A"))
col <- 
  ifelse(predcenter$Sampling == "B",
         "darkorange2", 
         "dodgerblue4")
scaleaphideffect_largeleaf$conf.low <- 
  scaleaphideffect_largeleaf$conf.low +1
scaleaphideffect_largeleaf$conf.high <- 
  scaleaphideffect_largeleaf$conf.high +1
scaleaphideffect_largeleaf$predicted <- 
  scaleaphideffect_largeleaf$predicted +1
scaleaphidplot_largeleaf <- 
  plot(scaleaphideffect_largeleaf,
       ci = T) + 
  geom_point(data = predcenter,
             mapping = aes(x = largeleaf, y = jitter(scaleaphid +1, 2)), 
             colour = col, 
             fill = col) +
  ggtitle("") + 
  xlab("Volume proximity index") +
  ylab("Honeydew producer abundance") +
  scale_color_manual(labels = c("Before", "After"), 
                     values = c("darkorange2", "dodgerblue4")) +
  scale_y_continuous(trans = "log",
                     breaks = c(1,3,7)) +
  theme(legend.position = "none",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

##Treatment
scaleaphidmodel_treatment <- 
  glmer(scaleaphid ~
          Treatment*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="log"),
        control = glmerControl(optimizer = "bobyqa"),
        data = predcenter)
simulationOutput <- 
  simulateResiduals(fittedModel = scaleaphidmodel_treatment, 
                    n = 2000)
plotSimulatedResiduals(simulationOutput = simulationOutput, 
                       asFactor = T,
                       quantreg = F)
summary(scaleaphidmodel_treatment)
scaleaphidtest_treatment <- 
  mixed(scaleaphid~ 
          Treatment*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="log"),
        control = glmerControl(optimizer = "bobyqa"),
        type = afex_options(type = "2"),
        data = predcenter,
        method = "LRT")$anova_table
###plot
####visreg
visreg(scaleaphidmodel_treatment,
       "Sampling", by = "Treatment")
####ggeffect
scaleaphideffect_treatment <- 
  ggeffect(scaleaphidmodel_treatment,
           type = "re",
           x.as.factor = T,
           terms = c("Sampling", "Treatment"),
           ci.lvl = 0.95)
scaleaphideffect_treatment$x <- 
  factor(scaleaphideffect_treatment$x, levels = c("B", "A"))
scaleaphideffect_treatment$group <- 
  factor(scaleaphideffect_treatment$group, levels = c("wo", "w", "wr"))
scaleaphidplot_treatment <- 
  ggplot(scaleaphideffect_treatment, 
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
  xlab("Sampling") +
  scale_x_discrete(limit = c("B", "A"),
                   labels = c("Before", "After"),
                   expand = expand_scale(add = c(0.6)))+
  ylab("Honeydew producer abundance") +
  scale_color_manual(name = "Treatment",
                     labels = c("Wihout", "With", "Removal"),
                     values = c("darkgreen", "saddlebrown", "ivory4")) +
  theme(legend.position = "none",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))

#Snails
##Volume proximity index
snailmodel_largeleaf <- 
  glmer(snail ~
          largeleaf*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="log"),
        control = glmerControl(optimizer = "bobyqa"),
        data = predcenter)
simulationOutput <- 
  simulateResiduals(fittedModel = snailmodel_largeleaf, 
                    n = 2000,
                    rank = T)
plotSimulatedResiduals(simulationOutput = simulationOutput,
                       quantreg = F,
                       rank = T)
summary(snailmodel_largeleaf)
snailtest_largeleaf <- 
  mixed(snail~ 
          largeleaf*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="log"),
        control = glmerControl(optimizer = "bobyqa"),
        type = afex_options(type = "2"),
        data = predcenter,
        method = "LRT")$anova_table
###plot
####visreg
visreg(snailmodel_largeleaf,
       "largeleaf", by = "Sampling")
###ggeffect
snaileffect_largeleaf <- 
  ggeffect(snailmodel_largeleaf,
           terms = c("largeleaf", "Sampling"),
           swap.pred = T,
           type = "re",
           ci.level = 0.95)
snaileffect_largeleaf$group <- 
  factor(snaileffect_largeleaf$group, levels = c("B", "A"))
col <- 
  ifelse(predcenter$Sampling == "B",
         "darkorange2", 
         "dodgerblue4")
snaileffect_largeleaf$conf.low <- 
  snaileffect_largeleaf$conf.low +1
snaileffect_largeleaf$conf.high <- 
  snaileffect_largeleaf$conf.high +1
snaileffect_largeleaf$predicted <- 
  snaileffect_largeleaf$predicted +1
snailplot_largeleaf <- 
  plot(snaileffect_largeleaf,
       ci = T) + 
  geom_point(data = predcenter,
             mapping = aes(x = largeleaf, y = jitter(snail +1, 2)), 
             colour = col, 
             fill = col) +
  ggtitle("") + 
  xlab("Volume proximity index") +
  ylab("Honeydew producer abundance") +
  scale_color_manual(labels = c("Before", "After"), 
                     values = c("darkorange2", "dodgerblue4")) +
  scale_y_continuous(trans = "log",
                     breaks = c(1,3,7)) +
  theme(legend.position = "none",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

##Treatment
snailmodel_treatment <- 
  glmer(round(sqrt(snail)) ~
          Treatment*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="log"),
        control = glmerControl(optimizer = "bobyqa"),
        data = predcenter)
simulationOutput <- 
  simulateResiduals(fittedModel = snailmodel_treatment, 
                    n = 2000)
plotSimulatedResiduals(simulationOutput = simulationOutput, 
                       asFactor = T,
                       quantreg = F)
summary(snailmodel_treatment)
snailtest_treatment <- 
  mixed(round(sqrt(snail))~ 
          Treatment*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="log"),
        type = afex_options(type = "2"),
        control = glmerControl(optimizer = "bobyqa"),
        data = predcenter,
        method = "LRT")$anova_table
###plot
####visreg
visreg(snailmodel_treatment,
       "Treatment", by = "Sampling")
###ggeffect
snaileffect_treatment <- 
  ggeffect(snailmodel_treatment,
           type = "re",
           x.as.factor = T,
           terms = c("Sampling", "Treatment"),
           ci.lvl = 0.95)
snaileffect_treatment$x <- 
  factor(snaileffect_treatment$x, levels = c("B", "A"))
snaileffect_treatment$group <- 
  factor(snaileffect_treatment$group, levels = c("wo", "w", "wr"))
snailplot_treatment <- 
  ggplot(snaileffect_treatment, 
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
  xlab("Sampling") +
  scale_x_discrete(limit = c("B", "A"),
                   labels = c("Before", "After"),
                   expand = expand_scale(add = c(0.6)))+
  scale_color_manual(name = "Treatment",
                     labels = c("Wihout", "With", "Removal"),
                     values = c("darkgreen", "saddlebrown", "ivory4")) +
  theme(legend.position = "none",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))



# Adonis all kinds per quadrat -------------------------------
#Volume proximity index
herbadonis_largeleaf <- 
  adonis(spread_herb[,8:14] ~
           largeleaf*Sampling,
         method = "bray",
         permutations = 2000,
         strata = spread_herb$Site,
         data = spread_herb)

#Predator index
herbadonis_predindex <- 
  adonis(spread_herb[,8:14] ~
           predindex*Sampling,
         method = "bray",
         permutations = 2000,
         strata = spread_herb$Site,
         data = spread_herb)

#Treatment
herbadonis_treatment <- 
  adonis(spread_herb[,8:14] ~
           Treatment*Sampling,
         method = "bray",
         permutations = 2000,
         strata = spread_herb$Site,
         data = spread_herb)




