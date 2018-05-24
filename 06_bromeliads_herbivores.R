#Bromeliads on herbivores

#Models all herbivore abundance per quadrat ------------------------------------------
#Volume index
  herbmodel_largeleaf <- 
  glmer(herb ~
          largeleaf*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="log"),
        data = predcenter)
simulationOutput <- 
  simulateResiduals(fittedModel = herbmodel_largeleaf, n = 1000)
plotSimulatedResiduals(simulationOutput = simulationOutput)
dispersion_glmer(herbmodel_largeleaf)
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

herbplot_largeleaf <- 
  plot(herbeffect_largeleaf,
       ci = T) + 
  geom_point(data = poolcenter,
             mapping = aes(x = largeleaf, y = jitter(herb, 2)), 
             colour = col, 
             fill = col) +
  ggtitle("") + 
  xlab("Volume proximity index") +
  ylab("Herbivore abundance") +
  scale_color_manual(labels = c("Before", "After"), 
                     values = c("darkorange2", "dodgerblue4")) +
  theme(legend.position = "none",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

#Predator index
herbmodel_predindex <- 
  glmer(herb ~
          predindex*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="log"),
        data = predcenter)
simulationOutput <- 
  simulateResiduals(fittedModel = herbmodel_predindex, n = 1000)
plotSimulatedResiduals(simulationOutput = simulationOutput)
dispersion_glmer(herbmodel_predindex)
summary(herbmodel_predindex)
herbtest_predindex <- 
  mixed(herb~ 
          predindex*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="log"),
        type = afex_options(type = "2"),
        data = predcenter,
        method = "LRT")$anova_table
visreg(herbmodel_predindex,
       "predindex", by = "Sampling")

#Treatment
herbmodel_treatment <- 
  glmer(herb ~
          Treatment*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="sqrt"),
        data = predcenter)
simulationOutput <- 
  simulateResiduals(fittedModel = herbmodel_treatment, n = 1000)
plotSimulatedResiduals(simulationOutput = simulationOutput, asFactor = T)
dispersion_glmer(herbmodel_treatment)
summary(herbmodel_treatment)
herbtest_treatment <- 
  mixed(herb~ 
          Treatment*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="sqrt"),
        type = afex_options(type = "2"),
        data = predcenter,
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




#Models all herbivore less snail abundance per quadrat ------------------------------------------
#Volume index
herbsnaillessmodel_largeleaf <- 
  glmer(herbsnailless ~
          largeleaf*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="sqrt"),
        data = predcenter)
simulationOutput <- 
  simulateResiduals(fittedModel = herbsnaillessmodel_largeleaf, n = 1000)
plotSimulatedResiduals(simulationOutput = simulationOutput)
dispersion_glmer(herbsnaillessmodel_largeleaf)
summary(herbsnaillessmodel_largeleaf)
herbsnaillesstest_largeleaf <- 
  mixed(herbsnailless~ 
          largeleaf*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="sqrt"),
        type = afex_options(type = "2"),
        data = predcenter,
        method = "LRT")$anova_table
visreg(herbsnaillessmodel_largeleaf,
       "largeleaf", by = "Sampling")

#Predator index
herbsnaillessmodel_predindex <- 
  glmer(herbsnailless ~
          predindex*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="sqrt"),
        data = predcenter)
simulationOutput <- 
  simulateResiduals(fittedModel = herbsnaillessmodel_predindex, n = 1000)
plotSimulatedResiduals(simulationOutput = simulationOutput)
dispersion_glmer(herbsnaillessmodel_predindex)
summary(herbsnaillessmodel_predindex)
herbsnaillesstest_predindex <- 
  mixed(herbsnailless~ 
          predindex*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="sqrt"),
        type = afex_options(type = "2"),
        data = predcenter,
        method = "LRT")$anova_table
visreg(herbsnaillessmodel_predindex,
       "predindex", by = "Sampling")

#Treatment
herbsnaillessmodel_treatment <- 
  glmer(herbsnailless ~
          Treatment*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="sqrt"),
        data = predcenter)
simulationOutput <- 
  simulateResiduals(fittedModel = herbsnaillessmodel_treatment, n = 1000)
plotSimulatedResiduals(simulationOutput = simulationOutput, asFactor = T)
dispersion_glmer(herbsnaillessmodel_treatment)
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





#Models on kinds of herbivores -------------------------------------------
#Herbeetle
##Volume index
herbeetlemodel_largeleaf <- 
  glmer(herbeetle ~
          largeleaf*Sampling +
          (1|Site/alltrees/quadrats),
        family = "poisson"(link = "log"),
        data = predcenter)
simulationOutput <- 
  simulateResiduals(fittedModel = herbeetlemodel_largeleaf, n = 1000)
plotSimulatedResiduals(simulationOutput = simulationOutput)
dispersion_glmer(herbeetlemodel_largeleaf)
summary(herbeetlemodel_largeleaf)
herbeetletest_largeleaf <- 
  mixed(herbeetle~ 
          largeleaf*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="log"),
        type = afex_options(type = "2"),
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

herbeetleplot_largeleaf <- 
  plot(herbeetleeffect_largeleaf,
       ci = T) + 
  geom_point(data = predcenter,
             mapping = aes(x = largeleaf, y = jitter(herbeetle, 2)), 
             colour = col, 
             fill = col) +
  ggtitle("") + 
  xlab("Volume proximity index") +
  ylab("Herbivorous beetle abundance") +
  scale_color_manual(labels = c("Before", "After"), 
                     values = c("darkorange2", "dodgerblue4")) +
  scale_y_continuous(trans = "log") +
  theme(legend.position = "none",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

##Predator index
herbeetlemodel_predindex <- 
  glmer(herbeetle ~
          predindex*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="log"),
        data = predcenter)
simulationOutput <- 
  simulateResiduals(fittedModel = herbeetlemodel_predindex, n = 1000)
plotSimulatedResiduals(simulationOutput = simulationOutput)
dispersion_glmer(herbeetlemodel_predindex)
summary(herbeetlemodel_predindex)
herbeetletest_predindex <- 
  mixed(herbeetle~ 
          predindex*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="log"),
        type = afex_options(type = "2"),
        data = predcenter,
        method = "LRT")$anova_table
visreg(herbeetlemodel_predindex,
       "predindex", by = "Sampling")
##Treatment
herbeetlemodel_treatment <- 
  glmer(herbeetle ~
          Treatment*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="sqrt"),
        data = predcenter)
simulationOutput <- 
  simulateResiduals(fittedModel = herbeetlemodel_treatment, n = 1000)
plotSimulatedResiduals(simulationOutput = simulationOutput, asFactor = T)
summary(herbeetlemodel_treatment)
dispersion_glmer(herbeetlemodel_treatment)
herbeetletest_treatment <- 
  mixed(herbeetle~ 
          Treatment*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="sqrt"),
        type = afex_options(type = "2"),
        data = predcenter,
        method = "LRT")$anova_table
visreg(herbeetlemodel_treatment,
       "Treatment", by = "Sampling")

#Hoppers
##Volume index
jumpmodel_largeleaf <- 
  glmer(jump ~
          largeleaf*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="log"),
        data = predcenter)
simulationOutput <- 
  simulateResiduals(fittedModel = jumpmodel_largeleaf, n = 1000)
plotSimulatedResiduals(simulationOutput = simulationOutput)
dispersion_glmer(jumpmodel_largeleaf)
summary(jumpmodel_largeleaf)
jumptest_largeleaf <- 
  mixed(jump~ 
          largeleaf*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="log"),
        type = afex_options(type = "2"),
        data = predcenter,
        method = "LRT")$anova_table
visreg(jumpmodel_largeleaf,
       "largeleaf", by = "Sampling")
##Predator index
jumpmodel_predindex <- 
  glmer(jump ~
          predindex*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="log"),
        data = predcenter)
simulationOutput <- 
  simulateResiduals(fittedModel = jumpmodel_predindex, n = 1000)
plotSimulatedResiduals(simulationOutput = simulationOutput)
dispersion_glmer(jumpmodel_predindex)
summary(jumpmodel_predindex)
jumptest_predindex <- 
  mixed(jump~ 
          predindex*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="log"),
        type = afex_options(type = "2"),
        data = predcenter,
        method = "LRT")$anova_table
visreg(jumpmodel_predindex,
       "predindex", by = "Sampling")
##Treatment
jumpmodel_treatment <- 
  glmer(jump ~
          Treatment*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="log"),
        data = predcenter)
plot(jumpmodel_treatment)
simulationOutput <- 
  simulateResiduals(fittedModel = jumpmodel_treatment, n = 1000)
plotSimulatedResiduals(simulationOutput = simulationOutput, asFactor = T)
dispersion_glmer(jumpmodel_treatment)
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
##Volume index
psyllidmodel_largeleaf <- 
  glmer(psyllid ~
          largeleaf*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="log"),
        data = predcenter)
simulationOutput <- 
  simulateResiduals(fittedModel = psyllidmodel_largeleaf, n = 1000)
plotSimulatedResiduals(simulationOutput = simulationOutput)
dispersion_glmer(psyllidmodel_largeleaf)
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

psyllidplot_largeleaf <- 
  plot(psyllideffect_largeleaf,
       ci = T) + 
  geom_point(data = predcenter,
             mapping = aes(x = largeleaf, y = jitter(psyllid, 2)), 
             colour = col, 
             fill = col) +
  ggtitle("") + 
  xlab("Volume proximity index") +
  ylab("Psyllid abundance") +
  scale_color_manual(labels = c("Before", "After"), 
                     values = c("darkorange2", "dodgerblue4")) +
  scale_y_continuous(trans = "log") +
  theme(legend.position = "none",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))


##Predator index
psyllidmodel_predindex <- 
  glmer(psyllid ~
          predindex*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="log"),
        data = predcenter)
simulationOutput <- 
  simulateResiduals(fittedModel = psyllidmodel_predindex, n = 1000)
plotSimulatedResiduals(simulationOutput = simulationOutput)
dispersion_glmer(psyllidmodel_largeleaf)
summary(psyllidmodel_predindex)
psyllidtest_predindex <- 
  mixed(psyllid~ 
          predindex*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="log"),
        type = afex_options(type = "2"),
        data = predcenter,
        method = "LRT")$anova_table
visreg(psyllidmodel_predindex,
       "predindex", by = "Sampling")
##Treatment
psyllidmodel_treatment <- 
  glmer(psyllid ~
          Treatment*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="sqrt"),
        data = predcenter)
simulationOutput <- 
  simulateResiduals(fittedModel = psyllidmodel_treatment, n = 1000)
plotSimulatedResiduals(simulationOutput = simulationOutput, asFactor = T)
dispersion_glmer(psyllidmodel_treatment)
summary(psyllidmodel_treatment)
psyllidtest_treatment <- 
  mixed(psyllid~ 
          Treatment*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="sqrt"),
        type = afex_options(type = "2"),
        data = predcenter,
        method = "LRT")$anova_table
visreg(psyllidmodel_treatment,
       "Treatment", by = "Sampling")


#Scales and aphids
##Volume index
scaleaphidmodel_largeleaf <- 
  glmer(scaleaphid ~
          largeleaf*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="sqrt"),
        data = predcenter)
simulationOutput <- 
  simulateResiduals(fittedModel = scaleaphidmodel_largeleaf, n = 1000)
plotSimulatedResiduals(simulationOutput = simulationOutput)
dispersion_glmer(scaleaphidmodel_largeleaf)
summary(scaleaphidmodel_largeleaf)
scaleaphidtest_largeleaf <- 
  mixed(scaleaphid~ 
          largeleaf*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="sqrt"),
        type = afex_options(type = "2"),
        data = predcenter,
        method = "LRT")$anova_table
visreg(scaleaphidmodel_largeleaf,
       "largeleaf", by = "Sampling")
##Predator index
scaleaphidmodel_predindex <- 
  glmer(scaleaphid ~
          predindex*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="sqrt"),
        data = predcenter)
simulationOutput <- 
  simulateResiduals(fittedModel = scaleaphidmodel_predindex, n = 1000)
plotSimulatedResiduals(simulationOutput = simulationOutput)
dispersion_glmer(scaleaphidmodel_predindex)
summary(scaleaphidmodel_predindex)
scaleaphidtest_predindex <- 
  mixed(scaleaphid~ 
          predindex*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="sqrt"),
        type = afex_options(type = "2"),
        data = predcenter,
        method = "LRT")$anova_table
visreg(scaleaphidmodel_predindex,
       "predindex", by = "Sampling")
##Treatment
scaleaphidmodel_treatment <- 
  glmer(round(scaleaphid^0.5) ~
          Treatment*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="log"),
        data = predcenter)
simulationOutput <- 
  simulateResiduals(fittedModel = scaleaphidmodel_treatment, n = 1000)
plotSimulatedResiduals(simulationOutput = simulationOutput, asFactor = T)
dispersion_glmer(scaleaphidmodel_treatment)
summary(scaleaphidmodel_treatment)
scaleaphidtest_treatment <- 
  mixed(round(scaleaphid^0.5)~ 
          Treatment*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="log"),
        type = afex_options(type = "2"),
        data = predcenter,
        method = "LRT")$anova_table
visreg(scaleaphidmodel_treatment,
       "Treatment", by = "Sampling")

#Snails
##Volume index
snailmodel_largeleaf <- 
  glmer(snail ~
          largeleaf*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="log"),
        data = predcenter)
simulationOutput <- 
  simulateResiduals(fittedModel = snailmodel_largeleaf, n = 1000)
plotSimulatedResiduals(simulationOutput = simulationOutput)
dispersion_glmer(snailmodel_largeleaf)
summary(snailmodel_largeleaf)
snailtest_largeleaf <- 
  mixed(snail~ 
          largeleaf*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="log"),
        type = afex_options(type = "2"),
        data = predcenter,
        method = "LRT")$anova_table
visreg(snailmodel_largeleaf,
       "largeleaf", by = "Sampling")
##Predator index
snailmodel_predindex <- 
  glmer(snail ~
          predindex*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="sqrt"),
        data = predcenter)
simulationOutput <- 
  simulateResiduals(fittedModel = snailmodel_predindex, n = 1000)
plotSimulatedResiduals(simulationOutput = simulationOutput)
dispersion_glmer(snailmodel_predindex)
summary(snailmodel_predindex)
snailtest_predindex <- 
  mixed(snail~ 
          predindex*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="sqrt"),
        type = afex_options(type = "2"),
        data = predcenter,
        method = "LRT")$anova_table
visreg(snailmodel_predindex,
       "predindex", by = "Sampling")
##Treatment
snailmodel_treatment <- 
  glmer(snail ~
          Treatment*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="sqrt"),
        data = predcenter)
simulationOutput <- 
  simulateResiduals(fittedModel = snailmodel_treatment, n = 1000)
plotSimulatedResiduals(simulationOutput = simulationOutput, asFactor = T)
dispersion_glmer(snailmodel_treatment)
summary(snailmodel_treatment)
snailtest_treatment <- 
  mixed(snail~ 
          Treatment*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="sqrt"),
        type = afex_options(type = "2"),
        data = predcenter,
        method = "LRT")$anova_table
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



#Adonis all kinds per quadrat -------------------------------
#Volume index
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





#Is the effect of bromeliads mediated by bromeliad predators?----------------------------------------------------
#all herbivores
##largeleaf
herbtest_largeleaf
predherbtest_brompreds
m3 <- 
  glmer(herb ~ 
          largeleaf*Sampling +  bromcenter*Sampling +
          (1|Site/alltrees/quadrats),
        family= "poisson"(link ="log"), 
        data =predcenter)
simulationOutput <- 
  simulateResiduals(fittedModel = m3, n = 1000)
plotSimulatedResiduals(simulationOutput = simulationOutput)
summary(m3)
dispersion_glmer(m3)
m3test <- 
  mixed(herb~ 
          largeleaf*Sampling +
          bromcenter*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="log"),
        type = afex_options(type = "2"),
        data = predcenter,
        method = "LRT")$anova_table
##predindex
herbtest_predindex
predherbtest_brompreds
m3 <- 
  glmer(herb ~ 
          predindex*Sampling +  bromcenter*Sampling +
          (1|Site/alltrees/quadrats),
        family= "poisson"(link ="log"), 
        data =predcenter)
simulationOutput <- 
  simulateResiduals(fittedModel = m3, n = 1000)
plotSimulatedResiduals(simulationOutput = simulationOutput)
summary(m3)
dispersion_glmer(m3)
m3test <- 
  mixed(herb~ 
          predindex*Sampling +
          bromcenter*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="log"),
        type = afex_options(type = "2"),
        data = predcenter,
        method = "LRT")$anova_table
##Treatment
herbtest_treatment
predherbtest_brompreds
m3 <- 
  glmer(herb ~ 
          Treatment*Sampling +  bromcenter*Sampling +
          (1|Site/alltrees/quadrats),
        family= "poisson"(link ="log"), 
        data =predcenter)
simulationOutput <- 
  simulateResiduals(fittedModel = m3, n = 1000)
plotSimulatedResiduals(simulationOutput = simulationOutput)
summary(m3)
dispersion_glmer(m3)
m3test <- 
  mixed(herb~ 
          Treatment*Sampling +
          bromcenter*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="log"),
        type = afex_options(type = "2"),
        data = predcenter,
        method = "LRT")$anova_table

#Snails
##predindex
snailtest_predindex
snailmodel_brompreds <- 
  glmer(snail ~ 
          bromcenter*Sampling +
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="sqrt"),
        data =predcenter)
simulationOutput <- 
  simulateResiduals(fittedModel = snailmodel_brompreds, n = 1000)
plotSimulatedResiduals(simulationOutput = simulationOutput)
summary(snailmodel_brompreds)
dispersion_glmer(snailmodel_brompreds)
snailtest_brompreds <- 
  mixed(snail~ 
          bromcenter*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="sqrt"),
        type = afex_options(type = "2"),
        data = predcenter,
        method = "LRT")$anova_table
m3 <- 
  glmer(snail ~ 
          predindex*Sampling +  bromcenter*Sampling +
          (1|Site/alltrees/quadrats),
        family= "poisson"(link ="sqrt"), 
        data =predcenter)
simulationOutput <- 
  simulateResiduals(fittedModel = m3, n = 1000)
plotSimulatedResiduals(simulationOutput = simulationOutput)
summary(m3)
dispersion_glmer(m3)
m3test <- 
  mixed(round(snail^0.5)~ 
          predindex*Sampling +
          bromcenter*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="sqrt"),
        type = afex_options(type = "2"),
        data = predcenter,
        method = "LRT")$anova_table
##Treatment
snailtest_treatment
snailtest_brompreds
m3 <- 
  glmer(snail ~ 
          Treatment*Sampling +  bromcenter*Sampling +
          (1|Site/alltrees/quadrats),
        family= "poisson"(link ="sqrt"), 
        data =predcenter)
simulationOutput <- 
  simulateResiduals(fittedModel = m3, n = 1000)
plotSimulatedResiduals(simulationOutput = simulationOutput)
summary(m3)
dispersion_glmer(m3)
m3test <- 
  mixed(snail~ 
          Treatment*Sampling +
          bromcenter*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="sqrt"),
        type = afex_options(type = "2"),
        data = predcenter,
        method = "LRT")$anova_table

#Is the effect of bromeliads mediated by mobile predators? -------------
#all herbivores
##largeleaf
herbtest_largeleaf
predherbtest_mobipreds
m3 <- 
  glmer(herb ~ 
          largeleaf*Sampling +  mobicenter*Sampling +
          (1|Site/alltrees/quadrats),
        family= "poisson"(link ="log"), 
        data =predcenter)
simulationOutput <- 
  simulateResiduals(fittedModel = m3, n = 1000)
plotSimulatedResiduals(simulationOutput = simulationOutput)
summary(m3)
dispersion_glmer(m3)
m3test <- 
  mixed(herb~ 
          largeleaf*Sampling +
          mobicenter*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="log"),
        type = afex_options(type = "2"),
        data = predcenter,
        method = "LRT")$anova_table
##Treatment
herbtest_treatment
predherbtest_mobipreds
m3 <- 
  glmer(herb ~ 
          Treatment*Sampling +  mobicenter*Sampling +
          (1|Site/alltrees/quadrats),
        family= "poisson"(link ="sqrt"), 
        data =predcenter)
simulationOutput <- 
  simulateResiduals(fittedModel = m3, n = 1000)
plotSimulatedResiduals(simulationOutput = simulationOutput)
summary(m3)
dispersion_glmer(m3)
m3test <- 
  mixed(herb~ 
          Treatment*Sampling +
          mobicenter*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="sqrt"),
        type = afex_options(type = "2"),
        data = predcenter,
        method = "LRT")$anova_table
