#Bromeliads on predators

# Models all predator abundance per quadrat -------------------------------------
#Volume proximity index
predsmodel_largeleaf <- 
  glmer(preds ~
          largeleaf*Sampling +
          (1|Site/alltrees/quadrats),
        family ="poisson"(link = "log"),
        data = poolcenter)
simulationOutput <- 
  simulateResiduals(fittedModel = predsmodel_largeleaf, 
                    n = 2000, 
                    rank = T)
plot(simulationOutput,
     quantreg = F,
     rank = T)
summary(predsmodel_largeleaf)
predstest_largeleaf <- 
  mixed(preds~ 
          largeleaf*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link = "log"),
        type = afex_options(type = "2"),
        data = poolcenter,
        method = "LRT")$anova_table
##plot
###visreg
visreg(predsmodel_largeleaf,
       "largeleaf", by = "Sampling")
###ggeffect
predseffect_largeleaf <- 
  ggeffect(predsmodel_largeleaf,
           terms = c("largeleaf", "Sampling"),
           swap.pred = T,
           type = "re",
           ci.level = 0.95)
predseffect_largeleaf$group <- 
  factor(predseffect_largeleaf$group, levels = c("B", "A"))
col <- 
  ifelse(poolcenter$Sampling == "B",
         "darkorange2", 
         "dodgerblue4")
predseffect_largeleaf$conf.low <- 
  predseffect_largeleaf$conf.low +1
predseffect_largeleaf$conf.high <- 
  predseffect_largeleaf$conf.high +1
predseffect_largeleaf$predicted <- 
  predseffect_largeleaf$predicted +1
predsplot_largeleaf <- 
  plot(predseffect_largeleaf,
       ci = T) + 
  geom_point(data = poolcenter,
             mapping = aes(x = largeleaf, y = jitter(preds +1, 2)), 
             colour = col, 
             fill = col) +
  ggtitle("") + 
  xlab("Volume proximity index") +
  ylab("All predator abundance") +
  scale_y_continuous(trans = "log",
                     breaks = c(1,7,40)) +
  scale_color_manual(labels = c("Before", "After"), 
                     values = c("darkorange2", "dodgerblue4")) +
  theme(legend.position = c(0.9 ,0.9),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

#Treatment
predsmodel_treatment <- 
  glmer(preds ~
          Treatment*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="log"),
        control=glmerControl(optimizer = "bobyqa"),
        data = poolcenter)
simulationOutput <- 
  simulateResiduals(fittedModel = predsmodel_treatment, 
                    n = 2000)
plotSimulatedResiduals(simulationOutput = simulationOutput, 
                       asFactor = T,
                       quantreg = F)
summary(predsmodel_treatment)
predstest_treatment <- 
  mixed(preds~ 
          Treatment*Sampling + 
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="log"),
        type = afex_options(type = "2"),
        control=glmerControl(optimizer = "bobyqa"),
        data = poolcenter,
        method = "LRT")$anova_table
##plot
###visreg
visreg(predsmodel_treatment,
       "Sampling", by = "Treatment")
###ggeffect
predseffect_treatment <- 
  ggeffect(predsmodel_treatment,
           type = "re",
           x.as.factor = T,
           terms = c("Sampling", "Treatment"),
           ci.lvl = 0.95)
predseffect_treatment$x <- 
  factor(predseffect_treatment$x, levels = c("B", "A"))
predseffect_treatment$group <- 
  factor(predseffect_treatment$group, levels = c("wo", "w", "wr"))
predsplot_treatment <- 
  ggplot(predseffect_treatment, 
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
  xlab("Sampling") +
  scale_x_discrete(limit = c("B", "A"),
                   labels = c("Before", "After"),
                   expand = expand_scale(add = c(0.6)))+
  ylab("Overall predator abundance") +
  scale_color_manual(name = "Treatment",
                     labels = c("Wihout", "With", "Removal"),
                     values = c("darkgreen", "saddlebrown", "ivory4")) +
  theme(legend.position = c(0.9,0.9),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))

 


# Models predator kinds per quadrat ---------------------------------------
#Ants
##Volume proximity index
antsmodel_largeleaf <- 
  glmer(ants~
          largeleaf*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="log"),
        data = predcenter)
simulationOutput <- 
  simulateResiduals(fittedModel = antsmodel_largeleaf, 
                    n = 2000,
                    rank = T)
plotSimulatedResiduals(simulationOutput = simulationOutput,
                       quantreg = F,
                       rank = T)
summary(antsmodel_largeleaf)
antstest_largeleaf <- 
  mixed(ants~ 
          sqrt(largeleaf+ 1)*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="log"),
        type = afex_options(type = "2"),
        data = predcenter,
        method = "LRT")$anova_table
###plot
####visreg
visreg(antsmodel_largeleaf,
       "largeleaf", by = "Sampling")
####ggeffect
plot(
  ggeffect(antsmodel_largeleaf,
     type = "re",
     x.as.factor = T,
     terms = c("largeleaf", "Sampling"),
     ci.lvl = 0.95))

##Treatment
antsmodel_treatment <- 
  glmer(ants ~
          Treatment*Sampling +
          (1|Site/alltrees/quadrats),
        family = "poisson"(link = "log"),
        control = glmerControl(optimizer = "bobyqa"),
        data = predcenter)
simulationOutput <- 
  simulateResiduals(fittedModel = antsmodel_treatment, 
                    n = 2000)
plotSimulatedResiduals(simulationOutput = simulationOutput, 
                       asFactor = T,
                       quantreg = F)
summary(antsmodel_treatment)
antstest_treatment <- 
  mixed(ants~ 
          Treatment*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link = "log"),
        type = afex_options(type = "2"),
        control = glmerControl(optimizer = "bobyqa"),
        data = predcenter,
        method = "LRT")
###plot
####visreg
visreg(antsmodel_treatment,
         "Sampling", by = "Treatment")
####ggeffect
antseffect_treatment <- 
  ggeffect(antsmodel_treatment,
           type = "re",
           x.as.factor = T,
           terms = c("Sampling", "Treatment"),
           ci.lvl = 0.95)
antseffect_treatment$x <- 
  factor(antseffect_treatment$x, levels = c("B", "A"))
antseffect_treatment$group <- 
  factor(antseffect_treatment$group, levels = c("wo", "w", "wr"))
antsplot_treatment <- 
  ggplot(antseffect_treatment, 
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
  ylab("Ant abundance") +
  scale_color_manual(name = "Treatment",
                     labels = c("Wihout", "With", "Removal"),
                     values = c("darkgreen", "saddlebrown", "ivory4")) +
  theme(legend.position = c(0.9,0.9),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))


#Predatory heteropterans
##Volume proximity index
heteropredmodel_largeleaf <- 
  glmer(heteropred ~
          largeleaf*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link="log"),
        data = predcenter)
simulationOutput <- 
  simulateResiduals(fittedModel = heteropredmodel_largeleaf, 
                    n = 2000,
                    rank = T)
plotSimulatedResiduals(simulationOutput = simulationOutput,
                       quantreg = F,
                       rank = T)
summary(heteropredmodel_largeleaf)
heteropredtest_largeleaf <- 
  mixed(heteropred~ 
          largeleaf*Sampling + 
          (1|Site/alltrees/quadrats),
        family="poisson"(link="log"),
        type = afex_options(type = "2"),
        data = predcenter,
        method = "LRT")$anova_table
###plot
####visreg
visreg(heteropredmodel_largeleaf,
       "largeleaf", by = "Sampling")
####ggeffect
plot(
  ggeffect(heteropredmodel_largeleaf,
           type = "re",
           x.as.factor = T,
           terms = c("largeleaf", "Sampling"),
           ci.lvl = 0.95))


##Treatment
heteropredmodel_treatment <- 
  glmer(heteropred ~
          Treatment*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="log"),
        control = glmerControl(optimizer = "bobyqa"),
        data = predcenter)
simulationOutput <- 
  simulateResiduals(fittedModel = heteropredmodel_treatment, 
                    n = 2000)
plotSimulatedResiduals(simulationOutput = simulationOutput, 
                       asFactor =T,
                       quantreg = F)
summary(heteropredmodel_treatment)
heteropredtest_treatment <- 
  mixed(heteropred~ 
          Treatment*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="log"),
        control = glmerControl(optimizer = "bobyqa"),
        type = afex_options(type = "2"),
        data = predcenter,
        method = "LRT")$anova_table
###plot
####visreg
visreg(heteropredmodel_treatment,
       "Treatment", by = "Sampling")
####ggeffect
plot(
  ggeffect(heteropredmodel_treatment,
           type = "re",
           terms = c("Sampling", "Treatment"),
           ci.lvl = 0.95))

#Hunting spiders
##Volume proximity index
huntspidsmodel_largeleaf <- 
  glmer(huntspids~
         largeleaf*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="log"),
        data = predcenter)
simulationOutput <- 
  simulateResiduals(fittedModel = huntspidsmodel_largeleaf, 
                    n = 2000,
                    rank = T)
plotSimulatedResiduals(simulationOutput = simulationOutput,
                       quantreg = F,
                       rank = T)
summary(huntspidsmodel_largeleaf)
huntspidstest_largeleaf <- 
  mixed(huntspids~ 
          largeleaf*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="log"),
        type = afex_options(type = "2"),
        data = predcenter,
        method = "LRT")$anova_table
###plot
####visreg
visreg(huntspidsmodel_largeleaf,
       "largeleaf", by = "Sampling")
####ggeffect
plot(
  ggeffect(huntspidsmodel_largeleaf,
           type = "re",
           x.as.factor = T,
           terms = c("largeleaf", "Sampling"),
           ci.lvl = 0.95))

##Treatment
huntspidsmodel_treatment <- 
  glmer(huntspids ~
          Treatment*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="sqrt"),
        data = predcenter)
simulationOutput <- 
  simulateResiduals(fittedModel = huntspidsmodel_treatment, 
                    n = 2000)
plotSimulatedResiduals(simulationOutput = simulationOutput, 
                       asFactor = T,
                       quantreg = F)
summary(huntspidsmodel_treatment)
huntspidstest_treatment <- 
  mixed(huntspids~ 
          Treatment*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="sqrt"),
        type = afex_options(type = "2"),
        data = predcenter,
        method = "LRT")$anova_table
###plot
####visreg
visreg(huntspidsmodel_treatment,
       "Treatment", by = "Sampling")
####ggeffect
huntspidseffect_treatment <- 
  ggeffect(huntspidsmodel_treatment,
           type = "re",
           x.as.factor = T,
           terms = c("Sampling", "Treatment"),
           ci.lvl = 0.95)
huntspidseffect_treatment$x <- 
  factor(huntspidseffect_treatment$x, levels = c("B", "A"))
huntspidseffect_treatment$group <- 
  factor(huntspidseffect_treatment$group, levels = c("wo", "w", "wr"))
huntspidsplot_treatment <- 
  ggplot(huntspidseffect_treatment, 
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
  ylab("Hunting spider abundance") +
  scale_color_manual(name = "Treatment",
                     labels = c("Wihout", "With", "Removal"),
                     values = c("darkgreen", "saddlebrown", "ivory4")) +
  theme(legend.position = c(0.9,0.9),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))

#Predatory flies
##Volume proximity index
predfliesmodel_largeleaf <- 
  glmer(predflies~
          largeleaf*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="log"),
        data = predcenter)
simulationOutput <- 
  simulateResiduals(fittedModel = predfliesmodel_largeleaf, 
                    n = 2000,
                    rank = T)
plotSimulatedResiduals(simulationOutput = simulationOutput,
                       quantreg = F,
                       rank = T)
summary(predfliesmodel_largeleaf)
predfliestest_largeleaf <- 
  mixed(predflies~ 
          largeleaf*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="log"),
        type = afex_options(type = "2"),
        data = predcenter,
        method = "LRT")$anova_table
###plot
####visreg
visreg(predfliesmodel_largeleaf,
       "largeleaf", by = "Sampling")
####ggeffect
plot(
  ggeffect(predfliesmodel_largeleaf,
           type = "re",
           x.as.factor = T,
           terms = c("largeleaf", "Sampling"),
           ci.lvl = 0.95))

##Treatment
predfliesmodel_treatment <- 
  glmer(predflies ~
          Treatment*Sampling +
          (1|Site/alltrees/quadrats),
        family = "poisson"(link = "sqrt"),
        data = predcenter)
simulationOutput <- 
  simulateResiduals(fittedModel = predfliesmodel_treatment, 
                    n = 2000)
plotSimulatedResiduals(simulationOutput = simulationOutput, 
                       asFactor = T,
                       quantreg = F)
summary(predfliesmodel_treatment)
predfliestest_treatment <- 
  mixed(predflies~ 
          Treatment*Sampling + 
          (1|Site/alltrees/quadrats),
        family = "poisson"(link = "sqrt"),
        type = afex_options(type = "2"),
        data = predcenter,
        method = "LRT")$anova_table
###plot
####visreg
visreg(predfliesmodel_treatment,
       "Treatment", by = "Sampling")
####ggeffect
plot(
  ggeffect(predfliesmodel_treatment,
           type = "re",
           terms = c("Sampling", "Treatment"),
           ci.lvl = 0.95))


#Cockroaches
##Volume proximity index
roachesmodel_largeleaf <- 
  glmer(roaches~
          largeleaf*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="sqrt"),
        data = predcenter)
simulationOutput <- 
  simulateResiduals(fittedModel = roachesmodel_largeleaf, 
                    n = 2000,
                    rank = T)
plotSimulatedResiduals(simulationOutput = simulationOutput,
                       quantreg = F,
                       rank = T)
summary(roachesmodel_largeleaf)
roachestest_largeleaf <- 
  mixed(roaches~ 
          largeleaf*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="sqrt"),
        type = afex_options(type = "2"),
        data = predcenter,
        method = "LRT")$anova_table
###plot
####visreg
visreg(roachesmodel_largeleaf,
       "largeleaf", by = "Sampling")
####ggeffect
plot(
  ggeffect(roachesmodel_largeleaf,
           type = "re",
           x.as.factor = T,
           terms = c("largeleaf", "Sampling"),
           ci.lvl = 0.95))

##Treatment
roachesmodel_treatment <- 
  glmer(roaches ~
          Treatment*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="sqrt"),
        data = predcenter)
simulationOutput <- 
  simulateResiduals(fittedModel = roachesmodel_treatment, 
                    n = 2000)
plotSimulatedResiduals(simulationOutput = simulationOutput, 
                       asFactor = T,
                       quantreg = F)
summary(roachesmodel_treatment)
roachestest_treatment <- 
  mixed(roaches~ 
          Treatment*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="sqrt"),
        type = afex_options(type = "2"),
        data = predcenter,
        method = "LRT")$anova_table
###plot
####visreg
visreg(roachesmodel_treatment,
       "Treatment", by = "Sampling")
####ggeffect
plot(
  ggeffect(roachesmodel_treatment,
           type = "re",
           terms = c("Sampling", "Treatment"),
           ci.lvl = 0.95))

#Web-weaving spiders
##Volume proximity index
webspidsmodel_largeleaf <- 
  glmer(webspids~
          largeleaf*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="sqrt"),
        data = predcenter)
simulationOutput <- 
  simulateResiduals(fittedModel = webspidsmodel_largeleaf, 
                    n = 2000,
                    rank = T)
plotSimulatedResiduals(simulationOutput = simulationOutput,
                       quantreg = F,
                       rank = T)
summary(webspidsmodel_largeleaf)
webspidstest_largeleaf <- 
  mixed(webspids~ 
          largeleaf*Sampling + 
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="sqrt"),
        type = afex_options(type = "2"),
        data = predcenter,
        method = "LRT")$anova_table
###plot
####visreg
visreg(webspidsmodel_largeleaf,
       "largeleaf", by = "Sampling")
####ggeffect
plot(
  ggeffect(webspidsmodel_largeleaf,
           type = "re",
           x.as.factor = T,
           terms = c("largeleaf", "Sampling"),
           ci.lvl = 0.95))

##Treatment
webspidsmodel_treatment <- 
  glmer(webspids ~
          Treatment*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="log"),
        control = glmerControl(optimizer = "bobyqa"),
        data = predcenter)
simulationOutput <- 
  simulateResiduals(fittedModel = webspidsmodel_treatment, 
                    n = 2000)
plotSimulatedResiduals(simulationOutput = simulationOutput, 
                       asFactor = T,
                       quantreg = F)
summary(webspidsmodel_treatment)
webspidstest_treatment <- 
  mixed(webspids~ 
          Treatment*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="log"),
        type = afex_options(type = "2"),
        control = glmerControl(optimizer = "bobyqa"),
        data = predcenter,
        method = "LRT")
###plot
####visreg
visreg(webspidsmodel_treatment,
       "Treatment", by = "Sampling")
####ggeffect
plot(
  ggeffect(webspidsmodel_treatment,
           type = "re",
           terms = c("Sampling", "Treatment"),
           ci.lvl = 0.95))

# Models parasitoid abundance per quadrat -------------------------------------
#Volume proximity index
paramodel_largeleaf <- 
  glmer(para ~
          largeleaf*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="log"),
        data = poolcenter)
simulationOutput <- 
  simulateResiduals(fittedModel = paramodel_largeleaf, 
                    n = 2000,
                    rank = T)
plotSimulatedResiduals(simulationOutput = simulationOutput,
                       quantreg = F,
                       rank = T)
summary(paramodel_largeleaf)
paratest_largeleaf <- 
  mixed(para~ 
          largeleaf*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="log"),
        type = afex_options(type = "2"),
        data = poolcenter,
        method = "LRT")$anova_table
##plot
###visreg
visreg(paramodel_largeleaf,
       "largeleaf", by = "Sampling")
###ggeffect
plot(
  ggeffect(paramodel_largeleaf,
           type = "re",
           x.as.factor = T,
           terms = c("largeleaf", "Sampling"),
           ci.lvl = 0.95))



#Treatment
paramodel_treatment <- 
  glmer(para ~
          Treatment*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="log"),
        data = poolcenter)
simulationOutput <- 
  simulateResiduals(fittedModel = paramodel_treatment, 
                    n = 2000)
plotSimulatedResiduals(simulationOutput = simulationOutput, 
                       asFactor = T,
                       quantreg = F)
summary(paramodel_treatment)
paratest_treatment <- 
  mixed(para~ 
          Treatment*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="log"),
        type = afex_options(type = "2"),
        data = poolcenter,
        method = "LRT")$anova_table
##plot
###visreg
visreg(paramodel_treatment,
       "Treatment", by = "Sampling")
###ggeffect
plot(
  ggeffect(paramodel_treatment,
           type = "re",
           terms = c("Sampling", "Treatment"),
           ci.lvl = 0.95))





# Models bromeliad predator abundance per quadrat largeleaf hell -------------------------------------
#Volume proximity index
bromypredmodel_largeleaf <- 
  glmer(bromypred~
          largeleaf*Sampling +
          (1|Site/alltrees/quadrats),
        family = "poisson"(link = "sqrt"),
        data = poolcenter)
simulationOutput <- 
  simulateResiduals(fittedModel =bromypredmodel_largeleaf, 
                    n = 2000,
                    rank = T)
plotSimulatedResiduals(simulationOutput = simulationOutput,
                       quantreg = F,
                       rank = T)
summary(bromypredmodel_largeleaf)
bromypredtest_largeleaf <- 
  mixed(bromypred~ 
          largeleaf*Sampling + 
          (1|Site/alltrees/quadrats),
        family = "poisson"(link = "sqrt"),
        type = afex_options(type = "2"),
        data = poolcenter,
        method = "LRT")$anova_table
##plot
###visreg
visreg(bromypredmodel_largeleaf,
       "largeleaf", by = "Sampling")
###ggeffect
brompredeffect_largeleaf <- 
  ggeffect(bromypredmodel_largeleaf,
                terms = c("largeleaf", "Sampling"),
           full.data = T,
            swap.pred = T,
           type = "re",
            ci.level = 0.95)
brompredeffect_largeleaf$group <- 
  factor(brompredeffect_largeleaf$group, levels = c("B", "A"))
col <- 
  ifelse(poolcenter$Sampling == "B",
         "darkorange2", 
         "dodgerblue4")
brompredeffect$conf.low <- 
  brompredeffect$conf.low +1
brompredeffect$conf.high <- 
  brompredeffect$conf.high +1
brompredplot_largeleaf <-  
  plot(brompredeffect_largeleaf,
       ci = T) + 
  geom_point(data = poolcenter,
             mapping = aes(x = largeleaf, y = jitter(bromypred +1, 2)), 
             colour = col, 
             fill = col) +
  ggtitle("") + 
  xlab("Volume proximity index") +
  ylab("Bromeliad-associated predator abundance") +
  scale_color_manual(labels = c("Before", "After"), 
                     values = c("darkorange2", "dodgerblue4")) +
  scale_y_continuous(trans = "log",
                     breaks= c(0, 3 , 20))+
  theme(legend.position = c(0.9,0.9),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

#Treatment
bromypredmodel_treatment <- 
  glmer(bromypred ~
          Treatment*Sampling +
          (1|Site/alltrees/quadrats),
        family = "poisson"(link = "sqrt"),
        data = poolcenter)
simulationOutput <- 
  simulateResiduals(fittedModel =bromypredmodel_treatment, 
                    n = 2000)
plotSimulatedResiduals(simulationOutput = simulationOutput, 
                       asFactor = T,
                       quantreg = F)
summary(bromypredmodel_treatment)
bromypredtest_treatment <- 
  mixed(bromypred~ 
          Treatment*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="sqrt"),
        type = afex_options(type = "2"),
        data = poolcenter,
        method = "LRT")$anova_table
##plot
###visreg
visreg(bromypredmodel_treatment,
       "Sampling", by = "Treatment")
###ggeffect
brompredeffect_treatment <- 
  ggeffect(bromypredmodel_treatment,
           type = "re",
           x.as.factor = T,
           terms = c("Sampling", "Treatment"),
           ci.lvl = 0.95)
brompredeffect_treatment$x <- 
  factor(brompredeffect_treatment$x, levels = c("B", "A"))
brompredeffect_treatment$group <- 
  factor(brompredeffect_treatment$group, levels = c("wo", "w", "wr"))
brompredplot_treatment <- 
  ggplot(brompredeffect_treatment, 
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
  xlab("Sampling") +
  scale_x_discrete(limit = c("B", "A"),
                   labels = c("Before", "After"),
                   expand = expand_scale(add = c(0.6)))+
  ylab("Bromeliad-associated predator abundance") +
  ylim(0.5,1.75)+
  scale_y_continuous(breaks = c(0.5,1,1.5))+
  scale_color_manual(name = "Treatment",
                     labels = c("Wihout", "With", "Removal"),
                     values = c("darkgreen", "saddlebrown", "ivory4")) +
  theme(legend.position = c(0.9,0.9),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))



# Models non-bromeliad predator abundance per quadrat ---------------------
#Volume proximity index
nonbromypredmodel_largeleaf <- 
  glmer(I(preds - bromypred) ~
          largeleaf*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="log"),
        data = poolcenter)
simulationOutput <- 
  simulateResiduals(fittedModel =nonbromypredmodel_largeleaf, 
                    n = 2000,
                    rank = T)
plotSimulatedResiduals(simulationOutput = simulationOutput,
                       quantreg=F,
                       rank = T)
summary(nonbromypredmodel_largeleaf)
nonbromypredtest_largeleaf <- 
  mixed(I(preds - bromypred)~ 
          largeleaf*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="log"),
        type = afex_options(type = "2"),
        data = poolcenter,
        method = "LRT")$anova_table
##plot
###visreg
visreg(nonbromypredmodel_largeleaf,
       "largeleaf", by = "Sampling")
###ggeffect
plot(
  ggeffect(nonbromypredmodel_largeleaf,
           type = "re",
           x.as.factor = T,
           terms = c("largeleaf", "Sampling"),
           ci.lvl = 0.95))

#Treatment
nonbromypredmodel_treatment <- 
  glmer(I(preds - bromypred) ~
          Treatment*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="log"),
        data = poolcenter)
simulationOutput <- 
  simulateResiduals(fittedModel =nonbromypredmodel_treatment, n = 1000)
plotSimulatedResiduals(simulationOutput = simulationOutput, asFactor = T)
summary(nonbromypredmodel_treatment)
nonbromypredtest_treatment <- 
  mixed(I(preds - bromypred)~ 
          Treatment*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="log"),
        type = afex_options(type = "2"),
        data = poolcenter,
        method = "LRT")$anova_table
##plot
###visreg
visreg(nonbromypredmodel_treatment,
       "Sampling", by = "Treatment")
###ggeffect
plot(
  ggeffect(nonbromypredmodel_treatment,
           type = "re",
           terms = c("Sampling", "Treatment"),
           ci.lvl = 0.95))

# Models bromeliad predator kinds per quadrat -----------------------------
#Bromeliad ants
##Volume proximity index
bromantsmodel_largeleaf <- 
  glmer(bromants ~
          largeleaf*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="log"),
        data = predcenter)
simulationOutput <- 
  simulateResiduals(fittedModel =bromantsmodel_largeleaf, 
                    n = 2000,
                    rank = T)
plotSimulatedResiduals(simulationOutput = simulationOutput,
                       quantreg = F,
                       rank = T)
summary(bromantsmodel_largeleaf)
bromantstest_largeleaf <- 
  mixed(bromants~ 
          largeleaf*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="log"),
        type = afex_options(type = "2"),
        data = predcenter,
        method = "LRT")$anova_table
###plot
####visreg
visreg(bromantsmodel_largeleaf,
       "largeleaf", by = "Sampling")
###ggeffect
plot(
  ggeffect(bromantsmodel_largeleaf,
           type = "re",
           x.as.factor = T,
           terms = c("largeleaf", "Sampling"),
           ci.lvl = 0.95))

##Treatment
bromantsmodel_treatment <- 
  glmer(bromants~
          Treatment*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="log"),
        control = glmerControl(optimizer = "bobyqa"),
        data = predcenter)
simulationOutput <- 
  simulateResiduals(fittedModel =bromantsmodel_treatment, 
                    n = 2000)
plotSimulatedResiduals(simulationOutput = simulationOutput, 
                       asFactor = T,
                       quantreg = F)
summary(bromantsmodel_treatment)
bromantstest_treatment <- 
  mixed(bromants~ 
          Treatment*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="log"),
        control = glmerControl(optimizer = "bobyqa"),
        type = afex_options(type = "2"),
        data = predcenter,
        method = "LRT")$anova_table
###plot
####visreg
visreg(bromantsmodel_treatment,
       "Sampling", by = "Treatment")
####ggeffect
 bromantseffect_treatment <- 
  ggeffect(bromantsmodel_treatment,
           type = "re",
           x.as.factor = T,
           terms = c("Sampling", "Treatment"),
           ci.lvl = 0.95)
 bromantseffect_treatment$x <- 
  factor( bromantseffect_treatment$x, levels = c("B", "A"))
 bromantseffect_treatment$group <- 
  factor( bromantseffect_treatment$group, levels = c("wo", "w", "wr"))
bromantsplot_treatment <- 
  ggplot(bromantseffect_treatment, 
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
  xlab("Sampling") +
  scale_x_discrete(limit = c("B", "A"),
                   labels = c("Before", "After"),
                   expand = expand_scale(add = c(0.6)))+
  ylab("Bromeliad-associated ant abundance") +
  scale_color_manual(name = "Treatment",
                     labels = c("Wihout", "With", "Removal"),
                     values = c("darkgreen", "saddlebrown", "ivory4")) +
  theme(legend.position = c(0.9,0.9),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))


#Bromeliad hunting spiders
##Volume proximity index
bromhuntspidsmodel_largeleaf <- 
  glmer(bromhuntspids ~
          largeleaf*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="log"),
        data = predcenter)
simulationOutput <- 
  simulateResiduals(fittedModel =bromhuntspidsmodel_largeleaf, 
                    n = 2000,
                    rank = T)
plotSimulatedResiduals(simulationOutput = simulationOutput,
                       quantreg = F,
                       rank = T)
summary(bromhuntspidsmodel_largeleaf)
bromhuntspidstest_largeleaf <- 
  mixed(bromhuntspids~ 
          largeleaf*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="log"),
        type = afex_options(type = "2"),
        data = predcenter,
        method = "LRT")$anova_table
###plot
####visreg
visreg(bromhuntspidsmodel_largeleaf,
       "largeleaf", by = "Sampling")
####ggeffect
plot(
  ggeffect(bromhuntspidsmodel_largeleaf,
           type = "re",
           x.as.factor = T,
           terms = c("largeleaf", "Sampling"),
           ci.lvl = 0.95))

##Treatment
bromhuntspidsmodel_treatment <- 
  glmer(bromhuntspids ~
          Treatment*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="log"),
        data = predcenter)
simulationOutput <- 
  simulateResiduals(fittedModel =bromhuntspidsmodel_treatment, 
                    n = 2000)
plotSimulatedResiduals(simulationOutput = simulationOutput, 
                       asFactor = T,
                       quantreg = F)
summary(bromhuntspidsmodel_treatment)
bromhuntspidstest_treatment <- 
  mixed(bromhuntspids~ 
          Treatment*Sampling + 
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="log"),
        type = afex_options(type = "2"),
        data = predcenter,
        method = "LRT")$anova_table
###plot
####visreg
visreg(bromhuntspidsmodel_treatment,
       "Sampling", by = "Treatment")
###ggeffect
bromhuntspidseffect_treatment <- 
  ggeffect(bromhuntspidsmodel_treatment,
           type = "re",
           x.as.factor = T,
           terms = c("Sampling", "Treatment"),
           ci.lvl = 0.95)
bromhuntspidseffect_treatment$x <- 
  factor(bromhuntspidseffect_treatment$x, levels = c("B", "A"))
bromhuntspidseffect_treatment$group <- 
  factor(bromhuntspidseffect_treatment$group, levels = c("wo", "w", "wr"))
bromhuntspidsplot_treatment <- 
  ggplot(bromhuntspidseffect_treatment, 
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
  xlab("Sampling") +
  scale_x_discrete(limit = c("B", "A"),
                   labels = c("Before", "After"),
                   expand = expand_scale(add = c(0.6)))+
  ylab("Bromeliad-associated hunting spider abundance") +
  scale_color_manual(name = "Treatment",
                     labels = c("Wihout", "With", "Removal"),
                     values = c("darkgreen", "saddlebrown", "ivory4")) +
  theme(legend.position = c(0.9,0.9),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))

#Bromeliad cockroaches
##Volume proximity index
bromroachesmodel_largeleaf <- 
  glmer(bromroaches ~
          largeleaf*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="sqrt"),
        data = predcenter)
simulationOutput <- 
  simulateResiduals(fittedModel =bromroachesmodel_largeleaf, 
                    n = 2000,
                    rank = T)
plotSimulatedResiduals(simulationOutput = simulationOutput,
                       quantreg = F,
                       rank = T)
summary(bromroachesmodel_largeleaf)
bromroachestest_largeleaf <- 
  mixed(bromroaches~ 
          largeleaf*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="sqrt"),
        type = afex_options(type = "2"),
        data = predcenter,
        method = "LRT")$anova_table
###plot
####visreg
visreg(bromroachesmodel_largeleaf,
       "largeleaf", by = "Sampling")
####ggeffect
plot(
  ggeffect(bromroachesmodel_largeleaf,
           type = "re",
           x.as.factor = T,
           terms = c("largeleaf", "Sampling"),
           ci.lvl = 0.95))

##Treatment
bromroachesmodel_treatment <- 
  glmer(bromroaches ~
          Treatment*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="log"),
        control = glmerControl(optimizer = "bobyqa"),
        data = predcenter)
simulationOutput <- 
  simulateResiduals(fittedModel =bromroachesmodel_treatment, 
                    n = 2000)
plotSimulatedResiduals(simulationOutput = simulationOutput, 
                       asFactor = T,
                       quantreg = F)
summary(bromroachesmodel_treatment)
bromroachestest_treatment <- 
  mixed(bromroaches~ 
          Treatment*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="log"),
        control = glmerControl(optimizer = "bobyqa"),
        type = afex_options(type = "2"),
        data = predcenter,
        method = "LRT")$anova_table
###plot
####visreg
visreg(bromroachesmodel_treatment,
       "Treatment", by = "Sampling")
####ggeffect

# Is the effect of bromeliads on ants and hunstpids restricted to bromeliad-associated ones?--------------------------------------------------------------------
#Ants
##Volume proximity index
nobromantsmodel_largeleaf <- 
  glmer(I(ants-bromants) ~
          largeleaf*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="log"),
        control = glmerControl(optimizer = "bobyqa"),
        data = predcenter)
simulationOutput <- 
  simulateResiduals(fittedModel =nobromantsmodel_largeleaf, 
                    n = 2000,
                    rank = T)
plotSimulatedResiduals(simulationOutput = simulationOutput,
                       quantreg = F,
                       rank = T)
summary(nobromantsmodel_largeleaf)
nobromantstest_largeleaf <- 
  mixed(I(ants-bromants)~ 
          largeleaf*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="log"),
        control = glmerControl(optimizer = "bobyqa"),
        type = afex_options(type = "2"),
        data = predcenter,
        method = "LRT")$anova_table
###plot
####visreg
visreg(nobromantsmodel_largeleaf,
       "largeleaf", by = "Sampling")
####ggeffect
plot(ggeffect(nobromantsmodel_largeleaf,
              terms = c("largeleaf", "Sampling"),
              type = "re",
              ci.level = 0.95))


##Treatment
nobromantsmodel_treatment <- 
  glmer(I(ants-bromants) ~
          Treatment*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="sqrt"),
        control = glmerControl(optimizer = "bobyqa"),
        data = predcenter)
simulationOutput <- 
  simulateResiduals(fittedModel =nobromantsmodel_treatment, 
                    n = 2000)
plotSimulatedResiduals(simulationOutput = simulationOutput, 
                       asFactor = T,
                       quantreg = F)
summary(nobromantsmodel_treatment)
nobromantstest_treatment <- 
  mixed(I(ants-bromants)~ 
          Treatment*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="sqrt"),
        control = glmerControl(optimizer = "bobyqa"),
        type = afex_options(type = "2"),
        data = predcenter,
        method = "LRT")$anova_table
###plot
####visreg
visreg(nobromantsmodel_treatment,
       "Sampling", by = "Treatment")
####ggeffect
nobromantseffect_treatment <- 
  ggeffect(nobromantsmodel_treatment,
           type = "re",
           x.as.factor = T,
           terms = c("Sampling", "Treatment"),
           ci.lvl = 0.95)
nobromantseffect_treatment$x <- 
  factor(nobromantseffect_treatment$x, levels = c("B", "A"))
nobromantseffect_treatment$group <- 
  factor(nobromantseffect_treatment$group, levels = c("wo", "w", "wr"))
nobromantsplot_treatment <- 
  ggplot(nobromantseffect_treatment, 
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
  xlab("Sampling") +
  scale_x_discrete(limit = c("B", "A"),
                   labels = c("Before", "After"),
                   expand = expand_scale(add = c(0.6)))+
  ylab("Non-bromeliad-associated ant abundance") +
  scale_color_manual(name = "Treatment",
                     labels = c("Wihout", "With", "Removal"),
                     values = c("darkgreen", "saddlebrown", "ivory4")) +
  theme(legend.position = c(0.9,0.9),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))

#Hunting spiders
##Volume proximity index
nobromhuntspidsmodel_largeleaf <- 
  glmer(I(huntspids - bromhuntspids) ~
          largeleaf*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="sqrt"),
        data = predcenter)
simulationOutput <- 
  simulateResiduals(fittedModel =nobromhuntspidsmodel_largeleaf, 
                    n = 2000,
                    rank = T)
plotSimulatedResiduals(simulationOutput = simulationOutput,
                       quantreg = F,
                       rank = T)
summary(nobromhuntspidsmodel_largeleaf)
nobromhuntspidstest_largeleaf <- 
  mixed(I(huntspids - bromhuntspids) ~ 
          largeleaf*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="sqrt"),
        type = afex_options(type = "2"),
        data = predcenter,
        method = "LRT")$anova_table
###plot
####visreg
visreg(nobromhuntspidsmodel_largeleaf,
       "largeleaf", by = "Sampling")
####ggeffect
plot(ggeffect(nobromhuntspidsmodel_largeleaf,
              terms = c("largeleaf", "Sampling"),
              type = "re",
              ci.level = 0.95))

##Treatment
nobromhuntspidsmodel_treatment <- 
  glmer(I(huntspids - bromhuntspids) ~
          Treatment*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="sqrt"),
        data = predcenter)
simulationOutput <- 
  simulateResiduals(fittedModel =nobromhuntspidsmodel_treatment, 
                    n = 2000)
plotSimulatedResiduals(simulationOutput = simulationOutput, 
                       asFactor = T,
                       quantreg = F)
summary(nobromhuntspidsmodel_treatment)
nobromhuntspidstest_treatment <- 
  mixed(I(huntspids - bromhuntspids) ~ 
          Treatment*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="sqrt"),
        type = afex_options(type = "2"),
        data = predcenter,
        method = "LRT")$anova_table
###plot
####visreg
visreg(nobromhuntspidsmodel_treatment,
       "Sampling", by = "Treatment")
####ggeffect
nobromhuntspidseffect_treatment <- 
  ggeffect(nobromhuntspidsmodel_treatment,
           type = "re",
           x.as.factor = T,
           terms = c("Sampling", "Treatment"),
           ci.lvl = 0.95)
nobromhuntspidseffect_treatment$x <- 
  factor(nobromhuntspidseffect_treatment$x, levels = c("B", "A"))
nobromhuntspidseffect_treatment$group <- 
  factor(nobromhuntspidseffect_treatment$group, levels = c("wo", "w", "wr"))
nobromhuntspidsplot_treatment <- 
  ggplot(nobromhuntspidseffect_treatment, 
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
  xlab("Sampling") +
  scale_x_discrete(limit = c("B", "A"),
                   labels = c("Before", "After"),
                   expand = expand_scale(add = c(0.6)))+
  ylab("Non-bromeliad-associated hunting spider abundance") +
  scale_color_manual(name = "Treatment",
                     labels = c("Wihout", "With", "Removal"),
                     values = c("darkgreen", "saddlebrown", "ivory4")) +
  theme(legend.position = c(0.9,0.9),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))

# Models tree predator abundance per quadrat -------------------------------------
#Volume proximity index
arbopredmodel_largeleaf <- 
  glmer.nb(arbopred ~
          largeleaf*Sampling +
          (1|Site/alltrees/quadrats),
          control = glmerControl(optimizer = "bobyqa"),
        data = poolcenter)
simulationOutput <- 
  simulateResiduals(fittedModel =arbopredmodel_largeleaf, 
                    n = 2000,
                    rank = T)
plotSimulatedResiduals(simulationOutput = simulationOutput,
                       quantreg = F,
                       rank = T)
summary(arbopredmodel_largeleaf)
arbopredtest_largeleaf <- 
  mixed(arbopred~ 
          largeleaf*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="negative.binomial"(theta = getME(arbopredmodel_largeleaf,
                                                  "glmer.nb.theta")),
        control = glmerControl(optimizer = "bobyqa"),
        type = afex_options(type = "2"),
        data = poolcenter,
        method = "LRT")$anova_table
##plot
###visreg
visreg(arbopredmodel_largeleaf,
       "largeleaf", by = "Sampling")
###ggeffect
plot(ggeffect(arbopredmodel_largeleaf,
              terms = c("largeleaf", "Sampling"),
              type = "re",
              ci.level = 0.95))


#Treatment
arbopredmodel_treatment <- 
  glmer(arbopred ~
          Treatment*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="sqrt"),
        data = poolcenter)
simulationOutput <- 
  simulateResiduals(fittedModel =arbopredmodel_treatment, 
                    n = 2000)
plotSimulatedResiduals(simulationOutput = simulationOutput, 
                       asFactor = T,
                       quantreg = F)
summary(arbopredmodel_treatment)
arbopredtest_treatment <- 
  mixed(arbopred~ 
          Treatment*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="sqrt"),
        type = afex_options(type = "2"),
        data = poolcenter,
        method = "LRT")$anova_table
##plot
###visreg
visreg(arbopredmodel_treatment,
       "Treatment", by = "Sampling")
###ggeffect
plot(
  ggeffect(arbopredmodel_treatment,
           type = "re",
           terms = c("Sampling", "Treatment"),
           ci.lvl = 0.95))









# Models mobile predator abundance per quadrat -------------------------------------
#Volume index
mobipredmodel_largeleaf <- 
  glmer(mobipred ~
          largeleaf*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="sqrt"),
        data = poolcenter)
simulationOutput <- 
  simulateResiduals(fittedModel =mobipredmodel_largeleaf, 
                    n = 2000,
                    rank = T)
plotSimulatedResiduals(simulationOutput = simulationOutput,
                       quantreg = F,
                       rank = T)
summary(mobipredmodel_largeleaf)
mobipredtest_largeleaf <- 
  mixed(mobipred~ 
          largeleaf*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="sqrt"),
        type = afex_options(type = "2"),
        data = poolcenter,
        method = "LRT")$anova_table
##plot
###visreg
visreg(mobipredmodel_largeleaf,
       "largeleaf", by = "Sampling")
###ggeffect
plot(
  ggeffect(mobipredmodel_largeleaf,
           type = "re",
           x.as.factor = T,
           terms = c("largeleaf", "Sampling"),
           ci.lvl = 0.95))


#Treatment
mobipredmodel_treatment <- 
  glmer(mobipred ~
          Treatment*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="sqrt"),
        data = poolcenter)
simulationOutput <- 
  simulateResiduals(fittedModel =mobipredmodel_treatment, n = 1000)
plotSimulatedResiduals(simulationOutput = simulationOutput, asFactor = T)
summary(mobipredmodel_treatment)
mobipredtest_treatment <- 
  mixed(mobipred~ 
          Treatment*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="sqrt"),
        type = afex_options(type = "2"),
        data = poolcenter,
        method = "LRT")$anova_table
##plot
###visreg
visreg(mobipredmodel_treatment,
       "Treatment", by = "Sampling")
###ggeffect
mobipredeffect_treatment <- 
  ggeffect(mobipredmodel_treatment,
           type = "re",
           x.as.factor = T,
           terms = c("Sampling", "Treatment"),
           ci.lvl = 0.95)
mobipredeffect_treatment$x <- 
  factor(mobipredeffect_treatment$x, levels = c("B", "A"))
mobipredeffect_treatment$group <- 
  factor(mobipredeffect_treatment$group, levels = c("wo", "w", "wr"))
mobipredplot_treatment <- 
  ggplot(mobipredeffect_treatment, 
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
  ylab("Aerial predator abundance") +
  ylim(0.5,1.75)+
  scale_y_continuous(breaks = c(0,0.5,1))+
  scale_color_manual(name = "Treatment",
                     labels = c("Wihout", "With", "Removal"),
                     values = c("darkgreen", "saddlebrown", "ivory4")) +
  theme(legend.position = c(0.9,0.9),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))









# Models intraguild predation ---------------------------------------------------
#Bromeliad predators on parasitoids
intramodel_brompara <- 
  glmer(para ~
          bromcenter*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="sqrt"),
        data = poolcenter)
simulationOutput <- 
  simulateResiduals(fittedModel =intramodel_brompara, 
                    n = 2000,
                    rank = T)
plotSimulatedResiduals(simulationOutput = simulationOutput,
                       quantreg = F,
                       rank = T)
summary(intramodel_brompara)
intratest_brompara <- 
  mixed(para~ 
          bromcenter*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="sqrt"),
        type = afex_options(type = "2"),
        data = poolcenter,
        method = "LRT")$anova_table
##plot
###visreg
visreg(intramodel_brompara,
       "bromcenter", by = "Sampling")
###ggeffect
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
         "darkorange2", 
         "dodgerblue4")
intraeffect_brompara$conf.low <- 
  intraeffect_brompara$conf.low +1
intraeffect_brompara$conf.high <- 
  intraeffect_brompara$conf.high +1
intraeffect_brompara$predicted <- 
  intraeffect_brompara$predicted +1
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
                     breaks = c(1,7,40)) +
  scale_color_manual(labels = c("Before", "After"), 
                     values = c("darkorange2", "dodgerblue4")) +
  theme(legend.position = c(0.9 ,0.9),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))


#Bromeliad predators on mobile predators
intramodel_brommobipred <- 
  glmer(mobipred ~
          bromcenter*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="log"),
        data = poolcenter)
simulationOutput <- 
  simulateResiduals(fittedModel =intramodel_brommobipred, 
                    n = 2000,
                    rank = T)
plotSimulatedResiduals(simulationOutput = simulationOutput,
                       quantreg = F,
                       rank = T)
summary(intramodel_brommobipred)
intratest_brommobipred <- 
  mixed(mobipred~ 
          bromcenter*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="log"),
        type = afex_options(type = "2"),
        data = poolcenter,
        method = "LRT")$anova_table
##plot
###visreg
visreg(intramodel_brommobipred,
       "bromcenter", by = "Sampling")
###ggeffect
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
         "darkorange2", 
         "dodgerblue4")
intraeffect_brommobipred$conf.low <- 
  intraeffect_brommobipred$conf.low +1
intraeffect_brommobipred$conf.high <- 
  intraeffect_brommobipred$conf.high +1
intraeffect_brommobipred$predicted <- 
  intraeffect_brommobipred$predicted +1
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
                     breaks = c(1,7,40)) +
  scale_color_manual(labels = c("Before", "After"), 
                     values = c("darkorange2", "dodgerblue4")) +
  theme(legend.position = c(0.9 ,0.9),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))


#Bromeliad predators on tree predators
intramodel_bromarbopred <- 
  glmer.nb(arbopred~
          bromcenter*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="sqrt"),
        control=glmerControl(optimizer = "bobyqa"),
        data = poolcenter)
simulationOutput <- 
  simulateResiduals(fittedModel =intramodel_bromarbopred, 
                    n = 2000,
                    rank = T)
plotSimulatedResiduals(simulationOutput = simulationOutput,
                       quantreg = F,
                       rank = T)
summary(intramodel_bromarbopred)
intratest_bromarbopred <- 
  mixed(arbopred~ 
          bromcenter*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="negative.binomial"(theta= getME(intramodel_bromarbopred,
                                                 "glmer.nb.nb")),
        type = afex_options(type = "2"),
        data = poolcenter,
        method = "LRT")$anova_table
##plot
###visreg
visreg(intramodel_bromarbopred,
       "bromcenter", by = "Sampling")
###ggeffect
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
         "darkorange2", 
         "dodgerblue4")
intraeffect_bromarbopred$conf.low <- 
  intraeffect_bromarbopred$conf.low +1
intraeffect_bromarbopred$conf.high <- 
  intraeffect_bromarbopred$conf.high +1
intraeffect_bromarbopred$predicted <- 
  intraeffect_bromarbopred$predicted +1
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
                     breaks = c(1,7,40)) +
  scale_color_manual(labels = c("Before", "After"), 
                     values = c("darkorange2", "dodgerblue4")) +
  theme(legend.position = c(0.9 ,0.9),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

#Parasitoids on tree predators
intramodel_paraarbopred <- 
  glmer(arbopred ~
          paracenter*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="sqrt"),
        data = poolcenter)
simulationOutput <- 
  simulateResiduals(fittedModel =intramodel_paraarbopred, 
                    n = 2000,
                    rank = T)
plotSimulatedResiduals(simulationOutput = simulationOutput,
                       quantreg = F,
                       rank = T)
summary(intramodel_paraarbopred)
intratest_paraarbopred <- 
  mixed(arbopred~ 
          paracenter*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="sqrt"),
        type = afex_options(type = "2"),
        data = poolcenter,
        method = "LRT")$anova_table
##plot
###visreg
visreg(intramodel_paraarbopred,
       "paracenter", by = "Sampling")
###ggeffect
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
         "darkorange2", 
         "dodgerblue4")
intraeffect_paraarbopred$conf.low <- 
  intraeffect_paraarbopred$conf.low +1
intraeffect_paraarbopred$conf.high <- 
  intraeffect_paraarbopred$conf.high +1
intraeffect_paraarbopred$predicted <- 
  intraeffect_paraarbopred$predicted +1
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
                     breaks = c(1,7,40)) +
  scale_color_manual(labels = c("Before", "After"), 
                     values = c("darkorange2", "dodgerblue4")) +
  theme(legend.position = c(0.9 ,0.9),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

#Parasitoids on mobile predators
intramodel_paramobipred <- 
  glmer(mobipred ~
          paracenter*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="log"),
        data = poolcenter)
simulationOutput <- 
  simulateResiduals(fittedModel =intramodel_paramobipred, 
                    n = 2000,
                    rank = T)
plotSimulatedResiduals(simulationOutput = simulationOutput,
                       quantreg = F,
                       rank = T)
summary(intramodel_paramobipred)
intratest_paramobipred <- 
  mixed(mobipred~ 
          paracenter*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="log"),
        type = afex_options(type = "2"),
        data = poolcenter,
        method = "LRT")$anova_table
##plot
###visreg
visreg(intramodel_paramobipred,
       "paracenter", by = "Sampling")
###ggeffect
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
         "darkorange2", 
         "dodgerblue4")
intraeffect_paramobipred$conf.low <- 
  intraeffect_paramobipred$conf.low +1
intraeffect_paramobipred$conf.high <- 
  intraeffect_paramobipred$conf.high +1
intraeffect_paramobipred$predicted <- 
  intraeffect_paramobipred$predicted +1
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
                     breaks = c(1,7,40)) +
  scale_color_manual(labels = c("Before", "After"), 
                     values = c("darkorange2", "dodgerblue4")) +
  theme(legend.position = c(0.9 ,0.9),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))


#Mobile predators on tree predators
intramodel_mobiarbopred <- 
  glmer(arbopred ~
          mobicenter*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="sqrt"),
        data = poolcenter)
simulationOutput <- 
  simulateResiduals(fittedModel =intramodel_mobiarbopred, 
                    n = 2000,
                    rank = T)
plotSimulatedResiduals(simulationOutput = simulationOutput,
                       quantreg = F,
                       rank = T)
summary(intramodel_mobiarbopred)
intratest_mobiarbopred <- 
  mixed(mobipred~ 
          arbocenter*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="sqrt"),
        type = afex_options(type = "2"),
        data = poolcenter,
        method = "LRT")$anova_table
##plot
###visreg
visreg(intramodel_mobiarbopred,
       "mobicenter", by = "Sampling")
###ggeffect
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
         "darkorange2", 
         "dodgerblue4")
intraeffect_mobiarbopred$conf.low <- 
  intraeffect_mobiarbopred$conf.low +1
intraeffect_mobiarbopred$conf.high <- 
  intraeffect_mobiarbopred$conf.high +1
intraeffect_mobiarbopred$predicted <- 
  intraeffect_mobiarbopred$predicted +1
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
                     breaks = c(1,7,40)) +
  scale_color_manual(labels = c("Before", "After"), 
                     values = c("darkorange2", "dodgerblue4")) +
  theme(legend.position = c(0.9 ,0.9),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

# Adonis all predator kinds per quadrat -------------------------------
#Volume index
predadonis_largeleaf <- 
  adonis(spread_pred[,8:16] ~
           largeleaf*Sampling,
         method = "bray",
         permutations = 2000,
         strata = spread_pred$Site,
         data = spread_pred)

#Predator index
predadonis_predindex <- 
  adonis(spread_pred[,8:16] ~
           predindex*Sampling,
         method = "bray",
         permutations = 2000,
         strata = spread_pred$Site,
         data = spread_pred)


#Treatment
predadonis_treatment <- 
  adonis(spread_pred[,8:16] ~
           Treatment*Sampling,
         method = "bray",
         permutations = 2000,
         strata = spread_pred$Site,
         data = spread_pred)


# Adonis bromeliad predator kinds per quadrat -------------------------------
#Volume index
brompredadonis_largeleaf <- 
  adonis(spread_brompred[,8:12] ~
           largeleaf*Sampling,
         method = "bray",
         permutations = 2000,
         strata = spread_brompred$Site,
         data = spread_brompred)

#Predator index
brompredadonis_predindex <- 
  adonis(spread_brompred[,8:12] ~
           predindex*Sampling,
         method = "bray",
         permutations = 2000,
         strata = spread_brompred$Site,
         data = spread_brompred)


#Treatment
brompredadonis_treatment <- 
  adonis(spread_brompred[,8:12] ~
           Treatment*Sampling,
         method = "bray",
         permutations = 2000,
         strata = spread_brompred$Site,
         data = spread_brompred)



# Adonis tree predator kinds per quadrat -------------------------------
#Volume index
treepredadonis_largeleaf <- 
  adonis(spread_arbopred[,8:13] ~
           largeleaf*Sampling,
         method = "bray",
         permutations = 2000,
         strata = spread_arbopred$Site,
         data = spread_arbopred)

#Predator index
treepredadonis_predindex <- 
  adonis(spread_arbopred[,8:13] ~
           predindex*Sampling,
         method = "bray",
         permutations = 2000,
         strata = spread_arbopred$Site,
         data = spread_arbopred)


#Treatment
treepredadonis_treatment <- 
  adonis(spread_arbopred[,8:13] ~
           Treatment*Sampling,
         method = "bray",
         permutations = 2000,
         strata = spread_arbopred$Site,
         data = spread_arbopred)






# Adonis mobile predator kinds per quadrat -------------------------------
#Volume index
mobipredadonis_largeleaf <- 
  adonis(spread_mobipred[,8:11] ~
           largeleaf*Sampling,
         method = "bray",
         permutations = 2000,
         strata = spread_mobipred$Site,
         data = spread_mobipred)

#Predator index
mobipredadonis_predindex <- 
  adonis(spread_mobipred[,8:11] ~
           predindex*Sampling,
         method = "bray",
         permutations = 2000,
         strata = spread_mobipred$Site,
         data = spread_mobipred)


#Treatment
mobipredadonis_treatment <- 
  adonis(spread_mobipred[,8:11] ~
           Treatment*Sampling,
         method = "bray",
         permutations = 2000,
         strata = spread_mobipred$Site,
         data = spread_mobipred)










# Adonis intraguild predation---------------------------------------------
#On bromeliad predators
##Tree predators on bromeliad predators
intradonis_bromtree <- 
  adonis(spread_brompred[,8:12] ~
           arbocenter*Sampling,
         method = "bray",
         permutations = 2000,
         strata = spread_brompred$Site,
         data = spread_brompred)
##Mobile predators on bromeliad predators
intradonis_brommobipred <- 
  adonis(spread_brompred[,8:12] ~
           mobicenter*Sampling,
         method = "bray",
         permutations = 2000,
         strata = spread_brompred$Site,
         data = spread_brompred)
##Parasitoids on bromeliad predators
intradonis_brompara <- 
  adonis(spread_brompred[,8:12] ~
           paracenter*Sampling,
         method = "bray",
         permutations = 2000,
         strata = spread_brompred$Site,
         data = spread_brompred)

#On tree predators
##Bromeliad predators on tree predators
intradonis_treebrom <- 
  adonis(spread_arbopred[,8:13] ~
           bromcenter*Sampling,
         method = "bray",
         permutations = 2000,
         strata = spread_arbopred$Site,
         data = spread_arbopred)
##Mobile predators on tree predators
intradonis_treemobipred <- 
  adonis(spread_arbopred[,8:13] ~
           mobicenter*Sampling,
         method = "bray",
         permutations = 2000,
         strata = spread_arbopred$Site,
         data = spread_arbopred)
##Parasitoids on tree predators
intradonis_treepara <- 
  adonis(spread_arbopred[,8:13] ~
           paracenter*Sampling,
         method = "bray",
         permutations = 2000,
         strata = spread_arbopred$Site,
         data = spread_arbopred)

#on mobile predators
##Tree predators on mobile predators
intradonis_mobipredtree <- 
  adonis(spread_mobipred[,8:11] ~
           arbocenter*Sampling,
         method = "bray",
         permutations = 2000,
         strata = spread_mobipred$Site,
         data = spread_mobipred)
##bromeliad predators on mobile predators
intradonis_mobipredbrom <- 
  adonis(spread_mobipred[,8:11] ~
           bromcenter*Sampling,
         method = "bray",
         permutations = 2000,
         strata = spread_mobipred$Site,
         data = spread_mobipred)

##parasitoids on mobile predators
intradonis_mobipredpara <- 
  adonis(spread_mobipred[,8:11] ~
           paracenter*Sampling,
         method = "bray",
         permutations = 2000,
         strata = spread_mobipred$Site,
         data = spread_mobipred)

