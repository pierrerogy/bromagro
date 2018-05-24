#Bromeliads on predators

# Models all arthropod abundance per quadrat  ------------------------------------------------------
#Volume index
todomodel_largeleaf <- 
  glmer.nb(todo~
          largeleaf*Sampling +
          (1|Site/alltrees/quadrats),
        data = poolcenter)
simulationOutput <- 
  simulateResiduals(fittedModel = todomodel_largeleaf, n = 1000)
plotSimulatedResiduals(simulationOutput = simulationOutput)
dispersion_glmer(todomodel_largeleaf)
summary(todomodel_largeleaf)
todotest_largeleaf <- 
  mixed(todo~ 
          largeleaf*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="negative.binomial"(theta = as.matrix(getME(todomodel_largeleaf, "theta"))[2,1]),
        type = afex_options(type = "2"),
        data = poolcenter,
        method = "LRT")$anova_table
##plot
visreg(todomodel_largeleaf,
       "largeleaf", by = "Sampling")

#Predator index
todomodel_predindex <- 
  glmer(todo~
          predindex*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="log"),
        data = poolcenter %>% file)
simulationOutput <- 
  simulateResiduals(fittedModel = todomodel_predindex, n = 1000)
plotSimulatedResiduals(simulationOutput = simulationOutput)
dispersion_glmer(todomodel_predindex)
summary(todomodel_predindex)
todotest_predindex <- 
  mixed(todo~ 
          predindex*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="log"),
        type = afex_options(type = "2"),
        data = poolcenter,
        method = "LRT")$anova_table
##plot
###visreg
visreg(todomodel_predindex,
       "predindex", by = "Sampling")
###largeleaf
todoeffect_predindex <- 
  ggeffect(todomodel_predindex,
           terms = c("predindex", "Sampling"),
           swap.pred = T,
           type = "re",
           ci.level = 0.95)
todoeffect_predindex$group <- 
  factor(todoeffect_predindex$group, levels = c("B", "A"))
col <- 
  ifelse(poolcenter$Sampling == "B",
         "darkorange2", 
         "dodgerblue4")
plot(todoeffect_predindex,
     ci = T) + 
  geom_point(data = poolcenter,
             mapping = aes(x = predindex, y = jitter(todo, 2)), 
             colour = col, 
             fill = col) +
  ggtitle("") + 
  xlab("Predator proximity index") +
  ylab("Arthropod abundance") +
  scale_color_manual(labels = c("Before", "After"), 
                     values = c("darkorange2", "dodgerblue4")) +
  theme(legend.position = c(0.9,0.9),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

#Treatment
todomodel_treatment <- 
  glmer.nb(todo ~
          Treatment*Sampling +
          (1|Site/alltrees/quadrats),
        data = poolcenter)
simulationOutput <- 
  simulateResiduals(fittedModel = todomodel_treatment, n = 1000)
plotSimulatedResiduals(simulationOutput = simulationOutput, asFactor = T)
dispersion_glmer(todomodel_treatment)
summary(todomodel_treatment)
todotest_treatment <- 
  mixed(todo~ 
          Treatment*Sampling + 
          (1|Site/alltrees/quadrats),
        family = "negative.binomial"(theta = as.matrix(getME(todomodel_treatment, "theta"))[2,1]),
        type = afex_options(type = "2"),
        data = poolcenter,
        method = "LRT")$anova_table
visreg(todomodel_treatment,
       "Treatment", by = "Sampling")

#Models all predator and parasitoid abundance per quadrat -------------------------------------
#Volume index
predparamodel_largeleaf <- 
  glmer(predpara ~
          largeleaf*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="log"),
        data = poolcenter)
simulationOutput <- 
  simulateResiduals(fittedModel = predparamodel_largeleaf, n = 1000)
plotSimulatedResiduals(simulationOutput = simulationOutput)
dispersion_glmer(predparamodel_largeleaf)
summary(predparamodel_largeleaf)
predparatest_largeleaf <- 
  mixed(predpara~ 
          largeleaf*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="log"),
        type = afex_options(type = "2"),
        data = poolcenter,
        method = "LRT")$anova_table
visreg(predparamodel_largeleaf,
       "largeleaf", by = "Sampling")

#Predator index
predparamodel_predindex <- 
  glmer(predpara ~
          predindex*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="log"),
        data = poolcenter)
simulationOutput <- 
  simulateResiduals(fittedModel = predparamodel_predindex, n = 1000)
plotSimulatedResiduals(simulationOutput = simulationOutput)
dispersion_glmer(predparamodel_predindex)
summary(predparamodel_predindex)
predparatest_predindex <- 
  mixed(predpara~ 
          predindex*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="log"),
        type = afex_options(type = "2"),
        data = poolcenter,
        method = "LRT")$anova_table
visreg(predparamodel_predindex,
       "predindex", by = "Sampling")

#Treatment
predparamodel_treatment <- 
  glmer(predpara ~
          Treatment*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="sqrt"),
        data = poolcenter)
simulationOutput <- 
  simulateResiduals(fittedModel = predparamodel_treatment, n = 1000)
plotSimulatedResiduals(simulationOutput = simulationOutput, asFactor = T)
dispersion_glmer(predparamodel_treatment)
summary(predparamodel_treatment)
predparatest_treatment <- 
  mixed(predpara~ 
          Treatment*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="sqrt"),
        type = afex_options(type = "2"),
        data = poolcenter,
        method = "LRT")$anova_table
visreg(predparamodel_treatment,
       "Treatment", by = "Sampling")





#Models all predator abundance per quadrat -------------------------------------
#Volume index
predsmodel_largeleaf <- 
  glmer(preds ~
          largeleaf*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="sqrt"),
        data = poolcenter)
simulationOutput <- 
  simulateResiduals(fittedModel = predsmodel_largeleaf, n = 1000)
plotSimulatedResiduals(simulationOutput = simulationOutput)
dispersion_glmer(predsmodel_largeleaf)
summary(predsmodel_largeleaf)
predstest_largeleaf <- 
  mixed(preds~ 
          largeleaf*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="sqrt"),
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

#Predator index
predsmodel_predindex <- 
  glmer(preds ~
          predindex*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="log"),
        data = poolcenter)
simulationOutput <- 
  simulateResiduals(fittedModel = predsmodel_predindex, n = 1000)
plotSimulatedResiduals(simulationOutput = simulationOutput)
dispersion_glmer(predsmodel_predindex)
summary(predsmodel_predindex)
predstest_predindex <- 
  mixed(preds~ 
          predindex*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="log"),
        type = afex_options(type = "2"),
        data = poolcenter,
        method = "LRT")$anova_table
##plot
visreg(predsmodel_predindex,
       "predindex", by = "Sampling")
plot(ggeffect(bromypredmodel_predindex,
              terms = c("predindex", "Sampling"),
              type = "re",
              ci.level = 0.95))
plot(effect("predindex:Sampling", 
            predsmodel_predindex),
     ci.style = "band",
     lines = list(multiline = T, 
                  lty =1, 
                  col = c("grey50", "black")),
     lattice = list(key.args =list(
       x = 0.79, 
       y = 1,
       cex = 0.75,
       between.columns = 0)),
     ylab = "Predator abundance",
     xlab = "Predator proximity index",
     type = "response",
     ylim = c(0,20),
     main = ""
)

#Treatment
predsmodel_treatment <- 
  glmer(preds ~
          Treatment*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="sqrt"),
        data = poolcenter)
simulationOutput <- 
  simulateResiduals(fittedModel = predsmodel_treatment, n = 1000)
plotSimulatedResiduals(simulationOutput = simulationOutput, asFactor = T)
summary(predsmodel_treatment)
predstest_treatment <- 
  mixed(preds~ 
          Treatment*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="sqrt"),
        type = afex_options(type = "2"),
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
                lwd = 1,
                position = position_dodge(0.3)) +
  geom_point(position = position_dodge(0.3), 
             lwd =3) +
  ggtitle("") + 
  xlab("Sampling") +
  scale_x_discrete(limit = c("B", "A"),
                   labels = c("Before", "After"),
                   expand = expand_scale(add = c(0.6)))+
  ylab("All predator abundance") +
  scale_color_manual(name = "Treatment",
                     labels = c("Wihout", "With", "Removal"),
                     values = c("darkgreen", "saddlebrown", "ivory4")) +
  theme(legend.position = c(0.9,0.9),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))

 


#Models predator kinds per quadrat ---------------------------------------
#ants
##largeleaf
antsmodel_largeleaf <- 
  glmer(ants ~
          largeleaf*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="log"),
        data = predcenter)
simulationOutput <- 
  simulateResiduals(fittedModel = antsmodel_largeleaf, n = 1000)
plotSimulatedResiduals(simulationOutput = simulationOutput)
dispersion_glmer(antsmodel_largeleaf)
summary(antsmodel_largeleaf)
antstest_largeleaf <- 
  mixed(ants~ 
          largeleaf*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="log"),
        type = afex_options(type = "2"),
        data = predcenter,
        method = "LRT")$anova_table
visreg(antsmodel_largeleaf,
       "largeleaf", by = "Sampling")
##predindex
antsmodel_predindex <- 
  glmer(ants ~
          predindex*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="log"),
        data = predcenter)
simulationOutput <- 
  simulateResiduals(fittedModel = antsmodel_predindex, n = 1000)
plotSimulatedResiduals(simulationOutput = simulationOutput)
dispersion_glmer(antsmodel_predindex)
summary(antsmodel_predindex)
antstest_predindex <- 
  mixed(ants~ 
          predindex*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="log"),
        type = afex_options(type = "2"),
        data = predcenter,
        method = "LRT")$anova_table
visreg(antsmodel_predindex,
       "predindex", by = "Sampling")
##Treatment
antsmodel_treatment <- 
  glmer(ants ~
          Treatment*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="log"),
        data = predcenter)
simulationOutput <- 
  simulateResiduals(fittedModel = antsmodel_treatment, n = 1000)
plotSimulatedResiduals(simulationOutput = simulationOutput, asFactor = T)
dispersion_glmer(antsmodel_treatment)
summary(antsmodel_treatment)
antstest_treatment <- 
  mixed(ants~ 
          Treatment*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="log"),
        type = afex_options(type = "2"),
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


#heteropred
##largeleaf
heteropredmodel_largeleaf <- 
  glmer(heteropred ~
          largeleaf*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="sqrt"),
        data = predcenter)
simulationOutput <- 
  simulateResiduals(fittedModel = heteropredmodel_largeleaf, n = 1000)
plotSimulatedResiduals(simulationOutput = simulationOutput)
dispersion_glmer(heteropredmodel_largeleaf)
summary(heteropredmodel_largeleaf)
heteropredtest_largeleaf <- 
  mixed(heteropred~ 
          largeleaf*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="sqrt"),
        type = afex_options(type = "2"),
        data = predcenter,
        method = "LRT")$anova_table
visreg(heteropredmodel_largeleaf,
       "largeleaf", by = "Sampling")
##predindex
heteropredmodel_predindex <- 
  glmer(heteropred ~
          predindex*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="sqrt"),
        data = predcenter)
simulationOutput <- 
  simulateResiduals(fittedModel = heteropredmodel_predindex, n = 1000)
plotSimulatedResiduals(simulationOutput = simulationOutput)
dispersion_glmer(heteropredmodel_predindex)
summary(heteropredmodel_predindex)
heteropredtest_predindex <- 
  mixed(heteropred~ 
          predindex*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="sqrt"),
        type = afex_options(type = "2"),
        data = predcenter,
        method = "LRT")$anova_table
visreg(heteropredmodel_predindex,
       "predindex", by = "Sampling")
##Treatment
heteropredmodel_treatment <- 
  glmer(heteropred ~
          Treatment*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="sqrt"),
        data = predcenter)
simulationOutput <- 
  simulateResiduals(fittedModel = heteropredmodel_treatment, n = 1000)
plotSimulatedResiduals(simulationOutput = simulationOutput, asFactor =T)
dispersion_glmer(heteropredmodel_treatment)
summary(heteropredmodel_treatment)
heteropredtest_treatment <- 
  mixed(heteropred~ 
          Treatment*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="sqrt"),
        type = afex_options(type = "2"),
        data = predcenter,
        method = "LRT")$anova_table
###plot
####visreg
visreg(heteropredmodel_treatment,
       "Treatment", by = "Sampling")



#huntspids
##largeleaf
huntspidsmodel_largeleaf <- 
  glmer(huntspids~
          largeleaf*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="log"),
        data = predcenter)
simulationOutput <- 
  simulateResiduals(fittedModel = huntspidsmodel_largeleaf, n = 1000)
plotSimulatedResiduals(simulationOutput = simulationOutput)
dispersion_glmer(huntspidsmodel_largeleaf)
summary(huntspidsmodel_largeleaf)
huntspidstest_largeleaf <- 
  mixed(huntspids~ 
          largeleaf*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="log"),
        type = afex_options(type = "2"),
        data = predcenter,
        method = "LRT")$anova_table
visreg(huntspidsmodel_largeleaf,
       "largeleaf", by = "Sampling")
##predindex
huntspidsmodel_predindex <- 
  glmer(huntspids ~
          predindex*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="log"),
        data = predcenter)
simulationOutput <- 
  simulateResiduals(fittedModel = huntspidsmodel_predindex, n = 1000)
plotSimulatedResiduals(simulationOutput = simulationOutput)
dispersion_glmer(huntspidsmodel_predindex)
summary(huntspidsmodel_predindex)
huntspidstest_predindex <- 
  mixed(huntspids~ 
          predindex*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="log"),
        type = afex_options(type = "2"),
        data = predcenter,
        method = "LRT")$anova_table
###plot
####visreg
visreg(huntspidsmodel_predindex,
       "predindex", by = "Sampling")
####ggeffect
plot(ggeffect(preds_predindex,
              terms = c("predindex", "Sampling"),
              type = "re",
              ci.level = 0.95))

##Treatment
huntspidsmodel_treatment <- 
  glmer(huntspids ~
          Treatment*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="sqrt"),
        data = predcenter)
simulationOutput <- 
  simulateResiduals(fittedModel = huntspidsmodel_treatment, n = 1000)
plotSimulatedResiduals(simulationOutput = simulationOutput, asFactor = T)
dispersion_glmer(huntspidsmodel_treatment)
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

#predflies
##largeleaf
predfliesmodel_largeleaf <- 
  glmer(predflies~
          largeleaf*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="log"),
        data = predcenter)
simulationOutput <- 
  simulateResiduals(fittedModel = predfliesmodel_largeleaf, n = 1000)
plotSimulatedResiduals(simulationOutput = simulationOutput)
dispersion_glmer(predfliesmodel_largeleaf)
summary(predfliesmodel_largeleaf)
predfliestest_largeleaf <- 
  mixed(predflies~ 
          largeleaf*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="log"),
        type = afex_options(type = "2"),
        data = predcenter,
        method = "LRT")$anova_table
visreg(predfliesmodel_largeleaf,
       "largeleaf", by = "Sampling")
##predindex
predfliesmodel_predindex <- 
  glmer(predflies ~
          predindex*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="log"),
        data = predcenter)
simulationOutput <- 
  simulateResiduals(fittedModel = predfliesmodel_predindex, n = 1000)
plotSimulatedResiduals(simulationOutput = simulationOutput)
dispersion_glmer(predfliesmodel_predindex)
summary(predfliesmodel_predindex)
predfliestest_predindex <- 
  mixed(predflies~ 
          predindex*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="log"),
        type = afex_options(type = "2"),
        data = predcenter,
        method = "LRT")$anova_table
visreg(predfliesmodel_predindex,
       "predindex", by = "Sampling")
##Treatment
predfliesmodel_treatment <- 
  glmer(predflies ~
          Treatment*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="sqrt"),
        data = predcenter)
simulationOutput <- 
  simulateResiduals(fittedModel = predfliesmodel_treatment, n = 1000)
plotSimulatedResiduals(simulationOutput = simulationOutput, asFactor = T)
dispersion_glmer(predfliesmodel_treatment)
summary(predfliesmodel_treatment)
predfliestest_treatment <- 
  mixed(predflies~ 
          Treatment*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="log"),
        type = afex_options(type = "2"),
        data = predcenter,
        method = "LRT")$anova_table
visreg(predfliesmodel_treatment,
       "Treatment", by = "Sampling")


#roaches
##largeleaf
roachesmodel_largeleaf <- 
  glmer(roaches~
          largeleaf*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="sqrt"),
        data = predcenter)
simulationOutput <- 
  simulateResiduals(fittedModel = roachesmodel_largeleaf, n = 1000)
plotSimulatedResiduals(simulationOutput = simulationOutput)
dispersion_glmer(roachesmodel_largeleaf)
summary(roachesmodel_largeleaf)
roachestest_largeleaf <- 
  mixed(roaches~ 
          largeleaf*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="sqrt"),
        type = afex_options(type = "2"),
        data = predcenter,
        method = "LRT")$anova_table
visreg(roachesmodel_largeleaf,
       "largeleaf", by = "Sampling")
##predindex
roachesmodel_predindex <- 
  glmer(roaches ~
          predindex*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="sqrt"),
        data = predcenter)
simulationOutput <- 
  simulateResiduals(fittedModel = roachesmodel_predindex, n = 1000)
plotSimulatedResiduals(simulationOutput = simulationOutput)
dispersion_glmer(roachesmodel_predindex)
summary(roachesmodel_predindex)
roachestest_predindex <- 
  mixed(roaches~ 
          predindex*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="sqrt"),
        type = afex_options(type = "2"),
        data = predcenter,
        method = "LRT")$anova_table
visreg(roachesmodel_predindex,
       "predindex", by = "Sampling")
##Treatment
roachesmodel_treatment <- 
  glmer(roaches ~
          Treatment*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="sqrt"),
        data = predcenter)
simulationOutput <- 
  simulateResiduals(fittedModel = roachesmodel_treatment, n = 1000)
plotSimulatedResiduals(simulationOutput = simulationOutput, asFactor = T)
dispersion_glmer(roachesmodel_treatment)
summary(roachesmodel_treatment)
roachestest_treatment <- 
  mixed(roaches~ 
          Treatment*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="sqrt"),
        type = afex_options(type = "2"),
        data = predcenter,
        method = "LRT")$anova_table
visreg(roachesmodel_treatment,
       "Treatment", by = "Sampling")

#webspids
##largeleaf
webspidsmodel_largeleaf <- 
  glmer(webspids~
          largeleaf*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="log"),
        data = predcenter)
simulationOutput <- 
  simulateResiduals(fittedModel = webspidsmodel_largeleaf, n = 1000)
plotSimulatedResiduals(simulationOutput = simulationOutput)
dispersion_glmer(webspidsmodel_largeleaf)
summary(webspidsmodel_largeleaf)
webspidstest_largeleaf <- 
  mixed(webspids~ 
          largeleaf*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="log"),
        type = afex_options(type = "2"),
        data = predcenter,
        method = "LRT")$anova_table
visreg(webspidsmodel_largeleaf,
       "largeleaf", by = "Sampling")
##predindex
webspidsmodel_predindex <- 
  glmer(webspids ~
          predindex*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="log"),
        data = predcenter)
simulationOutput <- 
  simulateResiduals(fittedModel = webspidsmodel_predindex, n = 1000)
plotSimulatedResiduals(simulationOutput = simulationOutput)
dispersion_glmer(webspidsmodel_predindex)
summary(webspidsmodel_predindex)
webspidstest_predindex <- 
  mixed(webspids~ 
          predindex*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="log"),
        type = afex_options(type = "2"),
        data = predcenter,
        method = "LRT")$anova_table
visreg(webspidsmodel_predindex,
       "predindex", by = "Sampling")
##Treatment
webspidsmodel_treatment <- 
  glmer(webspids ~
          Treatment*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="sqrt"),
        data = predcenter)
simulationOutput <- 
  simulateResiduals(fittedModel = webspidsmodel_treatment, n = 1000)
plotSimulatedResiduals(simulationOutput = simulationOutput, asFactor = T)
dispersion_glmer(webspidsmodel_treatment)
summary(webspidsmodel_treatment)
webspidstest_treatment <- 
  mixed(webspids~ 
          Treatment*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="sqrt"),
        type = afex_options(type = "2"),
        data = predcenter,
        method = "LRT")
visreg(webspidsmodel_treatment,
       "Treatment", by = "Sampling")
#Models parasitoid abundance per quadrat -------------------------------------
#Volume index
paramodel_largeleaf <- 
  glmer(para ~
          largeleaf*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="log"),
        data = poolcenter)
simulationOutput <- 
  simulateResiduals(fittedModel = paramodel_largeleaf, n = 1000)
plotSimulatedResiduals(simulationOutput = simulationOutput)
dispersion_glmer(paramodel_largeleaf)
summary(paramodel_largeleaf)
paratest_largeleaf <- 
  mixed(para~ 
          largeleaf*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="log"),
        type = afex_options(type = "2"),
        data = poolcenter,
        method = "LRT")$anova_table
visreg(paramodel_largeleaf,
       "largeleaf", by = "Sampling")

#Predator index
paramodel_predindex <- 
  glmer(para ~
          predindex*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="log"),
        data = poolcenter)
simulationOutput <- 
  simulateResiduals(fittedModel = paramodel_predindex, n = 1000)
plotSimulatedResiduals(simulationOutput = simulationOutput)
dispersion_glmer(paramodel_predindex)
summary(paramodel_predindex)
paratest_predindex <- 
  mixed(para~ 
          predindex*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="log"),
        type = afex_options(type = "2"),
        data = poolcenter,
        method = "LRT")$anova_table
visreg(paramodel_predindex,
       "predindex", by = "Sampling")

#Treatment
paramodel_treatment <- 
  glmer(para ~
          Treatment*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="sqrt"),
        data = poolcenter)
simulationOutput <- 
  simulateResiduals(fittedModel = paramodel_treatment, n = 1000)
plotSimulatedResiduals(simulationOutput = simulationOutput, asFactor = T)
dispersion_glmer(paramodel_treatment)
summary(paramodel_treatment)
paratest_treatment <- 
  mixed(para~ 
          Treatment*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="sqrt"),
        type = afex_options(type = "2"),
        data = poolcenter,
        method = "LRT")$anova_table
visreg(paramodel_treatment,
       "Treatment", by = "Sampling")






#Models bromeliad predator abundance per quadrat -------------------------------------
#Volume index
bromypredmodel_largeleaf <- 
  glmer(bromypred ~
          largeleaf*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="sqrt"),
        data = poolcenter)
simulationOutput <- 
  simulateResiduals(fittedModel =bromypredmodel_largeleaf, n = 1000)
plotSimulatedResiduals(simulationOutput = simulationOutput)
dispersion_glmer(bromypredmodel_largeleaf)
summary(bromypredmodel_largeleaf)
bromypredtest_largeleaf <- 
  mixed(bromypred~ 
          largeleaf*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="sqrt"),
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
            swap.pred = T,
           type = "re",
            ci.level = 0.95)
brompredeffect_largeleaf$group <- 
  factor(brompredeffect_largeleaf$group, levels = c("B", "A"))
col <- 
  ifelse(poolcenter$Sampling == "B",
         "darkorange2", 
         "dodgerblue4")

brompredplot_largeleaf <-  
  plot(brompredeffect_largeleaf,
       ci = T) + 
  geom_point(data = poolcenter,
             mapping = aes(x = largeleaf, y = jitter(bromypred, 2)), 
             colour = col, 
             fill = col) +
  ggtitle("") + 
  xlab("Volume proximity index") +
  ylab("Bromeliad-associated predator abundance") +
  scale_color_manual(labels = c("Before", "After"), 
                     values = c("darkorange2", "dodgerblue4")) +
  theme(legend.position = c(0.9,0.9),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))



#Predator index
bromypredmodel_predindex <- 
  glmer(bromypred ~
          predindex*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="sqrt"),
        data = poolcenter)
 simulationOutput <- 
  simulateResiduals(fittedModel =bromypredmodel_predindex, n = 1000)
plotSimulatedResiduals(simulationOutput = simulationOutput)
dispersion_glmer(bromypredmodel_predindex)
summary(bromypredmodel_predindex)
bromypredtest_predindex <- 
  mixed(bromypred~ 
          predindex*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="sqrt"),
        type = afex_options(type = "2"),
        data = poolcenter,
        method = "LRT")$anova_table
##plot
###visreg
visreg(bromypredmodel_predindex,
       "predindex", by = "Sampling")
###effects
plot(effect("predindex:Sampling", 
            bromypredmodel_predindex),
     ci.style = "band",
     lines = list(multiline = T, 
                  lty =1, 
                  col = c("grey50", "black")),
     lattice = list(key.args =list(
       x = 0.79, 
       y = 1,
       cex = 0.75,
       text=list(c("After","Before")),
       between.columns = 0)),
     axes = list(x=(list(rug = F, ticks = list(n=0, at = NULL)))),
     ylab = "Predator abundance",
     xlab.arg = "Volume proximity index",
     type = "response",
     ylim = c(0,5),
     main = ""
)

#Treatment
bromypredmodel_treatment <- 
  glmer(bromypred ~
          Treatment*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="sqrt"),
        data = poolcenter)
simulationOutput <- 
  simulateResiduals(fittedModel =bromypredmodel_treatment, n = 1000)
plotSimulatedResiduals(simulationOutput = simulationOutput, asFactor = T)
dispersion_glmer(bromypredmodel_treatment)
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
       "Sampling2", by = "Treatment")
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
                lwd = 1,
                position = position_dodge(0.3)) +
  geom_point(position = position_dodge(0.3), 
             lwd =3) +
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
        family="poisson"(link ="sqrt"),
        data = poolcenter)
simulationOutput <- 
  simulateResiduals(fittedModel =nonbromypredmodel_largeleaf, n = 1000)
plotSimulatedResiduals(simulationOutput = simulationOutput)
summary(nonbromypredmodel_largeleaf)
nonbromypredtest_largeleaf <- 
  mixed(I(preds - bromypred)~ 
          largeleaf*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="sqrt"),
        type = afex_options(type = "2"),
        data = poolcenter,
        method = "LRT")$anova_table

#Volume proximity index
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

#Models antless bromeliad predators abundance per quadrat -------------------------------------
#Volume index
bromantlessmodel_largeleaf <- 
  glmer(bromantless ~
          largeleaf*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="sqrt"),
        data = poolcenter)
simulationOutput <- 
  simulateResiduals(fittedModel =bromantlessmodel_largeleaf, n = 1000)
plotSimulatedResiduals(simulationOutput = simulationOutput)
dispersion_glmer(bromantlessmodel_largeleaf)
summary(bromantlessmodel_largeleaf)
bromantlesstest_largeleaf <- 
  mixed(bromantless~ 
          largeleaf*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="sqrt"),
        type = afex_options(type = "2"),
        data = poolcenter,
        method = "LRT")$anova_table
visreg(bromantlessmodel_largeleaf,
       "largeleaf", by = "Sampling")

#Predator index
bromantlessmodel_predindex <- 
  glmer(bromantless ~
          predindex*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="sqrt"),
        data = poolcenter)
simulationOutput <- 
  simulateResiduals(fittedModel =bromantlessmodel_predindex, n = 1000)
plotSimulatedResiduals(simulationOutput = simulationOutput)
dispersion_glmer(bromantlessmodel_predindex)
summary(bromantlessmodel_predindex)
bromantlesstest_predindex <- 
  mixed(bromantless~ 
          predindex*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="sqrt"),
        type = afex_options(type = "2"),
        data = poolcenter,
        method = "LRT")$anova_table
visreg(bromantlessmodel_predindex,
       "predindex", by = "Sampling")

#Treatment
bromantlessmodel_treatment <- 
  glmer(bromantless ~
          Treatment*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="sqrt"),
        data = poolcenter)
simulationOutput <- 
  simulateResiduals(fittedModel =bromantlessmodel_treatment, n = 1000)
plotSimulatedResiduals(simulationOutput = simulationOutput, asFactor = T)
dispersion_glmer(bromantlessmodel_treatment)
summary(bromantlessmodel_treatment)
bromantlesstest_treatment <- 
  mixed(bromantless~ 
          Treatment*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="sqrt"),
        type = afex_options(type = "2"),
        data = poolcenter,
        method = "LRT")$anova_table
visreg(bromantlessmodel_treatment,
       "Sampling", by = "Treatment")







#Models bromeliad predator kinds per quadrat -----------------------------
##Not enough opiliones (6) and predatory beetles (31) for valid modelling
#Bromeliad ants
##Volume index
bromantsmodel_largeleaf <- 
  glmer(bromants ~
          largeleaf*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="log"),
        data = predcenter)
simulationOutput <- 
  simulateResiduals(fittedModel =bromantsmodel_largeleaf, n = 1000)
plotSimulatedResiduals(simulationOutput = simulationOutput)
dispersion_glmer(bromantsmodel_largeleaf)
summary(bromantsmodel_largeleaf)
bromantstest_largeleaf <- 
  mixed(bromants~ 
          largeleaf*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="log"),
        type = afex_options(type = "2"),
        data = predcenter,
        method = "LRT")$anova_table
visreg(bromantsmodel_largeleaf,
       "largeleaf", by = "Sampling")
##Predator index
bromantsmodel_predindex <- 
  glmer(bromants ~
          predindex*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="sqrt"),
        data = predcenter)
simulationOutput <- 
  simulateResiduals(fittedModel =bromantsmodel_predindex, n = 1000)
plotSimulatedResiduals(simulationOutput = simulationOutput)
dispersion_glmer(bromantsmodel_predindex)
summary(bromantsmodel_predindex)
bromantstest_predindex <- 
  mixed(bromants~ 
          predindex*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="sqrt"),
        type = afex_options(type = "2"),
        data = predcenter,
        method = "LRT")$anova_table
visreg(bromantsmodel_predindex,
       "predindex", by = "Sampling")
##Treatment
bromantsmodel_treatment <- 
  glmer(I(round(sqrt(bromants)))~
          Treatment*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="sqrt"),
        data = predcenter)
simulationOutput <- 
  simulateResiduals(fittedModel =bromantsmodel_treatment, n = 1000)
plotSimulatedResiduals(simulationOutput = simulationOutput, asFactor = T)
dispersion_glmer(bromantsmodel_treatment)
summary(bromantsmodel_treatment)
bromantstest_treatment <- 
  mixed(I(round(sqrt(bromants)))~ 
          Treatment*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="sqrt"),
        type = afex_options(type = "2"),
        data = predcenter,
        method = "LRT")$anova_table
###plot
####visreg
visreg(bromantsmodel_treatment,
       "Sampling", by = "Treatment")
####ggeffect
 bromantseffect_treatment <- 
  ggeffect( bromantsmodel_treatment,
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
                lwd = 1,
                position = position_dodge(0.3)) +
  geom_point(position = position_dodge(0.3), 
             lwd =3) +
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
##Volume index
bromhuntspidsmodel_largeleaf <- 
  glmer(bromhuntspids ~
          largeleaf*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="log"),
        data = predcenter)
simulationOutput <- 
  simulateResiduals(fittedModel =bromhuntspidsmodel_largeleaf, n = 1000)
plotSimulatedResiduals(simulationOutput = simulationOutput)
dispersion_glmer(bromhuntspidsmodel_largeleaf)
summary(bromhuntspidsmodel_largeleaf)
bromhuntspidstest_largeleaf <- 
  mixed(bromhuntspids~ 
          largeleaf*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="log"),
        type = afex_options(type = "2"),
        data = predcenter,
        method = "LRT")$anova_table
visreg(bromhuntspidsmodel_largeleaf,
       "largeleaf", by = "Sampling")
##Predator index
bromhuntspidsmodel_predindex <- 
  glmer(bromhuntspids ~
          predindex*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="log"),
        data = predcenter)
simulationOutput <- 
  simulateResiduals(fittedModel =bromhuntspidsmodel_predindex, n = 1000)
plotSimulatedResiduals(simulationOutput = simulationOutput)
dispersion_glmer(bromhuntspidsmodel_predindex)
summary(bromhuntspidsmodel_predindex)
bromhuntspidstest_predindex <- 
  mixed(bromhuntspids~ 
          predindex*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="log"),
        type = afex_options(type = "2"),
        data = predcenter,
        method = "LRT")$anova_table
###plot
####visreg
visreg(bromhuntspidsmodel_predindex,
       "predindex", by = "Sampling")
####ggeffect
plot(ggeffect(bromhuntspidsmodel_predindex,
              terms = c("predindex", "Sampling"),
              type = "re",
              ci.level = 0.95))

##Treatment
bromhuntspidsmodel_treatment <- 
  glmer(bromhuntspids ~
          Treatment*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="log"),
        data = predcenter)
simulationOutput <- 
  simulateResiduals(fittedModel =bromhuntspidsmodel_treatment, n = 1000)
plotSimulatedResiduals(simulationOutput = simulationOutput, asFactor = T)
dispersion_glmer(bromhuntspidsmodel_treatment)
summary(bromhuntspidsmodel_treatment)
bromhuntspidstest_treatment <- 
  mixed(bromhuntspids~ 
          Treatment*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="sqrt"),
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
                lwd = 1,
                position = position_dodge(0.3)) +
  geom_point(position = position_dodge(0.3), 
             lwd =3) +
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

#Bromeliad roaches
##Volume index
bromroachesmodel_largeleaf <- 
  glmer(bromroaches ~
          largeleaf*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="sqrt"),
        data = predcenter)
simulationOutput <- 
  simulateResiduals(fittedModel =bromroachesmodel_largeleaf, n = 1000)
plotSimulatedResiduals(simulationOutput = simulationOutput)
dispersion_glmer(bromroachesmodel_largeleaf)
summary(bromroachesmodel_largeleaf)
bromroachestest_largeleaf <- 
  mixed(bromroaches~ 
          largeleaf*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="sqrt"),
        type = afex_options(type = "2"),
        data = predcenter,
        method = "LRT")$anova_table
visreg(bromroachesmodel_largeleaf,
       "largeleaf", by = "Sampling")
##Predator index
bromroachesmodel_predindex <- 
  glmer(bromroaches ~
          predindex*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="sqrt"),
        data = predcenter)
simulationOutput <- 
  simulateResiduals(fittedModel =bromroachesmodel_predindex, n = 1000)
plotSimulatedResiduals(simulationOutput = simulationOutput)
dispersion_glmer(bromroachesmodel_predindex)
summary(bromroachesmodel_predindex)
bromroachestest_predindex <- 
  mixed(bromroaches~ 
          predindex*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="sqrt"),
        type = afex_options(type = "2"),
        data = predcenter,
        method = "LRT")$anova_table
###plot
####visreg
visreg(bromroachesmodel_predindex,
       "predindex", by = "Sampling")
####ggeffect
plot(ggeffect(bromroachesmodel_predindex,
              terms = c("predindex", "Sampling"),
              type = "re",
              ci.level = 0.95))

##Treatment
bromroachesmodel_treatment <- 
  glmer(bromroaches ~
          Treatment*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="sqrt"),
        data = predcenter)
simulationOutput <- 
  simulateResiduals(fittedModel =bromroachesmodel_treatment, n = 1000)
plotSimulatedResiduals(simulationOutput = simulationOutput, asFactor = T)
dispersion_glmer(bromroachesmodel_treatment)
summary(bromroachesmodel_treatment)
bromroachestest_treatment <- 
  mixed(bromroaches~ 
          Treatment*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="sqrt"),
        type = afex_options(type = "2"),
        data = predcenter,
        method = "LRT")$anova_table
visreg(bromroachesmodel_treatment,
       "Treatment", by = "Sampling")

#Is the effect of bromeliads on ants and hunstpids restricted to bromeliad-associated ones?--------------------------------------------------------------------

#Bromeliad ants
##Volume index
nobromantsmodel_largeleaf <- 
  glmer(I(ants-bromants) ~
          largeleaf*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="sqrt"),
        data = predcenter)
simulationOutput <- 
  simulateResiduals(fittedModel =nobromantsmodel_largeleaf, n = 1000)
plotSimulatedResiduals(simulationOutput = simulationOutput)
dispersion_glmer(nobromantsmodel_largeleaf)
summary(nobromantsmodel_largeleaf)
nobromantstest_largeleaf <- 
  mixed(I(ants-bromants)~ 
          largeleaf*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="sqrt"),
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


##Predator index
nobromantsmodel_predindex <- 
  glmer(I(ants-bromants) ~
          predindex*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="sqrt"),
        data = predcenter)
simulationOutput <- 
  simulateResiduals(fittedModel =nobromantsmodel_predindex, n = 1000)
plotSimulatedResiduals(simulationOutput = simulationOutput)
dispersion_glmer(nobromantsmodel_predindex)
summary(nobromantsmodel_predindex)
nobromantstest_predindex <- 
  mixed(I(ants-bromants)~ 
          predindex*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="sqrt"),
        type = afex_options(type = "2"),
        data = predcenter,
        method = "LRT")$anova_table
visreg(nobromantsmodel_predindex,
       "predindex", by = "Sampling")
##Treatment
nobromantsmodel_treatment <- 
  glmer(I(ants-bromants) ~
          Treatment*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="sqrt"),
        data = predcenter)
simulationOutput <- 
  simulateResiduals(fittedModel =nobromantsmodel_treatment, n = 1000)
plotSimulatedResiduals(simulationOutput = simulationOutput, asFactor = T)
dispersion_glmer(nobromantsmodel_treatment)
summary(nobromantsmodel_treatment)
nobromantstest_treatment <- 
  mixed(I(ants-bromants)~ 
          Treatment*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="sqrt"),
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
                lwd = 1,
                position = position_dodge(0.3)) +
  geom_point(position = position_dodge(0.3), 
             lwd =3) +
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

#Bromeliad hunting spiders
##Volume index
nobromhuntspidsmodel_largeleaf <- 
  glmer(I(huntspids - bromhuntspids) ~
          largeleaf*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="sqrt"),
        data = predcenter)
simulationOutput <- 
  simulateResiduals(fittedModel =nobromhuntspidsmodel_largeleaf, n = 1000)
plotSimulatedResiduals(simulationOutput = simulationOutput)
dispersion_glmer(nobromhuntspidsmodel_largeleaf)
summary(nobromhuntspidsmodel_largeleaf)
nobromhuntspidstest_largeleaf <- 
  mixed(I(huntspids - bromhuntspids) ~ 
          largeleaf*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="sqrt"),
        type = afex_options(type = "2"),
        data = predcenter,
        method = "LRT")$anova_table
visreg(nobromhuntspidsmodel_largeleaf,
       "largeleaf", by = "Sampling")
##Predator index
nobromhuntspidsmodel_predindex <- 
  glmer(I(huntspids - bromhuntspids) ~
          predindex*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="sqrt"),
        data = predcenter)
simulationOutput <- 
  simulateResiduals(fittedModel =nobromhuntspidsmodel_predindex, n = 1000)
plotSimulatedResiduals(simulationOutput = simulationOutput)
dispersion_glmer(nobromhuntspidsmodel_predindex)
summary(nobromhuntspidsmodel_predindex)
nobromhuntspidstest_predindex <- 
  mixed(I(huntspids - bromhuntspids) ~ 
          predindex*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="sqrt"),
        type = afex_options(type = "2"),
        data = predcenter,
        method = "LRT")$anova_table
visreg(nobromhuntspidsmodel_predindex,
       "Sampling", by = "predindex")
##Treatment
nobromhuntspidsmodel_treatment <- 
  glmer(I(huntspids - bromhuntspids) ~
          Treatment*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="sqrt"),
        data = predcenter)
simulationOutput <- 
  simulateResiduals(fittedModel =nobromhuntspidsmodel_treatment, n = 1000)
plotSimulatedResiduals(simulationOutput = simulationOutput, asFactor = T)
dispersion_glmer(nobromhuntspidsmodel_treatment)
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
                lwd = 1,
                position = position_dodge(0.3)) +
  geom_point(position = position_dodge(0.3), 
             lwd =3) +
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

#Models tree predator abundance per quadrat -------------------------------------
#Volume index
arbopredmodel_largeleaf <- 
  glmer(arbopred ~
          largeleaf*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="sqrt"),
        data = poolcenter)
simulationOutput <- 
  simulateResiduals(fittedModel =arbopredmodel_largeleaf, n = 1000)
plotSimulatedResiduals(simulationOutput = simulationOutput)
dispersion_glmer(arbopredmodel_largeleaf)
summary(arbopredmodel_largeleaf)
arbopredtest_largeleaf <- 
  mixed(arbopred~ 
          largeleaf*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="sqrt"),
        type = afex_options(type = "2"),
        data = poolcenter,
        method = "LRT")$anova_table
visreg(arbopredmodel_largeleaf,
       "largeleaf", by = "Sampling")

#Predator index
arbopredmodel_predindex <- 
  glmer(arbopred ~
          predindex*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="sqrt"),
        data = poolcenter)
simulationOutput <- 
  simulateResiduals(fittedModel =arbopredmodel_predindex, n = 1000)
plotSimulatedResiduals(simulationOutput = simulationOutput)
dispersion_glmer(arbopredmodel_predindex)
summary(arbopredmodel_predindex)
arbopredtest_predindex <- 
  mixed(arbopred~ 
          predindex*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="sqrt"),
        type = afex_options(type = "2"),
        data = poolcenter,
        method = "LRT")$anova_table
visreg(arbopredmodel_predindex,
       "predindex", by = "Sampling")

#Treatment
arbopredmodel_treatment <- 
  glmer(arbopred ~
          Treatment*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="sqrt"),
        data = poolcenter)
simulationOutput <- 
  simulateResiduals(fittedModel =arbopredmodel_treatment, n = 1000)
plotSimulatedResiduals(simulationOutput = simulationOutput, asFactor = T)
dispersion_glmer(arbopredmodel_treatment)
summary(arbopredmodel_treatment)
arbopredtest_treatment <- 
  mixed(arbopred~ 
          Treatment*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="sqrt"),
        type = afex_options(type = "2"),
        data = poolcenter,
        method = "LRT")$anova_table
visreg(arbopredmodel_treatment,
       "Treatment", by = "Sampling")








#Models mobile predator abundance per quadrat -------------------------------------
#Volume index
mobipredmodel_largeleaf <- 
  glmer(mobipred ~
          largeleaf*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="log"),
        data = poolcenter)
simulationOutput <- 
  simulateResiduals(fittedModel =mobipredmodel_largeleaf, n = 1000)
plotSimulatedResiduals(simulationOutput = simulationOutput)
dispersion_glmer(mobipredmodel_largeleaf)
summary(mobipredmodel_largeleaf)
mobipredtest_largeleaf <- 
  mixed(mobipred~ 
          largeleaf*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="log"),
        type = afex_options(type = "2"),
        data = poolcenter,
        method = "LRT")$anova_table
visreg(mobipredmodel_largeleaf,
       "largeleaf", by = "Sampling")

#Predator index
mobipredmodel_predindex <- 
  glmer(mobipred ~
          predindex*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="log"),
        data = poolcenter)
simulationOutput <- 
  simulateResiduals(fittedModel =mobipredmodel_predindex, n = 1000)
plotSimulatedResiduals(simulationOutput = simulationOutput)
dispersion_glmer(mobipredmodel_predindex)
summary(mobipredmodel_predindex)
mobipredtest_predindex <- 
  mixed(mobipred~ 
          predindex*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="log"),
        type = afex_options(type = "2"),
        data = poolcenter,
        method = "LRT")$anova_table
visreg(mobipredmodel_predindex,
       "predindex", by = "Sampling")

#Treatment
mobipredmodel_treatment <- 
  glmer(mobipred ~
          Treatment*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="log"),
        data = poolcenter)
simulationOutput <- 
  simulateResiduals(fittedModel =mobipredmodel_treatment, n = 1000)
plotSimulatedResiduals(simulationOutput = simulationOutput, asFactor = T)
dispersion_glmer(mobipredmodel_treatment)
summary(mobipredmodel_treatment)
mobipredtest_treatment <- 
  mixed(mobipred~ 
          Treatment*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="sqrt"),
        type = afex_options(type = "2"),
        data = poolcenter,
        method = "LRT")$anova_table
visreg(mobipredmodel_treatment,
       "Treatment", by = "Sampling")
_largeleaf








#Models intraguild predation ---------------------------------------------------
#Bromeliad predators on parasitoids
intramodel_brompara <- 
  glmer(para ~
          bromcenter*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="log"),
        data = poolcenter)
simulationOutput <- 
  simulateResiduals(fittedModel =intramodel_brompara, n = 1000)
plotSimulatedResiduals(simulationOutput = simulationOutput)
dispersion_glmer(intramodel_brompara)
summary(intramodel_brompara)
intratest_brompara <- 
  mixed(para~ 
          bromcenter*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="log"),
        type = afex_options(type = "2"),
        data = poolcenter,
        method = "LRT")$anova_table
visreg(intramodel_brompara,
       "bromcenter", by = "Sampling")

#Bromeliad predators on mobile predators
intramodel_brommobipred <- 
  glmer(mobipred ~
          bromcenter*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="log"),
        data = poolcenter)
simulationOutput <- 
  simulateResiduals(fittedModel =intramodel_brommobipred, n = 1000)
plotSimulatedResiduals(simulationOutput = simulationOutput)
dispersion_glmer(intramodel_brommobipred)
summary(intramodel_brommobipred)
intratest_brommobipred <- 
  mixed(mobipred~ 
          bromcenter*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="log"),
        type = afex_options(type = "2"),
        data = poolcenter,
        method = "LRT")$anova_table
visreg(intramodel_brommobipred,
       "bromcenter", by = "Sampling")

#Bromeliad predators on tree predators
intramodel_bromarbopred <- 
  glmer(arbopred ~
          bromcenter*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="sqrt"),
        data = poolcenter)
simulationOutput <- 
  simulateResiduals(fittedModel =intramodel_brompara, n = 1000)
plotSimulatedResiduals(simulationOutput = simulationOutput)
dispersion_glmer(intramodel_bromarbopred)
summary(intramodel_bromarbopred)
intratest_bromarbopred <- 
  mixed(arbopred~ 
          bromcenter*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="sqrt"),
        type = afex_options(type = "2"),
        data = poolcenter,
        method = "LRT")$anova_table
visreg(intramodel_bromarbopred,
       "bromcenter", by = "Sampling")

#Parasitoids on tree predators
intramodel_pararbopred <- 
  glmer(arbopred ~
          paracenter*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="sqrt"),
        data = poolcenter)
simulationOutput <- 
  simulateResiduals(fittedModel =intramodel_pararbopred, n = 1000)
plotSimulatedResiduals(simulationOutput = simulationOutput)
dispersion_glmer(intramodel_pararbopred)
summary(intramodel_pararbopred)
intratest_pararbopred <- 
  mixed(arbopred~ 
          paracenter*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="sqrt"),
        type = afex_options(type = "2"),
        data = poolcenter,
        method = "LRT")$anova_table
visreg(intramodel_pararbopred,
       "paracenter", by = "Sampling")

#Parasitoids on mobile predators
intramodel_paramobipred <- 
  glmer(mobipred ~
          paracenter*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="log"),
        data = poolcenter)
simulationOutput <- 
  simulateResiduals(fittedModel =intramodel_paramobipred, n = 1000)
plotSimulatedResiduals(simulationOutput = simulationOutput)
dispersion_glmer(intramodel_paramobipred)
summary(intramodel_paramobipred)
intratest_paramobipred <- 
  mixed(mobipred~ 
          paracenter*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="log"),
        type = afex_options(type = "2"),
        data = poolcenter,
        method = "LRT")$anova_table
visreg(intramodel_paramobipred,
       "paracenter", by = "Sampling")

#Parasitoids on bromeliad predators
intramodel_parabromypred <- 
  glmer(bromypred ~
          paracenter*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="log"),
        data = poolcenter)
simulationOutput <- 
  simulateResiduals(fittedModel =intramodel_parabromypred, n = 1000)
plotSimulatedResiduals(simulationOutput = simulationOutput)
dispersion_glmer(intramodel_parabromypred)
summary(intramodel_parabromypred)
intratest_parabromypred <- 
  mixed(bromypred~ 
          paracenter*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="log"),
        type = afex_options(type = "2"),
        data = poolcenter,
        method = "LRT")$anova_table
visreg(intramodel_parabromypred,
       "paracenter", by = "Sampling")

#Tree predators on mobile predators
intramodel_arbomobipred <- 
  glmer(mobipred ~
          arbocenter*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="log"),
        data = poolcenter)
simulationOutput <- 
  simulateResiduals(fittedModel =intramodel_arbomobipred, n = 1000)
plotSimulatedResiduals(simulationOutput = simulationOutput)
dispersion_glmer(intramodel_arbomobipred)
summary(intramodel_arbomobipred)
intratest_arbomobipred <- 
  mixed(mobipred~ 
          arbocenter*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="log"),
        type = afex_options(type = "2"),
        data = poolcenter,
        method = "LRT")$anova_table
visreg(intramodel_arbomobipred,
       "arbocenter", by = "Sampling")

#Tree predators on parasitoids
intramodel_arbopara <- 
  glmer(para ~
          arbocenter*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="sqrt"),
        data = poolcenter)
simulationOutput <- 
  simulateResiduals(fittedModel =intramodel_arbopara, n = 1000)
plotSimulatedResiduals(simulationOutput = simulationOutput)
dispersion_glmer(intramodel_arbopara)
summary(intramodel_arbopara)
intratest_arbopara <- 
  mixed(para~ 
          arbocenter*Sampling+ 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="sqrt"),
        type = afex_options(type = "2"),
        data = poolcenter,
        method = "LRT")$anova_table
visreg(intramodel_arbopara,
       "arbocenter", by = "Sampling")

#Tree predators on bromeliad predators
intramodel_arbobromypred <- 
  glmer(bromypred ~
          arbocenter*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="sqrt"),
        data = poolcenter)
simulationOutput <- 
  simulateResiduals(fittedModel =intramodel_arbobromypred, n = 1000)
plotSimulatedResiduals(simulationOutput = simulationOutput)
dispersion_glmer(intramodel_arbobromypred)
summary(intramodel_arbobromypred)
intratest_arbobromypred <- 
  mixed(bromypred~ 
          arbocenter*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="log"),
        type = afex_options(type = "2"),
        data = poolcenter,
        method = "LRT")$anova_table
visreg(intramodel_arbobromypred,
       "arbocenter", by = "Sampling")

#Mobile predators on bromeliad predators
intramodel_mobibromypred <- 
  glmer(bromypred ~
          mobicenter*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="log"),
        data = poolcenter)
simulationOutput <- 
  simulateResiduals(fittedModel =intramodel_mobibromypred, n = 1000)
plotSimulatedResiduals(simulationOutput = simulationOutput)
dispersion_glmer(intramodel_mobibromypred)
summary(intramodel_mobibromypred)
intratest_mobibromypred <- 
  mixed(bromypred~ 
          mobicenter*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="log"),
        type = afex_options(type = "2"),
        data = poolcenter,
        method = "LRT")$anova_table
visreg(intramodel_mobibromypred,
       "mobicenter", by = "Sampling")

#Mobile predators on tree predators
intramodel_mobiarbopred <- 
  glmer(arbopred ~
          mobicenter*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="sqrt"),
        data = poolcenter)
simulationOutput <- 
  simulateResiduals(fittedModel =intramodel_mobiarbopred, n = 1000)
plotSimulatedResiduals(simulationOutput = simulationOutput)
dispersion_glmer(intramodel_mobiarbopred)
summary(intramodel_mobiarbopred)
intratest_mobiarbopred <- 
  mixed(arbopred~ 
          mobicenter*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="sqrt"),
        type = afex_options(type = "2"),
        data = poolcenter,
        method = "LRT")$anova_table
visreg(intramodel_mobiarbopred,
       "mobicenter", by = "Sampling")

#Mobile predators on tree predators
intramodel_mobipara <- 
  glmer(para ~
          mobicenter*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="sqrt"),
        data = poolcenter)
simulationOutput <- 
  simulateResiduals(fittedModel =intramodel_mobipara, n = 1000)
plotSimulatedResiduals(simulationOutput = simulationOutput)
dispersion_glmer(intramodel_mobipara)
summary(intramodel_mobipara)
intratest_mobipara <- 
  mixed(para~ 
          mobicenter*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="sqrt"),
        type = afex_options(type = "2"),
        data = poolcenter,
        method = "LRT")$anova_table
visreg(intramodel_mobipara,
       "mobicenter", by = "Sampling")



#Adonis all predator kinds and parasitoids per quadrat -------------------------------
#Volume index
predparadonis_largeleaf <- 
  adonis(spread_predpara[,8:17] ~
           largeleaf*Sampling,
         method = "bray",
         permutations = 2000,
         strata = spread_predpara$Site,
         data = spread_predpara)

#Predator index
predparadonis_predindex <- 
  adonis(spread_predpara[,8:17] ~
           predindex*Sampling,
         method = "bray",
         permutations = 2000,
         strata = spread_predpara$Site,
         data = spread_predpara)


#Treatment
predparadonis_treatment <- 
  adonis(spread_predpara[,8:17] ~
           Treatment*Sampling,
         method = "bray",
         permutations = 2000,
         strata = spread_predpara$Site,
         data = spread_predpara)

#Adonis all predator kinds per quadrat -------------------------------
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


#Adonis bromeliad predator kinds per quadrat -------------------------------
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



#Adonis tree predator kinds per quadrat -------------------------------
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






#Adonis mobile predator kinds per quadrat -------------------------------
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










#Adonis intraguild predation---------------------------------------------
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
##Tree predators on tree predators
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

