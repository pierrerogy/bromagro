#Leaf damage analysis
#Models with % damage averaged per plot

#Volume index
leafpoolmodel_largeleaf <- 
  glmer.nb(I(round((propdamage +0.01)*100))~ 
          largeleaf*Sampling + 
          (1|Site/alltrees/quadrats),
          control = glmerControl(optimizer = "bobyqa"),
        data = poolcenter)
simulationOutput <- 
  simulateResiduals(fittedModel = leafpoolmodel_largeleaf, 
                    n = 2000,
                    rank = T)
plotSimulatedResiduals(simulationOutput = simulationOutput,
                       quantreg = F,
                       rank = T)
summary(leafpoolmodel_largeleaf)
leafpooltest_largeleaf <- 
  mixed(I(round((propdamage +0.01)*100))~ 
          largeleaf*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="negative.binomial"(theta = getME(leafpoolmodel_largeleaf, "glmer.nb.theta")),
        type = afex_options(type = "2"),
        data = poolcenter,
        method = "LRT")$anova_table
visreg(leafpoolmodel_largeleaf,
       "largeleaf", by = "Sampling")


#Treatment
leafpoolmodel_treatment <- 
  glmer.nb(I(round((propdamage +0.01)*100))~ 
          Treatment*Sampling + 
          (1|Site/alltrees/quadrats),
          control = glmerControl(optimizer = "bobyqa"),
        data = poolcenter)
simulationOutput <- 
  simulateResiduals(fittedModel = leafpoolmodel_treatment, 
                    n = 2000)
plotSimulatedResiduals(simulationOutput = simulationOutput, 
                       asFactor = T,
                       quantreg = F)
summary(leafpoolmodel_treatment)
leafpooltest_treatment <- 
  mixed(I(round((propdamage +0.01)*100))~ 
          Treatment*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="negative.binomial"(theta = getME(leafpoolmodel_treatment, "glmer.nb.theta")),
        type = afex_options(type = "2"),
        data = poolcenter,
        method = "LRT")$anova_table
##plot
###visreg
visreg(leafpoolmodel_treatment,
       "Treatment", by = "Sampling")
###ggeffect\####ggeffect
leafpooleffect_treatment <- 
  ggeffect(leafpoolmodel_treatment,
           type = "re",
           x.as.factor = T,
           terms = c("Sampling", "Treatment"),
           ci.lvl = 0.95)
leafpooleffect_treatment$x <- 
  factor(leafpooleffect_treatment$x, levels = c("B", "A"))
leafpooleffect_treatment$group <- 
  factor(leafpooleffect_treatment$group, levels = c("wo", "w", "wr"))
leafpoolplot_treatment <- 
  ggplot(leafpooleffect_treatment, 
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
  ylab("Pooled leaf damage (%)") +
  scale_color_manual(name = "Treatment",
                     labels = c("Wihout", "With", "Removal"),
                     values = c("darkgreen", "saddlebrown", "ivory4")) +
  theme(legend.position = c(0.9,0.9),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))


#Herbivores
leafpoolmodel_herb <- 
  glmer.nb(I(round((propdamage +0.01)*100)) ~ 
          herbcenter*Sampling + 
          (1|Site/alltrees/quadrats),
        data = poolcenter)
simulationOutput <- 
  simulateResiduals(fittedModel = leafpoolmodel_herb, 
                    n = 2000, rank = T)
plotSimulatedResiduals(simulationOutput = simulationOutput,
                       quantreg = F,
                       rank = T)
summary(leafpoolmodel_herb)
leaftest_herb <- 
  mixed(I(round((propdamage +0.01)*100)) ~ 
          herbcenter*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="negative.binomial"(theta = getME(leafpoolmodel_herb, "glmer.nb.theta")),
        type = afex_options(type = "2"),
        data = poolcenter,
        method = "LRT")$anova_table
visreg(leafpoolmodel_herb,
       "herbcenter", by = "Sampling")

leafmodeleffect_herb <- 
  ggeffect(leafpoolmodel_herb,
           terms = c("herbcenter", "Sampling"),
           swap.pred = T,
           type = "re",
           ci.level = 0.95)
leafmodeleffect_herb$group <- 
  factor(leafmodeleffect_herb$group, levels = c("B", "A"))
col <- 
  ifelse(poolcenter$Sampling == "B",
         "darkorange2", 
         "dodgerblue4")

leafmodelplot_herb <-  
  plot(leafmodeleffect_herb,
       ci = T) + 
  geom_point(data = poolcenter,
             mapping = aes(x = herbcenter, y = jitter(round((propdamage +0.01)*100), 2)), 
             colour = col, 
             fill = col) +
  ggtitle("") + 
  xlab("Herbivore abundance") +
  ylab("Pooled leaf damage (%)") +
  scale_color_manual(labels = c("Before", "After"), 
                     values = c("darkorange2", "dodgerblue4")) +
  theme(legend.position = c(0.9,0.9),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))
