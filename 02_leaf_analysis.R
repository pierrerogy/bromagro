#Leaf damage analysis
#Models with % damage averaged per plot

#Proximity index
##Model
leafpoolmodel_largeleaf <- 
  glmer.nb(I(round((propdamage +0.01)*100))~ 
             largeleaf*Sampling + 
             (1|Site/alltrees/quadrats),
           control = glmerControl(optimizer = "bobyqa"),
           data = poolcenter)
##Assumptions
simulationOutput <- 
  simulateResiduals(fittedModel = leafpoolmodel_largeleaf, 
                    n = 2000,
                    rank = T)
plot(simulationOutput,
     quantreg = F,
     rank = T)
##Summary and tests
summary(leafpoolmodel_largeleaf)
leafpooltest_largeleaf <- 
  mixed(I(round((propdamage +0.01)*100))~ 
          largeleaf*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="negative.binomial"(theta = getME(leafpoolmodel_largeleaf, 
                                                  "glmer.nb.theta")),
        type = afex_options(type = "2"),
        data = poolcenter,
        method = "LRT")$anova_table
##Plot
###visreg
visreg(leafpoolmodel_largeleaf,
       "largeleaf", by = "Sampling")
###ggeffects
####Data
leafmodeleffect_largeleaf <- 
  ggeffect(leafpoolmodel_largeleaf,
           terms = c("largeleaf", "Sampling"),
           swap.pred = T,
           type = "re",
           ci.level = 0.95)
leafmodeleffect_largeleaf$group <- 
  factor(leafmodeleffect_largeleaf$group, levels = c("B", "A"))
col <- 
  ifelse(poolcenter$Sampling == "B",
         "goldenrod4", 
         "goldenrod")
####plot
leafpoolplot_largeleaf <-  
  plot(leafmodeleffect_largeleaf,
       ci = T,
       colors = col) + 
  geom_point(data = poolcenter,
             mapping = aes(x = largeleaf, y = jitter(round((propdamage +0.01)*100), 2)), 
             colour = col, 
             fill = col) +
  ggtitle("") + 
  xlab("Volume proximity index") +
  ylab("Pooled leaf damage (%)") +
  scale_color_manual(name = "Sampling period",
                     labels = c("Before", "After"), 
                     values = c("goldenrod4", "goldenrod")) +
  theme(legend.position = c(0.8,0.9),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))


#Tree type - Full model
##Model
leafpoolmodel_treetype <- 
  glmer.nb(I(round((propdamage +0.01)*100))~ 
             treetype*Sampling + 
             (1|Site/alltrees/quadrats),
           control = glmerControl(optimizer = "bobyqa"),
           contrasts = T,
           data = poolcenter)
##Assumptions
simulationOutput <- 
  simulateResiduals(fittedModel = leafpoolmodel_treetype, 
                    n = 2000)
plot(simulationOutput, 
     asFactor = T,
     quantreg = F)
##Summary and tests
summary(leafpoolmodel_treetype)
leafpooltest_treetype <- 
  mixed(I(round((propdamage +0.01)*100))~ 
          treetype*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="negative.binomial"(theta = getME(leafpoolmodel_treetype, 
                                                  "glmer.nb.theta")),
        type = afex_options(type = "2"),
        control = glmerControl(optimizer = "bobyqa"),
        data = poolcenter,
        method = "LRT")$anova_table
##Plot
###visreg
visreg(leafpoolmodel_treetype,
       "treetype", by = "Sampling")
###ggeffects
####Data
leafpooleffect_treetype <- 
  ggeffect(leafpoolmodel_treetype,
           type = "re",
           x.as.factor = T,
           terms = c("Sampling", "treetype"),
           ci.lvl = 0.95)
leafpooleffect_treetype$x <- 
  factor(leafpooleffect_treetype$x, levels = c("B", "A"))
leafpooleffect_treetype$group <- 
  factor(leafpooleffect_treetype$group, levels = c("wo", "w", "wr"))
####Plot
leafpoolplot_treetype <- 
  ggplot(leafpooleffect_treetype, 
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
  ylab("Pooled leaf damage (%)") +
  scale_color_manual(name = "Tree type",
                     labels = c("Without", "With", "Removal"),
                     values = c("tomato4", "dodgerblue4", "dodgerblue1")) +
  theme(legend.position = c(0.9,0.9),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))

#Tree type - Contrast models
##Observational model
###Model
leafpoolmodel_obs <- 
  glmer.nb(I(round((propdamage +0.01)*100))~ 
             treetype*Sampling + 
             (1|Site/alltrees/quadrats),
           data = poolcenter %>% 
             filter(treetype != "wr"))
###Assumptions
simulationOutput <- 
  simulateResiduals(fittedModel = leafpoolmodel_obs, 
                    n = 2000)
plot(simulationOutput, 
     asFactor = T,
     quantreg = F)
###Summary and tests
summary(leafpoolmodel_obs)
leafpooltest_obs <- 
  mixed(I(round((propdamage +0.01)*100))~ 
          treetype*Sampling + 
          (1|Site/alltrees/quadrats),
        data = poolcenter %>% 
          filter(treetype != "wr"),
        family ="negative.binomial"(theta = getME(leafpoolmodel_obs, 
                                                  "glmer.nb.theta")),
        type = afex_options(type = "2"),
        method = "LRT")$anova_table
##Experimental model
###Model
leafpoolmodel_exp <- 
  glmer.nb(I(round((propdamage +0.01)*100))~ 
             treetype*Sampling + 
             (1|Site/alltrees/quadrats),
           control = glmerControl(optimizer = "bobyqa"),
           data = poolcenter %>% 
             filter(treetype != "wo"))
###Assumptions
simulationOutput <- 
  simulateResiduals(fittedModel = leafpoolmodel_exp, 
                    n = 2000)
plot(simulationOutput, 
     asFactor = T,
     quantreg = F)
###Summary and tests
summary(leafpoolmodel_exp)
leafpooltest_exp <- 
  mixed(I(round((propdamage +0.01)*100))~ 
          treetype*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="negative.binomial"(theta = getME(leafpoolmodel_exp, 
                                                  "glmer.nb.theta")),
        control = glmerControl(optimizer = "bobyqa"),
        type = afex_options(type = "2"),
        data = poolcenter %>% 
          filter(treetype != "wo"),
        method = "LRT")$anova_table


#All herbivores
##Model
leafpoolmodel_herb <- 
  glmer.nb(I(round((propdamage +0.01)*100)) ~ 
             herbcenter*Sampling + 
             (1|Site/alltrees/quadrats),
           control = glmerControl(optimizer = "bobyqa"),
           data = poolcenter)
##Assumptions
simulationOutput <- 
  simulateResiduals(fittedModel = leafpoolmodel_herb, 
                    n = 2000, rank = T)
plot(simulationOutput,
     quantreg = F,
     rank = T)
##Summary and tests
summary(leafpoolmodel_herb)
leaftest_herb <- 
  mixed(I(round((propdamage +0.01)*100)) ~ 
          herbcenter*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="negative.binomial"(theta = getME(leafpoolmodel_herb, 
                                                  "glmer.nb.theta")),
        type = afex_options(type = "2"),
        data = poolcenter,
        method = "LRT")$anova_table
##Plot
###visreg
visreg(leafpoolmodel_herb,
       "herbcenter", by = "Sampling")
###ggeffects
####Data
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
         "goldenrod4", 
         "goldenrod")
####Plot
leafpoolplot_herb <-  
  plot(leafmodeleffect_herb,
       ci = T) + 
  geom_point(data = poolcenter,
             mapping = aes(x = herbcenter, y = jitter(round((propdamage +0.01)*100), 2)), 
             colour = col, 
             fill = col) +
  ggtitle("") + 
  xlab("Herbivore abundance") +
  ylab("Pooled leaf damage (%)") +
  scale_color_manual(name = "Sampling period",
                     labels = c("Before", "After"), 
                     values = c("goldenrod4", "goldenrod")) +
  theme(legend.position = c(0.8,0.9),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

#Leaf chewers
##Model
leafpoolmodel_chewer <- 
  glmer.nb(I(round((propdamage +0.01)*100)) ~ 
             chewercenter*Sampling + 
             (1|Site/alltrees/quadrats),
           glmerControl(optimizer = "bobyqa"),
           data = poolcenter)
##Assumptions
simulationOutput <- 
  simulateResiduals(fittedModel = leafpoolmodel_chewer, 
                    n = 2000, rank = T)
plot(simulationOutput,
     quantreg = F,
     rank = T)
##Summary and tests
summary(leafpoolmodel_chewer)
leaftest_chewer <- 
  mixed(I(round((propdamage +0.01)*100)) ~ 
          chewercenter*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="negative.binomial"(theta = getME(leafpoolmodel_chewer, 
                                                  "glmer.nb.theta")),
        type = afex_options(type = "2"),
        data = poolcenter,
        method = "LRT")$anova_table
##Plot
###visreg
visreg(leafpoolmodel_chewer,
       "chewercenter", by = "Sampling")
###ggeffects
####Data
leafmodeleffect_chewer <- 
  ggeffect(leafpoolmodel_chewer,
           terms = c("chewercenter", "Sampling"),
           swap.pred = T,
           type = "re",
           ci.level = 0.95)
leafmodeleffect_chewer$group <- 
  factor(leafmodeleffect_chewer$group, levels = c("B", "A"))
col <- 
  ifelse(poolcenter$Sampling == "B",
         "goldenrod4", 
         "goldenrod")
####Plot
leafpoolplot_chewer <-  
  plot(leafmodeleffect_chewer,
       ci = T) + 
  geom_point(data = poolcenter,
             mapping = aes(x = chewercenter, y = jitter(round((propdamage +0.01)*100), 2)), 
             colour = col, 
             fill = col) +
  ggtitle("") + 
  xlab("Leaf chewer abundance") +
  ylab("Pooled leaf damage (%)") +
  scale_color_manual(name = "Sampling period",
                     labels = c("Before", "After"), 
                     values = c("goldenrod4", "goldenrod")) +
  theme(legend.position = c(0.8,0.9),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))
