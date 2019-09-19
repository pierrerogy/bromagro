#Bromeliads on predators

# Models all predator abundance per quadrat -------------------------------------
#Volume proximity index
##Model
predsmodel_largeleaf <- 
  glmer(preds ~
          largeleaf*Sampling +
          (1|Site/alltrees/quadrats),
        family ="poisson"(link = "log"),
        data = poolcenter)
##Assumptions
simulationOutput <- 
  simulateResiduals(fittedModel = predsmodel_largeleaf, 
                    n = 2000, 
                    rank = T)
plot(simulationOutput,
     quantreg = F,
     rank = T)
##Summary and tests
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
###ggeffects
####Data
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
         "goldenrod4", 
         "goldenrod")
predseffect_largeleaf$conf.low <- 
  predseffect_largeleaf$conf.low +1
predseffect_largeleaf$conf.high <- 
  predseffect_largeleaf$conf.high +1
predseffect_largeleaf$predicted <- 
  predseffect_largeleaf$predicted +1
####Plot
predsplot_largeleaf <- 
  plot(predseffect_largeleaf,
       ci = T,
       colors = col) + 
  geom_point(data = poolcenter,
             mapping = aes(x = largeleaf, y = jitter(preds +1, 2)), 
             colour = col, 
             fill = col) +
  ggtitle("") + 
  xlab("Volume proximity index") +
  ylab("All predator abundance") +
  scale_y_continuous(trans = "log",
                     breaks = c(1,5,40),
                     limits = c(0.6, 70)) +
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
predsmodel_treetype <- 
  glmer(preds ~
          treetype*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="log"),
        control=glmerControl(optimizer = "bobyqa"),
        data = poolcenter)
##Assumptions
simulationOutput <- 
  simulateResiduals(fittedModel = predsmodel_treetype, 
                    n = 2000)
plot(simulationOutput,
     asFactor = T,
     quantreg = F)
##Summary and tests
summary(predsmodel_treetype)
predstest_treetype <- 
  mixed(preds~ 
          treetype*Sampling + 
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="log"),
        type = afex_options(type = "2"),
        control=glmerControl(optimizer = "bobyqa"),
        data = poolcenter,
        method = "LRT")$anova_table
##plot
###visreg
visreg(predsmodel_treetype,
       "Sampling", by = "treetype")
###ggeffects
####Data
predseffect_treetype <- 
  ggeffect(predsmodel_treetype,
           type = "re",
           x.as.factor = T,
           terms = c("Sampling", "treetype"),
           ci.lvl = 0.95)
predseffect_treetype$x <- 
  factor(predseffect_treetype$x, levels = c("B", "A"))
predseffect_treetype$group <- 
  factor(predseffect_treetype$group, levels = c("wo", "w", "wr"))
####Plot
predsplot_treetype <- 
  ggplot(predseffect_treetype, 
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
  ylab("Overall predator abundance") +
  scale_y_continuous(breaks = c(2,2.8,3.5)) +
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
predsmodel_obs <- 
  glmer(preds ~
          treetype*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="log"),
        control=glmerControl(optimizer = "bobyqa"),
        data = poolcenter %>% 
          filter(treetype != "wr"))
##Assumptions
simulationOutput <- 
  simulateResiduals(fittedModel = predsmodel_obs, 
                    n = 2000)
plot(simulationOutput,
     asFactor = T,
     quantreg = F)
##Summary and tests
summary(predsmodel_obs)
predstest_obs <- 
  mixed(preds~ 
          treetype*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="log"),
        type = afex_options(type = "2"),
        control=glmerControl(optimizer = "bobyqa"),
        data = poolcenter %>% 
          filter(treetype != "wr"),
        method = "LRT")$anova_table
##Observational model
###Model
predsmodel_exp <- 
  glmer(preds ~
          treetype*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="log"),
        control=glmerControl(optimizer = "bobyqa"),
        data = poolcenter %>% 
          filter(treetype != "wo"))
##Assumptions
simulationOutput <- 
  simulateResiduals(fittedModel = predsmodel_exp, 
                    n = 2000)
plot(simulationOutput,
     asFactor = T,
     quantreg = F)
##Summary and tests
summary(predsmodel_exp)
predstest_exp <- 
  mixed(preds~ 
          treetype*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="log"),
        type = afex_options(type = "2"),
        control=glmerControl(optimizer = "bobyqa"),
        data = poolcenter %>% 
          filter(treetype != "wo"),
        method = "LRT")$anova_table

# Models predator kinds per quadrat ---------------------------------------
#Ants
##Volume proximity index
###Model
antsmodel_largeleaf <- 
  glmer(ants~
          largeleaf*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="log"),
        data = poolcenter)
###Assumptions
simulationOutput <- 
  simulateResiduals(fittedModel = antsmodel_largeleaf, 
                    n = 2000,
                    rank = T)
plot(simulationOutput,
     quantreg = F,
     rank = T)
###Summary and tests
summary(antsmodel_largeleaf)
antstest_largeleaf <- 
  mixed(ants~ 
          largeleaf*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="log"),
        type = afex_options(type = "2"),
        data = poolcenter,
        method = "LRT")$anova_table
###plot
####visreg
visreg(antsmodel_largeleaf,
       "largeleaf", by = "Sampling")
####ggeffects
plot(
  ggeffect(antsmodel_largeleaf,
           type = "re",
           x.as.factor = T,
           terms = c("largeleaf", "Sampling"),
           ci.lvl = 0.95))
##Tree type - Full model
###Model
antsmodel_treetype <- 
  glmer(ants ~
          treetype*Sampling +
          (1|Site/alltrees/quadrats),
        family = "poisson"(link = "log"),
        control = glmerControl(optimizer = "bobyqa"),
        data = poolcenter)
###Assumptions
simulationOutput <- 
  simulateResiduals(fittedModel = antsmodel_treetype, 
                    n = 2000)
plot(simulationOutput,
     asFactor = T,
     quantreg = F)
###Summary and tests
summary(antsmodel_treetype)
antstest_treetype <- 
  mixed(ants~ 
          treetype*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link = "log"),
        type = afex_options(type = "2"),
        control = glmerControl(optimizer = "bobyqa"),
        data = poolcenter,
        method = "LRT")
###plot
####visreg
visreg(antsmodel_treetype,
       "Sampling", by = "treetype")
####ggeffects
#####Data
antseffect_treetype <- 
  ggeffect(antsmodel_treetype,
           type = "re",
           x.as.factor = T,
           terms = c("Sampling", "treetype"),
           ci.lvl = 0.95)
antseffect_treetype$x <- 
  factor(antseffect_treetype$x, levels = c("B", "A"))
antseffect_treetype$group <- 
  factor(antseffect_treetype$group, levels = c("wo", "w", "wr"))
#####Plot
antsplot_treetype <- 
  ggplot(antseffect_treetype, 
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
  ylab("Ant abundance") +
  scale_color_manual(name = "Tree type",
                     labels = c("Without", "With", "Removal"),
                     values = c("tomato4", "dodgerblue4", "dodgerblue1")) +
  theme(legend.position = c(0.9,0.9),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))
##Tree type - Contrast models
###Observational model
####Model
antsmodel_obs <- 
  glmer(ants ~
          treetype*Sampling +
          (1|Site/alltrees/quadrats),
        family = "poisson"(link = "log"),
        control = glmerControl(optimizer = "bobyqa"),
        data = poolcenter %>% 
          filter(treetype != "wr"))
####Assumptions
simulationOutput <- 
  simulateResiduals(fittedModel = antsmodel_obs, 
                    n = 2000)
plot(simulationOutput,
     asFactor = T,
     quantreg = F)
####Summary and tests
summary(antsmodel_obs)
antstest_obs <- 
  mixed(ants~ 
          treetype*Sampling +
          (1|Site/alltrees/quadrats),
        family ="poisson"(link = "log"),
        type = afex_options(type = "2"),
        control = glmerControl(optimizer = "bobyqa"),
        data = poolcenter %>% 
          filter(treetype != "wo"),
        method = "LRT")$anova_table
###Experimental model
####Model
antsmodel_exp <- 
  glmer(ants ~
          treetype*Sampling +
          (1|Site/alltrees/quadrats),
        family = "poisson"(link = "log"),
        control = glmerControl(optimizer = "bobyqa"),
        data = poolcenter %>% 
          filter(treetype != "wo"))
####Assumptions
simulationOutput <- 
  simulateResiduals(fittedModel = antsmodel_exp, 
                    n = 2000)
plot(simulationOutput,
     asFactor = T,
     quantreg = F)
####Summary and tests
summary(antsmodel_exp)
antstest_exp <- 
  mixed(ants~ 
          treetype*Sampling +
          (1|Site/alltrees/quadrats),
        family ="poisson"(link = "log"),
        type = afex_options(type = "2"),
        control = glmerControl(optimizer = "bobyqa"),
        data = poolcenter %>% 
          filter(treetype != "wo"),
        method = "LRT")$anova_table


#Predatory heteropterans
##Volume proximity index
###Model
heteropredmodel_largeleaf <- 
  glmer(heteropred ~
          largeleaf*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link="sqrt"),
        data = poolcenter)
###Assumptions
simulationOutput <- 
  simulateResiduals(fittedModel = heteropredmodel_largeleaf, 
                    n = 2000,
                    rank = T)
plot(simulationOutput,
     quantreg = F,
     rank = T)
###Summary and tests
summary(heteropredmodel_largeleaf)
heteropredtest_largeleaf <- 
  mixed(heteropred~ 
          largeleaf*Sampling + 
          (1|Site/alltrees/quadrats),
        family="poisson"(link="sqrt"),
        type = afex_options(type = "2"),
        data = poolcenter,
        method = "LRT")$anova_table
###plot
####visreg
visreg(heteropredmodel_largeleaf,
       "largeleaf", by = "Sampling")
####ggeffects
plot(
  ggeffect(heteropredmodel_largeleaf,
           type = "re",
           x.as.factor = T,
           terms = c("largeleaf", "Sampling"),
           ci.lvl = 0.95))
##Tree type - Full model
###Model
heteropredmodel_treetype <- 
  glmer(heteropred ~
          treetype*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="log"),
        control = glmerControl(optimizer = "bobyqa"),
        data = poolcenter)
###Assumptions
simulationOutput <- 
  simulateResiduals(fittedModel = heteropredmodel_treetype, 
                    n = 2000)
plot(simulationOutput,
     asFactor =T,
     quantreg = F)
###Summary and tests
summary(heteropredmodel_treetype)
heteropredtest_treetype <- 
  mixed(heteropred~ 
          treetype*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="log"),
        control = glmerControl(optimizer = "bobyqa"),
        type = afex_options(type = "2"),
        data = poolcenter,
        method = "LRT")$anova_table
###plot
####visreg
visreg(heteropredmodel_treetype,
       "treetype", by = "Sampling")
####ggeffects
plot(
  ggeffect(heteropredmodel_treetype,
           type = "re",
           terms = c("Sampling", "treetype"),
           ci.lvl = 0.95))
##Tree type - Contrast models
###Observational model
####Model
heteropredmodel_obs <- 
  glmer(heteropred ~
          treetype*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="log"),
        control = glmerControl(optimizer = "bobyqa"),
        data = poolcenter %>% 
          filter(treetype != "wr"))
####Assumptions
simulationOutput <- 
  simulateResiduals(fittedModel = heteropredmodel_obs, 
                    n = 2000)
plot(simulationOutput,
     asFactor =T,
     quantreg = F)
####Summary and tests
summary(heteropredmodel_obs)
heteropredtest_obs <- 
  mixed(heteropred~ 
          treetype*Sampling +
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="log"),
        control = glmerControl(optimizer = "bobyqa"),
        type = afex_options(type = "2"),
        data = poolcenter%>% 
          filter(treetype != "wr"),
        method = "LRT")$anova_table
###Experimental model
####Model
heteropredmodel_exp <- 
  glmer(heteropred ~
          treetype*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="log"),
        control = glmerControl(optimizer = "bobyqa"),
        data = poolcenter %>% 
          filter(treetype != "wo"))
####Assumptions
simulationOutput <- 
  simulateResiduals(fittedModel = heteropredmodel_exp, 
                    n = 2000)
plot(simulationOutput,
     asFactor =T,
     quantreg = F)
####Summary and tests
summary(heteropredmodel_exp)
heteropredtest_exp <- 
  mixed(heteropred~ 
          treetype*Sampling +
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="log"),
        control = glmerControl(optimizer = "bobyqa"),
        type = afex_options(type = "2"),
        data = poolcenter%>% 
          filter(treetype != "wo"),
        method = "LRT")$anova_table

#Hunting spiders
##Volume proximity index
###Model
huntspidsmodel_largeleaf <- 
  glmer(huntspids~
          largeleaf*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="log"),
        data = poolcenter)
###Assumptions
simulationOutput <- 
  simulateResiduals(fittedModel = huntspidsmodel_largeleaf, 
                    n = 2000,
                    rank = T)
plot(simulationOutput,
     quantreg = F,
     rank = T)
###Summary and tests
summary(huntspidsmodel_largeleaf)
huntspidstest_largeleaf <- 
  mixed(huntspids~ 
          largeleaf*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="log"),
        type = afex_options(type = "2"),
        data = poolcenter,
        method = "LRT")$anova_table
###plot
####visreg
visreg(huntspidsmodel_largeleaf,
       "largeleaf", by = "Sampling")
####ggeffects
plot(
  ggeffect(huntspidsmodel_largeleaf,
           type = "re",
           x.as.factor = T,
           terms = c("largeleaf", "Sampling"),
           ci.lvl = 0.95))
##Tree type - Full model
###Model
huntspidsmodel_treetype <- 
  glmer(huntspids ~
          treetype*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="sqrt"),
        data = poolcenter)
###Assumptions
simulationOutput <- 
  simulateResiduals(fittedModel = huntspidsmodel_treetype, 
                    n = 2000)
plot(simulationOutput,
     asFactor = T,
     quantreg = F)
###Summary and tests
summary(huntspidsmodel_treetype)
huntspidstest_treetype <- 
  mixed(huntspids~ 
          treetype*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="sqrt"),
        type = afex_options(type = "2"),
        data = poolcenter,
        method = "LRT")$anova_table
###plot
####visreg
visreg(huntspidsmodel_treetype,
       "treetype", by = "Sampling")
####ggeffects
#####Data
huntspidseffect_treetype <- 
  ggeffect(huntspidsmodel_treetype,
           type = "re",
           x.as.factor = T,
           terms = c("Sampling", "treetype"),
           ci.lvl = 0.95)
huntspidseffect_treetype$x <- 
  factor(huntspidseffect_treetype$x, levels = c("B", "A"))
huntspidseffect_treetype$group <- 
  factor(huntspidseffect_treetype$group, levels = c("wo", "w", "wr"))
#####Data
huntspidsplot_treetype <- 
  ggplot(huntspidseffect_treetype, 
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
  ylab("Hunting spider abundance") +
  scale_color_manual(name = "Tree type",
                     labels = c("Without", "With", "Removal"),
                     values = c("tomato4", "dodgerblue4", "dodgerblue1")) +
  theme(legend.position = c(0.9,0.9),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))
##Tree type - Contrast models
###Observational model
####Model
huntspidsmodel_obs <- 
  glmer(huntspids ~
          treetype*Sampling + 
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="sqrt"),
        data = poolcenter %>% 
          filter(treetype != "wr"))
####Assumptions
simulationOutput <- 
  simulateResiduals(fittedModel = huntspidsmodel_obs, 
                    n = 2000)
plot(simulationOutput,
     asFactor = T,
     quantreg = F)
####Summary and tests
summary(huntspidsmodel_obs)
huntspidstest_obs <- 
  mixed(huntspids~ 
          treetype*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="sqrt"),
        type = afex_options(type = "2"),
        data = poolcenter %>% 
          filter(treetype != "wr"),
        method = "LRT")$anova_table
###Experimental model
####Model
huntspidsmodel_exp <- 
  glmer(huntspids ~
          treetype*Sampling + 
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="sqrt"),
        data = poolcenter %>% 
          filter(treetype != "wo"))
####Assumptions
simulationOutput <- 
  simulateResiduals(fittedModel = huntspidsmodel_exp, 
                    n = 2000)
plot(simulationOutput,
     asFactor = T,
     quantreg = F)
####Summary and tests
summary(huntspidsmodel_exp)
huntspidstest_exp <- 
  mixed(huntspids~ 
          treetype*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="sqrt"),
        type = afex_options(type = "2"),
        data = poolcenter %>% 
          filter(treetype != "wo"),
        method = "LRT")$anova_table

#Predatory flies
##Volume proximity index
###Model
predfliesmodel_largeleaf <- 
  glmer(predflies~
          largeleaf*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="log"),
        data = poolcenter)
###Assumptions
simulationOutput <- 
  simulateResiduals(fittedModel = predfliesmodel_largeleaf, 
                    n = 2000,
                    rank = T)
plot(simulationOutput,
     quantreg = F,
     rank = T)
###Summary and tests
summary(predfliesmodel_largeleaf)
predfliestest_largeleaf <- 
  mixed(predflies~ 
          largeleaf*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="log"),
        type = afex_options(type = "2"),
        data = poolcenter,
        method = "LRT")$anova_table
###plot
####visreg
visreg(predfliesmodel_largeleaf,
       "largeleaf", by = "Sampling")
####ggeffects
plot(
  ggeffect(predfliesmodel_largeleaf,
           type = "re",
           x.as.factor = T,
           terms = c("largeleaf", "Sampling"),
           ci.lvl = 0.95))
##Tree type - Full model
###Model
predfliesmodel_treetype <- 
  glmer(predflies ~
          treetype*Sampling +
          (1|Site/alltrees/quadrats),
        family = "poisson"(link = "log"),
        data = poolcenter)
###Assumptions
simulationOutput <- 
  simulateResiduals(fittedModel = predfliesmodel_treetype, 
                    n = 2000)
plot(simulationOutput, 
     asFactor = T,
     quantreg = F)
###Summary and tests
summary(predfliesmodel_treetype)
predfliestest_treetype <- 
  mixed(predflies~ 
          treetype*Sampling + 
          (1|Site/alltrees/quadrats),
        family = "poisson"(link = "log"),
        type = afex_options(type = "2"),
        data = poolcenter,
        method = "LRT")$anova_table
###plot
####visreg
visreg(predfliesmodel_treetype,
       "treetype", by = "Sampling")
####ggeffects
plot(
  ggeffect(predfliesmodel_treetype,
           type = "re",
           terms = c("Sampling", "treetype"),
           ci.lvl = 0.95))
##Tree type - Contrast models
###Observational model
####Model
predfliesmodel_obs <- 
  glmer(predflies ~
          treetype*Sampling +
          (1|Site/alltrees/quadrats),
        family = "poisson"(link = "log"),
        data = poolcenter%>% 
          filter(treetype != "wr"))
####Assumptions
simulationOutput <- 
  simulateResiduals(fittedModel = predfliesmodel_obs, 
                    n = 2000)
plot(simulationOutput, 
     asFactor = T,
     quantreg = F)
####Summary and tests
summary(predfliesmodel_obs)
predfliestest_obs <- 
  mixed(predflies~ 
          treetype*Sampling + 
          (1|Site/alltrees/quadrats),
        family = "poisson"(link = "log"),
        type = afex_options(type = "2"),
        data = poolcenter %>% 
          filter(treetype != "wr"),
        method = "LRT")$anova_table
###Experimental model
####Model
predfliesmodel_exp <- 
  glmer(predflies ~
          treetype*Sampling +
          (1|Site/alltrees/quadrats),
        family = "poisson"(link = "log"),
        data = poolcenter%>% 
          filter(treetype != "wo"))
####Assumptions
simulationOutput <- 
  simulateResiduals(fittedModel = predfliesmodel_exp, 
                    n = 2000)
plot(simulationOutput, 
     asFactor = T,
     quantreg = F)
####Summary and tests
summary(predfliesmodel_exp)
predfliestest_exp <- 
  mixed(predflies~ 
          treetype*Sampling + 
          (1|Site/alltrees/quadrats),
        family = "poisson"(link = "log"),
        type = afex_options(type = "2"),
        data = poolcenter %>% 
          filter(treetype != "wo"),
        method = "LRT")$anova_table

#Cockroaches
##Volume proximity index
###Model
roachesmodel_largeleaf <- 
  glmer(roaches~
          largeleaf*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="sqrt"),
        data = poolcenter)
###Assumptions
simulationOutput <- 
  simulateResiduals(fittedModel = roachesmodel_largeleaf, 
                    n = 2000,
                    rank = T)
plot(simulationOutput,
     quantreg = F,
     rank = T)
###Summary and tests
summary(roachesmodel_largeleaf)
roachestest_largeleaf <- 
  mixed(roaches~ 
          largeleaf*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="sqrt"),
        type = afex_options(type = "2"),
        data = poolcenter,
        method = "LRT")$anova_table
###plot
####visreg
visreg(roachesmodel_largeleaf,
       "largeleaf", by = "Sampling")
####ggeffects
plot(
  ggeffect(roachesmodel_largeleaf,
           type = "re",
           x.as.factor = T,
           terms = c("largeleaf", "Sampling"),
           ci.lvl = 0.95))
##Tree type - Full model
###Model
roachesmodel_treetype <- 
  glmer(roaches ~
          treetype*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="sqrt"),
        data = poolcenter)
###Assumptions
simulationOutput <- 
  simulateResiduals(fittedModel = roachesmodel_treetype, 
                    n = 2000)
plot(simulationOutput, 
     asFactor = T,
     quantreg = F)
###Summary and tests
summary(roachesmodel_treetype)
roachestest_treetype <- 
  mixed(roaches~ 
          treetype*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="sqrt"),
        type = afex_options(type = "2"),
        data = poolcenter,
        method = "LRT")$anova_table
###plot
####visreg
visreg(roachesmodel_treetype,
       "treetype", by = "Sampling")
####ggeffects
plot(
  ggeffect(roachesmodel_treetype,
           type = "re",
           terms = c("Sampling", "treetype"),
           ci.lvl = 0.95))
##Tree type - Contrast models
###Observational model
####Model
roachesmodel_obs <- 
  glmer(roaches ~
          treetype*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="sqrt"),
        data = poolcenter%>% 
          filter(treetype != "wr"))
####Assumptions
simulationOutput <- 
  simulateResiduals(fittedModel = roachesmodel_obs, 
                    n = 2000)
plot(simulationOutput, 
     asFactor = T,
     quantreg = F)
####Summary and tests
summary(roachesmodel_obs)
roachestest_obs <- 
  mixed(roaches~ 
          treetype*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="sqrt"),
        type = afex_options(type = "2"),
        data = poolcenter %>% 
          filter(treetype != "wr"),
        method = "LRT")$anova_table
###Experimental model
####Model
roachesmodel_exp <- 
  glmer(roaches ~
          treetype*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="sqrt"),
        data = poolcenter%>% 
          filter(treetype != "wo"))
####Assumptions
simulationOutput <- 
  simulateResiduals(fittedModel = roachesmodel_exp, 
                    n = 2000)
plot(simulationOutput, 
     asFactor = T,
     quantreg = F)
####Summary and tests
summary(roachesmodel_exp)
roachestest_exp <- 
  mixed(roaches~ 
          treetype*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="sqrt"),
        type = afex_options(type = "2"),
        data = poolcenter %>% 
          filter(treetype != "wo"),
        method = "LRT")$anova_table


#Web-weaving spiders
##Volume proximity index
###Model
webspidsmodel_largeleaf <- 
  glmer(webspids~
          largeleaf*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="sqrt"),
        data = poolcenter)
###Assumptions
simulationOutput <- 
  simulateResiduals(fittedModel = webspidsmodel_largeleaf, 
                    n = 2000,
                    rank = T)
plot(simulationOutput,
     quantreg = F,
     rank = T)
###Summary and tests
summary(webspidsmodel_largeleaf)
webspidstest_largeleaf <- 
  mixed(webspids~ 
          largeleaf*Sampling + 
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="sqrt"),
        type = afex_options(type = "2"),
        data = poolcenter,
        method = "LRT")$anova_table
###plot
####visreg
visreg(webspidsmodel_largeleaf,
       "largeleaf", by = "Sampling")
####ggeffects
plot(
  ggeffect(webspidsmodel_largeleaf,
           type = "re",
           x.as.factor = T,
           terms = c("largeleaf", "Sampling"),
           ci.lvl = 0.95))
##Tree type - Full model
###Model
webspidsmodel_treetype <- 
  glmer(webspids ~
          treetype*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="log"),
        control = glmerControl(optimizer = "bobyqa"),
        data = poolcenter)
###Assumptions
simulationOutput <- 
  simulateResiduals(fittedModel = webspidsmodel_treetype, 
                    n = 2000)
plot(simulationOutput,
     asFactor = T,
     quantreg = F)
###Summary and tests
summary(webspidsmodel_treetype)
webspidstest_treetype <- 
  mixed(webspids~ 
          treetype*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="log"),
        type = afex_options(type = "2"),
        control = glmerControl(optimizer = "bobyqa"),
        data = poolcenter,
        method = "LRT")$anova_table
###plot
####visreg
visreg(webspidsmodel_treetype,
       "treetype", by = "Sampling")
####ggeffects
plot(
  ggeffect(webspidsmodel_treetype,
           type = "re",
           terms = c("Sampling", "treetype"),
           ci.lvl = 0.95))
##Tree type - Contrast models
###Observational model
###Model
webspidsmodel_obs <- 
  glmer(webspids ~
          treetype*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="log"),
        control = glmerControl(optimizer = "bobyqa"),
        data = poolcenter%>% 
          filter(treetype != "wr"))
###Assumptions
simulationOutput <- 
  simulateResiduals(fittedModel = webspidsmodel_obs, 
                    n = 2000)
plot(simulationOutput,
     asFactor = T,
     quantreg = F)
###Summary and tests
summary(webspidsmodel_obs)
webspidstest_obs <- 
  mixed(webspids~ 
          treetype*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="log"),
        type = afex_options(type = "2"),
        control = glmerControl(optimizer = "bobyqa"),
        data = poolcenter %>% 
          filter(treetype != "wr"),
        method = "LRT")$anova_table
###Experimental model
###Model
webspidsmodel_exp <- 
  glmer(webspids ~
          treetype*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="log"),
        control = glmerControl(optimizer = "bobyqa"),
        data = poolcenter%>% 
          filter(treetype != "wo"))
###Assumptions
simulationOutput <- 
  simulateResiduals(fittedModel = webspidsmodel_exp, 
                    n = 2000)
plot(simulationOutput,
     asFactor = T,
     quantreg = F)
###Summary and tests
summary(webspidsmodel_exp)
webspidstest_exp <- 
  mixed(webspids~ 
          treetype*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="log"),
        type = afex_options(type = "2"),
        control = glmerControl(optimizer = "bobyqa"),
        data = poolcenter %>% 
          filter(treetype != "wo"),
        method = "LRT")$anova_table

# Models parasitoid abundance per quadrat -------------------------------------
#Volume proximity index
##Model
paramodel_largeleaf <- 
  glmer(para ~
          largeleaf*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="sqrt"),
        data = poolcenter)
##Assumptions
simulationOutput <- 
  simulateResiduals(fittedModel = paramodel_largeleaf, 
                    n = 2000,
                    rank = T)
plot(simulationOutput,
     quantreg = F,
     rank = T)
##Summary and tests
summary(paramodel_largeleaf)
paratest_largeleaf <- 
  mixed(para~ 
          largeleaf*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="sqrt"),
        type = afex_options(type = "2"),
        data = poolcenter,
        method = "LRT")$anova_table
##plot
###visreg
visreg(paramodel_largeleaf,
       "largeleaf", by = "Sampling")
###ggeffects
plot(
  ggeffect(paramodel_largeleaf,
           type = "re",
           x.as.factor = T,
           terms = c("largeleaf", "Sampling"),
           ci.lvl = 0.95))

#Tree type - Full model
##Model
paramodel_treetype <- 
  glmer(para ~
          treetype*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="log"),
        data = poolcenter)
##Assumptions
simulationOutput <- 
  simulateResiduals(fittedModel = paramodel_treetype, 
                    n = 2000)
plot(simulationOutput,
     asFactor = T,
     quantreg = F)
##Summary and tests
summary(paramodel_treetype)
paratest_treetype <- 
  mixed(para~ 
          treetype*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="log"),
        type = afex_options(type = "2"),
        data = poolcenter,
        method = "LRT")$anova_table
##plot
###visreg
visreg(paramodel_treetype,
       "treetype", by = "Sampling")
###ggeffects
plot(
  ggeffect(paramodel_treetype,
           type = "re",
           terms = c("Sampling", "treetype"),
           ci.lvl = 0.95))

#Tree type - Contrast models
##Observational model
###Model
paramodel_obs <- 
  glmer(para ~
          treetype*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="log"),
        data = poolcenter %>% 
          filter(treetype != "wr"))
###Assumptions
simulationOutput <- 
  simulateResiduals(fittedModel = paramodel_obs, 
                    n = 2000)
plot(simulationOutput,
     asFactor = T,
     quantreg = F)
###Summary and tests
summary(paramodel_obs)
paratest_obs <- 
  mixed(para~ 
          treetype*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="log"),
        type = afex_options(type = "2"),
        data = poolcenter %>% 
          filter(treetype != "wr"),
        method = "LRT")$anova_table
##Experimental model
###Model
paramodel_exp <- 
  glmer(para ~
          treetype*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="log"),
        data = poolcenter %>% 
          filter(treetype != "wo"))
###Assumptions
simulationOutput <- 
  simulateResiduals(fittedModel = paramodel_exp, 
                    n = 2000)
plot(simulationOutput,
     asFactor = T,
     quantreg = F)
###Summary and tests
summary(paramodel_exp)
paratest_exp <- 
  mixed(para~ 
          treetype*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="log"),
        type = afex_options(type = "2"),
        data = poolcenter %>% 
          filter(treetype != "wo"),
        method = "LRT")$anova_table




# Models bromeliad-associated predator abundance per quadrat -------------------------------------
#Volume proximity index
##Model
brompredmodel_largeleaf <- 
  glmer(brompred~
          largeleaf*Sampling +
          (1|Site/alltrees/quadrats),
        family = "poisson"(link = "log"),
        data = poolcenter)
##Assumptions
simulationOutput <- 
  simulateResiduals(fittedModel =brompredmodel_largeleaf, 
                    n = 2000,
                    rank = T)
plot(simulationOutput,
     quantreg = F,
     rank = T)
##Summary and tests
summary(brompredmodel_largeleaf)
brompredtest_largeleaf <- 
  mixed(brompred~ 
          largeleaf*Sampling + 
          (1|Site/alltrees/quadrats),
        family = "poisson"(link = "log"),
        type = afex_options(type = "2"),
        data = poolcenter,
        method = "LRT")$anova_table
##plot
###visreg
visreg(brompredmodel_largeleaf,
       "largeleaf", by = "Sampling")
###ggeffects
####Data
brompredeffect_largeleaf <- 
  ggeffect(brompredmodel_largeleaf,
           terms = c("largeleaf", "Sampling"),
           full.data = T,
           swap.pred = T,
           type = "re",
           ci.level = 0.95)
brompredeffect_largeleaf$group <- 
  factor(brompredeffect_largeleaf$group, levels = c("B", "A"))
col <- 
  ifelse(poolcenter$Sampling == "B",
         "goldenrod4", 
         "goldenrod")
####Plot
brompredplot_largeleaf <-  
  plot(brompredeffect_largeleaf,
       ci = T,
       colors = col) + 
  geom_point(data = poolcenter,
             mapping = aes(x = largeleaf, y = jitter(brompred +1, 2)), 
             colour = col, 
             fill = col) +
  ggtitle("") + 
  xlab("Volume proximity index") +
  ylab("Bromeliad-associated predator abundance") +
  scale_color_manual(name = "Sampling period",
                     labels = c("Before", "After"), 
                     values = c("goldenrod4", "goldenrod")) +
  scale_y_continuous(trans = "log",
                     breaks= c(1, 5, 20))+
  theme(legend.position = c(0.8,0.9),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

#Tree type - Full model
##Model
brompredmodel_treetype <- 
  glmer(brompred ~
          treetype*Sampling +
          (1|Site/alltrees/quadrats),
        family = "poisson"(link = "sqrt"),
        data = poolcenter)
##Assumptions
simulationOutput <- 
  simulateResiduals(fittedModel =brompredmodel_treetype, 
                    n = 2000)
plot(simulationOutput,
     asFactor = T,
     quantreg = F)
##Summary and tests
summary(brompredmodel_treetype)
brompredtest_treetype <- 
  mixed(brompred~ 
          treetype*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="sqrt"),
        type = afex_options(type = "2"),
        data = poolcenter,
        method = "LRT")$anova_table
##plot
###visreg
visreg(brompredmodel_treetype,
       "Sampling", by = "treetype")
###ggeffects
####Data
brompredeffect_treetype <- 
  ggeffect(brompredmodel_treetype,
           type = "re",
           x.as.factor = T,
           terms = c("Sampling", "treetype"),
           ci.lvl = 0.95)
brompredeffect_treetype$x <- 
  factor(brompredeffect_treetype$x, levels = c("B", "A"))
brompredeffect_treetype$group <- 
  factor(brompredeffect_treetype$group, levels = c("wo", "w", "wr"))
####Plot
brompredplot_treetype <-
  ggplot(brompredeffect_treetype, 
         aes(x=x, 
             y=predicted,
             colour = group,
             group = group)) + 
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
  ylab("Brom.-associated predator abundance") +
  ylim(0.5,1.75)+
  scale_y_continuous(breaks = c(0.5,1,1.5))+
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
brompredmodel_obs <- 
  glmer(brompred ~
          treetype*Sampling +
          (1|Site/alltrees/quadrats),
        family = "poisson"(link = "log"),
        data = poolcenter %>% 
          filter(treetype != "wr"))
###Assumptions
simulationOutput <- 
  simulateResiduals(fittedModel =brompredmodel_obs, 
                    n = 2000)
plot(simulationOutput,
     asFactor = T,
     quantreg = F)
###Summary and tests
summary(brompredmodel_obs)
brompredtest_obs <- 
  mixed(brompred~ 
          treetype*Sampling +
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="log"),
        type = afex_options(type = "2"),
        data = poolcenter %>% 
          filter(treetype != "wr"),
        method = "LRT")$anova_table
##Experimental model
###Model
brompredmodel_exp <- 
  glmer(brompred ~
          treetype*Sampling +
          (1|Site/alltrees/quadrats),
        family = "poisson"(link = "log"),
        data = poolcenter %>% 
          filter(treetype != "wo"))
###Assumptions
simulationOutput <- 
  simulateResiduals(fittedModel =brompredmodel_exp, 
                    n = 2000)
plot(simulationOutput,
     asFactor = T,
     quantreg = F)
###Summary and tests
summary(brompredmodel_exp)
brompredtest_exp <- 
  mixed(brompred~ 
          treetype*Sampling +
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="log"),
        type = afex_options(type = "2"),
        data = poolcenter %>% 
          filter(treetype != "wo"),
        method = "LRT")$anova_table


# Models bromeliad-associated predator kinds per quadrat -----------------------------
#Bromeliad-associated ants
##Volume proximity index
###Model
bromantsmodel_largeleaf <- 
  glmer(bromants ~
          largeleaf*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="sqrt"),
        data = poolcenter)
###Assumptions
simulationOutput <- 
  simulateResiduals(fittedModel =bromantsmodel_largeleaf, 
                    n = 2000,
                    rank = T)
plot(simulationOutput,
     quantreg = F,
     rank = T)
###Summary and tests
summary(bromantsmodel_largeleaf)
bromantstest_largeleaf <- 
  mixed(bromants~ 
          largeleaf*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="sqrt"),
        type = afex_options(type = "2"),
        data = poolcenter,
        method = "LRT")$anova_table
###Plot
####visreg
visreg(bromantsmodel_largeleaf,
       "largeleaf", by = "Sampling")
###ggeffects
plot(
  ggeffect(bromantsmodel_largeleaf,
           type = "re",
           x.as.factor = T,
           terms = c("largeleaf", "Sampling"),
           ci.lvl = 0.95))
##Tree type - Full model
###Model
bromantsmodel_treetype <- 
  glmer(bromants~
          treetype*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="log"),
        control = glmerControl(optimizer = "bobyqa"),
        data = poolcenter)
###Assumptions
simulationOutput <- 
  simulateResiduals(fittedModel =bromantsmodel_treetype, 
                    n = 2000)
plot(simulationOutput,
     asFactor = T,
     quantreg = F)
###Summary and tests
summary(bromantsmodel_treetype)
bromantstest_treetype <- 
  mixed(bromants~ 
          treetype*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="log"),
        control = glmerControl(optimizer = "bobyqa"),
        type = afex_options(type = "2"),
        data = poolcenter,
        method = "LRT")$anova_table
###Plot
####visreg
visreg(bromantsmodel_treetype,
       "Sampling", by = "treetype")
####ggeffects
#####Data
bromantseffect_treetype <- 
  ggeffect(bromantsmodel_treetype,
           type = "re",
           x.as.factor = T,
           terms = c("Sampling", "treetype"),
           ci.lvl = 0.95)
bromantseffect_treetype$x <- 
  factor( bromantseffect_treetype$x, levels = c("B", "A"))
bromantseffect_treetype$group <- 
  factor( bromantseffect_treetype$group, levels = c("wo", "w", "wr"))
#####Plot
bromantsplot_treetype <- 
  ggplot(bromantseffect_treetype, 
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
  scale_y_continuous(breaks = c(0.1,0.2,0.3)) +
  ylab("Brom.-associated ant abundance") +
  scale_color_manual(name = "Tree type",
                     labels = c("Without", "With", "Removal"),
                     values = c("tomato4", "dodgerblue4", "dodgerblue1")) +
  theme(legend.position = c(0.9,0.9),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))
##Tree type - Contrast models
###Observational model
####Model
bromantsmodel_obs <- 
  glmer(bromants~
          treetype*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="log"),
        control = glmerControl(optimizer = "bobyqa"),
        data = poolcenter %>% 
          filter(treetype != "wr"))
####Assumptions
simulationOutput <- 
  simulateResiduals(fittedModel =bromantsmodel_obs, 
                    n = 2000)
plot(simulationOutput,
     asFactor = T,
     quantreg = F)
####Summary and tests
summary(bromantsmodel_obs)
bromantstest_obs <- 
  mixed(bromants~ 
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
bromantsmodel_exp <- 
  glmer(bromants~
          treetype*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="log"),
        control = glmerControl(optimizer = "bobyqa"),
        data = poolcenter %>% 
          filter(treetype != "wo"))
####Assumptions
simulationOutput <- 
  simulateResiduals(fittedModel =bromantsmodel_exp, 
                    n = 2000)
plot(simulationOutput,
     asFactor = T,
     quantreg = F)
####Summary and tests
summary(bromantsmodel_exp)
bromantstest_exp <- 
  mixed(bromants~ 
          treetype*Sampling +
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="log"),
        control = glmerControl(optimizer = "bobyqa"),
        type = afex_options(type = "2"),
        data = poolcenter %>% 
          filter(treetype != "wo"),
        method = "LRT")$anova_table


#Bromeliad-associated hunting spiders
##Volume proximity index
###Model
bromhuntspidsmodel_largeleaf <- 
  glmer(bromhuntspids ~
          largeleaf*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="sqrt"),
        data = poolcenter)
###Assumptions
simulationOutput <- 
  simulateResiduals(fittedModel =bromhuntspidsmodel_largeleaf, 
                    n = 2000,
                    rank = T)
plot(simulationOutput,
     quantreg = F,
     rank = T)
###Summary and tests
summary(bromhuntspidsmodel_largeleaf)
bromhuntspidstest_largeleaf <- 
  mixed(bromhuntspids~ 
          largeleaf*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="sqrt"),
        type = afex_options(type = "2"),
        data = poolcenter,
        method = "LRT")$anova_table
###Plot
####visreg
visreg(bromhuntspidsmodel_largeleaf,
       "largeleaf", by = "Sampling")
####ggeffects
plot(
  ggeffect(bromhuntspidsmodel_largeleaf,
           type = "re",
           x.as.factor = T,
           terms = c("largeleaf", "Sampling"),
           ci.lvl = 0.95))
##Tree type - Full model
###Model
bromhuntspidsmodel_treetype <- 
  glmer(bromhuntspids ~
          treetype*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="log"),
        data = poolcenter)
###Assumptions
simulationOutput <- 
  simulateResiduals(fittedModel =bromhuntspidsmodel_treetype, 
                    n = 2000)
plot(simulationOutput,
     asFactor = T,
     quantreg = F)
###Summary and tests
summary(bromhuntspidsmodel_treetype)
bromhuntspidstest_treetype <- 
  mixed(bromhuntspids~ 
          treetype*Sampling + 
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="log"),
        type = afex_options(type = "2"),
        data = poolcenter,
        method = "LRT")$anova_table
###Plot
####visreg
visreg(bromhuntspidsmodel_treetype,
       "Sampling", by = "treetype")
####ggeffects
#####Data
bromhuntspidseffect_treetype <- 
  ggeffect(bromhuntspidsmodel_treetype,
           type = "re",
           x.as.factor = T,
           terms = c("Sampling", "treetype"),
           ci.lvl = 0.95)
bromhuntspidseffect_treetype$x <- 
  factor(bromhuntspidseffect_treetype$x, levels = c("B", "A"))
bromhuntspidseffect_treetype$group <- 
  factor(bromhuntspidseffect_treetype$group, levels = c("wo", "w", "wr"))
#####Plot
bromhuntspidsplot_treetype <- 
  ggplot(bromhuntspidseffect_treetype, 
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
  ylab("Brom.-associated hunting spider abundance") +
  scale_color_manual(name = "Tree type",
                     labels = c("Without", "With", "Removal"),
                     values = c("tomato4", "dodgerblue4", "dodgerblue1")) +
  theme(legend.position = c(0.9,0.9),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))
##Tree type - Contrast models
###Observational model
####Model
bromhuntspidsmodel_obs <- 
  glmer(bromhuntspids ~
          treetype*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="log"),
        control = glmerControl(optimizer = "bobyqa"),
        data = poolcenter %>% 
          filter(treetype != "wr"))
####Assumptions
simulationOutput <- 
  simulateResiduals(fittedModel =bromhuntspidsmodel_obs, 
                    n = 2000)
plot(simulationOutput,
     asFactor = T,
     quantreg = F)
###Summary and tests
summary(bromhuntspidsmodel_obs)
bromhuntspidstest_obs <- 
  mixed(bromhuntspids~ 
          treetype*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="log"),
        control = glmerControl(optimizer = "bobyqa"),
        type = afex_options(type = "2"),
        data = poolcenter %>% 
          filter(treetype != "wr"),
        method = "LRT")$anova_table
###Experimental model
####Model
bromhuntspidsmodel_exp <- 
  glmer(bromhuntspids ~
          treetype*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="log"),
        control = glmerControl(optimizer = "bobyqa"),
        data = poolcenter %>% 
          filter(treetype != "wo"))
####Assumptions
simulationOutput <- 
  simulateResiduals(fittedModel =bromhuntspidsmodel_exp, 
                    n = 2000)
plot(simulationOutput,
     asFactor = T,
     quantreg = F)
###Summary and tests
summary(bromhuntspidsmodel_exp)
bromhuntspidstest_exp <- 
  mixed(bromhuntspids~ 
          treetype*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="log"),
        control = glmerControl(optimizer = "bobyqa"),
        type = afex_options(type = "2"),
        data = poolcenter %>% 
          filter(treetype != "wo"),
        method = "LRT")$anova_table

#Bromeliad-associated cockroaches
##Volume proximity index
###Model
bromroachesmodel_largeleaf <- 
  glmer(bromroaches ~
          largeleaf*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="log"),
        data = poolcenter)
###Assumptions
simulationOutput <- 
  simulateResiduals(fittedModel =bromroachesmodel_largeleaf, 
                    n = 2000,
                    rank = T)
plot(simulationOutput,
     quantreg = F,
     rank = T)
###Summary and tests
summary(bromroachesmodel_largeleaf)
bromroachestest_largeleaf <- 
  mixed(bromroaches~ 
          largeleaf*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="log"),
        type = afex_options(type = "2"),
        data = poolcenter,
        method = "LRT")$anova_table
###Plot
####visreg
visreg(bromroachesmodel_largeleaf,
       "largeleaf", by = "Sampling")
####ggeffects
plot(
  ggeffect(bromroachesmodel_largeleaf,
           type = "re",
           x.as.factor = T,
           terms = c("largeleaf", "Sampling"),
           ci.lvl = 0.95))
##Tree type - Full model
###Model
bromroachesmodel_treetype <- 
  glmer(bromroaches ~
          treetype*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="sqrt"),
        control = glmerControl(optimizer = "bobyqa"),
        data = poolcenter)
###Assumptions
simulationOutput <- 
  simulateResiduals(fittedModel =bromroachesmodel_treetype, 
                    n = 2000)
plot(simulationOutput, 
     asFactor = T,
     quantreg = F)
###Summary and tests
summary(bromroachesmodel_treetype)
bromroachestest_treetype <- 
  mixed(bromroaches~ 
          treetype*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="sqrt"),
        control = glmerControl(optimizer = "bobyqa"),
        type = afex_options(type = "2"),
        data = poolcenter,
        method = "LRT")$anova_table
###Plot
####visreg
visreg(bromroachesmodel_treetype,
       "treetype", by = "Sampling")
####ggeffects
plot(
  ggeffect(bromroachesmodel_treetype,
           type = "re",
           x.as.factor = T,
           terms = c("treetype", "Sampling"),
           ci.lvl = 0.95))
##Tree type - Contrast models
##Observational model
####Model
bromroachesmodel_obs <- 
  glmer(bromroaches ~
          treetype*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="log"),
        control = glmerControl(optimizer = "bobyqa"),
        data = poolcenter%>% 
          filter(treetype != "wr"))
####Assumptions
simulationOutput <- 
  simulateResiduals(fittedModel =bromroachesmodel_obs, 
                    n = 2000)
plot(simulationOutput, 
     asFactor = T,
     quantreg = F)
####Summary and tests
summary(bromroachesmodel_obs)
bromroachestest_obs <- 
  mixed(bromroaches~ 
          treetype*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="log"),
        control = glmerControl(optimizer = "bobyqa"),
        type = afex_options(type = "2"),
        data = poolcenter %>% 
          filter(treetype != "wr"),
        method = "LRT")$anova_table
##Experimental model
####Model
bromroachesmodel_exp <- 
  glmer(bromroaches ~
          treetype*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link = "log"),
        control = glmerControl(optimizer = "bobyqa"),
        data = poolcenter%>% 
          filter(treetype != "wo"))
####Assumptions
simulationOutput <- 
  simulateResiduals(fittedModel = bromroachesmodel_exp, 
                    n = 2000)
plot(simulationOutput, 
     asFactor = T,
     quantreg = F)
####Summary and tests
summary(bromroachesmodel_exp)
bromroachestest_exp <- 
  mixed(bromroaches~ 
          treetype*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="log"),
        control = glmerControl(optimizer = "bobyqa"),
        type = afex_options(type = "2"),
        data = poolcenter %>% 
          filter(treetype != "wo"),
        method = "LRT")$anova_table

# Non-bromeliad-associated ants and hunting spiders --------------------------------------------------------------------
#Ants
##Volume proximity index
###Model
nobromantsmodel_largeleaf <- 
  glmer(I(ants-bromants) ~
          largeleaf*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="sqrt"),
        control = glmerControl(optimizer = "bobyqa"),
        data = poolcenter)
###Assumtpions
simulationOutput <- 
  simulateResiduals(fittedModel =nobromantsmodel_largeleaf, 
                    n = 2000,
                    rank = T)
plot(simulationOutput,
     quantreg = F,
     rank = T)
###Summary and tests
summary(nobromantsmodel_largeleaf)
nobromantstest_largeleaf <- 
  mixed(I(ants-bromants)~ 
          largeleaf*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="sqrt"),
        control = glmerControl(optimizer = "bobyqa"),
        type = afex_options(type = "2"),
        data = poolcenter,
        method = "LRT")$anova_table
###Plot
####visreg
visreg(nobromantsmodel_largeleaf,
       "largeleaf", by = "Sampling")
####ggeffects
plot(ggeffect(nobromantsmodel_largeleaf,
              terms = c("largeleaf", "Sampling"),
              type = "re",
              ci.level = 0.95))
##Tree type - Full model
###Model
nobromantsmodel_treetype <- 
  glmer(I(ants-bromants) ~
          treetype*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="sqrt"),
        control = glmerControl(optimizer = "bobyqa"),
        data = poolcenter)
###Assumptions
simulationOutput <- 
  simulateResiduals(fittedModel =nobromantsmodel_treetype, 
                    n = 2000)
plot(simulationOutput,
     asFactor = T,
     quantreg = F)
###Summary and tests
summary(nobromantsmodel_treetype)
nobromantstest_treetype <- 
  mixed(I(ants-bromants)~ 
          treetype*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="sqrt"),
        control = glmerControl(optimizer = "bobyqa"),
        type = afex_options(type = "2"),
        data = poolcenter,
        method = "LRT")$anova_table
###Plot
####visreg
visreg(nobromantsmodel_treetype,
       "Sampling", by = "treetype")
####ggeffects
#####Data
nobromantseffect_treetype <- 
  ggeffect(nobromantsmodel_treetype,
           type = "re",
           x.as.factor = T,
           terms = c("Sampling", "treetype"),
           ci.lvl = 0.95)
nobromantseffect_treetype$x <- 
  factor(nobromantseffect_treetype$x, levels = c("B", "A"))
nobromantseffect_treetype$group <- 
  factor(nobromantseffect_treetype$group, levels = c("wo", "w", "wr"))
#####Plot
nobromantsplot_treetype <- 
  ggplot(nobromantseffect_treetype, 
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
  scale_y_continuous(breaks = c(0.1,0.2,0.3)) +
  geom_point(position = position_dodge(0.3), 
             lwd =6) +
  ggtitle("") + 
  xlab("Sampling period") +
  scale_x_discrete(limit = c("B", "A"),
                   labels = c("Before", "After"),
                   expand = expand_scale(add = c(0.6)))+
  ylab("Non-brom.-associated ant abundance") +
  scale_color_manual(name = "Tree type",
                     labels = c("Without", "With", "Removal"),
                     values = c("tomato4", "dodgerblue4", "dodgerblue1")) +
  theme(legend.position = c(0.9,0.9),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))
##Tree type - Contrast models
###Observational model
####Model
nobromantsmodel_obs <- 
  glmer(I(ants-bromants) ~
          treetype*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="sqrt"),
        control = glmerControl(optimizer = "bobyqa"),
        data = poolcenter %>% 
          filter(treetype != "wr"))
####Assumptions
simulationOutput <- 
  simulateResiduals(fittedModel =nobromantsmodel_obs, 
                    n = 2000)
plot(simulationOutput,
     asFactor = T,
     quantreg = F)
####Summary and tests
summary(nobromantsmodel_obs)
nobromantstest_obs <- 
  mixed(I(ants-bromants)~ 
          treetype*Sampling +
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="sqrt"),
        control = glmerControl(optimizer = "bobyqa"),
        type = afex_options(type = "2"),
        data = poolcenter %>% 
          filter(treetype != "wr"),
        method = "LRT")$anova_table
###Experimental model
####Model
nobromantsmodel_exp <- 
  glmer(I(ants-bromants) ~
          treetype*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="sqrt"),
        control = glmerControl(optimizer = "bobyqa"),
        data = poolcenter %>% 
          filter(treetype != "wo"))
####Assumptions
simulationOutput <- 
  simulateResiduals(fittedModel =nobromantsmodel_exp, 
                    n = 2000)
plot(simulationOutput,
     asFactor = T,
     quantreg = F)
####Summary and tests
summary(nobromantsmodel_exp)
nobromantstest_exp <- 
  mixed(I(ants-bromants)~ 
          treetype*Sampling +
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="sqrt"),
        control = glmerControl(optimizer = "bobyqa"),
        type = afex_options(type = "2"),
        data = poolcenter %>% 
          filter(treetype != "wo"),
        method = "LRT")$anova_table


#Hunting spiders
##Volume proximity index
###Model
nobromhuntspidsmodel_largeleaf <- 
  glmer(I(huntspids - bromhuntspids) ~
          largeleaf*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="log"),
        data = poolcenter)
###Assumptions
simulationOutput <- 
  simulateResiduals(fittedModel =nobromhuntspidsmodel_largeleaf, 
                    n = 2000,
                    rank = T)
plot(simulationOutput,
     quantreg = F,
     rank = T)
###Summary and tests
summary(nobromhuntspidsmodel_largeleaf)
nobromhuntspidstest_largeleaf <- 
  mixed(I(huntspids - bromhuntspids) ~ 
          largeleaf*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="log"),
        type = afex_options(type = "2"),
        data = poolcenter,
        method = "LRT")$anova_table
###Plot
####visreg
visreg(nobromhuntspidsmodel_largeleaf,
       "largeleaf", by = "Sampling")
####ggeffects
plot(ggeffect(nobromhuntspidsmodel_largeleaf,
              terms = c("largeleaf", "Sampling"),
              type = "re",
              ci.level = 0.95))
##Tree type - Full model
###Model
nobromhuntspidsmodel_treetype <- 
  glmer(I(huntspids - bromhuntspids) ~
          treetype*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="sqrt"),
        data = poolcenter)
###Assumptions
simulationOutput <- 
  simulateResiduals(fittedModel =nobromhuntspidsmodel_treetype, 
                    n = 2000)
plot(simulationOutput,
     asFactor = T,
     quantreg = F)
###Summary and tests
summary(nobromhuntspidsmodel_treetype)
nobromhuntspidstest_treetype <- 
  mixed(I(huntspids - bromhuntspids) ~ 
          treetype*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="sqrt"),
        type = afex_options(type = "2"),
        data = poolcenter,
        method = "LRT")$anova_table
###Plot
####visreg
visreg(nobromhuntspidsmodel_treetype,
       "Sampling", by = "treetype")
####ggeffects
nobromhuntspidseffect_treetype <- 
  ggeffect(nobromhuntspidsmodel_treetype,
           type = "re",
           x.as.factor = T,
           terms = c("Sampling", "treetype"),
           ci.lvl = 0.95)
nobromhuntspidseffect_treetype$x <- 
  factor(nobromhuntspidseffect_treetype$x, levels = c("B", "A"))
nobromhuntspidseffect_treetype$group <- 
  factor(nobromhuntspidseffect_treetype$group, levels = c("wo", "w", "wr"))
nobromhuntspidsplot_treetype <- 
  ggplot(nobromhuntspidseffect_treetype, 
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
  scale_y_continuous(breaks = c(0.4,0.8,1.1)) +
  scale_x_discrete(limit = c("B", "A"),
                   labels = c("Before", "After"),
                   expand = expand_scale(add = c(0.6)))+
  ylab("Non-brom.-associated hunting spider abundance") +
  scale_color_manual(name = "Tree type",
                     labels = c("Without", "With", "Removal"),
                     values = c("tomato4", "dodgerblue4", "dodgerblue1")) +
  theme(legend.position = c(0.9,0.9),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))
##Tree type - Contrast model
###Observational model
####Model
nobromhuntspidsmodel_obs <- 
  glmer(I(huntspids - bromhuntspids) ~
          treetype*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="log"),
        data = poolcenter %>% 
          filter(treetype != "wr"))
####Assumptions
simulationOutput <- 
  simulateResiduals(fittedModel =nobromhuntspidsmodel_obs, 
                    n = 2000)
plot(simulationOutput,
     asFactor = T,
     quantreg = F)
####Summary and tests
summary(nobromhuntspidsmodel_obs)
nobromhuntspidstest_obs <- 
  mixed(I(huntspids - bromhuntspids) ~ 
          treetype*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="log"),
        type = afex_options(type = "2"),
        data = poolcenter %>% 
          filter(treetype != "wr"),
        method = "LRT")$anova_table
###Experimental model
####Model
nobromhuntspidsmodel_exp <- 
  glmer(I(huntspids - bromhuntspids) ~
          treetype*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="sqrt"),
        data = poolcenter %>% 
          filter(treetype != "wo"))
####Assumptions
simulationOutput <- 
  simulateResiduals(fittedModel =nobromhuntspidsmodel_exp, 
                    n = 2000)
plot(simulationOutput,
     asFactor = T,
     quantreg = F)
####Summary and tests
summary(nobromhuntspidsmodel_exp)
nobromhuntspidstest_exp <- 
  mixed(I(huntspids - bromhuntspids) ~ 
          treetype*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="sqrt"),
        type = afex_options(type = "2"),
        data = poolcenter %>% 
          filter(treetype != "wo"),
        method = "LRT")$anova_table

# Models tree-associated  predator abundance per quadrat -------------------------------------
#Volume proximity index
##Model
arbopredmodel_largeleaf <- 
  glmer(arbopred ~
          largeleaf*Sampling +
          (1|Site/alltrees/quadrats),
        family = "poisson"(link="log"),
        control = glmerControl(optimizer = "bobyqa"),
        data = poolcenter)
##Assumptions
simulationOutput <- 
  simulateResiduals(fittedModel =arbopredmodel_largeleaf, 
                    n = 2000,
                    rank = T)
plot(simulationOutput,
     quantreg = F,
     rank = T)
##Summary and tests
summary(arbopredmodel_largeleaf)
arbopredtest_largeleaf <- 
  mixed(arbopred~ 
          largeleaf*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="log"),
        control = glmerControl(optimizer = "bobyqa"),
        type = afex_options(type = "2"),
        data = poolcenter,
        method = "LRT")$anova_table
##Plot
###visreg
visreg(arbopredmodel_largeleaf,
       "largeleaf", by = "Sampling")
###ggeffects
plot(ggeffect(arbopredmodel_largeleaf,
              terms = c("largeleaf", "Sampling"),
              type = "re",
              ci.level = 0.95))

#Tree type - Full model
##Model
arbopredmodel_treetype <- 
  glmer(arbopred ~
          treetype*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="sqrt"),
        data = poolcenter)
##Assumptions
simulationOutput <- 
  simulateResiduals(fittedModel =arbopredmodel_treetype, 
                    n = 2000)
plot(simulationOutput, 
     asFactor = T,
     quantreg = F)
##Summary and tests
summary(arbopredmodel_treetype)
arbopredtest_treetype <- 
  mixed(arbopred~ 
          treetype*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="sqrt"),
        type = afex_options(type = "2"),
        data = poolcenter,
        method = "LRT")$anova_table
##Plot
###visreg
visreg(arbopredmodel_treetype,
       "treetype", by = "Sampling")
###ggeffects
plot(
  ggeffect(arbopredmodel_treetype,
           type = "re",
           terms = c("Sampling", "treetype"),
           ci.lvl = 0.95))

#Tree type - Contrast models
##Observational model
###Model
arbopredmodel_obs <- 
  glmer(arbopred ~
          treetype*Sampling +
          (1|Site/alltrees/quadrats),
        family = "poisson"(link ="sqrt"),
        data = poolcenter %>% 
          filter(treetype != "wr"))
###Assumptions
simulationOutput <- 
  simulateResiduals(fittedModel =arbopredmodel_obs, 
                    n = 2000)
plot(simulationOutput, 
     asFactor = T,
     quantreg = F)
###Summary and tests
summary(arbopredmodel_obs)
arbopredtest_obs <- 
  mixed(arbopred~ 
          treetype*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="sqrt"),
        type = afex_options(type = "2"),
        data = poolcenter %>% 
          filter(treetype != "wr"),
        method = "LRT")$anova_table
##Experimental model
###Model
arbopredmodel_exp <- 
  glmer(arbopred ~
          treetype*Sampling +
          (1|Site/alltrees/quadrats),
        family = "poisson"(link ="sqrt"),
        data = poolcenter %>% 
          filter(treetype != "wo"))
###Assumptions
simulationOutput <- 
  simulateResiduals(fittedModel =arbopredmodel_exp, 
                    n = 2000)
plot(simulationOutput, 
     asFactor = T,
     quantreg = F)
###Summary and tests
summary(arbopredmodel_exp)
arbopredtest_exp <- 
  mixed(arbopred~ 
          treetype*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="sqrt"),
        type = afex_options(type = "2"),
        data = poolcenter %>% 
          filter(treetype != "wo"),
        method = "LRT")$anova_table



# Models aerial predator abundance per quadrat -------------------------------------
#Volume proximity index
##Model
mobipredmodel_largeleaf <- 
  glmer(mobipred ~
          largeleaf*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="sqrt"),
        data = poolcenter)
##Assumptions
simulationOutput <- 
  simulateResiduals(fittedModel =mobipredmodel_largeleaf, 
                    n = 2000,
                    rank = T)
plot(simulationOutput,
     quantreg = F,
     rank = T)
##Summary and tests
summary(mobipredmodel_largeleaf)
mobipredtest_largeleaf <- 
  mixed(mobipred~ 
          largeleaf*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="sqrt"),
        type = afex_options(type = "2"),
        data = poolcenter,
        method = "LRT")$anova_table
##Plot
###visreg
visreg(mobipredmodel_largeleaf,
       "largeleaf", by = "Sampling")
###ggeffects
plot(
  ggeffect(mobipredmodel_largeleaf,
           type = "re",
           x.as.factor = T,
           terms = c("largeleaf", "Sampling"),
           ci.lvl = 0.95))

#Tree type - Full model
##Model
mobipredmodel_treetype <- 
  glmer(mobipred ~
          treetype*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="sqrt"),
        data = poolcenter)
##Assumptions
simulationOutput <- 
  simulateResiduals(fittedModel =mobipredmodel_treetype, 
                    n = 2000,
                    rank = T)
plot(simulationOutput, 
     asFactor = T,
     quantreg = F)
##Summary and tests
summary(mobipredmodel_treetype)
mobipredtest_treetype <- 
  mixed(mobipred~ 
          treetype*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="sqrt"),
        type = afex_options(type = "2"),
        data = poolcenter,
        method = "LRT")$anova_table
##Plot
###visreg
visreg(mobipredmodel_treetype,
       "treetype", by = "Sampling")
###ggeffects
####Data
mobipredeffect_treetype <- 
  ggeffect(mobipredmodel_treetype,
           type = "re",
           x.as.factor = T,
           terms = c("Sampling", "treetype"),
           ci.lvl = 0.95)
mobipredeffect_treetype$x <- 
  factor(mobipredeffect_treetype$x, levels = c("B", "A"))
mobipredeffect_treetype$group <- 
  factor(mobipredeffect_treetype$group, levels = c("wo", "w", "wr"))
####Plot
mobipredplot_treetype <- 
  ggplot(mobipredeffect_treetype, 
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
  ylab("Aerial predator abundance") +
  ylim(0.5,1.75)+
  scale_y_continuous(breaks = c(0,0.5,1))+
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
mobipredmodel_obs <- 
  glmer(mobipred ~
          treetype*Sampling +
          (1|Site/alltrees/quadrats),
        family = "poisson"(link ="sqrt"),
        data = poolcenter %>% 
          filter(treetype != "wr"))
###Assumptions
simulationOutput <- 
  simulateResiduals(fittedModel = mobipredmodel_obs, 
                    n = 2000,
                    rank = T)
plot(simulationOutput, 
     asFactor = T,
     quantreg = F)
###Summary and tests
summary(mobipredmodel_obs)
mobipredtest_obs <- 
  mixed(mobipred ~ 
          treetype*Sampling + 
          (1|Site/alltrees/quadrats),
        family = "poisson"(link = "sqrt"),
        type = afex_options(type = "2"),
        data = poolcenter %>% 
          filter(treetype != "wr"),
        method = "LRT")$anova_table
##Experimental model
###Model
mobipredmodel_exp <- 
  glmer(mobipred ~
          treetype*Sampling +
          (1|Site/alltrees/quadrats),
        family = "poisson"(link ="log"),
        data = poolcenter %>% 
          filter(treetype != "wo"))
###Assumptions
simulationOutput <- 
  simulateResiduals(fittedModel = mobipredmodel_exp, 
                    n = 2000,
                    rank = T)
plot(simulationOutput, 
     asFactor = T,
     quantreg = F)
###Summary and tests
summary(mobipredmodel_exp)
mobipredtest_exp <- 
  mixed(mobipred ~ 
          treetype*Sampling + 
          (1|Site/alltrees/quadrats),
        family = "poisson"(link = "log"),
        type = afex_options(type = "2"),
        data = poolcenter %>% 
          filter(treetype != "wo"),
        method = "LRT")$anova_table

# Adonis all predator kinds per quadrat  -------------------------------
#Volume proximity index
predadonis_largeleaf <- 
  adonis(spread_pred[,6:14]~
           largeleaf*Sampling,
         distance = "bray",
         strata = spread_pred$Site,
         data = spread_pred,
         permutations = 2000)

#Tree type - Full model
predadonis_treetype <- 
  adonis(spread_pred[,6:14] ~
           treetype*Sampling,
         method = "bray",
         permutations = 2000,
         strata = spread_pred$Site,
         data = spread_pred)

#Tree type - Contrast models
##Observational model
predadonis_obs <- 
  adonis(spread_pred[which(spread_pred$treetype != "wr" &
                             rowSums(spread_pred[,6:14]) != 0),6:14] ~
           treetype*Sampling,
         method = "bray",
         permutations = 2000,
         strata = spread_pred$Site[which(spread_pred$treetype != "wr")],
         data = spread_pred %>% 
           filter(treetype != "wr"))
##Experimental model
predadonis_exp <- 
  adonis(spread_pred[which(spread_pred$treetype != "wo" &
                             rowSums(spread_pred[,6:14]) != 0),6:14] ~
           treetype*Sampling,
         method = "bray",
         permutations = 2000,
         strata = spread_pred$Site[which(spread_pred$treetype != "wo")],
         data = spread_pred %>% 
           filter(treetype != "wo"))

# Adonis bromeliad-associated predator kinds per quadrat -------------------------------
#Volume proximity index
brompredadonis_largeleaf <- 
  adonis(spread_brompred[,9:13] ~
           largeleaf*Sampling,
         method = "bray",
         permutations = 2000,
         strata = spread_brompred$Site,
         data = spread_brompred)

#Tree type - Full model
brompredadonis_treetype <- 
  adonis(spread_brompred[,9:13] ~
           treetype*Sampling,
         method = "bray",
         permutations = 2000,
         strata = spread_brompred$Site,
         data = spread_brompred)

#Tree type - Contrast models
##Observational model
brompredadonis_obs <- 
  adonis(spread_brompred[which(spread_brompred$treetype != "wr" &
                             rowSums(spread_brompred[,9:13]) != 0),9:13] ~
           treetype*Sampling,
         method = "bray",
         permutations = 2000,
         strata = spread_brompred$Site[which(spread_brompred$treetype != "wr")],
         data = spread_brompred %>% 
           filter(treetype != "wr"))
##Experimental model
brompredadonis_exp <- 
  adonis(spread_brompred[which(spread_brompred$treetype != "wo" &
                                 rowSums(spread_brompred[,9:13]) != 0),9:13] ~
           treetype*Sampling,
         method = "bray",
         permutations = 2000,
         strata = spread_brompred$Site[which(spread_brompred$treetype != "wo")],
         data = spread_brompred %>% 
           filter(treetype != "wo"))

# Adonis tree-associated predator kinds per quadrat -------------------------------
#Volume proximity index
treepredadonis_largeleaf <- 
  adonis(spread_arbopred[,9:14] ~
           largeleaf*Sampling,
         method = "bray",
         permutations = 2000,
         strata = spread_arbopred$Site,
         data = spread_arbopred)

#Tree type - Full model
treepredadonis_treetype <- 
  adonis(spread_arbopred[,9:14] ~
           treetype*Sampling,
         method = "bray",
         permutations = 2000,
         strata = spread_arbopred$Site,
         data = spread_arbopred)

#Tree type - Contrast models
##Observational model
treepredadonis_obs <- 
  adonis(spread_arbopred[which(spread_arbopred$treetype != "wr" &
                                 rowSums(spread_arbopred[,9:14]) != 0),9:14] ~
           treetype*Sampling,
         method = "bray",
         permutations = 2000,
         strata = spread_arbopred$Site[which(spread_arbopred$treetype != "wr")],
         data = spread_arbopred %>% 
           filter(treetype != "wr"))
##Experimental model
treepredadonis_exp <- 
  adonis(spread_arbopred[which(spread_arbopred$treetype != "wo" &
                                 rowSums(spread_arbopred[,9:14]) != 0),9:14] ~
           treetype*Sampling,
         method = "bray",
         permutations = 2000,
         strata = spread_arbopred$Site[which(spread_arbopred$treetype != "wo")],
         data = spread_arbopred %>% 
           filter(treetype != "wo"))






# Adonis aerial predator kinds per quadrat -------------------------------
#Volume proximity index
mobipredadonis_largeleaf <- 
  adonis(spread_mobipred[,9:12] ~
           largeleaf*Sampling,
         method = "bray",
         permutations = 2000,
         strata = spread_mobipred$Site,
         data = spread_mobipred)

#Tree type - Full model
mobipredadonis_treetype <- 
  adonis(spread_mobipred[,9:12] ~
           treetype*Sampling,
         method = "bray",
         permutations = 2000,
         strata = spread_mobipred$Site,
         data = spread_mobipred)

#Tree type - Contrast models
##Observational model
mobipredadonis_obs <- 
  adonis(spread_mobipred[which(spread_mobipred$treetype != "wr" &
                                 rowSums(spread_mobipred[,9:12]) != 0),9:12] ~
           treetype*Sampling,
         method = "bray",
         permutations = 2000,
         strata = spread_mobipred$Site[which(spread_mobipred$treetype != "wr")],
         data = spread_mobipred %>% 
           filter(treetype != "wr"))
##Experimental model
mobipredadonis_exp <- 
  adonis(spread_mobipred[which(spread_mobipred$treetype != "wo" &
                                 rowSums(spread_mobipred[,9:12]) != 0),9:12] ~
           treetype*Sampling,
         method = "bray",
         permutations = 2000,
         strata = spread_mobipred$Site[which(spread_mobipred$treetype != "wo")],
         data = spread_mobipred %>% 
           filter(treetype != "wo"))







