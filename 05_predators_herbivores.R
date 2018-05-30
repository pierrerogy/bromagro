#Predator effect on herbivore
# Models ------------------------------------------------------------------
#All predators
predherbmodel_pred <- 
  glmer(herb ~
          predcenter*Sampling +
          (1|Site/alltrees/quadrats),
        family= "poisson"(link = "sqrt"),
        data = poolcenter)
simulationOutput <- 
  simulateResiduals(fittedModel =predherbmodel_pred, 
                    n = 2000)
plotSimulatedResiduals(simulationOutput = simulationOutput,
                       quantreg = F)
summary(predherbmodel_pred)
predherbtest_pred <- 
  mixed(herb~ 
          predcenter*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="sqrt"),
        type = afex_options(type = "2"),
        data = poolcenter,
        method = "LRT")$anova_table
##plot
###visreg
visreg(predherbmodel_pred,
       "predcenter", by = "Sampling")
###ggeffect
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
         "darkorange2", 
         "dodgerblue4")
predherbeffect_pred$conf.low <- 
  predherbeffect_pred$conf.low +1
predherbeffect_pred$conf.high <- 
  predherbeffect_pred$conf.high +1
predherbeffect_pred$predicted <- 
  predherbeffect_pred$predicted +1
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
                     breaks = c(1,7,40)) +
  scale_color_manual(labels = c("Before", "After"), 
                     values = c("darkorange2", "dodgerblue4")) +
  theme(legend.position = c(0.9 ,0.9),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

#Bromeliad-associated predators
predherbmodel_brompreds <- 
  glmer(herb ~
          bromcenter*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="sqrt"),
        data = poolcenter)
simulationOutput <- 
  simulateResiduals(fittedModel =predherbmodel_brompreds, 
                    n = 2000)
plotSimulatedResiduals(simulationOutput = simulationOutput,
                       quantreg = F)
summary(predherbmodel_brompreds)
predherbtest_brompreds <- 
  mixed(herb~ 
          bromcenter*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="sqrt"),
        type = afex_options(type = "2"),
        data = poolcenter,
        method = "LRT")$anova_table
##plot
###
visreg(predherbmodel_brompreds,
       "bromcenter", by = "Sampling")
###ggeffect
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
         "darkorange2", 
         "dodgerblue4")
predherbeffect_brompreds$conf.low <- 
  predherbeffect_brompreds$conf.low +1
predherbeffect_brompreds$conf.high <- 
  predherbeffect_brompreds$conf.high +1
predherbeffect_brompreds$predicted <- 
  predherbeffect_brompreds$predicted +1
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
                     breaks = c(1,7,40)) +
  scale_color_manual(labels = c("Before", "After"), 
                     values = c("darkorange2", "dodgerblue4")) +
  theme(legend.position = c(0.9 ,0.9),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))


#Tree predators
predherbmodel_treepreds <- 
  glmer(herb ~
          arbocenter*Sampling +
          (1|Site/alltrees/quadrats),
          family = "poisson"(link ="sqrt"),
        data = poolcenter)
simulationOutput <- 
  simulateResiduals(fittedModel =predherbmodel_treepreds, 
                    n = 2000)
plotSimulatedResiduals(simulationOutput = simulationOutput,
                       quantreg = F)
summary(predherbmodel_treepreds)
predherbtest_treepreds <- 
  mixed(herb~ 
          arbocenter*Sampling + 
          (1|Site/alltrees/quadrats),
        family = "poisson"(link ="sqrt"),
        type = afex_options(type = "2"),
        data = poolcenter,
        method = "LRT")$anova_table
##plot
###visreg
visreg(predherbmodel_treepreds,
       "arbocenter", by = "Sampling")
###ggeffect
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
         "darkorange2", 
         "dodgerblue4")
predherbeffect_treepreds$conf.low <- 
  predherbeffect_treepreds$conf.low +1
predherbeffect_treepreds$conf.high <- 
  predherbeffect_treepreds$conf.high +1
predherbeffect_treepreds$predicted <- 
  predherbeffect_treepreds$predicted +1
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
                     breaks = c(1,7,40)) +
  scale_color_manual(labels = c("Before", "After"), 
                     values = c("darkorange2", "dodgerblue4")) +
  theme(legend.position = c(0.9 ,0.9),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

#Mobile predators
predherbmodel_mobipreds <- 
  glmer(herb ~
          mobicenter*Sampling +
          (1|Site/alltrees/quadrats),
        family="poisson"(link ="sqrt"),
        data = poolcenter)
simulationOutput <- 
  simulateResiduals(fittedModel =predherbmodel_mobipreds, 
                    n = 2000)
plotSimulatedResiduals(simulationOutput = simulationOutput,
                       quantreg = F)
summary(predherbmodel_mobipreds)
predherbtest_mobipreds <- 
  mixed(herb~ 
          mobicenter*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="sqrt"),
        type = afex_options(type = "2"),
        data = poolcenter,
        method = "LRT")$anova_table
##plot
###visreg
visreg(predherbmodel_mobipreds,
       "mobicenter", by = "Sampling")
###ggeffect
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
         "darkorange2", 
         "dodgerblue4")
predherbeffect_mobipreds$conf.low <- 
  predherbeffect_mobipreds$conf.low +1
predherbeffect_mobipreds$conf.high <- 
  predherbeffect_mobipreds$conf.high +1
predherbeffect_mobipreds$predicted <- 
  predherbeffect_mobipreds$predicted +1
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
                     breaks = c(1,7,40)) +
  scale_color_manual(labels = c("Before", "After"), 
                     values = c("darkorange2", "dodgerblue4")) +
  theme(legend.position = c(0.9 ,0.9),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

#Parasitoids
predherbmodel_para <- 
  glmer.nb(herb~
             paracenter*Sampling +
             (1|Site/alltrees/quadrats),
           data = poolcenter)
simulationOutput <- 
  simulateResiduals(fittedModel =predherbmodel_para, 
                    n = 2000)
plotSimulatedResiduals(simulationOutput = simulationOutput,
                       quantreg = F)
summary(predherbmodel_para)
predherbtest_para <- 
  mixed(herb~ 
          paracenter*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="negative.binomial"(theta = getME(predherbmodel_para,
                                                  "glmer.nb.theta")),
        type = afex_options(type = "2"),
        data = poolcenter,
        method = "LRT")$anova_table
##plot
###
visreg(predherbmodel_para,
       "paracenter", by = "Sampling")
###ggeffect
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
         "darkorange2", 
         "dodgerblue4")
predherbeffect_para$conf.low <- 
  predherbeffect_para$conf.low +1
predherbeffect_para$conf.high <- 
  predherbeffect_para$conf.high +1
predherbeffect_para$predicted <- 
  predherbeffect_para$predicted +1
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
                     breaks = c(1,7,40)) +
  scale_color_manual(labels = c("Before", "After"), 
                     values = c("darkorange2", "dodgerblue4")) +
  theme(legend.position = c(0.9 ,0.9),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

# Adonis ------------------------------------------------------------------
#All predators
herbadonis_preds <- 
  adonis(spread_herb[,8:14] ~
           predcenter*Sampling,
         method = "bray",
         permutations = 2000,
         strata = spread_herb$Site,
         data = spread_herb)
  

#Bromeliad predators
herbadonis_bromypred <- 
  adonis(spread_herb[,8:14] ~
           bromcenter*Sampling,
         method = "bray",
         permutations = 2000,
         strata = spread_herb$Site,
         data = spread_herb)  


#Tree predators
herbadonis_arbopred <- 
  adonis(spread_herb[,8:14] ~
           arbocenter*Sampling,
         method = "bray",
         permutations = 2000,
         strata = spread_herb$Site,
         data = spread_herb)  

#Mobile predators
herbadonis_mobipred <- 
  adonis(spread_herb[,8:14] ~
           mobicenter*Sampling,
         method = "bray",
         permutations = 2000,
         strata = spread_herb$Site,
         data = spread_herb) 

#Parasitoids
herbadonis_para <- 
  adonis(spread_herb[,8:14] ~
           paracenter*Sampling,
         method = "bray",
         permutations = 2000,
         strata = spread_herb$Site,
         data = spread_herb) 



# Model Loop predator shrunk (not for broms) and herbivores expanded -------
#For log link function
##Creating vectors to indicate locations of dependent and independent groups
#Not reinitialising them before every loop will clutter things up
###dependent
first_start<-
  8
first_end<- 
  15
first_nvar<-
  first_end-first_start+1
first_variable<-
  rep(NA, first_nvar)
first_disper <- 
  rep(NA, first_nvar)
first_pval_var<-
  rep(NA, first_nvar)
first_pval_sam<-
  rep(NA, first_nvar)
first_pval_int<-
  rep(NA, first_nvar)
first_AIC <- 
  rep(NA, first_nvar)
first_link <- 
  rep(NA, first_nvar)
first_intercept <- 
  rep(NA, first_nvar)
first_coefvar <- 
  rep(NA, first_nvar)
first_coefsam <- 
  rep(NA, first_nvar)
first_coefint <- 
  rep(NA, first_nvar)
first_interceptzval <- 
  rep(NA, first_nvar)
first_coefvarzval <- 
  rep(NA, first_nvar)
first_coefsamzval <- 
  rep(NA, first_nvar)
first_coefintzval <- 
  rep(NA, first_nvar)
###independent
sec_start<-
  33
sec_end<-
  41
sec_nvar<-
  sec_end-sec_start+1
sec_variable<-
  rep(NA, sec_nvar)
sec_disper <- 
  rep(NA, sec_nvar)
sec_pval_var<-
  rep(NA, sec_nvar)
sec_pval_sam<-
  rep(NA, sec_nvar)
sec_pval_int<-
  rep(NA, sec_nvar)
sec_AIC <- 
  rep(NA, sec_nvar)
sec_link <- 
  rep(NA, sec_nvar)
sec_intercept <- 
  rep(NA, sec_nvar)
sec_coefvar <- 
  rep(NA, sec_nvar)
sec_coefsam <- 
  rep(NA, sec_nvar)
sec_coefint <- 
  rep(NA, sec_nvar)
sec_interceptzval <- 
  rep(NA, sec_nvar)
sec_coefvarzval <- 
  rep(NA, sec_nvar)
sec_coefsamzval <- 
  rep(NA, sec_nvar)
sec_coefintzval <- 
  rep(NA, sec_nvar)
number <- 
  1
##loop
for (i in first_start:first_end){
  first = colnames(predcenter)[i]
  for (j in sec_start:sec_end){
    second = colnames(predcenter)[j]
    possibleError <- 
      tryCatch(
        model <- 
          glmer(get(first) ~ get(second)*Sampling +
                  (1|Site/alltrees/quadrats),
                family ="poisson"(link="log"),
                data =predcenter),
        error=function(e) e,
        warning=function(w) w)
    if(inherits(possibleError, c("error", "warning"))) next  
    
    pval_var <- Anova(model)[1,3]
    pval_sam <- Anova(model)[2,3]
    pval_int <- Anova(model)[3,3]
    AIC <- AIC(model)
    link <- model@resp$family$link
    dispersion <- dispersion_glmer(model)
    summary <- summary(model)
    
    
    first_pval_var[number] = as.numeric(pval_var)
    first_pval_sam[number] = as.numeric(pval_sam)
    first_pval_int[number] = as.numeric(pval_int)
    first_AIC[number] = as.numeric(AIC)
    first_link[number] = as.character(link)
    first_variable[number] = first
    first_disper[number] = dispersion
    first_intercept[number] = summary$coefficients[1,1]
    first_coefvar[number] = ifelse(first_pval_var[number] < 0.05,
                                   summary$coefficients[2,1],
                                   0)
    first_coefsam[number] = ifelse(first_pval_sam[number] < 0.05,
                                   summary$coefficients[3,1],
                                   0)
    first_coefint[number] = ifelse(first_pval_int[number] < 0.05,
                                   summary$coefficients[4,1],
                                   0)
    first_interceptzval[number] = summary$coefficients[1,3]
    first_coefvarzval[number] = ifelse(first_pval_var[number] < 0.05,
                                       summary$coefficients[2,3],
                                       0)
    first_coefsamzval[number] = ifelse(first_pval_sam[number] < 0.05,
                                       summary$coefficients[3,3],
                                       0)
    first_coefintzval[number] = ifelse(first_pval_int[number] < 0.05,
                                       summary$coefficients[4,3],
                                       0)
    number = number + 1
    
    sec_pval_var[number] = as.numeric(pval_var)
    sec_pval_sam[number] = as.numeric(pval_sam)
    sec_pval_int[number] = as.numeric(pval_int)
    sec_AIC[number] = as.numeric(AIC)
    sec_link[number] = as.character(link)
    sec_variable[number] = second
    sec_disper[number] = dispersion
    sec_intercept[number] = summary$coefficients[1,1]
    sec_coefvar[number] = ifelse(sec_pval_var[number] < 0.05,
                                 summary$coefficients[2,1],
                                 0)
    sec_coefsam[number] = ifelse(sec_pval_sam[number] < 0.05,
                                 summary$coefficients[3,1],
                                 0)
    sec_coefint[number] = ifelse(sec_pval_int[number] < 0.05,
                                 summary$coefficients[4,1],
                                 0)
    sec_interceptzval[number] = summary$coefficients[1,3]
    sec_coefvarzval[number] = ifelse(sec_pval_var[number] < 0.05,
                                     summary$coefficients[2,3],
                                     0)
    sec_coefsamzval[number] = ifelse(sec_pval_sam[number] < 0.05,
                                     summary$coefficients[3,3],
                                     0)
    sec_coefintzval[number] = ifelse(sec_pval_int[number] < 0.05,
                                     summary$coefficients[4,3],
                                     0)
    number = number + 1
  }
}

##Create dataframe with results
first <- 
  data.frame(first_variable, first_link, first_disper, first_AIC, 
             first_pval_var, first_pval_sam, first_pval_int, 
             first_intercept, first_coefvar, first_coefsam, first_coefint,
             first_interceptzval,first_coefvarzval, first_coefsamzval, first_coefintzval,
             stringsAsFactors = F)
second <- 
  data.frame(sec_variable, sec_link, sec_disper, sec_AIC, 
             sec_pval_var, sec_pval_sam, sec_pval_int, 
             sec_intercept, sec_coefvar, sec_coefsam, sec_coefint,
             sec_interceptzval,sec_coefvarzval, sec_coefsamzval, sec_coefintzval,
             stringsAsFactors = F)

##Rename dataframes and bind
first <- 
  first %>% 
  rename(
    variable = first_variable,
    AIC = first_AIC,
    link = first_link,
    dispersion = first_disper,
    p_variable = first_pval_var,
    p_sampling = first_pval_sam,
    p_interaction = first_pval_int,
    intercept = first_intercept,
    var_coef = first_coefvar,
    sam_coef = first_coefsam,
    interaction_coef = first_coefint,
    intercept_z = first_interceptzval,
    var_z = first_coefvarzval,
    sam_z = first_coefsamzval,
    interaction_z = first_coefintzval
  ) %>% 
  mutate(type = "dependent")

second <- 
  second %>% 
  rename(
    variable = sec_variable,
    AIC = sec_AIC,
    link = sec_link,
    dispersion = sec_disper,
    p_variable = sec_pval_var,
    p_sampling = sec_pval_sam,
    p_interaction = sec_pval_int,
    intercept = sec_intercept,
    var_coef = sec_coefvar,
    sam_coef = sec_coefsam,
    interaction_coef = sec_coefint,
    intercept_z = sec_interceptzval,
    var_z = sec_coefvarzval,
    sam_z = sec_coefsamzval,
    interaction_z = sec_coefintzval) %>% 
  mutate(type = "independent")

association_lastframe_log <- 
  rbind(first, second)
association_lastframe_log <-
  na.omit(association_lastframe_log)


##Bring all variables in one row
association_lastframe_log <- 
  association_lastframe_log %>% 
  spread(type, variable) %>% 
  dplyr::select(dependent, independent, link, dispersion,
                AIC, p_variable, p_sampling, p_interaction,
                intercept, var_coef, sam_coef, interaction_coef,
                intercept_z, var_z, sam_z, interaction_z)

#For sqrt link function
###dependent
first_start<-
  8
first_end<- 
  15
first_nvar<-
  first_end-first_start+1
first_variable<-
  rep(NA, first_nvar)
first_disper <- 
  rep(NA, first_nvar)
first_pval_var<-
  rep(NA, first_nvar)
first_pval_sam<-
  rep(NA, first_nvar)
first_pval_int<-
  rep(NA, first_nvar)
first_AIC <- 
  rep(NA, first_nvar)
first_link <- 
  rep(NA, first_nvar)
first_intercept <- 
  rep(NA, first_nvar)
first_coefvar <- 
  rep(NA, first_nvar)
first_coefsam <- 
  rep(NA, first_nvar)
first_coefint <- 
  rep(NA, first_nvar)
first_interceptzval <- 
  rep(NA, first_nvar)
first_coefvarzval <- 
  rep(NA, first_nvar)
first_coefsamzval <- 
  rep(NA, first_nvar)
first_coefintzval <- 
  rep(NA, first_nvar)
###independent
sec_start<-
  33
sec_end<-
  41
sec_nvar<-
  sec_end-sec_start+1
sec_variable<-
  rep(NA, sec_nvar)
sec_disper <- 
  rep(NA, sec_nvar)
sec_pval_var<-
  rep(NA, sec_nvar)
sec_pval_sam<-
  rep(NA, sec_nvar)
sec_pval_int<-
  rep(NA, sec_nvar)
sec_AIC <- 
  rep(NA, sec_nvar)
sec_link <- 
  rep(NA, sec_nvar)
sec_intercept <- 
  rep(NA, sec_nvar)
sec_coefvar <- 
  rep(NA, sec_nvar)
sec_coefsam <- 
  rep(NA, sec_nvar)
sec_coefint <- 
  rep(NA, sec_nvar)
sec_interceptzval <- 
  rep(NA, sec_nvar)
sec_coefvarzval <- 
  rep(NA, sec_nvar)
sec_coefsamzval <- 
  rep(NA, sec_nvar)
sec_coefintzval <- 
  rep(NA, sec_nvar)
number <- 
  1
##loop
for (i in first_start:first_end){
  first = colnames(predcenter)[i]
  for (j in sec_start:sec_end){
    second = colnames(predcenter)[j]
    possibleError <- 
      tryCatch(
        model <- 
          glmer(get(first) ~ get(second)*Sampling +
                  (1|Site/alltrees/quadrats),
                family ="poisson"(link="sqrt"),
                data =predcenter),
        error=function(e) e,
        warning=function(w) w)
    if(inherits(possibleError, c("error", "warning"))) next  
    
    pval_var <- Anova(model)[1,3]
    pval_sam <- Anova(model)[2,3]
    pval_int <- Anova(model)[3,3]
    AIC <- AIC(model)
    link <- model@resp$family$link
    dispersion <- dispersion_glmer(model)
    summary <- summary(model)
    
    
    first_pval_var[number] = as.numeric(pval_var)
    first_pval_sam[number] = as.numeric(pval_sam)
    first_pval_int[number] = as.numeric(pval_int)
    first_AIC[number] = as.numeric(AIC)
    first_link[number] = as.character(link)
    first_variable[number] = first
    first_disper[number] = dispersion
    first_intercept[number] = summary$coefficients[1,1]
    first_coefvar[number] = ifelse(first_pval_var[number] < 0.05,
                                   summary$coefficients[2,1],
                                   0)
    first_coefsam[number] = ifelse(first_pval_sam[number] < 0.05,
                                   summary$coefficients[3,1],
                                   0)
    first_coefint[number] = ifelse(first_pval_int[number] < 0.05,
                                   summary$coefficients[4,1],
                                   0)
    first_interceptzval[number] = summary$coefficients[1,3]
    first_coefvarzval[number] = ifelse(first_pval_var[number] < 0.05,
                                       summary$coefficients[2,3],
                                       0)
    first_coefsamzval[number] = ifelse(first_pval_sam[number] < 0.05,
                                       summary$coefficients[3,3],
                                       0)
    first_coefintzval[number] = ifelse(first_pval_int[number] < 0.05,
                                       summary$coefficients[4,3],
                                       0)
    number = number + 1
    
    sec_pval_var[number] = as.numeric(pval_var)
    sec_pval_sam[number] = as.numeric(pval_sam)
    sec_pval_int[number] = as.numeric(pval_int)
    sec_AIC[number] = as.numeric(AIC)
    sec_link[number] = as.character(link)
    sec_variable[number] = second
    sec_disper[number] = dispersion
    sec_intercept[number] = summary$coefficients[1,1]
    sec_coefvar[number] = ifelse(sec_pval_var[number] < 0.05,
                                 summary$coefficients[2,1],
                                 0)
    sec_coefsam[number] = ifelse(sec_pval_sam[number] < 0.05,
                                 summary$coefficients[3,1],
                                 0)
    sec_coefint[number] = ifelse(sec_pval_int[number] < 0.05,
                                 summary$coefficients[4,1],
                                 0)
    sec_interceptzval[number] = summary$coefficients[1,3]
    sec_coefvarzval[number] = ifelse(sec_pval_var[number] < 0.05,
                                     summary$coefficients[2,3],
                                     0)
    sec_coefsamzval[number] = ifelse(sec_pval_sam[number] < 0.05,
                                     summary$coefficients[3,3],
                                     0)
    sec_coefintzval[number] = ifelse(sec_pval_int[number] < 0.05,
                                     summary$coefficients[4,3],
                                     0)
    number = number + 1
  }
}

##Create dataframe with results
first <- 
  data.frame(first_variable, first_link, first_disper, first_AIC, 
             first_pval_var, first_pval_sam, first_pval_int, 
             first_intercept, first_coefvar, first_coefsam, first_coefint,
             first_interceptzval,first_coefvarzval, first_coefsamzval, first_coefintzval,
             stringsAsFactors = F)
second <- 
  data.frame(sec_variable, sec_link, sec_disper, sec_AIC, 
             sec_pval_var, sec_pval_sam, sec_pval_int, 
             sec_intercept, sec_coefvar, sec_coefsam, sec_coefint,
             sec_interceptzval,sec_coefvarzval, sec_coefsamzval, sec_coefintzval,
             stringsAsFactors = F)

##Rename dataframes and bind
first <- 
  first %>% 
  rename(
    variable = first_variable,
    AIC = first_AIC,
    link = first_link,
    dispersion = first_disper,
    p_variable = first_pval_var,
    p_sampling = first_pval_sam,
    p_interaction = first_pval_int,
    intercept = first_intercept,
    var_coef = first_coefvar,
    sam_coef = first_coefsam,
    interaction_coef = first_coefint,
    intercept_z = first_interceptzval,
    var_z = first_coefvarzval,
    sam_z = first_coefsamzval,
    interaction_z = first_coefintzval
  ) %>% 
  mutate(type = "dependent")

second <- 
  second %>% 
  rename(
    variable = sec_variable,
    AIC = sec_AIC,
    link = sec_link,
    dispersion = sec_disper,
    p_variable = sec_pval_var,
    p_sampling = sec_pval_sam,
    p_interaction = sec_pval_int,
    intercept = sec_intercept,
    var_coef = sec_coefvar,
    sam_coef = sec_coefsam,
    interaction_coef = sec_coefint,
    intercept_z = sec_interceptzval,
    var_z = sec_coefvarzval,
    sam_z = sec_coefsamzval,
    interaction_z = sec_coefintzval) %>% 
  mutate(type = "independent")

association_lastframe_sqrt <- 
  rbind(first, second)
association_lastframe_sqrt <-
  na.omit(association_lastframe_sqrt)


##Bring all variables in one row
association_lastframe_sqrt <- 
  association_lastframe_sqrt %>% 
  spread(type, variable) %>% 
  dplyr::select(dependent, independent, link, dispersion,
                AIC, p_variable, p_sampling, p_interaction,
                intercept, var_coef, sam_coef, interaction_coef,
                intercept_z, var_z, sam_z, interaction_z)

#For inv link function
###dependent
first_start<-
  8
first_end<- 
  15
first_nvar<-
  first_end-first_start+1
first_variable<-
  rep(NA, first_nvar)
first_disper <- 
  rep(NA, first_nvar)
first_pval_var<-
  rep(NA, first_nvar)
first_pval_sam<-
  rep(NA, first_nvar)
first_pval_int<-
  rep(NA, first_nvar)
first_AIC <- 
  rep(NA, first_nvar)
first_link <- 
  rep(NA, first_nvar)
first_intercept <- 
  rep(NA, first_nvar)
first_coefvar <- 
  rep(NA, first_nvar)
first_coefsam <- 
  rep(NA, first_nvar)
first_coefint <- 
  rep(NA, first_nvar)
first_interceptzval <- 
  rep(NA, first_nvar)
first_coefvarzval <- 
  rep(NA, first_nvar)
first_coefsamzval <- 
  rep(NA, first_nvar)
first_coefintzval <- 
  rep(NA, first_nvar)
###independent
sec_start<-
  33
sec_end<-
  41
sec_nvar<-
  sec_end-sec_start+1
sec_variable<-
  rep(NA, sec_nvar)
sec_disper <- 
  rep(NA, sec_nvar)
sec_pval_var<-
  rep(NA, sec_nvar)
sec_pval_sam<-
  rep(NA, sec_nvar)
sec_pval_int<-
  rep(NA, sec_nvar)
sec_AIC <- 
  rep(NA, sec_nvar)
sec_link <- 
  rep(NA, sec_nvar)
sec_intercept <- 
  rep(NA, sec_nvar)
sec_coefvar <- 
  rep(NA, sec_nvar)
sec_coefsam <- 
  rep(NA, sec_nvar)
sec_coefint <- 
  rep(NA, sec_nvar)
sec_interceptzval <- 
  rep(NA, sec_nvar)
sec_coefvarzval <- 
  rep(NA, sec_nvar)
sec_coefsamzval <- 
  rep(NA, sec_nvar)
sec_coefintzval <- 
  rep(NA, sec_nvar)
number <- 
  1
##loop
for (i in first_start:first_end){
  first = colnames(predcenter)[i]
  for (j in sec_start:sec_end){
    second = colnames(predcenter)[j]
    possibleError <- 
      tryCatch(
        model <- 
          glmer(get(first) ~ get(second)*Sampling +
                  (1|Site/alltrees/quadrats),
                family ="poisson"(link="inverse"),
                data =predcenter),
        error=function(e) e,
        warning=function(w) w)
    if(inherits(possibleError, c("error", "warning"))) next  
    
    pval_var <- Anova(model)[1,3]
    pval_sam <- Anova(model)[2,3]
    pval_int <- Anova(model)[3,3]
    AIC <- AIC(model)
    link <- model@resp$family$link
    dispersion <- dispersion_glmer(model)
    summary <- summary(model)
    
    
    first_pval_var[number] = as.numeric(pval_var)
    first_pval_sam[number] = as.numeric(pval_sam)
    first_pval_int[number] = as.numeric(pval_int)
    first_AIC[number] = as.numeric(AIC)
    first_link[number] = as.character(link)
    first_variable[number] = first
    first_disper[number] = dispersion
    first_intercept[number] = summary$coefficients[1,1]
    first_coefvar[number] = ifelse(first_pval_var[number] < 0.05,
                                   summary$coefficients[2,1],
                                   0)
    first_coefsam[number] = ifelse(first_pval_sam[number] < 0.05,
                                   summary$coefficients[3,1],
                                   0)
    first_coefint[number] = ifelse(first_pval_int[number] < 0.05,
                                   summary$coefficients[4,1],
                                   0)
    first_interceptzval[number] = summary$coefficients[1,3]
    first_coefvarzval[number] = ifelse(first_pval_var[number] < 0.05,
                                       summary$coefficients[2,3],
                                       0)
    first_coefsamzval[number] = ifelse(first_pval_sam[number] < 0.05,
                                       summary$coefficients[3,3],
                                       0)
    first_coefintzval[number] = ifelse(first_pval_int[number] < 0.05,
                                       summary$coefficients[4,3],
                                       0)
    number = number + 1
    
    sec_pval_var[number] = as.numeric(pval_var)
    sec_pval_sam[number] = as.numeric(pval_sam)
    sec_pval_int[number] = as.numeric(pval_int)
    sec_AIC[number] = as.numeric(AIC)
    sec_link[number] = as.character(link)
    sec_variable[number] = second
    sec_disper[number] = dispersion
    sec_intercept[number] = summary$coefficients[1,1]
    sec_coefvar[number] = ifelse(sec_pval_var[number] < 0.05,
                                 summary$coefficients[2,1],
                                 0)
    sec_coefsam[number] = ifelse(sec_pval_sam[number] < 0.05,
                                 summary$coefficients[3,1],
                                 0)
    sec_coefint[number] = ifelse(sec_pval_int[number] < 0.05,
                                 summary$coefficients[4,1],
                                 0)
    sec_interceptzval[number] = summary$coefficients[1,3]
    sec_coefvarzval[number] = ifelse(sec_pval_var[number] < 0.05,
                                     summary$coefficients[2,3],
                                     0)
    sec_coefsamzval[number] = ifelse(sec_pval_sam[number] < 0.05,
                                     summary$coefficients[3,3],
                                     0)
    sec_coefintzval[number] = ifelse(sec_pval_int[number] < 0.05,
                                     summary$coefficients[4,3],
                                     0)
    number = number + 1
  }
}

##Create dataframe with results
first <- 
  data.frame(first_variable, first_link, first_disper, first_AIC, 
             first_pval_var, first_pval_sam, first_pval_int, 
             first_intercept, first_coefvar, first_coefsam, first_coefint,
             first_interceptzval,first_coefvarzval, first_coefsamzval, first_coefintzval,
             stringsAsFactors = F)
second <- 
  data.frame(sec_variable, sec_link, sec_disper, sec_AIC, 
             sec_pval_var, sec_pval_sam, sec_pval_int, 
             sec_intercept, sec_coefvar, sec_coefsam, sec_coefint,
             sec_interceptzval,sec_coefvarzval, sec_coefsamzval, sec_coefintzval,
             stringsAsFactors = F)

##Rename dataframes and bind
first <- 
  first %>% 
  rename(
    variable = first_variable,
    AIC = first_AIC,
    link = first_link,
    dispersion = first_disper,
    p_variable = first_pval_var,
    p_sampling = first_pval_sam,
    p_interaction = first_pval_int,
    intercept = first_intercept,
    var_coef = first_coefvar,
    sam_coef = first_coefsam,
    interaction_coef = first_coefint,
    intercept_z = first_interceptzval,
    var_z = first_coefvarzval,
    sam_z = first_coefsamzval,
    interaction_z = first_coefintzval
  ) %>% 
  mutate(type = "dependent")

second <- 
  second %>% 
  rename(
    variable = sec_variable,
    AIC = sec_AIC,
    link = sec_link,
    dispersion = sec_disper,
    p_variable = sec_pval_var,
    p_sampling = sec_pval_sam,
    p_interaction = sec_pval_int,
    intercept = sec_intercept,
    var_coef = sec_coefvar,
    sam_coef = sec_coefsam,
    interaction_coef = sec_coefint,
    intercept_z = sec_interceptzval,
    var_z = sec_coefvarzval,
    sam_z = sec_coefsamzval,
    interaction_z = sec_coefintzval) %>% 
  mutate(type = "independent")

association_lastframe_inv <- 
  rbind(first, second)
association_lastframe_inv <-
  na.omit(association_lastframe_inv)


##Bring all variables in one row
association_lastframe_inv <- 
  association_lastframe_inv %>% 
  spread(type, variable) %>% 
  dplyr::select(dependent, independent, link, dispersion,
                AIC, p_variable, p_sampling, p_interaction,
                intercept, var_coef, sam_coef, interaction_coef,
                intercept_z, var_z, sam_z, interaction_z)
#Now bind all three together and sort them base on p-values, dispersion and AIC
association_lastframe <- 
  rbind(association_lastframe_log,
        association_lastframe_inv,
        association_lastframe_sqrt)

##getting the lowest AIC per pair
calc_lowAIC<- 
  association_lastframe %>% 
  filter(0.75<dispersion & dispersion<1.4) %>% 
  dplyr::select(dependent, independent, link, AIC) %>% 
  group_by(dependent, independent, link) %>% 
  spread(link, AIC) 
calc_lowAIC$link <- 
  colnames(calc_lowAIC[,3:5])[apply(calc_lowAIC[,3:5],1,which.min)]
##finally removing the others
association_lastframe<-
  calc_lowAIC %>% 
  dplyr::select(-log, -sqrt, -inverse) %>% 
  filter(dependent !=independent) %>% 
  left_join(association_lastframe) %>% 
  filter(p_variable < 0.05 | p_interaction < 0.05) %>% 
  mutate(abs_var = abs(var_coef), abs_interaction = abs(interaction_coef))
write.csv(association_lastframe, "associations_herbdivided.csv")
##for clarity
association_clean <- 
  association_lastframe %>% 
  dplyr::select(dependent, independent, p_variable, var_coef, p_interaction, interaction_coef)

# Exploring interactions (run script #7 beforehand) --------------------------------------------------
#Hoppers and bromeliad hunting spiders
jumpmodel_bromhuntspids <- 
  glmer(jump ~ 
          bromhuntspids_center*Sampling +
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="sqrt"),
        data =predcenter)
simulationOutput <- 
  simulateResiduals(fittedModel = jumpmodel_bromhuntspids, 
                    n = 2000)
plotSimulatedResiduals(simulationOutput = simulationOutput,
                       quantreg = F)
summary(jumpmodel_bromhuntspids)
jumptest_bromhuntspids <- 
  mixed(jump~ 
          bromhuntspids_center*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="sqrt"),
        type = afex_options(type = "2"),
        data = predcenter,
        method = "LRT")$anova_table
##plot
###visreg
visreg(jumpmodel_bromhuntspids,
       "bromhuntspids_center", by = "Sampling")
###ggeffect
jumpeffect_bromhuntspids <- 
  ggeffect(jumpmodel_bromhuntspids,
           terms = c("bromhuntspids_center", "Sampling"),
           swap.pred = T,
           type = "re",
           ci.level = 0.95)
jumpeffect_bromhuntspids$group <- 
  factor(jumpeffect_bromhuntspids$group, levels = c("B", "A"))
col <- 
  ifelse(poolcenter$Sampling == "B",
         "darkorange2", 
         "dodgerblue4")

jumpplot_bromhuntspids <- 
  plot(jumpeffect_bromhuntspids,
       ci = T) + 
  geom_point(data = predcenter,
             mapping = aes(x = jitter(bromhuntspids_center, 3),
                           y = jitter(jump, 3)), 
             colour = col, 
             fill = col) +
  ggtitle("") + 
  xlab("Bromeliad-associated active hunting spider abundance") +
  ylab("Hopper abundance") +
  scale_color_manual(labels = c("Before", "After"), 
                     values = c("darkorange2", "dodgerblue4")) +
  theme(legend.position = c(0.9,0.9),
        axis.title.x= element_text(size = 9),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

#Snails and bromeliad ants
snailmodel_bromant <- 
  glmer(snail ~ 
          bromant_center*Sampling +
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="log"),
        data =predcenter)
simulationOutput <- 
  simulateResiduals(fittedModel = snailmodel_bromant, 
                    n = 2000)
plotSimulatedResiduals(simulationOutput = simulationOutput,
                       quantreg = F)
summary(snailmodel_bromant)
snailtest_bromant <- 
  mixed(snail~ 
          bromant_center*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="poisson"(link="log"),
        type = afex_options(type = "2"),
        data = predcenter,
        method = "LRT")$anova_table
visreg(snailmodel_bromant,
       "bromant_center", by = "Sampling")

