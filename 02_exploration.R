# Pattern visualisation ---------------------------------------------------

##Everything
par(mfrow =c(1,2))
plot(leafdamage$propdamage ~ leafdamage$index)
plot(leafdamage$propdamage ~ leafdamage$largeleaf)
par(mfrow = c(1,1))

##B and A difference per treatment
par(mfrow = c(2,3))
plot(leafdamage$propdamage[leafdamage$Sampling == "B"
                           & leafdamage$Treatment == "wr"],
     ylim =c(0,1)) 
plot(leafdamage$propdamage[leafdamage$Sampling == "B"
                           & leafdamage$Treatment == "w"], 
     ylim =c(0,1)) 
plot(leafdamage$propdamage[leafdamage$Sampling == "B"
                           & leafdamage$Treatment == "wo"], 
     ylim =c(0,1)) 
plot(leafdamage$propdamage[leafdamage$Sampling == "A"
                           & leafdamage$Treatment == "wr"], 
     ylim =c(0,1))
plot(leafdamage$propdamage[leafdamage$Sampling == "A"
                           & leafdamage$Treatment == "w"], 
     ylim =c(0,1))
plot(leafdamage$propdamage[leafdamage$Sampling == "A"
                           & leafdamage$Treatment == "wo"],
     ylim =c(0,1))
par(mfrow = c(1,1))
###Same but removing 0s
par(mfrow = c(2,3))
plot(leafdamage$propdamage[leafdamage$Sampling == "B"
                           & leafdamage$Treatment == "wr"
                           & leafdamage$propdamage > 0], 
     ylim =c(0,1)) 
plot(leafdamage$propdamage[leafdamage$Sampling == "B"
                           & leafdamage$Treatment == "w"
                           & leafdamage$propdamage > 0],
     ylim =c(0,1)) 
plot(leafdamage$propdamage[leafdamage$Sampling == "B"
                           & leafdamage$Treatment == "wo"
                           & leafdamage$propdamage > 0], 
     ylim =c(0,1)) 
plot(leafdamage$propdamage[leafdamage$Sampling == "A"
                           & leafdamage$Treatment == "wr"
                           & leafdamage$propdamage > 0], 
     ylim =c(0,1))
plot(leafdamage$propdamage[leafdamage$Sampling == "A"
                           & leafdamage$Treatment == "w"
                           & leafdamage$propdamage > 0],
     ylim =c(0,1))
plot(leafdamage$propdamage[leafdamage$Sampling == "A"
                           & leafdamage$Treatment == "wo"
                           & leafdamage$propdamage > 0], 
     ylim =c(0,1))
par(mfrow =c(1,1))

##Looking at means

mean(leafdamage_noNAs$propdamage[leafdamage_noNAs$propdamage > 0])
mean(leafdamage_noNAs$propdamage)

##looking at distributions
par(mfrow =c(2,1))
hist(leafdamage$propdamage[leafdamage$propdamage > 0])
par(mfrow =c(1,1))

##Is there an original pattern
plot(leafdamage$propdamage[leafdamage$Sampling == "B" 
                           & leafdamage$Treatment == c("wr", "w")] ~
       leafdamage$largeleaf[leafdamage$Sampling == "B" 
                            & leafdamage$Treatment == c("wr", "w")])
plot(propdamage[leafdamage$Sampling == "B"
                & leafdamage$Treatment == "w"] ~ 
       largeleaf[leafdamage$Sampling == "B"
                 & leafdamage$Treatment == "w"],
     xlab = "proximity index", 
     ylab = "leaf damage", 
     data = leafdamage) 

##lets plot everything
##B
plot(leafdamage$largeleaf[leafdamage$Sampling == "B"],
     leafdamage$propdamage[leafdamage$Sampling == "B"],
     col = c("red", "blue", "green")[factor(leafdamage$Treatment[leafdamage$Sampling == "B"])],
     pch = c(17))
##A
plot(leafdamage$largeleaf[leafdamage$Sampling == "A"],
     leafdamage$propdamage[leafdamage$Sampling == "A"],
     col = c("red", "blue", "green")[factor(leafdamage$Treatment)[leafdamage$Sampling == "A"]],
     pch = c(17))

##How many 0s total
nrow(leafdamage_noNAs %>% 
       filter(propdamage == 0))

nrow(leafdamage_noNAs %>% filter(propdamage == 0))/nrow(leafdamage_noNAs)
nrow(leafdamage_noNAs %>% 
       filter(propdamage < 0.05))









# Bromeliad-Tree community comparisons ----------------------------------
bromtree_cca <- 
  vegan::cca(bromtree_comparison ~ loc, 
           data = whereloc)

anova.cca(bromtree_cca,
          permutations = 2000,
          strata = whereloc$Site)
pdf("bromtree_cca.pdf",
    width = 10,
    height = 10)
ordiplot(bromtree_cca,
         type ="n")
col <- 
  ifelse(whereloc$loc == "brom",
         "saddlebrown", "darkgreen")
points(bromtree_cca, 
       display = 'sites', 
       pch = 16,
       col=col)
ordihull(bromtree_cca,
         groups = whereloc$loc,
         show.groups = "brom",
         draw = "polygon",
         border =  "transparent",
         col = "saddlebrown")
ordihull(bromtree_cca,
         groups = whereloc$loc,
         show.groups = "tree",
         draw = "polygon",
         border =  "transparent",
         col = "darkgreen")

legend ("topleft", 
        legend = c("Bromeliad communities",
                   "Tree communities"), 
        pch = 16, 
        pt.bg = c("saddlebrown", "darkgreen"), 
        col = c("saddlebrown", "darkgreen"))
dev.off()



# Bromeliad-Tree community comparison per site -----------------------------------------------
#CCA
##CP
bromtree_comparisoncp <- 
  puramona %>% 
  ungroup %>% 
  filter(Site=="CP") %>% 
  dplyr::select(Site, kind, Sampling, alltrees, Abundance) %>% 
  group_by(Site, kind, Sampling, alltrees) %>% 
  summarise_all(funs(sum)) %>% 
  mutate(loc = "tree") %>% 
  bind_rows(purabromzy %>% 
              ungroup %>% 
              filter(Site =="CP") %>% 
              dplyr::select(Site, kind, Sampling, alltrees, Abundance) %>% 
              group_by(Site,kind, Sampling, alltrees) %>% 
              summarise_all(funs(sum)) %>% 
              mutate(loc = "brom")) %>% 
  spread(key= kind, 
         Abundance, 
         fill = 0) %>% 
  unite(where, alltrees, loc, Sampling, sep= "_", remove = F)
bromtree_comparisoncp <- 
  data.frame(bromtree_comparisoncp,
             row.names = bromtree_comparisoncp$where)
whereloccp <- 
  bromtree_comparisoncp[,1:5]
bromtree_comparisoncp <- 
  data.frame(bromtree_comparisoncp[,6:42])
#Remove kinds with less than 10
bromtree_comparisoncp <- 
  bromtree_comparisoncp[,colSums(bromtree_comparisoncp) > 10]
#Hellinger transformation
bromtree_comparisoncp <- 
  data.frame(decostand(bromtree_comparisoncp, 
                       "hellinger"))
##CCA
bromtree_cca_cp <- 
  vegan::cca(bromtree_comparisoncp ~ loc, 
             data = whereloccp %>% 
               filter(Site =="CP"))
anova.cca(bromtree_cca_cp,
          permutations = 2000,
          strata = whereloccp$Site)
##DO
bromtree_comparisondo <- 
  puramona %>% 
  ungroup %>% 
  filter(Site=="DO") %>% 
  dplyr::select(Site, kind, Sampling, alltrees, Abundance) %>% 
  group_by(Site, kind, Sampling, alltrees) %>% 
  summarise_all(funs(sum)) %>% 
  mutate(loc = "tree") %>% 
  bind_rows(purabromzy %>% 
              ungroup %>% 
              filter(Site =="DO") %>% 
              dplyr::select(Site, kind, Sampling, alltrees, Abundance) %>% 
              group_by(Site,kind, Sampling, alltrees) %>% 
              summarise_all(funs(sum)) %>% 
              mutate(loc = "brom")) %>% 
  spread(key= kind, 
         Abundance, 
         fill = 0) %>% 
  unite(where, alltrees, loc, Sampling, sep= "_", remove = F)
bromtree_comparisondo <- 
  data.frame(bromtree_comparisondo,
             row.names = bromtree_comparisondo$where)
wherelocdo <- 
  bromtree_comparisondo[,1:5]
bromtree_comparisondo <- 
  data.frame(bromtree_comparisondo[,6:42])
#Remove kinds with less than 10
bromtree_comparisondo <- 
  bromtree_comparisondo[,colSums(bromtree_comparisondo) > 10]
#Hellinger transformation
bromtree_comparisondo <- 
  data.frame(decostand(bromtree_comparisondo, 
                       "hellinger"))
##CCA
bromtree_cca_do <- 
  vegan::cca(bromtree_comparisondo ~ loc, 
             data = wherelocdo %>% 
               filter(Site =="DO"))
anova.cca(bromtree_cca_do,
          permutations = 2000,
          strata = wherelocdo$Site)
##ER
bromtree_comparisoner <- 
  puramona %>% 
  ungroup %>% 
  filter(Site=="ER") %>% 
  dplyr::select(Site, kind, Sampling, alltrees, Abundance) %>% 
  group_by(Site, kind, Sampling, alltrees) %>% 
  summarise_all(funs(sum)) %>% 
  mutate(loc = "tree") %>% 
  bind_rows(purabromzy %>% 
              ungroup %>% 
              filter(Site =="ER") %>% 
              dplyr::select(Site, kind, Sampling, alltrees, Abundance) %>% 
              group_by(Site,kind, Sampling, alltrees) %>% 
              summarise_all(funs(sum)) %>% 
              mutate(loc = "brom")) %>% 
  spread(key= kind, 
         Abundance, 
         fill = 0) %>% 
  unite(where, alltrees, loc, Sampling, sep= "_", remove = F)
bromtree_comparisoner <- 
  data.frame(bromtree_comparisoner,
             row.names = bromtree_comparisoner$where)
wherelocer <- 
  bromtree_comparisoner[,1:5]
bromtree_comparisoner <- 
  data.frame(bromtree_comparisoner[,6:46])
#Remove kinds with less than 10
bromtree_comparisoner <- 
  bromtree_comparisoner[,colSums(bromtree_comparisoner) > 10]
#Hellinger transformation
bromtree_comparisoner <- 
  data.frame(decostand(bromtree_comparisoner, 
                       "hellinger"))
##CCA
bromtree_cca_er <- 
  vegan::cca(bromtree_comparisoner ~ loc, 
             data = wherelocer %>% 
               filter(Site =="ER"))
anova.cca(bromtree_cca_er,
          permutations = 2000,
          strata = wherelocer$Site)
##Plot
pdf("bromtree_site.pdf",
    height=20,
    width=10)
par(mfrow= c(3,1))
ordiplot(bromtree_cca_cp,
         type ="n")
col <- 
  ifelse(whereloccp$loc == "brom",
         "saddlebrown", "darkgreen")
points(bromtree_cca_cp, 
       display = 'sites', 
       pch = 16,
       col=col)
ordihull(bromtree_cca_cp,
         groups = whereloccp$loc,
         show.groups = "brom",
         draw = "polygon",
         border =  "transparent",
         col = "saddlebrown")
ordihull(bromtree_cca_cp,
         groups = whereloccp$loc,
         show.groups = "tree",
         draw = "polygon",
         border =  "transparent",
         col = "darkgreen")
title(main= "CP")
ordiplot(bromtree_cca_er,
         type ="n")
col <- 
  ifelse(wherelocer$loc == "brom",
         "saddlebrown", "darkgreen")
points(bromtree_cca_er, 
       display = 'sites', 
       pch = 16,
       col=col)
ordihull(bromtree_cca_er,
         groups = wherelocer$loc,
         show.groups = "brom",
         draw = "polygon",
         border =  "transparent",
         col = "saddlebrown")
ordihull(bromtree_cca_er,
         groups = wherelocer$loc,
         show.groups = "tree",
         draw = "polygon",
         border =  "transparent",
         col = "darkgreen")
title(main= "ER")
ordiplot(bromtree_cca_do,
         type ="n")
col <- 
  ifelse(wherelocdo$loc == "brom",
         "saddlebrown", "darkgreen")
points(bromtree_cca_do, 
       display = 'sites', 
       pch = 16,
       col=col)
ordihull(bromtree_cca_do,
         groups = wherelocdo$loc,
         show.groups = "brom",
         draw = "polygon",
         border =  "transparent",
         col = "saddlebrown")
ordihull(bromtree_cca_do,
         groups = wherelocdo$loc,
         show.groups = "tree",
         draw = "polygon",
         border =  "transparent",
         col = "darkgreen")
title(main= "DO")
dev.off()

# Plotting herbivores and predators ----------------------------------------------------------------
#Predator and herbivore abundance with volume proximity index
pdf("predherb_largeleaf.pdf",
    width = 10,
    height = 10)
gridExtra::grid.arrange(grobs = list(brompredplot_largeleaf,
                                     herbplot_largeleaf),
                        ncol = 1)
dev.off()

#Predator and herbivore abundance with treatment
pdf("predherb_treatment.pdf",
    width = 5,
    height = 7)
gridExtra::grid.arrange(grobs = list(brompredplot_treatment,
                                     herbplot_treatment),
                        ncol = 1)
dev.off()

#Psyllids and volume proximity index
pdf("psyllid_largeleaf.pdf",
    width = 10,
    height = 5)
psyllidplot_largeleaf
dev.off()

# Large treatment grid ----------------------------------
#Treatment
pdf("panel_treatment.pdf",
    height = 20,
    width = 15)
grid.arrange(grobs = list(predsplot_treatment + ggtitle("a") + theme(legend.position = "none",
                                                                     axis.title.x=element_blank(),
                                                                     axis.text.x=element_blank(),
                                                                     axis.ticks.x=element_blank(),
                                                                     axis.title=element_text(size=rel(1.5))), 
                          brompredplot_treatment+ ggtitle("b")+ theme(legend.title=element_text(size=rel(1.5)), 
                                                                      legend.text=element_text(size=rel(1.5)),
                                                                      axis.title.x=element_blank(),
                                                                      axis.text.x=element_blank(),
                                                                      axis.ticks.x=element_blank(),
                                                                      axis.title=element_text(size=rel(1.5))),
                          bromantsplot_treatment+ ggtitle("c")+ theme(legend.position = "none",
                                                                      axis.title.x=element_blank(),
                                                                      axis.text.x=element_blank(),
                                                                      axis.ticks.x=element_blank(),
                                                                      axis.title=element_text(size=rel(1.5))), 
                          bromhuntspidsplot_treatment+ ggtitle("d")+ theme(legend.position = "none",
                                                                           axis.title.x=element_blank(),
                                                                           axis.text.x=element_blank(),
                                                                           axis.ticks.x=element_blank(),
                                                                           axis.title=element_text(size=rel(1.2))),
                          nobromantsplot_treatment+ ggtitle("e")+ theme(legend.position = "none",
                                                                        axis.title.x=element_blank(),
                                                                        axis.text.x=element_blank(),
                                                                        axis.ticks.x=element_blank(),
                                                                        axis.title=element_text(size=rel(1.5))), 
                          nobromhuntspidsplot_treatment+ ggtitle("f")+ theme(legend.position = "none",
                                                                             axis.title.x=element_blank(),
                                                                             axis.text.x=element_blank(),
                                                                             axis.ticks.x=element_blank(),
                                                                             axis.title=element_text(size=rel(1.2))),
                          herbplot_treatment + ggtitle("g")+ theme(legend.position = "none",
                                                                   axis.title=element_text(size=rel(1.5)),
                                                                   axis.text.x=element_text(size=rel(1.5))), 
                          leafpoolplot_treatment + ggtitle("h")+ theme(legend.position = "none",
                                                                       axis.title=element_text(size=rel(1.5)),
                                                                       axis.text.x=element_text(size=rel(1.5)))),
             ncol = 2)
dev.off()

# Large treatment grid per site----------------------------------
#CP
##All predatores
predsmodelcp_treatment <- 
  glmer(preds ~
          Treatment*Sampling +
          (1|alltrees/quadrats),
        family="poisson"(link ="log"),
        data = poolcenter %>% 
          filter(Site =="CP"))
simulationOutput <- 
  simulateResiduals(fittedModel = predsmodelcp_treatment, 
                    n = 2000)
plot(simulationOutput, 
     asFactor = T,
     quantreg = F)
summary(predsmodelcp_treatment)
predstestcp_treatment <- 
  mixed(preds~ 
          Treatment*Sampling + 
          (1|alltrees/quadrats),
        family="poisson"(link ="log"),
        type = afex_options(type = "2"),
        data = poolcenter %>% 
          filter(Site=="CP"),
        method = "LRT")$anova_table
##plot
###visreg
visreg(predsmodelcp_treatment,
       "Sampling", by = "Treatment")
###ggeffect
predseffectcp_treatment <- 
  ggeffect(predsmodelcp_treatment,
           type = "re",
           x.as.factor = T,
           terms = c("Sampling", "Treatment"),
           ci.lvl = 0.95)
predseffectcp_treatment$x <- 
  factor(predseffectcp_treatment$x, levels = c("B", "A"))
predseffectcp_treatment$group <- 
  factor(predseffectcp_treatment$group, levels = c("wo", "w", "wr"))
predsplotcp_treatment <- 
  ggplot(predseffectcp_treatment, 
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
##Bromeliad-associated predators
brompredmodelcp_treatment <- 
  glmer(bromypred ~
          Treatment*Sampling +
          (1|alltrees/quadrats),
        family="poisson"(link ="sqrt"),
        data = poolcenter %>% 
          filter(Site =="CP"))
simulationOutput <- 
  simulateResiduals(fittedModel = brompredmodelcp_treatment, 
                    n = 2000)
plot(simulationOutput, 
     asFactor = T,
     quantreg = F)
summary(brompredmodelcp_treatment)
brompredtestcp_treatment <- 
  mixed(bromypred~ 
          Treatment*Sampling + 
          (1|alltrees/quadrats),
        family="poisson"(link ="sqrt"),
        type = afex_options(type = "2"),
        data = poolcenter %>% 
          filter(Site =="CP"),
        method = "LRT")$anova_table
##plot
###visreg
visreg(brompredmodelcp_treatment,
       "Sampling", by = "Treatment")
###ggeffect
brompredeffectcp_treatment <- 
  ggeffect(brompredmodelcp_treatment,
           type = "re",
           x.as.factor = T,
           terms = c("Sampling", "Treatment"),
           ci.lvl = 0.95)
brompredeffectcp_treatment$x <- 
  factor(brompredeffectcp_treatment$x, levels = c("B", "A"))
brompredeffectcp_treatment$group <- 
  factor(brompredeffectcp_treatment$group, levels = c("wo", "w", "wr"))
brompredplotcp_treatment <- 
  ggplot(brompredeffectcp_treatment, 
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
  scale_color_manual(name = "Treatment",
                     labels = c("Wihout", "With", "Removal"),
                     values = c("darkgreen", "saddlebrown", "ivory4")) +
  theme(legend.position = c(0.9,0.9),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))
##Bromeliad-associated ants
bromantsmodelcp_treatment <- 
  glmer(bromants ~
          Treatment*Sampling +
          (1|alltrees/quadrats),
        family="poisson"(link ="sqrt"),
        data = predcenter %>% 
          filter(Site =="CP"))
simulationOutput <- 
  simulateResiduals(fittedModel = bromantsmodelcp_treatment, 
                    n = 2000)
plot(simulationOutput, 
     asFactor = T,
     quantreg = F)
summary(bromantsmodelcp_treatment)
bromantstestcp_treatment <- 
  mixed(bromants~ 
          Treatment*Sampling + 
          (1|alltrees/quadrats),
        family="poisson"(link ="sqrt"),
        type = afex_options(type = "2"),
        data = predcenter %>% 
          filter(Site =="CP"),
        method = "LRT")$anova_table
##plot
###visreg
visreg(bromantsmodelcp_treatment,
       "Sampling", by = "Treatment")
###ggeffect
bromantseffectcp_treatment <- 
  ggeffect(bromantsmodelcp_treatment,
           type = "re",
           x.as.factor = T,
           terms = c("Sampling", "Treatment"),
           ci.lvl = 0.95)
bromantseffectcp_treatment$x <- 
  factor(bromantseffectcp_treatment$x, levels = c("B", "A"))
bromantseffectcp_treatment$group <- 
  factor(bromantseffectcp_treatment$group, levels = c("wo", "w", "wr"))
bromantsplotcp_treatment <- 
  ggplot(bromantseffectcp_treatment, 
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
##Bromeliad-associated hunting spiders
bromhuntspidsmodelcp_treatment <- 
  glmer(bromhuntspids ~
          Treatment*Sampling +
          (1|alltrees/quadrats),
        family ="poisson"(link ="log"),
        control = glmerControl(optimizer="bobyqa"),
        data = predcenter %>% 
          filter(Site =="CP"))
simulationOutput <- 
  simulateResiduals(fittedModel = bromhuntspidsmodelcp_treatment, 
                    n = 2000)
plot(simulationOutput, 
     asFactor = T,
     quantreg = F)
summary(bromhuntspidsmodelcp_treatment)
bromhuntspidstestcp_treatment <- 
  mixed(bromhuntspids~ 
          Treatment*Sampling + 
          (1|alltrees/quadrats),
        family="poisson"(link ="log"),
        type = afex_options(type = "2"),
        control = glmerControl(optimizer="bobyqa"),
        data = predcenter %>% 
          filter(Site =="CP"),
        method = "LRT")$anova_table
##plot
###visreg
visreg(bromhuntspidsmodelcp_treatment,
       "Sampling", by = "Treatment")
###ggeffect
bromhuntspidseffectcp_treatment <- 
  ggeffect(bromhuntspidsmodelcp_treatment,
           type = "re",
           x.as.factor = T,
           terms = c("Sampling", "Treatment"),
           ci.lvl = 0.95)
bromhuntspidseffectcp_treatment$x <- 
  factor(bromhuntspidseffectcp_treatment$x, levels = c("B", "A"))
bromhuntspidseffectcp_treatment$group <- 
  factor(bromhuntspidseffectcp_treatment$group, levels = c("wo", "w", "wr"))
bromhuntspidsplotcp_treatment <- 
  ggplot(bromhuntspidseffectcp_treatment, 
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
##Non-bromeliad-associated ants
nobromantsmodelcp_treatment <- 
  glmer(I(ants-bromants) ~
          Treatment*Sampling +
          (1|alltrees/quadrats),
        family ="poisson"(link ="sqrt"),
        data = predcenter %>% 
          filter(Site =="CP"))
simulationOutput <- 
  simulateResiduals(fittedModel = nobromantsmodelcp_treatment, 
                    n = 2000)
plot(simulationOutput, 
     asFactor = T,
     quantreg = F)
summary(nobromantsmodelcp_treatment)
nobromantstestcp_treatment <- 
  mixed(I(ants-bromants)~ 
          Treatment*Sampling + 
          (1|alltrees/quadrats),
        family="poisson"(link ="sqrt"),
        type = afex_options(type = "2"),
        data = predcenter %>% 
          filter(Site =="CP"),
        method = "LRT")$anova_table
##plot
###visreg
visreg(nobromantsmodelcp_treatment,
       "Sampling", by = "Treatment")
###ggeffect
nobromantseffectcp_treatment <- 
  ggeffect(nobromantsmodelcp_treatment,
           type = "re",
           x.as.factor = T,
           terms = c("Sampling", "Treatment"),
           ci.lvl = 0.95)
nobromantseffectcp_treatment$x <- 
  factor(nobromantseffectcp_treatment$x, levels = c("B", "A"))
nobromantseffectcp_treatment$group <- 
  factor(nobromantseffectcp_treatment$group, levels = c("wo", "w", "wr"))
nobromantsplotcp_treatment <- 
  ggplot(nobromantseffectcp_treatment, 
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
  ylab(" Non bromeliad-associated ant abundance") +
  scale_color_manual(name = "Treatment",
                     labels = c("Wihout", "With", "Removal"),
                     values = c("darkgreen", "saddlebrown", "ivory4")) +
  theme(legend.position = c(0.9,0.9),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))
##Non-bromeliad-associated hunting spiders
nobromhuntspidsmodelcp_treatment <- 
  glmer(I(huntspids-bromhuntspids) ~
          Treatment*Sampling +
          (1|alltrees/quadrats),
        family ="poisson"(link ="log"),
        data = predcenter %>% 
          filter(Site =="CP"))
simulationOutput <- 
  simulateResiduals(fittedModel = nobromhuntspidsmodelcp_treatment, 
                    n = 2000)
plot(simulationOutput, 
     asFactor = T,
     quantreg = F)
summary(nobromhuntspidsmodelcp_treatment)
nobromhuntspidstestcp_treatment <- 
  mixed(I(huntspids-bromhuntspids)~ 
          Treatment*Sampling + 
          (1|alltrees/quadrats),
        family="poisson"(link ="log"),
        type = afex_options(type = "2"),
        data = predcenter %>% 
          filter(Site =="CP"),
        method = "LRT")$anova_table
##plot
###visreg
visreg(nobromhuntspidsmodelcp_treatment,
       "Sampling", by = "Treatment")
###ggeffect
nobromhuntspidseffectcp_treatment <- 
  ggeffect(nobromhuntspidsmodelcp_treatment,
           type = "re",
           x.as.factor = T,
           terms = c("Sampling", "Treatment"),
           ci.lvl = 0.95)
nobromhuntspidseffectcp_treatment$x <- 
  factor(nobromhuntspidseffectcp_treatment$x, levels = c("B", "A"))
nobromhuntspidseffectcp_treatment$group <- 
  factor(nobromhuntspidseffectcp_treatment$group, levels = c("wo", "w", "wr"))
nobromhuntspidsplotcp_treatment <- 
  ggplot(nobromhuntspidseffectcp_treatment, 
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
  ylab("Non bromeliad-associated hunting spider abundance") +
  scale_color_manual(name = "Treatment",
                     labels = c("Wihout", "With", "Removal"),
                     values = c("darkgreen", "saddlebrown", "ivory4")) +
  theme(legend.position = c(0.9,0.9),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))
##Herbivores
herbmodelcp_treatment <- 
  glmer(herb ~
          Treatment*Sampling +
          (1|alltrees/quadrats),
        family ="poisson"(link ="sqrt"),
        data = poolcenter %>% 
          filter(Site =="CP"))
simulationOutput <- 
  simulateResiduals(fittedModel = herbmodelcp_treatment, 
                    n = 2000)
plot(simulationOutput, 
     asFactor = T,
     quantreg = F)
summary(herbmodelcp_treatment)
herbtestcp_treatment <- 
  mixed(herb~ 
          Treatment*Sampling + 
          (1|alltrees/quadrats),
        family="poisson"(link ="sqrt"),
        type = afex_options(type = "2"),
        data = poolcenter %>% 
          filter(Site =="CP"),
        method = "LRT")$anova_table
##plot
###visreg
visreg(herbmodelcp_treatment,
       "Sampling", by = "Treatment")
###ggeffect
herbeffectcp_treatment <- 
  ggeffect(herbmodelcp_treatment,
           type = "re",
           x.as.factor = T,
           terms = c("Sampling", "Treatment"),
           ci.lvl = 0.95)
herbeffectcp_treatment$x <- 
  factor(herbeffectcp_treatment$x, levels = c("B", "A"))
herbeffectcp_treatment$group <- 
  factor(herbeffectcp_treatment$group, levels = c("wo", "w", "wr"))
herbplotcp_treatment <- 
  ggplot(herbeffectcp_treatment, 
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
  scale_color_manual(name = "Treatment",
                     labels = c("Wihout", "With", "Removal"),
                     values = c("darkgreen", "saddlebrown", "ivory4")) +
  theme(legend.position = c(0.9,0.9),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))
#Leaf damage
leafpoolmodelcp_treatment <- 
  glmer.nb(I(round((propdamage +0.01)*100))~ 
             Treatment*Sampling + 
             (1|alltrees/quadrats),
           control = glmerControl(optimizer = "bobyqa"),
           data = poolcenter %>% 
             filter(Site =="CP"))
simulationOutput <- 
  simulateResiduals(fittedModel = leafpoolmodelcp_treatment, 
                    n = 2000)
plot(simulationOutput,
     asFactor = T,
     quantreg = F)
summary(leafpoolmodelcp_treatment)
leafpooltestcp_treatment <- 
  mixed(I(round((propdamage +0.01)*100))~ 
          Treatment*Sampling + 
          (1|alltrees/quadrats),
        family ="negative.binomial"(theta = getME(leafpoolmodelcp_treatment, "glmer.nb.theta")),
        type = afex_options(type = "2"),
        control = glmerControl(optimizer = "bobyqa"),
        data = poolcenter %>% 
          filter(Site =="CP"),
        method = "LRT")$anova_table
##plot
###visreg
visreg(leafpoolmodelcp_treatment,
       "Treatment", by = "Sampling")
###ggeffect
leafpooleffectcp_treatment <- 
  ggeffect(leafpoolmodelcp_treatment,
           type = "re",
           x.as.factor = T,
           terms = c("Sampling", "Treatment"),
           ci.lvl = 0.95)
leafpooleffectcp_treatment$x <- 
  factor(leafpooleffectcp_treatment$x, levels = c("B", "A"))
leafpooleffect_treatment$group <- 
  factor(leafpooleffectcp_treatment$group, levels = c("wo", "w", "wr"))
leafpoolplotcp_treatment <- 
  ggplot(leafpooleffectcp_treatment, 
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
##plot
pdf("panel_treatmentcp.pdf",
    height = 20,
    width = 20)
grid.arrange(grobs = list(predsplotcp_treatment + ggtitle("a") + theme(legend.position = "none",
                                                                     axis.title.x=element_blank(),
                                                                     axis.text.x=element_blank(),
                                                                     axis.ticks.x=element_blank()), 
                          brompredplotcp_treatment+ ggtitle("b, only interaction significant")+ theme(axis.title.x=element_blank(),
                                                                      axis.text.x=element_blank(),
                                                                      axis.ticks.x=element_blank()),
                          bromantsplotcp_treatment+ ggtitle("c, only interaction signifcant")+ theme(legend.position = "none",
                                                                      axis.title.x=element_blank(),
                                                                      axis.text.x=element_blank(),
                                                                      axis.ticks.x=element_blank()), 
                          bromhuntspidsplotcp_treatment+ ggtitle("d, not significant")+ theme(legend.position = "none",
                                                                           axis.title.x=element_blank(),
                                                                           axis.text.x=element_blank(),
                                                                           axis.ticks.x=element_blank()),
                          nobromantsplotcp_treatment+ ggtitle("e, not significant")+ theme(legend.position = "none",
                                                                        axis.title.x=element_blank(),
                                                                        axis.text.x=element_blank(),
                                                                        axis.ticks.x=element_blank()), 
                          nobromhuntspidsplotcp_treatment+ ggtitle("f")+ theme(legend.position = "none",
                                                                             axis.title.x=element_blank(),
                                                                             axis.text.x=element_blank(),
                                                                             axis.ticks.x=element_blank()),
                          herbplotcp_treatment + ggtitle("g")+ theme(legend.position = "none"), 
                          leafpoolplotcp_treatment + ggtitle("h, treatment significant")+ theme(legend.position = "none")),
             ncol = 2)
dev.off()


#ER
##All predatores
predsmodeler_treatment <- 
  glmer.nb(preds ~
          Treatment*Sampling +
          (1|alltrees/quadrats),
        family="poisson"(link ="sqrt"),
        control=glmerControl(optimizer="bobyqa"),
        data = poolcenter %>% 
          filter(Site =="ER"))
simulationOutput <- 
  simulateResiduals(fittedModel = predsmodeler_treatment, 
                    n = 2000)
plot(simulationOutput, 
     asFactor = T,
     quantreg = F)
summary(predsmodeler_treatment)
predstester_treatment <- 
  mixed(preds~ 
          Treatment*Sampling + 
          (1|alltrees/quadrats),
        family="negative.binomial"(theta=getME(predsmodeler_treatment,
                                               "glmer.nb.theta")),
        control=glmerControl(optimizer="bobyqa"),
        type = afex_options(type = "2"),
        data = poolcenter %>% 
          filter(Site=="ER"),
        method = "LRT")$anova_table
##plot
###visreg
visreg(predsmodeler_treatment,
       "Sampling", by = "Treatment")
###ggeffect
predseffecter_treatment <- 
  ggeffect(predsmodeler_treatment,
           type = "re",
           x.as.factor = T,
           terms = c("Sampling", "Treatment"),
           ci.lvl = 0.95)
predseffecter_treatment$x <- 
  factor(predseffecter_treatment$x, levels = c("B", "A"))
predseffecter_treatment$group <- 
  factor(predseffecter_treatment$group, levels = c("wo", "w", "wr"))
predsploter_treatment <- 
  ggplot(predseffecter_treatment, 
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
##Bromeliad-associated predators
brompredmodeler_treatment <- 
  glmer(bromypred ~
          Treatment*Sampling +
          (1|alltrees/quadrats),
        family="poisson"(link ="log"),
        data = poolcenter %>% 
          filter(Site =="ER"))
simulationOutput <- 
  simulateResiduals(fittedModel = brompredmodeler_treatment, 
                    n = 2000)
plot(simulationOutput, 
     asFactor = T,
     quantreg = F)
summary(brompredmodeler_treatment)
brompredtester_treatment <- 
  mixed(bromypred~ 
          Treatment*Sampling + 
          (1|alltrees/quadrats),
        family="poisson"(link ="log"),
        type = afex_options(type = "2"),
        data = poolcenter %>% 
          filter(Site =="ER"),
        method = "LRT")$anova_table
##plot
###visreg
visreg(brompredmodeler_treatment,
       "Sampling", by = "Treatment")
###ggeffect
brompredeffecter_treatment <- 
  ggeffect(brompredmodeler_treatment,
           type = "re",
           x.as.factor = T,
           terms = c("Sampling", "Treatment"),
           ci.lvl = 0.95)
brompredeffecter_treatment$x <- 
  factor(brompredeffecter_treatment$x, levels = c("B", "A"))
brompredeffecter_treatment$group <- 
  factor(brompredeffecter_treatment$group, levels = c("wo", "w", "wr"))
brompredploter_treatment <- 
  ggplot(brompredeffecter_treatment, 
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
  scale_color_manual(name = "Treatment",
                     labels = c("Wihout", "With", "Removal"),
                     values = c("darkgreen", "saddlebrown", "ivory4")) +
  theme(legend.position = c(0.9,0.9),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))
##Bromeliad-associated ants
bromantsmodeler_treatment <- 
  glmer(bromants ~
          Treatment*Sampling +
          (1|alltrees/quadrats),
        family="poisson"(link ="log"),
        data = predcenter %>% 
          filter(Site =="ER"))
simulationOutput <- 
  simulateResiduals(fittedModel = bromantsmodeler_treatment, 
                    n = 2000)
plot(simulationOutput, 
     asFactor = T,
     quantreg = F)
summary(bromantsmodeler_treatment)
bromantstester_treatment <- 
  mixed(bromants~ 
          Treatment*Sampling + 
          (1|alltrees/quadrats),
        family="poisson"(link ="log"),
        type = afex_options(type = "2"),
        data = predcenter %>% 
          filter(Site =="ER"),
        method = "LRT")$anova_table
##plot
###visreg
visreg(bromantsmodeler_treatment,
       "Sampling", by = "Treatment")
###ggeffect
bromantseffecter_treatment <- 
  ggeffect(bromantsmodeler_treatment,
           type = "re",
           x.as.factor = T,
           terms = c("Sampling", "Treatment"),
           ci.lvl = 0.95)
bromantseffecter_treatment$x <- 
  factor(bromantseffecter_treatment$x, levels = c("B", "A"))
bromantseffecter_treatment$group <- 
  factor(bromantseffecter_treatment$group, levels = c("wo", "w", "wr"))
bromantsploter_treatment <- 
  ggplot(bromantseffecter_treatment, 
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
##Bromeliad-associated hunting spiders
bromhuntspidsmodeler_treatment <- 
  glmer(bromhuntspids ~
          Treatment*Sampling +
          (1|alltrees/quadrats),
        family ="poisson"(link ="log"),
        control = glmerControl(optimizer="bobyqa"),
        data = predcenter %>% 
          filter(Site =="ER"))
simulationOutput <- 
  simulateResiduals(fittedModel = bromhuntspidsmodeler_treatment, 
                    n = 2000)
plot(simulationOutput, 
     asFactor = T,
     quantreg = F)
summary(bromhuntspidsmodeler_treatment)
bromhuntspidstester_treatment <- 
  mixed(bromhuntspids~ 
          Treatment*Sampling + 
          (1|alltrees/quadrats),
        family="poisson"(link ="log"),
        type = afex_options(type = "2"),
        control = glmerControl(optimizer="bobyqa"),
        data = predcenter %>% 
          filter(Site =="ER"),
        method = "LRT")$anova_table
##plot
###visreg
visreg(bromhuntspidsmodeler_treatment,
       "Sampling", by = "Treatment")
###ggeffect
bromhuntspidseffecter_treatment <- 
  ggeffect(bromhuntspidsmodeler_treatment,
           type = "re",
           x.as.factor = T,
           terms = c("Sampling", "Treatment"),
           ci.lvl = 0.95)
bromhuntspidseffecter_treatment$x <- 
  factor(bromhuntspidseffecter_treatment$x, levels = c("B", "A"))
bromhuntspidseffecter_treatment$group <- 
  factor(bromhuntspidseffecter_treatment$group, levels = c("wo", "w", "wr"))
bromhuntspidsploter_treatment <- 
  ggplot(bromhuntspidseffecter_treatment, 
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
##Non-bromeliad-associated ants
nobromantsmodeler_treatment <- 
  glmer(I(ants-bromants) ~
          Treatment*Sampling +
          (1|alltrees/quadrats),
        family ="poisson"(link ="log"),
        data = predcenter %>% 
          filter(Site =="ER"))
simulationOutput <- 
  simulateResiduals(fittedModel = nobromantsmodeler_treatment, 
                    n = 2000)
plot(simulationOutput, 
     asFactor = T,
     quantreg = F)
summary(nobromantsmodeler_treatment)
nobromantstester_treatment <- 
  mixed(I(ants-bromants)~ 
          Treatment*Sampling + 
          (1|alltrees/quadrats),
        family="poisson"(link ="log"),
        type = afex_options(type = "2"),
        data = predcenter %>% 
          filter(Site =="ER"),
        method = "LRT")$anova_table
##plot
###visreg
visreg(nobromantsmodeler_treatment,
       "Sampling", by = "Treatment")
###ggeffect
nobromantseffecter_treatment <- 
  ggeffect(nobromantsmodeler_treatment,
           type = "re",
           x.as.factor = T,
           terms = c("Sampling", "Treatment"),
           ci.lvl = 0.95)
nobromantseffecter_treatment$x <- 
  factor(nobromantseffecter_treatment$x, levels = c("B", "A"))
nobromantseffecter_treatment$group <- 
  factor(nobromantseffecter_treatment$group, levels = c("wo", "w", "wr"))
nobromantsploter_treatment <- 
  ggplot(nobromantseffecter_treatment, 
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
  ylab(" Non bromeliad-associated ant abundance") +
  scale_color_manual(name = "Treatment",
                     labels = c("Wihout", "With", "Removal"),
                     values = c("darkgreen", "saddlebrown", "ivory4")) +
  theme(legend.position = c(0.9,0.9),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))
##Non-bromeliad-associated hunting spiders
nobromhuntspidsmodeler_treatment <- 
  glmer(I(huntspids-bromhuntspids) ~
          Treatment*Sampling +
          (1|alltrees/quadrats),
        family ="poisson"(link ="log"),
        data = predcenter %>% 
          filter(Site =="ER"))
simulationOutput <- 
  simulateResiduals(fittedModel = nobromhuntspidsmodeler_treatment, 
                    n = 2000)
plot(simulationOutput, 
     asFactor = T,
     quantreg = F)
summary(nobromhuntspidsmodeler_treatment)
nobromhuntspidstester_treatment <- 
  mixed(I(huntspids-bromhuntspids)~ 
          Treatment*Sampling + 
          (1|alltrees/quadrats),
        family="poisson"(link ="log"),
        type = afex_options(type = "2"),
        data = predcenter %>% 
          filter(Site =="ER"),
        method = "LRT")$anova_table
##plot
###visreg
visreg(nobromhuntspidsmodeler_treatment,
       "Sampling", by = "Treatment")
###ggeffect
nobromhuntspidseffecter_treatment <- 
  ggeffect(nobromhuntspidsmodeler_treatment,
           type = "re",
           x.as.factor = T,
           terms = c("Sampling", "Treatment"),
           ci.lvl = 0.95)
nobromhuntspidseffecter_treatment$x <- 
  factor(nobromhuntspidseffecter_treatment$x, levels = c("B", "A"))
nobromhuntspidseffecter_treatment$group <- 
  factor(nobromhuntspidseffecter_treatment$group, levels = c("wo", "w", "wr"))
nobromhuntspidsploter_treatment <- 
  ggplot(nobromhuntspidseffecter_treatment, 
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
  ylab("Non bromeliad-associated hunting spider abundance") +
  scale_color_manual(name = "Treatment",
                     labels = c("Wihout", "With", "Removal"),
                     values = c("darkgreen", "saddlebrown", "ivory4")) +
  theme(legend.position = c(0.9,0.9),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))
##Herbivores
herbmodeler_treatment <- 
  glmer(herb ~
          Treatment*Sampling +
          (1|alltrees/quadrats),
        family ="poisson"(link ="log"),
        data = poolcenter %>% 
          filter(Site =="ER"))
simulationOutput <- 
  simulateResiduals(fittedModel = herbmodeler_treatment, 
                    n = 2000)
plot(simulationOutput, 
     asFactor = T,
     quantreg = F)
summary(herbmodeler_treatment)
herbtester_treatment <- 
  mixed(herb~ 
          Treatment*Sampling + 
          (1|alltrees/quadrats),
        family="poisson"(link ="log"),
        type = afex_options(type = "2"),
        data = poolcenter %>% 
          filter(Site =="ER"),
        method = "LRT")$anova_table
##plot
###visreg
visreg(herbmodeler_treatment,
       "Sampling", by = "Treatment")
###ggeffect
herbeffecter_treatment <- 
  ggeffect(herbmodeler_treatment,
           type = "re",
           x.as.factor = T,
           terms = c("Sampling", "Treatment"),
           ci.lvl = 0.95)
herbeffecter_treatment$x <- 
  factor(herbeffecter_treatment$x, levels = c("B", "A"))
herbeffecter_treatment$group <- 
  factor(herbeffecter_treatment$group, levels = c("wo", "w", "wr"))
herbploter_treatment <- 
  ggplot(herbeffecter_treatment, 
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
  scale_color_manual(name = "Treatment",
                     labels = c("Wihout", "With", "Removal"),
                     values = c("darkgreen", "saddlebrown", "ivory4")) +
  theme(legend.position = c(0.9,0.9),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))
#Leaf damage
leafpoolmodeler_treatment <- 
  glmer.nb(I(round((propdamage +0.01)*100))~ 
             Treatment*Sampling + 
             (1|alltrees/quadrats),
           control = glmerControl(optimizer = "bobyqa"),
           data = poolcenter %>% 
             filter(Site =="ER"))
simulationOutput <- 
  simulateResiduals(fittedModel = leafpoolmodeler_treatment, 
                    n = 2000)
plot(simulationOutput,
     asFactor = T,
     quantreg = F)
summary(leafpoolmodeler_treatment)
leafpooltester_treatment <- 
  mixed(I(round((propdamage +0.01)*100))~ 
          Treatment*Sampling + 
          (1|alltrees/quadrats),
        family ="negative.binomial"(theta = getME(leafpoolmodeler_treatment, "glmer.nb.theta")),
        type = afex_options(type = "2"),
        control = glmerControl(optimizer = "bobyqa"),
        data = poolcenter %>% 
          filter(Site =="ER"),
        method = "LRT")$anova_table
##plot
###visreg
visreg(leafpoolmodeler_treatment,
       "Treatment", by = "Sampling")
###ggeffect
leafpooleffecter_treatment <- 
  ggeffect(leafpoolmodeler_treatment,
           type = "re",
           x.as.factor = T,
           terms = c("Sampling", "Treatment"),
           ci.lvl = 0.95)
leafpooleffecter_treatment$x <- 
  factor(leafpooleffecter_treatment$x, levels = c("B", "A"))
leafpooleffecter_treatment$group <- 
  factor(leafpooleffecter_treatment$group, levels = c("wo", "w", "wr"))
leafpoolploterer_treatment <- 
  ggplot(leafpooleffecter_treatment, 
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
##plot
pdf("panel_treatmenter.pdf",
    height = 20,
    width = 20)
grid.arrange(grobs = list(predsploter_treatment + ggtitle("a") + theme(legend.position = "none",
                                                                       axis.title.x=element_blank(),
                                                                       axis.text.x=element_blank(),
                                                                       axis.ticks.x=element_blank()), 
                          brompredploter_treatment+ ggtitle("b")+ theme(axis.title.x=element_blank(),
                                                                                                      axis.text.x=element_blank(),
                                                                                                      axis.ticks.x=element_blank()),
                          bromantsploter_treatment+ ggtitle("c, only treatment significant")+ theme(legend.position = "none",
                                                                                                     axis.title.x=element_blank(),
                                                                                                     axis.text.x=element_blank(),
                                                                                                     axis.ticks.x=element_blank()), 
                          bromhuntspidsploter_treatment+ ggtitle("d")+ theme(legend.position = "none",
                                                                                              axis.title.x=element_blank(),
                                                                                              axis.text.x=element_blank(),
                                                                                              axis.ticks.x=element_blank()),
                          nobromantsploter_treatment+ ggtitle("e, interaction and season significant")+ theme(legend.position = "none",
                                                                                           axis.title.x=element_blank(),
                                                                                           axis.text.x=element_blank(),
                                                                                           axis.ticks.x=element_blank()), 
                          nobromhuntspidsploter_treatment+ ggtitle("f, only sampling significant")+ theme(legend.position = "none",
                                                                               axis.title.x=element_blank(),
                                                                               axis.text.x=element_blank(),
                                                                               axis.ticks.x=element_blank()),
                          herbploter_treatment + ggtitle("g, only sampling significant")+ theme(legend.position = "none"), 
                          leafpoolploter_treatment + ggtitle("h")+ theme(legend.position = "none")),
             ncol = 2)
dev.off()








#DO
##All predatores
predsmodeldo_treatment <- 
  glmer.nb(preds ~
          Treatment*Sampling +
          (1|alltrees/quadrats),
        family="poisson"(link ="sqrt"),
        control =glmerControl(optimizer ="bobyqa"),
        data = poolcenter %>% 
          filter(Site =="DO"))
simulationOutput <- 
  simulateResiduals(fittedModel = predsmodeldo_treatment, 
                    n = 2000)
plot(simulationOutput, 
     asFactor = T,
     quantreg = F)
summary(predsmodeldo_treatment)
predstestdo_treatment <- 
  mixed(preds~ 
          Treatment*Sampling + 
          (1|alltrees/quadrats),
        family="negative.binomial"(theta = getME(predsmodeldo_treatment,
                                   "glmer.nb.theta")),
        type = afex_options(type = "2"),
        data = poolcenter %>% 
          filter(Site=="DO"),
        method = "LRT")$anova_table
##plot
###visreg
visreg(predsmodeldo_treatment,
       "Sampling", by = "Treatment")
###ggeffect
predseffectdo_treatment <- 
  ggeffect(predsmodeldo_treatment,
           type = "re",
           x.as.factor = T,
           terms = c("Sampling", "Treatment"),
           ci.lvl = 0.95)
predseffectdo_treatment$x <- 
  factor(predseffectdo_treatment$x, levels = c("B", "A"))
predseffectdo_treatment$group <- 
  factor(predseffectdo_treatment$group, levels = c("wo", "w", "wr"))
predsplotdo_treatment <- 
  ggplot(predseffectdo_treatment, 
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
##Bromeliad-associated predators
brompredmodeldo_treatment <- 
  glmer(bromypred ~
          Treatment*Sampling +
          (1|alltrees/quadrats),
        family="poisson"(link ="sqrt"),
        data = poolcenter %>% 
          filter(Site =="DO"))
simulationOutput <- 
  simulateResiduals(fittedModel = brompredmodeldo_treatment, 
                    n = 2000)
plot(simulationOutput, 
     asFactor = T,
     quantreg = F)
summary(brompredmodeldo_treatment)
brompredtestdo_treatment <- 
  mixed(bromypred~ 
          Treatment*Sampling + 
          (1|alltrees/quadrats),
        family="poisson"(link ="sqrt"),
        type = afex_options(type = "2"),
        data = poolcenter %>% 
          filter(Site =="DO"),
        method = "LRT")$anova_table
##plot
###visreg
visreg(brompredmodeldo_treatment,
       "Sampling", by = "Treatment")
###ggeffect
brompredeffectdo_treatment <- 
  ggeffect(brompredmodeldo_treatment,
           type = "re",
           x.as.factor = T,
           terms = c("Sampling", "Treatment"),
           ci.lvl = 0.95)
brompredeffectdo_treatment$x <- 
  factor(brompredeffectdo_treatment$x, levels = c("B", "A"))
brompredeffectdo_treatment$group <- 
  factor(brompredeffectdo_treatment$group, levels = c("wo", "w", "wr"))
brompredplotdo_treatment <- 
  ggplot(brompredeffectdo_treatment, 
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
  scale_color_manual(name = "Treatment",
                     labels = c("Wihout", "With", "Removal"),
                     values = c("darkgreen", "saddlebrown", "ivory4")) +
  theme(legend.position = c(0.9,0.9),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))
##Bromeliad-associated ants
bromantsmodeldo_treatment <- 
  glmer(bromants ~
          Treatment*Sampling +
          (1|alltrees/quadrats),
        family="poisson"(link ="sqrt"),
        data = predcenter %>% 
          filter(Site =="DO"))
simulationOutput <- 
  simulateResiduals(fittedModel = bromantsmodeldo_treatment, 
                    n = 2000)
plot(simulationOutput, 
     asFactor = T,
     quantreg = F)
summary(bromantsmodeldo_treatment)
bromantstestdo_treatment <- 
  mixed(bromants~ 
          Treatment*Sampling + 
          (1|alltrees/quadrats),
        family="poisson"(link ="sqrt"),
        type = afex_options(type = "2"),
        data = predcenter %>% 
          filter(Site =="DO"),
        method = "LRT")$anova_table
##plot
###visreg
visreg(bromantsmodeldo_treatment,
       "Sampling", by = "Treatment")
###ggeffect
bromantseffectdo_treatment <- 
  ggeffect(bromantsmodeldo_treatment,
           type = "re",
           x.as.factor = T,
           terms = c("Sampling", "Treatment"),
           ci.lvl = 0.95)
bromantseffectdo_treatment$x <- 
  factor(bromantseffectdo_treatment$x, levels = c("B", "A"))
bromantseffectdo_treatment$group <- 
  factor(bromantseffectdo_treatment$group, levels = c("wo", "w", "wr"))
bromantsplotdo_treatment <- 
  ggplot(bromantseffectdo_treatment, 
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
##Bromeliad-associated hunting spiders
bromhuntspidsmodeldo_treatment <- 
  glmer(bromhuntspids ~
          Treatment*Sampling +
          (1|alltrees/quadrats),
        family ="poisson"(link ="log"),
        data = predcenter %>% 
          filter(Site =="DO"))
simulationOutput <- 
  simulateResiduals(fittedModel = bromhuntspidsmodeldo_treatment, 
                    n = 2000)
plot(simulationOutput, 
     asFactor = T,
     quantreg = F)
summary(bromhuntspidsmodeldo_treatment)
bromhuntspidstestdo_treatment <- 
  mixed(bromhuntspids~ 
          Treatment*Sampling + 
          (1|alltrees/quadrats),
        family="poisson"(link ="log"),
        type = afex_options(type = "2"),
        data = predcenter %>% 
          filter(Site =="DO"),
        method = "LRT")$anova_table
##plot
###visreg
visreg(bromhuntspidsmodeldo_treatment,
       "Sampling", by = "Treatment")
###ggeffect
bromhuntspidseffectdo_treatment <- 
  ggeffect(bromhuntspidsmodeldo_treatment,
           type = "re",
           x.as.factor = T,
           terms = c("Sampling", "Treatment"),
           ci.lvl = 0.95)
bromhuntspidseffectdo_treatment$x <- 
  factor(bromhuntspidseffectdo_treatment$x, levels = c("B", "A"))
bromhuntspidseffectdo_treatment$group <- 
  factor(bromhuntspidseffectdo_treatment$group, levels = c("wo", "w", "wr"))
bromhuntspidsplotdo_treatment <- 
  ggplot(bromhuntspidseffectdo_treatment, 
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
##Non-bromeliad-associated ants
nobromantsmodeldo_treatment <- 
  glmer(I(ants-bromants) ~
          Treatment*Sampling +
          (1|alltrees/quadrats),
        family ="poisson"(link ="sqrt"),
        data = predcenter %>% 
          filter(Site =="DO"))
simulationOutput <- 
  simulateResiduals(fittedModel = nobromantsmodeldo_treatment, 
                    n = 2000)
plot(simulationOutput, 
     asFactor = T,
     quantreg = F)
summary(nobromantsmodeldo_treatment)
nobromantstestdo_treatment <- 
  mixed(I(ants-bromants)~ 
          Treatment*Sampling + 
          (1|alltrees/quadrats),
        family="poisson"(link ="sqrt"),
        type = afex_options(type = "2"),
        data = predcenter %>% 
          filter(Site =="DO"),
        method = "LRT")$anova_table
##plot
###visreg
visreg(nobromantsmodeldo_treatment,
       "Sampling", by = "Treatment")
###ggeffect
nobromantseffectdo_treatment <- 
  ggeffect(nobromantsmodeldo_treatment,
           type = "re",
           x.as.factor = T,
           terms = c("Sampling", "Treatment"),
           ci.lvl = 0.95)
nobromantseffectdo_treatment$x <- 
  factor(nobromantseffectdo_treatment$x, levels = c("B", "A"))
nobromantseffectdo_treatment$group <- 
  factor(nobromantseffectdo_treatment$group, levels = c("wo", "w", "wr"))
nobromantsplotdo_treatment <- 
  ggplot(nobromantseffectdo_treatment, 
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
  ylab("Non bromeliad-associated ant abundance") +
  scale_color_manual(name = "Treatment",
                     labels = c("Wihout", "With", "Removal"),
                     values = c("darkgreen", "saddlebrown", "ivory4")) +
  theme(legend.position = c(0.9,0.9),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))
##Non-bromeliad-associated hunting spiders
nobromhuntspidsmodeldo_treatment <- 
  glmer(I(huntspids-bromhuntspids) ~
          Treatment*Sampling +
          (1|alltrees/quadrats),
        family ="poisson"(link ="log"),
        data = predcenter %>% 
          filter(Site =="DO"))
simulationOutput <- 
  simulateResiduals(fittedModel = nobromhuntspidsmodeldo_treatment, 
                    n = 2000)
plot(simulationOutput, 
     asFactor = T,
     quantreg = F)
summary(nobromhuntspidsmodeldo_treatment)
nobromhuntspidstestdo_treatment <- 
  mixed(I(huntspids-bromhuntspids)~ 
          Treatment*Sampling + 
          (1|alltrees/quadrats),
        family="poisson"(link ="log"),
        type = afex_options(type = "2"),
        data = predcenter %>% 
          filter(Site =="DO"),
        method = "LRT")$anova_table
##plot
###visreg
visreg(nobromhuntspidsmodeldo_treatment,
       "Sampling", by = "Treatment")
###ggeffect
nobromhuntspidseffectdo_treatment <- 
  ggeffect(nobromhuntspidsmodeldo_treatment,
           type = "re",
           x.as.factor = T,
           terms = c("Sampling", "Treatment"),
           ci.lvl = 0.95)
nobromhuntspidseffectdo_treatment$x <- 
  factor(nobromhuntspidseffectdo_treatment$x, levels = c("B", "A"))
nobromhuntspidseffectdo_treatment$group <- 
  factor(nobromhuntspidseffectdo_treatment$group, levels = c("wo", "w", "wr"))
nobromhuntspidsplotdo_treatment <- 
  ggplot(nobromhuntspidseffectdo_treatment, 
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
  ylab("Non bromeliad-associated hunting spider abundance") +
  scale_color_manual(name = "Treatment",
                     labels = c("Wihout", "With", "Removal"),
                     values = c("darkgreen", "saddlebrown", "ivory4")) +
  theme(legend.position = c(0.9,0.9),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))
##Herbivores
herbmodeldo_treatment <- 
  glmer.nb(herb ~
          Treatment*Sampling +
          (1|alltrees/quadrats),
        control=glmerControl(optimizer="bobyqa"),
        data = poolcenter %>% 
          filter(Site =="DO"))
simulationOutput <- 
  simulateResiduals(fittedModel = herbmodeldo_treatment, 
                    n = 2000)
plot(simulationOutput, 
     asFactor = T,
     quantreg = F)
summary(herbmodeldo_treatment)
herbtestdo_treatment <- 
  mixed(herb~ 
          Treatment*Sampling + 
          (1|alltrees/quadrats),
        family="negative.binomial"(theta=getME(herbmodeldo_treatment,
                                               "glmer.nb.theta")),
        control=glmerControl(optimizer="bobyqa"),
        type = afex_options(type = "2"),
        data = poolcenter %>% 
          filter(Site =="DO"),
        method = "LRT")$anova_table
##plot
###visreg
visreg(herbmodeldo_treatment,
       "Sampling", by = "Treatment")
###ggeffect
herbeffectdo_treatment <- 
  ggeffect(herbmodeldo_treatment,
           type = "re",
           x.as.factor = T,
           terms = c("Sampling", "Treatment"),
           ci.lvl = 0.95)
herbeffectdo_treatment$x <- 
  factor(herbeffectdo_treatment$x, levels = c("B", "A"))
herbeffectdo_treatment$group <- 
  factor(herbeffectdo_treatment$group, levels = c("wo", "w", "wr"))
herbplotdo_treatment <- 
  ggplot(herbeffectdo_treatment, 
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
  scale_color_manual(name = "Treatment",
                     labels = c("Wihout", "With", "Removal"),
                     values = c("darkgreen", "saddlebrown", "ivory4")) +
  theme(legend.position = c(0.9,0.9),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))
#Leaf damage
leafpoolmodeldo_treatment <- 
  glmer.nb(I(round((propdamage +0.01)*100))~ 
             Treatment*Sampling + 
             (1|alltrees/quadrats),
           control = glmerControl(optimizer = "bobyqa"),
           data = poolcenter %>% 
             filter(Site =="DO"))
simulationOutput <- 
  simulateResiduals(fittedModel = leafpoolmodeldo_treatment, 
                    n = 2000)
plot(simulationOutput,
     asFactor = T,
     quantreg = F)
summary(leafpoolmodeldo_treatment)
leafpooltestdodo_treatment <- 
  mixed(I(round((propdamage +0.01)*100))~ 
          Treatment*Sampling + 
          (1|alltrees/quadrats),
        family ="negative.binomial"(theta = getME(leafpoolmodeldo_treatment, "glmer.nb.theta")),
        type = afex_options(type = "2"),
        control = glmerControl(optimizer = "bobyqa"),
        data = poolcenter %>% 
          filter(Site =="DO"),
        method = "LRT")$anova_table
##plot
###visreg
visreg(leafpoolmodeldo_treatment,
       "Treatment", by = "Sampling")
###ggeffect
leafpooleffectdo_treatment <- 
  ggeffect(leafpoolmodeldo_treatment,
           type = "re",
           x.as.factor = T,
           terms = c("Sampling", "Treatment"),
           ci.lvl = 0.95)
leafpooleffect_treatment$x <- 
  factor(leafpooleffect_treatment$x, levels = c("B", "A"))
leafpooleffect_treatment$group <- 
  factor(leafpooleffect_treatment$group, levels = c("wo", "w", "wr"))
leafpoolplotdo_treatment <- 
  ggplot(leafpooleffectdo_treatment, 
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
##plot
pdf("panel_treatmentdo.pdf",
    height = 20,
    width = 20)
grid.arrange(grobs = list(predsplotdo_treatment + ggtitle("a") + theme(legend.position = "none",
                                                                       axis.title.x=element_blank(),
                                                                       axis.text.x=element_blank(),
                                                                       axis.ticks.x=element_blank()), 
                          brompredplotdo_treatment+ ggtitle("b, only interaction significant")+ theme(axis.title.x=element_blank(),
                                                                                                      axis.text.x=element_blank(),
                                                                                                      axis.ticks.x=element_blank()),
                          bromantsplotdo_treatment+ ggtitle("c, only season signifcant")+ theme(legend.position = "none",
                                                                                                     axis.title.x=element_blank(),
                                                                                                     axis.text.x=element_blank(),
                                                                                                     axis.ticks.x=element_blank()), 
                          bromhuntspidsplotdo_treatment+ ggtitle("d, nothing significant")+ theme(legend.position = "none",
                                                                                              axis.title.x=element_blank(),
                                                                                              axis.text.x=element_blank(),
                                                                                              axis.ticks.x=element_blank()),
                          nobromantsplotdo_treatment+ ggtitle("e, only interaction significant")+ theme(legend.position = "none",
                                                                                           axis.title.x=element_blank(),
                                                                                           axis.text.x=element_blank(),
                                                                                           axis.ticks.x=element_blank()), 
                          nobromhuntspidsplotdo_treatment+ ggtitle("f, only season significant")+ theme(legend.position = "none",
                                                                               axis.title.x=element_blank(),
                                                                               axis.text.x=element_blank(),
                                                                               axis.ticks.x=element_blank()),
                          herbplotdo_treatment + ggtitle("g, nothing significant")+ theme(legend.position = "none"), 
                          leafpoolplotdo_treatment + ggtitle("h, nothing significant")+ theme(legend.position = "none")),
             ncol = 2)
dev.off()

# Volume index plot grid --------------------------------------------------

pdf("panel_largeleaf.pdf",
    height = 15,
    width = 5)
grid.arrange(grobs = list(predsplot_largeleaf + 
                            ggtitle("a") +
                            theme(axis.title.x=element_blank(),
                                  axis.text.x=element_blank(),
                                  axis.ticks.x=element_blank()),
                          herbplot_largeleaf +
                            ggtitle("b") +
                            theme(legend.position = "none",
                                  axis.title.x=element_blank(),
                                  axis.text.x=element_blank(),
                                  axis.ticks.x=element_blank()),
                          herbeetleplot_largeleaf + 
                            ggtitle("c") +
                            theme(legend.position = "none",
                                  axis.title.x=element_blank(),
                                  axis.text.x=element_blank(),
                                  axis.ticks.x=element_blank()),
                          psyllidplot_largeleaf + 
                            ggtitle("d") +
                            theme(legend.position = "none")),
             ncol =1)
dev.off()

# Volume index plot grid per site--------------------------------------------------
#CP
##All Predators
predsmodelcp_largeleaf <- 
  glmer(preds ~
          largeleaf*Sampling +
          (1|alltrees/quadrats),
        family="poisson"(link="log"),
        data = poolcenter %>% 
          filter(Site =="CP"))
simulationOutput <- 
  simulateResiduals(fittedModel = predsmodelcp_largeleaf, 
                    n = 2000, 
                    rank = T)
plot(simulationOutput,
     quantreg = F,
     rank = T)
summary(predsmodelcp_largeleaf)
predstestcp_largeleaf <- 
  mixed(preds~ 
          largeleaf*Sampling + 
          (1|alltrees/quadrats),
        family ="poisson"(link="log"),
        type = afex_options(type = "2"),
        data = poolcenter %>% 
          filter(Site =="CP"),
        method = "LRT")$anova_table
##plot
###visreg
visreg(predsmodelcp_largeleaf,
       "largeleaf", by = "Sampling")
##ggeffect
predseffectcp_largeleaf <- 
  ggeffect(predsmodelcp_largeleaf,
           terms = c("largeleaf", "Sampling"),
           swap.pred = T,
           type = "re",
           ci.level = 0.95)
predseffectcp_largeleaf$group <- 
  factor(predseffectcp_largeleaf$group, 
         levels = c("B", "A"))
col <- 
  ifelse((poolcenter %>% 
            filter(Site =="CP"))$Sampling == "B",
         "darkorange2", 
         "dodgerblue4")
predseffectcp_largeleaf$conf.low <- 
  predseffectcp_largeleaf$conf.low +1
predseffectcp_largeleaf$conf.high <- 
  predseffectcp_largeleaf$conf.high +1
predseffectcp_largeleaf$predicted <- 
  predseffectcp_largeleaf$predicted +1
predsplotcp_largeleaf <- 
  plot(predseffectcp_largeleaf,
       ci = T) + 
  geom_point(data = poolcenter %>% 
               filter(Site=="CP"),
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
##Herbivores
herbmodelcp_largeleaf <- 
  glmer(herb ~
          largeleaf*Sampling +
          (1|alltrees/quadrats),
        family="poisson"(link="sqrt"),
        data = poolcenter %>% 
          filter(Site =="CP"))
simulationOutput <- 
  simulateResiduals(fittedModel = herbmodelcp_largeleaf, 
                    n = 2000, 
                    rank = T)
plot(simulationOutput,
     quantreg = F,
     rank = T)
summary(predsmodelcp_largeleaf)
herbtestcp_largeleaf <- 
  mixed(herb~ 
          largeleaf*Sampling + 
          (1|alltrees/quadrats),
        family ="poisson"(link="sqrt"),
        type = afex_options(type = "2"),
        data = poolcenter %>% 
          filter(Site =="CP"),
        method = "LRT")$anova_table
##plot
###visreg
visreg(herbmodelcp_largeleaf,
       "largeleaf", by = "Sampling")
###ggeffect
herbeffectcp_largeleaf <- 
  ggeffect(herbmodelcp_largeleaf,
           terms = c("largeleaf", "Sampling"),
           swap.pred = T,
           type = "re",
           ci.level = 0.95)
herbeffectcp_largeleaf$group <- 
  factor(herbeffectcp_largeleaf$group, 
         levels = c("B", "A"))
col <- 
  ifelse((poolcenter %>% 
            filter(Site =="CP"))$Sampling == "B",
         "darkorange2", 
         "dodgerblue4")
herbeffectcp_largeleaf$conf.low <- 
  herbeffectcp_largeleaf$conf.low +1
herbeffectcp_largeleaf$conf.high <- 
  herbeffectcp_largeleaf$conf.high +1
herbeffectcp_largeleaf$predicted <- 
  herbeffectcp_largeleaf$predicted +1
herbplotcp_largeleaf <- 
  plot(herbeffectcp_largeleaf,
       ci = T) + 
  geom_point(data = poolcenter %>% 
               filter(Site=="CP"),
             mapping = aes(x = largeleaf, y = jitter(herb +1, 2)), 
             colour = col, 
             fill = col) +
  ggtitle("") + 
  xlab("Volume proximity index") +
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
##Herbivorous beetles
herbeetlemodelcp_largeleaf <- 
  glmer(herbeetle ~
          largeleaf*Sampling +
          (1|alltrees/quadrats),
        family="poisson"(link="log"),
        data = poolcenter %>% 
          filter(Site =="CP"))
simulationOutput <- 
  simulateResiduals(fittedModel = herbeetlemodelcp_largeleaf, 
                    n = 2000, 
                    rank = T)
plot(simulationOutput,
     quantreg = F,
     rank = T)
summary(predsmodelcp_largeleaf)
herbeetletestcp_largeleaf <- 
  mixed(herbeetle~ 
          largeleaf*Sampling + 
          (1|alltrees/quadrats),
        family ="poisson"(link="log"),
        type = afex_options(type = "2"),
        data = poolcenter %>% 
          filter(Site =="CP"),
        method = "LRT")$anova_table
##plot
###visreg
visreg(herbeetlemodelcp_largeleaf,
       "largeleaf", by = "Sampling")
###ggeffect
herbeetleeffectcp_largeleaf <- 
  ggeffect(herbeetlemodelcp_largeleaf,
           terms = c("largeleaf", "Sampling"),
           swap.pred = T,
           type = "re",
           ci.level = 0.95)
herbeetleeffectcp_largeleaf$group <- 
  factor(herbeetleeffectcp_largeleaf$group, 
         levels = c("B", "A"))
col <- 
  ifelse((poolcenter %>% 
            filter(Site =="CP"))$Sampling == "B",
         "darkorange2", 
         "dodgerblue4")
herbeetleeffectcp_largeleaf$conf.low <- 
  herbeetleeffectcp_largeleaf$conf.low +1
herbeetleeffectcp_largeleaf$conf.high <- 
  herbeetleeffectcp_largeleaf$conf.high +1
herbeetleeffectcp_largeleaf$predicted <- 
  herbeetleeffectcp_largeleaf$predicted +1
herbeetleplotcp_largeleaf <- 
  plot(herbeetleeffectcp_largeleaf,
       ci = T) + 
  geom_point(data = poolcenter %>% 
               filter(Site=="CP"),
             mapping = aes(x = largeleaf, y = jitter(herbeetle +1, 2)), 
             colour = col, 
             fill = col) +
  ggtitle("") + 
  xlab("Volume proximity index") +
  ylab("Herbivorous beetle abundance") +
  scale_y_continuous(trans = "log",
                     breaks = c(1,7,40)) +
  scale_color_manual(labels = c("Before", "After"), 
                     values = c("darkorange2", "dodgerblue4")) +
  theme(legend.position = c(0.9 ,0.9),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))
##Psyllids
psyllidmodelcp_largeleaf <- 
  glmer(psyllid ~
          largeleaf*Sampling +
          (1|alltrees/quadrats),
        family="poisson"(link="sqrt"),
        data = poolcenter %>% 
          filter(Site =="CP"))
simulationOutput <- 
  simulateResiduals(fittedModel = psyllidmodelcp_largeleaf, 
                    n = 2000, 
                    rank = T)
plot(simulationOutput,
     quantreg = F,
     rank = T)
summary(predsmodelcp_largeleaf)
psyllidtestcp_largeleaf <- 
  mixed(psyllid~ 
          largeleaf*Sampling + 
          (1|alltrees/quadrats),
        family ="poisson"(link="sqrt"),
        type = afex_options(type = "2"),
        data = poolcenter %>% 
          filter(Site =="CP"),
        method = "LRT")$anova_table
##plot
###visreg
visreg(psyllidmodelcp_largeleaf,
       "largeleaf", by = "Sampling")
###ggeffect
psyllideffectcp_largeleaf <- 
  ggeffect(psyllidmodelcp_largeleaf,
           terms = c("largeleaf", "Sampling"),
           swap.pred = T,
           type = "re",
           ci.level = 0.95)
psyllideffectcp_largeleaf$group <- 
  factor(psyllideffectcp_largeleaf$group, 
         levels = c("B", "A"))
col <- 
  ifelse((poolcenter %>% 
            filter(Site =="CP"))$Sampling == "B",
         "darkorange2", 
         "dodgerblue4")
psyllideffectcp_largeleaf$conf.low <- 
  psyllideffectcp_largeleaf$conf.low +1
psyllideffectcp_largeleaf$conf.high <- 
  psyllideffectcp_largeleaf$conf.high +1
psyllideffectcp_largeleaf$predicted <- 
  psyllideffectcp_largeleaf$predicted +1
psyllidplotcp_largeleaf <- 
  plot(psyllideffectcp_largeleaf,
       ci = T) + 
  geom_point(data = poolcenter %>% 
               filter(Site=="CP"),
             mapping = aes(x = largeleaf, 
                           y = jitter(psyllid +1, 2)), 
             colour = col, 
             fill = col) +
  ggtitle("") + 
  xlab("Volume proximity index") +
  ylab("Psyllid abundance") +
  scale_y_continuous(trans = "log",
                     breaks = c(1,7,40)) +
  scale_color_manual(labels = c("Before", "After"), 
                     values = c("darkorange2", "dodgerblue4")) +
  theme(legend.position = c(0.9 ,0.9),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))
##plot
pdf("panel_largeleafcp.pdf",
    height = 20,
    width = 20)
grid.arrange(grobs = list(predsplotcp_largeleaf + 
                            ggtitle("a, not significant") +
                            theme(legend.position = c(0.9,0.9),
                                  axis.title.x=element_blank(),
                                  axis.text.x=element_blank(),
                                  axis.ticks.x=element_blank()),
                          herbplotcp_largeleaf +
                            ggtitle("b") +
                            theme(legend.position = "none",
                                  axis.title.x=element_blank(),
                                  axis.text.x=element_blank(),
                                  axis.ticks.x=element_blank()),
                          herbeetleplotcp_largeleaf + 
                            ggtitle("c") +
                            theme(legend.position = "none",
                                  axis.title.x=element_blank(),
                                  axis.text.x=element_blank(),
                                  axis.ticks.x=element_blank()),
                          psyllidplotcp_largeleaf + 
                            ggtitle("d") +
                            theme(legend.position = "none")),
             ncol =1)
dev.off()

#ER
##All Predators
predsmodeler_largeleaf <- 
  glmer(preds ~
          largeleaf*Sampling +
          (1|alltrees/quadrats),
        family="poisson"(link="sqrt"),
        data = poolcenter %>% 
          filter(Site =="ER"))
simulationOutput <- 
  simulateResiduals(fittedModel = predsmodeler_largeleaf, 
                    n = 2000, 
                    rank = T)
plot(simulationOutput,
     quantreg = F,
     rank = T)
summary(predsmodeler_largeleaf)
predstester_largeleaf <- 
  mixed(preds~ 
          largeleaf*Sampling + 
          (1|alltrees/quadrats),
        family ="poisson"(link="sqrt"),
        type = afex_options(type = "2"),
        data = poolcenter %>% 
          filter(Site =="ER"),
        method = "LRT")$anova_table
##plot
###visreg
visreg(predsmodeler_largeleaf,
       "largeleaf", by = "Sampling")
###ggeffect
predseffecter_largeleaf <- 
  ggeffect(predsmodeler_largeleaf,
           terms = c("largeleaf", "Sampling"),
           swap.pred = T,
           type = "re",
           ci.level = 0.95)
predseffecter_largeleaf$group <- 
  factor(predseffecter_largeleaf$group, 
         levels = c("B", "A"))
col <- 
  ifelse((poolcenter %>% 
            filter(Site =="ER"))$Sampling == "B",
         "darkorange2", 
         "dodgerblue4")
predseffecter_largeleaf$conf.low <- 
  predseffecter_largeleaf$conf.low +1
predseffecter_largeleaf$conf.high <- 
  predseffecter_largeleaf$conf.high +1
predseffecter_largeleaf$predicted <- 
  predseffecter_largeleaf$predicted +1
predsploter_largeleaf <- 
  plot(predseffecter_largeleaf,
       ci = T) + 
  geom_point(data = poolcenter %>% 
               filter(Site=="ER"),
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

##Herbivores 
herbmodeler_largeleaf <- 
  glmer(herb ~
          largeleaf*Sampling +
          (1|alltrees/quadrats),
        family="poisson"(link="log"),
        data = poolcenter %>% 
          filter(Site =="ER"))
simulationOutput <- 
  simulateResiduals(fittedModel = herbmodeler_largeleaf, 
                    n = 2000, 
                    rank = T)
plot(simulationOutput,
     quantreg = F,
     rank = T)
summary(predsmodeler_largeleaf)
herbtester_largeleaf <- 
  mixed(herb~ 
          largeleaf*Sampling + 
          (1|alltrees/quadrats),
        family ="poisson"(link="log"),
        type = afex_options(type = "2"),
        data = poolcenter %>% 
          filter(Site =="ER"),
        method = "LRT")$anova_table
##plot
###visreg
visreg(herbmodeler_largeleaf,
       "largeleaf", by = "Sampling")
###ggeffect
herbeffecter_largeleaf <- 
  ggeffect(herbmodeler_largeleaf,
           terms = c("largeleaf", "Sampling"),
           swap.pred = T,
           type = "re",
           ci.level = 0.95)
herbeffecter_largeleaf$group <- 
  factor(herbeffecter_largeleaf$group, 
         levels = c("B", "A"))
col <- 
  ifelse((poolcenter %>% 
            filter(Site =="ER"))$Sampling == "B",
         "darkorange2", 
         "dodgerblue4")
herbeffecter_largeleaf$conf.low <- 
  herbeffecter_largeleaf$conf.low +1
herbeffecter_largeleaf$conf.high <- 
  herbeffecter_largeleaf$conf.high +1
herbeffecter_largeleaf$predicted <- 
  herbeffecter_largeleaf$predicted +1
herbploter_largeleaf <- 
  plot(herbeffecter_largeleaf,
       ci = T) + 
  geom_point(data = poolcenter %>% 
               filter(Site=="ER"),
             mapping = aes(x = largeleaf, y = jitter(herb +1, 2)), 
             colour = col, 
             fill = col) +
  ggtitle("") + 
  xlab("Volume proximity index") +
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
##Herbivorous beetles
herbeetlemodeler_largeleaf <- 
  glmer(herbeetle ~
          largeleaf*Sampling +
          (1|alltrees/quadrats),
        family="poisson"(link="sqrt"),
        data = poolcenter %>% 
          filter(Site =="ER"))
simulationOutput <- 
  simulateResiduals(fittedModel = herbeetlemodeler_largeleaf, 
                    n = 2000, 
                    rank = T)
plot(simulationOutput,
     quantreg = F,
     rank = T)
summary(predsmodeler_largeleaf)
herbeetletester_largeleaf <- 
  mixed(herbeetle~ 
          largeleaf*Sampling + 
          (1|alltrees/quadrats),
        family ="poisson"(link="sqrt"),
        type = afex_options(type = "2"),
        data = poolcenter %>% 
          filter(Site =="ER"),
        method = "LRT")$anova_table
##plot
###visreg
visreg(herbeetlemodeler_largeleaf,
       "largeleaf", by = "Sampling")
###ggeffect
herbeetleeffecter_largeleaf <- 
  ggeffect(herbeetlemodeler_largeleaf,
           terms = c("largeleaf", "Sampling"),
           swap.pred = T,
           type = "re",
           ci.level = 0.95)
herbeetleeffecter_largeleaf$group <- 
  factor(herbeetleeffecter_largeleaf$group, 
         levels = c("B", "A"))
col <- 
  ifelse((poolcenter %>% 
            filter(Site =="ER"))$Sampling == "B",
         "darkorange2", 
         "dodgerblue4")
herbeetleeffecter_largeleaf$conf.low <- 
  herbeetleeffecter_largeleaf$conf.low +1
herbeetleeffecter_largeleaf$conf.high <- 
  herbeetleeffecter_largeleaf$conf.high +1
herbeetleeffecter_largeleaf$predicted <- 
  herbeetleeffecter_largeleaf$predicted +1
herbeetleploter_largeleaf <- 
  plot(herbeetleeffecter_largeleaf,
       ci = T) + 
  geom_point(data = poolcenter %>% 
               filter(Site=="ER"),
             mapping = aes(x = largeleaf, y = jitter(herbeetle +1, 2)), 
             colour = col, 
             fill = col) +
  ggtitle("") + 
  xlab("Volume proximity index") +
  ylab("Herbivorous beetle abundance") +
  scale_y_continuous(trans = "log",
                     breaks = c(1,7,40)) +
  scale_color_manual(labels = c("Before", "After"), 
                     values = c("darkorange2", "dodgerblue4")) +
  theme(legend.position = c(0.9 ,0.9),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))
#Psyllids not applicable because ony one specimen found
##plot
pdf("panel_largeleafer.pdf",
    height = 20,
    width = 20)
grid.arrange(grobs = list(predsploter_largeleaf + 
                            ggtitle("a, only interaction significant") +
                            theme(axis.title.x=element_blank(),
                                  axis.text.x=element_blank(),
                                  axis.ticks.x=element_blank()),
                          herbploter_largeleaf +
                            ggtitle("b, not significant") +
                            theme(legend.position = c(0.9,0.9),
                                  axis.title.x=element_blank(),
                                  axis.text.x=element_blank(),
                                  axis.ticks.x=element_blank()),
                          herbeetleploter_largeleaf + 
                            ggtitle("c, not significant") +
                            theme(legend.position = "none"),
                          ggplot(NULL) + 
                            ggtitle("d, only one psyllid found, not applicable") +
                            theme(legend.position = "none")),
             ncol =1)
dev.off()


#DO
##All Predators
predsmodeldo_largeleaf <- 
  glmer(preds ~
          largeleaf*Sampling +
          (1|alltrees/quadrats),
        family="poisson"(link="log"),
        data = poolcenter %>% 
          filter(Site =="DO"))
simulationOutput <- 
  simulateResiduals(fittedModel = predsmodeldo_largeleaf, 
                    n = 2000, 
                    rank = T)
plot(simulationOutput,
     quantreg = F,
     rank = T)
summary(predsmodeldo_largeleaf)
predstestdo_largeleaf <- 
  mixed(preds~ 
          largeleaf*Sampling + 
          (1|alltrees/quadrats),
        family ="poisson"(link="log"),
        type = afex_options(type = "2"),
        data = poolcenter %>% 
          filter(Site =="DO"),
        method = "LRT")$anova_table
##plot
###visreg
visreg(predsmodeldo_largeleaf,
       "largeleaf", by = "Sampling")
###ggeffect
predseffectdo_largeleaf <- 
  ggeffect(predsmodeldo_largeleaf,
           terms = c("largeleaf", "Sampling"),
           swap.pred = T,
           type = "re",
           ci.level = 0.95)
predseffectdo_largeleaf$group <- 
  factor(predseffectdo_largeleaf$group, 
         levels = c("B", "A"))
col <- 
  ifelse((poolcenter %>% 
            filter(Site =="DO"))$Sampling == "B",
         "darkorange2", 
         "dodgerblue4")
predseffectdo_largeleaf$conf.low <- 
  predseffectdo_largeleaf$conf.low +1
predseffectdo_largeleaf$conf.high <- 
  predseffectdo_largeleaf$conf.high +1
predseffectdo_largeleaf$predicted <- 
  predseffectdo_largeleaf$predicted +1
predsplotdo_largeleaf <- 
  plot(predseffectdo_largeleaf,
       ci = T) + 
  geom_point(data = poolcenter %>% 
               filter(Site=="DO"),
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

##Herbivores 
herbmodeldo_largeleaf <- 
  glmer(herb ~
          largeleaf*Sampling +
          (1|alltrees/quadrats),
        family="poisson"(link="sqrt"),
        data = poolcenter %>% 
          filter(Site =="DO"))
simulationOutput <- 
  simulateResiduals(fittedModel = herbmodeldo_largeleaf, 
                    n = 2000, 
                    rank = T)
plot(simulationOutput,
     quantreg = F,
     rank = T)
summary(herbmodeldo_largeleaf)
herbtestdo_largeleaf <- 
  mixed(herb~ 
          largeleaf*Sampling + 
          (1|alltrees/quadrats),
        family ="poisson"(link="sqrt"),
        type = afex_options(type = "2"),
        data = poolcenter %>% 
          filter(Site =="DO"),
        method = "LRT")$anova_table
##plot
###visreg
visreg(herbmodeldo_largeleaf,
       "largeleaf", by = "Sampling")
###ggeffect
herbeffectdo_largeleaf <- 
  ggeffect(herbmodeldo_largeleaf,
           terms = c("largeleaf", "Sampling"),
           swap.pred = T,
           type = "re",
           ci.level = 0.95)
herbeffectdo_largeleaf$group <- 
  factor(herbeffectdo_largeleaf$group, 
         levels = c("B", "A"))
col <- 
  ifelse((poolcenter %>% 
            filter(Site =="DO"))$Sampling == "B",
         "darkorange2", 
         "dodgerblue4")
herbeffectdo_largeleaf$conf.low <- 
  herbeffectdo_largeleaf$conf.low +1
herbeffectdo_largeleaf$conf.high <- 
  herbeffectdo_largeleaf$conf.high +1
herbeffectdo_largeleaf$predicted <- 
  herbeffectdo_largeleaf$predicted +1
herbplotdo_largeleaf <- 
  plot(herbeffectdo_largeleaf,
       ci = T) + 
  geom_point(data = poolcenter %>% 
               filter(Site=="DO"),
             mapping = aes(x = largeleaf, y = jitter(herb +1, 2)), 
             colour = col, 
             fill = col) +
  ggtitle("") + 
  xlab("Volume proximity index") +
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
##Herbivorous beetles
herbeetlemodeldo_largeleaf <- 
  glmer(herbeetle ~
          largeleaf*Sampling +
          (1|alltrees/quadrats),
        family="poisson"(link="sqrt"),
        data = poolcenter %>% 
          filter(Site =="DO"))
simulationOutput <- 
  simulateResiduals(fittedModel = herbeetlemodeldo_largeleaf, 
                    n = 2000, 
                    rank = T)
plot(simulationOutput,
     quantreg = F,
     rank = T)
summary(predsmodeldo_largeleaf)
herbeetletestdo_largeleaf <- 
  mixed(herbeetle~ 
          largeleaf*Sampling + 
          (1|alltrees/quadrats),
        family ="poisson"(link="sqrt"),
        type = afex_options(type = "2"),
        data = poolcenter %>% 
          filter(Site =="DO"),
        method = "LRT")$anova_table
##plot
###visreg
visreg(herbeetlemodeldo_largeleaf,
       "largeleaf", by = "Sampling")
###ggeffect
herbeetleeffectdo_largeleaf <- 
  ggeffect(herbeetlemodeldo_largeleaf,
           terms = c("largeleaf", "Sampling"),
           swap.pred = T,
           type = "re",
           ci.level = 0.95)
herbeetleeffectdo_largeleaf$group <- 
  factor(herbeetleeffectdo_largeleaf$group, 
         levels = c("B", "A"))
col <- 
  ifelse((poolcenter %>% 
            filter(Site =="DO"))$Sampling == "B",
         "darkorange2", 
         "dodgerblue4")
herbeetleeffectdo_largeleaf$conf.low <- 
  herbeetleeffectdo_largeleaf$conf.low +1
herbeetleeffectdo_largeleaf$conf.high <- 
  herbeetleeffectdo_largeleaf$conf.high +1
herbeetleeffectdo_largeleaf$predicted <- 
  herbeetleeffectdo_largeleaf$predicted +1
herbeetleplotdo_largeleaf <- 
  plot(herbeetleeffectdo_largeleaf,
       ci = T) + 
  geom_point(data = poolcenter %>% 
               filter(Site=="DO"),
             mapping = aes(x = largeleaf, y = jitter(herbeetle +1, 2)), 
             colour = col, 
             fill = col) +
  ggtitle("") + 
  xlab("Volume proximity index") +
  ylab("Herbivorous beetle abundance") +
  scale_y_continuous(trans = "log",
                     breaks = c(1,7,40)) +
  scale_color_manual(labels = c("Before", "After"), 
                     values = c("darkorange2", "dodgerblue4")) +
  theme(legend.position = c(0.9 ,0.9),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))
##Psyllids
psyllidmodeldo_largeleaf <- 
  glmer(psyllid ~
             largeleaf*Sampling +
             (1|alltrees/quadrats),
           family="poisson"(link="log"),
           data = poolcenter %>% 
             filter(Site =="DO"))
simulationOutput <- 
  simulateResiduals(fittedModel = psyllidmodeldo_largeleaf, 
                    n = 2000, 
                    rank = T)
plot(simulationOutput,
     quantreg = F,
     rank = T)
summary(predsmodeldo_largeleaf)
psyllidtestdo_largeleaf <- 
  mixed(psyllid~ 
          largeleaf*Sampling + 
          (1|alltrees/quadrats),
        family ="poisson"(link="log"),
        type = afex_options(type = "2"),
        data = poolcenter %>% 
          filter(Site =="DO"),
        method = "LRT")$anova_table
##plot
###visreg
visreg(psyllidmodeldo_largeleaf,
       "largeleaf", by = "Sampling")
###ggeffect
psyllideffectdo_largeleaf <- 
  ggeffect(psyllidmodeldo_largeleaf,
           terms = c("largeleaf", "Sampling"),
           swap.pred = T,
           type = "re",
           ci.level = 0.95)
psyllideffectdo_largeleaf$group <- 
  factor(psyllideffectdo_largeleaf$group, 
         levels = c("B", "A"))
col <- 
  ifelse((poolcenter %>% 
            filter(Site =="DO"))$Sampling == "B",
         "darkorange2", 
         "dodgerblue4")
psyllideffectdo_largeleaf$conf.low <- 
  psyllideffectdo_largeleaf$conf.low +1
psyllideffectdo_largeleaf$conf.high <- 
  psyllideffectdo_largeleaf$conf.high +1
psyllideffectdo_largeleaf$predicted <- 
  psyllideffectdo_largeleaf$predicted +1
psyllidplotdo_largeleaf <- 
  plot(psyllideffectdo_largeleaf,
       ci = T) + 
  geom_point(data = poolcenter %>% 
               filter(Site=="DO"),
             mapping = aes(x = largeleaf, 
                           y = jitter(psyllid +1, 2)), 
             colour = col, 
             fill = col) +
  ggtitle("") + 
  xlab("Volume proximity index") +
  ylab("Psyllid abundance") +
  scale_y_continuous(trans = "log",
                     breaks = c(1,7,40)) +
  scale_color_manual(labels = c("Before", "After"), 
                     values = c("darkorange2", "dodgerblue4")) +
  theme(legend.position = c(0.9 ,0.9),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))
##plot
pdf("panel_largeleafdo.pdf",
    height = 20,
    width = 20)
grid.arrange(grobs = list(predsplotdo_largeleaf + 
                            ggtitle("a, only interaction significant") +
                            theme(axis.title.x=element_blank(),
                                  axis.text.x=element_blank(),
                                  axis.ticks.x=element_blank()),
                          herbplotdo_largeleaf +
                            ggtitle("b, not significant, no seasonal effect") +
                            theme(legend.position = c(0.9,0.9),
                                  axis.title.x=element_blank(),
                                  axis.text.x=element_blank(),
                                  axis.ticks.x=element_blank()),
                          herbeetleplotdo_largeleaf + 
                            ggtitle("c, not significant") +
                            theme(legend.position = "none",
                                  axis.title.x=element_blank(),
                                  axis.text.x=element_blank(),
                                  axis.ticks.x=element_blank()),
                          psyllidplotdo_largeleaf + 
                            ggtitle("d, not significant") +
                            theme(legend.position = "none")),
             ncol =1)
dev.off()

# Intraguild grid ---------------------------------------------------------
pdf("intraplot_brompreds.pdf",
    height = 15,
    width = 5)
grid.arrange(grobs = list(intraplot_brompara + ggtitle("i") + theme(axis.title.x=element_blank(),
                                                                    axis.text.x=element_blank(),
                                                                    axis.ticks.x=element_blank(),
                                                                    axis.title=element_text(size=rel(1.5))),
                          intraplot_brommobipred + ggtitle("ii") + theme(legend.position = "none",
                                                                        axis.title.x=element_blank(),
                                                                        axis.text.x=element_blank(),
                                                                        axis.ticks.x=element_blank(),
                                                                        axis.title=element_text(size=rel(1.5))),
                          intraplot_bromarbopred + ggtitle("iii") + theme(legend.position = "none",
                                                                          axis.title=element_text(size=rel(1.5)))),
                          ncol=1)
dev.off()
                                                                        
pdf("intraplot_para.pdf",
    height = 10,
    width = 5)
grid.arrange(grobs = list(intraplot_paraarbopred + ggtitle("i") + theme(axis.title.x=element_blank(),
                                                                        axis.text.x=element_blank(),
                                                                        axis.ticks.x=element_blank(),
                                                                        axis.title=element_text(size=rel(1.5))),
                          intraplot_paramobipred + ggtitle("ii") + theme(legend.position = "none",
                                                                         axis.title=element_text(size=rel(1.5)))),
             ncol=1)                       

dev.off()


pdf("intraplot_aerial.pdf",
    height = 5,
    width = 5)
intraplot_mobiarbopred + theme(axis.title=element_text(size=rel(1.5)))
dev.off()


# Predators on herbivores grid --------------------------------------------
pdf("predherbplot_preds.pdf",
    height = 15,
    width = 10)
grid.arrange(grobs = list(predherbplot_pred + 
                            theme(legend.position = "none",
                                  axis.title=element_text(size=rel(1.5))) +
                            ggtitle("a"),
                          predherbplot_brompreds +
                            ggtitle("b") +
                            theme(axis.title=element_text(size=rel(1.5))) +
                            ylab(NULL),
                          predherbplot_treepreds + 
                            ggtitle("c") +
                            theme(legend.position = "none",
                                  axis.title=element_text(size=rel(1.5))),
                          predherbplot_mobipreds  + 
                            ggtitle("d") +
                            ylab(NULL) +
                            theme(legend.position = "none",
                                  axis.title=element_text(size=rel(1.5))),
                          predherbplot_para + 
                            ggtitle("d") +
                            theme(legend.position = "none",
                                  axis.title=element_text(size=rel(1.5)))),
             ncol =2)
dev.off()






# Testing bromeliad seasonal overall content -------------------------
#Make frame
calc_season <- 
  distance %>% 
  ungroup %>% 
  dplyr::select(Site,alltrees, Bromeliad, Longest_leaf_length) %>% 
  rename(leaf = Longest_leaf_length) %>% 
  unite(brom, alltrees, Bromeliad, sep ="_", remove = F) %>% 
  unique() %>% 
  left_join(
    purabromzy %>% 
      ungroup() %>% 
      dplyr::select(alltrees, Bromeliad, Sampling, Abundance) %>% 
      rename(todo = Abundance) %>% 
      group_by(alltrees, Bromeliad, Sampling) %>% 
      summarise_all(funs(sum)))
#Add 0s and sampling for empty bromeliads, and remove NAs
calc_season$todo <- 
  ifelse(calc_season$alltrees == "DO_E11" & calc_season$Bromeliad == "e" |
           calc_season$alltrees == "DO_D7" & calc_season$Bromeliad == "a" |
           calc_season$alltrees == "CP_J1" & calc_season$Bromeliad == "b" |
           calc_season$alltrees == "CP_J2" & calc_season$Bromeliad == "c",
         0, 
         calc_season$todo)
calc_season$Sampling[is.na(calc_season$Sampling)] <- 
  "B"
calc_season <- 
  na.omit(calc_season)
#Add predators and remove subsequent NAs
calc_season <- 
  calc_season %>% 
  left_join(
    purabromzy %>% 
      ungroup() %>% 
      filter(Diet %in% c("pred", "scav", "omni")) %>% 
      ###Removing irrelevant families
      filter(Family %notin% c("Gryllidae", "Anthicidae", "Aderidae", "Tenebrionidae")) %>% 
      dplyr::select(alltrees, Bromeliad, Abundance) %>% 
      rename(preds = Abundance) %>% 
      group_by(alltrees, Bromeliad) %>% 
      summarise_all(funs(sum)))
calc_season$preds[is.na(calc_season$preds)] <- 
  0

#Testing
##Everything
seasonmodel_todo <- 
  glmer(I(round(todo^0.25)) ~
          I(log(leaf))*Sampling +
         (1|Site/alltrees),
         family = "poisson"(link ="sqrt"),
        data = calc_season %>% filter(brom != "DO_G11_c"))
simulationOutput <- 
  simulateResiduals(fittedModel = seasonmodel_todo, 
                    n = 2000)
plot(simulationOutput,
     quantreg = F)
summary(seasonmodel_todo)
seasontest_todo <- 
  mixed(I(round(todo^0.25)) ~
          log(leaf)*Sampling +
          (1|Site/alltrees),
        family = "poisson"(link ="sqrt"),
        data = calc_season %>% filter(brom != "DO_G11_c"),
        type = afex_options(type = "2"),
        method = "LRT")$anova_table
###plot
####visreg
visreg(seasonmodel_todo,
       "Sampling", by="leaf")
####ggeffect
plot(ggeffect(seasonmodel_todo,
              terms = c("leaf", "Sampling"),
              type = "re",
              ci.level = 0.95))

##Predators
seasonmodel_preds <- 
  glmer(I(round(preds^0.25)) ~
         I(log(leaf))*Sampling +
         (1|Site/alltrees),
       family = "poisson"(link ="sqrt"),
       data = calc_season %>% filter(brom != "DO_G11_c"))
simulationOutput <- 
  simulateResiduals(fittedModel = seasonmodel_preds, 
                    n = 2000)
plotSimulatedResiduals(simulationOutput = simulationOutput,
                       quantreg = F)
summary(seasonmodel_preds)
seasontest_preds <- 
  mixed(I(round(preds^0.25)) ~
          I(log(leaf))*Sampling +
          (1|Site/alltrees),
        family = "poisson"(link ="sqrt"),
        data = calc_season %>% filter(brom != "DO_G11_c"),
        type = afex_options(type = "2"),
        method = "LRT")$anova_table
###plot
####visreg
visreg(seasonmodel_preds,
       "Sampling", by = "leaf")
####ggeffect
plot(ggeffect(seasonmodel_preds,
              terms = c("leaf", "Sampling"),
              type = "re",
              ci.level = 0.95))




# Solo plotting -----------------------------------------------------------
pdf("solo_predtreatment.pdf",
    width= 5,
    height = 5)
predsplot_treatment
dev.off()

pdf("solo_brompredlargeleaf.pdf",
    width= 8,
    height = 5)
brompredplot_largeleaf
dev.off()

pdf("solo_brompredtreatment.pdf",
    width= 5,
    height = 5)
brompredplot_treatment 
dev.off()

pdf("solo_mobipredtreatment.pdf",
    width= 5,
    height = 5)
mobipredplot_treatment
dev.off()

pdf("solo_leafpoolherb.pdf",
    width= 8,
    height = 5)
leafmodelplot_herb
dev.off()

pdf("solo_herblargeleaf.pdf",
    width= 8,
    height = 5)
herbplot_largeleaf
dev.off()

pdf("solo_brompredherb.pdf",
    width= 8,
    height = 5)
predherbplot_brompreds
dev.off()

pdf("solo_herbtreatment.pdf",
    width= 5,
    height = 5)
herbplot_treatment
dev.off()

pdf("solo_bromanttreatment.pdf",
    width= 5,
    height = 5)
bromantsplot_treatment
dev.off()

pdf("solo_nobromantplottreatment.pdf",
    width= 5,
    height = 5)
nobromantsplot_treatment
dev.off()

pdf("solo_bromhuntspidstreatment.pdf",
    width= 5,
    height = 5)
bromhuntspidsplot_treatment
dev.off()

pdf("solo_nobromhuntspidsplottreatment.pdf",
    width= 5,
    height = 5)
nobromhuntspidsplot_treatment
dev.off()

pdf("solo_leafpooltreatment.pdf",
    width= 5,
    height = 5)
leafpoolplot_treatment 
dev.off()
# Adonis frame (run #04 and #06 beforehand) --------------------------------
#bromeliad parameters in categories
write.csv(
  rbind(predadonis_largeleaf$aov.tab,predadonis_predindex$aov.tab, predadonis_treatment$aov.tab,
        brompredadonis_largeleaf$aov.tab, brompredadonis_predindex$aov.tab, brompredadonis_treatment$aov.tab,
        treepredadonis_largeleaf$aov.tab, treepredadonis_predindex$aov.tab, treepredadonis_treatment$aov.tab,
        mobipredadonis_largeleaf$aov.tab, mobipredadonis_predindex$aov.tab, mobipredadonis_treatment$aov.tab,
        herbadonis_largeleaf$aov.tab, herbadonis_predindex$aov.tab, herbadonis_treatment$aov.tab), 
  "bromfunctionalgroups_adonis.csv")

#predator categories on herbivores
write.csv(
  rbind(herbadonis_preds$aov.tab, 
        herbadonis_bromypred$aov.tab,
        herbadonis_arbopred$aov.tab,
        herbadonis_mobipred$aov.tab,
        herbadonis_para$aov.tab),
  "predherb_adonis.csv")

