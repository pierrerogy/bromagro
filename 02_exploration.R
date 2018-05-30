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









# Bromeliad-Tree communities comparisons ----------------------------------
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
    width = 20)
grid.arrange(grobs = list(predsplot_treatment + ggtitle("a") + theme(legend.position = "none",
                                                                     axis.title.x=element_blank(),
                                                                     axis.text.x=element_blank(),
                                                                     axis.ticks.x=element_blank()), 
                          brompredplot_treatment+ ggtitle("b")+ theme(axis.title.x=element_blank(),
                                                                      axis.text.x=element_blank(),
                                                                      axis.ticks.x=element_blank()),
                          bromantsplot_treatment+ ggtitle("c")+ theme(legend.position = "none",
                                                                      axis.title.x=element_blank(),
                                                                      axis.text.x=element_blank(),
                                                                      axis.ticks.x=element_blank()), 
                          bromhuntspidsplot_treatment+ ggtitle("d")+ theme(legend.position = "none",
                                                                           axis.title.x=element_blank(),
                                                                           axis.text.x=element_blank(),
                                                                           axis.ticks.x=element_blank()),
                          nobromantsplot_treatment+ ggtitle("e")+ theme(legend.position = "none",
                                                                        axis.title.x=element_blank(),
                                                                        axis.text.x=element_blank(),
                                                                        axis.ticks.x=element_blank()), 
                          nobromhuntspidsplot_treatment+ ggtitle("f")+ theme(legend.position = "none",
                                                                             axis.title.x=element_blank(),
                                                                             axis.text.x=element_blank(),
                                                                             axis.ticks.x=element_blank()),
                          herbplot_treatment + ggtitle("g")+ theme(legend.position = "none"), 
                          leafpoolplot_treatment + ggtitle("h")+ theme(legend.position = "none")),
             ncol = 2)
dev.off()

# Volume index plot grid --------------------------------------------------

pdf("panel_largeleaf.pdf",
    height = 20,
    width = 20)
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

# Intraguild grid ---------------------------------------------------------
pdf("intraplot_brompreds.pdf",
    height = 17,
    width = 8)
grid.arrange(grobs = list(intraplot_brompara + ggtitle("a") + theme(axis.title.x=element_blank(),
                                                                    axis.text.x=element_blank(),
                                                                    axis.ticks.x=element_blank()),
                          intraplot_brommobipred + ggtitle("b") + theme(legend.position = "none",
                                                                        axis.title.x=element_blank(),
                                                                        axis.text.x=element_blank(),
                                                                        axis.ticks.x=element_blank()),
                          intraplot_bromarbopred + ggtitle("c") + theme(legend.position = "none")),
                          ncol=1)
dev.off()
                                                                        
pdf("intraplot_para.pdf",
    height = 11,
    width = 8)
grid.arrange(grobs = list(intraplot_paraarbopred + ggtitle("a") + theme(axis.title.x=element_blank(),
                                                                        axis.text.x=element_blank(),
                                                                        axis.ticks.x=element_blank()),
                          intraplot_paramobipred + ggtitle("b") + theme(legend.position = "none")),
             ncol=1)                       

dev.off()


pdf("intraplot_aerial.pdf",
    height = 5,
    width = 8)
intraplot_mobiarbopred
dev.off()


# Predators on herbivores grid --------------------------------------------
pdf("predherbplot_preds.pdf",
    height = 15,
    width = 15)
grid.arrange(grobs = list(predherbplot_pred + 
                            ggtitle("a"),
                          predherbplot_brompreds +
                            ggtitle("b") +
                            theme(legend.position = "none"),
                          predherbplot_treepreds + 
                            ggtitle("c") +
                            theme(legend.position = "none"),
                          predherbplot_mobipreds  + 
                            ggtitle("d") +
                            theme(legend.position = "none"),
                          predherbplot_para + 
                            ggtitle("d") +
                            theme(legend.position = "none")),
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
plotSimulatedResiduals(simulationOutput = simulationOutput,
                       quantreg = F)
summary(seasonmodel_todo)
dispersion_glmer(seasonmodel_todo)
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
# Bargraph draft ----------------------------------------------------------


##bromeliad predators
chart <- 
  as.data.frame(effect("Treatment:Sampling", 
                       bromypredmodel_treatment))
chartwo <- 
  chart[which(chart$Treatment=="wo"),]
chartw <- 
  chart[which(chart$Treatment=="w"),]
chartwr <- 
  chart[which(chart$Treatment=="wr"),]
chartmeanbrompred <- 
  cbind(chartwo[,3], chartw[,3], chartwr[,3])
colnames(chartmeanbrompred) <- 
  c("Without", "With", "Removal")
rownames(chartmeanbrompred) <- 
  c("A", "B")
chartmeanbrompred <- 
  rbind(chartmeanbrompred[2,], chartmeanbrompred[1,])

chartcibrompred <- 
  rbind(chartwo[2,5:6],chartwo[1,5:6],
        chartw[2,5:6], chartw[1,5:6], 
        chartwr[2,5:6], chartwr[1,5:6])
barplot <- 
  barplot(chartmeanbrompred, 
          beside=T,
          ylim = c(0,5),
          ylab = "Bromeliad-associated predator abundance",
          space = c(0,0.5),
          col = c("grey30", "grey70"))
box(bty="l")
segments(barplot, chartcibrompred$lower, 
         barplot,chartcibrompred$upper, 
         lwd = 1.5)
arrows(barplot, chartcibrompred$lower, 
       barplot, chartcibrompred$upper, 
       lwd = 1.5,
       angle = 90,
       code = 3, 
       length = 0.05)
legend("topright",
       legend = c("Before", "After"),
       col = c("grey30", "grey70"),
       pch = 15,
       pt.bg =  c("grey30", "grey70")
)
##herbivores
chart <- 
  as.data.frame(effect("Treatment:Sampling", 
                       herbmodel_treatment))
chartwo <- 
  chart[which(chart$Treatment=="wo"),]
chartw <- 
  chart[which(chart$Treatment=="w"),]
chartwr <- 
  chart[which(chart$Treatment=="wr"),]
chartmeanherb <- 
  cbind(chartwo[,3], chartw[,3], chartwr[,3])
colnames(chartmeanherb) <- 
  c("Without", "With", "Removal")
rownames(chartmeanherb) <- 
  c("A", "B")
chartmeanherb <- 
  rbind(chartmeanherb[2,], chartmeanherb[1,])

chartciherb <- 
  rbind(chartwo[2,5:6],chartwo[1,5:6],
        chartw[2,5:6], chartw[1,5:6], 
        chartwr[2,5:6], chartwr[1,5:6])
barplot <- 
  barplot(chartmeanherb, 
          beside=T,
          ylim = c(0,5),
          ylab = "Herbivore abundance",
          space = c(0,0.5),
          col = c("grey30", "grey70"))
box(bty="l")
segments(barplot, chartciherb$lower, 
         barplot,chartciherb$upper, 
         lwd = 1.5)
arrows(barplot, chartciherb$lower, 
       barplot, chartciherb$upper, 
       lwd = 1.5,
       angle = 90,
       code = 3, 
       length = 0.05)
