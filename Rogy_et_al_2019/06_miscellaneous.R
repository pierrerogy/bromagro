#Miscellaneous
# Bromeliad-tree community comparison ----------------------------------
#CCA
bromtree_cca <- 
  vegan::cca(bromtree_comparison ~ loc, 
             data = whereloc)

anova.cca(bromtree_cca,
          permutations = 2000,
          strata = whereloc$Site)
#Plot
plot.new()
plot(NULL,
     type ="n",
     cex.axis = 1.8,
     cex.lab = 1.8,
     ylim = c(-3, 4),
     xlim = c(-2, 4),
     xlab = "CCA1",
     ylab = "CA1")
title("A",
      ##adj = to move title right/left
      ##line = to move title up/down
      adj = 0,
      cex = 2)
col <- 
  ifelse(whereloc$loc == "brom",
         "grey70", "grey40")
bromtree_cca_plot <- 
  points(bromtree_cca, 
         display = 'sites', 
         pch = 16,
         col=col)
ordihull(bromtree_cca,
         groups = whereloc$loc,
         show.groups = "brom",
         draw = "lines",
         col = "grey70",
         lwd = 4)
ordihull(bromtree_cca,
         groups = whereloc$loc,
         show.groups = "tree",
         draw = "lines",
         col = "grey40",
         lwd = 4) 

#Convert plot to  grob format
grid.echo()
bromtree_cca_plot <- 
  grid.grab()

# Effect of season on bromeliad contents -------------------------
#Make frame
calc_season <- 
  distance %>% 
  ungroup %>% 
  dplyr::select(Site,alltrees, Bromeliad, Longest_leaf_length) %>% 
  rename(leaf = Longest_leaf_length) %>% 
  unite(brom, alltrees, Bromeliad, sep ="_", remove = F) %>% 
  unique() %>% 
  left_join(
    clean_dissection %>% 
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
    clean_dissection %>% 
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
###Model
seasonmodel_todo <- 
  glmer(I(round(todo^0.25)) ~
          I(log(leaf))*Sampling +
          (1|Site/alltrees),
        family = "poisson"(link ="sqrt"),
        data = calc_season %>% filter(brom != "DO_G11_c"))
###Assumptions
simulationOutput <- 
  simulateResiduals(fittedModel = seasonmodel_todo, 
                    n = 2000)
plot(simulationOutput,
     quantreg = F)
###Summary and tests
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
###Model
seasonmodel_preds <- 
  glmer(I(round(preds^0.25)) ~
          I(log(leaf))*Sampling +
          (1|Site/alltrees),
        family = "poisson"(link ="sqrt"),
        data = calc_season %>% filter(brom != "DO_G11_c"))
###Assumptions
simulationOutput <- 
  simulateResiduals(fittedModel = seasonmodel_preds, 
                    n = 2000)
plot(simulationOutput,
     quantreg = F)
###Summary and tests
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





# Bar graphs for diet abundance per habitat -----------------------
#Abundance 
##Data
###Prepare
bromtree_dietab <- 
  clean_vacuum %>% 
  ungroup %>% 
  dplyr::select(Site, kind, Diet, Sampling, alltrees, Abundance) %>% 
  group_by(Site, kind, Diet, Sampling, alltrees) %>% 
  summarise_all(funs(sum)) %>% 
  mutate(loc = "tree") %>% 
  bind_rows(clean_dissection %>% 
              ungroup %>% 
              dplyr::select(Site, kind, Diet, Sampling, alltrees, Abundance) %>% 
              group_by(Site, kind, Diet, Sampling, alltrees) %>% 
              summarise_all(funs(sum)) %>% 
              mutate(loc = "brom")) %>% 
  ##Unique name for location,and sampling period of each sample
  unite(where, alltrees, loc, Sampling, sep= "_", remove = F) %>% 
  ##Remove the few confusing kleptoparasites and agromyzids
  filter(Diet != "klep") %>% 
  filter(kind != "herbflies")

##Pool all omnivores and pollinators together
bromtree_dietab$Diet[which(bromtree_dietab$Diet == "scav")] <- 
  "omni"
bromtree_dietab$Diet[which(bromtree_dietab$Diet %in% c("poll", "nect"))] <- 
  "poll/nect"
bromtree_dietab$Diet[which(bromtree_dietab$kind %in% c("heteroherb", "jump", "scaleaphid","ortho"))] <- 
  "phloem"
bromtree_dietab$Diet[which(bromtree_dietab$kind %in% c("herbeetle", "lepi", "snail"))] <- 
  "chewer"

##Group and compute diet proportion for each sample
bromtree_dietab <- 
  bromtree_dietab %>% 
  ungroup() %>% 
  dplyr::select(-kind) %>% 
  group_by(where, Diet, Sampling, Site, alltrees, loc) %>% 
  summarise_all(funs(sum)) %>% 
  ##Proportion for each sample
  ungroup() %>% 
  group_by(where) %>% 
  mutate(Abundance= Abundance/sum(Abundance)) 

###Get mean and sd with aggregate()
bromtree_dietab <- 
  aggregate(bromtree_dietab$Abundance , list(bromtree_dietab$Diet,
                                             bromtree_dietab$loc), 
            drop = F,
            mean) %>% 
  rename(mean=x) %>% 
  left_join(aggregate(bromtree_dietab$Abundance , list(bromtree_dietab$Diet,
                                                       bromtree_dietab$loc), 
                      drop = F,
                      sd)) %>% 
  rename(Diet=Group.1,
         loc=Group.2,
         sd=x)
###Replace rows singletons (sd = 0) with 0s
bromtree_dietab$mean[which(is.na(bromtree_dietab$sd))] <-
  0
bromtree_dietab[is.na(bromtree_dietab)] <-
  0
###Convert standard deviation to standard error
bromtree_dietab$sd[which(bromtree_dietab$loc == "brom")] <- 
  bromtree_dietab$sd[which(bromtree_dietab$loc == "brom")]/sqrt(58)
bromtree_dietab$sd[which(bromtree_dietab$loc == "tree")] <- 
  bromtree_dietab$sd[which(bromtree_dietab$loc == "tree")]/sqrt(238)
###Convert standard error to 95% confidence interval
bromtree_dietab$sd <- 
  bromtree_dietab$sd*1.96
bromtree_dietab <- 
  bromtree_dietab %>% 
  rename(ci=sd)
###Reorder factor levels for plot
bromtree_dietab$Diet <- 
  factor(bromtree_dietab$Diet, levels = c("phloem", "chewer", "pred", "omni",
                                          "para","detr", "myco", "poll/nect", "hema", "xylo", "gran",
                                          "nada", "unkn"))
##Plot
bromtree_dietab_plot <- 
  ggplot(data=bromtree_dietab, 
         aes(x=Diet,
             y=(mean +0.01)*100, 
             fill = loc)) +
  geom_bar(stat="identity",  position=position_dodge()) +
  geom_errorbar(aes(ymin = (mean - ci + 0.01)*100, ymax = (mean + ci + 0.01)*100), 
                width = 0.2,
                position = position_dodge(0.9)) +
  ylab("Mean abundance percentage")+
  xlab("") +
  scale_fill_manual(name = "",
                    labels = c("Bromeliad communities", "Tree communities"), 
                    values = c("grey70", "grey40")) +
  theme(legend.position = c(0.9,0.9),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))  +
  scale_x_discrete(breaks = c("phloem", "chewer", "pred", "omni",
                              "para","detr", "myco", "poll/nect", "hema", "xylo", "gran",
                              "nada", "unkn"),
                   labels = c("Sap", "Chw", "Prd", "Omn",
                              "Par","Det", "Myc", "Pol", "Hem", "Xyl", "Gra","Non", 
                              "Unk")) +
  scale_y_continuous(trans = "log", 
                     breaks = c(5, 10, 30, 90),
                     limits = c(1,100)) + 
  annotate("text", x = 0.9, y = 20, label = "*", size = 8) +
  annotate("text", x = 1.9, y = 15, label = "*", size = 8) +
  annotate("text", x = 3, y = 90, label = "*", size = 8) +
  annotate("text", x = 4.9, y = 13, label = "*", size = 8) +
  annotate("text", x = 5.9, y = 21, label = "*", size = 8) +
  annotate("text", x = 7, y = 7, label = "*", size = 8) +
  annotate("text", x = 8, y = 8, label = "*", size = 8) +
  annotate("text", x = 8.9, y = 9, label = "*", size = 8) +
  annotate("text", x = 9.9, y = 13, label = "*", size = 8) +
  annotate("text", x = 10.9, y = 6, label = "*", size = 8) +
  annotate("text", x = 11.9, y = 14, label = "*", size = 8) +
  annotate("text", x = 12.9, y = 10, label = "*", size = 8) 

# Bar graphs for diet richness per habitat -----------------------
#Abundance 
##Data
###Prepare
bromtree_dietri <- 
  clean_vacuum %>% 
  ungroup %>% 
  dplyr::select(Site, Morphospecies, kind, Diet, Sampling, alltrees, Abundance) %>% 
  group_by(Site, Morphospecies, kind, Diet, Sampling, alltrees) %>% 
  summarise_all(funs(sum)) %>% 
  mutate(loc = "tree") %>% 
  bind_rows(clean_dissection %>% 
              ungroup %>% 
              dplyr::select(Site, Morphospecies, kind, Diet, Sampling, alltrees, Abundance) %>% 
              group_by(Site, kind, Morphospecies, Diet, Sampling, alltrees) %>% 
              summarise_all(funs(sum)) %>% 
              mutate(loc = "brom")) %>% 
  ##Unique name for location,and sampling period of each sample
  unite(where, alltrees, loc, Sampling, sep= "_", remove = F) %>% 
  ##Remove the few confusing kleptoparasites and agromyzids
  filter(Diet != "klep") %>% 
  filter(kind != "herbflies")

##Pool all omnivores and pollinators together
bromtree_dietri$Diet[which(bromtree_dietri$Diet == "scav")] <- 
  "omni"
bromtree_dietri$Diet[which(bromtree_dietri$Diet %in% c("poll", "nect"))] <- 
  "poll/nect"
bromtree_dietri$Diet[which(bromtree_dietri$kind %in% c("heteroherb", "jump", "scaleaphid","ortho"))] <- 
  "phloem"
bromtree_dietri$Diet[which(bromtree_dietri$kind %in% c("herbeetle", "lepi", "snail"))] <- 
  "chewer"
##Convert to richness, group and compute diet proportion for each sample
bromtree_dietri <- 
  bromtree_dietri %>% 
  ungroup() %>% 
  mutate(Abundance = 1) %>%
  dplyr::select(-kind, -Morphospecies) %>%
  group_by(where, Diet, Sampling, Site, alltrees, loc) %>% 
  summarise_all(funs(sum)) %>% 
  ##Proportion
  ungroup() %>% 
  group_by(where) %>% 
  mutate(Abundance= Abundance/sum(Abundance))

###Get mean and sd with aggregate()
bromtree_dietri <- 
  aggregate(bromtree_dietri$Abundance , list(bromtree_dietri$Diet,
                                             bromtree_dietri$loc), 
            drop = F,
            mean) %>% 
  rename(mean=x) %>% 
  left_join(aggregate(bromtree_dietri$Abundance , list(bromtree_dietri$Diet,
                                                       bromtree_dietri$loc), 
                      drop = F,
                      sd)) %>% 
  rename(Diet=Group.1,
         loc=Group.2,
         sd=x)
###Replace rows singletons (sd = 0) with 0s
bromtree_dietri$mean[which(is.na(bromtree_dietri$sd))] <-
  0
bromtree_dietri[is.na(bromtree_dietri)] <-
  0
###Convert standard deviation to standard error
bromtree_dietri$sd[which(bromtree_dietri$loc == "brom")] <- 
  bromtree_dietri$sd[which(bromtree_dietri$loc == "brom")]/sqrt(58)
bromtree_dietri$sd[which(bromtree_dietri$loc == "tree")] <- 
  bromtree_dietri$sd[which(bromtree_dietri$loc == "tree")]/sqrt(238)
###Convert standard error to 95% confidence interval
bromtree_dietri$sd <- 
  bromtree_dietri$sd*1.96
bromtree_dietri <- 
  bromtree_dietri %>% 
  rename(ci=sd)
###Reorder factor levels for plot
bromtree_dietri$Diet <- 
  factor(bromtree_dietri$Diet, levels = c("phloem", "chewer", "pred", "omni",
                                          "para","detr", "myco", "poll/nect", "hema", "xylo", "gran",
                                          "nada", "unkn"))
##Plot
bromtree_dietri_plot <- 
  ggplot(data=bromtree_dietri, 
         aes(x=Diet,
             y=(mean +0.01)*100, 
             fill = loc)) +
  geom_bar(stat="identity",  position=position_dodge()) +
  geom_errorbar(aes(ymin = (mean - ci + 0.01)*100, ymax = (mean + ci + 0.01)*100), 
                width = 0.2,
                position = position_dodge(0.9))  +
  ylab("Mean richness percentage")+
  xlab("") +
  scale_fill_manual(name = "",
                    labels = c("Bromeliad communities", "Tree communities"), 
                    values = c("grey70", "grey40")) +
  theme(legend.position = c(0.9,0.9),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))  +
  scale_x_discrete(breaks = c("phloem", "chewer", "pred", "omni",
                              "para","detr", "myco", "poll/nect", "hema", "xylo", "gran",
                              "nada", "unkn"),
                   labels = c("Sap", "Chw", "Prd", "Omn",
                              "Par","Det", "Myc", "Pol", "Hem", "Xyl", "Gra","Non", 
                              "Unk"))+
  scale_y_continuous(trans = "log", 
                     breaks = c(5, 10, 30, 90),
                     limits = c(1,100)) + 
  annotate("text", x = 2, y = 25, label = "*", size = 8) +
  annotate("text", x = 4, y = 35, label = "*", size = 8) +
  annotate("text", x = 8, y = 8, label = "*", size = 8) +
  annotate("text", x = 8.9, y = 8, label = "*", size = 8)


# Figure 2 --------------------------------------------
tiff("figure2.tiff",
     height = 24,
     width = 36,
     unit="cm",
     res = 200)
grid.arrange(grobs = list(bromtree_cca_plot,
                          bromtree_dietab_plot +
                            ggtitle("B") +
                            theme(plot.title = element_text(face = "bold",
                                                            size=rel(2)),
                                  legend.text = element_text(size=rel(2)),
                                  legend.position = c(0.7,0.9),
                                  axis.text.x = element_blank(),
                                  axis.text.y=element_text(size=rel(1.9)),
                                  axis.title=element_text(size=rel(1.9))),
                          bromtree_dietri_plot +
                            ggtitle("C") +
                            xlab("Functional groups") +
                            theme(legend.position = "none",
                                  plot.title = element_text(face = "bold",
                                                            size=rel(2)),
                                  axis.text.y=element_text(size=rel(1.9)),
                                  axis.text.x=element_text(size=rel(1.8)),
                                  axis.title=element_text(size=rel(1.9)))),
             layout_matrix = rbind(c(1, 2),
                                   c(1, 3)))
dev.off()

# Figure 3 ----------------------------------
tiff("figure3.tiff",
     height = 19,
     width = 12,
     unit="in",
     res = 200)
grid.arrange(grobs = list(predsplot_treetype + 
                            ggtitle("A") + 
                            theme(legend.position = "none",
                                  axis.title.x=element_blank(),
                                  axis.text.x=element_blank(),
                                  axis.ticks.x=element_blank(),
                                  axis.text.y=element_text(size=rel(1.5)),
                                  axis.title=element_text(size=rel(1.5)),
                                  plot.title = element_text(size=rel(1.5))), 
                          brompredplot_treetype+ 
                            ggtitle("B")+ 
                            theme(legend.title=element_text(size=rel(1.45)), 
                                  legend.text=element_text(size=rel(1.45)),
                                  axis.text.y=element_text(size=rel(1.5)),
                                  axis.title.x=element_blank(),
                                  axis.text.x=element_blank(),
                                  axis.ticks.x=element_blank(),
                                  axis.title=element_text(size=rel(1.5)),
                                  plot.title = element_text(size=rel(1.5))),
                          bromantsplot_treetype+ 
                            ggtitle("C")+ 
                            theme(legend.position = "none",
                                  axis.title.x=element_blank(),
                                  axis.text.y=element_text(size=rel(1.5)),
                                  axis.text.x=element_blank(),
                                  axis.ticks.x=element_blank(),
                                  axis.title=element_text(size=rel(1.5)),
                                  plot.title = element_text(size=rel(1.5))), 
                          bromhuntspidsplot_treetype+ 
                            ggtitle("D")+ 
                            theme(legend.position = "none",
                                  axis.title.x=element_blank(),
                                  axis.text.x=element_blank(),
                                  axis.text.y=element_text(size=rel(1.5)),
                                  axis.ticks.x=element_blank(),
                                  axis.title=element_text(size=rel(1.35)),
                                  plot.title = element_text(size=rel(1.5))),
                          nobromantsplot_treetype+ 
                            ggtitle("E")+ 
                            theme(legend.position = "none",
                                  axis.title.x=element_blank(),
                                  axis.text.x=element_blank(),
                                  axis.ticks.x=element_blank(),
                                  axis.text.y=element_text(size=rel(1.5)),
                                  axis.title=element_text(size=rel(1.5)),
                                  plot.title = element_text(size=rel(1.5))), 
                          nobromhuntspidsplot_treetype+ 
                            ggtitle("F")+ 
                            theme(legend.position = "none",
                                  axis.title.x=element_blank(),
                                  axis.text.x=element_blank(),
                                  axis.text.y=element_text(size=rel(1.5)),
                                  axis.ticks.x=element_blank(),
                                  axis.title=element_text(size=rel(1.3)),
                                  plot.title = element_text(size=rel(1.5))),
                          herbplot_treetype + 
                            ggtitle("G")+ 
                            theme(legend.position = "none",
                                  axis.title=element_text(size=rel(1.5)),
                                  axis.text.x=element_text(size=rel(2)),
                                  axis.text.y=element_text(size=rel(1.5)),
                                  plot.title = element_text(size=rel(1.5))), 
                          leafpoolplot_treetype + 
                            ggtitle("H")+ 
                            theme(legend.position = "none",
                                  axis.title=element_text(size=rel(1.5)),
                                  axis.text.x=element_text(size=rel(2)),
                                  axis.text.y=element_text(size=rel(1.5)),
                                  plot.title = element_text(size=rel(1.5)))),
             ncol = 2)
dev.off()

# Figure S1  --------------------------------------------------
tiff("figureS1.tiff",
     height = 10,
     width = 4,
     unit="in",
     res = 200)
grid.arrange(grobs = list(predsplot_largeleaf + 
                            ggtitle("A") +
                            theme(axis.title.x=element_blank(),
                                  axis.text.x=element_blank(),
                                  axis.ticks.x=element_blank()),
                          brompredplot_largeleaf + 
                            ggtitle("B") +
                            theme(legend.position = "none",
                                  axis.title.x=element_blank(),
                                  axis.text.x=element_blank(),
                                  axis.ticks.x=element_blank()),
                          herbplot_largeleaf +
                            ggtitle("C") +
                            theme(legend.position = "none",
                                  axis.title.x=element_blank(),
                                  axis.text.x=element_blank(),
                                  axis.ticks.x=element_blank()),
                          chewerplot_largeleaf + 
                            ggtitle("D") +
                            theme(legend.position = "none")),
             ncol =1)
dev.off()



# Figure S2 - S4 --------------------------------------------
#All herbivores
tiff("figureS2.tiff",
     height = 18,
     width = 12,
     unit="in",
     res = 200)
grid.arrange(grobs = list(predherbplot_pred + 
                            theme(legend.position = "none",
                                  axis.title=element_text(size=rel(1.5)),
                                  axis.text=element_text(size=rel(1.5)),
                                  plot.title = element_text(size=rel(1.5))) +
                            ggtitle("A"),
                          predherbplot_brompreds +
                            ggtitle("B") +
                            theme(axis.title=element_text(size=rel(1.5)),
                                  axis.text=element_text(size=rel(1.5)),
                                  axis.text.y = element_blank(),
                                  legend.text = element_text(size=rel(1.5)),
                                  legend.title = element_text(size=rel(1.5)),
                                  plot.title = element_text(size=rel(1.5))) +
                            ylab(NULL),
                          predherbplot_treepreds + 
                            ggtitle("C") +
                            theme(legend.position = "none",
                                  axis.title=element_text(size=rel(1.5)),
                                  axis.text=element_text(size=rel(1.5)),
                                  plot.title = element_text(size=rel(1.5))),
                          predherbplot_mobipreds  + 
                            ggtitle("D") +
                            ylab(NULL) +
                            theme(legend.position = "none",
                                  axis.title=element_text(size=rel(1.5)),
                                  axis.text=element_text(size=rel(1.5)),
                                  axis.text.y = element_blank(),
                                  plot.title = element_text(size=rel(1.5))),
                          predherbplot_para + 
                            ggtitle("E") +
                            theme(legend.position = "none",
                                  axis.title=element_text(size=rel(1.5)),
                                  axis.text=element_text(size=rel(1.5)),
                                  plot.title = element_text(size=rel(1.5)))),
             ncol =2)
dev.off()

#Leaf chewers
tiff("figureS3.tiff",
     height = 18,
     width = 12,
     unit="in",
     res = 200)
grid.arrange(grobs = list(predchewerplot_pred + 
                            theme(legend.position = "none",
                                  axis.title=element_text(size=rel(1.5)),
                                  axis.text=element_text(size=rel(1.5)),
                                  plot.title = element_text(size=rel(1.5))) +
                            ggtitle("A"),
                          predchewerplot_brompreds +
                            ggtitle("B") +
                            theme(axis.title=element_text(size=rel(1.5)),
                                  axis.text=element_text(size=rel(1.5)),
                                  axis.text.y = element_blank(),
                                  legend.text = element_text(size=rel(1.5)),
                                  legend.title = element_text(size=rel(1.5)),
                                  plot.title = element_text(size=rel(1.5))) +
                            ylab(NULL),
                          predchewerplot_treepreds + 
                            ggtitle("C") +
                            theme(legend.position = "none",
                                  axis.title=element_text(size=rel(1.5)),
                                  axis.text=element_text(size=rel(1.5)),
                                  plot.title = element_text(size=rel(1.5))),
                          predchewerplot_mobipreds  + 
                            ggtitle("D") +
                            ylab(NULL) +
                            theme(legend.position = "none",
                                  axis.title=element_text(size=rel(1.5)),
                                  axis.text=element_text(size=rel(1.5)),
                                  axis.text.y = element_blank(),
                                  plot.title = element_text(size=rel(1.5))),
                          predchewerplot_para + 
                            ggtitle("E") +
                            theme(legend.position = "none",
                                  axis.title=element_text(size=rel(1.5)),
                                  axis.text=element_text(size=rel(1.5)),
                                  plot.title = element_text(size=rel(1.5)))),
             ncol =2)
dev.off()

#Phloem feeders
tiff("figureS4.tiff",
     height = 18,
     width = 12,
     unit="in",
     res = 200)
grid.arrange(grobs = list(predphloemplot_pred + 
                            theme(legend.position = "none",
                                  axis.title=element_text(size=rel(1.5)),
                                  axis.text=element_text(size=rel(1.5)),
                                  plot.title = element_text(size=rel(1.5))) +
                            ggtitle("A"),
                          predphloemplot_brompreds +
                            ggtitle("B") +
                            theme(axis.title=element_text(size=rel(1.5)),
                                  axis.text=element_text(size=rel(1.5)),
                                  axis.text.y = element_blank(),
                                  legend.text = element_text(size=rel(1.5)),
                                  legend.title = element_text(size=rel(1.5)),
                                  plot.title = element_text(size=rel(1.5))) +
                            ylab(NULL),
                          predphloemplot_treepreds + 
                            ggtitle("C") +
                            theme(legend.position = "none",
                                  axis.title=element_text(size=rel(1.5)),
                                  axis.text=element_text(size=rel(1.5)),
                                  plot.title = element_text(size=rel(1.5))),
                          predphloemplot_mobipreds  + 
                            ggtitle("D") +
                            ylab(NULL) +
                            theme(legend.position = "none",
                                  axis.title=element_text(size=rel(1.5)),
                                  axis.text=element_text(size=rel(1.5)),
                                  axis.text.y = element_blank(),
                                  plot.title = element_text(size=rel(1.5))),
                          predphloemplot_para + 
                            ggtitle("E") +
                            theme(legend.position = "none",
                                  axis.title=element_text(size=rel(1.5)),
                                  axis.text=element_text(size=rel(1.5)),
                                  plot.title = element_text(size=rel(1.5)))),
             ncol =2)
dev.off()

# Figure S5 - S7 ---------------------------------------------------------
#Bromeliad-associated predators
tiff("figureS5.tiff",
     height = 15,
     width = 5,
     unit="in",
     res = 200)
grid.arrange(grobs = list(intraplot_brompara + ggtitle("A") + theme(axis.title.x=element_blank(),
                                                                    axis.text.x=element_blank(),
                                                                    axis.text.y=element_text(size=rel(1.5)),
                                                                    axis.ticks.x=element_blank(),
                                                                    legend.title=element_text(size=rel(1.5)),
                                                                    legend.text=element_text(size=rel(1.5)),
                                                                    axis.title=element_text(size=rel(1.5)),
                                                                    plot.title = element_text(size=rel(1.5))),
                          intraplot_brommobipred + ggtitle("B") + theme(legend.position = "none",
                                                                        axis.title.x=element_blank(),
                                                                        axis.text.y=element_text(size=rel(1.5)),
                                                                        axis.text.x=element_text(size=rel(1.5)),
                                                                        axis.ticks.x=element_blank(),
                                                                        axis.title=element_text(size=rel(1.5)),
                                                                        plot.title = element_text(size=rel(1.5))),
                          intraplot_bromarbopred + ggtitle("C") + theme(legend.position = "none",
                                                                        axis.text.y=element_text(size=rel(1.5)),
                                                                        axis.text.x=element_text(size=rel(1.5)),
                                                                        axis.title=element_text(size=rel(1.5)),
                                                                        plot.title = element_text(size=rel(1.5)))),
             ncol=1)
dev.off()     

#Parasitoids
tiff("figureS6.tiff",
     height = 10,
     width = 5,
     unit="in",
     res = 200)
grid.arrange(grobs = list(intraplot_paraarbopred + ggtitle("A") + theme(axis.title.x=element_blank(),
                                                                        axis.text.x=element_blank(),
                                                                        axis.ticks.x=element_blank(),
                                                                        legend.title=element_text(size=rel(1.5)),
                                                                        legend.text=element_text(size=rel(1.5)),
                                                                        axis.text.y=element_text(size=rel(1.5)),
                                                                        axis.title=element_text(size=rel(1.5)),
                                                                        plot.title = element_text(size=rel(1.5))),
                          intraplot_paramobipred + ggtitle("B") + theme(legend.position = "none",
                                                                        axis.text.y=element_text(size=rel(1.5)),
                                                                        axis.text.x=element_text(size=rel(1.5)),
                                                                        axis.title=element_text(size=rel(1.5)),
                                                                        plot.title = element_text(size=rel(1.5)))),
             ncol=1)                       

dev.off()

#Aerial predators
tiff("figureS7.tiff",
     height = 5,
     width = 5,
     unit="in",
     res = 200)
intraplot_mobiarbopred + theme(axis.title=element_text(size=rel(1.5)),
                               legend.text=element_text(size=rel(1.5)),
                               legend.title=element_text(size=rel(1.5)),
                               axis.text.y=element_text(size=rel(1.5)),
                               axis.text.x=element_text(size=rel(1.5)))
dev.off()





