# Multivariate analyses on community structure and behaviour

# Adonis functional and taxonomic groups CP---------------------------------------------------------
#Diet with herbivores split between functional groups
##Bromeliads
obsadoniscp_bromdiet_rev <- 
  adonis(obsdietcp_rev[,5:15] ~
           Bromeliads,
         method = "bray",
         permutations = 2000,
         strata = obsdietcp_rev$block,
         data = obsdietcp_rev)
##Bromeliad-associated predators
obsadoniscp_brompreddiet_rev <- 
  adonis(obsdietcp_nobrompreds[,5:15] ~
           brompred,
         method = "bray",
         permutations = 2000,
         strata = obsdietcp_nobrompreds_rev$block,
         data = obsdietcp_nobrompreds_rev)


#Kinds
##Bromeliads
obsadoniscp_bromkind <- 
  adonis(obskindcp[,5:31] ~
           Bromeliads,
         method = "bray",
         permutations = 2000,
         strata = obskindcp$block,
         data = obskindcp)
##Bromeliad predators
obsadoniscp_brompredkind <- 
  adonis(obskindcp_nobrompreds[,5:31] ~
           brompred,
         method = "bray",
         permutations = 2000,
         strata = obskindcp_nobrompreds$block,
         data = obskindcp_nobrompreds)

#Restricted kinds
##Bromeliads
obsadoniscp_bromkind_sub <- 
  adonis(obskindcp_sub[,5:20] ~
           Bromeliads,
         method = "bray",
         permutations = 2000,
         strata = obskindcp_sub$block,
         data = obskindcp_sub)
##Bromeliad predators
obsadoniscp_brompredkind_sub <- 
  adonis(obskindcp_sub_nobrompreds[,5:20] ~
           brompred,
         method = "bray",
         permutations = 2000,
         strata = obskindcp_sub_nobrompreds$block,
         data = obskindcp_sub_nobrompreds)

# Adonis functional and taxonomic groups DO ---------------------------------------------------------
#Diet with herbivores split between functional groups
##Bromeliads
obsadonisdo_bromdiet_rev <- 
  adonis(obsdietdo_rev[,4:11] ~
           Bromeliads,
         method = "bray",
         permutations = 2000,
         data = obsdietdo_rev)
##Bromeliad predators
obsadonisdo_brompreddiet_rev <- 
  adonis(obsdietdo_nobrompreds_rev[,4:11] ~
           brompred,
         method = "bray",
         permutations = 2000,
         data = obsdietdo_nobrompreds_rev)

#Kinds
##Bromeliads
obsadonisdo_bromkind <- 
  adonis(obskinddo[,4:26] ~
           Bromeliads,
         method = "bray",
         permutations = 2000,
         data = obskinddo)
##Bromeliad predators
obsadonisdo_brompredkind <- 
  adonis(obskinddo_nobrompreds[,4:26] ~
           brompred,
         method = "bray",
         permutations = 2000,
         data = obskinddo_nobrompreds)

#Restricted kinds
##Bromeliads
obsadonisdo_bromkind_sub <- 
  adonis(obskinddo_sub[,4:18] ~
           Bromeliads,
         method = "bray",
         permutations = 2000,
         data = obskinddo_sub)
##Bromeliad predators
obsadonisdo_brompredkind_sub <- 
  adonis(obskinddo_sub_nobrompreds[,4:18] ~
           brompred,
         method = "bray",
         permutations = 2000,
         data = obskinddo_sub_nobrompreds)

# Adonis behaviour with leaf chewers and phloem feeders--------------------------------------------------------
#CP
##Duration
###Bromeliads
obsadonis_behavdurcp_brom_rev <- 
  adonis(spread_behavdurcp_rev[,5:14] ~
           Bromeliads,
         method = "euclidean",
         permutations = 2000,
         strata = spread_behavdurcp_rev$block,
         data = spread_behavdurcp_rev)
###Bromeliad-associated predators
obsadonis_behavdurcp_brompred_rev <- 
  adonis(spread_behavdurcp_nobrompreds_rev[,5:14] ~
           brompred,
         method = "euclidean",
         permutations = 2000,
         strata = spread_behavdurcp_nobrompreds_rev$block,
         data = spread_behavdurcp_nobrompreds_rev)
##Frequency
###Bromeliads
obsadonis_behavfreqcp_brom_rev<- 
  adonis(spread_behavfreqcp_rev[,5:14] ~
           Bromeliads,
         method = "bray",
         permutations = 2000,
         strata = spread_behavfreqcp$block,
         data = spread_behavfreqcp)
###Bromeliad-associated predators
obsadonis_behavfreqcp_brompred_rev <- 
  adonis(spread_behavfreqcp_nobrompreds_rev[,5:14] ~
           brompred,
         method = "bray",
         permutations = 2000,
         strata = spread_behavfreqcp_nobrompreds_rev$block,
         data = spread_behavfreqcp_nobrompreds_rev)

#DO
##Duration
###Bromeliads
obsadonis_behavdurdo_brom_rev <- 
  adonis(spread_behavdurdo_rev[,5:17] ~
           Bromeliads,
         method = "euclidean",
         permutations = 2000,
         strata = spread_behavdurdo_rev$block,
         data = spread_behavdurdo_rev)
###Bromeliad-associated predators
obsadonis_behavdurdo_brompred_rev <- 
  adonis(spread_behavdurdo_nobrompreds_rev[,5:17] ~
           brompred,
         method = "euclidean",
         permutations = 2000,
         strata = spread_behavdurdo_nobrompreds_rev$block,
         data = spread_behavdurdo_nobrompreds_rev)
##Frequency
##Bromeliads
obsadonis_behavfreqdo_brom_rev <- 
  adonis(spread_behavfreqdo_rev[,5:17] ~
           Bromeliads,
         method = "bray",
         permutations = 2000,
         strata = spread_behavfreqdo_rev$block,
         data = spread_behavfreqdo_rev)
###Bromeliad-associated predators
obsadonis_behavfreqdo_brompred_rev <- 
  adonis(spread_behavfreqdo_nobrompreds_rev[,5:17] ~
           brompred,
         method = "bray",
         permutations = 2000,
         strata = spread_behavfreqdo_nobrompreds_rev$block,
         data = spread_behavfreqdo_nobrompreds_rev)

# Diel adonises with leaf chewers and phloem feeders --------------------------------------------------
#Dietary groups
##Bromeliads
noctadonis_bromdiet_rev <- 
  adonis(noctdiet_rev[,6:16] ~
           Bromeliads*Time,
         method = "bray",
         strata = noctdiet_rev$block,
         permutations = 2000,
         data = noctdiet_rev)
##Bromeliad-associated predators
noctadonis_brompreddiet_rev <- 
  adonis(noctdiet_nobrompreds_rev[,6:16] ~
           brompred*Time,
         method = "bray",
         strata = noctdiet_nobrompreds_rev$block,
         permutations = 2000,
         data = noctdiet_nobrompreds_rev)

#Behaviour duration
##Bromeliads
noctadonis_brombehavdur_rev <- 
  adonis(spread_noctbehavdur_rev[,6:15] ~
           Bromeliads*Time,
         method = "euclidean",
         strata = spread_noctbehavdur_rev$block,
         permutations = 2000,
         data = spread_noctbehavdur_rev)
##Bromeliad-associated predators
noctadonis_brompredbehavdur_rev <- 
  adonis(spread_noctbehavdur_nobrompreds_rev[,6:15] ~
           brompred*Time,
         method = "euclidean",
         strata = spread_noctbehavdur_nobrompreds_rev$block,
         permutations = 2000,
         data = spread_noctbehavdur_nobrompreds_rev)

#Behaviour frequency
##Bromeliads
noctadonis_brombehavfreq_rev <- 
  adonis(spread_noctbehavfreq_rev[,6:15] ~
           Bromeliads*Time,
         method = "bray",
         strata = spread_noctbehavfreq_rev$block,
         permutations = 2000,
         data = spread_noctbehavfreq_rev)
##Bromeliad-associated predators
noctadonis_brompredbehavfreq_rev <- 
  adonis(spread_noctbehavfreq_nobrompreds_rev[,6:15] ~
           brompred*Time,
         method = "bray",
         strata = spread_noctbehavfreq_nobrompreds_rev$block,
         permutations = 2000,
         data = spread_noctbehavfreq_nobrompreds_rev)


