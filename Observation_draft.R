#Read data
observation <- 
  read.csv("C://Users/pierr/OneDrive/Projects/bromagro/Data/Observation.csv",
           stringsAsFactors = F)
str(observation)

# Tidying  ---------------------------------------------------------

#Create unique names for trees 
observation <- 
  observation[,1:24] %>% 
  unite(alltrees, Site, Tree, sep = '_', remove = F) %>% 
  ##remove trees mistakenly observed or wrong treatment
  filter(alltrees %notin% c("CP_J2", "DO_K5", "DO_K6",
                            "CP_A1", "CP_H5", "CP_G3")) %>% 
  filter(block != "X" ) %>% 
  ##remove oothecas
  filter(Diet != "") %>% 
  filter(behaviour !="")

#Correct typos and factorize
observation$Bromeliads <- 
  factor(observation$Bromeliads, levels = c("present", "absent"))
observation$Bromeliads[which(observation$alltrees =="CP_F2")] <- 
  "present"
observation$Time <- 
  factor(observation$Time, levels = c("Day", "Night"))


#Add dummy column for frequency count
observation$number <- 
  1

#Remove mozzies
observation <- 
  observation %>% 
  filter(Family != "Culicidae")

#Convert date to day number
observation$Day <- 
  as.Date(observation$Day,
          format='%m-%d-%Y')
observation$Day <- 
  lubridate::yday(observation$Day)
##bringing to better "starting point"
observation$Day <- 
  observation$Day - 152

#Bring all predators under the same diet
obspred <- 
  observation %>% 
  ungroup() %>% 
  filter(block %in% c("A", "B", "C", "N")) %>% 
  filter(Diet %in% c("pred", "scav", "omni", "para") &
           Family %notin% c("Gryllidae", "Anthicidae", "Aderidae", "Tenebrionidae")) %>% 
  dplyr::select(Day, Site, alltrees, block, rep, Observer, obs, Bromeliads, Specimen, number) %>% 
  group_by(Day, Site, alltrees, block, rep, Observer, obs, Bromeliads, Specimen) %>% 
  summarise_all(funs(mean)) %>% 
  ungroup() %>% 
  dplyr::select(-Specimen, -Observer, -obs) %>% 
  group_by(Day, Site, alltrees, block, rep, Bromeliads) %>% 
  summarise_all(funs(sum)) 
obspred <-
  unique(observation %>% 
           dplyr::select(Day, Site, alltrees, block, rep, Bromeliads)) %>% 
  left_join(obspred)
obspred$number[is.na(obspred$number)] <- 
  0


noctobspred <- 
  observation %>% 
  ungroup() %>% 
  filter(Site == "CP" & block %in% c("A", "B", "C")) %>% 
  filter(Diet %in% c("pred", "scav", "omni", "para") &
           Family %notin% c("Gryllidae", "Anthicidae", "Aderidae", "Tenebrionidae")) %>% 
  dplyr::select(Site, alltrees, block, rep, Observer, obs, Time, Bromeliads, Specimen, number) %>% 
  group_by(Site, alltrees, block, rep, Observer, obs, Time, Bromeliads, Specimen) %>% 
  summarise_all(funs(mean)) %>% 
  ungroup() %>% 
  dplyr::select(-Specimen, -Observer, -obs) %>% 
  group_by(Site, alltrees, block, rep, Time, Bromeliads) %>% 
  summarise_all(funs(sum))
noctobspred <- 
  unique(observation %>% 
           filter(Site =="CP") %>% 
           dplyr::select(alltrees, block, rep, Time, Bromeliads)) %>%
  left_join(noctobspred)
noctobspred$number[is.na(noctobspred$number)] <- 
  0

#Add column for bromeliad predators
observation <- 
  observation %>% 
  left_join(calc_bromstuff %>% 
              rename(Species = Morphospecies))
##Make abundance column
##overall
calc_obsbrompred <- 
  observation %>% 
  ungroup() %>% 
  filter(where == "brom") %>% 
  filter(Diet %in% c("pred","omni","scav")) %>% 
  filter(Family %notin% c("Gryllidae", "Anthicidae", "Aderidae", "Tenebrionidae")) %>% 
  dplyr::select(alltrees, rep, Observer, obs, Specimen, number) %>% 
  group_by(alltrees, rep, Observer, obs, Specimen) %>% 
  summarise_all(funs(mean)) %>% 
  ungroup() %>% 
  dplyr::select(-Specimen, - Observer, -obs) %>% 
  group_by(alltrees, rep) %>%
  summarise_all(funs(sum)) %>% 
  rename(brompred = number)
##Night/Day
calc_noctobsbrompred <- 
  observation %>% 
  ungroup() %>% 
  filter(Site == "CP") %>% 
  filter(where == "brom") %>% 
  filter(Diet %in% c("pred","omni","scav")) %>% 
  filter(Family %notin% c("Gryllidae", "Anthicidae", "Aderidae", "Tenebrionidae")) %>% 
  dplyr::select(alltrees, rep, Observer, obs, Time, Specimen, number) %>% 
  group_by(alltrees, rep, Observer, obs, Time, Specimen) %>% 
  summarise_all(funs(mean)) %>% 
  ungroup() %>% 
  dplyr::select(-Specimen, -Observer, -obs) %>% 
  group_by(alltrees, rep, Time) %>%
  summarise_all(funs(sum)) %>% 
  rename(brompred = number)
str(observation)
str(calc_obsbrompred)

#Make column to filter out bromeliad predators
observation$isolation <- 
  ifelse(observation$where == "brom" &
           observation$Diet %in% c("pred","omni","scav") &
           observation$Family %notin% c("Gryllidae", "Anthicidae", "Aderidae", "Tenebrionidae"),
         "yes",
         NA)
# Adding kinds ------------------------------------------------------------

observation$kind <- 
  NA
##Add categories, based on taxonomy/behavior
observation$kind <- 
  ifelse(
    ##herbivores
    observation$Order == "Gastropoda" & 
      observation$Diet == "herb",  "snail",
    ifelse(
      observation$cleanspec == "NewSp_leaf_miner", "miner",
      ifelse(
        observation$Order == "Lepidoptera"& 
          observation$Diet == "herb",  "lepi",
        ifelse(
          observation$Order == "Coleoptera" &
            observation$Diet %in% c("herb", "poll"),  "herbeetle",
          ifelse(
            observation$cleanspec %in% c("Hemiptera_BI", "Hemiptera_BP", "Homoptera_CF","Homoptera_CG",
                                         "Homoptera_CJ", "Hemiptera_F"),"jump",
            ifelse(
              observation$Family %in% c("Aphididae", "Coccoidea"), "scaleaphid",
              ifelse(
                observation$Suborder == "Auchenorrhyncha",  "jump",
                ifelse(
                  observation$Suborder == "Heteroptera" &
                    observation$Diet == "herb",  "heteroherb",
                  ifelse(
                    observation$Order == "Orthoptera" &
                      observation$Diet == "herb",  "ortho",
                    ##predators
                    ifelse(
                      observation$Family =="Formicidae",  "ants",
                      ifelse(
                        observation$Family == "Vespidae",  "wasps",
                        ifelse(
                          observation$Order == "Blattodea" &
                            observation$Diet =="scav",  "roaches",
                          ifelse(
                            observation$Order == "Neuroptera",  "lacewings",
                            ifelse(
                              observation$Order == "Coleoptera" &
                                observation$Diet == "pred",  "predbeetle",
                              ifelse(
                                observation$Suborder == "Heteroptera" &
                                  observation$Diet == "pred",  "heteropred",
                                ifelse(
                                  observation$Order == "Opiliones",  "opilio",
                                  ifelse(
                                    observation$Order == "Diptera" &
                                      observation$Diet == "pred",  "predflies",
                                    ifelse(
                                      observation$Order == "Diptera" &
                                        observation$Diet == "herb",  "herbflies",
                                      ifelse(
                                        observation$Order == "Dermaptera", "earwig",
                                        ifelse(
                                          observation$Order == "Mantodea", "mantid",
                                          ifelse(
                                            observation$Order == "Araneae" &
                                              observation$cleanspec %in% c("Spider_A", "Spider_X", "Spider_Z", "Spider_AC", "Spider_AD", "Spider_AL",
                                                                           "Spider_AU", "Spider_AW", "Spider_AZ", "Spider_BA", "Spider_BF", "Spider_BO",
                                                                           "Spider_BG", "Spider_BR", "Spider_BU", "Spider_BV", "Spider_BW", "Spider_G",
                                                                           "Spider_BZ", "Spider_CI", "Spider_CJ", "Spider_CQ", "Spider_CW", "Spider_DD",
                                                                           "Spider_DI", "Spider_DO", "Spider_DQ", "Spider_DU", "Spider_DV", "Spider_I",
                                                                           "Spider_DY", "Spider_EL", "Spider_EQ", "Spider_EM", "Spider_EY", "Spider_S",
                                                                           "Spider_FK", "Spider_FM", "Spider_GB","NewSp_blackgreenred_weaverspider_silverbutt",
                                                                           "Unknown_weaver_spider"),  "webspids", 
                                            ifelse(
                                              observation$Order == "Araneae" &
                                                observation$cleanspec %notin% c("Spider_A", "Spider_X", "Spider_Z", "Spider_AC", "Spider_AD", "Spider_AL",
                                                                                "Spider_AU", "Spider_AW", "Spider_AZ", "Spider_BA", "Spider_BF", "Spider_BO",
                                                                                "Spider_BG", "Spider_BR", "Spider_BU", "Spider_BV", "Spider_BW", "Spider_G",
                                                                                "Spider_BZ", "Spider_CI", "Spider_CJ", "Spider_CQ", "Spider_CW", "Spider_DD",
                                                                                "Spider_DI", "Spider_DO", "Spider_DQ", "Spider_DU", "Spider_DV", "Spider_I",
                                                                                "Spider_DY", "Spider_EL", "Spider_EQ", "Spider_EM", "Spider_EY", "Spider_S",
                                                                                "Spider_FK", "Spider_FM", "Spider_GB", "NewSp_blackgreenred_weaverspider_silverbutt",
                                                                                "Unknown_weaver_spider"), "huntspids", 
                                              ifelse(
                                                observation$Diet =="para", "para", 
                                                ifelse(
                                                  observation$Suborder != "", as.character(observation$Suborder), as.character(observation$Order)
                                                ))))))))))))))))))))))))



# Diet and kind frames for bromeliads ---------------------------------------------------------
#Diet
##CP
obsdietcp <-
  observation %>%
  filter(Site =="CP" & 
           block %in% c("A", "B", "C")) %>% 
  dplyr::select(alltrees, block, rep, Observer, obs, Bromeliads, Diet, Specimen,number) %>% 
  group_by(alltrees, block, rep, Observer, obs, Bromeliads, Diet, Specimen) %>% 
  summarise_all(funs(mean)) %>% 
  ungroup() %>% 
  dplyr::select(-Specimen, -Observer, -obs) %>% 
  group_by(alltrees, block, rep, Bromeliads, Diet) %>%
  summarise_all(funs(sum)) %>% 
  spread(key = Diet, number, fill =0)
  
###Remove low abundance diets
colSums(obsdietcp[,5:17])
obsdietcp <- 
  obsdietcp %>% 
  dplyr::select(-gran,-poll,-xylo)
###merge
obsdietcp <- 
  obsdietcp %>% 
  left_join(calc_obsbrompred)
obsdietcp$brompred[is.na(obsdietcp$brompred)] <- 
  0

##DO
obsdietdo <- 
  observation %>%
  filter(Site =="DO") %>% 
  dplyr::select(alltrees, block, Observer, obs, Bromeliads, Diet, Specimen,number) %>% 
  group_by(alltrees, block, Observer, obs, Bromeliads, Diet, Specimen) %>% 
  summarise_all(funs(mean)) %>% 
  ungroup() %>% 
  dplyr::select(-Specimen, -Observer, -obs) %>% 
  group_by(alltrees, block, Bromeliads, Diet) %>%
  summarise_all(funs(sum)) %>% 
  spread(key = Diet, number, fill =0)
###Remove low abundance diets
colSums(obsdietdo[,4:16])
obsdietdo <- 
  obsdietdo %>% 
  dplyr::select(-gran, -hema, -poll,-13, -nada, -omni, -myco)
###merge
obsdietdo <- 
  obsdietdo %>% 
  left_join(calc_obsbrompred[,-2])
obsdietdo$brompred[is.na(obsdietdo$brompred)] <- 
  0

#kind
##CP
obskindcp <- 
  observation %>%
  filter(Site == "CP" & 
           block %in% c("A", "B", "C")) %>% 
  dplyr::select(alltrees, block, rep, Observer, obs, Bromeliads, kind, Specimen,number) %>% 
  group_by(alltrees, block, rep, Observer, obs, Bromeliads, kind, Specimen) %>% 
  summarise_all(funs(mean)) %>% 
  ungroup() %>% 
  dplyr::select(-Specimen, -Observer, -obs) %>% 
  group_by(alltrees, block, rep, Bromeliads, kind) %>%
  summarise_all(funs(sum)) %>% 
  spread(key = kind, number, fill =0)
###Remove low abundance kinds
colSums(obskindcp[,5:41])
obskindcp <- 
  obskindcp %>% 
  dplyr::select(-Diplopoda, -earwig, -Hemiptera, Heteroptera, -mantid,
                -miner, -Orthoptera, -Sternorrhyncha, -Thysanoptera, - wasps)

##DO
obskinddo <- 
  observation %>%
  filter(Site == "DO") %>% 
  dplyr::select(alltrees, block, Observer, obs, Bromeliads, kind, Specimen,number) %>% 
  group_by(alltrees, block, Observer, obs, Bromeliads, kind, Specimen) %>% 
  summarise_all(funs(mean)) %>% 
  ungroup() %>% 
  dplyr::select(-Specimen, -Observer, -obs) %>% 
  group_by(alltrees, block, Bromeliads, kind) %>%
  summarise_all(funs(sum)) %>% 
  spread(key = kind, number, fill =0)
###Remove low abundance kinds
colSums(obskinddo[,4:39])
obskinddo <- 
  obskinddo %>% 
  dplyr::select(-Apocrita, -Coleoptera, -Hemiptera, -Heteroptera, -lacewings,
                -Lepidoptera, -mantid, -opilio, -Orthoptera, -Polyphaga,
                -predflies, -Sternorrhyncha, -Thysanoptera)

#Restricted kinds
obskindcp_sub <- 
  observation %>%
  filter(Site == "CP" & 
           block %in% c("A", "B", "C")) %>% 
  filter(kind %in% c("herbeetle", "heteroherb", "jump", "lepi", "miner",
                         "ortho", "psyllid", "scaleaphid", "snail", "ants", "heteropred", 
                         "huntspids", "lacewings", "predbeetle", "predflies", "opilio",
                         "roaches", "wasps", "webspids", "para")) %>% 
  dplyr::select(alltrees, block, rep, Observer, obs, Bromeliads, kind, Specimen,number) %>% 
  group_by(alltrees, block, rep, Observer, obs, Bromeliads, kind, Specimen) %>% 
  summarise_all(funs(mean)) %>% 
  ungroup() %>% 
  dplyr::select(-Specimen, -Observer, -obs) %>% 
  group_by(alltrees, block, rep, Bromeliads, kind) %>%
  summarise_all(funs(sum)) %>% 
  spread(key = kind, number, fill =0)
###Remove low abundance kinds
colSums(obskindcp_sub[,5:22])
obskindcp_sub <- 
  obskindcp_sub %>% 
  dplyr::select(-miner, -wasps)

##DO
obskinddo_sub <- 
  observation %>%
  filter(Site == "DO") %>% 
  filter(kind %in% c("herbeetle", "heteroherb", "jump", "lepi", "miner",
                         "ortho", "psyllid", "scaleaphid", "snail", "ants", "heteropred", 
                         "huntspids", "lacewings", "predbeetle", "predflies", "opilio",
                         "roaches", "wasps", "webspids", "para")) %>% 
  dplyr::select(alltrees, block, Observer, obs, Bromeliads, kind, Specimen,number) %>% 
  group_by(alltrees, block, Observer, obs, Bromeliads, kind, Specimen) %>% 
  summarise_all(funs(mean)) %>% 
  ungroup() %>% 
  dplyr::select(-Specimen, -Observer, -obs) %>%  
  group_by(alltrees, block, Bromeliads, kind) %>%
  summarise_all(funs(sum)) %>% 
  spread(key = kind, number, fill =0)
###Remove low abundance kinds
colSums(obskinddo_sub[,4:21])
obskinddo_sub <- 
  obskinddo_sub %>% 
  dplyr::select(-lacewings, -opilio, -predflies)


# Diet and kind frames for bromeliad-associated predtaors ---------------------------------------------------------
#Diet
##CP
obsdietcp_nobrompreds <- 
  observation %>%
  filter(Site =="CP" & 
           block %in% c("A", "B", "C") &
           is.na(isolation)) %>% 
  dplyr::select(alltrees, block, rep, Observer, obs, Bromeliads, Diet, Specimen,number) %>% 
  group_by(alltrees, block, rep, Observer, obs, Bromeliads, Diet, Specimen) %>% 
  summarise_all(funs(mean)) %>% 
  ungroup() %>% 
  dplyr::select(-Specimen, -Observer, -obs) %>% 
  group_by(alltrees, block, rep, Bromeliads, Diet) %>%
  summarise_all(funs(sum)) %>% 
  spread(key = Diet, number, fill =0)
###Remove low abundance diets
colSums(obsdietcp_nobrompreds[,5:17])
obsdietcp_nobrompreds <- 
  obsdietcp_nobrompreds %>% 
  dplyr::select(-gran,-poll,-xylo)
###merge
obsdietcp_nobrompreds <- 
  obsdietcp_nobrompreds %>% 
  left_join(calc_obsbrompred)
obsdietcp_nobrompreds$brompred[is.na(obsdietcp_nobrompreds$brompred)] <- 
  0

##DO
obsdietdo_nobrompreds <- 
  observation %>%
  filter(Site =="DO" &
           is.na(isolation)) %>% 
  dplyr::select(alltrees, block, Observer, obs, Bromeliads, Diet, Specimen,number) %>% 
  group_by(alltrees, block, Observer, obs, Bromeliads, Diet, Specimen) %>% 
  summarise_all(funs(mean)) %>% 
  ungroup() %>% 
  dplyr::select(-Specimen, -Observer, -obs) %>% 
  group_by(alltrees, block, Bromeliads, Diet) %>%
  summarise_all(funs(sum)) %>% 
  spread(key = Diet, number, fill =0)
###Remove low abundance diets
colSums(obsdietdo_nobrompreds[,4:16])
obsdietdo_nobrompreds <- 
  obsdietdo_nobrompreds %>% 
  dplyr::select(-gran, -hema, -poll,-13, -nada, -omni, -myco)
###merge
obsdietdo_nobrompreds <- 
  obsdietdo_nobrompreds %>% 
  left_join(calc_obsbrompred)
obsdietdo_nobrompreds$brompred[is.na(obsdietdo_nobrompreds$brompred)] <- 
  0

#Functional groups
##CP
obskindcp_nobrompreds <- 
  observation %>%
  filter(Site == "CP" & 
           block %in% c("A", "B", "C") &
           is.na(isolation)) %>% 
  dplyr::select(alltrees, block, rep, Observer, obs, Bromeliads, kind, Specimen,number) %>% 
  group_by(alltrees, block, rep, Observer, obs, Bromeliads, kind, Specimen) %>% 
  summarise_all(funs(mean)) %>% 
  ungroup() %>% 
  dplyr::select(-Specimen, -Observer, -obs) %>% 
  group_by(alltrees, block, rep, Bromeliads, kind) %>%
  summarise_all(funs(sum)) %>% 
  spread(key = kind, number, fill =0)
###Remove low abundance kinds
colSums(obskindcp_nobrompreds[,5:41])
obskindcp_nobrompreds <- 
  obskindcp_nobrompreds %>% 
  dplyr::select(-Diplopoda, -earwig, -Hemiptera, Heteroptera, -mantid,
                -miner, -Orthoptera, -Sternorrhyncha, -Thysanoptera, - wasps)
###merge
obskindcp_nobrompreds <- 
  obskindcp_nobrompreds %>% 
  left_join(calc_obsbrompred)
obskindcp_nobrompreds$brompred[is.na(obskindcp_nobrompreds$brompred)] <- 
  0

##DO
obskinddo_nobrompreds <- 
  observation %>%
  filter(Site == "DO" &
           is.na(isolation)) %>% 
  dplyr::select(alltrees, block,  Observer, obs, Bromeliads, kind, Specimen,number) %>% 
  group_by(alltrees, block, Observer, obs, Bromeliads, kind, Specimen) %>% 
  summarise_all(funs(mean)) %>% 
  ungroup() %>% 
  dplyr::select(-Specimen, -Observer, -obs) %>% 
  group_by(alltrees, block, Bromeliads, kind) %>%
  summarise_all(funs(sum)) %>% 
  spread(key = kind, number, fill =0)
###Remove low abundance kinds
colSums(obskinddo_nobrompreds[,4:39])
obskinddo_nobrompreds <- 
  obskinddo_nobrompreds %>% 
  dplyr::select(-Apocrita, -Coleoptera, -Hemiptera, -Heteroptera, -lacewings,
                -Lepidoptera, -mantid, -opilio, -Orthoptera, -Polyphaga,
                -predflies, -Sternorrhyncha, -Thysanoptera)
###merge
obskinddo_nobrompreds <- 
  obskinddo_nobrompreds %>% 
  left_join(calc_obsbrompred)
obskinddo_nobrompreds$brompred[is.na(obskinddo_nobrompreds$brompred)] <- 
  0


#Restricted kinds
obskindcp_sub_nobrompreds <- 
  observation %>%
  filter(Site == "CP"& 
           block %in% c("A", "B", "C") &
           is.na(isolation)) %>% 
  filter(kind %in% c("herbeetle", "heteroherb", "jump", "lepi", "miner",
                     "ortho", "psyllid", "scaleaphid", "snail", "ants", "heteropred", 
                     "huntspids", "lacewings", "predbeetle", "predflies", "opilio",
                     "roaches", "webspids", "para")) %>% 
  dplyr::select(alltrees, block, rep, Observer, obs, Bromeliads, kind, Specimen,number) %>% 
  group_by(alltrees, block, rep, Observer, obs, Bromeliads, kind, Specimen) %>% 
  summarise_all(funs(mean)) %>% 
  ungroup() %>% 
  dplyr::select(-Specimen, -Observer, -obs) %>% 
  group_by(alltrees, block, rep, Bromeliads, kind) %>%
  summarise_all(funs(sum)) %>% 
  spread(key = kind, number, fill =0)
###Remove low abundance kinds
colSums(obskindcp_sub_nobrompreds[,5:21])
obskindcp_sub_nobrompreds <- 
  obskindcp_sub_nobrompreds %>% 
  dplyr::select(-miner)
###merge
obskindcp_sub_nobrompreds <- 
  obskindcp_sub_nobrompreds %>% 
  left_join(calc_obsbrompred)
obskindcp_sub_nobrompreds$brompred[is.na(obskindcp_sub_nobrompreds$brompred)] <- 
  0

##DO
obskinddo_sub_nobrompreds <- 
  observation %>%
  filter(Site == "DO" &
           is.na(isolation)) %>% 
  filter(kind %in% c("herbeetle", "heteroherb", "jump", "lepi", "miner",
                     "ortho", "psyllid", "scaleaphid", "snail", "ants", "heteropred", 
                     "huntspids", "lacewings", "predbeetle", "predflies", "opilio",
                     "roaches", "wasps", "webspids", "para")) %>% 
  dplyr::select(alltrees, block, Observer, obs, Bromeliads, kind, Specimen,number) %>% 
  group_by(alltrees, block,Observer, obs, Bromeliads, kind, Specimen) %>% 
  summarise_all(funs(mean)) %>%
  ungroup() %>% 
  dplyr::select(-Specimen, -Observer, -obs) %>%  
  group_by(alltrees, block, Bromeliads, kind) %>%
  summarise_all(funs(sum)) %>% 
  spread(key = kind, number, fill =0)
###Remove low abundance kinds
colSums(obskinddo_sub_nobrompreds[,4:21])
obskinddo_sub_nobrompreds <- 
  obskinddo_sub_nobrompreds %>% 
  dplyr::select(-lacewings, -opilio, -predflies)
###merge
obskinddo_sub_nobrompreds <- 
  obskinddo_sub_nobrompreds %>% 
  left_join(calc_obsbrompred)
obskinddo_sub_nobrompreds$brompred[is.na(obskinddo_sub_nobrompreds$brompred)] <- 
  0





# Behaviour and interaction frames for bromeliads -----------------------------------------
#CP behaviour 
##duration
spread_behavdurcp <- 
  observation %>% 
  ungroup() %>% 
  filter(Site == "CP" & 
           block %in% c("A", "B", "C")) %>% 
  dplyr::select(alltrees, block, Bromeliads, rep, Observer, obs, Specimen, behaviour, Behavior_Duration) %>% 
  group_by(alltrees, block, Bromeliads, rep, Observer, obs, Specimen, behaviour) %>% 
  summarise_all(funs(mean)) %>%
  ungroup() %>% 
  dplyr::select(-Specimen, -Observer, -obs) %>% 
  group_by(alltrees, block, Bromeliads, rep, behaviour) %>%
  summarise_all(funs(sum)) %>% 
  spread(key=behaviour, Behavior_Duration, fill =0)
###Hellinger
spread_behavdurcp <- 
  data.frame(spread_behavdurcp[,1:5], 
             decostand(spread_behavdurcp[,6:11],
                       "hellinger"))
##Frequency
spread_behavfreqcp <- 
  observation %>% 
  ungroup() %>% 
  filter(Site == "CP" & 
           block %in% c("A", "B", "C")) %>% 
  dplyr::select(alltrees, block, Bromeliads, rep, Observer, obs, Specimen, behaviour, number) %>% 
  group_by(alltrees, block, Bromeliads, rep, Observer, obs, Specimen, behaviour) %>% 
  summarise_all(funs(mean)) %>%
  ungroup() %>% 
  dplyr::select(-Specimen, -Observer, -obs) %>%
  group_by(alltrees, block, Bromeliads, rep, behaviour) %>%
  summarise_all(funs(sum)) %>% 
  spread(key=behaviour, number, fill =0)

#CP interaction
spread_interactioncp <- 
  observation %>% 
  ungroup() %>% 
  filter(Site == "CP" & 
           block %in% c("A", "B", "C")) %>% 
  dplyr::select(alltrees, block, Bromeliads, rep, Observer, obs, Specimen, interaction, number) %>% 
  group_by(alltrees, block, Bromeliads, rep, Observer, obs, Specimen, interaction) %>% 
  summarise_all(funs(mean)) %>% 
  ungroup() %>% 
  dplyr::select(-Specimen, - Observer, -obs) %>% 
  group_by(alltrees, block, Bromeliads, rep, interaction) %>% 
  summarise_all(funs(sum)) %>%
  spread(key =interaction, number, fill = 0) %>% 
  rename(nada=V1) %>% 
  mutate(todo = nada + pos +neg)
  

#DO behaviour 
##duration
spread_behavdurdo <- 
  observation %>% 
  ungroup() %>% 
  filter(Site == "DO") %>% 
  dplyr::select(alltrees, block, Bromeliads, Observer, obs, rep, Specimen, behaviour, Behavior_Duration) %>% 
  group_by(alltrees, block, Bromeliads, rep, Observer, obs, Specimen, behaviour) %>% 
  summarise_all(funs(mean)) %>%
  ungroup() %>% 
  dplyr::select(-Specimen, -Observer, -obs) %>% 
  group_by(alltrees, block, Bromeliads, rep, behaviour) %>%
  summarise_all(funs(sum)) %>% 
  spread(key=behaviour, Behavior_Duration, fill =0)
###Hellinger
spread_behavdurdo <- 
  data.frame(spread_behavdurdo[,1:5], 
             decostand(spread_behavdurdo[,6:14],
                       "hellinger"))
##Frequency
spread_behavfreqdo <- 
  observation %>% 
  ungroup() %>% 
  filter(Site == "DO") %>% 
  dplyr::select(alltrees, block, Bromeliads, rep, Observer, obs, Specimen, behaviour, number) %>% 
  group_by(alltrees, block, Bromeliads, rep, Observer, obs, Specimen, behaviour) %>% 
  summarise_all(funs(mean)) %>%
  ungroup() %>% 
  dplyr::select(-Specimen, -Observer, -obs) %>% 
  group_by(alltrees, block, Bromeliads, rep, behaviour) %>%
  summarise_all(funs(sum)) %>% 
  spread(key=behaviour, number, fill =0)

#DO interaction
spread_interactiondo <- 
  observation %>% 
  ungroup() %>% 
  filter(Site == "DO") %>% 
  dplyr::select(alltrees, block, Bromeliads, rep, Observer, obs, Specimen, interaction, number) %>% 
  group_by(alltrees, block, Bromeliads, rep, Observer, obs, Specimen, interaction) %>% 
  summarise_all(funs(mean)) %>% 
  ungroup() %>% 
  dplyr::select(-Specimen, -Observer, -obs) %>% 
  group_by(alltrees, block, Bromeliads, rep, interaction) %>% 
  summarise_all(funs(sum)) %>%
  spread(key =interaction, number, fill = 0) %>% 
  rename(nada=V1)%>% 
  mutate(todo = nada + pos +neg)


# Behaviour and interaction frames for bromeliad-associated predators -----------------------------------------
#CP behaviour 
##duration
spread_behavdurcp_nobrompreds <- 
  observation %>% 
  ungroup() %>% 
  filter(Site == "CP" & 
           block %in% c("A", "B", "C") &
           is.na(isolation)) %>% 
  dplyr::select(alltrees, block, Bromeliads, rep, Observer, obs, Specimen, behaviour, Behavior_Duration) %>% 
  group_by(alltrees, block, Bromeliads, rep, Observer, obs, Specimen, behaviour) %>% 
  summarise_all(funs(mean)) %>%
  ungroup() %>% 
  dplyr::select(-Specimen, -Observer, -obs) %>% 
  group_by(alltrees, block, Bromeliads, rep, behaviour) %>%
  summarise_all(funs(sum)) %>% 
  spread(key=behaviour, Behavior_Duration, fill =0)
###Hellinger
spread_behavdurcp_nobrompreds  <- 
  data.frame(spread_behavdurcp_nobrompreds [,1:5], 
             decostand(spread_behavdurcp_nobrompreds [,6:11],
                       "hellinger"))
###merge
spread_behavdurcp_nobrompreds <- 
  spread_behavdurcp_nobrompreds %>% 
  left_join(calc_obsbrompred)
spread_behavdurcp_nobrompreds$brompred[is.na(spread_behavdurcp_nobrompreds$brompred)] <- 
  0

##Frequency
spread_behavfreqcp_nobrompreds  <- 
  observation %>% 
  ungroup() %>% 
  filter(Site == "CP" & 
           block %in% c("A", "B", "C") &
           is.na(isolation)) %>% 
  dplyr::select(alltrees, block, Bromeliads, rep, Observer, obs, Specimen, behaviour, number) %>% 
  group_by(alltrees, block, Bromeliads, rep, Observer, obs, Specimen, behaviour) %>% 
  summarise_all(funs(mean)) %>%
  ungroup() %>% 
  dplyr::select(-Specimen, -Observer, -obs) %>% 
  group_by(alltrees, block, Bromeliads, rep, behaviour) %>%
  summarise_all(funs(sum)) %>% 
  spread(key=behaviour, number, fill =0)
###merge
spread_behavfreqcp_nobrompreds <- 
  spread_behavfreqcp_nobrompreds %>% 
  left_join(calc_obsbrompred)
spread_behavfreqcp_nobrompreds$brompred[is.na(spread_behavfreqcp_nobrompreds$brompred)] <- 
  0

#DO behaviour 
##duration
spread_behavdurdo_nobrompreds  <- 
  observation %>% 
  ungroup() %>% 
  filter(Site == "DO" &
           is.na(isolation)) %>% 
  dplyr::select(alltrees, block, Bromeliads, rep, Observer, obs, Specimen, behaviour, Behavior_Duration) %>% 
  group_by(alltrees, block, Bromeliads, rep, Observer, obs, Specimen, behaviour) %>% 
  summarise_all(funs(mean)) %>%
  ungroup() %>% 
  dplyr::select(-Specimen, -Observer, -obs) %>% 
  group_by(alltrees, block, Bromeliads, rep, behaviour) %>%
  summarise_all(funs(sum)) %>% 
  spread(key=behaviour, Behavior_Duration, fill =0)
###Hellinger
spread_behavdurdo_nobrompreds  <- 
  data.frame(spread_behavdurdo_nobrompreds [,1:5], 
             decostand(spread_behavdurdo_nobrompreds [,6:14],
                       "hellinger"))
###merge
spread_behavdurdo_nobrompreds <- 
  spread_behavdurdo_nobrompreds %>% 
  left_join(calc_obsbrompred)
spread_behavdurdo_nobrompreds$brompred[is.na(spread_behavdurdo_nobrompreds$brompred)] <- 
  0

##Frequency
spread_behavfreqdo_nobrompreds  <- 
  observation %>% 
  ungroup() %>% 
  filter(Site == "DO" &
           is.na(isolation)) %>% 
  dplyr::select(alltrees, block, Bromeliads, rep, Observer, obs, Specimen, behaviour, number) %>% 
  group_by(alltrees, block, Bromeliads, rep, Observer, obs, Specimen, behaviour) %>% 
  summarise_all(funs(mean)) %>%
  ungroup() %>% 
  dplyr::select(-Specimen, -Observer, -obs) %>% 
  group_by(alltrees, block, Bromeliads, rep, behaviour) %>%
  summarise_all(funs(sum)) %>% 
  spread(key=behaviour, number, fill =0)
###merge
spread_behavfreqdo_nobrompreds <- 
  spread_behavfreqdo_nobrompreds %>% 
  left_join(calc_obsbrompred)
spread_behavfreqdo_nobrompreds$brompred[is.na(spread_behavfreqdo_nobrompreds$brompred)] <- 
  0


# Night/Day frames for bromeliads --------------------------------------------------------
#Diet
noctdiet <- 
  observation %>% 
  ungroup() %>% 
  filter(Site == "CP" & 
           block %in% c("A", "B", "C")) %>% 
  dplyr::select(alltrees, block, rep, Observer, obs, Bromeliads, Time, Diet, Specimen,number) %>% 
  group_by(alltrees, block, rep, Observer, obs, Bromeliads, Time, Diet, Specimen) %>% 
  summarise_all(funs(mean)) %>% 
  ungroup() %>% 
  dplyr::select(-Specimen, -Observer, -obs) %>% 
  group_by(alltrees, block, rep, Bromeliads, Time, Diet) %>%
  summarise_all(funs(sum)) %>% 
  spread(key = Diet, number, fill =0)
###Remove low abundance diets
colSums(noctdiet[,6:18])
noctdiet <- 
  noctdiet %>% 
  dplyr::select(-gran,-poll,-xylo)
###merge
noctdiet<- 
  noctdiet %>% 
  left_join(calc_noctobsbrompred)
noctdiet$brompred[is.na(noctdiet$brompred)] <- 
  0

#Kinds
noctkind <- 
  observation %>% 
  ungroup() %>% 
  filter(Site == "CP" & 
           block %in% c("A", "B", "C")) %>% 
  dplyr::select(alltrees, block, rep, Observer, obs, Bromeliads, Time, kind, Specimen,number) %>% 
  group_by(alltrees, block, rep, Observer, obs, Bromeliads, Time, kind, Specimen) %>% 
  summarise_all(funs(mean)) %>% 
  ungroup() %>% 
  dplyr::select(-Specimen, -Observer, -obs) %>% 
  group_by(alltrees, block, rep, Bromeliads, Time, kind) %>%
  summarise_all(funs(sum)) %>% 
  spread(key = kind, number, fill =0)
###Remove low abundance kinds
colSums(noctkind[,6:42])
noctkind <- 
  noctkind %>% 
  dplyr::select(-Diplopoda, -earwig, -Hemiptera, Heteroptera, -mantid,
                -miner, -Orthoptera, -Sternorrhyncha, -Thysanoptera, - wasps)

#Restricted kinds
noctkind_sub <- 
  observation %>% 
  ungroup() %>% 
  filter(Site == "CP" & 
           block %in% c("A", "B", "C")) %>% 
  filter(kind %in% c("herbeetle", "heteroherb", "jump", "lepi", "miner",
                     "ortho", "psyllid", "scaleaphid", "snail", "ants", "heteropred", 
                     "huntspids", "lacewings", "predbeetle", "predflies", "opilio",
                     "roaches", "webspids", "para")) %>% 
  dplyr::select(alltrees, block, rep, Observer, obs, Bromeliads, Time, kind, Specimen,number) %>% 
  group_by(alltrees, block, rep, Observer, obs, Bromeliads, Time, kind, Specimen) %>% 
  summarise_all(funs(mean)) %>% 
  ungroup() %>% 
  dplyr::select(-Specimen, -Observer, -obs) %>% 
  group_by(alltrees, block, rep, Bromeliads, Time, kind) %>%
  summarise_all(funs(sum)) %>% 
  spread(key = kind, number, fill =0)
###Remove low abundance kinds
colSums(noctkind_sub[,6:22])
noctkind_sub <- 
  noctkind_sub %>% 
  dplyr::select(-miner)

#Behaviour 
##duration
spread_noctbehavdur <- 
  observation %>% 
  ungroup() %>% 
  filter(Site == "CP" 
         & block %in% c("A", "B", "C")) %>% 
  dplyr::select(alltrees, block, Observer, obs, Bromeliads, rep, Time, Specimen, behaviour, Behavior_Duration) %>% 
  group_by(alltrees, block, Bromeliads, rep, Observer, obs, Time, Specimen, behaviour) %>% 
  summarise_all(funs(mean)) %>%
  ungroup() %>% 
  dplyr::select(-Specimen, -Observer, -obs) %>% 
  group_by(alltrees, block, Bromeliads, rep, Time, behaviour) %>%
  summarise_all(funs(sum)) %>% 
  spread(key=behaviour, Behavior_Duration, fill =0)
###Hellinger
spread_noctbehavdur <- 
  data.frame(spread_noctbehavdur[,1:5], 
             decostand(spread_noctbehavdur[,6:12],
                       "hellinger"))
##Frequency
spread_noctbehavfreq <- 
  observation %>% 
  ungroup() %>% 
  filter(Site == "CP" 
         & block %in% c("A", "B", "C")) %>% 
  dplyr::select(alltrees, block, Bromeliads, rep, Observer, obs, Time, Specimen, behaviour, number) %>% 
  group_by(alltrees, block, Bromeliads, rep, Observer, obs, Time, Specimen, behaviour) %>% 
  summarise_all(funs(mean)) %>%
  ungroup() %>% 
  dplyr::select(-Specimen, -Observer, -obs) %>% 
  group_by(alltrees, block, Bromeliads, rep, Time, behaviour) %>%
  summarise_all(funs(sum)) %>% 
  spread(key=behaviour, number, fill =0)

#Interaction
spread_noctinteraction <- 
  observation %>% 
  ungroup() %>% 
  filter(Site == "CP" 
         & block %in% c("A", "B", "C")) %>% 
  dplyr::select(alltrees, block, Bromeliads, rep, Observer, obs, Time, Specimen, interaction, number) %>% 
  group_by(alltrees, block, Bromeliads, rep, Observer, obs, Time, Specimen, interaction) %>% 
  summarise_all(funs(mean)) %>% 
  ungroup() %>% 
  dplyr::select(-Specimen, -Observer, -obs) %>% 
  group_by(alltrees, block, Bromeliads, rep, Time, interaction) %>% 
  summarise_all(funs(sum)) %>%
  spread(key =interaction, number, fill = 0) %>% 
  rename(nada=V1) %>% 
  mutate(todo = nada + pos +neg)

# Night/Day frames for bromeliads-associated predators --------------------------------------------------------
#Diet
noctdiet_nobrompreds <- 
  observation %>% 
  ungroup() %>% 
  filter(Site == "CP" & 
           block %in% c("A", "B", "C") &
           is.na(isolation)) %>% 
  dplyr::select(alltrees, block, rep, Observer, obs, Bromeliads, Time, Diet, Specimen,number) %>% 
  group_by(alltrees, block, rep, Observer, obs, Bromeliads, Time, Diet, Specimen) %>% 
  summarise_all(funs(mean)) %>% 
  ungroup() %>% 
  dplyr::select(-Specimen, -Observer, -obs) %>% 
  group_by(alltrees, block, rep, Bromeliads, Time, Diet) %>%
  summarise_all(funs(sum)) %>% 
  spread(key = Diet, number, fill =0)
###Remove low abundance diets
colSums(noctdiet_nobrompreds[,6:18])
noctdiet_nobrompreds <- 
  noctdiet_nobrompreds %>% 
  dplyr::select(-gran,-poll,-xylo)
###merge
noctdiet_nobrompreds<- 
  noctdiet_nobrompreds %>% 
  left_join(calc_noctobsbrompred)
noctdiet_nobrompreds$brompred[is.na(noctdiet_nobrompreds$brompred)] <- 
  0

#Kinds
noctkind_nobrompreds <- 
  observation %>% 
  ungroup() %>% 
  filter(Site == "CP" & 
           block %in% c("A", "B", "C") &
           is.na(isolation)) %>% 
  dplyr::select(alltrees, block, rep, Observer, obs, Bromeliads, Time, kind, Specimen,number) %>% 
  group_by(alltrees, block, rep, Observer, obs, Bromeliads, Time, kind, Specimen) %>% 
  summarise_all(funs(mean)) %>% 
  ungroup() %>% 
  dplyr::select(-Specimen, -Observer, -obs) %>% 
  group_by(alltrees, block, rep, Bromeliads, Time, kind) %>%
  summarise_all(funs(sum)) %>% 
  spread(key = kind, number, fill =0)
###Remove low abundance kinds
colSums(noctkind_nobrompreds[,6:42])
noctkind_nobrompreds <- 
  noctkind_nobrompreds %>% 
  dplyr::select(-Diplopoda, -earwig, -Hemiptera, Heteroptera, -mantid,
                -miner, -Orthoptera, -Sternorrhyncha, -Thysanoptera, - wasps)
###merge
noctkind_nobrompreds<- 
  noctkind_nobrompreds %>% 
  left_join(calc_noctobsbrompred)
noctkind_nobrompreds$brompred[is.na(noctkind_nobrompreds$brompred)] <- 
  0

#Restricted kinds
noctkind_sub_nobrompreds <- 
  observation %>% 
  ungroup() %>% 
  filter(Site == "CP" & 
           block %in% c("A", "B", "C") &
           is.na(isolation)) %>% 
  filter(kind %in% c("herbeetle", "heteroherb", "jump", "lepi", "miner",
                     "ortho", "psyllid", "scaleaphid", "snail", "ants", "heteropred", 
                     "huntspids", "lacewings", "predbeetle", "predflies", "opilio",
                     "roaches", "webspids", "para")) %>% 
  dplyr::select(alltrees, block, rep, Observer, obs, Bromeliads, Time, kind, Specimen,number) %>% 
  group_by(alltrees, block, rep, Observer, obs, Bromeliads, Time, kind, Specimen) %>% 
  summarise_all(funs(mean)) %>% 
  ungroup() %>% 
  dplyr::select(-Specimen, -Observer, -obs) %>% 
  group_by(alltrees, block, rep, Bromeliads, Time, kind) %>%
  summarise_all(funs(sum)) %>% 
  spread(key = kind, number, fill =0)
###Remove low abundance kinds
colSums(noctkind_sub_nobrompreds[,6:22])
noctkind_sub_nobrompreds <- 
  noctkind_sub_nobrompreds %>% 
  dplyr::select(-miner)
###merge
noctkind_sub_nobrompreds<- 
  noctkind_sub_nobrompreds %>% 
  left_join(calc_noctobsbrompred)
noctkind_sub_nobrompreds$brompred[is.na(noctkind_sub_nobrompreds$brompred)] <- 
  0

#Behaviour 
##duration
spread_noctbehavdur_nobrompreds <- 
  observation %>% 
  ungroup() %>% 
  filter(Site == "CP" &
           block %in% c("A", "B", "C") &
           is.na(isolation)) %>% 
  dplyr::select(alltrees, block, Bromeliads, rep, Observer, obs, Time, Specimen, behaviour, Behavior_Duration) %>% 
  group_by(alltrees, block, Bromeliads, rep, Observer, obs, Time, Specimen, behaviour) %>% 
  summarise_all(funs(mean)) %>%
  ungroup() %>% 
  dplyr::select(-Specimen, -Observer, -obs) %>% 
  group_by(alltrees, block, Bromeliads, rep, Time, behaviour) %>%
  summarise_all(funs(sum)) %>% 
  spread(key=behaviour, Behavior_Duration, fill =0)
###Hellinger
spread_noctbehavdur_nobrompreds <- 
  data.frame(spread_noctbehavdur_nobrompreds[,1:5], 
             decostand(spread_noctbehavdur_nobrompreds[,6:12],
                       "hellinger"))
###merge
spread_noctbehavdur_nobrompreds<- 
  spread_noctbehavdur_nobrompreds %>% 
  left_join(calc_noctobsbrompred)
spread_noctbehavdur_nobrompreds$brompred[is.na(spread_noctbehavdur_nobrompreds$brompred)] <- 
  0

##Frequency
spread_noctbehavfreq_nobrompreds <- 
  observation %>% 
  ungroup() %>% 
  filter(Site == "CP" 
         & block %in% c("A", "B", "C") &
           is.na(isolation)) %>% 
  dplyr::select(alltrees, block, Bromeliads, rep, Observer, obs, Time, Specimen, behaviour, number) %>% 
  group_by(alltrees, block, Bromeliads, rep, Observer, obs, Time, Specimen, behaviour) %>% 
  summarise_all(funs(mean)) %>%
  ungroup() %>% 
  dplyr::select(-Specimen, -Observer, -obs) %>% 
  group_by(alltrees, block, Bromeliads, rep, Time, behaviour) %>%
  summarise_all(funs(sum)) %>% 
  spread(key=behaviour, number, fill =0)
###merge
spread_noctbehavfreq_nobrompreds<- 
  spread_noctbehavfreq_nobrompreds %>% 
  left_join(calc_noctobsbrompred)
spread_noctbehavfreq_nobrompreds$brompred[is.na(spread_noctbehavfreq_nobrompreds$brompred)] <- 
  0



# Models on herbivores/predators  ------------------------------------------
#CP
##Predators
obsmodelcp_pred <- 
  glmer.nb(number ~ 
          Bromeliads+ 
            (1|block/alltrees),
        control =glmerControl(optimizer = "bobyqa"),
        data =obspred %>% filter(Site == "CP"))
simulationOutput <- 
  simulateResiduals(fittedModel =obsmodelcp_pred, 
                    n = 2000)
plot(simulationOutput,
     asFactor =T,
     quantreg = F)
summary(obsmodelcp_pred)
obstestcp_pred <- 
  mixed(number ~ 
          Bromeliads +
          (1|block/alltrees),
        data =obspred %>% filter(Site == "CP"),
        family = "negative.binomial"(theta = getME(obsmodelcp_pred,
                                                   "glmer.nb.theta")),
        control =glmerControl(optimizer = "bobyqa"),
        method = "LRT")$anova_table
visreg(obsmodelcp_pred, 
       "Bromeliads")
##Bromeliad-associated predators
obsmodelcp_brompred <-
  glmer(brompred ~ 
          Bromeliads +
          (1|block/alltrees),
        family = "poisson"(link ="log"),
        data =obsdietcp)
simulationOutput <- 
  simulateResiduals(fittedModel =obsmodelcp_brompred, 
                    n = 2000,
                    rank = T)
plot(simulationOutput,
     asFactor =T,
     quantreg = F)
summary(obsmodelcp_brompred)
obstestcp_brompred <- 
  mixed(brompred ~ 
          Bromeliads +
          (1|block/alltrees),
        data =obsdietcp,
        family = "poisson"(link ="log"),
        method = "LRT")$anova_table
visreg(obsmodelcp_brompred, 
       "Bromeliads")
##Herbivores
obsmodelcp_herb <- 
  glmer(herb ~ 
          Bromeliads +
          (1|block/alltrees),
          family ="poisson"(link ="inverse"),
        data =obsdietcp %>%
          filter(herb <200))
simulationOutput <- 
  simulateResiduals(fittedModel =obsmodelcp_herb, 
                    n = 2000)
plot(simulationOutput,
     asFactor =T,
     quantreg = F)
summary(obsmodelcp_herb)
obstestcp_herb <- 
  mixed(herb ~ 
          Bromeliads +
          (1|block/alltrees),
        family = "poisson"(link ="inverse"),
        data =obsdietcp %>% 
          filter(herb <200),
        method = "LRT")$anova_table
visreg(obsmodelcp_herb, 
       "Bromeliads")

#DO
##Predators
obsmodeldo_pred <- 
  glm.nb(number ~ 
        Bromeliads,
        data =obspred %>% filter(Site =="DO"))
simulationOutput <- 
  simulateResiduals(fittedModel =obsmodeldo_pred, 
                    n = 2000)
plot(simulationOutput,
     asFactor =T,
     quantreg = F)
summary(obsmodeldo_pred)
Anova(obsmodeldo_pred)
visreg(obsmodeldo_pred, 
       "Bromeliads")
##Bromeliad-associated predators
obsmodeldo_brompred <-
  glm.nb(brompred ~ 
             Bromeliads, 
           data = obsdietdo)
simulationOutput <- 
  simulateResiduals(fittedModel =obsmodeldo_brompred, 
                    n = 2000)
plot(simulationOutput,
     asFactor =T,
     quantreg = F)
summary(obsmodeldo_brompred)
Anova(obsmodeldo_brompred)
visreg(obsmodeldo_brompred, 
       "Bromeliads")
##Herbivores
obsmodeldo_herb <- 
  glm.nb(herb ~ 
        Bromeliads,
        data =obsdietdo)
simulationOutput <- 
  simulateResiduals(fittedModel =obsmodeldo_herb, 
                    n = 2000)
plot(simulationOutput,
     asFactor =T,
     quantreg = F)
summary(obsmodeldo_herb)
Anova(obsmodeldo_herb)
visreg(obsmodeldo_herb, 
       "Bromeliads")

# Models interactions -----------------------------------------------------
#CP
##Negative
intermodel_cpneg <-
  glmer(neg ~ 
          Bromeliads*todo+ 
          (1|block/alltrees),
        family = "poisson"(link = "log"),
        control = glmerControl(optimizer = "bobyqa"),
        data = spread_interactioncp)
simulationOutput <- 
  simulateResiduals(fittedModel =intermodel_cpneg, 
                    n = 2000,
                    rank = T)
plot(simulationOutput, 
     asFactor =F,
     quantreg = F,
     rank = T)
summary(intermodel_cpneg)
intertest_cpneg <- 
  mixed(neg ~ 
          Bromeliads*todo+ 
          (1|block/alltrees),
        family = "poisson"(link = "log"),
        control = glmerControl(optimizer = "bobyqa"),
        data = spread_interactioncp,
        method = "LRT")$anova_table
visreg(intermodel_cpneg, 
       "todo", 
       by="Bromeliads")
##positive
intermodel_cppos <-
  glmer.nb(pos ~ 
          Bromeliads*todo+ 
          (1|block/alltrees),
        family = "poisson"(link = "log"),
        data = spread_interactioncp)
simulationOutput <- 
  simulateResiduals(fittedModel =intermodel_cppos, 
                    n = 2000,
                    rank = T)
plot(simulationOutput, 
     asFactor =F,
     quantreg = F,
     rank = T)
summary(intermodel_cppos)
intertest_cppos <- 
  mixed(pos ~ 
          Bromeliads*todo+ 
          (1|block/alltrees),
        family = "negative.binomial"(theta = getME(intermodel_cppos,
                                                   "glmer.nb.theta")),
        data = spread_interactioncp,
        method = "LRT")$anova_table
visreg(intermodel_cppos, 
       "todo", 
       by="Bromeliads")
#DO
##Negative
intermodel_doneg <-
  glm.nb(neg ~ 
          Bromeliads*todo,
        data = spread_interactiondo)
simulationOutput <- 
  simulateResiduals(fittedModel =intermodel_doneg, 
                    n = 2000,
                    rank = T)
plotSimulatedResiduals(simulationOutput = simulationOutput, 
                       asFactor =F,
                       quantreg = F,
                       rank = T)
testZeroInflation(simulationOutput)
summary(intermodel_doneg)
Anova(intermodel_doneg)
visreg(intermodel_doneg, 
       "todo", 
       by="Bromeliads")
##positive
intermodel_dopos <-
  glm.nb(sqrt(pos) ~ 
          Bromeliads*todo,
        data = spread_interactiondo)
simulationOutput <- 
  simulateResiduals(fittedModel =intermodel_dopos, 
                    n = 2000,
                    rank = T)
plotSimulatedResiduals(simulationOutput = simulationOutput, 
                       asFactor =F,
                       quantreg = F,
                       rank = T)
summary(intermodel_dopos)
Anova(intermodel_dopos)
visreg(intermodel_dopos, 
       "todo", 
       by="Bromeliads")
# Adonis diet/kinds CP---------------------------------------------------------
#Diet
##Bromeliads
obsadoniscp_bromdiet <- 
  adonis(obsdietcp[,5:14] ~
           Bromeliads,
         method = "bray",
         permutations = 2000,
         strata = obsdietcp$block,
         data = obsdietcp)
##Bromeliad predators
obsadoniscp_brompreddiet <- 
  adonis(obsdietcp_nobrompreds[,5:14] ~
           brompred,
         method = "bray",
         permutations = 2000,
         strata = obsdietcp_nobrompreds$block,
         data = obsdietcp_nobrompreds)

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

# Adonis diet/kinds DO ---------------------------------------------------------
#Diet
##Bromeliads
obsadonisdo_bromdiet <- 
  adonis(obsdietdo[,4:9] ~
           Bromeliads,
         method = "bray",
         permutations = 2000,
         data = obsdietdo)
##Bromeliad predators
obsadonisdo_brompreddiet <- 
  adonis(obsdietdo_nobrompreds[,4:9] ~
           brompred,
         method = "bray",
         permutations = 2000,
         data = obsdietdo_nobrompreds)

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


# Adonis behaviour --------------------------------------------------------
#CP
##Duration
###Bromeliads
obsadonis_behavdurcp_brom <- 
  adonis(spread_behavdurcp[,5:11] ~
           Bromeliads,
         method = "euclidean",
         permutations = 2000,
         strata = spread_behavdurcp$block,
         data = spread_behavdurcp)
###Bromeliad-associated predators
obsadonis_behavdurcp_brompred <- 
  adonis(spread_behavdurcp_nobrompreds[,5:11] ~
          brompred,
         method = "euclidean",
         permutations = 2000,
         strata = spread_behavdurcp_nobrompreds$block,
         data = spread_behavdurcp_nobrompreds)
##Frequency
###Bromeliads
obsadonis_behavfreqcp_brom <- 
  adonis(spread_behavfreqcp[,5:11] ~
           Bromeliads,
         method = "bray",
         permutations = 2000,
         strata = spread_behavfreqcp$block,
         data = spread_behavfreqcp)
###Bromeliad-associated predators
obsadonis_behavfreqcp_brompred <- 
  adonis(spread_behavfreqcp_nobrompreds[,5:11] ~
           brompred,
         method = "bray",
         permutations = 2000,
         strata = spread_behavfreqcp_nobrompreds$block,
         data = spread_behavfreqcp_nobrompreds)

#DO
##Duration
###Bromeliads
obsadonis_behavdurdo_brom <- 
  adonis(spread_behavdurdo[,5:14] ~
           Bromeliads,
         method = "euclidean",
         permutations = 2000,
         strata = spread_behavdurdo$block,
         data = spread_behavdurdo)
###Bromeliad-associated predators
obsadonis_behavdurdo_brompred <- 
  adonis(spread_behavdurdo_nobrompreds[,5:14] ~
           brompred,
         method = "euclidean",
         permutations = 2000,
         strata = spread_behavdurdo_nobrompreds$block,
         data = spread_behavdurdo_nobrompreds)
##Frequency
##Bromeliads
obsadonis_behavfreqdo_brom <- 
  adonis(spread_behavfreqdo[,5:14] ~
           Bromeliads,
         method = "bray",
         permutations = 2000,
         strata = spread_behavfreqdo$block,
         data = spread_behavfreqdo)
###Bromeliad-associated predators
obsadonis_behavfreqdo_brompred <- 
  adonis(spread_behavfreqdo_nobrompreds[,5:14] ~
           brompred,
         method = "bray",
         permutations = 2000,
         strata = spread_behavfreqdo_nobrompreds$block,
         data = spread_behavfreqdo_nobrompreds)
# Night and Day models -----------------------------------------------------------
#Predators
noctmodel_pred <- 
  glmer.nb(number ~ 
          Bromeliads*Time+ 
          (1|block/alltrees),
          control = glmerControl(optimizer = "bobyqa"),
          data =noctobspred)
simulationOutput <- 
  simulateResiduals(fittedModel =noctmodel_pred, 
                    n = 2000)
plot(simulationOutput, 
     asFactor =T,
     quantreg = F)
summary(noctmodel_pred)
noctest_pred <- 
  mixed(number ~ 
          Bromeliads*Time +
          (1|block/alltrees),
        data =noctobspred,
        family = "negative.binomial"(theta = getME(noctmodel_pred,
                                     "glmer.nb.theta")),
        control = glmerControl(optimizer = "bobyqa"),
        method = "LRT")$anova_table
visreg(noctmodel_pred,
       "Bromeliads",
       by = "Time")

#Bromeliad  predators
noctmodel_brompred <- 
  glmer(brompred ~ 
          Bromeliads*Time+ 
          (1|block/alltrees),
        family = "poisson"(link ="log"),
        data =noctdiet)
simulationOutput <- 
  simulateResiduals(fittedModel =noctmodel_brompred, 
                    n = 2000)
plot(simulationOutput, 
     asFactor =T,
     quantreg = F)
summary(noctmodel_brompred)
noctest_brompred <- 
  mixed(brompred ~ 
          Bromeliads*Time +
          (1|block/alltrees),
        data =noctdiet,
        family = "poisson"(link ="log"),
        method = "LRT")$anova_table
visreg(noctmodel_brompred, 
       "Bromeliads",
       by = "Time")

#Herbivores
noctmodel_herb <- 
  glmer.nb(herb ~ 
             Bromeliads*Time+ 
             (1|block/alltrees),
           control = glmerControl(optimizer = "bobyqa"),
           data =noctdiet %>% 
             filter(herb < 200))
simulationOutput <- 
  simulateResiduals(fittedModel =noctmodel_herb, 
                    n = 2000)
plot(simulationOutput, 
     asFactor =T,
     quantreg = F)
summary(noctmodel_herb)
noctest_herb <- 
  mixed(herb~ 
          Bromeliads*Time +
          (1|block/alltrees),
        data =noctdiet%>% 
          filter(herb < 200),
        family = "negative.binomial"(theta = getME(noctmodel_herb,
                                                   "glmer.nb.theta")),
        control = glmerControl(optimizer = "bobyqa"),
        method = "LRT")$anova_table
visreg(noctmodel_herb, 
       "Time",
       by = "Bromeliads")

# Night and Day adonises --------------------------------------------------
#Diet
##Bromeliads
noctadonis_bromdiet <- 
  adonis(noctdiet[,6:15] ~
           Bromeliads*Time,
         method = "bray",
         strata = noctdiet$block,
         permutations = 2000,
         data = noctdiet)
##Bromeliad-associated predators
noctadonis_brompreddiet <- 
  adonis(noctdiet_nobrompreds[,6:15] ~
           brompred*Time,
         method = "bray",
         strata = noctdiet_nobrompreds$block,
         permutations = 2000,
         data = noctdiet_nobrompreds)

#Kinds
##Bromeliads
noctadonis_bromkind <- 
  adonis(noctkind[,6:32] ~
           Bromeliads*Time,
         method = "bray",
         strata = noctkind$block,
         permutations = 2000,
         data = noctkind)
##Bromeliad-associated predators
noctadonis_brompredkind <- 
  adonis(noctkind_nobrompreds[,6:33] ~
           brompred*Time,
         method = "bray",
         strata = noctkind_nobrompreds$block,
         permutations = 2000,
         data = noctkind_nobrompreds)


#Restricted kinds
##Bromeliads
noctadonis_bromkind_sub <- 
  adonis(noctkind_sub[,6:21] ~
           Bromeliads*Time,
         method = "bray",
         strata = noctkind_sub$block,
         permutations = 2000,
         data = noctkind_sub)
##Bromeliad-associated predators
noctadonis_brompredkind_sub <- 
  adonis(noctkind_sub_nobrompreds[,6:21] ~
           brompred*Time,
         method = "bray",
         strata = noctkind_sub_nobrompreds$block,
         permutations = 2000,
         data = noctkind_sub_nobrompreds)

#Behaviour duration
##Bromeliads
noctadonis_brombehavdur <- 
  adonis(spread_noctbehavdur[,6:12] ~
           Bromeliads*Time,
         method = "euclidean",
         strata = spread_noctbehavdur$block,
         permutations = 2000,
         data = spread_noctbehavdur)
##Bromeliad-associated predators
noctadonis_brompredbehavdur <- 
  adonis(spread_noctbehavdur_nobrompreds[,6:12] ~
           brompred*Time,
         method = "euclidean",
         strata = spread_noctbehavdur_nobrompreds$block,
         permutations = 2000,
         data = spread_noctbehavdur_nobrompreds)

#Behaviour frequency
##Bromeliads
noctadonis_brombehavfreq <- 
  adonis(spread_noctbehavfreq[,6:12] ~
           Bromeliads*Time,
         method = "bray",
         strata = spread_noctbehavfreq$block,
         permutations = 2000,
         data = spread_noctbehavfreq)
##Bromeliad-associated predators
noctadonis_brompredbehavfreq <- 
  adonis(spread_noctbehavfreq_nobrompreds[,6:12] ~
           brompred*Time,
         method = "bray",
         strata = spread_noctbehavfreq_nobrompreds$block,
         permutations = 2000,
         data = spread_noctbehavfreq_nobrompreds)


# Night and Day interactions -----------------------------------------------
##Negative
noctintermodel_neg <-
  glmer(neg ~ 
          Bromeliads*Time*todo+ 
          (1|block/alltrees),
        family = "poisson"(link = "log"),
        control = glmerControl(optimizer = "bobyqa"),
        data = spread_noctinteraction)
simulationOutput <- 
  simulateResiduals(fittedModel =noctintermodel_neg, 
                    n = 2000,
                    rank = T)
plot(simulationOutput, 
     asFactor =F,
     quantreg = F,
     rank = T)
summary(noctintermodel_neg)
noctintertest_neg <- 
  mixed(neg ~ 
          Bromeliads*Time*todo+ 
          (1|block/alltrees),
        family = "poisson"(link = "log"),
        control = glmerControl(optimizer = "bobyqa"),
        data = spread_noctinteraction,
        method = "LRT")$anova_table
visreg(noctintermodel_neg, 
       "Bromeliads", 
       by="Time")
##positive
noctintermodel_pos <-
  glmer(pos ~ 
          Bromeliads*Time*todo+ 
          (1|block/alltrees),
        family = "poisson"(link = "sqrt"),
        data = spread_noctinteraction)
simulationOutput <- 
  simulateResiduals(fittedModel =noctintermodel_pos, 
                    n = 2000,
                    rank = T)
plot(simulationOutput, 
     asFactor =F,
     quantreg = F,
     rank = T)
summary(noctintermodel_pos)
noctintertest_pos <- 
  mixed(pos ~ 
          Bromeliads*Time*todo+ 
          (1|block/alltrees),
        family = "poisson"(link = "sqrt"),
        data = spread_noctinteraction,
        method = "LRT")$anova_table
visreg(noctintermodel_pos, 
       "Time", 
       by="Bromeliads")
plot(ggeffect(noctintermodel_pos,
              terms = c("Bromeliads", "Time"),
              swap.pred = F,
              type = "re",
              ci.level = 0.95))


# Model plots -------------------------------------------------------------------
#Overall pred
obseffectcp_pred <- 
  ggeffect(obsmodelcp_pred,
           type = "re",
           x.as.factor = T,
           terms = c("Bromeliads"),
           ci.lvl = 0.95)
obsplotcp_pred <- 
  ggplot(obseffectcp_pred, 
         aes(x=x, 
             y=predicted, 
             colour= group)) + 
  geom_errorbar(aes(ymin=conf.low, 
                    ymax=conf.high), 
                width=0.1,
                lwd = 2,
                position = position_dodge(0.3)) +
  geom_point(position = position_dodge(0.3), 
             lwd =6) +
  ggtitle("") + 
  xlab("Bromeliads") +
  scale_x_discrete(limit = c("absent", "present"),
                   labels = c("Absent", "Present"),
                   expand = expand_scale(add = c(0.6)))+
  ylab("Predator abundance") +
  scale_color_manual(name = "",
                     labels = c(""),
                     values = c("steelblue")) +
  theme(legend.position = "",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))

#Day/Night pred
nocteffect_pred <- 
  ggeffect(noctmodel_pred,
           type = "re",
           x.as.factor = T,
           terms = c("Bromeliads", "Time"),
           ci.lvl = 0.95)
noctplot_pred <- 
  ggplot(nocteffect_pred, 
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
  xlab("Bromeliads") +
  scale_x_discrete(limit = c("absent", "present"),
                   labels = c("Absent", "Present"),
                   expand = expand_scale(add = c(0.6)))+
  ylab("Predator abundance") +
  scale_color_manual(name = "Time of observation",
                     labels = c("Day", "Night"),
                     values = c("ivory4", "black")) +
  theme(legend.position = c(0.82,0.9),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))

#Overall brompred
obseffectcp_brompred <- 
  ggeffect(obsmodelcp_brompred,
           type = "re",
           x.as.factor = T,
           terms = c("Bromeliads"),
           ci.lvl = 0.95)
obsplotcp_brompred <- 
  ggplot(obseffectcp_brompred, 
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
  xlab("Bromeliads") +
  scale_x_discrete(limit = c("absent", "present"),
                   labels = c("Absent", "Present"),
                   expand = expand_scale(add = c(0.6)))+
  ylab("Bromeliad-associated predator abundance") +
  scale_color_manual(name = "",
                     labels = "",
                     values = c("steelblue")) +
  theme(legend.position = "",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))


#Day/night brompred
nocteffect_brompred <- 
  ggeffect(noctmodel_brompred,
           type = "re",
           x.as.factor = T,
           terms = c("Bromeliads", "Time"),
           ci.lvl = 0.95)
noctplot_brompred <- 
  ggplot(nocteffect_brompred, 
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
  xlab("Bromeliads") +
  scale_x_discrete(limit = c("absent", "present"),
                   labels = c("Absent", "Present"),
                   expand = expand_scale(add = c(0.6)))+
  ylab("Bromeliad-associated predator abundance") +
  scale_color_manual(name = "Time of observation",
                     labels = c("Day", "Night"),
                     values = c("ivory4", "black")) +
  theme(legend.position = c(0.9,0.9),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))

#Panel plot

pdf("obspredbrompred_overall_time.pdf",
    width = 10,
    height= 10)

gridExtra::grid.arrange(grobs = list(obsplotcp_pred +
                                       ggtitle("a") + 
                                       theme(axis.title.x=element_blank(),
                                             axis.text.x=element_blank(),
                                             axis.ticks.x=element_blank()),
                                     noctplot_pred +
                                       ggtitle("b") +
                                       theme(axis.title.x=element_blank(),
                                             axis.text.x=element_blank(),
                                             axis.ticks.x=element_blank(),
                                             axis.title.y=element_blank()),
                                     obsplotcp_brompred +
                                       ggtitle("c"),
                                     noctplot_brompred +
                                       ggtitle("d") +
                                       theme(legend.position = "",
                                             axis.ticks.x=element_blank(),
                                             axis.title.y=element_blank())
                                     ),
                        ncol =2)
dev.off()

# Overall PCoA with plot--------------------------------------------------------------
pdf("Observationcp_pcoa.pdf",
    width = 5,
    height = 15)
par(mfrow= c(3,1))
#CP bromeliad diet
dietcp_pcoa <- 
  vegan::wcmdscale(vegdist(obsdietcp[,5:14],
                           distance = "bray"),
                   add = T,
                   k=2,
                   eig = T)
col <-
  ifelse(obsdietcp$Bromeliads == "present",
         "saddlebrown",
         "darkgreen")
ordiplot(dietcp_pcoa,
         type="n")
title("a",
      adj = 0)
points(dietcp_pcoa$points,
       pch = 16,
       col=col)
ordihull(dietcp_pcoa,
         groups = obsdietcp$Bromeliads,
         show.groups = "present",
         draw = "polygon",
         border =  "transparent",
         col = "saddlebrown")
ordihull(dietcp_pcoa,
         groups = obsdietcp$Bromeliads,
         show.groups = "absent",
         draw = "polygon",
         border =  "transparent",
         col = "darkgreen")

#CP bromeliad- restricted functional groups
kindcp_sub_pcoa <- 
  wcmdscale(vegdist(obskindcp_sub[,5:20],
                           method ="bray"), 
            k=2,
            eig =T)
##plot
ordiplot(kindcp_sub_pcoa,
         type ="n")
title("b",
      adj = 0)
col <- 
  ifelse(obskindcp_sub$Bromeliads == "present",
         "saddlebrown", 
         "darkgreen")
points(kindcp_sub_pcoa$points,
       pch = 16,
       col=col)
ordihull(kindcp_sub_pcoa,
         groups = obskindcp_sub$Bromeliads,
         show.groups = "present",
         draw = "polygon",
         border =  "transparent",
         col = "saddlebrown")
ordihull(kindcp_sub_pcoa,
         groups = obskindcp_sub$Bromeliads,
         show.groups = "absent",
         draw = "polygon",
         border =  "transparent",
         col = "darkgreen")

#CP bromeliad predators-behaviour frequency
behavfreqcp_nobrompreds_pcoa <- 
  cmdscale(vegdist(spread_behavfreqcp_nobrompreds[,5:11],
                   method ="bray"), 
           k=2,
           add=T,
           eig =T)
behavfreqcp_nobrompreds_pcoa <- 
  add.spec.scores(behavfreqcp_nobrompreds_pcoa,
                  spread_behavfreqcp_nobrompreds[,5:11],
                  method="wa.scores",
                  multi=1,
                  Rscale=F)

ordiplot(behavfreqcp_nobrompreds_pcoa,
         type ="n")
title("c",
      adj = 0)
col <- 
  ifelse(spread_behavfreqcp_nobrompreds$Bromeliads == "present",
         "saddlebrown", 
         "darkgreen")
points(behavfreqcp_nobrompreds_pcoa$points,
       pch = 16,
       col=col)
ordihull(behavfreqcp_nobrompreds_pcoa,
         groups = spread_behavfreqcp_nobrompreds$Bromeliads,
         show.groups = "present",
         draw = "polygon",
         border =  "transparent",
         col = "saddlebrown")
ordihull(behavfreqcp_nobrompreds_pcoa,
         groups = spread_behavfreqcp_nobrompreds$Bromeliads,
         show.groups = "absent",
         draw = "polygon",
         border =  "transparent",
         col = "darkgreen")
brompredvector <- 
  envfit(behavfreqcp_nobrompreds_pcoa$points,
         spread_behavfreqcp_nobrompreds$brompred,
         permutations = 2000)
plot(brompredvector,
     col ="black",
     labels = "Brom. predators",
     lwd = 2,
     cex= 1)

orditorp(behavfreqcp_nobrompreds_pcoa$cproj,
         display = "species",
         pcex = 0,
         cex = 1,
         air = 0.01)

dev.off()
par(mfrow = c(1,1))
# Night and Day PCoA -------------------------------------------------------
pdf("obs_noctpcoa.pdf",
    width =5,
    height = 15)
par(mfrow=c(3,1))
#Diet
noctdiet_pcoa <- 
  cmdscale(vegdist(noctdiet[,6:15],
                   distance = "bray"),
           k=2,
           eig = T)
##plot
col <- 
  ifelse(noctdiet$Bromeliads == "present",
         "saddlebrown", 
         "darkgreen")
pch <-
  ifelse(noctdiet$Time == "Day",
         18, 
         17)

ordiplot(noctdiet_pcoa,
         type = "n")
title("a",
      adj =0)
points(noctdiet_pcoa$points,
       pch = pch,
       col=col)

legend ("topright", 
        legend = c("Bromeliads present",
                   "Bromeliads absent"), 
        pch = 15, 
        pt.bg = c("saddlebrown", "darkgreen"), 
        col = c("saddlebrown", "darkgreen"))
legend ("topleft", 
        legend = c("Day",
                   "Night"), 
        pch = c(18,17), 
        pt.bg = "black", 
        col = "black")

#kind
noctkind_pcoa <- 
  cmdscale(vegdist(noctkind[,6:33],
                           distance = "bray"),
                   k=2,
                   eig = T)
##plot
col <- 
  ifelse(noctkind$Bromeliads == "present",
         "saddlebrown", 
         "darkgreen")
pch <-
  ifelse(noctkind$Time == "Day",
         18, 
         17)

ordiplot(noctkind_pcoa,
         type = "n")
title("b",
      adj =0)
points(noctkind_pcoa$points,
       pch = pch,
       col=col)

#Behaviour frequency
noctbehavfreq_pcoa <- 
  cmdscale(vegdist(spread_noctbehavfreq[,6:12],
                           distance = "bray"),
           k=2,
           eig = T)
noctbehavfreq_pcoa <- 
  add.spec.scores(noctbehavfreq_pcoa,
                  spread_noctbehavfreq[,6:12],
                  method="wa.scores",
                  multi=1,
                  Rscale=F)

##plot
col <- 
  ifelse(spread_noctbehavfreq$Bromeliads == "present",
         "saddlebrown", 
         "darkgreen")
pch <-
  ifelse(spread_noctbehavfreq$Time == "Day",
         18, 
         17)

ordiplot(noctbehavfreq_pcoa,
         type = "n")
title("c",
      adj =0)

points(noctbehavfreq_pcoa$points,
       pch = pch,
       col=col)
orditorp(noctbehavfreq_pcoa$cproj,
         display = "species",
         pcex = 0,
         cex = 1,
         labels = c("Herbivory", "Mobile", "Predation/Parasitism", "Reproduction",
                    "Stationary", "Tending", "Transporting"),
         air = 0.01)


dev.off()
par(mfrow = c(1,1))

# Adonis frames -----------------------------------------------------------
#Diet and kind
##CP
write.csv(
  rbind(obsadoniscp_bromdiet$aov.tab,
        obsadoniscp_brompreddiet$aov.tab,
        obsadoniscp_bromkind$aov.tab,
        obsadoniscp_brompredkind$aov.tab,
        obsadoniscp_bromkind_sub$aov.tab,
        obsadoniscp_brompredkind_sub$aov.tab),
  "cpdietkind_adonis.csv")
##DO
write.csv(
  rbind(obsadonisdo_bromdiet$aov.tab,
        obsadonisdo_brompreddiet$aov.tab,
        obsadonisdo_bromkind$aov.tab,
        obsadonisdo_brompredkind$aov.tab,
        obsadonisdo_bromkind_sub$aov.tab,
        obsadonisdo_brompredkind_sub$aov.tab),
  "dodietkind_adonis.csv")

#Behaviour
write.csv(
  rbind(obsadonis_behavdurcp_brom$aov.tab,
        obsadonis_behavdurcp_brompred$aov.tab,
        obsadonis_behavfreqcp_brom$aov.tab,
        obsadonis_behavfreqcp_brompred$aov.tab,
        obsadonis_behavdurdo_brom$aov.tab,
        obsadonis_behavdurdo_brompred$aov.tab,
        obsadonis_behavfreqdo_brom$aov.tab,
        obsadonis_behavfreqdo_brompred$aov.tab),
  "cpdobehavdurfreq_adonis.csv")

#Night and Day
write.csv(
  rbind(noctadonis_bromdiet$aov.tab,
        noctadonis_brompreddiet$aov.tab,
        noctadonis_bromkind$aov.tab,
        noctadonis_brompredkind$aov.tab,
        noctadonis_bromkind_sub$aov.tab,
        noctadonis_brompredkind_sub$aov.tab,
        noctadonis_brombehavdur$aov.tab, 
        noctadonis_brompredbehavdur$aov.tab, 
        noctadonis_brombehavfreq$aov.tab,
        noctadonis_brompredbehavfreq$aov.tab),
  "noctadonis.csv")




# Testing by date --------------------------------------------------------
datemodelcp_pred <- 
  glmer.nb(number ~
          Day +
          (1|block/alltrees),
        control = glmerControl(optimizer ="bobyqa"),
        data = obspred %>% 
          filter(Site =="CP"))
simulationOutput <- 
  simulateResiduals(fittedModel =datemodelcp_pred, 
                    n = 2000,
                    rank = T)
plot(simulationOutput,
     asFactor =T,
     quantreg = F)


datedietcp <-
  observation %>%
  filter(Site =="CP" & 
           block %in% c("A", "B", "C")) %>% 
  dplyr::select(Day, alltrees, block, rep, Observer, obs, Bromeliads, Diet, Specimen,number) %>% 
  group_by(Day, alltrees, block, rep, Observer, obs, Bromeliads, Diet, Specimen) %>% 
  summarise_all(funs(mean)) %>% 
  ungroup() %>% 
  dplyr::select(-Specimen, -Observer, -obs) %>% 
  group_by(Day, alltrees, block, rep, Bromeliads, Diet) %>%
  summarise_all(funs(sum)) %>% 
  spread(key = Diet, number, fill =0)
datedietcp <- 
  datedietcp %>% 
  left_join(calc_obsbrompred)
datedietcp$brompred[is.na(datedietcp$brompred)] <- 
  0
#model
datemodelcp_brompred <- 
  glmer.nb(herb ~
          log(Day) +
          (1|block/alltrees),
        family = "poisson"(link="inverse"),
        control = glmerControl(optimizer = "bobyqa"),
        data = datedietcp %>% 
          filter(herb<200))
simulationOutput <- 
  simulateResiduals(fittedModel =datemodelcp_brompred, 

                    n = 2000,
                    rank = T)
plot(simulationOutput,
     asFactor =T,
     quantreg = F)
datetestcp_pred <- 
  mixed(number ~ 
          Bromeliads +
          (1|block/alltrees),
        data =obspred %>% filter(Site == "CP"),
        family = "negative.binomial"(theta = getME(obsmodelcp_pred,
                                                   "glmer.nb.theta")),
        control =glmerControl(optimizer = "bobyqa"),
        method = "LRT")$anova_table


