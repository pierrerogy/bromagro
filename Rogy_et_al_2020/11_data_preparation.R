#Data preparation
##Reminder: Run Script #01 (in Rogy_et_al_2019) beforehand
library(BiodiversityR)
library(scales)
#Read data
observation <- 
  read.csv("Data/observation.csv",
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

#Remove mosquitoes
observation <- 
  observation %>% 
  filter(Family != "Culicidae")

#Convert date to day number
##Enter date format
observation$Day <- 
  as.Date(observation$Day,
          format='%m-%d-%Y',
          origin = lubridate::origin)
##Convert to day number
observation$Day <- 
  lubridate::yday(observation$Day)

#MAdd column for predators and potential predators
##Overall 
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
###Add column
obspred <-
  unique(observation %>% 
           dplyr::select(Day, Site, alltrees, block, rep, Bromeliads)) %>% 
  left_join(obspred)
###Remove NAs
obspred$number[is.na(obspred$number)] <- 
  0
##Night/Day
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
###Add column
noctobspred <- 
  unique(observation %>% 
           filter(Site =="CP") %>% 
           dplyr::select(alltrees, block, rep, Time, Bromeliads)) %>%
  left_join(noctobspred)
###Remove NAs
noctobspred$number[is.na(noctobspred$number)] <- 
  0

#Add column for bromeliad predators
observation <- 
  observation %>% 
  left_join(calc_bromstuff %>% 
              rename(Species = Morphospecies))
##Compute abundance column
###Overall
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
###Double check
str(observation)
str(calc_obsbrompred)

#Make column to filter out bromeliad predators later on
observation$isolation <- 
  ifelse(observation$where == "brom" &
           observation$Diet %in% c("pred","omni","scav") &
           observation$Family %notin% c("Gryllidae", "Anthicidae", "Aderidae", "Tenebrionidae"),
         "yes",
         NA)

# Adding kinds ------------------------------------------------------------
#Add dummy column
observation$kind <- 
  NA
#Add categories, based on taxonomy/behavior
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

# Contrast between leaf chewers and phloem feeders ---------------------
#Make new data frame
observation_rev <- 
  observation
#Change diet string
observation_rev$Diet <- 
  ifelse(observation_rev$Diet =="herb" & 
           observation_rev$kind %in% c("herbeetle", "ortho", "lepi", "snail") | 
           observation_rev$cleanspec == "NewSp_leaf_cutter_ant", "chewer",
         ifelse(observation_rev$Diet =="herb" & 
                  observation_rev$kind %in% c("scaleaphid", "jump", "heteroherb") |
                  observation_rev$kind == "Sternorrhyncha", "phloem",
                ifelse(observation_rev$kind == "miner", "miner",
                       observation_rev$Diet
                )))
#Change behaviour string
observation_rev$behaviour <- 
  ifelse(observation_rev$Diet %in% c("chewer", "poll") &
           observation_rev$behaviour == "herb_feeding", "leaf_chewing",
         ifelse(observation_rev$Diet == "phloem" &
                  observation_rev$behaviour == "herb_feeding", "phloem_sucking",
                ifelse(observation$kind == "miner", "mining",
                       ifelse(observation_rev$behaviour == "herb_feeding" & 
                                observation_rev$Diet %in% c("detr", "unkn"), "detr_feeding", 
                              observation_rev$behaviour))))

# Dietary and taxonomic group frames for bromeliad-level analysis ---------------------------------------------------------
#Dietary groups
##CP
obsdietcp <-
  observation %>%
  ###Keep site and correct replicates
  filter(Site =="CP" & 
           block %in% c("A", "B", "C")) %>% 
  ###Select relevant columns
  dplyr::select(alltrees, block, rep, Observer, obs, Bromeliads, Diet, Specimen,number) %>% 
  ###Group by specimen and average
  ###Specimens could have more than one bahviour in a single observation
  group_by(alltrees, block, rep, Observer, obs, Bromeliads, Diet, Specimen) %>% 
  summarise_all(funs(mean)) %>% 
  ###Ungroup to pool differently later on
  ungroup() %>%
  ###Sum per tree per replicate
  dplyr::select(-Specimen, -Observer, -obs) %>% 
  group_by(alltrees, block, rep, Bromeliads, Diet) %>%
  summarise_all(funs(sum)) %>% 
  spread(key = Diet, number, fill =0)
###Remove low abundance diets
colSums(obsdietcp[,5:17])
obsdietcp <- 
  obsdietcp %>% 
  dplyr::select(-gran,-poll,-xylo)
###Merge
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
###Merge
obsdietdo <- 
  obsdietdo %>% 
  left_join(calc_obsbrompred[,-2])
obsdietdo$brompred[is.na(obsdietdo$brompred)] <- 
  0

#All taxonomic groups
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
###Remove low abundance groups
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
###Remove low abundance groups
colSums(obskinddo[,4:39])
obskinddo <- 
  obskinddo %>% 
  dplyr::select(-Apocrita, -Coleoptera, -Hemiptera, -Heteroptera, -lacewings,
                -Lepidoptera, -mantid, -opilio, -Orthoptera, -Polyphaga,
                -predflies, -Sternorrhyncha, -Thysanoptera)

#Restricted taxonomic groups
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
###Remove low abundance groups
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
###Remove low abundance groups
colSums(obskinddo_sub[,4:21])
obskinddo_sub <- 
  obskinddo_sub %>% 
  dplyr::select(-lacewings, -opilio, -predflies)


# Dietary and taxonomic group frames for bromeliad-associated predator analysis ---------------------------------------------------------
#Dietary groups
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
###Merge
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
###Merge
obsdietdo_nobrompreds <- 
  obsdietdo_nobrompreds %>% 
  left_join(calc_obsbrompred)
obsdietdo_nobrompreds$brompred[is.na(obsdietdo_nobrompreds$brompred)] <- 
  0

#All taxonomic groups
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
###Remove low abundance groups
colSums(obskindcp_nobrompreds[,5:41])
obskindcp_nobrompreds <- 
  obskindcp_nobrompreds %>% 
  dplyr::select(-Diplopoda, -earwig, -Hemiptera, Heteroptera, -mantid,
                -miner, -Orthoptera, -Sternorrhyncha, -Thysanoptera, - wasps)
###Merge
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
###Remove low abundance groups
colSums(obskinddo_nobrompreds[,4:39])
obskinddo_nobrompreds <- 
  obskinddo_nobrompreds %>% 
  dplyr::select(-Apocrita, -Coleoptera, -Hemiptera, -Heteroptera, -lacewings,
                -Lepidoptera, -mantid, -opilio, -Orthoptera, -Polyphaga,
                -predflies, -Sternorrhyncha, -Thysanoptera)
###Merge
obskinddo_nobrompreds <- 
  obskinddo_nobrompreds %>% 
  left_join(calc_obsbrompred)
obskinddo_nobrompreds$brompred[is.na(obskinddo_nobrompreds$brompred)] <- 
  0

#Restricted taxonomic groups
##CP
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
###Remove low abundance groups
colSums(obskindcp_sub_nobrompreds[,5:21])
obskindcp_sub_nobrompreds <- 
  obskindcp_sub_nobrompreds %>% 
  dplyr::select(-miner)
###Merge
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
###Remove low abundance groups
colSums(obskinddo_sub_nobrompreds[,4:21])
obskinddo_sub_nobrompreds <- 
  obskinddo_sub_nobrompreds %>% 
  dplyr::select(-lacewings, -opilio, -predflies)
###Merge
obskinddo_sub_nobrompreds <- 
  obskinddo_sub_nobrompreds %>% 
  left_join(calc_obsbrompred)
obskinddo_sub_nobrompreds$brompred[is.na(obskinddo_sub_nobrompreds$brompred)] <- 
  0






# Dietary group frames with leaf chewers and phloem feeders separate ------------------------
#For bromeliads
##CP
obsdietcp_rev <-
  observation_rev %>%
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
colSums(obsdietcp_rev[,5:19])
obsdietcp_rev <- 
  obsdietcp_rev %>% 
  dplyr::select(-gran,-poll,-miner,-xylo)
##DO
obsdietdo_rev <- 
  observation_rev %>%
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
colSums(obsdietdo_rev[,4:18])
obsdietdo_rev <- 
  obsdietdo_rev %>% 
  dplyr::select(-gran, -hema, -poll,-15, -nada, -omni, -myco)

#For bromeliad-associated predators
##CP
obsdietcp_nobrompreds_rev <- 
  observation_rev %>%
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
colSums(obsdietcp_nobrompreds_rev[,5:19])
obsdietcp_nobrompreds_rev <- 
  obsdietcp_nobrompreds_rev %>% 
  dplyr::select(-gran,-poll,-miner, -xylo)
###merge
obsdietcp_nobrompreds_rev <- 
  obsdietcp_nobrompreds_rev %>% 
  left_join(calc_obsbrompred)
obsdietcp_nobrompreds_rev$brompred[is.na(obsdietcp_nobrompreds_rev$brompred)] <- 
  0
##DO
obsdietdo_nobrompreds_rev <- 
  observation_rev %>%
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
colSums(obsdietdo_nobrompreds_rev[,4:18])
obsdietdo_nobrompreds_rev <- 
  obsdietdo_nobrompreds_rev %>% 
  dplyr::select(-gran, -hema, -poll,-15, -nada, -omni, -myco)
###merge
obsdietdo_nobrompreds_rev <- 
  obsdietdo_nobrompreds_rev %>% 
  left_join(calc_obsbrompred)
obsdietdo_nobrompreds_rev$brompred[is.na(obsdietdo_nobrompreds_rev$brompred)] <- 
  0

# Behaviour and interaction frames for bromeliads -----------------------------------------
#CP
##Behaviour duration
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
###Hellinger transformation
spread_behavdurcp <- 
  data.frame(spread_behavdurcp[,1:4], 
             decostand(spread_behavdurcp[,5:11],
                       "hellinger"))
##Behaviour frequency
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
##Interactions
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


#DO
##Behaviour duration
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
###Hellinger transformation
spread_behavdurdo <- 
  data.frame(spread_behavdurdo[,1:4], 
             decostand(spread_behavdurdo[,5:14],
                       "hellinger"))
##Behaviour frequency
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
#Interactions
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


# Behaviour frames for bromeliad-associated predators -----------------------------------------
#CP  
##Behaviour duration
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
###Hellinger transformation
spread_behavdurcp_nobrompreds  <- 
  data.frame(spread_behavdurcp_nobrompreds [,1:4], 
             decostand(spread_behavdurcp_nobrompreds [,5:11],
                       "hellinger"))
###Merge
spread_behavdurcp_nobrompreds <- 
  spread_behavdurcp_nobrompreds %>% 
  left_join(calc_obsbrompred)
spread_behavdurcp_nobrompreds$brompred[is.na(spread_behavdurcp_nobrompreds$brompred)] <- 
  0
##Behaviour fequency
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
###Merge
spread_behavfreqcp_nobrompreds <- 
  spread_behavfreqcp_nobrompreds %>% 
  left_join(calc_obsbrompred)
spread_behavfreqcp_nobrompreds$brompred[is.na(spread_behavfreqcp_nobrompreds$brompred)] <- 
  0

#DO 
##Behaviour duration
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
  data.frame(spread_behavdurdo_nobrompreds [,1:4], 
             decostand(spread_behavdurdo_nobrompreds [,5:14],
                       "hellinger"))
###Merge
spread_behavdurdo_nobrompreds <- 
  spread_behavdurdo_nobrompreds %>% 
  left_join(calc_obsbrompred)
spread_behavdurdo_nobrompreds$brompred[is.na(spread_behavdurdo_nobrompreds$brompred)] <- 
  0
##Behaviour frequency
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
###Merge
spread_behavfreqdo_nobrompreds <- 
  spread_behavfreqdo_nobrompreds %>% 
  left_join(calc_obsbrompred)
spread_behavfreqdo_nobrompreds$brompred[is.na(spread_behavfreqdo_nobrompreds$brompred)] <- 
  0



# Behaviour frames with leaf chewers and phloem feeders for bromeliad-level analysis------------------------
#CP 
##Behaviour duration
spread_behavdurcp_rev <- 
  observation_rev %>% 
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
###Hellinger transformation
spread_behavdurcp_rev <- 
  data.frame(spread_behavdurcp_rev[,1:4], 
             decostand(spread_behavdurcp_rev[,5:14],
                       "hellinger"))
##Behaviour frequency
spread_behavfreqcp_rev <- 
  observation_rev %>% 
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

#DO 
##Behaviour duration
spread_behavdurdo_rev <- 
  observation_rev %>% 
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
###Hellinger transformation
spread_behavdurdo_rev <- 
  data.frame(spread_behavdurdo_rev[,1:4], 
             decostand(spread_behavdurdo_rev[,5:17],
                       "hellinger"))
##Behaviour frequency
spread_behavfreqdo_rev <- 
  observation_rev %>% 
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

# Behaviour frames with leaf chewers and phloem feeders for bromeliad-associated predator analysis --------
#CP
##Behaviour duration
spread_behavdurcp_nobrompreds_rev <- 
  observation_rev %>% 
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
###Hellinger transformation
spread_behavdurcp_nobrompreds_rev  <- 
  data.frame(spread_behavdurcp_nobrompreds_rev[,1:4], 
             decostand(spread_behavdurcp_nobrompreds_rev[,5:14],
                       "hellinger"))
###Merge
spread_behavdurcp_nobrompreds_rev <- 
  spread_behavdurcp_nobrompreds_rev %>% 
  left_join(calc_obsbrompred)
spread_behavdurcp_nobrompreds_rev$brompred[is.na(spread_behavdurcp_nobrompreds_rev$brompred)] <- 
  0
##Behaviour frequency
spread_behavfreqcp_nobrompreds_rev  <- 
  observation_rev %>% 
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
###Merge
spread_behavfreqcp_nobrompreds_rev <- 
  spread_behavfreqcp_nobrompreds_rev %>% 
  left_join(calc_obsbrompred)
spread_behavfreqcp_nobrompreds_rev$brompred[is.na(spread_behavfreqcp_nobrompreds_rev$brompred)] <- 
  0

#DO
##Behaviour duration
spread_behavdurdo_nobrompreds_rev  <- 
  observation_rev %>% 
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
###Hellinger transformation
spread_behavdurdo_nobrompreds_rev  <- 
  data.frame(spread_behavdurdo_nobrompreds_rev[,1:4], 
             decostand(spread_behavdurdo_nobrompreds_rev[,5:17],
                       "hellinger"))
###Merge
spread_behavdurdo_nobrompreds_rev <- 
  spread_behavdurdo_nobrompreds_rev %>% 
  left_join(calc_obsbrompred)
spread_behavdurdo_nobrompreds_rev$brompred[is.na(spread_behavdurdo_nobrompreds_rev$brompred)] <- 
  0
##Behaviour frequency
spread_behavfreqdo_nobrompreds_rev  <- 
  observation_rev %>% 
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
###Merge
spread_behavfreqdo_nobrompreds_rev <- 
  spread_behavfreqdo_nobrompreds_rev %>% 
  left_join(calc_obsbrompred)
spread_behavfreqdo_nobrompreds_rev$brompred[is.na(spread_behavfreqdo_nobrompreds_rev$brompred)] <- 
  0



# Diel frames for bromeliad-level analysis with all herbivores--------------------------------------------------------
#Dietary groups
##CP
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

# Diel frames for bromeliads with leaf chewers and phloem feeders--------------------------------------------------------
#Diet
noctdiet_rev <- 
  observation_rev %>% 
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
colSums(noctdiet_rev[,6:20])
noctdiet_rev <- 
  noctdiet_rev %>% 
  dplyr::select(-gran,-poll,-xylo,-miner)
###merge
noctdiet_rev <- 
  noctdiet_rev %>% 
  left_join(calc_noctobsbrompred)
noctdiet_rev$brompred[is.na(noctdiet_rev$brompred)] <- 
  0

#Behaviour 
##Duration
spread_noctbehavdur_rev <- 
  observation_rev %>% 
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
spread_noctbehavdur_rev <- 
  data.frame(spread_noctbehavdur_rev[,1:5], 
             decostand(spread_noctbehavdur_rev[,6:15],
                       "hellinger"))
##Frequency
spread_noctbehavfreq_rev <- 
  observation_rev %>% 
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



# Diel frames for bromeliads-associated predators with all herbivores --------------------------------------------------------
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



# Diel frames for bromeliads-associated predators with leaf chewers and phloem feeders --------------------------------------------------------
#Diet
noctdiet_nobrompreds_rev <- 
  observation_rev %>% 
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
colSums(noctdiet_nobrompreds_rev[,6:20])
noctdiet_nobrompreds_rev <- 
  noctdiet_nobrompreds_rev %>% 
  dplyr::select(-gran,-poll,-xylo,-miner)
###merge
noctdiet_nobrompreds_rev <- 
  noctdiet_nobrompreds_rev %>% 
  left_join(calc_noctobsbrompred)
noctdiet_nobrompreds_rev$brompred[is.na(noctdiet_nobrompreds_rev$brompred)] <- 
  0

#Behaviour 
##Duration
spread_noctbehavdur_nobrompreds_rev <- 
  observation_rev %>% 
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
spread_noctbehavdur_nobrompreds_rev <- 
  data.frame(spread_noctbehavdur_nobrompreds_rev[,1:5], 
             decostand(spread_noctbehavdur_nobrompreds_rev[,6:15],
                       "hellinger"))
###merge
spread_noctbehavdur_nobrompreds_rev<- 
  spread_noctbehavdur_nobrompreds_rev %>% 
  left_join(calc_noctobsbrompred)
spread_noctbehavdur_nobrompreds_rev$brompred[is.na(spread_noctbehavdur_nobrompreds_rev$brompred)] <- 
  0

##Frequency
spread_noctbehavfreq_nobrompreds_rev <- 
  observation_rev %>% 
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
spread_noctbehavfreq_nobrompreds_rev <- 
  spread_noctbehavfreq_nobrompreds_rev %>% 
  left_join(calc_noctobsbrompred)
spread_noctbehavfreq_nobrompreds_rev$brompred[is.na(spread_noctbehavfreq_nobrompreds_rev$brompred)] <- 
  0



