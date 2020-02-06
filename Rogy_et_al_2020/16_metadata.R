#Metadata
#Load Packages
library(devtools)
library("EML")
library(dplyr)

#Set list of columns, with definitions and details
attributes <-
  data.frame(
    attributeName = c(
      "Site",
      "block",
      "rep",
      "Day",
      "Time",
      "Observer",
      "Tree",
      "Bromeliads",
      "obs",
      "Order",
      "Suborder",
      "Family",
      "cleanspec",
      "Specimen",
      "Diet",
      "behaviour",
      "Behavior_Duration",
      "interaction",
      "Interaction_Target",
      "Interaction_Duration",
      "Interaction_Outcome"), 
    attributeDefinition = c(
      "Site ID",
      "Block or group ID (NB: block X includes extra trees, not part of block or group)",
      "Repetition of observation on block",
      "Day of observation",
      "Observation during the day or at night",
      "Name of observer",
      "Tree ID",
      "Presence of bromeliads in the tree",
      "Observation ID",
      "Order of morphospecies",
      "Suborder of morphospecies",
      "Family of morphospecies",
      "Name of morphospecies",
      "Individual specimen in each observation",
      "Diet of morphospecies",
      "Behaviour of specimen",
      "Duration of observed behaviour",
      "Type of interspecific interaction",
      "Target of interspecific interaction",
      "Duration of interspecific interaction",
      "Outcome of interspecific interaction"),
    formatString = c(
      NA,
      NA,
      NA,
      "dd.mm.yyyy",
      NA,
      NA,
      NA,
      NA,
      NA,
      NA,
      NA,
      NA,
      NA,
      NA,
      NA,
      NA,
      NA,
      NA,
      NA,
      NA,
      NA),
    definition = c(        
      NA,
      NA,
      NA,
      NA,
      NA,
      NA,
      NA,
      NA,
      NA,
      NA,
      NA,
      NA,
      NA,
      NA,
      NA,
      NA,
      NA,
      NA,
      NA,
      NA,
      NA),
    unit = c(
      NA,
      NA,
      NA,
      NA,
      NA,
      NA,
      NA,
      NA,
      NA,
      NA,
      NA,
      NA,
      NA,
      NA,
      NA,
      NA,
      "second",
      NA,
      NA,
      "second",
      NA),
    numberType = c(
      NA,
      NA,
      NA,
      NA,
      NA,
      NA,
      NA,
      NA,
      NA,
      NA,
      NA,
      NA,
      NA,
      NA,
      NA,
      NA,
      "integer",
      NA,
      NA,
      "integer",
      NA),
    stringsAsFactors = FALSE
  )

#Merge the two dataframes and compute column types
attributeList <-
  set_attributes(attributes, 
                 col_classes = c("character",
                                 "character",
                                 "character",
                                 "character",
                                 "character",
                                 "character",
                                 "character",
                                 "character",
                                 "character",
                                 "character",
                                 "character",
                                 "character",
                                 "character",
                                 "character",
                                 "character",
                                 "character",
                                 "numeric",
                                 "character",
                                 "character",
                                 "numeric",
                                 "character"))

#Create location for pre-XML CSV, then bind them
physical <- 
  set_physical("observation.csv")

dataTable <- 
  new("dataTable",
      entityName = "observation.csv",
      entityDescription = "Observational survey data",
      physical = physical,
      attributeList = attributeList)

#Add personal info
R_person <-
  as.person("Pierre Rogy <rogy@zoology.ubc.ca>")
Pierre <-
  as(R_person, "creator")

#Add title and abstract
title <- 
  "Bromeliads affect the interactions and composition of invertebrates on their support tree"
abstract <- 
  "Individual species can have profound effects on ecological communities, but, in hyperdiverse systems, it can be challenging to determine the underlying ecological mechanisms. Simplifying species' responses by trophic level or functional group may be useful, but characterizing the trait structure of communities may be better related to niche processes. A largely overlooked trait in such community-level analyses is behaviour. In the Neotropics, epiphytic tank bromeliads (Bromeliaceae) harbour a distinct fauna of terrestrial invertebrates that is mainly composed of predators, such as ants and spiders. As these bromeliad-associated predators tend to forage on the bromeliads' support tree, they may influence the arboreal invertebrate fauna. We examined how, by increasing associated predator habitat, bromeliads may affect arboreal invertebrates. Specifically, we observed the trophic and functional group composition, and the behaviour and interspecific interactions of arboreal invertebrates in trees with and without bromeliads. Bromeliads modified the functional composition of arboreal invertebrates, but not the overall abundance of predators and herbivores. Bromeliads did not alter the overall behavioural profile of arboreal invertebrates, but did lead to more positive interactions in the day than at night, with a reverse pattern on trees without bromeliads. In particular, tending behaviours were influenced by bromeliad-associated predators. These results indicate that detailed examination of the functional affiliations and behaviour of organisms can reveal complex effects of habitat-forming species like bromeliads, even when total densities of trophic groups are insensitive."

#Merge everything
dataset <- 
  new("dataset",
      title = title,
      creator = Pierre,
      abstract = abstract,
      dataTable = dataTable)

#Write the XML file
write_eml(dataset,
          file = "observation.xml")

