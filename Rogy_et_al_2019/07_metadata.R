#Metadata
#Load Packages
library(devtools)
devtools::install_github("ropensci/EML", build_vignettes = TRUE)
library("EML")
library(dplyr)

# Distance ----------------------------------------------------------------
#Set list of columns, with definitions and details
attributes <-
  data.frame(
    attributeName = c(
      "Site",
      "Sampling",
      "Day",
      "Tree",
      "Treatment",
      "Square",
      "Bromeliad_#",
      "Distance",
      "Diameter",
      "Longest_leaf_length"), 
    attributeDefinition = c(
      "Site ID",
      "Sampling period",
      "Day of collection",
      "Tree ID",
      "More appropriately called 'tree type', if bromeliads were naturally present, absent, or removed after the first sampling period",
      "In roman numerals, quadrat ID",
      "Bromeliad ID",
      "Distance from the bromeliad to the center of the quadrat",
      "Diameter of the central well of the bromeliad",
      "Length of the longest leaf of the bromeliad"),
    formatString = c(
      NA,        
      NA,         
      "dd.mm.yyyy",         
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
      NA),
    unit = c(
      NA,        
      NA,         
      NA,         
      NA,
      NA,        
      NA,         
      NA,         
      "centimeter",
      "centimeter",
      "centimeter"),
    numberType = c(
      NA,
      NA,
      NA,
      NA,
      NA,
      NA,
      NA,
      "decimal",
      "decimal",
      "decimal"),
    stringsAsFactors = FALSE
  )

#Create vectors for the factors levels
Site <- 
  c(CP = "One of the individually owned plantations, named after the owners Calixto and Petrona Moraga-Ríos",
    DO = "The commercial plantation, property of Del Oro S.A.",
    ER = "One of the individually owned plantations, named after the owner Ernesto Rodriguez")
Sampling <- 
  c(A = "After sampling period (wet season)",
    B = "Before sampling period (dry season)")
Treatment <- 
  c(w = "Trees naturally with bromeliads in both sampling periods",
    wo = "Trees naturally without bromeliads in both sampling periods",
    wr = "Trees naturally with bromeliads in the 'Before' sampling period, bromliads subsequently removed (manipulative treatment)")

#Bind the factor vectors
factors <- rbind(
  data.frame(
    attributeName = "Site",
    code = names(Site),
    definition = unname(Site)),
  data.frame(
    attributeName = "Sampling",
    code = names(Sampling),
    definition = unname(Sampling)),
  data.frame(
    attributeName = "Treatment",
    code = names(Treatment),
    definition = unname(Treatment)))

#Merge the two dataframes and compute column types
attributeList <-
  set_attributes(attributes, 
                 factors, 
                 col_classes = c("factor", 
                                 "factor", 
                                 "Date", 
                                 "character", 
                                 "factor",
                                 "character",
                                 "character",
                                 "numeric",
                                 "numeric",
                                 "numeric"))

#Create location for pre-XML CSV, then bind them
physical <- 
  set_physical("distance.csv")

dataTable <- 
  new("dataTable",
      entityName = "distance.csv",
      entityDescription = "Bromeliad to quadrat distance file",
      physical = physical,
      attributeList = attributeList)

#Add personal info
R_person <-
  as.person("Pierre Rogy <rogy@zoology.ubc.ca>")
Pierre <-
  as(R_person, "creator")

#Add title and abstract
title <- 
  "Epiphytic bromeliads indirectly affect invertebrate food webs on their support tree"
abstract <- 
  "Ecosystem engineers are species that affect others through the provision of habitat rather than consumptive resources. The extent to which ecosystem engineers can indirectly affect entire food webs, however, is poorly understood. Epiphytic tank bromeliads (Bromeliaceae) are ecosystem engineers that are common throughout the Neotropics, and are associated with a variety of predatory arthropods. Here, we examine if bromeliads, by increasing predator densities, indirectly benefit their support tree through reduction in herbivorous insects and leaf damage. We observed and manipulated bromeliad densities in Costa Rican orange orchards, and measured impacts on leaf damage and arboreal and bromeliad invertebrate communities in two different seasons. Our results show that bromeliads are associated with predatory and herbivorous invertebrates but not leaf damage. Bromeliads were correlated with increased densities of their associated predators, especially ants and hunting spiders, but we could not confirm a causal link. Associations with bromeliads changed over time, with seasonal shifts interfering with responses to our manipulations. Bromeliads had a reduced association with predators in the dry season, and a null association between bromeliads and herbivorous invertebrates in the dry season unexpectedly became positive in the wet season. In summary, we have only limited evidence that bromeliads indirectly promote the top-down control of arboreal herbivores; instead, our manipulations suggest that bromeliads increase herbivore densities in the wet season. This research suggests that although bromeliads may act as ecosystem engineers, indirectly influencing the invertebrate food web on support trees, their effects are trophically complex and seasonally dependent."

#Merge everything
dataset <- 
  new("dataset",
      title = title,
      creator = Pierre,
      abstract = abstract,
      dataTable = dataTable)

#Write the XML file
write_eml(dataset,
          file = "distance.xml")

# Leaf damage ----------------------------------------------------------------
#Set list of columns, with definitions and details
attributes <-
  data.frame(
    attributeName = c(
      "Analyst",
      "Site",
      "Sampling",
      "Day",
      "Tree",
      "Treatment",
      "Square",
      "Leaf",
      "Original_area",
      "Actal_area",
      "Miner",
      "Hemiptera"), 
    attributeDefinition = c(
      "Name of analyst",
      "Site ID",
      "Sampling period",
      "Day of collection",
      "Tree ID",
      "More appropriately called 'tree type', if bromeliads were naturally present, absent, or removed after the first sampling period",
      "In roman numerals, quadrat ID",
      "Leaf ID",
      "Estimated area of the leaf",
      "Original area minus area removed by leaf chewers",
      "Area damaged by leaf miners",
      "Area damaged by sap feeders"),
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
      NA),
    unit = c(
      NA,        
      NA,         
      NA,         
      NA,
      NA,
      NA,
      NA,        
      "dimensionless",         
      "squareCentimeters",         
      "squareCentimeters",
      "squareCentimeters",
      "squareCentimeters"),
    numberType = c(
      NA,
      NA,
      NA,
      NA,
      NA,
      NA,
      NA,
      "integer",
      "decimal",
      "decimal",
      "decimal",
      "decimal"),
    stringsAsFactors = FALSE
  )

#Create vectors for the factors levels
Site <- 
  c(CP = "One of the individually owned plantations, named after the owners Calixto and Petrona Moraga-Ríos",
    DO = "The commercial plantation, property of Del Oro S.A.",
    ER = "One of the individually owned plantations, named after the owner Ernesto Rodriguez")
Sampling <- 
  c(A = "After sampling period (wet season)",
    B = "Before sampling period (dry season)")
Treatment <- 
  c(w = "Trees naturally with bromeliads in both sampling periods",
    wo = "Trees naturally without bromeliads in both sampling periods",
    wr = "Trees naturally with bromeliads in the 'Before' sampling period, bromliads subsequently removed (manipulative treatment)")

#Bind the factor vectors
factors <- rbind(
  data.frame(
    attributeName = "Site",
    code = names(Site),
    definition = unname(Site)),
  data.frame(
    attributeName = "Sampling",
    code = names(Sampling),
    definition = unname(Sampling)),
  data.frame(
    attributeName = "Treatment",
    code = names(Treatment),
    definition = unname(Treatment)))

#Merge the two dataframes and compute column types
attributeList <-
  set_attributes(attributes, 
                 factors, 
                 col_classes = c("character",
                                 "factor", 
                                 "factor", 
                                 "Date", 
                                 "character", 
                                 "factor",
                                 "character",
                                 "character",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric"))

#Create location for pre-XML CSV, then bind them
physical <- 
  set_physical("leafdamage.csv")

dataTable <- 
  new("dataTable",
      entityName = "leafdamage.csv",
      entityDescription = "Leaf damage file",
      physical = physical,
      attributeList = attributeList)

#Add personal info
R_person <-
  as.person("Pierre Rogy <rogy@zoology.ubc.ca>")
Pierre <-
  as(R_person, "creator")

#Add title and abstract
title <- 
  "Epiphytic bromeliads indirectly affect invertebrate food webs on their support tree"
abstract <- 
  "Ecosystem engineers are species that affect others through the provision of habitat rather than consumptive resources. The extent to which ecosystem engineers can indirectly affect entire food webs, however, is poorly understood. Epiphytic tank bromeliads (Bromeliaceae) are ecosystem engineers that are common throughout the Neotropics, and are associated with a variety of predatory arthropods. Here, we examine if bromeliads, by increasing predator densities, indirectly benefit their support tree through reduction in herbivorous insects and leaf damage. We observed and manipulated bromeliad densities in Costa Rican orange orchards, and measured impacts on leaf damage and arboreal and bromeliad invertebrate communities in two different seasons. Our results show that bromeliads are associated with predatory and herbivorous invertebrates but not leaf damage. Bromeliads were correlated with increased densities of their associated predators, especially ants and hunting spiders, but we could not confirm a causal link. Associations with bromeliads changed over time, with seasonal shifts interfering with responses to our manipulations. Bromeliads had a reduced association with predators in the dry season, and a null association between bromeliads and herbivorous invertebrates in the dry season unexpectedly became positive in the wet season. In summary, we have only limited evidence that bromeliads indirectly promote the top-down control of arboreal herbivores; instead, our manipulations suggest that bromeliads increase herbivore densities in the wet season. This research suggests that although bromeliads may act as ecosystem engineers, indirectly influencing the invertebrate food web on support trees, their effects are trophically complex and seasonally dependent."

#Merge everything
dataset <- 
  new("dataset",
      title = title,
      creator = Pierre,
      abstract = abstract,
      dataTable = dataTable)

#Write the XML file
write_eml(dataset,
          file = "leafdamage.xml")

# Vacuum ----------------------------------------------------------------
#Set list of columns, with definitions and details
attributes <-
  data.frame(
    attributeName = c(
      "Subphylum",
      "Order",
      "Suborder",
      "Family",
      "Subfamily",
      "Genus",
      "Species",
      "Morphopecies",
      "Species_old_name",
      "Diet",
      "Site",
      "Sampling",
      "Day",
      "Tree",
      "Square",
      "Treatment",
      "Abundance"), 
    attributeDefinition = c(
      "Subphylum of the morphospecies",
      "Order of the morphospecies",
      "Suborder of the morphospecies",
      "Family of the morphospecies",
      "Subfamily of the morphospecies",
      "Genus of the morphospecies",
      "Binomial species name",
      "Current morphospecies ID",
      "Original name of the morphospecies, includes duplicates",
      "Diet of the morphospecies",
      "Site ID",
      "Sampling period",
      "Day of collection",
      "Tree ID",
      "In roman numerals, quadrat ID",
      "More appropriately called 'tree type', if bromeliads were naturally present, absent, or removed after the first sampling period",
      "Number of collected specimens"),
    formatString = c(
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
      "dd.mm.yyyy",  
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
      "dimensionless"),
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
      "integer"),
    stringsAsFactors = FALSE
  )

#Create vectors for the factors levels
Site <- 
  c(CP = "One of the individually owned plantations, named after the owners Calixto and Petrona Moraga-Ríos",
    DO = "The commercial plantation, property of Del Oro S.A.",
    ER = "One of the individually owned plantations, named after the owner Ernesto Rodriguez")
Sampling <- 
  c(A = "After sampling period (wet season)",
    B = "Before sampling period (dry season)")
Treatment <- 
  c(w = "Trees naturally with bromeliads in both sampling periods",
    wo = "Trees naturally without bromeliads in both sampling periods",
    wr = "Trees naturally with bromeliads in the 'Before' sampling period, bromliads subsequently removed (manipulative treatment)")
Diet <- 
  c(detr = "Detritivore",
    gran = "Seed predator",
    hema = "Blood feeder",
    herb = "Herbivore",
    klep = "Kleptoparasite",
    myco = "Mycophagous",
    nada = "Non-feeder",
    nect = "Nectar feeder, later collapsed with poll and poll/nect",
    omni = "Omnivore",
    poll = "Palynivore, later collapsed with nect and poll/nect",
    'poll/nect' = "Nectar feeder and palynivore, later collapsed with poll and nect",
    pred = "Predator",
    scav = "Scavenger, chiefly cockroaches",
    unkn = "Unknown feeding behaviour",
    xylo = "Xylophagous")

#Bind the factor vectors
factors <- rbind(
  data.frame(
    attributeName = "Site",
    code = names(Site),
    definition = unname(Site)),
  data.frame(
    attributeName = "Sampling",
    code = names(Sampling),
    definition = unname(Sampling)),
  data.frame(
    attributeName = "Treatment",
    code = names(Treatment),
    definition = unname(Treatment)),
  data.frame(
    attributeName = "Diet",
    code = names(Diet),
    definition = unname(Diet)))

#Merge the two dataframes and compute column types
attributeList <-
  set_attributes(attributes, 
                 factors, 
                 col_classes = c("character",
                                 "character",
                                 "character",
                                 "character",
                                 "character",
                                 "character",
                                 "character",
                                 "character",
                                 "character",
                                 "factor",
                                 "factor", 
                                 "factor", 
                                 "Date", 
                                 "character", 
                                 "character",
                                 "factor",
                                 "numeric"))

#Create location for pre-XML CSV, then bind them
physical <- 
  set_physical("vacuum.csv")

dataTable <- 
  new("dataTable",
      entityName = "vacuum.csv",
      entityDescription = "Vacuum samples",
      physical = physical,
      attributeList = attributeList)

#Add personal info
R_person <-
  as.person("Pierre Rogy <rogy@zoology.ubc.ca>")
Pierre <-
  as(R_person, "creator")

#Add title and abstract
title <- 
  "Epiphytic bromeliads indirectly affect invertebrate food webs on their support tree"
abstract <- 
  "Ecosystem engineers are species that affect others through the provision of habitat rather than consumptive resources. The extent to which ecosystem engineers can indirectly affect entire food webs, however, is poorly understood. Epiphytic tank bromeliads (Bromeliaceae) are ecosystem engineers that are common throughout the Neotropics, and are associated with a variety of predatory arthropods. Here, we examine if bromeliads, by increasing predator densities, indirectly benefit their support tree through reduction in herbivorous insects and leaf damage. We observed and manipulated bromeliad densities in Costa Rican orange orchards, and measured impacts on leaf damage and arboreal and bromeliad invertebrate communities in two different seasons. Our results show that bromeliads are associated with predatory and herbivorous invertebrates but not leaf damage. Bromeliads were correlated with increased densities of their associated predators, especially ants and hunting spiders, but we could not confirm a causal link. Associations with bromeliads changed over time, with seasonal shifts interfering with responses to our manipulations. Bromeliads had a reduced association with predators in the dry season, and a null association between bromeliads and herbivorous invertebrates in the dry season unexpectedly became positive in the wet season. In summary, we have only limited evidence that bromeliads indirectly promote the top-down control of arboreal herbivores; instead, our manipulations suggest that bromeliads increase herbivore densities in the wet season. This research suggests that although bromeliads may act as ecosystem engineers, indirectly influencing the invertebrate food web on support trees, their effects are trophically complex and seasonally dependent."

#Merge everything
dataset <- 
  new("dataset",
      title = title,
      creator = Pierre,
      abstract = abstract,
      dataTable = dataTable)

#Write the XML file
write_eml(dataset,
          file = "vacuum.xml")

# Dissection ----------------------------------------------------------------
#Set list of columns, with definitions and details
attributes <-
  data.frame(
    attributeName = c(
      "Subphylum",
      "Order",
      "Suborder",
      "Family",
      "Subfamily",
      "Genus",
      "Species",
      "Morphopecies",
      "Species_old_name",
      "Diet",
      "Site",
      "Sampling",
      "Day",
      "Tree",
      "Treatment",
      "Bromeliad",
      "Abundance"), 
    attributeDefinition = c(
      "Subphylum of the morphospecies",
      "Order of the morphospecies",
      "Suborder of the morphospecies",
      "Family of the morphospecies",
      "Subfamily of the morphospecies",
      "Genus of the morphospecies",
      "Binomial species name",
      "Current morphospecies ID",
      "Original name of the morphospecies, includes duplicates",
      "Diet of the morphospecies",
      "Site ID",
      "Sampling period",
      "Day of collection",
      "Tree ID",
      "More appropriately called 'tree type', if bromeliads were naturally present, absent, or removed after the first sampling period",
      "Bromeliad ID",
      "Number of collected specimens"),
    formatString = c(
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
      "dd.mm.yyyy",  
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
      "dimensionless"),
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
      "integer"),
    stringsAsFactors = FALSE
  )

#Create vectors for the factors levels
Site <- 
  c(CP = "One of the individually owned plantations, named after the owners Calixto and Petrona Moraga-Ríos",
    DO = "The commercial plantation, property of Del Oro S.A.",
    ER = "One of the individually owned plantations, named after the owner Ernesto Rodriguez")
Sampling <- 
  c(A = "After sampling period (wet season)",
    B = "Before sampling period (dry season)")
Treatment <- 
  c(w = "Trees naturally with bromeliads in both sampling periods",
    wo = "Trees naturally without bromeliads in both sampling periods",
    wr = "Trees naturally with bromeliads in the 'Before' sampling period, bromliads subsequently removed (manipulative treatment)")
Diet <- 
  c(detr = "Detritivore",
    gran = "Seed predator",
    hema = "Blood feeder",
    herb = "Herbivore",
    klep = "Kleptoparasite",
    myco = "Mycophagous",
    nada = "Non-feeder",
    nect = "Nectar feeder, later collapsed with poll and poll/nect",
    omni = "Omnivore",
    poll = "Palynivore, later collapsed with nect and poll/nect",
    'poll/nect' = "Nectar feeder and palynivore, later collapsed with poll and nect",
    pred = "Predator",
    scav = "Scavenger, chiefly cockroaches",
    unkn = "Unknown feeding behaviour",
    xylo = "Xylophagous")

#Bind the factor vectors
factors <- rbind(
  data.frame(
    attributeName = "Site",
    code = names(Site),
    definition = unname(Site)),
  data.frame(
    attributeName = "Sampling",
    code = names(Sampling),
    definition = unname(Sampling)),
  data.frame(
    attributeName = "Treatment",
    code = names(Treatment),
    definition = unname(Treatment)),
  data.frame(
    attributeName = "Diet",
    code = names(Diet),
    definition = unname(Diet)))

#Merge the two dataframes and compute column types
attributeList <-
  set_attributes(attributes, 
                 factors, 
                 col_classes = c("character",
                                 "character",
                                 "character",
                                 "character",
                                 "character",
                                 "character",
                                 "character",
                                 "character",
                                 "character",
                                 "factor",
                                 "factor", 
                                 "factor", 
                                 "Date", 
                                 "character", 
                                 "factor",
                                 "character",
                                 "numeric"))

#Create location for pre-XML CSV, then bind them
physical <- 
  set_physical("dissection.csv")

dataTable <- 
  new("dataTable",
      entityName = "dissection.csv",
      entityDescription = "Dissection samples",
      physical = physical,
      attributeList = attributeList)

#Add personal info
R_person <-
  as.person("Pierre Rogy <rogy@zoology.ubc.ca>")
Pierre <-
  as(R_person, "creator")

#Add title and abstract
title <- 
  "Epiphytic bromeliads indirectly affect invertebrate food webs on their support tree"
abstract <- 
  "Ecosystem engineers are species that affect others through the provision of habitat rather than consumptive resources. The extent to which ecosystem engineers can indirectly affect entire food webs, however, is poorly understood. Epiphytic tank bromeliads (Bromeliaceae) are ecosystem engineers that are common throughout the Neotropics, and are associated with a variety of predatory arthropods. Here, we examine if bromeliads, by increasing predator densities, indirectly benefit their support tree through reduction in herbivorous insects and leaf damage. We observed and manipulated bromeliad densities in Costa Rican orange orchards, and measured impacts on leaf damage and arboreal and bromeliad invertebrate communities in two different seasons. Our results show that bromeliads are associated with predatory and herbivorous invertebrates but not leaf damage. Bromeliads were correlated with increased densities of their associated predators, especially ants and hunting spiders, but we could not confirm a causal link. Associations with bromeliads changed over time, with seasonal shifts interfering with responses to our manipulations. Bromeliads had a reduced association with predators in the dry season, and a null association between bromeliads and herbivorous invertebrates in the dry season unexpectedly became positive in the wet season. In summary, we have only limited evidence that bromeliads indirectly promote the top-down control of arboreal herbivores; instead, our manipulations suggest that bromeliads increase herbivore densities in the wet season. This research suggests that although bromeliads may act as ecosystem engineers, indirectly influencing the invertebrate food web on support trees, their effects are trophically complex and seasonally dependent."

#Merge everything
dataset <- 
  new("dataset",
      title = title,
      creator = Pierre,
      abstract = abstract,
      dataTable = dataTable)

#Write the XML file
write_eml(dataset,
          file = "dissection.xml")
