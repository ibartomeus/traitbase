library(devtools)
library(roxygen2)
library(knitr)
library(taxize) 
library(countrycode) 
library(plyr) 
library(sp) 
library(rworldmap) 
library(reshape)

#document
document()

#create the initial datasets
bee_taxonomy <- data.frame(genus = NA,
                           species = NA,
                           order = NA,
                           superfamily = NA,
                           family = NA,
                           subfamily = NA,
                           tribe = NA,
                           subgenus = NA) 
#All taxonomy tables will have the same structure regardless of the taxa.

bee_specimens <- data.frame(id = NA,
                        link_id = NA,
                        genus = NA,
                        species = NA,
                        sex = NA,
                        category = NA, #this is necesary?
                        trait = NA,
                        value = NA,
                        reference = NA,
                        credit = NA,
                        email = NA) 

bee_observations <- data.frame(id = NA,
                           link_id = NA,
                           genus = NA,
                           species = NA,
                           sex = NA,
                           interaction_type = NA,
                           partner_genus = NA,
                           partner_species = NA,
                           day = NA,
                           month = NA,
                           year = NA,
                           country = NA, #Add state for US?
                           location = NA,
                           lat = NA,
                           long = NA,
                           reference = NA,
                           credit = NA,
                           email = NA, 
                           collector = NA,
                           taxonomist = NA)

#this is not final
bee_schema <- data.frame(category = c("metadata", "metadata", 
                                      "morphological", "morphological", 
                                      "ecological", "ecological",
                                      "life_history", "life_history"), 
                     trait = c("sex", "interaction_type", 
                               "IT", "tongue_length", 
                               "sociality", "nest_site",
                               "offspring", "lifespan"),
                     units = c("factor", "factor",
                               "mm", "mm", 
                               "factor", "factor",
                               "individuals", "days"),
                     test = c("c('male', 'female', 'queen')",
                              "c('mutualist', 'parastite')",
                              "c(10)", "c(10)",
                              "c('social', 'solitary')",
                              "c('soil', 'soil_clay', 'soil_sand', 'soil_gipsy',
                                          'wood', 'cavity', 'hole', 'stem')",
                              "c(0:1000)", "c(1:1000)"),
                     description = c("sex", "interaction type",
                                     "interior distance between tegulas", 
                                     "Length of the tongue (prementum + glossa)", 
                                     "social behaviour: social, solitary", 
                                     "nest site: soil_clay, soil_sand, soil_gipsy, soil,
                                          wood, cavity, hole, stem",
                                     "number of offspring per female", 
                                     "numbers of days lived as adult"))



#WARNING do not run this once data is uploaded
#write.csv(bee_observations, file="data/bee_observations.csv")
#write.csv(bee_specimens, file="data/bee_specimens.csv")
#save(bee_taxonomy, file="data/bee_taxonomy.rda")
#save(bee_schema, file="data/bee_schema.rda")


#load("data/bee_schema.rda")
bee_schema

#test functions:

dat<- data.frame( link_id = c(1,2,3),
                     genus = c("Andrena", "Osmia", "Bombus"),
                     species = c("cineraria", "bicornis", "terrestris"),
                     sex = c("female", "male", NA),
                     category = rep("morphological", 3), 
                     trait = c("IT", "tongue_length", "IT"),
                     value = c(1.2, 2.3, 2.9),
                     reference = rep("fake",3),
                     credit = rep("nacho", 3),
                     email = NA)
                    
 
dat2 <- data.frame(link_id = c(1,2,3),
                   genus = c("Andrena", "Osmia", "Bombus"),
                   species = c("cineraria", "bicornis", "terrestris"),
                   sex = c("female", "male", "queen"),
                    interaction_type = c("pollination", "pollination", "pollination"),
                    partner_genus = c("Lavandula", "Carpobrotus",
                                      "Rosmarinus"),
                    partner_species = c("stoechas", "edulis", NA),
                    day = c(1:3),
                    month = c(1:3),
                    year = rep(2015,3),
                    country = rep("Spain", 3), #Add state for US?
                    location = rep("home", 3),
                    lat = rep(40.00, 3),
                    long = rep(4.00, 3),
                    reference = rep("fake", 3),
                    credit = rep("me", 3),
                    email = rep(NA, 3), 
                    collector = rep("me", 3),
                    taxonomist = rep("me", 3))

check_data(dat, type = "specimens")
check_data(dat2, type = "observations")

add_data(dat, type = "specimens", check = TRUE)
add_data(dat2, type = "observations", check = TRUE)

