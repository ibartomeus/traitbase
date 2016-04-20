library(devtools)
library(roxygen2)
library(knitr)
document()


#create the initial datasets
species <- data.frame(id = NA,
                      Genus = NA,
                      species = NA,
                      order = NA,
                      family = NA,
                      subfamily = NA)


specimens <- data.frame(id = NA,
                        link_id = NA,
                        Genus = NA,
                        species = NA,
                        sex = NA,
                        trait_category = NA, 
                        trait = NA,
                        value = NA,
                        reference = NA,
                        credit = NA)

observations <- data.frame(id = NA,
                           link_id = NA,
                           Genus = NA,
                           species = NA,
                           sex = NA,
                           host_genus = NA,
                           host_species = NA,
                           day = NA,
                           month = NA,
                           year = NA,
                           location = NA,
                           lat = NA,
                           long = NA,
                           reference = NA,
                           credit = NA,
                           collector = NA,
                           taxonomist = NA)

#initial traits
schema <- data.frame(trait_category = c("morphological", "morphological", 
                                        "ecological", "ecological",
                                        "life_history", "life_history"), 
                     trait = c("IT", "tongue_length", 
                               "sociality", "nest_site",
                               "offspring", "lifespan"),
                     units = c("mm", "mm", 
                               "factor", "factor",
                               "individuals", "days"),
                     description = c("interior distance between tegulas", 
                                     "nest location", 
                                     "social behaviour: social, solitary", 
                                     "nest site: soil, wood, cavity, hole, stem",
                                     "number of offspring per female", 
                                     "numbers of days lived as adult"))


save(observations, file="data/observations.rda")
save(specimens, file="data/specimens.rda")
save(taxonomy, file="data/taxonomy.rda")
save(schema, file="data/schema.rda")



#issues:
#-What to do with means and se's? Use only mean n= 1, reverse ingenieer?

