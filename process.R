library(devtools)
library(roxygen2)
library(knitr)
library(taxize) #add to dependencies
library(countrycode) #idem
library(plyr) #also
library(git2r) #also

#add this as func
num.decimals <- function(x) {
    stopifnot(class(x)=="numeric")
    x <- sub("0+$","",x)
    x <- sub("^.+[.]","",x)
    nchar(x)
}


#document
document()


#create the initial datasets
bee_species <- data.frame(id = NA,
                      genus = NA,
                      species = NA,
                      order = NA,
                      superfamily = NA,
                      family = NA,
                      subfamily = NA,
                      tribe = NA,
                      subgenus = NA) #modify .R!


bee_specimens <- data.frame(id = NA,
                        link_id = NA,
                        Genus = NA,
                        species = NA,
                        sex = NA,
                        trait_category = NA, 
                        trait = NA,
                        value = NA,
                        reference = NA,
                        credit = NA)

bee_observations <- data.frame(id = NA,
                           link_id = NA,
                           Genus = NA,
                           species = NA,
                           sex = NA,
                           host_genus = NA,
                           host_species = NA,
                           day = NA,
                           month = NA,
                           year = NA,
                           country = NA, 
                           location = NA,
                           lat = NA,
                           long = NA,
                           reference = NA,
                           credit = NA,
                           collector = NA,
                           taxonomist = NA)

#initial traits
bee_schema <- data.frame(trait_category = c("morphological", "morphological", 
                                        "ecological", "ecological",
                                        "life_history", "life_history"), 
                     trait = c("IT", "tongue_length", 
                               "sociality", "nest_site",
                               "offspring", "lifespan"),
                     units = c("mm", "mm", 
                               "factor", "factor",
                               "individuals", "days"),
                     test = c("c(0:3)", "c(0:3)",
                              "c('social', 'solitary')",
                              "c('soil', 'soil_clay', 'soil_sand', 'soil_gipsy',
                                          'wood', 'cavity', 'hole', 'stem')",
                              "c(0:1000)", "c(1:1000)"),
                     description = c("interior distance between tegulas", 
                                     "Length of the tongue (prementum + glossa)", 
                                     "social behaviour: social, solitary", 
                                     "nest site: soil_clay, soil_sand, soil_gipsy, soil,
                                          wood, cavity, hole, stem",
                                     "number of offspring per female", 
                                     "numbers of days lived as adult"))


save(bee_observations, file="data/bee_observations.rda")
save(bee_specimens, file="data/bee_specimens.rda")
save(bee_taxonomy, file="data/bee_taxonomy.rda")
save(bee_schema, file="data/bee_schema.rda")



#issues:
#- What to do with means and se's? Use only mean n= 1, reverse ingenieer?
#- Bibtext?
# what about synomins. can we make a function that cleans and updates synonims in the master data?
# use travis with e.g.check data.

load("data/schema.rda")
schema
