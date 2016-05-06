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
                        trait_category = NA, #this is necesary?
                        trait = NA,
                        value = NA,
                        reference = NA,
                        credit = NA)

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
                              "c(0:3)", "c(0:3)",
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



save(bee_observations, file="data/bee_observations.rda")
save(bee_specimens, file="data/bee_specimens.rda")
save(bee_taxonomy, file="data/bee_taxonomy.rda")
save(bee_schema, file="data/bee_schema.rda")



#issues:
#- What to do with means and se's? Use only mean n= 1, reverse ingenieer?
#- Bibtext?
#- what about synomins. can we make a function that cleans and updates synonims in the master data?
#- use travis with e.g.check data.

#load("data/bee_schema.rda")
bee_schema


#to explore: 
library(ckanr)
?resource_update   

