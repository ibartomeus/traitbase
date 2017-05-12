#manual unit tests

#things needed
library(taxize)
source("R/clean_species.R")
source("R/help_functions.R")
library(reshape2)
#library(devtools)
#install_github("metadevpro/traitbaser")
library(traitbaser)
#cnx <- connect(url = "http://www.traitbase.info", "demo", "1234")
cnx <- connect(url = "http://traitbase-qa.herokuapp.com/", "demo", "1234")

#test data
d <- read.csv("processed_data/testdata.csv", header = TRUE, sep = ";")
head(d)
errors <- validateDataset(cnx, d)
errors

parse_errors(errors)


#way to clean errors
temp <- clean_species(species = d$species[1:3])

#one by one
d$test
parse_errors(validateDataset(cnx, d[1,]))
