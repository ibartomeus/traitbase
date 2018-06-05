#manual unit tests

#things needed
library(taxize)
source("R/clean_species.R")
source("R/help_functions.R")
library(reshape2)
#library(devtools)
#install_github("metadevpro/traitbaser")
library(traitbaser)
#cnx <- connect(url = "https://traitbase.info", usr, psw)
cnx <- connect(url = "https://traitbase-qa.herokuapp.com/", "test", "test")

#test data
d <- read.csv("tests/testdata.csv", header = TRUE, sep = ";")
head(d)
errors <- validateDataset(cnx, d)
errors

parse_errors(errors)


#way to clean errors
temp <- clean_species(species = d$species[1:3])

#one by one
d$test
parse_errors(validateDataset(cnx, d[1,]))
