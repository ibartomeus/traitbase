#test

#things needed
library(taxize)
source("R/clean_species.R")
library(reshape2)
#library(devtools)
install_github("metadevpro/traitbaser")
library(traitbaser)
#cnx <- connect(url = "http://www.traitbase.info", "demo", "1234")
cnx <- connect(url = "http://traitbase-qa.herokuapp.com/", "demo", "1234")

#test data
d <- read.csv("processed_data/testdata.csv", header = TRUE, sep = ";")
head(d)
errors <- validateDataset(cnx, d)
errors
str(errors)
parse_errors <- function(errors){
    if(errors$valid){
        message("This dataset is valid")
    } else{
        message("This dataset is NOT valid")
    }
    if(length(errors$warnings)){
        temp <- unlist(errors$warnings, use.names = FALSE)
        message(temp[seq(2,length(temp),2)])
    } 
    if(length(errors$errors)){
        temp <- unlist(errors$errors, use.names = FALSE)
        err <- temp[seq(2,length(temp),2)]
        cod <- temp[seq(1,length(temp),2)]
        taxonomy <- err[which(cod == "402")]
        tax <- as.numeric(substr(taxonomy,nchar(taxonomy)-1, nchar(taxonomy)))
    } 
    list(err, tax)
}

parse_errors(errors)


#way to clean errors
temp <- clean_species(d$species[1:3])
temp

importDataset(cnx, txt[c(1,3)]) 
importDataset(cnx, txt[c(1,3)]) 

#one by one
d$test
validateDataset(cnx, txt[c(1,13)])
validateDataset(cnx, txt[c(1,14)])
validateDataset(cnx, txt[c(1,15)])
validateDataset(cnx, txt[c(1,16)])
validateDataset(cnx, txt[c(1,17)])
