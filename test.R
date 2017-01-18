#test

#things needed
library(taxize)
source("R/clean_species.R")
library(reshape2)
#library(devtools)
#install_github("metadevpro/traitbaser")
library(traitbaser)
cnx <- connect(url = "http://www.traitbase.info", "demo", "1234")
#temporal function
df_to_rl <- function(x){
    header <- paste(colnames(d), collapse = ", ")
    temp <- apply(d, MARGIN = 1, paste, collapse = ", ")
    c(header, temp)
} 

#Issues
#Why capital/lower case letters OK
#ambigous not kept. OK
#do not fail with irreal lat/longs OK
#NA in day do not show up. Imposrts well. Which is the behaviour of NA? Or is any non numeric value?

#Questions
#Schema is linked to validation by Superfamily, right?


#test data
d <- read.csv("processed_data/testdata.csv", header = TRUE)
head(d)
txt <- df_to_rl(d)
errors <- validateDataset(cnx, txt)
errors
unlist(errors[[1]])
unlist(errors[[2]])
unlist(errors[[3]], use.names = FALSE) #errors
unlist(errors[[4]], use.names = FALSE) #warnings

#way to clean errors
temp <- clean_species(d$species[1:3])
temp

importDataset(cnx, txt[c(1,3)]) 
importDataset(cnx, txt[c(1,3)]) 
