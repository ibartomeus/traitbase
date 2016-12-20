#this script load raw data located at data and produces input for traitbase.
#By running this script you cna recreate the database from scratch.

#
source("R/clean_species.R")


#Test data

#Data from Cap creus.----

d <- read.csv("raw_data/ITCapCreustest.csv", header = TRUE)
head(d)

#need to recover: (function fill_columns will do that!)
#local_id (obs) species (obs) day (obs) month (obs) year (obs) lat (obs) long (obs) 
#location (obs) country (obs) collector (obs) taxonomist (obs) trait1 (measures) 
#trait 2 (measures) doi (Dataset) Contributor_name (Contributors), Contributor_last_name (Contributors) 
#ORCID (Contributors)

#colnames <-  "local_id","species","collector","taxonomist","day","month","year","lat","long","location",
 #"country","m_IT","m_sex","m_plant_genus","m_plant_species","doi","Contributor_name","Contributor_lastname","ORCID","name"


colnames(d)
colnames(d)[1] <- "local_id"
d$species <- paste(d$Genero, d$Especie)
colnames(d)[11] <- "day"
colnames(d)[12] <- "month"
colnames(d)[13] <- "year"
d$lat <- "42.3202451"
d$long <- "3.314970799999969"
d$location <- "Cap de creus"
d$country <- "Spain"
d$collector <- "I. Bartomeus"
colnames(d)[10] <- "taxonomist"
colnames(d)[4] <- "m_IT" #use this in traits!
colnames(d)[7] <- "m_sex" #use this in traits!
colnames(d)[8] <- "m_plant_genus" #use this in traits!
colnames(d)[9] <- "m_plant_species" #use this in traits!
d$doi <- "10.1007/s00442-007-0946-1"
d$Contributor_name <- "Ignasi"
d$Contributor_last_name <- "Bartomeus"
d$ORCID <- "0000-0001-7893-4389"
d$name <- "Bartomeus_cap_creus"


head(d)
#reorder
#local_id (obs) species (obs) day (obs) month (obs) year (obs) lat (obs) long (obs) 
#location (obs) country (obs) collector (obs) taxonomist (obs) trait1 (measures) 
#trait 2 (measures) doi (Dataset) Contributor_name (Contributors), Contributor_last_name (Contributors) 
#ORCID (Contributors), "name"

d <- d[,c("local_id", "species", "collector", "taxonomist",           
     "day", "month", "year", "lat", "long", "location", "country", 
     "m_IT", "m_sex", "m_plant_genus", "m_plant_species",
     "doi", "Contributor_name", "Contributor_last_name", "ORCID", "name")]
head(d)

#for now
unique(d$species)
d <- subset(d, !species %in% c("Andrena?? sp", "Andrena  agilissima", "Anthidium  sticticum", 
                               "Anthidium  septemspinosum", "Anthidium  sp", "Chalicodoma pyrenaica"))

#test error
colnames(d)[3] <- "collectorr"
    
write.csv(d, file = "processed_data/capCreus.csv", row.names = FALSE)

#check data: function check data will do that.


#Data from Nederland bees-----

d <- read.table("raw_data/Oliveira_etal.txt", header = TRUE, fileEncoding="UCS-2LE")
head(d)
str(d)

colnames(d)
d$species <- paste(d$Sample, d$species)
colnames(d)[8] <- "month"
colnames(d)[7] <- "year"
d$country <- "Netherlands"
colnames(d)[9] <- "m_IT" #use this in traits!
d$doi <- "10.1371/journal.pone.0148983"
d$name <- "Oliveira_nederlands"
d$m_sex <- d$sex
head(d)

d <- d[,c("species",           
          "month", "year", "country", 
          "m_IT", "m_sex",
          "doi", "name")]

clean_species(d$species)

x <- .Last.value

(x$species, x$matched_name2)

unique(x)

#test errors
write.csv(head(d), file = "processed_data/nederlands_short.csv", row.names = FALSE)

head(d)
write.csv(d, file = "processed_data/nederlands.csv", row.names = FALSE)

#Data from XXX----



