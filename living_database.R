#this script upload raw data located at raw_data folder and produces 
#input for traitbase. By running this script you cna recreate the database 
#from scratch.

#source helper functions and packages
source("R/clean_species.R")
library(taxize)
library(reshape2)
#library(devtools)
#install_github("metadevpro/traitbaser")
library(traitbaser)
cnx <- connect(url = "http://www.traitbase.info", "demo", "1234")
#temporal function
df_to_rl <- function(x){
    x[is.na(x)] <- ""
    header <- paste(colnames(x), collapse = ", ")
    temp <- apply(x, MARGIN = 1, paste, collapse = ", ")
    c(header, temp)
} 

#Input data needs to be in a data.frame with the following columns:

#colnames:
#"name": UNIQUE. Name of the datset (should be unique and will be lastname_year 
    #followed by a,b,c if different datasets has the same name_year combination)
#"Desciption": UNIQUE. brief description of the dataset.
#"Credit": UNIQUE. Optional, in case we want to add some free text about how to credit the dataset
#"doi": UNIQUE. If published doi of the dataset/paper

#"local_id": Any id set in the original paper
#"species": Genua species (needs to be a valid taxon name
#"collector": If known who the collector was
#"taxonomist": If known who the taxonomist was
#"day","month","year": In separate columns
#"lat","long","location": Lat long and descriptive location if available.
#"country": If known where was collected.

#Traits: measures start by "m_"
    #"m_IT" 
    #"m_sex" (male, female, queen, ...)
    #"m_plant_genus"
    #"m_plant_species"
    #...
        #standard error start by "se_"
    #"se_IT"
    #"se_sex"
    #"se_plant_genus"
    #"se_plant_species"
    #...
        #sample size start by "n_"
    #"n_IT"
    #"n_sex"
    #"n_plant_genus"
    #"n_plant_species"
    #...

#"Contributor_name": Who to give credit name. Usa as many rows as contributors. Rest can be NA.
#"Contributor_lastname": Who to give credit last name. Usa as many rows as contributors. Rest can be NA.
#"Contributor_country": OPTIONAL. Who to give credit last name. Usa as many rows as contributors. Rest can be NA.
#"Contributor_organization": OPTIONAL. Who to give credit last name. Usa as many rows as contributors. Rest can be NA.
#"Contributor_url": OPTIONAL. Who to give credit last name. Usa as many rows as contributors. Rest can be NA.
#"Contributor_email": OPTIONAL. Who to give credit last name. Usa as many rows as contributors. Rest can be NA.
#"Contributor_ORCID": Who to give credit ORCID. Usa as many rows as contributors. Rest can be NA.

#Add data---
#template:
#1) Read data (read.table, read.csv...)
#2) Check observations colnames ("local_id", "species","collector","taxonomist",
    #"day","month","year","lat","long","location","country")
    #Add lat long from google maps or paper if possible.
#Check traits colnames(m_trait, se_trait, n_trait)
#3) Add known missing columns (name, description, credit, doi)
#Add contributor information (if doi, can be ignored)
    #Do not look for contributor info in detail.
#4) Remove unused columns
#5) Write dataset?

#Data from Oliveira et al., 2016------

#1) Read data (read.table, read.csv...)
d <- read.table("raw_data/Oliveira_etal.txt", header = TRUE, fileEncoding="UCS-2LE")
head(d)
str(d)

#2) Check observations colnames ("local_id", "species","collector","taxonomist",
    #"day","month","year","lat","long","location","country")
    #Add lat long from google maps or paper if possible.
#Check traits colnames(m_trait, se_trait, n_trait)
colnames(d)
head(d)
#(no need to comment all, I am only commenting the first one as example)
d$local_id <- c(1:nrow(d)) #Add local_id manually 
d$species <- paste(d$Sample, d$species) #build species
#note collector and taxonomist is missing and it's ok.
colnames(d)[8] <- "month"
colnames(d)[7] <- "year" 
date <- as.POSIXlt(strptime(d$date, "%d/%m/%Y")) #convert to date class
d$day <- date$mday #extract the day only
#lat long and location missing. It's ok
d$country <- "Netherlands" #Add country based on paper description
colnames(d)[9] <- "m_IT" #rename trait
summary(d$m_IT) #check is numeric and range is ok
d$n_IT <- 1 #sample size is one for all
d$m_sex <- d$sex #less elegant way to rename a column
levels(d$m_sex) <- c("female", "male") #recode for standardizing all datsets.

#3) Add known missing columns (name, description, credit, doi)
#Add contributor information (if doi, can be ignored)
d$doi <- "10.1371/journal.pone.0148983" #Add doi
d$name <- "Oliveira_2016" # Add name of the dataset
d$description <- "Dataset describing bodi sizes for 10 bee species alomg > 100 years in the Nederlands" # Add name of the dataset
d$Contributor_name <- rep(NA, nrow(d)) #create an empty column
d$Contributor_name[1:4] <- c("MO", "BM", "J", "D") #populate the first forut rows
d$Contributor_lastname <- rep(NA, nrow(d)) #create an empty column
d$Contributor_lastname[1:4] <- c("Oliveira", "Freitas", "Scheper", "Kleijn") #populate the first forut rows

#4) Remove unused columns
head(d)
d <- d[,c("local_id", "species",           
          "day", "month", "year", "country", 
          "m_IT", "n_IT", "m_sex",
          "doi", "name", "description", 
          "Contributor_name", "Contributor_lastname")]

#5) test and upload dataset
head(d)
txt <- df_to_rl(d)
errors <- validateDataset(cnx, txt)
errors
#txt <- readLines("processed_data/Oliveira_2016.csv")

unique(d$month) #great catch!
d[which(d$month > 12),"month"] <- c(7,4,5)

head(d)
txt <- df_to_rl(d)
errors <- validateDataset(cnx, txt)
errors

importDataset(cnx, txt) #works!

#Data from Osorio-Canadas et al., 2016----

#1) Read data 

d <- read.csv("raw_data/Osorio-Canadas_2016.csv", 
              header = TRUE, sep =";", dec= ",", na.strings = c("", "-"))
head(d)
str(d)
#2) Check observations colnames

#Delete first row (it is useless)

d <- d[-1,]
d$local_id <- c(1:nrow(d))
d$species <- paste(d$Genus, d$Species)
d$country <- "Spain"
colnames(d)[8] <- "m_IT" 
colnames(d)[9] <- "se_IT" 
colnames(d)[10] <- "n_IT" 

#3) Add known missing columns (name, description, credit, doi)

#Add doi
d$doi <- "10.1111/ele.12687" 
# Add name of the dataset
d$name <- "Osorio-Canadas_2016"
d$description <- "Dataset with IT measure standard error and sample size also coldest temperature but was not included"
d$Contributor_name <- rep(NA, nrow(d)) #create an empty column
d$Contributor_name[1:6] <- c("S", "X", "A", "A","R", "J") 
d$Contributor_lastname <- rep(NA, nrow(d)) #create an empty column
d$Contributor_lastname[1:6] <- c("Osorio-Canadas", "Arnan", "Rodrigo", "Torne-Noguera", "Molowny", "Bosch") #populate the first forut rows


#4) Remove unused columns

d <- d[,c("local_id", "species", "country",
          "m_IT", "se_IT", "n_IT",
          "doi", "name", "description", 
          "Contributor_name", "Contributor_lastname")]
head(d)

#tasks: 1. change (,) to (.)   
d$m_IT <- as.numeric(gsub(pattern = ",", replacement = ".", fixed = TRUE, as.character(d$m_IT)))
d$se_IT <- as.numeric(gsub(pattern = ",", replacement = ".", fixed = TRUE, as.character(d$se_IT)))

#5) test and upload dataset
d$se_IT[is.na(d$se_IT)] <- 0
str(d)
head(d)
txt <- df_to_rl(d)
errors <- validateDataset(cnx, txt)
errors

#synonim:
d$species[which(d$species == "Anthophora mucida")] <- "Anthophora biciliata"
d$species[which(d$species == "Anthophora acervorum")] <- "Anthophora plumipes"
d$species[which(d$species == "Anthophora salviae")] <- "Amegilla cognata"

#clean species!
temp <- clean_species(d$species)

#test Amegilla cognata added manually
txt <- df_to_rl(d)
errors <- validateDataset(cnx, txt)
errors

head(txt)
importDataset(cnx, txt) #fails, only adds a few rows!

#Data from Stone & Willmer, 1989----

#I have to create the Csv and pass the data manually (old paper)

#1) Read data (in prep)

d <- read.csv("raw_data/Stone_1989.csv", header = TRUE, sep = ";", dec= ",")
head(d)

#2) Check observations colnames

d$local_id <- c(1:nrow(d))
colnames(d)[1] <- "species" 
colnames(d)[2] <- "m_fresh_mass"  #fresh mass in the paper the unit is g, but I think it is wrong and it is mg
colnames(d)[3] <- "n_fresh_mass" 


#3) Add known missing columns (name, description, credit, doi)

#Add doi, no doi found in crossref
#d$doi <- No doi # NB you can check if there is doi here: http://www.questionpoint.org/crs/servlet/org.oclc.ask.AskPatronFetchQA?&language=1&qid=196591
d$name <- "Stone_1989"
d$description <- "Dataset with body mass and minimum ambient temperature at foraging occurs"
#the fllwing lines are not necesary as there is doi, but for completness of the example
d$Contributor_name <- rep(NA, nrow(d)) #create an empty column
d$Contributor_name[1:2] <- c("G.N.", "P.G.") 
d$Contributor_lastname <- rep(NA, nrow(d)) #create an empty column
d$Contributor_lastname[1:2] <- c("Stone", "Willmer") #populate the first forut rows

#4) Remove unused columns

d <- d[,c("local_id", "species",
          "m_fresh_mass", "n_fresh_mass",
          "name", "description", 
          "Contributor_name", "Contributor_lastname")]

d$species #need to remove names in species. Maybe can be done in raw data.

#5) test and upload dataset
head(d)
txt <- df_to_rl(d)
errors <- validateDataset(cnx, txt)
errors #Yeah! it complains for species names :D
importDataset(cnx, txt) #fails!

#Data from Borrel, 2007  ----

#CHECK ALSO ONLINE MATERIAL

#1) Read data 

d <- read.csv("raw_data/Borrell_2006.csv", head= T, sep =  ";")

#2) Check observations colnames

d$local_id <- c(1:nrow(d))
colnames(d)[1] <- "species" 
colnames(d)[2] <-"m_fresh_mass" # unit:mg
colnames(d)[4] <-"m_tongue_length" #unit: mm
d$country <- "Costa Rica & Panama"

#3) Add known missing columns (name, description, credit, doi)

d$doi <- "10.1086/512689" #Searched in crossref
d$name <- "Borrell_2006"
d$description <- "Dataset with body mass and the length of the tongue (the real measure of the tongue was with the tongue folded, half of the total tongue lenght) "
d$Contributor_name <- rep(NA, nrow(d)) 
d$Contributor_name[1:2] <- c("B.J") 
d$Contributor_lastname <- rep(NA, nrow(d)) 
d$Contributor_lastname[1:2] <- c("Borrell") 

#4) Remove unused columns

d <- d[,c("local_id", "species", "country","m_fresh_mass", "m_tongue_length", "doi", "name", "description", 
          "Contributor_name", "Contributor_lastname")]
head(d)

#5 test and upload dataset

head(d)
txt <- df_to_rl(d)
errors <- validateDataset(cnx, txt)
errors

d$species <- as.character(d$species)
d$species[which(d$species == "Euglossa flammea ")] <- "Euglossa flammea"
d <- d[-which(d$species == "Eualema meriana"),]  #BAD SOLUTION IN TESTING ONLY
d <- d[-which(d$species == "Eualema polychroma"),]  #BAD SOLUTION IN TESTING ONLY
d <- d[-which(d$species == "Eualema cingulata"),]  #BAD SOLUTION IN TESTING ONLY
d$species[which(d$species == "Exaerete Frontalis")] <- "Exaerete frontalis"
d <- d[-which(d$species == "Eualema nigrita"),]  #BAD SOLUTION IN TESTING ONLY

importDataset(cnx, txt) #same error about species not in ITIS

#Data from Cariveau et al., 2016 ------

#1) Read data 

load("raw_data/Cariveau_2016.rda")
tongues -> d
d$local_id <- c(1:nrow(d))
d$species <- paste(d$genus, d$species)

#2 and 3, this time I did it creating new columns instead of reaclling them...

d$country <- "United States"
d$location <- "New Jersey"
d$mean_tongue_length_mm -> d$m_tongue_length #Unit: mm
d$mean_IT_length_mm -> d$m_IT #Unit: mm
d$sample_size -> d$n_tongue_length #Same sample size for both
d$sample_size -> d$n_IT #Same sample size for both
d$doi <- "10.1371/journal.pone.0151482" 
d$name <- "Cariveau_2016"
d$description <- "Dataset with measures of tongue, IT, glossa, prementum and their residuals, just included tongue and IT"
d$Contributor_name <- rep(NA, nrow(d)) 
d$Contributor_name[1:7] <- c("D.P.", "G.K.","I.", "J.", "J.", "J.", "R." ) 
d$Contributor_lastname <- rep(NA, nrow(d)) 
d$Contributor_lastname[1:7] <- c("Cariveau", "Nayak", "Bartomeus", "Zientek", "Ascher", "Gibbs", "Winfree") 

#4) Remove unused columns

d <- d[,c("local_id", "species", "country", "location", "m_tongue_length", "n_tongue_length", 
          "m_IT", "n_IT", "doi", "name", "description", "Contributor_name", "Contributor_lastname")]

#5 test and upload dataset

head(d)
txt <- df_to_rl(d)
errors <- validateDataset(cnx, txt[1:28])
errors

importDataset(cnx, txt[1:28]) #same error about species not in ITIS


#test
head(d)
d$test <- NA
d$name <- "test"
txt <- df_to_rl(d)
errors <- validateDataset(cnx, txt[1:28])
errors
importDataset(cnx, txt[1:28]) #same error about species not in ITIS


#Data from Bartomeus, 2013------

#add IT, think how integrate both

#1) Read data 

d <- read.csv("raw_data/Bartomeus_2013.csv", header = TRUE, sep = ";", dec= ",")

#2) Check observations colnames

d$local_id <- c(1:nrow(d))
d$species <- paste(d$Genus, d$species)
colnames(d)[8] <- "m_nest_site" 
colnames(d)[9] <- "m_sociality" 
colnames(d)[10] <- "m_parasitic" 
colnames(d)[11] <- "m_dietary_specialization" 

#col 11: Dietary_specialization. Maybe floral specialization like in the paper?

#This paper has new columns as 1)nest site, 2)Sociality; 3)Parasitic, 4) dietary specialization

#3) Add known missing columns 

d$country <- "United States"
d$doi <- "10.1073/pnas.1218503110" 
d$name <- "Bartomeus_2013"
d$description <- "Dataset with qualitative traits (nest site, sociality, pasatism, dietary specialization)"
d$Contributor_name <- rep(NA, nrow(d)) 
d$Contributor_name[1:7] <- c("I.", "J.", "J.", "B.", "D.", "S.", "R.") 
d$Contributor_lastname <- rep(NA, nrow(d)) 
d$Contributor_lastname[1:7] <- c("Bartomeus", "Ascher", "Gibbs", "Danforth", "Wagner", "Hedtke", "Winfree") 

#4) Remove unused columns

d <- d[,c("local_id", "species", "country", "m_nest_site", "m_sociality", "m_parasitic", "description", 
          "m_dietary_specialization","doi", "name", "Contributor_name", "Contributor_lastname")]

#5) test and upload dataset

head(d)
txt <- df_to_rl(d)
errors <- validateDataset(cnx, txt[1:84])
errors

importDataset(cnx, txt[1:84]) #fix to upload full dataset.


#Read data from Kremen, 2015-----

#1) Read data 

d <- read.csv("raw_data/Kremen_2015.csv", header = TRUE, sep = ";", dec= ",")

#2) Check observations colnames

d$local_id <- c(1:nrow(d))
colnames(d)[1] <- "species" 
colnames(d)[9] <- "m_sociality" 
colnames(d)[10] <- "m_dietary_specialization" 
colnames(d)[11] <- "m_IT" 

#date??

#3) Add known missing columns 

d$country <- "United States"
d$location <- "Centra Valley of California (Yolo County)"
d$doi <- "10.1111/1365-2664.12418" 
d$name <- "Kremen_2015"
d$description <- "Dataset with qualitative traits (sociality, dietary specialization) and quantitative traits 
(IT for bess and wing length for flies, also the coefficient d' whih is the deviation of the interaction)"
d$Contributor_name <- rep(NA, nrow(d)) 
d$Contributor_name[1:2] <- c("C.", "L.") 
d$Contributor_lastname <- rep(NA, nrow(d)) 
d$Contributor_lastname[1:2] <- c("Kremen", "Gonigle") 

#4) Remove unused columns


d <- d[,c("local_id", "species", "country", "location", "m_sociality",  "m_dietary_specialization", 
          "m_IT",  "description","doi", "name", "Contributor_name", "Contributor_lastname")]


#5) test and upload dataset

#Read data from Gonzalez et al., 2016-----

#1) Read data 

d <- read.csv("raw_data/Gonzalez_2016.csv", header = TRUE, sep = ";", dec= ",")

#2) Check observations colnames

d$local_id <- c(1:nrow(d))
colnames(d)[1] <- "species"
colnames(d)[2] <- "m_IT"

#3) Add known missing columns 

d$country <- "Turkey"
d$location <- "Gorukle Campus of Uludag University, Bursa"
d$lat <- "40-13-35N, 28-52-13E"
d$doi <- "10.3897/jhr.51.9353" 
d$name <- "Gonzalez_2016"
d$description <- "Dataset with information about IT measure and the postion of the trap to capture the bee"
#When the data was until the level of genus was eliminated
d$Contributor_name <- rep(NA, nrow(d)) 
d$Contributor_name[1:5] <- c("V.H.", "K.E.", "I.", "J.M.", "J.F.") 
d$Contributor_lastname <- rep(NA, nrow(d)) 
d$Contributor_lastname[1:5] <- c("Gonzalez", "Park", "Cakmak", "Hranitz", "Barthell") 

#4) Remove unused columns

d <- d[,c("local_id", "species", "country", "location", "lat",  
          "m_IT",  "description","doi", "name", "Contributor_name", "Contributor_lastname")]


#5) test and upload dataset


#Read data from Forrest et al., 2015-----


#1) Read data 

d <- read.csv("raw_data/Forrest_2015.csv", header = TRUE, sep = ";", dec= ",")

#2) Check observations colnames

colnames(d)[1] <- "species"
colnames(d)[5] <- "m_IT"
colnames(d)[10] <- "m_sociality"
colnames(d)[11] <- "m_detary_specialization"  #MODIFY!!!

#3) Add known missing columns 

d$country <- "United States"
d$location <- "Sacramento Valley, California"
d$doi <- "10.1111/1365-2664.12433" 
d$name <- "Forrest_2015"
d$description <-""
d$Contributor_name <- rep(NA, nrow(d)) 
d$Contributor_name[1:3] <- c("J.R.K.", "R.W.","C.") 
d$Contributor_lastname <- rep(NA, nrow(d)) 
d$Contributor_lastname[1:3] <- c("Forrest", "Thorp", "Kremen") 

#4) Remove unused columns

d <- d[,c("local_id", "species", "country", "location", "m_IT", "m_sociality", "m_dietary_spcialization",
         "description","doi", "name", "Contributor_name", "Contributor_lastname")]

#5) test and upload dataset



