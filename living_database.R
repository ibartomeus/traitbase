#this script upload raw data located at raw_data folder and produces 
#input for traitbase. By running this script you cna recreate the database 
#from scratch.

#source helper functions and packages
source("R/clean_species.R")
library(taxize)
library(reshape2)
#library(devtools)
#install_github("metadevpro/traitbaser")
#library(traitbaser)
#cnx <- connect(url = "http://traitbase-qa.herokuapp.com/", "demo", "1234")
#cnx <- connect(url = "http://www.traitbase.info", "demo", "1234")
#temporal function
#df_to_rl <- function(x){
#    x[is.na(x)] <- ""
#    header <- paste(colnames(x), collapse = ", ")
#    temp <- apply(x, MARGIN = 1, paste, collapse = ", ")
#    c(header, temp)
#} #not needed anymore, I think. 

#two functions I may need
validate_sliced <- function(cnx, d){
    if(length(unique(d$species)) > 50){
        errors <- list()
        cuts <- seq(1,nrow(d),50)
        cuts[length(cuts)+1] <- nrow(d)
        for(i in 1:(length(cuts)-1)){
            insert <- d[cuts[i]:cuts[i+1],]
            errors[[i]] <- validateDataset(cnx, insert)
        }
    } else {
        errors <- validateDataset(cnx, d)
    }
    errors
}
import_sliced <- function(cnx, d){
    if(length(unique(d$species)) > 50){
        errors <- list()
        cuts <- seq(1,nrow(d),50)
        cuts[length(cuts)+1] <- nrow(d)
        for(i in 1:(length(cuts)-1)){
            insert <- d[cuts[i]:cuts[i+1],]
            errors[[i]] <- importDataset(cnx, insert)
        }
    } else {
        errors <- importDataset(cnx, d)
    }
    errors
}

#Input data needs to be in a data.frame with the following columns:

#colnames:
#"name": UNIQUE. Name of the datset (should be unique and will be lastname_year 
    #followed by a,b,c if different datasets has the same name_year combination)
#"Desciption": UNIQUE. brief description of the dataset.
#"Credit": UNIQUE. Optional, in case we want to add some free text about how to credit the dataset
#"doi": UNIQUE. If published doi of the dataset/paper

#"local_id": Any id set in the original paper
#"species": Genus species (needs to be a valid taxon name
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
#5) Upload dataset

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
d$se_IT <- 0 #se is 0 for all
d$m_sex <- d$sex #less elegant way to rename a column
levels(d$m_sex) <- c("female", "male") #recode for standardizing all datsets.
d$n_sex <- 1 #sample size is one for all
d$se_sex <- 0 #se is 0 for all

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
          "m_IT", "n_IT", "se_IT", "m_sex",
          "n_sex", "se_sex",
          "doi", "name", "description", 
          "Contributor_name", "Contributor_lastname")]

#5) test and upload dataset
head(d)
#txt <- df_to_rl(d)
errors <- validateDataset(cnx, d)
errors
#txt <- readLines("processed_data/Oliveira_2016.csv")

unique(d$month) #great catch!
d[which(d$month > 12),"month"] <- c(7,4,5)

head(d)
errors <- validateDataset(cnx, d)
errors

importDataset(cnx, d) #works!

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
d$name <- "OsorioCanadas_2016"
d$description <- "Dataset with IT measure standard error and sample size for spanish bees (phenology sumaries not included)"
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
d$se_IT <- ifelse(is.na(d$se_IT), 0, d$se_IT)

#5) test and upload dataset
str(d)
head(d)
#errors <- validateDataset(cnx, d) #time out

errors <- validate_sliced(cnx, d)
errors

#clean species!
temp <- clean_species(d$species) #SLOWWWW needs used inputs.
temp$final_names <- as.character(d$final_names)
temp[which(temp$species == "Andrena carbonaria"), 4] <- "Andrena pilipes" 
temp[which(temp$species == "Andrena niveata lecana"), 4] <- "Andrena niveata" 
temp[which(temp$species == "Dioxys tridentata"), 4] <- "Aglaoapis tridentata" 
temp[which(temp$species == "Lasioglossum atrovirens"), 4] <- "Lasioglossum soror" 
temp[which(temp$species == "Nomiapis bispinosa"), 4] <- "Pseudapis bispinosa" 
temp[which(temp$species == "Nomiapis diversipes"), 4] <- "Pseudapis diversipes" 
temp[which(temp$species == "Panurgus arctos"), 4] <- "Panurgus cephalotes" 
temp[which(temp$species == "Rhodanthidium septendentatum"), 4] <- "Rhodanthidium septemdentatum" 
temp[which(temp$species == "Sphecodes aff.miniatus"), 4] <- "Sphecodes miniatus" 
#Andrena propinqua -> Added as new species
#Anthophora salviae -> #can be either Amegilla salviae o Anthophora crinipes. Ignore for now
#Hoplitis cristata -> Added as new species
#Osmia anceyi -> Added as new species

d$species <- temp$final_names

errors <- validate_sliced(cnx, d)
errors

importDataset(cnx, d) #fails, only adds a few rows!

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
d$m_sex <- d$sex
d$n_sex <- d$n_fresh_mass


#3) Add known missing columns (name, description, credit, doi)

#Add doi, no doi found in crossref
#d$doi <- No doi # NB you can check if there is doi here: http://www.questionpoint.org/crs/servlet/org.oclc.ask.AskPatronFetchQA?&language=1&qid=196591
d$name <- "Stone_1989"
d$credit <- "Stone, G. N., and P. G. Willmer. 1989. “Warm-Up Rates and Body Temperatures in Bees: The Importance of Body Size, Thermal Regime and Phylogeny.” The Journal of Experimental Biology 147 (1): 303–28.9"
d$description <- "Dataset with body mass and minimum ambient temperature for foraging"
#the fllwing lines are not necesary as there is doi, but for completness of the example
d$Contributor_name <- rep(NA, nrow(d)) #create an empty column
d$Contributor_name[1:2] <- c("G.N.", "P.G.") 
d$Contributor_lastname <- rep(NA, nrow(d)) #create an empty column
d$Contributor_lastname[1:2] <- c("Stone", "Willmer") #populate the first forut rows

#4) Remove unused columns

d <- d[,c("local_id", "species", "credit",
          "m_fresh_mass", "n_fresh_mass",
          "m_sex", "n_sex",
          "name", "description", 
          "Contributor_name", "Contributor_lastname")]

#5) test and upload dataset
head(d)
errors <- validateDataset(cnx, d)
errors #Yeah! it complains for species names :D
importDataset(cnx, d) 

#Data from Borrel, 2007  ----

#CHECK ALSO ONLINE MATERIAL

#1) Read data 

d <- read.csv("raw_data/Borrell_2006.csv", head= T, sep =  ";")

#2) Check observations colnames

d$local_id <- c(1:nrow(d))
colnames(d)[1] <- "species" 
colnames(d)[2] <-"m_fresh_mass" # unit:mg
colnames(d)[4] <-"m_tongue_length" #unit: mm
d$country <- "Central America"
d$m_tongue_length <- d$m_tongue_length*2

#3) Add known missing columns (name, description, credit, doi)

d$doi <- "10.1086/512689" #Searched in crossref
d$name <- "Borrell_2006"
d$description <- "Dataset with body mass and folded tongue length (to get real tongue length we x2 as stated in the paper) "
d$Contributor_name <- rep(NA, nrow(d)) 
d$Contributor_name[1:2] <- c("B.J") 
d$Contributor_lastname <- rep(NA, nrow(d)) 
d$Contributor_lastname[1:2] <- c("Borrell") 

#4) Remove unused columns

d <- d[,c("local_id", "species", "country","m_fresh_mass", "m_tongue_length",
          "doi", "name", "description", 
          "Contributor_name", "Contributor_lastname")]
head(d)

#5 test and upload dataset

head(d)
errors <- validateDataset(cnx, d)
errors

d$species <- as.character(d$species)
d$species[which(d$species == "Euglossa flammea ")] <- "Euglossa flammea"
d <- d[-which(d$species == "Eualema meriana"),]  #BAD SOLUTION IN TESTING ONLY
d <- d[-which(d$species == "Eualema polychroma"),]  #BAD SOLUTION IN TESTING ONLY
d <- d[-which(d$species == "Eualema cingulata"),]  #BAD SOLUTION IN TESTING ONLY
d$species[which(d$species == "Exaerete Frontalis")] <- "Exaerete frontalis"
d <- d[-which(d$species == "Eualema nigrita"),]  #BAD SOLUTION IN TESTING ONLY

importDataset(cnx, d) #same error about species not in ITIS

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
errors <- validateDataset(cnx, d)
errors

importDataset(cnx, d) #same error about species not in ITIS

#Data from Bartomeus, 2013------

#1) Read data 

d <- read.csv("raw_data/Bartomeus_2013.csv", header = TRUE, sep = ";", dec= ",")




#2) Check observations colnames

d$local_id <- c(1:nrow(d))
d$species <- paste(d$Genus, d$species)
summary(d)
colnames(d)[8] <- "m_nest_site" 
colnames(d)[9] <- "m_sociality"
levels(d$m_sociality) <- c("social", "facultative", "solitary")
colnames(d)[10] <- "m_parasitic" 
levels(d$m_parasitic) <- c("no", "yes")
colnames(d)[11] <- "m_floral_specialization" 
levels(d$m_floral_specialization) <- c("oligolectic", "polylectic")
colnames(d)[12] <- "m_voltinism" 
levels(d$m_voltinism) <- c("multivoltina", "univoltine")
colnames(d)[13] <- "m_IT" 
d$m_sex <- "female" 
#need to rescue info on queens!
temp <- subset(d, is.na(d$ITqueen) == FALSE)
temp$m_sex <- "queen"
temp$m_nest_site <- NA
temp$m_sociality <- NA
temp$m_parasitic <- NA
temp$m_floral_specialization <- NA
temp$m_voltinism <- NA
temp$m_IT <- temp$ITqueen
d <- rbind(d, temp)

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

d <- d[,c("local_id", "species", "country", "m_nest_site", "m_sociality", "m_parasitic", 
          "m_sex", "m_IT", "m_voltinism", "description", "m_floral_specialization","doi",
          "name", "Contributor_name", "Contributor_lastname")]

#5) test and upload dataset

head(d)
errors <- validateDataset(cnx, d)
errors

importDataset(cnx, d) #fix names


#Read data from Kremen, 2015-----

#1) Read data 

d <- read.csv("raw_data/Kremen_2015.csv", header = TRUE, sep = ";", dec= ",")

#2) Check observations colnames

head(d)
summary(d)
#d[which(d$MeanITD < 0), "MeanITD"] <- NA #!!
#NEED TO EXCTART AGAIN; BETTER DATA IN THE SUP MAT: ITD IS ln!! 
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
head(d)
d$local_id <- c(1:nrow(d))
colnames(d)[1] <- "species"
colnames(d)[2] <- "m_IT"

#3) Add known missing columns 

d$country <- "Turkey"
d$location <- "Gorukle Campus of Uludag University, Bursa"
d$lat <- "40-13-35N, 28-52-13E" #FIX THAT!
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

head(d)
colnames(d)[1] <- "species"
colnames(d)[5] <- "m_IT"
colnames(d)[10] <- "m_sociality" 
colnames(d)[11] <- "m_floral_specialization"  #MODIFY!!! yo polylectic/oligolectic
#Add nesting behaviour/location!

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

d <- read.csv("processed_data/testdata_add.csv")
head(d)
validateDataset(cnx, d)
importDataset(cnx, d) #fix names



#Read data from Carstensen_et_al_2012-----

#1) Read data 

d <- read.csv("raw_data/Carstensen_et_al_2012.csv", 
              header = TRUE, sep =";", dec= ",", na.strings = c("", "-"))

head(d)
str(d)

#2) Check observations colnames

d$local_id <- c(1:nrow(d))
colnames(d)[3] <- "plant_species"
colnames(d)[4] <- "species"

head(d)
str(d)
#split plant 
position <- regexpr(pattern = " ", d$plant_species)
d$m_plant_genus <- substr(d$plant_species, 1, position-1)
d$m_plant_species <- substr(d$plant_species, position+1, nchar(as.character(d$plant_species)))
d$n_plant_genus <- 1
d$n_plant_species <- 1
d$se_plant_genus <- 0
d$se_plant_species <- 0
    
#split date #tenias el ejemplo en la linea 128 
date <- as.POSIXlt(strptime(d$Date, "%d/%m/%Y")) #convert to date class
d$day <- date$mday #extract the day only
d$month <- date$mon+1 #extract the day only
d$year <- date$year + 1900 #extract the day only

#clean species (i.e. I don't want the parenthesis)
unique(d$species)
position1 <- regexpr(pattern = "(", d$species, fixed = TRUE)
position2 <- regexpr(pattern = ")", d$species, fixed = TRUE)
d$species2 <- ifelse(grepl("(",d$species, fixed = TRUE),
                     paste(substr(d$species, 1, position1-1),
                           substr(d$species, position2+1, nchar(as.character(d$species)))),
                     as.character(d$species)) #need to clean spaces
d$species <- d$species2

#3) Add known missing columns 

d$country <- "Brazil"
d$location <- "National Park of Serra do Cipó"
d$doi <- "10.1371/journal.pone.0117763"
d$name <- "Carstensen_et_al_2015"
d$description <- "Dataset about interactions"

#Add lat/long per site and maybe keep in location via 
levels(d$Site)
d$location <- paste(d$location, ":", d$Site)
d$lat <- "" #NEED TO DO
d$long <- "" #NEED TO DO
 
head(d)
str(d)

#4) Remove unused columns ...



#I keep the template for later:
#1) Read data (read.table, read.csv...)
#2) Check observations colnames ("local_id", "species","collector","taxonomist",
#"day","month","year","lat","long","location","country")
#Add lat long from google maps or paper if possible.
#Check traits colnames(m_trait, se_trait, n_trait)
#3) Add known missing columns (name, description, credit, doi)
#Add contributor information (if doi, can be ignored)
#Do not look for contributor info in detail.
#4) Remove unused columns
#5) Upload dataset

