#This script upload raw data located at raw_data folder and upload it 
#to traitbase. By running this script you can recreate the database 
#from scratch being all decisions made completely reproducible.

#Install packages----
library(taxize) #loaded with traitbase? Not Yet.
library(reshape2)
library(devtools)
#install_github("metadevpro/traitbaser") #use this one
#install_github("ibartomeus/traitbaser") 
library(traitbaser)
source("psw.R") #protected psw.
#First thing you need is to establish the connection 
#with your user and pasword credentials. If you want writing permits,
#email me at nacho.bartomeus@gmail.com-
cnx <- connect(url = "https://traitbase.info", usr, psw)
#cnx <- connect(url = "https://traitbase-qa.herokuapp.com/", usr, psw) #for testing only


#Input data needs to be in a data.frame with the following columns:
#colnames:
#"name": UNIQUE. Name of the datset (should be unique and will be lastname_year 
    #followed by a,b,c if different datasets has the same name_year combination)
#"Desciption": UNIQUE. brief description of the dataset.
#"Credit": UNIQUE. Optional, in case we want to add some free text about how to credit the dataset
#"doi": UNIQUE. If published doi of the dataset/paper

#"local_id": Any id set in the original paper
#"species": genus species (needs to be a valid taxon name).
#"collector": If known who the collector was
#"taxonomist": If known who the taxonomist was
#"day","month","year": In separate columns
#"lat","long","location": Lat long and descriptive location if available.
#"country": If known where was collected.

#Traits: measures start by "m_". Traits defined as per schema. 
#Here examples of bee traits:
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

#"contributor_name": Who to give credit name. Usa as many rows as contributors. Rest of the columns can be NA.
#"contributor_lastname": Who to give credit last name. Usa as many rows as contributors. Rest can be NA.
#"contributor_country": OPTIONAL. Who to give credit last name. Usa as many rows as contributors. Rest can be NA.
#"contributor_organization": OPTIONAL. Who to give credit last name. Usa as many rows as contributors. Rest can be NA.
#"contributor_url": OPTIONAL. Who to give credit last name. Usa as many rows as contributors. Rest can be NA.
#"contributor_email": OPTIONAL. Who to give credit last name. Usa as many rows as contributors. Rest can be NA.
#"contributor_ORCID": Who to give credit ORCID. Usa as many rows as contributors. Rest can be NA.

#Add data---
#template:
#1) Read data (read.table, read.csv...)
#2) Check observations colnames ("local_id", "species","collector","taxonomist",
    #"day","month","year","lat","long","location","country")
    #Add lat long from google maps or paper if possible.
    #Check traits colnames(m_trait, se_trait, n_trait)
#3) Add known missing columns (name, description, credit, doi)
    #Add contributor information (contributor_name, contributor_lastname, contributor_ORCID)
    #Do not look for other contributor info in detail.
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
d$se_IT <- 0 #se is 0 for all by definition (n = 1)
d$m_sex <- d$sex #less elegant way to rename a column
levels(d$m_sex) <- c("female", "male") #recode for standardizing all datsets.
d$n_sex <- 1 #sample size is one for all
d$se_sex <- 0 #se is 0 for all

#3) Add known missing columns (name, description, credit, doi)
#Add contributor information (if doi, can be ignored)
d$doi <- "10.1371/journal.pone.0148983" #Add doi
d$name <- "Oliveira_2016" # Add name of the dataset
d$description <- "Dataset describing bodi sizes for 10 bee species along > 100 years in the Nederlands" # Add name of the dataset
d$contributor_name <- rep(NA, nrow(d)) #create an empty column
d$contributor_name[1:4] <- c("MO", "BM", "J", "D") #populate the first four rows, because it has four authors
d$contributor_lastname <- rep(NA, nrow(d)) #create an empty column
d$contributor_lastname[1:4] <- c("Oliveira", "Freitas", "Scheper", "Kleijn") #populate the first forut rows
d$contributor_ORCID <- rep(NA, nrow(d)) #create an empty column
d$contributor_ORCID[1:4] <- c("NA1", "NA2", "NA3", "0000-0003-2500-7164") #populate the first forut rows

#4) Remove unused columns
head(d)
d <- d[,c("local_id", "species",           
          "day", "month", "year", "country", 
          "m_IT", "n_IT", "se_IT", "m_sex",
          "n_sex", "se_sex",
          "doi", "name", "description", 
          "contributor_name", "contributor_lastname",
          "contributor_ORCID")]

#5) test and upload dataset
head(d)
errors <- validateDataset(cnx, d)
parseErrors(errors)

unique(d$month) #great catch!
d[which(d$month > 12),"month"] <- c(7,4,5)

head(d)
errors <- validateDataset(cnx, d)
parseErrors(errors)

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
d <- d[-which(d$Species %in% c("sp.1", "sp.2", "sp.3",
                               "sp.4", "sp.5", "sp.6",
                               "sp.7", "sp.8", "sp.9",
                               "sp.10", "sp.11", "sp.12",
                               "sp.13", "sp.14", "sp.15", 
                               "sp.16", "sp. 9", "sp. 1")), ]

d$species <- paste(d$Genus, d$Species)
#remove " " at the end of line, also sp.'s, subspecies, cf.'s, aff.
d$species <- trimws(d$species, which = "right")
d$species <- gsub("cf.", "", d$species)
d$species <- gsub("aff.", "", d$species)
d$species <- gsub("  ", " ", d$species)

d$country <- "Spain"
colnames(d)[8] <- "m_IT" 
colnames(d)[9] <- "se_IT" 
colnames(d)[10] <- "n_IT" 

d$m_IT <- as.numeric(as.character(gsub(",", ".", d$m_IT)))
d$se_IT <- as.numeric(as.character(gsub(",", ".", d$se_IT)))
d$se_IT <- ifelse(is.na(d$se_IT), 0, d$se_IT)
d$n_IT <- as.numeric(as.character(gsub(",", ".", d$n_IT)))

#3) Add known missing columns (name, description, credit, doi)

#Add doi
d$doi <- "10.1111/ele.12687" 
# Add name of the dataset
d$name <- "OsorioCanadas_2016"
d$description <- "Dataset with IT measure standard error and sample size for spanish bees (phenology sumaries not included)"
d$contributor_name <- rep(NA, nrow(d)) #create an empty column
d$contributor_name[1:6] <- c("S", "X", "A", "A","R", "J") 
d$contributor_lastname <- rep(NA, nrow(d)) #create an empty column
d$contributor_lastname[1:6] <- c("Osorio-Canadas", "Arnan", "Rodrigo", "Torne-Noguera", "Molowny", "Bosch") #populate the first forut rows
d$contributor_ORCID <- rep(NA, nrow(d)) #create an empty column
d$contributor_ORCID[1:6] <- c("NA4", "NA5", "NA6", "NA7", "NA8", "NA9") #populate the first forut rows


#4) Remove unused columns
head(d)
d <- d[,c("local_id", "species", "country",
          "m_IT", "se_IT", "n_IT",
          "doi", "name", "description", 
          "contributor_name", "contributor_lastname",
          "contributor_ORCID")]
head(d)

#5) test and upload dataset
str(d)
head(d)
errors <- validateDataset(cnx, d) 
temp <- parseErrors(errors)

#clean species! NOT DONE YET
#temp <- cleanSpecies(d$species[temp[[2]]]) #SLOW needs user inputs.
#temp$final_names <- as.character(d$final_names)
# temp[which(temp$species == "Andrena carbonaria"), 4] <- "Andrena pilipes" 
# temp[which(temp$species == "Andrena niveata lecana"), 4] <- "Andrena niveata" 
# temp[which(temp$species == "Dioxys tridentata"), 4] <- "Aglaoapis tridentata" 
# temp[which(temp$species == "Lasioglossum atrovirens"), 4] <- "Lasioglossum soror" 
# temp[which(temp$species == "Nomiapis bispinosa"), 4] <- "Pseudapis bispinosa" 
# temp[which(temp$species == "Nomiapis diversipes"), 4] <- "Pseudapis diversipes" 
# temp[which(temp$species == "Panurgus arctos"), 4] <- "Panurgus cephalotes" 
# temp[which(temp$species == "Rhodanthidium septendentatum"), 4] <- "Rhodanthidium septemdentatum" 
# temp[which(temp$species == "Sphecodes aff.miniatus"), 4] <- "Sphecodes miniatus" 
#Andrena propinqua -> Added as new species
#Anthophora salviae -> #can be either Amegilla salviae o Anthophora crinipes. Ignore for now
#Hoplitis cristata -> Added as new species
#Osmia anceyi -> Added as new species

#d$species <- temp$final_names
#removing ambigous species mannually
d <- subset(d, !species %in% c("Osmia scutellaris", "Osmia anceyi", "Osmia ligurica"))
head(d)

errors <- validateDataset(cnx, d) 
temp <- parseErrors(errors) #I would go for it.

importDataset(cnx, d[-c(temp[[2]]),]) 


#Data from Stone & Willmer, 1989----
#I have to create the Csv and pass the data manually (old paper)

#1) Read data 

d <- read.csv("raw_data/Stone_1989.csv", header = TRUE, sep = ";", dec= ",")
head(d)

#2) Check observations colnames

d$local_id <- c(1:nrow(d))
colnames(d)[1] <- "species" 
unique(as.character(d$species))
colnames(d)[2] <- "m_fresh_mass"  #fresh mass in the paper the unit is g, but I think it is wrong and it is mg
d$m_fresh_mass <- d$m_fresh_mass/10 #update to grams
colnames(d)[3] <- "n_fresh_mass" 
d$m_sex <- d$sex
d$n_sex <- d$n_fresh_mass


#3) Add known missing columns (name, description, credit, doi)

#Add doi, no doi found in crossref
#d$doi <- No doi # NB you can check if there is doi here: http://www.questionpoint.org/crs/servlet/org.oclc.ask.AskPatronFetchQA?&language=1&qid=196591
d$name <- "Stone_1989"
d$credit <- "Stone, G. N., and P. G. Willmer. 1989. “Warm-Up Rates and Body Temperatures in Bees: The Importance of Body Size, Thermal Regime and Phylogeny.” The Journal of Experimental Biology 147 (1): 303–28.9"
d$description <- "Dataset with body mass and minimum ambient temperature for foraging"

d$contributor_name <- rep(NA, nrow(d)) #create an empty column
d$contributor_name[1:2] <- c("G.N.", "P.G.") 
d$contributor_lastname <- rep(NA, nrow(d)) #create an empty column
d$contributor_lastname[1:2] <- c("Stone", "Willmer") #populate the first forut rows
d$contributor_ORCID <- rep(NA, nrow(d)) #create an empty column
d$contributor_ORCID[1:2] <- c("NA10", "NA11") #populate the first forut rows

#4) Remove unused columns

d <- d[,c("local_id", "species", "credit",
          "m_fresh_mass", "n_fresh_mass",
          "m_sex", "n_sex",
          "name", "description", 
          "contributor_name", "contributor_lastname",
          "contributor_ORCID")]

#5) test and upload dataset
head(d)
errors <- validateDataset(cnx, d)
(temp <- parseErrors(errors))

#corregir
#temp <- cleanSpecies(d$species) 
#temp

#d$species <- temp$final_names

#errors <- validateDataset(cnx, d)
#(temp <- parseErrors(errors))

#errors que quedan:
to_rm <- d$species[temp[[2]]]
#simply ignore them.
importDataset(cnx, d[-which(d$species %in% to_rm),]) 

#Data from Borrel, 2007  ----
#CHECK ALSO ONLINE MATERIAL

#1) Read data 
d <- read.csv("raw_data/Borrell_2006.csv", head= T, sep =  ";")
head(d)
#2) Check observations colnames
d$local_id <- c(1:nrow(d))
colnames(d)[1] <- "species" 
unique(as.character(d$species))
d$species <- trimws(d$species)
d$species[which(d$species == "Exaerete Frontalis")] <- "Exaerete frontalis"
colnames(d)[2] <-"m_fresh_mass" # unit:mg
colnames(d)[4] <-"m_tongue_length" #unit: mm
d$country <- "Central America"
d$m_tongue_length <- d$m_tongue_length*2

#3) Add known missing columns (name, description, credit, doi)

d$doi <- "10.1086/512689" #Searched in crossref
d$name <- "Borrell_2006"
d$description <- "Dataset with body mass and folded tongue length (to get real tongue length we x2 as stated in the paper) "
d$contributor_name <- rep(NA, nrow(d)) 
d$contributor_name[1:2] <- c("B.J") 
d$contributor_lastname <- rep(NA, nrow(d)) 
d$contributor_lastname[1:2] <- c("Borrell") 
d$contributor_ORCID <- rep(NA, nrow(d)) 
d$contributor_ORCID[1:2] <- c("NA12") 

#4) Remove unused columns

d <- d[,c("local_id", "species", "country","m_fresh_mass", "m_tongue_length",
          "doi", "name", "description", 
          "contributor_name", "contributor_lastname", "contributor_ORCID")]
head(d)

#5 test and upload dataset

head(d)
errors <- validateDataset(cnx, d)
(temp <- parseErrors(errors))

to_rm <- d$species[temp[[2]]]
#simply ignore them.
importDataset(cnx, d[-which(d$species %in% to_rm),]) 


#Data from Cariveau et al., 2016 ------

#1) Read data 

load("raw_data/Cariveau_2016.rda")
tongues -> d
d$local_id <- c(1:nrow(d))
d$species <- paste(d$genus, d$species)
unique(d$species)

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
d$contributor_name <- rep(NA, nrow(d)) 
d$contributor_name[1:7] <- c("D.P.", "G.K.","I.", "J.", "J.", "J.", "R." ) 
d$contributor_lastname <- rep(NA, nrow(d)) 
d$contributor_lastname[1:7] <- c("Cariveau", "Nayak", "Bartomeus", "Zientek", "Ascher", "Gibbs", "Winfree") 
d$contributor_ORCID <- rep(NA, nrow(d)) 
d$contributor_ORCID[1:7] <- c("NA13", "NA14", "0000-0001-7893-4389", "NA15", "NA16", "NA17", "NA18") 

#4) Remove unused columns
head(d)
d <- d[,c("local_id", "species", "country", "location", "m_tongue_length", "n_tongue_length", 
          "m_IT", "n_IT", "doi", "name", "description", "contributor_name", "contributor_lastname",
          "contributor_ORCID")]

#5 test and upload dataset

head(d)
errors <- validateDataset(cnx, d)
(temp <- parseErrors(errors))
to_rm <- d$species[temp[[2]]]
#simply ignore them.
importDataset(cnx, d[-which(d$species %in% to_rm),]) #

#Data from Bartomeus, 2013------

#1) Read data 

d <- read.csv("raw_data/Bartomeus_2013.csv", header = TRUE, sep = ";", dec= ",")

#2) Check observations colnames
head(d)
d$local_id <- c(1:nrow(d))
d$species <- paste(d$Genus, d$species)
unique(d$species)
summary(d)
colnames(d)[8] <- "m_nest_site" 
colnames(d)[9] <- "m_sociality"
levels(d$m_sociality) <- c("social", "facultative", "solitary")
colnames(d)[10] <- "m_parasitism" 
levels(d$m_parasitism) <- c("no", "yes")
colnames(d)[11] <- "m_floral_specialization" 
levels(d$m_floral_specialization) <- c("oligolectic", "polylectic")
colnames(d)[12] <- "m_voltinism" 
levels(d$m_voltinism) <- c("multivoltine", "univoltine")
colnames(d)[13] <- "m_IT" 
d$m_sex <- "female" 
#need to rescue info on queens! 
temp <- subset(d, is.na(d$ITqueen) == FALSE)
temp$m_sex <- "queen"
temp$m_nest_site <- NA
temp$m_sociality <- NA
temp$m_parasitism <- NA
temp$m_floral_specialization <- NA
temp$m_voltinism <- NA
temp$m_IT <- temp$ITqueen
d <- rbind(d, temp)
#I don't know n and se... needed? or assumed 1.

#3) Add known missing columns 

d$country <- "United States"
d$doi <- "10.1073/pnas.1218503110" 
d$name <- "Bartomeus_2013"
d$description <- "Dataset with qualitative traits (nest site, sociality, pasatism, dietary specialization)"
d$contributor_name <- rep(NA, nrow(d)) 
d$contributor_name[1:7] <- c("I.", "J.", "J.", "B.", "D.", "S.", "R.") 
d$contributor_lastname <- rep(NA, nrow(d)) 
d$contributor_lastname[1:7] <- c("Bartomeus", "Ascher", "Gibbs", "Danforth", "Wagner", "Hedtke", "Winfree") 
d$contributor_ORCID <- rep(NA, nrow(d)) 
d$contributor_ORCID[1:7] <- c("0000-0001-7893-4389", "NA16", "NA18", "NA19", "NA20", "NA21", "NA18") 

#4) Remove unused columns
head(d)
d <- d[,c("local_id", "species", "country", "m_nest_site", "m_sociality", "m_parasitism", 
          "m_sex", "m_IT", "m_voltinism", "description", "m_floral_specialization","doi",
          "name", "contributor_name", "contributor_lastname", "contributor_ORCID")]

#5) test and upload dataset

head(d)
errors <- validateDataset(cnx, d)
(temp <- parseErrors(errors))

to_rm <- d$species[temp[[2]]]
#simply ignore them.
importDataset(cnx, d[-which(d$species %in% to_rm),]) 

#Read data from Kremen, 2015-----
#NOT WORKING-----
#1) Read data 
d <- read.csv("raw_data/Kremen_2015.csv", header = TRUE, sep = ";", dec= ",")

#2) Check observations colnames

summary(d)
#d[which(d$MeanITD < 0), "MeanITD"] <- NA #!!
#NEED TO EXCTART AGAIN; BETTER DATA IN THE SUP MAT: ITD IS ln!! 
d$local_id <- c(1:nrow(d))
colnames(d)[1] <- "species" 
unique(as.character(d$species))
colnames(d)[7] <- "m_nest_location" 
levels(d$m_nest_location) <- c("aboveground", "belowground", "below_and_aboveground")
colnames(d)[8] <- "m_nest_construction" 
levels(d$m_nest_construction) <- c("excavator", "non_excavator")
colnames(d)[9] <- "m_sociality" 
levels(d$m_sociality) 
colnames(d)[10] <- "m_floral_specialization" 
levels(d$m_floral_specialization)
#colnames(d)[11] <- "m_IT" #See above.
#For this probably use one entry per species
dup <- duplicated(d[,c(1,7,8,9,10)])
d <- d[!dup,]

#extract date? In second round.
#colnames(d)[3] <- "year"
#convert julian day to day/month

#3) Add known missing columns 

d$country <- "United States"
d$location <- "Centra Valley of California (Yolo County)"
d$doi <- "10.1111/1365-2664.12418" 
d$name <- "Kremen_2015"
d$description <- "Dataset with qualitative traits (sociality, dietary specialization) and quantitative traits 
(IT for bess and wing length for flies, also the coefficient d' whih is the deviation of the interaction)"
d$contributor_name <- rep(NA, nrow(d)) 
d$contributor_name[1:2] <- c("C.", "L.") 
d$contributor_lastname <- rep(NA, nrow(d)) 
d$contributor_lastname[1:2] <- c("Kremen", "Gonigle") 
d$contributor_ORCID <- rep(NA, nrow(d)) 
d$contributor_ORCID[1:2] <- c("NA22", "NA23") 

#4) Remove unused columns

d <- d[,c("local_id", "species", "country", "location", "m_sociality", "m_nest_location",
          "m_nest_construction", "m_floral_specialization", 
          "description","doi", "name", "contributor_name", "contributor_lastname",
          "contributor_ORCID")]


#5) test and upload dataset

head(d)
str(d)
errors <- validateDataset(cnx, d) 
(temp <- parseErrors(errors)) #gives very strange error...

to_rm <- d$species[temp[[2]]]
#simply ignore them.
importDataset(cnx, d[-which(d$species %in% to_rm),]) 


#Read data from Gonzalez et al., 2016-----

#1) Read data 

d <- read.csv("raw_data/Gonzalez_2016.csv", header = TRUE, sep = ";", dec= ",")

#2) Check observations colnames
head(d)
d$local_id <- c(1:nrow(d))
colnames(d)[1] <- "species"
d$species <- trimws(d$species)
colnames(d)[2] <- "m_IT"

#3) Add known missing columns 

d$country <- "Turkey"
d$location <- "Gorukle Campus of Uludag University, Bursa"
#d$lat <- "40-13-35N" #FIX THAT!
#d$long <- "28-52-13E" #FIX THAT!
d$doi <- "10.3897/jhr.51.9353" 
d$name <- "Gonzalez_2016"
d$description <- "Dataset with information about IT measure and the postion of the trap to capture the bee"
#When the data was until the level of genus was eliminated
d$contributor_name <- rep(NA, nrow(d)) 
d$contributor_name[1:5] <- c("V.H.", "K.E.", "I.", "J.M.", "J.F.") 
d$contributor_lastname <- rep(NA, nrow(d)) 
d$contributor_lastname[1:5] <- c("Gonzalez", "Park", "Cakmak", "Hranitz", "Barthell") 
d$contributor_ORCID <- rep(NA, nrow(d)) 
d$contributor_ORCID[1:5] <- c("NA24", "NA25", "NA26", "NA27", "NA28") 

#4) Remove unused columns

d <- d[,c("local_id", "species", "country", "location", #"lat",  
          "m_IT",  "description","doi", "name", "contributor_name", "contributor_lastname",
          "contributor_ORCID")]


#5) test and upload dataset

head(d)
errors <- validateDataset(cnx, d)
(temp <- parseErrors(errors))

to_rm <- d$species[temp[[2]]]
#simply ignore them.
importDataset(cnx, d[-which(d$species %in% c(to_rm, "Osmia bidentata")),])


#Read data from Forrest et al., 2015-----

#1) Read data 
d <- read.csv("raw_data/Forrest_2015.csv", header = TRUE, sep = ";", dec= ",")

#2) Check observations colnames

head(d)
colnames(d)[1] <- "species"
d$species <- gsub("(Synhalonia) ", "", d$species, fixed = TRUE)
d$species <- gsub("(Dialictus) ", "", d$species, fixed = TRUE)
d$species <- gsub("(Lasioglossum) ", "", d$species, fixed = TRUE)
colnames(d)[5] <- "m_IT"
colnames(d)[10] <- "m_sociality" 
#rescue parasitism:
d$m_parasitism <- d$m_sociality
levels(d$m_parasitism) <- c("yes", "no", "no")
levels(d$m_sociality) <- c("solitary", "social", "solitary")
colnames(d)[11] <- "m_floral_specialization"  
levels(d$m_floral_specialization) <- c("oligolectic", "polylectic", NA) #check NA!
#d[which(d$m_floral_specialization == "UK"),] #I yhink UK is unknown.
#Add nesting behaviour/location!
colnames(d)[8] <- "m_nest_location" 
levels(d$m_nest_location) <- c("aboveground", "belowground", "below_and_aboveground", NA)
colnames(d)[9] <- "m_nest_construction" 
levels(d$m_nest_construction) <- c("excavator", "non_excavator")

#3) Add known missing columns 

d$local_id <- 1:nrow(d)
d$country <- "United States"
d$location <- "Sacramento Valley, California"
d$doi <- "10.1111/1365-2664.12433" 
d$name <- "Forrest_2015"
d$description <- "Dataset of California Bee traits"
d$contributor_name <- rep(NA, nrow(d)) 
d$contributor_name[1:3] <- c("J.R.K.", "R.W.","C.") 
d$contributor_lastname <- rep(NA, nrow(d)) 
d$contributor_lastname[1:3] <- c("Forrest", "Thorp", "Kremen") 
d$contributor_ORCID <- rep(NA, nrow(d)) 
d$contributor_ORCID[1:3] <- c("NA29", "NA30", "NA22") 


#4) Remove unused columns

d <- d[,c("local_id", "species", "country", "location", "m_IT", "m_sociality", 
          "m_floral_specialization",
         "m_nest_location", "m_nest_construction", "m_parasitism", 
         "description", "doi", "name", "contributor_name", "contributor_lastname",
         "contributor_ORCID")]

#5) test and upload dataset

head(d)
errors <- validateDataset(cnx, d)
(temp <- parseErrors(errors))

to_rm <- d$species[temp[[2]]]
#simply ignore them.
importDataset(cnx, d[-which(d$species %in% to_rm),]) 

#Read data from Carstensen_et_al_2012-----
#NOT ENTERED FOR NOW AS VERY FEW SPECIES ARE IDENTIFIED...

#1) Read data 
d <- read.csv("raw_data/Carstensen_et_al_2012.csv", 
              header = TRUE, sep =";", dec= ",", na.strings = c("", "-"))
head(d)

#2) Check observations colnames
d$local_id <- c(1:nrow(d))
colnames(d)[3] <- "plant_species"
colnames(d)[4] <- "species"

#split plant 
position <- regexpr(pattern = " ", d$plant_species)
d$m_plant_genus <- substr(d$plant_species, 1, position-1)
d$m_plant_species <- substr(d$plant_species, position+1, nchar(as.character(d$plant_species)))
#d$n_plant_genus <- 1
#d$n_plant_species <- 1
#d$se_plant_genus <- 0
#d$se_plant_species <- 0
d$m_plant_species[which(d$m_plant_species %in% c("sp.", "sp. 2"))] <- NA 
d$m_plant_species <- paste(d$m_plant_genus, d$m_plant_species)   

#split date 
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
unique(d$species) #ALMOST NO GOOD ID's!!

head(d)
#3) Add known missing columns 

d$country <- "Brazil"
d$location <- "National Park of Serra do Cipó"
d$doi <- "10.1371/journal.pone.0117763"
d$name <- "Carstensen_et_al_2015"
d$description <- "Dataset about interactions"
d$contributor_name <- rep(NA, nrow(d)) 
d$contributor_name[1:4] <- c("D.W.", "M.", "K.", "L.P.C.") 
d$contributor_lastname <- rep(NA, nrow(d)) 
d$contributor_lastname[1:4] <- c("Carstensen", "Sabatino", "Trøjelsgaard", "Morellato")
d$contributor_ORCID <- rep(NA, nrow(d)) 
d$contributor_ORCID[1:4] <- c("NA31", "NA32", "NA33", "NA34")


#Add lat/long per site and maybe keep in location via 
levels(d$Site)
d$location <- paste(d$location, ":", d$Site)
d$lat <- ifelse(d$site =="Cedro", "-19.2320778",
                ifelse (d$site=="Gigante","-19.2473083",
                        ifelse (d$site=="Paulino","-19.2553111",
                                ifelse (d$site=="Tinkerbell","-19.220725",
                                        ifelse (d$site=="Midway", "-19.2702972",
                                                ifelse (d$site=="Elefante", "-19.2934528",
                                                        ifelse (d$site=="Soizig", "-19.2728028",NA)))))))
d$long <- ifelse(d$site =="Cedro","-43.576394444444446",
                 ifelse (d$site=="Gigante","-43.510197222222224",
                         ifelse (d$site=="Paulino","-43.583869444444446",
                                 ifelse (d$site=="Tinkerbell","-43.58296388888889",
                                         ifelse (d$site=="Midway", "-43.550352777777775",
                                                 ifelse (d$site=="Elefante", "-43.55553333333333",
                                                         ifelse (d$site=="Soizig", "-43.57983611111111",NA))))))) 

head(d)
str(d)

#4) Remove unused columns ...



#5) Upload dataset 
errors <- validateDataset(cnx, d)
temp <- parseErrors(errors)

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



#Read data from Fortel_et_al_2014.csv-----

#1) Read data (read.table, read.csv...)

d <- read.csv("raw_data/Fortel_et_al_2014.csv", 
              header = TRUE, sep =";", dec= ",", na.strings = c("", "-"))
head(d)
#2) Check observations colnames ("local_id", "species","collector","taxonomist",
#"day","month","year","lat","long","location","country")

d$local_id <- c(1:nrow(d))

colnames(d)[1]<- "species"
unique(as.character(d$species))
colnames(d)[4]<-"m_IT"
#use points
d$m_IT <- as.numeric(as.character(gsub(",", ".", d$m_IT)))
colnames(d)[6] <-"m_parasitism"
levels(d$m_parasitism) <- c("no", "yes")
colnames(d)[7] <- "m_sociality"
levels(d$m_sociality) <- c(NA, "social", "solitary")
colnames(d)[8] <- "m_nest_site"
levels(d$m_nest_site) <- c("cavity", NA, "soil")
colnames(d)[3] <- "n_IT" 
d$n_IT

#3) Add known missing columns (name, description, credit, doi)
d$country <- "France"
d$location <- "Grand Lyon"
d$doi <- "10.1371/journal.pone.0104679"
d$name <- "Fortel_et_al_2014"
d$description <- "Dataset about traits"
d$contributor_name <- rep(NA, nrow(d)) 
d$contributor_name[1:8] <- c("L.","M.","L.","A.L.", "M.", "H.", "O.","B.") 
d$contributor_lastname <- rep(NA, nrow(d)) 
d$contributor_lastname[1:8] <- c("Fortel", "Henry", "Guilbaud", "Guirao","Kuhlmann","Mouret","Rollin","Vaissière")
d$contributor_ORCID <- rep(NA, nrow(d)) 
d$contributor_ORCID[1:8] <- c("NA31", "NA32", "NA33", "NA34","NA35","NA36","NA37","NA38")

d$lat <-"45.7666667"
d$long <-"4.833333333333333"

#4) Remove unused columns
head(d)
colnames(d)
d <- d[,c("local_id", "species", "country", "location", "m_IT", "n_IT", "m_sociality", 
          "m_nest_site", "m_parasitism", "lat", "long",
          "description", "doi", "name", "contributor_name", "contributor_lastname",
          "contributor_ORCID")]

#5) Upload dataset
errors <- validateDataset(cnx, d)
(temp <- parseErrors(errors))

to_rm <- d$species[c(temp[[2]],262)]
#simply ignore them.
importDataset(cnx, d[-which(d$species %in% to_rm),]) 


#Read data from Gonzalez_et_al_Tabla_1_1999.csv-----

#1) Read data (read.table, read.csv...)

d <- read.csv("raw_data/Gonzalez_et_al_Tabla_1_1999.csv", 
              header = TRUE, sep =";", dec= ",", na.strings = c("", "-"))
head(d)
#2) Check observations colnames ("local_id", "species","collector","taxonomist",
#"day","month","year","lat","long","location","country")

d$local_id <- c(1:nrow(d))
colnames(d)[1]<-"date"
colnames(d)[2]<-"family"
colnames(d)[3]<-"genus"
colnames(d)[4]<-"species"
colnames(d)[5]<-"n_species"
colnames(d)[6]<-"m_sex"

#3) Add known missing columns (name, description, credit, doi)

date <- as.POSIXlt(strptime(d$date, "%d/%m/%Y")) #convert to date class
d$day <- date$mday #extract the day only
d$month <- date$mon+1 #extract the day only
d$year <- date$year + 1900 #extract the day only
d$species <- paste(d$genus, d$species)
unique(d$species)
d$country <- "Spain"
d$location <- "Viana de Cega"
d$credit <- "Zoologica Baetica vol. 10, 87-111"
d$name <- "Gonzalez_et_al_Tabla_1_1999"
d$description <-"Dataset about relationship of species studied, with indication of the number of specimens collected
during each of the sampling periods."

d$lat <-"41.5129466"
d$long <-"-4.758804199999986"

head(d)
#4) Remove unused columns
colnames(d)
d <- d[,c("local_id", "species" ,"m_sex", 
                    "day", "month",
                    "year" , "country" ,"location",   
                    "credit"     , "name"        ,"description",
                    "lat"        , "long")]

head(d)
#test and upload dataset
errors <- validateDataset(cnx, d)
(temp <- parseErrors(errors))

to_rm <- d$species[c(temp[[2]],262)]
#simply ignore them.
importDataset(cnx, d[-which(d$species %in% to_rm),]) 

#NOT WORKING! m_columns needed (there is m_sex)¿?----



#Read data from Skandalis_2009.csv-----


d <- read.csv("raw_data/Skandalis_2009_.csv", header = TRUE, sep = ";", dec= ",", encoding = "") 
head(d)

#2) Check observations colnames ("local_id", "species","collector","taxonomist",
#"day","month","year","lat","long","location","country")

d$local_id <- c(1:nrow(d))
colnames(d)[1]<-"location"
colnames(d)[3]<-"m_HW" #LIMPIAR DATOS EN LOS PARÉNTESIS O LOS SE. CONSULTAR CON NACHO
colnames(d)[4]<-"m_IT"#LIMPIAR DATOS EN LOS PARÉNTESIS O LOS SE. CONSULTAR CON NACHO
colnames(d)[5]<-"m_CV"#LIMPIAR DATOS EN LOS PARÉNTESIS O LOS SE. CONSULTAR CON NACHO
colnames(d)[6]<-"m_thoracic_lenght"#LIMPIAR DATOS EN LOS PARÉNTESIS O LOS SE. CONSULTAR CON NACHO
colnames(d)[7]<-"m_thoracic_width"#LIMPIAR DATOS EN LOS PARÉNTESIS O LOS SE. CONSULTAR CON NACHO
colnames(d)[8]<-"m_thoracic_depth"#LIMPIAR DATOS EN LOS PARÉNTESIS O LOS SE. CONSULTAR CON NACHO
colnames(d)[9]<-"m_thoracic_volume"#LIMPIAR DATOS EN LOS PARÉNTESIS O LOS SE. CONSULTAR CON NACHO
colnames(d)[10]<-"m_fresh_mass"#LIMPIAR DATOS EN LOS PARÉNTESIS O LOS SE. CONSULTAR CON NACHO
colnames(d)[11]<-"m_dry_mass"#LIMPIAR DATOS EN LOS PARÉNTESIS O LOS SE. CONSULTAR CON NACHO



#3) Add known missing columns (name, description, credit, doi)

d$country <- ifelse(d$location =="Maryland", "USA",
                    ifelse (d$location=="Ontario","Canada", NA))

d$site<- ifelse(d$location =="Maryland", "campus of the USDA Beltsville Agricultural Research Station",
                ifelse (d$location=="Ontario","St. Catharines", NA))
                    

d$doi<-"http://dx.doi.org/10.2317/JKES711.05.1"
d$name <- "Skandalis_2009"
d$description <-"Dataset about traits"

d$lat <-ifelse(d$location =="Maryland", "39",
               ifelse (d$location=="Ontario","43", NA))
d$long <-ifelse(d$location =="Maryland", "-76",
                 ifelse (d$location=="Ontario","-79", NA))



head(d)

#test and upload dataset
errors <- validateDataset(cnx, d)#Error: lexical error: invalid char in json text. <!DOCTYPE html> 	<html> 	  <hea (right here) ------^
temp <- parse_errors(errors)#This dataset is NOT valid Column #0 'genus' is not recognized and it will be ignored.Column #1 'species' is not recognized and it will be ignored.Column #2 'total_lenght' is not recognized and it will be ignored.Column #3 'IT' is not recognized and it will be ignored.Column #4 'sex' is not recognized and it will be ignored.

#NO species!



#Read data from Ascher_et_al_2016.csv-----

d <- read.csv("raw_data/Ascher_et_al_2016.csv", header = TRUE, sep = ";", dec= ",")
head(d)

d$local_id <- c(1:nrow(d))
colnames(d)[1]<-"genus"
colnames(d)[3]<-"total_lenght"
colnames(d)[4]<-"m_IT"
colnames(d)[5]<-"m_sex"

d$name <- "Ascher_et_al_2016"
d$description <-"Dataset about Megachile traits"
unique(d$genus)
d$genus2 <- gsub(" (Aethomegachile)", "", d$genus, fixed = TRUE)
d$genus2 <- gsub(" (Alocanthedon)", "", d$genus2, fixed = TRUE)
d$genus2 <- gsub(" (Callomegachile)", "", d$genus2, fixed = TRUE)
d$genus2 <- gsub("  (Callomegachile)", "", d$genus2, fixed = TRUE)
d$genus2 <- gsub(" (Chelostomoda)", "", d$genus2, fixed = TRUE)
d$genus2 <- gsub(" (Creightonella)", "", d$genus2, fixed = TRUE)
d$genus2 <- gsub(" (Eutricharaea)", "", d$genus2, fixed = TRUE)
d$genus2 <- gsub("  (Eutricharaea)", "", d$genus2, fixed = TRUE)
d$genus2 <- gsub(" (Paracella)", "", d$genus2, fixed = TRUE)
unique(d$genus2)
genus2 <- trimws(d$genus2)
d$species <- paste(d$genus2, d$SPECIE)
d$country<- "Singapore"
d$credit<-"Ascher et al. 2016. Journal, numero: http://zoobank.org/urn:lsid:zoobank.org:pub:0F042FC4-23A3-4C6F-8CDC-DDBAA412DB1A" #credit (referemnce: url)

d$contributor_name <- rep(NA, nrow(d)) 
d$contributor_name[1:5] <- c("J.S.","S.R.","Z.W.W.","J.X.Q.","E.J.Y.")
d$contributor_lastname <- rep(NA, nrow(d)) 
d$contributor_lastname[1:5] <- c("Ascher", "Risch", "Soh", "Lee", "Soh")
d$contributor_ORCID <- rep(NA, nrow(d)) 
d$contributor_ORCID[1:5] <- c("NA16", "NA38", "NA39", "NA40", "NA41")

head(d)
d <- d[,c("local_id","species","m_IT","m_sex",
          "name","description","country",
          "credit","contributor_name","contributor_lastname", "contributor_ORCID")]
head(d)

#test and upload dataset
errors <- validateDataset(cnx, d)
(temp <- parseErrors(errors))

to_rm <- d$species[temp[[2]]]
#simply ignore them.
importDataset(cnx, d[-which(d$species %in% to_rm),]) 

#Read data from  Hoehn_2008----------

d <- read.csv("raw_data/Hoehn_2008.csv", header = TRUE, sep = ";", dec= ",")
head(d)

d$local_id <- c(1:nrow(d))


d$name <- "Hoehn_2008"
d$description <-"Dataset about body sizes"
d$species <- paste(d$genus, d$specie)
unique(d$species)
colnames(d)[3] <- "m_IT"
colnames(d)[4] <- "se_IT"
colnames(d)[5] <- "n_IT"


d$location<-"Lore Lindu National Park, Central Sulawesi"
d$lat<- "-1.5"
d$long<- "120.03333333333333"
d$country<- "Indonesia"
d$doi<-"10.1098/rspb.2008.0405 " 

d$contributor_name <- rep(NA, nrow(d)) 
d$contributor_name[1:4] <- c("P.","T.","J.M.","I.")
d$contributor_lastname <- rep(NA, nrow(d)) 
d$contributor_lastname[1:4] <- c("Hoehn", "Tscharntke", "Tylianakis", "Steffan-Dewenter")
d$contributor_ORCID <- rep(NA, nrow(d)) 
d$contributor_ORCID[1:4] <- c("N42", "N43", "N44", "N45")

head(d)

d <- d[,c("local_id","species","m_IT","se_IT","n_IT","name","description",
          "location","country","lat","long","doi","contributor_name","contributor_lastname",
          "contributor_ORCID")]
head(d)

#test and upload dataset
errors <- validateDataset(cnx, d)
(temp <- parseErrors(errors))

to_rm <- d$species[c(temp[[2]], 10)]
#simply ignore them.
importDataset(cnx, d[-which(d$species %in% to_rm),]) 


#Read data from  Hagen_2013----------

d <- read.csv("raw_data/Hagen_2013.csv", header = TRUE, sep = ";", dec= ",")
head(d)

d$local_id <- c(1:nrow(d))
colnames(d)[3] <- "m_parasitism"

d$name <- "Hagen_2013"
d$description <-"Dataset about life type"
d$species <- paste(d$genus, d$specie)
d$location<-" Aarhus"
d$country<- "Denmark"
d$doi<-"10.1007/s00040-013-0290-x" 

d$contributor_name <- rep(NA, nrow(d)) 
d$contributor_name[1:2] <- c("M.","Y.L.")
d$contributor_lastname <- rep(NA, nrow(d)) 
d$contributor_lastname[1:2] <- c("Hagen", "Dupont")
d$contributor_ORCID <- rep(NA, nrow(d)) 
d$contributor_ORCID[1:2] <- c("NA46", "NA47")

#NO LAT AND LONG DATA

d <- d[,c("local_id","species","m_parasitism", "name","description",
          "location","country","doi","contributor_name","contributor_lastname", 
          "contributor_ORCID")]
head(d)


#test and upload dataset
errors <- validateDataset(cnx, d)
(temp <- parseErrors(errors))

to_rm <- d$species[temp[[2]]]
#simply ignore them.
importDataset(cnx, d[-which(d$species %in% to_rm),]) 


#Read data from  Burkle_2013----------

d <- read.csv("raw_data/Burkle_2013.csv", header = TRUE, sep = ",", dec= ",")
head(d)

d$local_id <- c(1:nrow(d))

colnames(d)[3] <- "n_plant_species"
colnames(d)[1] <- "m_plant_species"
colnames(d)[2] <- "species"

d$name <- "Burkle_2013"
d$description <-"The paper use historic data sets to quantified the degree to which global change over 120 
years disrupted plant-pollinator interactions."
d$location<-"Carlinville, Illinois"
d$country<-"USA"
d$doi<-"10.1126/science.1232728"


d$contributor_name <- rep(NA, nrow(d)) 
d$contributor_name[1:3] <- c("L.A.","J.C.","T.M.")
d$contributor_lastname <- rep(NA, nrow(d)) 
d$contributor_lastname[1:3] <- c("Burkle", "Marlin","Knight")
d$contributor_ORCID <- rep(NA, nrow(d)) 
d$contributor_ORCID[1:3] <- c("NA46", "NA47","NA48") #a best aproach would be to use first_last name...


#split plant
position <- regexpr(pattern = "_", d$m_plant_species)
d$m_plant_genus <- substr(d$m_plant_species, 1, position-1)
d$plant_species2 <- substr(d$m_plant_species, position+1, nchar(as.character(d$m_plant_species)))
d$m_plant_species <- paste(d$m_plant_genus, d$plant_species2)
#split bee
position <- regexpr(pattern = "_", d$species)
d$genus <- substr(d$species, 1, position-1)
d$species2 <- substr(d$species, position+1, nchar(as.character(d$species)))
d$species <- paste(d$genus, d$species2)

head(d)

d <- d[,c("local_id", "m_plant_genus","m_plant_species", "species","n_plant_species",
          "name","description",
          "location","country","doi","contributor_name","contributor_lastname",
          "contributor_ORCID")]

head(d)


#test and upload dataset
errors <- validateDataset(cnx, d)
temp <- parseErrors(errors) 

#to_rm <- d$species[temp[[2]]]
#simply ignore them.
importDataset(cnx, d) 

#NOT WORKING!

#Read data from  Fowler_2016----------

d <- read.csv("raw_data/Fowler_2016.csv", header = TRUE, sep = ";", dec= ",")
head(d)

colnames(d)[1]<-"genus"
colnames(d)[2]<-"species"
colnames(d)[3]<-"m_plant_genus"
d$species <- paste(d$genus, d$species)
head(d)

split_genus1 <- strsplit(as.character(d$m_plant_genus), ',')
d2 <- data.frame(m_plant_genus=unlist(split_genus1), species=rep(d$species, lengths(split_genus1)))
unique(as.character(d2$m_plant_genus))
d <- d2
d$m_plant_genus <- trimws(d$m_plant_genus)
d$m_floral_specialization <- "oligolectic"

unique(d$species)
d$species <- gsub("**", "", d$species, fix = TRUE)
d$species <- gsub("*", "", d$species, fix = TRUE)
d$local_id <- c(1:nrow(d))
d$name <- "Fowler_2016"
d$description <-"Webpage about specialist bees and pollinator-plant interactions."
d$location<-"Mid-Atlantic and Northeastern United States"
d$country<-"USA"
d$credit<-"http://jarrodfowler.com/specialist_bees.html"

d$contributor_name <- rep(NA, nrow(d)) 
d$contributor_name[1:2] <- c("J.","S.")
d$contributor_lastname <- rep(NA, nrow(d)) 
d$contributor_lastname[1:2] <- c("Fowler", "Droege")
d$contributor_ORCID <- rep(NA, nrow(d)) 
d$contributor_ORCID[1:2] <- paste(d$contributor_name[1:2], d$contributor_lastname[1:2])


d <- d[,c("local_id", "species", "m_floral_specialization", "m_plant_genus", "name", "description",
          "location","country","credit","contributor_name","contributor_lastname",
          "contributor_ORCID")]

head(d)

#test and upload dataset
errors <- validateDataset(cnx, d)
(temp <- parseErrors(errors))

to_rm <- d$species[temp[[2]]]
#simply ignore them.
importDataset(cnx, d[-which(d$species %in% to_rm),]) 


#Read data from  Mafalda_2017----------


d <- read.csv("raw_data/Mafalda_2017.csv", header = TRUE, sep = ";")
head(d, 12)

colnames(d)[1]<-"genus"
colnames(d)[2]<-"species"
colnames(d)[3]<-"m_floral_specialization"
levels(d$m_floral_specialization) <- c(NA, "polylectic", "oligolectic",  "polylectic")
colnames(d)[4]<-"m_sociality"
levels(d$m_sociality) <- c(NA, "solitary", "social", "solitary")
#rescue paratisism (NEXT ROUND)

colnames(d)[5]<-"m_nest_site"
levels(d$m_nest_site) <- c(NA,"cavity","cavity",NA, "soil")  
d$species <- paste(d$genus, d$species)
unique(as.character(d$species))
d$m_plant_species <- paste(d$plant_genus, d$plant_specie)
unique(d$m_plant_species)
d$m_plant_genus <- d$plant_genus
d$local_id <- c(1:nrow(d))
d$name <- "Mafalda_2017"
d$description <-"Dataset about ecologycal traits and some interacctions data"
d$location<-"Tapada da Ajuda, Lisboa"
d$country<-"Portugal"
d$credit<-"http://hdl.handle.net/10451/27533"

d$contributor_name <- rep(NA, nrow(d)) 
d$contributor_name[1:1] <- c("M.N.C.C")
d$contributor_lastname <- rep(NA, nrow(d)) 
d$contributor_lastname[1:1] <- c("Rocha")
d$contributor_ORCID <- rep(NA, nrow(d)) 
d$contributor_ORCID[1:1] <- paste(d$contributor_name[1:1], d$contributor_lastname[1:1])


d <- d[,c("local_id", "species", "m_floral_specialization", "m_sociality",
          "m_nest_site", "m_plant_genus","m_plant_species","name","description",
          "location","country","credit","contributor_name","contributor_lastname",
          "contributor_ORCID")]
head(d)

#RELLENAR ESPACIOS EN BLANCO CON "NA"
d$m_plant_genus <- as.character(d$m_plant_genus)
d$m_plant_genus[which(d$m_plant_genus == "")] <- NA

d$m_plant_species <- as.character(d$m_plant_species)
unique(d$m_plant_species)
d$m_plant_species[which(d$m_plant_species == " ")] <- NA

#test and upload dataset
errors <- validateDataset(cnx, d)
(temp <- parseErrors(errors))

to_rm <- d$species[temp[[2]]]
#simply ignore them.
importDataset(cnx, d[-which(d$species %in% to_rm),]) 


#Read data from  Hupfenmüller_2014----------


d <- read.csv("raw_data/Hupfenmuller_2014.csv", header = TRUE, sep = ";", dec= ",")
head(d, 20)
d$body #discard.

colnames(d)[1]<-"family"
colnames(d)[2]<-"genus"
colnames(d)[4]<-"n_species"
colnames(d)[5]<-"interaction"
colnames(d)[6]<-"m_sociality"
levels(d$m_sociality) <- c(NA,"solitary", "social","solitary")
#rescue parasitism! (NEXT ROUND)

d$species <- paste(d$genus, d$specie)
unique(as.character(d$species))
d$local_id <- c(1:nrow(d))
d$name <- "Hupfenmuller_2014"
d$description <-"Dataset about ecologycal traits and abundance"
d$location<-"Upper Franconia, Bavaria"
d$country<-"Germany"
d$doi<-"10.1371/journal.pone.0104439"
d$year<-"2010"


d$contributor_name <- rep(NA, nrow(d)) 
d$contributor_name[1:3] <- c("S.","I.","A.")
d$contributor_lastname <- rep(NA, nrow(d)) 
d$contributor_lastname[1:3] <- c("Hupfenmüller","Steffan-Dewenter","Holzschuh")
d$contributor_ORCID <- rep(NA, nrow(d)) 
d$contributor_ORCID[1:3] <- paste(d$contributor_name[1:3], d$contributor_lastname[1:3])


d <- d[,c("local_id", "species","m_sociality","year","name","description",
          "location","country","doi","contributor_name","contributor_lastname",
          "contributor_ORCID")]
head(d)
#test and upload dataset
errors <- validateDataset(cnx, d)
(temp <- parseErrors(errors))

d <- subset(d, is.na(m_sociality) == FALSE)

errors <- validateDataset(cnx, d)
(temp <- parseErrors(errors))

to_rm <- d$species[temp[[2]]]
#simply ignore them.
importDataset(cnx, d[-which(d$species %in% to_rm),]) 


#Read data from  Barbir_2014----------
#NOT ADDED AS NO m_ provided.

d <- read.csv("raw_data/Barbir_2014.csv", header = TRUE, sep = ",", dec= ",")
head(d)

colnames(d)[1]<-"genus"
colnames(d)[5]<-"m_bsize" #this is body length in mm... 

d$species <- paste(d$genus, d$species)
d$local_id <- c(1:nrow(d))
d$name <- "Barbir_2014"
d$description <-"Dataset about size traits"
d$location<-" La Poveda, Arganda del Rey, Madrid"
d$country<-"Spain"
d$doi<-"10.1111/afe.12076"
d$year<-"2011/2012"
d$lat<-"40.3166667"
d$long<-"-3.4833333333333334"

d$contributor_name <- rep(NA, nrow(d)) 
d$contributor_name[1:4] <- c("J.","F.R.","C.","J.")
d$contributor_lastname <- rep(NA, nrow(d)) 
d$contributor_lastname[1:4] <- c("Barbir","Badenes-Pérez","Fernández-Quintanilla","Dorado")



d <- d[,c("local_id","genus", "species","m_bsize","year","name","description",
          "location","country","lat","long","doi","contributor_name","contributor_lastname")]

head(d)

#REVISAR M_BSIZE (BODY SIZE).

#test and upload dataset
errors <- validateDataset(cnx, d)#Error: lexical error: invalid char in json text.<!DOCTYPE html> 	<html> 	  <hea (right here) ------^
temp <- parse_errors(errors)#This dataset is NOT valid Column #1 'genus' is not recognized and it will be ignored.Column #2 'species' is not recognized and it will be ignored.Column #3 'floral_specialization' is not recognized and it will be ignored.Column #4 'sociality' is not recognized and it will be ignored.Column #5 'nest_location' is not recognized and it will be ignored.Column #6 'plant_genus' is not recognized and it will be ignored.Column #7 'plant_specie' is not recognized and it will be ignored.




#Read data from  Sydenham_2016----------
# NOT ADDED YET.

d <- read.csv("raw_data/Sydenham_2016.csv", header = TRUE, sep = ";", dec= ",")
head(d)
str(d)

colnames(d)[1]<-"genus"

d$species <- paste(d$genus, d$species)
d$local_id <- c(1:nrow(d))
d$name <- "Sydenham_2016"
d$description <-"Dataset about IT size"
d$country<-"Norway"
d$doi<-"10.1002/ece3.1871"
d$year<-"2009/2010/2013"

d$contributor_name <- rep(NA, nrow(d)) 
d$contributor_name[1:4] <- c("M.A.K.","L.D.","S.R.","K.")
d$contributor_lastname <- rep(NA, nrow(d)) 
d$contributor_lastname[1:4] <- c("Sydenham","Häusler","Moe","Eldegard")



d <- d[,c("local_id","genus", "species","N","m_IT", "n_IT","se_IT","year","name","description",
          "country","doi","contributor_name","contributor_lastname")]


#SIN LOCALIZACIÓN EXACTA. NO LAT, NO LONG. ¿CREAR COLUMNA PARA REMARCAR QUE SON CAVITY NESTING? LIMPIAR ESPACIOS
head(d)


#test and upload dataset
errors <- validateDataset(cnx, d)#Error: lexical error: invalid char in json text.<!DOCTYPE html> 	<html> 	  <hea (right here) ------^
temp <- parse_errors(errors)#This dataset is NOT valid Column #1 'genus' is not recognized and it will be ignored.Column #2 'species' is not recognized and it will be ignored.Column #3 'floral_specialization' is not recognized and it will be ignored.Column #4 'sociality' is not recognized and it will be ignored.Column #5 'nest_location' is not recognized and it will be ignored.Column #6 'plant_genus' is not recognized and it will be ignored.Column #7 'plant_specie' is not recognized and it will be ignored.

to_rm <- d$species[temp[[2]]]
#simply ignore them.
importDataset(cnx, d[-which(d$species %in% to_rm),]) 



#Read data from  Tur_2013----------


d <- read.csv("raw_data/Tur_2013.csv", header = TRUE, sep = ";", dec= ",")
head(d)


colnames(d)[1]<-"plant_family"
colnames(d)[2]<-"m_plant_genus"
unique(d$m_plant_genus)
colnames(d)[3]<-"plant_specie"
colnames(d)[4]<-"family"
colnames(d)[5]<-"genus"
colnames(d)[6]<-"species"
colnames(d)[7]<-"interaction"
colnames(d)[8]<-"site"

d$local_id <- c(1:nrow(d))
d$name <- "Tur_2013"
d$description <-"Dataset interactions frequencies"
d$location<- ifelse(d$site =="SB", "Son Bosc, Mallorca","Puig Major, Mallorca")
d$country<-"Spain"
#d$month<-ifelse(d$site =="SB", "4-7","5-8")
#d$year<-"2009/2010"
d$doi<-"10.1371/journal.pone.0078294"
d$m_plant_species <- paste(d$m_plant_genus, d$plant_specie)
unique(d$m_plant_species)
d$species <- paste(d$genus, d$species)
unique(d$species) #very dirty... let's see if validate can handle it.

d$contributor_name <- rep(NA, nrow(d)) 
d$contributor_name[1:3] <- c("C.","R.","A.")
d$contributor_lastname <- rep(NA, nrow(d)) 
d$contributor_lastname[1:3] <- c("Tur","Castro-Urgal","Travest")
d$contributor_ORCID <- rep(NA, nrow(d)) 
d$contributor_ORCID[1:3] <- paste(d$contributor_name[1:3], d$contributor_lastname[1:3])

d$lat <- ifelse(d$site =="SB", "39.774475","39.7998639")
d$long<-ifelse(d$site=="SB", "3.129261111111111", "2.7855027777777774" )

head(d)

d <- d[,c("m_plant_genus", "species", "local_id", "name", "description",
          "location", "country", "doi", "m_plant_species", "contributor_name",
          "contributor_lastname", "contributor_ORCID", "lat", "long")]

#test and upload dataset
errors <- validateDataset(cnx, d)
(temp <- parseErrors(errors))

to_rm <- d$species[temp[[2]]]
#simply ignore them.
importDataset(cnx, d[-which(d$species %in% to_rm),]) 
#NOT ENTERED; TOO MANY SPECIES ERRORS; NEED CLEANING:

#Read data from  Normandin_2016----------


d <- read.csv("raw_data/Normandin_2016.csv", header = TRUE, sep = ";", dec= ",")
head(d)

colnames(d)[1]<-"genus"
colnames(d)[4]<-"m_IT"
colnames(d)[5]<-"m_sociality"
levels(d$m_sociality) <- c(NA, "solitary", "solitary", "social", "solitary", "solitary")
#need to rescue parastism! NEXT ROUND
colnames(d)[8]<-"m_floral_specialization"
levels(d$m_floral_specialization) <- c("oligolectic", "polylectic")
colnames(d)[3]<-"m_nest_site"
levels(d$m_nest_site) <- c("other", NA,                    
                              "stem", "soil",                  
                              "cavity", 
                              "cavity",
                              "cavity")

d$local_id <- c(1:nrow(d))
d$name <- "Normandin_2016"
d$description <-"Dataset about ecological and morphological traits"
d$location<- "Montreal & Quebec"
d$country<-"Canada"
#d$year<-"2012/2013"
d$doi<-"10.7717/peerj.3051"
d$contributor_name <- rep(NA, nrow(d)) 
d$contributor_name[1:4] <- c("E.","N.J.","C.M.","V.")
d$contributor_lastname <- rep(NA, nrow(d)) 
d$contributor_lastname[1:4] <- c("Normandin","Vereecken","Buddle","Fournier")
d$contributor_ORCID <- rep(NA, nrow(d)) 
d$contributor_ORCID[1:4] <- paste(d$contributor_name[1:4] , d$contributor_lastname[1:4] )
d$lat<- "45.5011111" # & 46.8022222"
d$long<-"-73.65611111111112" # & -71.26388888888889" 
d$species <- paste(d$genus, d$specie)
unique(as.character(d$species))

head(d)
#LIMPIAR ESPACIOS Y COLUMNAS NO DESEADAS.
#DUDAS ACERCA DE CÓMO PONER LOCATION Y LAT/LONG EN ESTE PAPER. EN LA TABLA INTRODUCIDA 
#NO TENEMOS NADA QUE NOS DIFERENCIA ENTRE ZONAS, YA QUE NO SE HAN
#AÑADIDO LAS ABUNDANCIAS QUE APARECEN EN OTRAS TABLAS. HAY TABLAS TAMBIÉN 
#DE LOS DISTINTOS SITES (MUCHOS), CADA UNO CON SU LAT/LONG, AUNQUE CREO QUE ES INFORMACIÓN
#NO ESENCIAL. 

colnames(d)
d <- d[,c("m_nest_site", "m_IT", "m_sociality", "m_floral_specialization",
          "local_id", "name", "description", "location", "country", "doi",
          "contributor_name", "contributor_lastname", "contributor_ORCID",
          "lat", "long", "species")]

#test and upload dataset
errors <- validateDataset(cnx, d) 
(temp <- parseErrors(errors))

to_rm <- d$species[c(temp[[2]], 193)]
#simply ignore them.
importDataset(cnx, d[-which(d$species %in% to_rm),]) 




#Read data from  Macior_1974----------

d <- read.csv("raw_data/Macior_1974.csv", header = TRUE, sep = ",", dec= ",")
head(d)

colnames(d)[1]<-"species"
unique(d$species)
colnames(d)[2]<-"m_tongue_length"
colnames(d)[3]<-"se_tongue_length"

d$local_id <- c(1:nrow(d))
d$name <- "Macior_1974"
d$credit<-"Pollination ecology of the Front Range of the Colorado Rocky Mountains, 1974. Macior, L.W. Melanderia 15 :1-59."
d$description <-"Dataset about tongue length measures"
d$location<-"Colorado Rocky Mountains"
d$country<-"U.S.A."
d$year<-"1974"


d$contributor_name <- rep(NA, nrow(d)) 
d$contributor_name[1:1] <- c("L.W.")
d$contributor_lastname <- rep(NA, nrow(d)) 
d$contributor_lastname[1:1] <- c("Macior")
d$contributor_ORCID <- rep(NA, nrow(d)) 
d$contributor_ORCID[1:1] <- c("L.W. Macior")

colnames(d)
d <- d[,c("local_id","species","m_tongue_length","se_tongue_length","year","name",
          "description","country","location",
          "contributor_name","contributor_lastname","credit", "contributor_ORCID")]

head(d)

#test and upload dataset
errors <- validateDataset(cnx, d)
(temp <- parseErrors(errors)) 

d <- subset(d, is.na(m_tongue_length) == FALSE)

to_rm <- d$species[temp[[2]]]
#simply ignore them.
importDataset(cnx, d[-which(d$species %in% to_rm),]) 


#ESTOS DATOS LOS HE ENCONTRADO A TRAVÉS DE OTRO ARTÍCULO:http://datadryad.org/resource/doi:10.5061/dryad.10278. 
#HE INTENTADO IR A LA FUENTE ORIGINAL, EL ARTÍCULO DE MACIOR, PERO NO HE ENCONTRADO PRÁCTICAMENTE NADA. 


##Data from Kendall et al., 2018 ------

#1) Read data 

load("raw_data/pollimetry_dataset.rdata")
pollimetry_dataset -> d
head(d)
unique(d$Family)
d <- subset(d, Family != "Syrphidae")
d <- droplevels(d)
d$local_id <- d$Tag
d$species <- gsub("_", " ", d$Species)
unique(d$species)
#remove spp, sp, sp1, brasp1, etc..
d <- d[-grep("spp", d$species, fixed = TRUE, value = FALSE),]
d <- d[-grep("sp[0-9]", d$species, value = FALSE),]
unique(d$species)
#let's see if validate catch the rest.

d$country <- d$Country
d$location <- d$Location
d$Spec.wgt -> d$m_dry_mass 
d$IT -> d$m_IT 
d$doi <- "10.5281/zenodo.1313905" 
d$name <- "Kendall_2018"
d$m_sex <- d$Sex
unique(d$Plant)
d$description <- "Dataset with measures of dry weight, IT"
d$lat <- d$Latitude 
d$long <- d$Longitude
#date
date <- as.POSIXlt(strptime(d$Col.date, "%d-%m-%y")) #convert to date class
d$day <- date$mday #extract the day only
d$month <- date$mon+1 #extract the month only
d$year <- date$year + 1900 #extract the year only

d$contributor_name <- rep(NA, nrow(d)) 
d$contributor_name[1:19] <- c("Liam K.", "Romina", "Vesna", "Daniel P.", "Matthias",
                             "Katherine C.R.", "Breno M.", "Mark", "Andrea",
                             "Francisco P.", "Joanne M.", "Janaely S."
                             , "Zachary M.", "Stuart P.M.", "Juanita", "Laura", 
                             "Louis", "Nicolas J.", "Ignasi") 
d$contributor_lastname <- rep(NA, nrow(d)) 
d$contributor_lastname[1:19] <- c("Kendall", "Rader", "Gagic", "Cariveau", "Albrecht",
                                  "Baldock", "Freitas", "Hall", "Holzschuh", 
                                  "Molina", "Morten", "Pereira", "Portman", "Roberts",
                                  "Rodriguez", "Russo", "Sutter", "Vereecken", "Bartomeus")
d$contributor_ORCID <- rep(NA, nrow(d)) 
d$contributor_ORCID[1:19] <- paste(d$contributor_name[1:19], d$contributor_lastname[1:19])
d$contributor_ORCID[c(4,19)] <- c("NA13", "0000-0001-7893-4389") 

#4) Remove unused columns
head(d)
d <- d[,c("local_id", "species", "lat", "long", "country", "location", "m_sex", "day", "month", "year",
          "m_dry_mass", "m_IT", "doi", "name", "description", "contributor_name", "contributor_lastname",
          "contributor_ORCID")]

#5 test and upload dataset

head(d)
errors <- validateDataset(cnx, d)
(temp <- parseErrors(errors))

d[which(d$year > 2017),"year"] <- 2016

errors <- validateDataset(cnx, d)
(temp <- parseErrors(errors))

to_rm <- d$species[c(temp[[2]], 776, 782, 1190, 1798, 2202)]
#simply ignore them.
importDataset(cnx, d[-which(d$species %in% to_rm),]) #

##Data from Hofmann et al., 2019 ------

#1) Read data 
d <- read.csv("raw_data/Hofmann_etal_2019.csv", header = TRUE, sep = ",")
str(d)
head(d)

#species has authority in the name. Find second space and remove everything after that
position <- regexpr(pattern = " .*?( )", d$Species, perl = TRUE)
position <- as.numeric(attr(position, 'capture.start'))
d$Species <- substr(d$Species, 0, position-1) 

#2) Check observations colnames ("local_id", "species","collector","taxonomist",
#"day","month","year","lat","long","location","country")
#Add lat long from google maps or paper if possible.
#Check traits colnames(m_trait, se_trait, n_trait)
colnames(d)

#first deal with cuckoo bees
d$parasitism <- 'no'
d$parasitism[d$Pollen.specialisation == 'cuckoo'] <- 'yes'
d$Sociality[d$parasitism == 'yes'] <- NA
d$Pollen.specialisation[d$parasitism == 'yes'] <- NA

#then rename nest location names
levels(d$Nest.location)
# above --> other
# ground --> soil
# ground and above --> other
# host nest --> NA
library(plyr)
d$Nest.location <- revalue(d$Nest.location, c("above"="other", "ground"="soil",
                           "ground and above"="other", "host nest"= 'other'))
d$Nest.location[d$parasitism == 'yes'] <- NA


#3) Add known missing columns (name, description, credit, doi)
#Add contributor information (contributor_name, contributor_lastname, contributor_ORCID)
#Do not look for other contributor info in detail.

d$country <- 'Germany'
d$name <- 'Hofmann et al. 2019'
d$description <- 'Dataset of life history of Central European Bees'
d$doi <- "10.1098/rspb.2019.0316"
d$contributor_name <- rep(NA, nrow(d)) 
d$contributor_name[1:3] <- c('Michaela M.', 'Constantin M.', 'Susanne S.')
d$contributor_lastname <- rep(NA, nrow(d)) 
d$contributor_lastname[1:3] <- c('Hofmann', 'Zohner', 'Renner')
d$contributor_ORCID <- rep(NA, nrow(d)) 
d$contributor_ORCID[1:3] <- c('NA14', '0000-0002-8302-4854', '0000-0003-3704-0703')

#4) Remove unused columns
d <- d[,c('Species', 'Pollen.specialisation', 'Nest.location','Sociality','Size.mean',
     "parasitism", "country", "doi", "name", "description", "contributor_name", "contributor_lastname",
     "contributor_ORCID")]
names(d) <- c('species', 'm_floral_specialization', 'm_nest_site','m_sociality','m_body_size',
              "m_parasitism", "country", "doi", "name", "description", "contributor_name", "contributor_lastname",
              "contributor_ORCID")

#5 test and upload dataset

head(d)

d$species <- revalue(d$species, c('Eucera dentata' = 'Tetraloniella dentata',
                     'Andrena scotica' = 'Andrena carantonica',
                     'Osmia lepeletieri' = 'Hoplitis lepeletieri',
                     'Osmia adunca' = 'Hoplitis adunca',
                     'Osmia tuberculata' = 'Hoplitis tuberculata',
                     'Eucera macroglossa' = 'Tetralonia malvae',
                     'Coelioxys conica' = 'Coelioxys quadridentata',
                     'Osmia villosa' = 'Hoplitis villosa',
                     'Osmia tridentata' = 'Hoplitis tridentata',
                     'Osmia papaveris' = 'Hoplitis papaveris',
                     'Osmia loti' = 'Hoplitis loti',
                     'Osmia bicornis' = 'Osmia rufa',
                     'Osmia anthocopoides' = 'Hoplitis anthocopoides',
                     'Osmia acuticornis' = 'Hoplitis acuticornis',
                     'Halictus eurygnathus' = 'Halictus compressus',
                     'Anthidium byssinum' = 'Trachusa byssina',
                     'Osmia ravouxi' = 'Hoplitis ravouxi',
                     'Osmia claviventris' = 'Hoplitis claviventris',
                     'Eucera alticincta' = 'Tetraloniella alticincta',
                     'Rhophitoides canus' = 'Rophites canus',
                     'Osmia spinulosa' = 'Hoplosmia spinulosa',
                     'Anthidium nanum' = 'Pseudoanthidium scapulare',
                     'Osmia truncorum' = 'Heriades truncorum',
                     'Osmia crenulata' = 'Heriades crenulatus',
                     'Hylaeus dilatatus' = 'Hylaeus annularis',
                     'Osmia leucomelana' = 'Hoplitis leucomelana',
                     'Osmia rapunculi' = 'Chelostoma rapunculi',
                     'Eucera salicariae' = 'Tetraloniella salicariae',
                     'Anthidium strigatum' = 'Anthidiellum strigatum'))

errors <- validateDataset(cnx, d)
(temp <- parseErrors(errors))


to_rm <- d$species[temp[[2]]]
#simply ignore them.
importDataset(cnx, d[-which(d$species %in% to_rm),]) 



#----
#priority would Asensio localities.
#De Palma papers?




