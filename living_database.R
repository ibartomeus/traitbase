#this script upload raw data located at raw_data folder and produces 
#input for traitbase. By running this script you cna recreate the database 
#from scratch.

#source helper functions and packages
source("R/clean_species.R")
source("R/help_functions.R")
library(taxize)
library(reshape2)
#library(devtools)
#install_github("metadevpro/traitbaser", force = TRUE) #works with basic R...
library(traitbaser)
cnx <- connect(url = "http://traitbase-qa.herokuapp.com/", "demo", "1234")
#cnx <- connect(url = "http://www.traitbase.info", "demo", "1234")

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

#"contributor_name": Who to give credit name. Usa as many rows as contributors. Rest can be NA.
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
d$contributor_name <- rep(NA, nrow(d)) #create an empty column
d$contributor_name[1:4] <- c("MO", "BM", "J", "D") #populate the first forut rows
d$contributor_lastname <- rep(NA, nrow(d)) #create an empty column
d$contributor_lastname[1:4] <- c("Oliveira", "Freitas", "Scheper", "Kleijn") #populate the first forut rows

#4) Remove unused columns
head(d)
d <- d[,c("local_id", "species",           
          "day", "month", "year", "country", 
          "m_IT", "n_IT", "se_IT", "m_sex",
          "n_sex", "se_sex",
          "doi", "name", "description", 
          "contributor_name", "contributor_lastname")]

#5) test and upload dataset
head(d)
#txt <- df_to_rl(d)
errors <- validateDataset(cnx, d)
parse_errors(errors)

unique(d$month) #great catch!
d[which(d$month > 12),"month"] <- c(7,4,5)

head(d)
errors <- validateDataset(cnx, d)
parse_errors(errors)

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
d$contributor_name <- rep(NA, nrow(d)) #create an empty column
d$contributor_name[1:6] <- c("S", "X", "A", "A","R", "J") 
d$contributor_lastname <- rep(NA, nrow(d)) #create an empty column
d$contributor_lastname[1:6] <- c("Osorio-Canadas", "Arnan", "Rodrigo", "Torne-Noguera", "Molowny", "Bosch") #populate the first forut rows


#4) Remove unused columns

d <- d[,c("local_id", "species", "country",
          "m_IT", "se_IT", "n_IT",
          "doi", "name", "description", 
          "contributor_name", "contributor_lastname")]
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
parse_errors(errors)

#clean species!
temp <- clean_species(d$species) #SLOWWWW needs user inputs.
temp$final_names <- as.character(d$final_names)
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
d$m_fresh_mass <- d$m_fresh_mass/10 #NOt mg... crap...
colnames(d)[3] <- "n_fresh_mass" 
d$m_sex <- d$sex
d$n_sex <- d$n_fresh_mass


#3) Add known missing columns (name, description, credit, doi)

#Add doi, no doi found in crossref
#d$doi <- No doi # NB you can check if there is doi here: http://www.questionpoint.org/crs/servlet/org.oclc.ask.AskPatronFetchQA?&language=1&qid=196591
d$name <- "Stone_1989"
d$credit <- "Stone, G. N., and P. G. Willmer. 1989. “Warm-Up Rates and Body Temperatures in Bees: The Importance of Body Size, Thermal Regime and Phylogeny.” The Journal of Experimental Biology 147 (1): 303–28.9"
d$description <- "Dataset with body mass and minimum ambient temperature for foraging"
#the follwing lines are not necesary as there is doi, but for completness of the example
d$contributor_name <- rep(NA, nrow(d)) #create an empty column
d$contributor_name[1:2] <- c("G.N.", "P.G.") 
d$contributor_lastname <- rep(NA, nrow(d)) #create an empty column
d$contributor_lastname[1:2] <- c("Stone", "Willmer") #populate the first forut rows

#4) Remove unused columns

d <- d[,c("local_id", "species", "credit",
          "m_fresh_mass", "n_fresh_mass",
          "m_sex", "n_sex",
          "name", "description", 
          "contributor_name", "contributor_lastname")]

#5) test and upload dataset
head(d)
errors <- validateDataset(cnx, d)
temp <- parse_errors(errors)

#corregir
temp <- clean_species(d$species) 
temp

d$species <- temp$final_names

errors <- validateDataset(cnx, d)
temp <- parse_errors(errors)

#errors que quedan:

to_rm <- d$species[temp[[2]]]
#simply ignore them.
importDataset(cnx, d[-which(d$species %in% to_rm),]) #only remove first instance!

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
d$contributor_name <- rep(NA, nrow(d)) 
d$contributor_name[1:2] <- c("B.J") 
d$contributor_lastname <- rep(NA, nrow(d)) 
d$contributor_lastname[1:2] <- c("Borrell") 

#4) Remove unused columns

d <- d[,c("local_id", "species", "country","m_fresh_mass", "m_tongue_length",
          "doi", "name", "description", 
          "contributor_name", "contributor_lastname")]
head(d)

#5 test and upload dataset

head(d)
errors <- validateDataset(cnx, d)
(temp <- parse_errors(errors))

to_rm <- d$species[temp[[2]]]
#simply ignore them.
importDataset(cnx, d[-which(d$species %in% to_rm),]) #only remove first instance!


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
d$contributor_name <- rep(NA, nrow(d)) 
d$contributor_name[1:7] <- c("D.P.", "G.K.","I.", "J.", "J.", "J.", "R." ) 
d$contributor_lastname <- rep(NA, nrow(d)) 
d$contributor_lastname[1:7] <- c("Cariveau", "Nayak", "Bartomeus", "Zientek", "Ascher", "Gibbs", "Winfree") 

#4) Remove unused columns

d <- d[,c("local_id", "species", "country", "location", "m_tongue_length", "n_tongue_length", 
          "m_IT", "n_IT", "doi", "name", "description", "contributor_name", "contributor_lastname")]

#5 test and upload dataset

head(d)
errors <- validateDataset(cnx, d)
(temp <- parse_errors(errors))
to_rm <- d$species[temp[[2]]]
#simply ignore them.
importDataset(cnx, d[-which(d$species %in% to_rm),]) #only remove first instance!

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
levels(d$m_voltinism) <- c("multivoltine", "univoltine")
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
d$contributor_name <- rep(NA, nrow(d)) 
d$contributor_name[1:7] <- c("I.", "J.", "J.", "B.", "D.", "S.", "R.") 
d$contributor_lastname <- rep(NA, nrow(d)) 
d$contributor_lastname[1:7] <- c("Bartomeus", "Ascher", "Gibbs", "Danforth", "Wagner", "Hedtke", "Winfree") 

#4) Remove unused columns

d <- d[,c("local_id", "species", "country", "m_nest_site", "m_sociality", "m_parasitic", 
          "m_sex", "m_IT", "m_voltinism", "description", "m_floral_specialization","doi",
          "name", "contributor_name", "contributor_lastname")]

#5) test and upload dataset

head(d)
errors <- validateDataset(cnx, d)
(temp <- parse_errors(errors))

to_rm <- d$species[temp[[2]]]
#simply ignore them.
importDataset(cnx, d[-which(d$species %in% to_rm),]) #only remove first instance!

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
d$contributor_name <- rep(NA, nrow(d)) 
d$contributor_name[1:2] <- c("C.", "L.") 
d$contributor_lastname <- rep(NA, nrow(d)) 
d$contributor_lastname[1:2] <- c("Kremen", "Gonigle") 

#4) Remove unused columns


d <- d[,c("local_id", "species", "country", "location", "m_sociality",  "m_dietary_specialization", 
          "m_IT",  "description","doi", "name", "contributor_name", "contributor_lastname")]


#5) test and upload dataset

head(d)
errors <- validateDataset(cnx, d) #split!
(temp <- parse_errors(errors))

to_rm <- d$species[temp[[2]]]
#simply ignore them.
importDataset(cnx, d[-which(d$species %in% to_rm),]) #only remove first instance!


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
d$lat <- "40-13-35N" #FIX THAT!
d$long <- "28-52-13E" #FIX THAT!
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

head(d)
errors <- validateDataset(cnx, d)
(temp <- parse_errors(errors))

to_rm <- d$species[temp[[2]]]
#simply ignore them.
importDataset(cnx, d[-which(d$species %in% to_rm),]) #only remove first instance!


#Read data from Forrest et al., 2015-----


#1) Read data 

d <- read.csv("raw_data/Forrest_2015.csv", header = TRUE, sep = ";", dec= ",")

#2) Check observations colnames

head(d)
colnames(d)[1] <- "species"
colnames(d)[5] <- "m_IT"
colnames(d)[10] <- "m_sociality" 
colnames(d)[11] <- "m_floral_specialization"  #MODIFY!!! yo polylectic/oligolectic
d$m_floral_specialization <- as.factor(d$m_floral_specialization)
levels(d$m_floral_specialization) <- c("oligolectic", "polylectic", NA) #check NA!
#Add nesting behaviour/location!
levels(d$m_sociality) <- c("solitary", "social" ,  "solitary") #check NA!

#3) Add known missing columns 

d$local_id <- NA
d$country <- "United States"
d$location <- "Sacramento Valley, California"
d$doi <- "10.1111/1365-2664.12433" 
d$name <- "Forrest_2015"
d$description <- ""
d$Contributor_name <- rep(NA, nrow(d)) 
d$Contributor_name[1:3] <- c("J.R.K.", "R.W.","C.") 
d$Contributor_lastname <- rep(NA, nrow(d)) 
d$Contributor_lastname[1:3] <- c("Forrest", "Thorp", "Kremen") 

#4) Remove unused columns

d <- d[,c("local_id", "species", "country", "location", "m_IT", "m_sociality", "m_floral_specialization",
         "description","doi", "name", "Contributor_name", "Contributor_lastname")]

#5) test and upload dataset

head(d)
errors <- validateDataset(cnx, d)
(temp <- parse_errors(errors))

to_rm <- d$species[temp[[2]]]
#simply ignore them.
importDataset(cnx, d[-which(d$species %in% to_rm),]) #only remove first instance!


#Read data from Carstensen_et_al_2012-----

#1) Read data 

d <- read.csv("raw_data/Carstensen_et_al_2012.csv", 
              header = TRUE, sep =";", dec= ",", na.strings = c("", "-"))

#2) Check observations colnames

d$local_id <- c(1:nrow(d))
colnames(d)[1]<- "Site"
colnames(d)[3] <- "plant_species"
colnames(d)[4] <- "species"

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
d$Contributor_name <- rep(NA, nrow(d)) 
d$Contributor_name[1:4] <- c("D.W.", "M.", "K.", "L.P.C.") 
d$Contributor_lastname <- rep(NA, nrow(d)) 
d$Contributor_lastname[1:4] <- c("Carstensen", "Sabatino", "Trøjelsgaard", "Morellato")


#Add lat/long per site and maybe keep in location via 
levels(d$Site)
d$location <- paste(d$location, ":", d$Site)
d$lat <- ifelse(d$Site =="Cedro", "-19.2320778",
                ifelse (d$Site=="Gigante","-19.2473083",
                        ifelse (d$Site=="Paulino","-19.2553111",
                                ifelse (d$Site=="Tinkerbell","-19.220725",
                                        ifelse (d$Site=="Midway", "-19.2702972",
                                                ifelse (d$Site=="Elefante", "-19.2934528",
                                                        ifelse (d$Site=="Soizig", "-19.2728028",NA)))))))
d$long <- ifelse(d$Site =="Cedro","-43.576394444444446",
                 ifelse (d$Site=="Gigante","-43.510197222222224",
                         ifelse (d$Site=="Paulino","-43.583869444444446",
                                 ifelse (d$Site=="Tinkerbell","-43.58296388888889",
                                         ifelse (d$Site=="Midway", "-43.550352777777775",
                                                 ifelse (d$Site=="Elefante", "-43.55553333333333",
                                                         ifelse (d$Site=="Soizig", "-43.57983611111111",NA))))))) #casi lo tenias

head(d)
str(d)
print(d$lat)
print(d$long)
print(d$Site)

#4) Remove unused columns ...



#5) Upload dataset



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
colnames(d)[4]<-"m_IT"
colnames(d)[5]<-"tongue_lenght"
colnames(d)[7]<- "Sociality"
colnames(d)[8]<-"nest_location"
colnames(d)[3]<-"n_IT" #check!

#3) Add known missing columns (name, description, credit, doi)
d$country <- "France"
d$location <- "Grand Lyon"
d$doi <- "10.1371/journal.pone.0104679"
d$name <- "Fortel_et_al_2014"
d$description <- "Dataset about traits"
d$Contributor_name <- rep(NA, nrow(d)) 
d$Contributor_name[1:8] <- c("L.","M.","L.","A.L.", "M.", "H.", "O.","B.") 
d$Contributor_lastname <- rep(NA, nrow(d)) 
d$Contributor_lastname[1:8] <- c("Fortel", "Henry", "Guilbaud", "Guirao","Kuhlmann","Mouret","Rollin","Vaissière")

d$lat <-"45.7666667"
d$long <-"4.833333333333333"

position <- regexpr(pattern = " ", d$species)
d$m_genus <- substr(d$species, 1, position-1) #WRONG!
d$m_species <- substr(d$species, position+1, nchar(as.character(d$species)))
d$n_genus <- 1
d$n_species <- 1
d$se_genus <- 0
d$se_species <- 0

#4) Remove unused columns


#5) Upload dataset


head(d)


#Read data from Gonzalez_et_al_Tabla_1_1999.csv-----

#1) Read data (read.table, read.csv...)

d <- read.csv("raw_data/Gonzalez_et_al_Tabla_1_1999.csv", 
              header = TRUE, sep =";", dec= ",", na.strings = c("", "-"))

#2) Check observations colnames ("local_id", "species","collector","taxonomist",
#"day","month","year","lat","long","location","country")

d$local_id <- c(1:nrow(d))
colnames(d)[1]<-"Date"
colnames(d)[2]<-"family"
colnames(d)[3]<-"genus"
colnames(d)[4]<-"species"

#3) Add known missing columns (name, description, credit, doi)

date <- as.POSIXlt(strptime(d$Date, "%d/%m/%Y")) #convert to date class
d$day <- date$mday #extract the day only
d$month <- date$mon+1 #extract the day only
d$year <- date$year + 1900 #extract the day only

d$country <- "Spain"
d$location <- "Viana de Cega"
d$credit <- "Zoologica Baetica vol. 10, 87-111"
d$name <- "Gonzalez_et_al_Tabla_1_1999"
d$description <-"Dataset about relationship of species studied, with indication of the number of specimens collected
during each of the sampling periods. "

d$lat <-"41.5129466"
d$long <-"-4.758804199999986"


#4) Remove unused columns


#5) Upload dataset
head(d)




#Read data from Skandalis_2009.csv-----


d <- read.csv("raw_data/Skandalis_2009_.csv", header = TRUE, sep = ";", dec= ",", encoding = "") #A mi no me carga¿? Quizas hay que editar caracteres raros de la tabla original.
d$local_id <- c(1:nrow(d))

#2) Check observations colnames ("local_id", "species","collector","taxonomist",
#"day","month","year","lat","long","location","country")

d$local_id <- c(1:nrow(d))
colnames(d)[3]<-"IT"
colnames(d)[9]<-"fresh_mass"
colnames(d)[10]<-"dry_mass"


#3) Add known missing columns (name, description, credit, doi)

d$country <- ifelse(d$Location =="Maryland", "USA",
                    ifelse (d$Location=="Ontario","Canada", NA))

d$site<- ifelse(d$Location =="Maryland", "campus of the USDA Beltsville Agricultural Research Station",
                ifelse (d$Location=="Ontario","St. Catharines", NA))
                    

d$doi<-"http://dx.doi.org/10.2317/JKES711.05.1"
d$name <- "Skandalis_2009"
d$description <-"Dataset about traits"

d$lat <-ifelse(d$Location =="Maryland", "39",
               ifelse (d$Location=="Ontario","43", NA))
d$long <-ifelse(d$Location =="Maryland", "-76",
                 ifelse (d$Location=="Ontario","-79", NA))

head(d)


#4) Remove unused columns


#5) Upload dataset


head(d)



#Read data from Ascher_et_al_2016.csv-----

d <- read.csv("raw_data/Ascher_et_al_2016.csv", header = TRUE, sep = ";", dec= ",")
head(d)

d$local_id <- c(1:nrow(d))
colnames(d)[1]<-"genus"
colnames(d)[2]<-"specie"
colnames(d)[3]<-"total_lenght"

d$name <- "Ascher_et_al_2016"
d$description <-"Dataset about Megachile traits"
d$species <- paste(d$Genus, d$Specie)
d$country<- "Singapore"
d$credit<-"Ascher et al. 2016. Journal, numero: http://zoobank.org/urn:lsid:zoobank.org:pub:0F042FC4-23A3-4C6F-8CDC-DDBAA412DB1A" #credit (referemnce: url)

d$Contributor_name <- rep(NA, nrow(d)) 
d$Contributor_name[1:5] <- c("J.S.","S.R.","Z.W.W.","J.X.Q.","E.J.Y.")
d$Contributor_lastname <- rep(NA, nrow(d)) 
d$Contributor_lastname[1:5] <- c("Ascher", "Risch", "Soh", "Lee", "Soh")

#CONSULTAR PAPER CON NACHO.

head(d)


#Read data from Cane_1987.csv-----

d <- read.csv("raw_data/Cane_1987.csv", header = TRUE, sep = ";", dec= ",")
head(d)


colnames(d)[4]<-"m_dry_mass"


d$local_id <- c(1:nrow(d))
d$name <- "Cane_1987"
d$description <-"Dataset about Apoidea IT"
d$species <- paste(d$genus, d$specie)


d$Contributor_name <- rep(NA, nrow(d)) 
d$Contributor_name[1:1] <- c("J.H.")
d$Contributor_lastname <- rep(NA, nrow(d)) 
d$Contributor_lastname[1:1] <- c("Cane")
d$credit<- "Journal of the Kansas Entomological Society Vol. 60, No. 1 (Jan., 1987), pp. 145-147, http://www.jstor.org/stable/25084877"

#SIN DATOS SOBRE LOCALIDAD, PAÍS, LAT Y LONG, ETC.
head(d)





#Read data from  Hoehn_2008----------


d <- read.csv("raw_data/Hoehn_2008.csv", header = TRUE, sep = ";", dec= ",")
head(d)

d$local_id <- c(1:nrow(d))


d$name <- "Hoehn_2008"
d$description <-"Dataset about body sizes"
d$species <- paste(d$genus, d$specie)
d$location<-"Lore Lindu National Park, Central Sulawesi"
d$lat<- "-1.5"
d$long<- "120.03333333333333"
d$country<- "Indonesia"
d$doi<-"10.1098/rspb.2008.0405 " 

d$Contributor_name <- rep(NA, nrow(d)) 
d$Contributor_name[1:4] <- c("P.","T.","J.M.","I.")
d$Contributor_lastname <- rep(NA, nrow(d)) 
d$Contributor_lastname[1:4] <- c("Hoehn", "Tscharntke", "Tylianakis", "Steffan-Dewenter")

head(d)



#Read data from  Hagen_2013----------

d <- read.csv("raw_data/Hagen_2013.csv", header = TRUE, sep = ";", dec= ",")
head(d)

d$local_id <- c(1:nrow(d))

d$name <- "Hagen_2013"
d$description <-"Dataset about life type"
d$species <- paste(d$genus, d$specie)
d$location<-" Aarhus"
d$country<- "Denmark"
d$doi<-"10.1007/s00040-013-0290-x" 

d$Contributor_name <- rep(NA, nrow(d)) 
d$Contributor_name[1:2] <- c("M.","Y.L.")
d$Contributor_lastname <- rep(NA, nrow(d)) 
d$Contributor_lastname[1:2] <- c("Hagen", "Dupont")

#NO LAT AND LONG DATA
head(d)



#Read data from  Burkle_2013----------

d <- read.csv("raw_data/Burkle_2013.csv", header = TRUE, sep = ",", dec= ",")


d$local_id <- c(1:nrow(d))

d$name <- "Burkle_2013"
d$description <-"The paper use historic data sets to quantified the degree to which global change over 120 
years disrupted plant-pollinator interactions."
d$location<-"Carlinville, Illinois"
d$country<-"USA"
d$doi<-"10.1126/science.1232728"
#d$year<-

d$Contributor_name <- rep(NA, nrow(d)) 
d$Contributor_name[1:3] <- c("L.A.","J.C.","T.M.")
d$Contributor_lastname <- rep(NA, nrow(d)) 
d$Contributor_lastname[1:3] <- c("Burkle", "Marlin","Knight")

colnames(d)[2]<-"pollinator"

#split plant
position <- regexpr(pattern = "_", d$plant)
d$plant_genus <- substr(d$plant, 1, position-1)
d$plant_species <- substr(d$plant, position+1, nchar(as.character(d$plant)))

#split pollinator
position <- regexpr(pattern = "_", d$pollinator)
d$pollinator_genus <- substr(d$pollinator, 1, position-1)
d$pollinator_specie <- substr(d$pollinator, position+1, nchar(as.character(d$pollinator)))




d <- d[,c("local_id", "plant_genus","plant_species", "pollinator_genus", "pollinator_specie", "name","description",
          "location","country","doi","Contributor_name","Contributor_lastname")]


#HAY QUE CORREGIR Y REPASAR. POLLINATOR?

head(d)


#Read data from  Fowler_2016----------

d <- read.csv("raw_data/Fowler_2016.csv", header = TRUE, sep = ";", dec= ",")
head(d)

d$local_id <- c(1:nrow(d))
d$name <- "Fowler_2016"
d$description <-"Webpage about specialist bees and pollinator-plant interactions."
d$location<-"Mid-Atlantic and Northeastern United States"
d$country<-"USA"
d$credit<-"http://jarrodfowler.com/specialist_bees.html"

d$Contributor_name <- rep(NA, nrow(d)) 
d$Contributor_name[1:2] <- c("J.","S.")
d$Contributor_lastname <- rep(NA, nrow(d)) 
d$Contributor_lastname[1:2] <- c("Fowler", "Droege")


colnames(d)[2]<-"specie"
colnames(d)[3]<-"plant_genus"


d <- d[,c("local_id", "plant_genus","Genus", "specie", "name","description",
          "location","country","credit","Contributor_name","Contributor_lastname")]
head(d)




#Read data from  Mafalda_2017----------


d <- read.csv("raw_data/Mafalda_2017.csv", header = TRUE, sep = ";", dec= ",")
head(d)


d$local_id <- c(1:nrow(d))
d$name <- "Mafalda_2017"
d$description <-"Dataset about ecologycal traits and some interacctions data"
d$location<-"Tapada da Ajuda, Lisboa"
d$country<-"Portugal"
d$credit<-"http://hdl.handle.net/10451/27533"

d$Contributor_name <- rep(NA, nrow(d)) 
d$Contributor_name[1:1] <- c("M.N.C.C")
d$Contributor_lastname <- rep(NA, nrow(d)) 
d$Contributor_lastname[1:1] <- c("Rocha")


colnames(d)[2]<-"specie"
colnames(d)[3]<-"floral_specialization"
colnames(d)[5]<-"nest_location"

d <- d[,c("local_id","Genus", "specie", "floral_specialization", "sociality", "nest_location", "plant_genus","plant_specie","name","description",
          "location","country","credit","Contributor_name","Contributor_lastname")]
head(d)

#REPASAR. RELLENAR ESPACIOS EN BLANCO CON "NA"




#Read data from  Hupfenmüller_2014----------


d <- read.csv("raw_data/Hupfenmuller_2014.csv", header = TRUE, sep = ";", dec= ",")
head(d)


d$local_id <- c(1:nrow(d))
d$name <- "Hupfenmuller_2014"
d$description <-"Dataset about ecologycal traits and abundance"
d$location<-"Upper Franconia, Bavaria"
d$country<-"Germany"
d$doi<-"10.1371/journal.pone.0104439"
d$year<-"2010"


d$Contributor_name <- rep(NA, nrow(d)) 
d$Contributor_name[1:3] <- c("S.","I.","A.")
d$Contributor_lastname <- rep(NA, nrow(d)) 
d$Contributor_lastname[1:3] <- c("Hupfenmüller","Steffan-Dewenter","Holzschuh")


colnames(d)[6]<-"sociality"

d <- d[,c("local_id","Genus", "specie", "Abundance", "sociality","year","name","description",
          "location","country","doi","Contributor_name","Contributor_lastname")]
head(d)



#Read data from  Barbir_2014----------


d <- read.csv("raw_data/Barbir_2014.csv", header = TRUE, sep = ",", dec= ",")
head(d)

d$local_id <- c(1:nrow(d))
d$name <- "Barbir_2014"
d$description <-"Dataset about size traits"
d$location<-" La Poveda, Arganda del Rey, Madrid"
d$country<-"Spain"
d$doi<-"10.1111/afe.12076"
d$year<-"2011/2012"
d$lat<-"40.3166667"
d$long<-"-3.4833333333333334"

d$Contributor_name <- rep(NA, nrow(d)) 
d$Contributor_name[1:4] <- c("J.","F.R.","C.","J.")
d$Contributor_lastname <- rep(NA, nrow(d)) 
d$Contributor_lastname[1:4] <- c("Barbir","Badenes-Pérez","Fernández-Quintanilla","Dorado")

colnames(d)[5]<-"m_bsize"

d <- d[,c("local_id","Genus", "specie","m_bsize","year","name","description",
          "location","country","lat","long","doi","Contributor_name","Contributor_lastname")]

head(d)

#REPASAR Y ESTANDARIZAR. REVISAR M_BSIZE.



#Read data from  Sydenham_2016----------



d <- read.csv("raw_data/Sydenham_2016.csv", header = TRUE, sep = ";", dec= ",")
head(d)

d$local_id <- c(1:nrow(d))
d$name <- "Sydenham_2016"
d$description <-"Dataset about IT size"
d$country<-"Norway"
d$doi<-"10.1002/ece3.1871"
d$year<-"2009/2010/2013"


d$Contributor_name <- rep(NA, nrow(d)) 
d$Contributor_name[1:4] <- c("M.A.K.","L.D.","S.R.","K.")
d$Contributor_lastname <- rep(NA, nrow(d)) 
d$Contributor_lastname[1:4] <- c("Sydenham","Häusler","Moe","Eldegard")

colnames(d)[3]<-"individuals"

d <- d[,c("local_id","Genus", "specie","individuals","m_IT", "n_IT","se_IT","year","name","description",
          "country","doi","Contributor_name","Contributor_lastname")]


#SIN LOCALIZACIÓN EXACTA. NO LAT, NO LONG. ¿CREAR COLUMNA PARA REMARCAR QUE SON CAVITY NESTING?
head(d)




#Read data from  Tur_2013----------


d <- read.csv("raw_data/Tur_2013.csv", header = TRUE, sep = ";", dec= ",")
head(d)

d$local_id <- c(1:nrow(d))
d$name <- "Tur_2013"
d$description <-"Dataset interactions frequencies"
d$location<- ifelse(d$Site =="SB", "Son Bosc, Mallorca","Puig Major, Mallorca")
d$country<-"Spain"
d$month<-ifelse(d$Site =="SB", "4-7","5-8")
d$year<-"2009/2010"
d$doi<-"10.1371/journal.pone.0078294"

d$Contributor_name <- rep(NA, nrow(d)) 
d$Contributor_name[1:3] <- c("C.","R.","A.")
d$Contributor_lastname <- rep(NA, nrow(d)) 
d$Contributor_lastname[1:3] <- c("Tur","Castro-Urgal","Travest")

d$lat <- ifelse(d$Site =="SB", "39.774475","39.7998639")
d$long<-ifelse(d$Site=="SB", "3.129261111111111", "2.7855027777777774" )

head(d)

#NACHO, EN ESTE CASO TENEMOS POLINIZADORES, EN GENERAL, NO SOLO ABEJAS. HE DEJADO
#LA TABLA PRÁCTICAMENTE TAL Y COMO ESTABA, DEJANDO LAS COLUMNAS DE "FAMILY",
#"POLLINATOR", ETC. ÉCHALE UN OJO A LOS DATOS Y ME DICES SI HACEMOS LIMPIEZA DE TABLA.




#Read data from  Normandin_2016----------


d <- read.csv("raw_data/Normandin_2016.csv", header = TRUE, sep = ";", dec= ",")
head(d)

d$local_id <- c(1:nrow(d))
d$name <- "Normandin_2016"
d$description <-"Dataset about ecological and morphological traits"
d$location<- "Montreal & Quebec"
d$country<-"Canada"
d$year<-"2012/2013"
d$doi<-"10.7717/peerj.3051"
d$Contributor_name <- rep(NA, nrow(d)) 
d$Contributor_name[1:4] <- c("E.","N.J.","C.M.","V.")
d$Contributor_lastname <- rep(NA, nrow(d)) 
d$Contributor_lastname[1:4] <- c("Normandin","Vereecken","Buddle","Fournier")
d$lat<- "45.5011111 & 46.8022222"
d$long<-"-73.65611111111112 & -71.26388888888889" 

colnames(d)[4]<-"m_IT"
colnames(d)[5]<-"Sociality"
colnames(d)[8]<-"floral_specialization"

head(d)

#DUDAS ACERCA DE CÓMO PONER LOCATION Y LAT/LONG EN ESTE PAPER. EN LA TABLA INTRODUCIDA NO TENEMOS NADA QUE NOS DIFERENCIA ENTRE ZONAS, YA QUE NO SE HAN
#AÑADIDO LAS ABUNDANCIAS QUE APARECEN EN OTRAS TABLAS. HAY TABLAS TAMBIÉN DE LOS DISTINTOS SITES (MUCHOS), CADA UNO CON SU LAT/LONG, AUNQUE CREO QUE ES INFORMACIÓN
#NO ESENCIAL. 


d <- read.csv("raw_data/Macior_1974.csv", header = TRUE, sep = ",", dec= ",")
head(d)


d$local_id <- c(1:nrow(d))
d$name <- "Macior_1974"
d$credit<-"Pollination ecology of the Front Range of the Colorado Rocky Mountains, 1974. Macior, L.W. Melanderia 15 :1-59."
d$description <-"Dataset about tongue length measures"
d$location<-"Colorado Rocky Mountains"
d$country<-"U.S.A."
d$year<-"1974"


d$Contributor_name <- rep(NA, nrow(d)) 
d$Contributor_name[1:1] <- c("L.W.")
d$Contributor_lastname <- rep(NA, nrow(d)) 
d$Contributor_lastname[1:1] <- c("Macior")


colnames(d)[1]<-"Species"
colnames(d)[2]<-"m_tongue_length"
colnames(d)[3]<-"se_tongue_length"

#split pollinator
position <- regexpr(pattern = " ", d$Species)
d$Genus <- substr(d$Species, 1, position-1)
d$specie <- substr(d$Species, position+1, nchar(as.character(d$Species)))


head(d)

#ESTOS DATOS LOS HE ENCONTRADO A TRAVÉS DE OTRO ARTÍCULO:http://datadryad.org/resource/doi:10.5061/dryad.10278. 
#HE INTENTADO IR A LA FUENTE ORIGINAL, EL ARTÍCULO DE MACIOR, PERO NO HE ENCONTRADO PRÁCTICAMENTE NADA. 
#CORREGIR TABS Y ESPACIOS.
