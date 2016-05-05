#' @name check_data
#' 
#' @title Function to check the data fits the schema and values are reasonable.
#' 
#' @description checks planed: fits the schema, taxonomy, units, geography, phenology
#'  
#' @param schema which schema is the data prepared for: Only "bee" available.
#' @param data a dataset to check
#' @param type specimens or observations data
#' @param error Should give an error or just remove failing rows? default FALSE. 
#' 
#' @return a clean dataset of an error
#'
#' @examples 
#' #not run
#' 
#' data1 <- data.frame(id = NA,
#' link_id = NA,
#' Genus = NA,
#' species = NA,
#' sex = NA,
#' trait_category = NA, 
#' trait = NA,
#' value = NA,
#' reference = NA,
#' credit = NA)
#' 
#' data2 <- data.frame(id = NA,
#' link_id = NA,
#' Genus = NA,
#' species = NA,
#' sex = NA,
#' host_genus = NA,
#' host_species = NA,
#' day = NA,
#' month = NA,
#' year = NA,
#' location = NA,
#' lat = NA,
#' long = NA,
#' reference = NA,
#' credit = NA,
#' collector = NA,
#' taxonomist = NA)

#' check_data(data1, type = "specimens")
#' check_data(data2, type = "specimens")
#'  
#' @export
check_data <- function(data, schema = "bee",
                       type = c("specimens", "observations"),
                       error = FALSE){
    if(schema != "bee") stop("only available for bees")
    schema <- bee_schema
    #checks common for both types
    if(length(unique(data$link_id)) != length(data$link_id)){
        stop("link_id should be unique") # isue: can I make it unique, and if so, 
        #can I force observations and specimens same way? 
    }
    if(is.na(data$Genus) | is.na(data$species)){
        stop("Genus and species can't be NA. Only fully identified species are accepted")
    }
    data$Gen_sp <- paste(data$Genus, data$species)
    taxas <- tax_name(query = data$Gen_sp, get = "genus", verbose = FALSE)
    if(length(which(is.na(taxas$genus))) > 0){
        data2 <- data[-which(is.na(taxas$genus)),]
        warning("Species not in itis removed") #add error = TRUE and id of rows removed
    }
    if(!data$sex %in% c("male", "female", "queen", "worker")){
        stop("Sex should be one of 'male', 'female', 'queen', 'worker' or NA")
    } #here should we use the schema?
    #reference: linked to bibtext??? Not sure yet how to implement that.
    #credit: Anything goes?
    #now check specimens
    if(type == "specimens"){
        if(colnames(data) != colnames(specimens)[-"id"]){
            stop("Column names should match specimens columnames: see ?specimens")
            #ToDo: automatically add some columns filled with NA with warning
        } 
        #tests for each column
        if(!data$trait_category %in% schema$trait_category){
            stop("trait_category should match schema (see ?schema). If you need a new category contact me")
        }
        trait_schema <- paste(data$trait_category, data$trait, sep = "_")
        if(!trait_schema %in% paste(schema$trait_category, schema$trait, sep = "_")){
            stop("trait should match schema (see ?schema). If you need a new trait type contact me")
        }
        #value: 
        #list traits
        traits <- unique(data$trait)
        #subset by trait
        for(i in 1:length(traits)){
            temp <- subset(data, trait == traits[i])
            schema_test <- subset(schema, trait == traits[i])
            #e.g.
            #schema_test <- "c(1:3)"
            #schema_test <- "c('nest', 'nest_soil')"
            #check
            if(!temp %in% eval(parse(text=schema_test))){
                stop("value not apropiate...")
            }
        }
        #check also which columns has NA's allowed or not... 
    }
    if(type == "specimens"){
        #host_genus
        taxas <- tax_name(query = data$host_genus, get = "genus", verbose = FALSE)
        if(length(which(is.na(taxas$genus))) > 0){
            data2 <- data[-which(is.na(taxas$genus)),]
            warning("Genus not in itis removed") #add error = TRUE and id of rows removed
        } #check ITITS is the palce for plants. I know it is the best for bees, but...
        #host_species
        if(!is.na(data$host_species)){
            data$Host_sp <- paste(data$host_genus, data$host_species)
            taxas <- tax_name(query = data$Host_sp, get = "genus", verbose = FALSE)
            if(length(which(is.na(taxas$genus))) > 0){
                data2 <- data[-which(is.na(taxas$genus)),] #better only remove species (= NA), not genus, then.
                warning("Species not in itis removed") #add error = TRUE and id of rows removed
            } #check ITITS is the palce for plants. I know it is the best for bees, but...
        }
        #day
        if(!data$day %in% c(1:31)){
            stop("months should have up to 31 days only") #I am not cheacking by month...
        } #I should indicate where it fails (along all the script)
        #month
        if(!data$month %in% c(1:12)){
            stop("months should be numerated 1 to 12") 
        }
        #year
        if(!data$year %in% c(1700:3000)){
            stop("year should be four digits and > 1700") 
        }
        #date
        if(as.POSIXct(paste(data$year, data$day, data$month, sep = "-")) 
                      > as.POSIXct(Sys.Date())){
            stop("Collection date can't be on the futuere")
        }
        #country: 
        if(!data$contry %in% countrycode_data$country.name){
            stop("country not recognized, see ?countrycode_data for a list")
        } #we can try the regex match and fix here on the fly!
        #location: any string goes
        #lat:
        if(data$lat %in% c(-85:85)){
            stop("latitude should be between -85 and 85")
        }
        #long
        if(data$lat %in% c(-180:180)){
            stop("longitude should be between -180 and 180")
        }
        #accurancy
        if(num.decimals(data$lat) | num.decimals(data$long) < 2){
            warning("longitude and latitude have very low resolution")
        }
        #check lat long not in the see & in the targeted country...
        
        #collector: any string goes
        #taxonomist: any string goes
    }
}

