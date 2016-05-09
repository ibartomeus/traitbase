#' @name check_data
#' 
#' @title Function to check the data fits the schema and values are reasonable.
#' 
#' @description checks planed: fits the schema, taxonomy, units, geography, phenology
#'  
#' @param schema which schema is the data prepared for: Only "bee" available.
#' @param dat a dataset to check
#' @param type specimens or observations data
#' @param error Should give an error or just remove failing rows? default FALSE. 
#' 
#' @return a clean dataset or an error
#'
#' @examples 
#' #not run
#' 
#' data1 <- data.frame(id = NA,
#' link_id = NA,
#' genus = NA,
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
#' genus = NA,
#' species = NA,
#' sex = NA,
#' interaction_type = NA,
#' partner_genus = NA,
#' partner_species = NA,
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
#' check_data(data2, type = "observations")
#'  
#' @export
check_data <- function(dat, schema = "bee",
                       type = c("specimens", "observations"),
                       error = FALSE){
    if(schema != "bee") stop("only available for bees")
    load("data/bee_schema.rda")
    schema <- bee_schema

    #checks common things for both types
    if(length(unique(dat$link_id)) != length(dat$link_id)){
        stop("link_id should be unique") 
    }
    if(is.na(dat$genus) | is.na(dat$species)){
        stop("Genus and species can't be NA. Only fully identified species are accepted")
    }
    dat$gen_sp <- paste(data$genus, data$species)
    taxas <- tax_name(query = dat$gen_sp, get = "genus", verbose = FALSE)
    if(length(which(is.na(taxas$genus))) > 0){
        dat <- dat[-which(is.na(taxas$genus)),]
        warning("Species not in itis removed") #add error = TRUE and id of rows removed
    }
    if(!dat$sex %in% eval(parse(text = schema[which(schema$trait == "sex"), 
                                              "test"]))){
        stop("sex should be one of 'male', 'female', 'queen', or NA")
    } #check NA's are accepted. put schema in the warning.
    #reference: linked to bibtext??? Not sure yet how to implement that.
    #credit: Anything goes?
    
    #now check specimens
    if(type == "specimens"){
        load("data/bee_specimens.rda") #this may be lots of data... think better way?
        #maybe just save a head() of each dataset? Is this lame?
        specimens <- bee_specimens #fix that when more than one schema is available.
        if(colnames(dat) != colnames(specimens)[-"id"]){
            stop("Column names should match specimens columnames: see ?specimens")
            #ToDo: automatically add missing columns filled with NA with warning
        } 
        #tests for each column
        if(!dat$trait_category %in% schema$category){
            stop("trait_category should match schema (see ?schema). If you need a new category contact me")
        }
        trait_schema <- paste(dat$category, dat$trait, sep = "_")
        if(!trait_schema %in% paste(schema$category, schema$trait, sep = "_")){
            stop("trait should match schema (see taxa_schema). If you need a new trait type contact me")
        }
        #value: 
        #list traits
        traits <- unique(dat$trait)
        #subset by trait
        for(i in 1:length(traits)){
            temp <- subset(data, trait == traits[i])
            schema_test <- subset(schema, trait == traits[i])
            #e.g.
            #schema_test <- "c(1:3)"
            #schema_test <- "c('nest', 'nest_soil')"
            #check
            if(!temp %in% eval(parse(text=schema_test))){
                stop("value not llowed by the schema. See taxa_schema") #better error message?
            }
        }
        #check also which columns are allowed to be NA's or not... 
    }
    if(type == "specimens"){
        #check columns
        #idem from above, but we should avoid load.
        #partner_genus
        taxas <- tax_name(query = dat$partner_genus, get = "genus", verbose = FALSE)
        if(length(which(is.na(taxas$genus))) > 0){
            dat[which(is.na(taxas$genus)),] <- NA
            warning("Genus not in itis. Made NA") #add error = TRUE and id of rows fixed
        } #check ITITS is the palce for plants. I know it is the best for bees, but...
        #partner_species
        if(!is.na(dat$partner_species)){
            dat$partner_sp <- paste(dat$partner_genus, dat$partner_species)
            taxas <- tax_name(query = data$partner_sp, get = "genus", verbose = FALSE)
            if(length(which(is.na(taxas$genus))) > 0){
                dat[which(is.na(taxas$genus)),] <- NA 
                warning("species not in itis, turned to NA") #add error = TRUE and id of rows fixed
            } 
        }
        #day
        if(!dat$day %in% c(1:31)){
            stop("months should have up to 31 days only") #I am not cheacking by month...
        } #I should indicate where it fails (along all the script)
        #month
        if(!dat$month %in% c(1:12)){
            stop("months should be numerated 1 to 12") 
        }
        #year
        if(!dat$year %in% c(1700:3000)){
            stop("year should be four digits and > 1700") 
        }
        #date
        if(as.POSIXct(paste(dat$year, dat$day, dat$month, sep = "-")) 
                      > as.POSIXct(Sys.Date())){
            stop("Collection date can't be on the future")
        }
        #country: 
        if(!dat$contry %in% countrycode_data$country.name){
            stop("country not recognized, see ?countrycode_data for a list of accepted names")
        } #we can try the regex to match and fix coomon mistakes here on the fly!
        #location: any string goes
        #lat:
        if(dat$lat %in% c(-85:85)){
            stop("latitude should be between -85 and 85")
        }
        #long
        if(dat$lat %in% c(-180:180)){
            stop("longitude should be between -180 and 180")
        }
        #accurancy
        if(num.decimals(dat$lat) | num.decimals(dat$long) < 2){
            warning("longitude and latitude have very low resolution.")
        }
        #check lat long not in the see & in the targeted country...
        match_country <- coords2country(dat$lat, dat$long)
        discrepancies <- which(dat$country != match_country)
        if(length(discrpeancies) > 0){
            warning("Some points fall on a different country than you mention or on the sea") #indicate which
        } #what to do with US states?
        #collector: any string goes
        #taxonomist: any string goes
    }
    dat
}

