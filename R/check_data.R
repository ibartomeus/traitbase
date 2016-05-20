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
    if(any(is.na(dat$genus)) | any(is.na(dat$species))){
        stop("Genus and species can't be NA. Only fully identified species are accepted")
    }
    gen_sp <- paste(dat$genus, dat$species)
    taxas <- clean_species(gen_sp)
    if(length(which(is.na(taxas$final_names))) > 0){
        dat <- dat[-which(is.na(taxas$final_names)),]
        warning("Species not in itis removed") #add error = TRUE and id of rows removed
    }
    if(any(!dat$sex %in% 
           eval(parse(text = as.character(schema[which(schema$trait == "sex"), 
                                              "test"]))))){
        stop("sex should be one of 'male', 'female', 'queen', or NA")
    } #check when NA's are accepted. Put schema in the warnings.
    #reference: linked to bibtext??? Not sure yet how to implement that.
    #credit: Anything goes?
    #email: can check for [@ .]
    #now check specimens
    if(type == "specimens"){
        template <- data.frame(link_id = NA,
                               genus = NA,
                               species = NA,
                               sex = NA,
                               trait_category = NA, 
                               trait = NA,
                               value = NA,
                               reference = NA,
                               credit = NA,
                               email = NA) 
        if(any(colnames(dat) != colnames(template))){
            stop("Column names should match specimens colum names: see ?specimens")
            #ToDo: automatically add missing columns filled with NA with warning
        } 
        #tests for each column
        if(any(!dat$trait_category %in% schema$category)){
            stop("trait_category should match schema (load('taxa_'schema.rda)). 
                 If you need a new category contact me")
        }
        trait_schema <- paste(dat$category, dat$trait, sep = "_")
        if(any(!trait_schema %in% paste(schema$category, schema$trait, sep = "_"))){
            stop("trait should match schema (load('taxa_'schema.rda)). 
                 If you need a new trait type contact me")
        }
        #value: 
        #list traits
        traits <- as.character(unique(dat$trait))
        #subset by trait
        for(i in 1:length(traits)){
            temp <- subset(dat, trait == traits[i])
            schema_test <- as.character(schema[which(schema$trait == traits[i]), 
                                               "test"])
            if(is.factor(temp$value)){
                if(any(!temp$value %in% eval(parse(text=schema_test)))){
                    stop("value not allowed by the schema. See load('taxa_'schema.rda)") 
                    #better error message? Yes, add at least the trait
                } 
            }else{
                if(any(temp$value < 0 | temp$value < eval(parse(text=schema_test)))){
                    stop("value not allowed by the schema. See load('taxa_'schema.rda)") 
                 }
            }
        }
        #check also which columns are allowed to be NA's or not... 
    }
    if(type == "specimens"){
        template <- data.frame(id = NA,
                               link_id = NA,
                               genus = NA,
                               species = NA,
                               sex = NA,
                               interaction_type = NA,
                               partner_genus = NA,
                               partner_species = NA,
                               day = NA,
                               month = NA,
                               year = NA,
                               country = NA, #Add state for US?
                               location = NA,
                               lat = NA,
                               long = NA,
                               reference = NA,
                               credit = NA,
                               email = NA, 
                               collector = NA,
                               taxonomist = NA)
        if(colnames(dat) != colnames(template)[-"id"]){
            stop("Column names should match observations colum names: see ?observations")
            #ToDo: automatically add missing columns filled with NA with warning
        }
        #partner_genus
        #need to fix also plant taxas with clean_data().
        partner_taxas <- tax_name(query = dat$partner_genus, get = "genus", verbose = FALSE)
        if(length(which(is.na(partner_taxas$genus))) > 0){
            dat[which(is.na(partner_taxas$genus)),] <- NA
            warning("Genus not in itis. Made NA") #add error = TRUE and id of rows fixed
        } #check ITITS is the palce for plants. I know it is the best for bees, but...
        #partner_species
        if(!is.na(dat$partner_species)){
            dat$partner_sp <- paste(dat$partner_genus, dat$partner_species)
            partner_taxas <- tax_name(query = data$partner_sp, get = "genus", verbose = FALSE)
            if(length(which(is.na(partner_taxas$genus))) > 0){
                dat[which(is.na(partner_taxas$genus)),] <- NA 
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
            warning("Some points fall on a different country than you mention or on the sea")
            #indicate which!
        } #what to do with US states?
        #collector: any string goes
        #taxonomist: any string goes
    }
    print("data checked!") #with this warnings?
    dat
}


#why
tax_name(query = "Osmia bicornis", get = "species")
tax_name(query = "Osmia bicornis", get = "genus")
