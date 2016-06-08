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
    #Should I add a verbose and an option not to check taxonomy?
    #This may be useful for testing purposes, but not safe.
    if(schema != "bee") stop("only available for bees")
    load("data/bee_schema.rda")
    schema_ <- schema
    schema <- bee_schema
    
    #check, fill and reorder columns
    dat <- fill_columns(dat, type = type)
    
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
        warning(paste("Species not in itis removed:", 
                      taxas$species[which(is.na(taxas$final_names))])) #add error = TRUE
        taxas <- taxas[-which(is.na(taxas$final_names)),]
        temp <- unlist(strsplit(as.character(taxas$final_names), " "))
        dat$genus <- temp[c(1:length(temp)) %% 2 != 0]
        dat$species <- temp[c(1:length(temp)) %% 2 == 0]
        message(paste(taxas$species[which(paste(dat$genus, dat$species)
                                          != taxas$species)],
                      "species had misspellings or were synonims and had been fixed"))
    } else{
        temp <- unlist(strsplit(as.character(taxas$final_names), " "))
        dat$genus <- temp[c(1:length(temp)) %% 2 != 0]
        dat$species <- temp[c(1:length(temp)) %% 2 == 0]
        message(paste(taxas$species[which(paste(dat$genus, dat$species)
                                          != taxas$species)],
                      "species had misspellings or were synonims and had been fixed to",
                      taxas$final_names[which(paste(dat$genus, dat$species)
                                          != taxas$species)]))
    } 
    if(all(!is.na(dat$sex)) & any(!dat$sex[!is.na(dat$sex)] %in% 
                             eval(parse(text = as.character(schema[which(schema$trait == "sex"), 
                                                                   "test"]))))){
        stop(paste("sex should be one of", 
                   as.character(schema[which(schema$trait == "sex"), 
                                       "test"])))
    } 
    #reference: linked to bibtext??? DOI? Not sure yet how to implement that.
    #credit: Anything goes?
    #email: can check for [@ .]
    #now check specimens
    if(type == "specimens"){
        template <- data.frame(link_id = NA,
                               genus = NA,
                               species = NA,
                               sex = NA,
                               category = NA, 
                               trait = NA,
                               value = NA,
                               reference = NA,
                               credit = NA,
                               email = NA) 
        if(any(colnames(dat) != colnames(template))){
            stop(paste0("Column names should match specimens colum names: see ?",
                        schema_, "specimens"))
        } 
        #tests for each column
        if(all(!is.na(dat$category)) & any(!dat$category[!is.na(dat$category)] 
                                           %in% schema$category)){
            stop(paste0("trait_category should match schema (load(", 
                        schema_, "_schema.rda). If you need a new category contact me"))
        } 
        trait_schema <- paste(dat$category, dat$trait, sep = "_")
        if(any(is.na(dat$category))){
            if(any(!dat$trait %in% schema$trait)){
                stop(paste0("trait should match schema (load(", 
                            schema_, "_schema.rda). If you need a new category contact me"))
            }
        }
        if(any(!trait_schema %in% paste(schema$category, schema$trait, sep = "_"))){
            stop(paste0("trait & category should match schema (load(", 
                        schema_, "_schema.rda). If you need a new category contact me"))
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
                    stop(paste("Some values of trait ", traits[i], 
                               "not allowed by the schema. See schema (load(", 
                               schema_, "_schema.rda)).")) 
                } 
            }else{
                if(any(temp$value < 0 | temp$value > eval(parse(text=schema_test)))){
                    stop(paste("Some values of trait ", traits[i], 
                               "not allowed by the schema. See schema (load(", 
                               schema_, "_schema.rda))."))                  
                    }
            }
        }
    }
    if(type == "observations"){
        template <- data.frame(link_id = NA,
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
        if(any(colnames(dat) != colnames(template))){
            stop(paste0("Column names should match observations colum names: see ?",
                        schema_, "observations"))
        }
        #partner_genus
        partner_sp <- paste(dat$partner_genus, dat$partner_species)
        partner_taxas <- clean_species(partner_sp)
        if(length(which(is.na(taxas$synonym_names))) > 0){
            dat <- dat[-which(is.na(taxas$synonym_names)),]
            warning(paste("Genus not recognized removed:", 
                          partner_taxas$species[which(is.na(partner_taxas$synonym_names))])) #add error = TRUE
            partner_taxas <- partner_taxas[-which(is.na(partner_taxas$synonym_names)),]
            spaces <- grep(pattern = " ", partner_taxas$synonym_names)
            temp <- unlist(strsplit(as.character(partner_taxas$synonym_names[spaces]), " "))
            dat$partner_genus[spaces] <- temp[c(1:length(temp)) %% 2 != 0]
            dat$partner_species[spaces] <- temp[c(1:length(temp)) %% 2 == 0]
            dat$partner_genus[-spaces] <- as.character(partner_taxas$synonym_names[-spaces])
            dat$partner_species[-spaces] <- NA
            message(paste(partner_taxas$species[which(paste(dat$partner_genus,
                                                            dat$partner_species)
                                                      != partner_taxas$species)],
                          "species had misspellings or were synonims and had been fixed to",
                          partner_taxas$synonym_names[which(paste(dat$partner_genus,
                                                                  dat$partner_species)
                                                            != partner_taxas$synonym_names)]))
        } else{
            temp <- unlist(strsplit(as.character(partner_taxas$synonym_names[spaces]), " "))
            dat$partner_genus[spaces] <- temp[c(1:length(temp)) %% 2 != 0]
            dat$partner_species[spaces] <- temp[c(1:length(temp)) %% 2 == 0]
            dat$partner_genus[-spaces] <- as.character(partner_taxas$synonym_names[-spaces])
            dat$partner_species[-spaces] <- NA
            message(paste(partner_taxas$species[which(paste(dat$partner_genus,
                                                            dat$partner_species)
                                                      != partner_taxas$synonym_names)],
                          "species had misspellings or were synonims and had been fixed to",
                          partner_taxas$synonym_names[which(paste(dat$partner_genus,
                                                            dat$partner_species)
                                                      != partner_taxas$synonym_names)]))
        } 
        #day
        if(any(!dat$day %in% c(NA,1:31))){
            stop("months should have up to 31 days only") 
            #I am not cheacking by month...
        } 
        #month
        if(any(!dat$month %in% c(NA, 1:12))){
            stop("months should be numbered from 1 to 12") 
        }
        #year
        if(any(!dat$year %in% c(NA, 1700:3000))){
            stop("year should be four digits and > 1700") 
        }
        #date
        if(any(as.POSIXct(paste(dat$year, dat$day, dat$month, sep = "-")) 
               > as.POSIXct(Sys.Date()))){
            stop("Collection date can't be on the future")
            #not checking NA's here, necessary?
        }
        #country: 
        if(any(!dat$country %in% c(NA,countrycode_data$country.name))){
            stop("country not recognized, see ?countrycode_data for a list of accepted names")
            #we can try the regex to match and fix coomon mistakes here on the fly!
            #NA's allowed. Need to add check
        }
        #location: any string goes
        #lat:
        if(any(!dat$lat %in% c(NA,-85:85))){
            stop("latitude should be between -85 and 85")
        }
        #long
        if(any(!dat$lat %in% c(NA, -180:180))){
            stop("longitude should be between -180 and 180")
        }
        #accurancy
        if(any(num.decimals(dat$lat) | num.decimals(dat$long) < 2)){
            warning("longitude and latitude have very low resolution.")
            #NA's here?
        }
        #check lat long not in the see & in the targeted country...
        match_country <- coords2country(dat$lat, dat$long)
        discrepancies <- which(as.character(dat$country) != as.character(match_country))
        if(length(discrepancies) > 0){
            warning(paste("The following rows fall on a different country than you
                          mention or on the sea:", paste(discrepancies, sep = " ",
                                                         collapse = " ")))
            #we may need to do that in other error messages.
        } #what to do with US states?
        #collector: any string goes
        #taxonomist: any string goes
    }
    message("data checked!")
    dat
} 
 