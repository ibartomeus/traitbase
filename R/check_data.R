#' @name check_data
#' 
#' @title Function to check the data fits the schema and values are reasonable.
#' 
#' @description checks planed: fits the schema, taxonomy, units, geography, phenology
#'  
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
check_data <- function(data, 
                       type = c("specimens", "observations"),
                       error = FALSE){
    #common
    if(length(unique(data$link_id)) != length(data$link_id)){
        stop("link_id should be unique")
    }
    if(is.na(data$Genus) | is.na(data$species)){
        stop("Genus and species can't be NA. Only identified species are accepted")
    }
    data$Gen_sp <- paste(data$Genus, data$species)
    taxas <- tax_name(query = data$Gen_sp, get = "genus", verbose = FALSE)
    if(length(which(is.na(taxas$genus))) > 0){
        data2 <- data[-which(is.na(taxas$genus)),]
        warning("Species not in itis removed") #add error = TRUE and rows removed
    }
    if(!data$sex %in% c("male", "female", "queen", "worker")){
        stop("Sex should be one of 'male', 'female', 'queen', 'worker' or NA")
    }
    #reference: linked to bibtext???
    #credit: Anything goes?
    #now check specimens
    if(type == "specimens"){
        if(colnames(data) != colnames(specimens)[-1]){
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
        #value: tricky, becasue it depends on the trait :(
        #check also which columns has NA's allowed or not... 
    }
    if(type == "specimens"){
        #host_genus
        #host_species
        #day
        #month
        #year
        #location
        #lat
        #long
        #lat long not in the see...
        #collector
        #taxonomist
    }
}

