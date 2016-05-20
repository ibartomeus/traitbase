
#flag outliers
flag_outliers <- function(){

}


#done first in specimens and observations
fix_synonims <- function(){
    mynames <- unique(paste(bee_taxonomy$Genus, bee_taxonomy$species))
    temp <- synonyms(mynames, db="itis")
    synonym_ids <- grep(pattern = "acc_name", temp)
    accepted_names <- unlist(lapply(temp[synonym_ids], '[', "acc_name"), use.names = FALSE)
    mynames[synonym_ids] <- accepted_names
}

#once synonims are fixed.
update_taxonomy <- function(schema = "bee"){
    if(schema != "bee"){
        stop("only bee schema implmented so far")
    }
    load(bee_specimens)
    load(bee_taxonomy)
    dat <- bee_specimens
    #check all species have taxonomiy entry and if not, 
    #create one and populate with taxize
    dat$Gen_sp <- paste(dat$Genus, dat$species)
    sp_to_add <- dat$Gen_sp[which(!dat$Gen_sp %in% 
                                      paste(bee_taxonomy$Genus, bee_taxonomy$species))]
    #rank_ref
    taxas <- tax_name(query = sp_to_add, get = c("subgenus", "genus","tribe", "subfamily","family", "superfamily", "order"), verbose = FALSE)
    #add new data
    taxas$species <- strsplit(taxas$query, " ")[[1]][2]
    taxas_to_add <- plyr::rbind.fill(bee_taxonomy[1,, drop = FALSE], taxas[,-c(1:2)])
    #add id?? Now, or when accepting the pull request?
    #local
    write.csv(taxas_to_add[-1,], "bee_taxonomy.csv", row.names = FALSE, append = TRUE)
    #fwrite is available to test now in data.table v1.9.7 in development I should use it for large files.
    #itis not always return order, add it manually for bees? Maybe later.   
}

#add unique id's

add_id <- function(){
    
}

#generate soecies level
generate_species_level <- function(){
    #should we select which traits to use? or do all by default?
    
    #this function uses one entry per species in taxonomy
    
    #Adds mean and se (any other metric is better: range?) 
    #trait value (per sex) and species. 
    #cat values as % of dominant trait.
    
    #Add range, phenology (where!) and specialization from observations (and n)
    
    #how to give credit to data holders for this? 
    #Select all id's contributing to a species-trait combination?
    
    #Note: This can trigger info from e.g. gbif, USGS, 
}
