#' @name clean_data
#' 
#' @title Helper function to clean taxonomy.
#' 
#' @description This correct misspellings, synonyms and flags not recognizes species.
#'  
#' @param species A vector of species to check
#' 
#' @return data.frame of original and corrected species.
#'
#' @examples 
#' #not run
#' species <- c("Osmia rufa", "Osmia bicornis", "Osmia ruffa", 
#'            "Osmia wikifluqie", "watermelon pie", "Osmia sp.")
#' clean_species(species)

#Note: code should be updated when synonyms is vectorized by scott.
#' @export
clean_species <- function(species){
    #misspellings
    species2 <- unique(species) #how to keep track of this?
    temp <- gnr_resolve(species2, best_match_only = TRUE, canonical = TRUE)
    dat <- merge(data.frame(species2), temp[,c("user_supplied_name", "matched_name2")], 
                      by.x = "species2", by.y = "user_supplied_name", all.x = TRUE)
    #synonims
    #here we ca save time by re-ding a unique() and removing NA's
    species3 <- unique(dat$matched_name2)
    species3 <- species3[!is.na(species3)]
    temp <- synonyms(species3, db="itis")
    synonym_ids <- grep(pattern = "acc_name", temp) #is this the optimal solution?
    accepted_names <- unlist(lapply(temp[synonym_ids], '[', "acc_name"), use.names = FALSE)
    synonym_names <- species3
    synonym_names[synonym_ids] <- accepted_names
    key <- data.frame(species3, synonym_names)
    dat <- merge(dat, key, 
                      by.x = "matched_name2", by.y = "species3", all.x = TRUE)
    #clean non accepted species
    species4 <- unique(dat$synonym_names)
    species4 <- species4[!is.na(species4)]
    out <- list()
    for(i in 1:length(species4)){
        out[[i]] <- tax_name(species4[i], get = "species")
    }
    out2 <- plyr::ldply(out, data.frame)
    final_names <- species4
    final_names[which(is.na(out2$species))] <- NA
    key2 <- data.frame(species4, final_names)
    dat <- merge(dat, key2, 
                 by.x = "synonym_names", by.y = "species4", all.x = TRUE)
    #output
    dat <- merge(data.frame(species), dat, by.x = "species", by.y = "species2",
          all.x = TRUE)
    dat[,c(1,3,2,4)]
}

