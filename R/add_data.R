#' @name add_data
#' 
#' @title Appends data to the database
#' 
#' @description This creates a pull request with the data appended to the csv's. Data will be updated upon aproval.
#'  
#' @param schema which schema is the data prepared for: Only "bee" available.
#' @param data a dataset to check
#' @param type specimens or observations data
#' @param check Should data be checked beforhand? default TRUE. 
#' 
#' @return a pull request.
#'
#' @examples 
#' #not run
#'  
#' @export

#Note:  readr::read_csv() should be used to read csv, as is faster.
add_data <- function(data, schema = "bee", type = c("specimens", "observations"), 
                     check = TRUE, user, psw){
    if(check){
        check_data(data, schema = schema, type = type)  
    }
    #check all species have taxonomiy entry and if not, 
    #create one and populate with taxize
    data$Gen_sp <- paste(data$Genus, data$species)
    sp_to_add <- data$Gen_sp[which(!data$Gen_sp %in% paste(bee_species$Genus, bee_species$species))]
    #rank_ref
    taxas <- tax_name(query = sp_to_add, get = c("subgenus", "genus","tribe", "subfamily","family", "superfamily", "order"), verbose = FALSE)
    #add new data
    taxas$species <- strsplit(taxas$query, " ")[[1]][2]
    taxas_to_add <- plyr::rbind.fill(bee_species[1,, drop = FALSE], taxas[,-c(1:2)])
    #add id?? Now, or when accepting the pull request?
    #local
    write.csv(taxas_to_add[-1,], "bee_species.csv", row.names = FALSE, append = TRUE)
    #fwrite is available to test now in data.table v1.9.7 in development I should use it for large files.
    #itis not always return order, add it manually for bees? Maybe later.    
    #remote
    ## Create a temporary directory to hold the repository
    path <- file.path(tempfile(pattern="smalldata-"), "smalldata")
    dir.create(path, recursive=TRUE)
    ## Clone the git2r repository
    repo <- clone("https://github.com/ibartomeus/smalldata", path)
    add(repo, "bee_species.csv") #need to be done in local first?
    commit(repo, "added data") 
    push(repo, "origin", "refs/head/master") #??
    #or pull request
    pull(repo) #would this mean pull request if you have no access?
    #need to delete temporal dir?
    
    #chat with paco: Option 
    #1) the clone-pull if you use git
    #2) shynny app
    #3) Send the data elsewhere: figshare? Open Framework? Gdocs?
    #4) Make the csv to live elsewhere! Pedro Molina? Figshare? 
    
}