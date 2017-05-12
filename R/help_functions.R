parse_errors <- function(errors){
    if(errors$valid){
        message("This dataset is valid")
    } else{
        message("This dataset is NOT valid")
    }
    if(length(errors$warnings)){
        temp <- unlist(errors$warnings, use.names = FALSE)
        message(temp[seq(2,length(temp),2)])
    } 
    if(length(errors$errors)){
        temp <- unlist(errors$errors, use.names = FALSE)
        err <- temp[seq(2,length(temp),2)]
        cod <- temp[seq(1,length(temp),2)]
        taxonomy <- err[which(cod == "402")]
        tax <- as.numeric(substr(taxonomy,regexpr("at line",taxonomy)+7, nchar(taxonomy)))-1 #to remove header, which is line 1
    } 
    list(err, tax)
}


#two functions I may need
validate_sliced <- function(cnx, d){
    if(length(unique(d$species)) > 50){
        errors <- list()
        cuts <- seq(1,nrow(d),50)
        cuts[length(cuts)+1] <- nrow(d)
        for(i in 1:(length(cuts)-1)){
            insert <- d[cuts[i]:cuts[i+1],]
            errors[[i]] <- validateDataset(cnx, insert) #awefull format.
        }
    } else {
        errors <- validateDataset(cnx, d)
    }
    errors
}


import_sliced <- function(cnx, d){
    if(length(unique(d$species)) > 50){
        errors <- list()
        cuts <- seq(1,nrow(d),50)
        cuts[length(cuts)+1] <- nrow(d)
        for(i in 1:(length(cuts)-1)){
            insert <- d[cuts[i]:cuts[i+1],]
            errors[[i]] <- importDataset(cnx, insert) #not working because name gets duplicated 
        }
    } else {
        errors <- importDataset(cnx, d)
    }
    errors
}
