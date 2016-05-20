#' @name add_data
#' 
#' @title Appends data to the database
#' 
#' @description This appended data to the csv's. Only for admins for now.
#'  
#' @param schema Which schema is the data prepared for: Only "bee" available.
#' @param data A dataset to add
#' @param type 'specimens' or 'observations' data
#' @param check Should data be checked beforhand? default TRUE. 
#' 
#' @return data ready to be pulled.
#'
#' @examples 
#' #not run
#'  
#' @export

add_data <- function(dat, schema = "bee", type = c("specimens", "observations"), 
                     check = TRUE, local = TRUE){
    if(check){
        dat <- check_data(dat, schema = schema, type = type)  
    }
    dat$link_id <- paste0(dat$genus, dat$species, dat$link_id)
    dat$id <- NA #can be added later on
    dat <- dat[,c(ncol(dat), 1:(ncol(dat)-1))] #to keep order.
    if(type == "specimens"){
        path <- paste0("data/", schema, "_specimens.csv")
        #local
        if(local) write.csv(dat, path, row.names = FALSE, append = TRUE)
        #fwrite (data.table v1.9.7) in development for large files?
    }
    if(type == "observations"){
        path <- paste0("data/", schema, "_observations.csv")
        #local
        if(local) write.csv(dat, path, row.names = FALSE, append = TRUE)
        #fwrite (data.table v1.9.7) in development for large files?
    }
}


#issue: Make id_link unique AND matching. Only option is to upload simultaneaouly?
#this is not optimal at all.