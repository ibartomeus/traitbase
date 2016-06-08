#' @name fill_columns
#' 
#' @title Function to check, add, and reorder column names in data.
#' 
#' @description checks: colnames, colorder, add missing columns.
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
#' fill_columns(data1, type = "specimens")
#' fill_columns(data2, type = "observations")
#'  
#' @export
fill_columns <- function(dat, #schema = "bee",
                         type = c("specimens", "observations"),
                         error = FALSE){
    if(type == "specimens"){
        needed <- c("link_id", "genus", "species", "trait",    
                    "value")
        to_order <- c("link_id", "genus", "species", "sex", "category", "trait",    
                      "value", "reference", "credit", "email")
        if(any(!needed %in% colnames(dat))){
            stop(paste(needed[!needed %in% colnames(dat)], "columns are needed"))
        }
        if(is.null(dat$sex)){
            dat$sex <- NA
        }
        if(is.null(dat$category)){
            #if(schema != "bee") stop("only available for bees")
            #load("data/bee_schema.rda")
            #schema <- bee_schema
            #temp <- merge(dat[,"trait", drop = FALSE], 
            #             schema[,c("category", "trait")],
            #              all.x = TRUE)
            #is merge preserving order?
            #dat$category <- temp$category
            dat$category <- NA
        }
        if(is.null(dat$reference)){
            dat$reference <- NA
        }
        if(is.null(dat$credit)){
            dat$credit <- NA
            message("Note that no credit is specified")
        }
        if(is.null(dat$email)){
            dat$email <- NA
        }
        dat <- dat[,to_order]
        droped <- !colnames(dat) %in% to_order
        if(any(droped)){
            message(paste("note that", droped ,"columns droped"))
        }
    }
    if(type == "observations"){
        needed <- c("link_id", "genus", "species", "day", "month", "year") #force lat/long?
        to_order <- c("link_id", "genus", "species",        
                      "sex", "interaction_type", "partner_genus", "partner_species", 
                      "day", "month", "year", "country",         
                      "location", "lat", "long", "reference",       
                      "credit", "email", "collector","taxonomist")
        if(any(!needed %in% colnames(dat))){
            stop(paste(needed[!needed %in% colnames(dat)], "columns are needed"))
        }
        if(is.null(dat$sex)){
            dat$sex <- NA
        }
        if(is.null(dat$interaction_type)){
            dat$interaction_type <- NA
        }
        if(is.null(dat$partner_genus)){
            dat$partner_genus <- NA
        }
        if(is.null(dat$partner_species)){
            dat$partner_species <- NA
        }
        if(is.null(dat$country)){
            dat$country <- NA
        }
        if(is.null(dat$location)){
            dat$location <- NA
        }
        if(is.null(dat$lat)){
            dat$lat <- NA
        }
        if(is.null(dat$long)){
            dat$long <- NA
        }
        if(is.null(dat$collector)){
            dat$collector <- NA
        }
        if(is.null(dat$taxonomist)){
            dat$taxonomist <- NA
        }
        if(is.null(dat$reference)){
            dat$reference <- NA
        }
        if(is.null(dat$credit)){
            dat$credit <- NA
            message("Note that no credit is specified")
        }
        if(is.null(dat$email)){
            dat$email <- NA
        }
        dat <- dat[,to_order]
        droped <- !colnames(dat) %in% to_order
        if(any(droped)){
            message(paste("note that", droped ,"columns droped"))
        }
    }
    dat
}