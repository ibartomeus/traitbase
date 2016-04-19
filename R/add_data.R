add_data <- function(data, type = c("specimens", "observations"), check = TRUE){
    if(check){
        check_data(data, type = type)  
    }
    #check all species have taxonomiy entry and if not, 
    #create one and populate with taxasice
    
    #Apend to the csv via pull request
}