check_data <- function(data, type = c("specimens", "observations")){
    if(type == "specimens"){
        if(colnames(data) =! c("",)) print("colnames should match...")
        #test for each column
        #taxize, max. min, NA's allowed or not...
    }
}

