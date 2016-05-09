#' @name add_data
#' 
#' @title Appends data to the database
#' 
#' @description This creates a pull request with the data appended to the csv's. 
#'      Data will be updated upon aproval.
#'  
#' @param schema Which schema is the data prepared for: Only "bee" available.
#' @param data A dataset to add
#' @param type 'specimens' or 'observations' data
#' @param check Should data be checked beforhand? default TRUE. 
#' 
#' @return a pull request?
#'
#' @examples 
#' #not run
#'  
#' @export

add_data <- function(dat, schema = "bee", type = c("specimens", "observations"), 
                     check = TRUE, local = FALSE, user, psw){
    if(check){
        check_data(dat, schema = schema, type = type)  
    }
    if(type = "specimens"){
        path <- paste0("data/", schema, "_specimens.csv")
        #local
        if(local) write.csv(dat, path, row.names = FALSE, append = TRUE)
        #fwrite (data.table v1.9.7) in development for large files?
        #remote
        ## Create a temporary directory to hold the repository
        temp_path <- file.path(tempfile(pattern="traits-"), "traits")
        dir.create(temp_path, recursive=TRUE)
        ## Clone the git2r repository
        repo <- clone("https://github.com/ibartomeus/traitdata.git", temp_path)
        write.csv(dat, paste0(temp_path, "/bee_specimens.csv"), row.names = FALSE, append = TRUE)
        add(repo, "bee_specimens.csv") 
        commit(repo, "added data to check")
        #commits(repo)
        #for me:
        push(repo, "origin", "refs/heads/master") #not working
        #or pull request? 
        #I understand it's not easy from command line :(
        
        #need to delete temporal dir? #unink()
    }
}

#option 2 gdocs
extract_key_from_url("https://docs.google.com/spreadsheets/d/1fpnI4eqShShFymmyayIfWUOkiNsGulVXhAT3-KhSa-g")
test <- gs_key("1fpnI4eqShShFymmyayIfWUOkiNsGulVXhAT3-KhSa-g")
gs_edit_cells(test, ws = "temp_data", input = head(iris), trim = TRUE)
for (i in 2:6) {
    gs_add_row(test, ws = "temp_data", input = iris[i, ])
    Sys.sleep(0.3)
}
iris %>%
    head(5) %>%
    write.csv("iris.csv", row.names = FALSE)
iris_ss <- gs_upload("iris.csv")

#users:
gs_auth(new_user = TRUE)
user_session_info <- gs_user()

#chat with paco: Option 
#1) the clone-pull if you use git
#2) shynny app
#3) Send the data elsewhere: figshare? Open Framework? Gdocs?
#4) Make the csv to live elsewhere! Pedro Molina? Figshare? 

#issue: Make id_link unique AND matching. Only option is to upload simultaneaouly?
#this is not optimal at all.