#' Specimens trait measurments
#' 
#' A dataset containing species traits at the specimen level.
#' 
#' \itemize{
#'   \item id: unique id of the table  
#'   \item link_id: original id of the fisical specimen. May be linked to the "observation" table. 
#'   \item Genus: Genus  
#'   \item species: species
#'   \item sex: male, female, queen, worker
#'   \item trait_category: morphological, life history, ecological, physiological
#'   \item trait: any accepted trait (with its units) in the schema
#'   \item value: numerical value or factor
#'   \item reference: bibtext code of the published reference
#'   \item credit: string of names to acknowledge
#'   }
#' 
#' @docType data
#' @keywords datasets
#' @name specimens
#' @usage data(specimens)
#' @format a data.frame with ten columns and hopefully lots of rows.
"specimens"