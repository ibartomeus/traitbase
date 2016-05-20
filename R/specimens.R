#' Specimens trait measurments
#' 
#' A dataset containing species traits at the specimen level.
#' 
#' \itemize{
#'   \item link_id: original id of the fisical specimen. May be linked to the "observation" table. 
#'   \item genus: Genus  
#'   \item species: species
#'   \item sex: male, female. For bees also: queen, worker for bees (see schema)
#'   \item trait_category: morphological, life history, ecological, physiological
#'   \item trait: any accepted trait (with its units) in the schema
#'   \item value: numerical value or factor
#'   \item reference: bibtext code of the published reference
#'   \item credit: string of names to acknowledge
#'   \item email: contactet person email.
#'   }
#' 
#' @docType data
#' @keywords datasets
#' @name specimens
#' @usage data(specimens)
#' @format a data.frame with ten columns and hopefully lots of rows.
#' @aliases bee_specimens
"specimens"