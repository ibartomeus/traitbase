#' Schema of the traits used
#' 
#' This defines the traits, units and explnations for each taxon. Only Bees implmented so far.
#' 
#' \itemize{
#'   \item category: metadata, morphological, life history, ecological, physiological
#'   \item trait: any accepted trait (with its units) in the schema
#'   \item units: units of the trait variable
#'   \item test: test to apply to this category
#'   \item description: verbal description
#'   }
#' 
#' @docType data
#' @keywords datasets
#' @name schema
#' @usage data(schema)
#' @format a data.frame with 4 columns and a few rows.
#' @aliases bee_schema
"schema" 