#' Specimen observations
#' 
#' A dataset containing observations of specimens from where to calculate relevant traits.
#' 
#' \itemize{
#'   \item id: unique id of the table  
#'   \item link_id: original id of the fisical specimen. May be linked to the "specimen" table. 
#'   \item Genus: Genus   
#'   \item species: species 
#'   \item sex: male, female, queen, worker
#'   \item host_genus: genus of the host plant
#'   \item host_species: species of the host plant
#'   \item day: day of the month of the observation
#'   \item month: month of the observation
#'   \item year: year of the observation
#'   \item country: country (or US state) where collected
#'   \item location: verbal description of the location, including nearest town/city
#'   \item lat: latitude degrees
#'   \item long: longitude degrees
#'   \item reference: bibtext code of the published reference
#'   \item credit: string of names to acknowledge
#'   \item collector: name of the collector
#'   \item taxonomist: name of the taxonomist
#'   }
#'   
#' @docType data
#' @keywords datasets
#' @name observations
#' @usage data(observations)
#' @format a data.frame with ten columns and hopefully lots of rows.
"observations"