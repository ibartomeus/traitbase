#' Specimen observations
#' 
#' A dataset containing observations of specimens from where to calculate relevant traits.
#' 
#' \itemize{
#'   \item link_id: original id of the fisical specimen. May be linked to the "specimen" table. 
#'   \item genus: genus   
#'   \item species: species 
#'   \item sex: male, female. For bees also queen, worker (see schema)
#'   \item interaction_type: mutuaism, parastism, etc... (see schema). 
#'   \item partner_genus: genus of the partner (e.g. host)
#'   \item partner_species: species of the partner (e.g. host)
#'   \item day: day of the month of the observation
#'   \item month: month of the observation
#'   \item year: year of the observation
#'   \item country: country where collected
#'   \item location: verbal description of the location, including nearest town/city
#'   \item lat: latitude degrees
#'   \item long: longitude degrees
#'   \item reference: bibtext code of the published reference
#'   \item credit: string of names to acknowledge
#'   \item email: contactet person email 
#'   \item collector: name of the collector
#'   \item taxonomist: name of the taxonomist
#'   }
#'   
#' @docType data
#' @keywords datasets
#' @name observations
#' @usage data(observations)
#' @format a data.frame with ten columns and hopefully lots of rows.
#' @aliases bee_observations
"observations"