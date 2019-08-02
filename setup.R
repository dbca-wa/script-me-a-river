#------------------------------------------------------------------------------#
# Packages
library(here)
library(ckanr)
library(monitoR)


#------------------------------------------------------------------------------#
# Settings
if (file.exists("~/.Rprofile")) source("~/.Rprofile")
ckanr::ckanr_setup(url = Sys.getenv("CKAN_URL"), key = Sys.getenv("CKAN_API_KEY"))

#------------------------------------------------------------------------------#
# Functions

#' Create or update a CKAN resource.
#'
#'
#' @details The data will be written to CSV in a directory `data/` with the 
#'   resource title in snake_case. If no resource ID is given, a resource 
#'   will be created. The resource ID is returned in either case.
#' @param data A data frame to write to disk
#' @param resource_title A CKAN resource title
#' @param dataset_id A CKAN dataset (package) ID
#' @param resource_id A CKAN resource ID, default: NULL
#' @return The resource ID of the created or updated resource.
#' @examples
#' \notrun{
#' d <- ckanr::package_show("threatened-ecological-communities-database")
#'
#' # Run this once to create resource and retrieve resource ID
#' upload_to_ckan(a_tibble, "Resource title", d$id, resource_id = NULL)
#' # returns "502c74d7-32be-453f-aff6-c50aedd3deed" - paste into resource_id
#'
#' # Re-run this to update resource with new data
#' upload_to_ckan(a_tibble, "Resource title", d$id, 
#'   resource_id = "502c74d7-32be-453f-aff6-c50aedd3deed")
#' }
upload_to_ckan <- function(data,
                           resource_title,
                           dataset_id,
                           resource_id=NULL){
  
  resource_filename <- resource_title %>%
    stringr::str_to_lower(.) %>%
    stringr::str_replace_all(., " ", "_") %>%
    paste0(".csv") %>%
    file.path("data", .)
  
  write_delim(data, resource_filename, delim = ",")
  
  if (is.null(resource_id)){
    cat("No resource ID given, creating a new resource for", 
        resource_title, ": ")
    r <- ckanr::resource_create(package_id = dataset_id,
                                format = "csv",
                                name = resource_title,
                                upload = resource_filename)
  } else {
    cat("Updating CKAN resource", resource_title, ": ")
    r <- ckanr::resource_update(resource_id,
                                resource_filename)
    
  }
  r$id
}

#' Create a CKAN resource from surfer plot/s.
#'
#'
#' @details The surfer plots residing in the Project directory `plots/` will 
#'   be uploaded to CKAN as a resource with the file name as a title. It can 
#'   handle multiple plots. 
#' @examples
#' \notrun{
#' CKAN_surfR_upload()
#' }

CKAN_surfR_upload <- function(){
  plts <- list.files(path = here::here("plots"), pattern = ".pdf")
  for(plt in plts){
    resource_title <- stringr::str_split(plt, pattern = "\\.")[[1]][1]
    dataset_id <- "c1be7f9c-4ac8-4ed4-a370-5173a7fedb71"
    f <- stringr::str_split(plt, pattern = "\\.")[[1]][2]
    resource_filename <- here::here("plots", plt)
    resource_id=NULL
    
    
    cat("No resource ID given, creating a new resource for",
        resource_title, ": ")
    r <- ckanr::resource_create(package_id = dataset_id,
                                format = f,
                                name = resource_title,
                                upload = resource_filename)
    r$id
    cat("\n")
  }
}

#' Helper function to remove one or more surfer plots from CKAN
#' 
#' 
#' @details Blunt function to remove ALL surfer plots from the
#'   water-quality-monitoring-swan-canning-estuary-2019 dataset. Intended
#'   as a quick clean up tool for use while testing.
#' @param result is the result from running a the following resource query
#'   result <- ckanr::resource_search(q = "name:surfer")
#' @examples
#' \notrun{
#' surfR_remove(result = ckanr::resource_search(q = "name:surfer"))
#' } 
CKAN_surfR_remove <- function(result){
  for(i in seq_along(result)){
    cat("Deleting", result$results[[i]]$id, ": ")
    ckanr::resource_delete(id = result$results[[i]]$id)
    cat("\n")
  }
}


