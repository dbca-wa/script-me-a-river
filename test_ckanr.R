library(tidyverse)
setwd("Z:/DEC/SwanCanningRiversMonitoringProgram_SP2018-072/DATA/Working/script-me-a-river")

# Settings
if (file.exists("~/.Rprofile")) source("~/.Rprofile")
ckanr::ckanr_setup(url = Sys.getenv("CKAN_URL"), key = Sys.getenv("CKAN_API_KEY"))




plts <- list.files(path = here::here("plots"), pattern = ".pdf")
#plt = plts[1]
for(plt in plts){
  #print(plt)
  resource_title <- str_split(plt, pattern = "\\.")[[1]][1]
  dataset_id <- "c1be7f9c-4ac8-4ed4-a370-5173a7fedb71"
  f <- str_split(plt, pattern = "\\.")[[1]][2]
  resource_filename <- here::here("plots", plt)
  resource_id=NULL
  
  if (is.null(resource_id)){
    cat("No resource ID given, creating a new resource for", 
        resource_title, ": ")
    r <- ckanr::resource_create(package_id = dataset_id,
                                format = f,
                                name = resource_title,
                                upload = resource_filename)
  } else {
    cat("Updating CKAN resource", resource_title, ": ")
    r <- ckanr::resource_update(resource_id,
                                resource_filename)
    
  }
  r$id
}
r <- ckanr::resource_search(q = "name:surfer")

length(r$results)
#r$results[[1]]$id
surfR_remove <- function(result){
  for(i in seq_along(result)){
    cat("Deleting", result$results[[i]]$id, ": ")
    ckanr::resource_delete(id = result$results[[i]]$id)
    cat("\n")
  }
}
surfR_remove(result = r)

ckanr::resource_delete(id = r$results[[1]]$id)


surfR_upload_to_ckan <- function(){
  plts <- list.files(path = here::here("plots"), pattern = ".pdf")
  for(plt in plts){
    resource_title <- str_split(plt, pattern = "\\.")[[1]][1]
    dataset_id <- "c1be7f9c-4ac8-4ed4-a370-5173a7fedb71"
    f <- str_split(plt, pattern = "\\.")[[1]][2]
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

surfR_upload_to_ckan()
