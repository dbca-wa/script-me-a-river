#------------------------------------------------------------------------------#
# Packages
library(here)
library(ckanr)
library(monitoR)
library(tidyverse)
library(lubridate)


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

#' Helper function to make sonde data analysis ready
#' 
#' 
#' @details Sonde data from a monitoring run is ingested from `data/``, 
#'   variables names standardised and all data for one date is appended to a   
#'   single table with optional data export to csv.
#'   
#' @param export Save csv of result to `csv/`, default: FALSE
#'
#' @return Table of all variables recorded by sonde with optional csv export
#'   of the same.
#'
#' @examples
#' \notrun{
#' raw_to_ready(export = TRUE)
#' }

raw_to_ready <- function(export = FALSE){
  excels <- list.files(path = here::here("data"), pattern = "^\\d{8}.*xlsx$")
  excels_p <- paste0(here::here("data"),"/",  excels)
  day <- substr(excels, 1, 8)
  river <- ifelse(str_detect(excels[1], "c"), "c", "s")
  dat_out <- data.frame()
  # error check - 2 workbooks OR dates don't match
  if(length(excels) != 2 | day[1] != day[2]){
    stop("Expecting only 2 excel workbooks of sonde data with same date")
  } else {
    for(p in excels_p){
      # make sure of correct tab
      sheet <- readxl::excel_sheets(p)[stringr::str_sub(stringr::str_to_lower(readxl::excel_sheets(p)), 1, 1) == "e"]
      dat <- readxl::read_excel(p, sheet = sheet)
      
      # Determine sonde from format
      if(stringr::str_detect(dat[1,1], "D/M/Y|M/D/Y")){
        # handle "YV" sondes with double header
        dat2 <- readxl::read_excel(p, sheet = sheet, col_names = FALSE)
        cells <- unpivotr::as_cells(dat2)
        col_headers <-
          cells %>%
          dplyr::filter(row <= 2, !is.na(chr)) %>%
          dplyr::select(row, col, header = chr) %>%
          tidyr::spread(row, header) %>%
          dplyr::mutate(`2` = ifelse(is.na(`2`), "_X", `2`)) %>%
          dplyr::mutate(header = ifelse(`2` != "_X", paste(`1`, `2`), `1`)) %>%
          dplyr::pull(header)
        dat3 <- readxl::read_excel(p, sheet = sheet, skip = 2, col_names = FALSE)
        names(dat3) <- tolower(col_headers)
        
        # standardise names
        search_for_these <- c("date time m/d/y hh:mm:ss",
                              "site codes", "sitenum", "site name", "site", "site code", "site names",
                              "temp c", "temp °c", "temp øc", "temp", "°c",
                              "odosat %",
                              "odo mg/l", "do+ conc mg/l" , "do mg/l",
                              "spcond us",
                              "sal ppt", "salinity ppt", "sal-ppt",
                              "ph",
                              "depth meters", "depth m", "depth", "dep m",
                              "turbid+ ntu",
                              "chl rfu",
                              "chl ug/l", "chlorophyll µg/l", "chlorophyll ug/l", "chlorophyll æg/l")
        replace_with_these <- c("Date",
                                rep("Site", 6), 
                                rep("°C", 5), 
                                "DO %",
                                rep("DO mg/L", 3),
                                "SPC-mS/cm",
                                rep("SAL-ppt", 3),
                                "pH",
                                rep("DEP m", 4),
                                "NTU",
                                "Chl RFU",
                                rep("Chl ug/L", 4))
        found <- match(colnames(dat3), search_for_these, nomatch = 0)
        colnames(dat3)[colnames(dat3) %in% search_for_these] <- replace_with_these[found]
        
        # return only required data for surfer
        # for MDY problems here for alternate format find one to test
        dat4 <- dat3 %>%
          dplyr::mutate(Time = substr(as.character(Date), 12, 19),
                        Date = ymd(day[1])) %>%
          dplyr::select(Date, Time, Site, `°C`, `DO %`, `DO mg/L`, `SPC-mS/cm`, `SAL-ppt`, 
                        `pH`, `DEP m`, `NTU`, `Chl RFU`, `Chl ug/L`) %>%
          dplyr::mutate(Loc = river)
        
        #return(dat4)
        dat_out <- dplyr::bind_rows(dat_out, dat4)
        
      } else {
        # workaround for us/ms unit disparity
        if(sum(str_detect(names(dat), "SPC-mS/cm")) == 0){
          dat["SPC-mS/cm"[!("SPC-mS/cm" %in% colnames(dat))]] = dat$`SPC-uS/cm` /1000}
        
        dat4 <- dat %>%
          dplyr::mutate(Time = substr(as.character(Time), 12, 19),
                        `DEP m` = `VPos m`,
                        Date = ymd(day[1])) %>%
          dplyr::select(Date, Time, Site, `°C`, `DO %`, `DO mg/L`, `SPC-mS/cm`, `SAL-ppt`, 
                        `pH`, `DEP m`, `NTU`, `Chl RFU`, `Chl ug/L`) %>%
          dplyr::mutate(Loc = river)
        
        #return(dat4)
        dat_out <- dplyr::bind_rows(dat_out, dat4)
      }
    }
    if(export == TRUE){
      csv_name <- paste0(here::here("csv"), "/",day[1], "_sonde_clean.csv")
      write_csv(dat_out, csv_name)
      return(dat_out)
    } else {
      return(dat_out)
    }
  }
}
