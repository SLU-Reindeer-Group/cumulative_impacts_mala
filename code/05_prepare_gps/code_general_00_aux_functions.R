#' ---
#' title: 'Auxiliary functions for GPS cleaning'
#' author: Bernardo Niebuhr
#' ---

library(stringr)
library(dplyr)
library(tidyr)
library(sf)

#' Change the year present in the end of stings and names of individuals
#'
#' @description 
#' `change_year_name` returns the name of individual animals with a 
#' correspondent change in years
#'
#' @details 
#' Sometimes an individual is monitored for a period from one year to another.
#' Let's say we are studing the behavior of an animal in the winter between 2014-2015,
#' and we call this animal "individual_01_2014". However, it may be convention to 
#' call this the winter from 2015, not 2014. In this case, we can add a year and update
#' it in the animal's name.
#' 
#' @param x vector of strings corresponding to animal individual names, with the corresponding
#' monitoring year in the end of the name. E.g. "individual_01_2014".
#' 
#' @param change character: whether to "add" or "remove" years from the animals' names.
#' 
#' @param years_to_change number of years to be added or removed from the animals' names.
#' 
#' @return a vector of animal names with the year updated accordinly
#' 
#' @examples
#' names <- c("an1_2014", "an2_2015", "an3_2016")
#' change_year_name(names, "add", 2)
change_year_name <- function(x, change = c("add", "remove")[1], years_to_change = 1) {
  
  # check
  possible_changes <- c("add", "remove")
  if(!(change %in% possible_changes)) 
    stop(paste0("Argument 'change' should be one of the following options: ", 
                paste(possible_changes, collapse = ", "), "."))
  
  # name removing year in the end
  names_no_year <- stringr::str_sub(x, 1, -5)
  
  # get year from animal names
  years <- x %>% 
    stringr::str_split("_") %>% 
    sapply(., FUN = last) %>% 
    as.numeric()
  
  # update years considering the winter 2013-2014 as 2014
  if(change == "add") {
    years_updated <- years + years_to_change
  } else {
    years_updated <- years - years_to_change
  }
  
  # return original names with year updated
  names_updated <- paste0(names_no_year, years_updated)
  names_updated
}


#' Identify the lines to be removed from a table of GPS positions
#' 
#' This function identifies the lines to be removed from a table of GPS positions, based on dates when the collar of 
#' each individual was off or when individual animals were herded. For cases of collar removal, 
#' it also identifies lines for which the same collar was used, when the name of the individual 
#' should change, since it might be a new individual monitored with the same collar.
#' 
#' Add parameters, explain how the datasets should be organized, and what is the output
lines_to_remove <- function(dataset, when_to_remove, 
                            individual_namecol_dataset = "Object_ID_year", individual_timecol_dataset = "timestamp",
                            individual_namecol_remove = "Object_ID_year", 
                            individual_timecol_start_remove = "timestamp_end",
                            individual_timecol_end_remove = "timestamp_begin") {
  
  # initialize lines to remove
  lin_to_remove <- c()
  
  # lines corresponding to new individual after collar removal
  new_ind_lines <- c()
  
  # loop for each of the events
  for(i in 1:nrow(when_to_remove)) {
    
    print(i)
    
    # get line
    line <- when_to_remove[i,]
    
    # identify the lines to be deleted
    # where <- which(dataset$Object_ID_year == line$Object_ID_year &
    #                  dataset$timestamp >= line$timestamp_end &
    #                  dataset$timestamp <= line$timestamp_begin)
    where <- which(dataset[[individual_namecol_dataset]] == line[[individual_namecol_remove]] &
                     dataset[[individual_timecol_dataset]] >= line[[individual_timecol_start_remove]] &
                     dataset[[individual_timecol_dataset]] <= line[[individual_timecol_end_remove]])
    
    # if the length of lines is > 0, add them to the list of lines to be removed
    if(length(where) > 0) {
      lin_to_remove <- c(lin_to_remove, where)
      
      # check if the following lines are from the same individual
      # if they are, add a marker to change the name of the individual
      
      # lines for the same individual after the collar was put on again
      where_new_ind <- which(dataset[[individual_namecol_dataset]] == line[[individual_namecol_remove]] &
                               dataset[[individual_timecol_dataset]] > line[[individual_timecol_end_remove]])
      
      # if there are records after that for the same individual, this is a new individual
      if(length(where_new_ind) > 0) {
        new_ind_lines <- c(new_ind_lines, where_new_ind)
      }
    }
  }
  list(lines_to_remove = lin_to_remove,
       lines_of_new_individuals = new_ind_lines)
}


#' Check if locations are within polygons 
#' 
#' This function checks if geographical locations are within polygons and, if positive, 
#' it determines which polygon and gives a number to that visit to the polygon. This is used to
#' identify moments when individuals visit specific areas or patches, how long they stay there,
#' and how many times it happened (since each visit is given a different id).
#' 
#' This was used in the context of GPS data cleaning, to remove or identify possible changes in 
#' GPS collars when several subsequent positions were found within houses, buildings, or urban
#' areas.
#' 
#' @param gps_locations sf object. object with GPS locations or other type ot geographical data with 
#' x and y identifying locations.
#' @param areas sf object. areas or polygons within which one wants to check if the `gps_locations` are
#' located.
#' @param feature_name character. name of the feature (e.g. urban areas, hourse, or habitat type),
#' to be used for column creation name in the output.
#' @param min_point  numeric. Minimum number of points to characterize a visit to a polygon
#' (and avoid random errors/positions to be identified as visits).
#' 
#' @return the input gps_locations, with columns identifying, for each location, if 
#' it was within a polygon, and if positive, which polygon and which visit (burst) was
#' that.
check_within <- function(gps_locations, areas, feature_name, min_points = 4) {
  
  within <- sf::st_within(gps_locations, areas) %>%
    as.data.frame() %>%
    dplyr::mutate(diff = col.id - lag(col.id),
                  temp_id = ifelse(diff != 0 | is.na(diff), 1, 0),
                  csum = paste0(feature_name, cumsum(temp_id))) %>%
    tidyr::nest(data = c(row.id, col.id, diff, temp_id)) %>%
    dplyr::mutate(n = lapply(data, nrow)) %>%
    tidyr::unnest(n) %>% 
    tidyr::unnest(data) %>%
    dplyr::filter(n > min_points)
  
  tab <- dplyr::as_tibble(matrix(as.character(NA), nrow = nrow(gps_locations), ncol = 2,
                                  dimnames = list(NULL, 
                                                  paste0(feature_name, c("_polygon", "_burst")))))
  within$col.id <- as.character(within$col.id)
  
  tab[within$row.id,] <- within[c("col.id", "csum")]
  
  return(bind_cols(gps_locations, tab))
}
