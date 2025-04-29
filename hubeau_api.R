# Hub'Eau Qualité des cours d'eau API Integration
# Documentation: https://hubeau.eaufrance.fr/page/api-qualite-cours-deau
# API Version: v2

library(httr)
library(jsonlite)
library(dplyr)
library(tidyr)
library(ggplot2)

#' Base URL for the Hub'Eau API v2
HUBEAU_QUALITE_BASE_URL <- "https://hubeau.eaufrance.fr/api/v2/qualite_rivieres"

#' Format query parameters for the Hub'Eau API
#' 
#' @param params List of parameters to format
#' @return A character string for the URL query
format_query_params <- function(params) {
  # Remove NULL values from the parameter list
  params <- params[!sapply(params, is.null)]
  
  # Handle special parameters that might be vectors (like code_parametre)
  for (name in names(params)) {
    if (length(params[[name]]) > 1) {
      params[[name]] <- paste(params[[name]], collapse = ",")
    }
  }
  
  return(params)
}

#' Execute a request to the Hub'Eau API
#' 
#' @param endpoint API endpoint to call
#' @param params Parameters for the query
#' @param format_json Whether to parse JSON response
#' @return API response, parsed if format_json is TRUE
hubeau_request <- function(endpoint, params = list(), format_json = TRUE) {
  # Build URL
  url <- paste0(HUBEAU_QUALITE_BASE_URL, "/", endpoint)
  
  # Format parameters
  params_formatted <- format_query_params(params)
  
  # Execute request with error handling
  tryCatch({
    response <- GET(url, query = params_formatted)
    
    # Check for HTTP errors
    if (http_error(response)) {
      status_code <- status_code(response)
      message <- content(response, "text", encoding = "UTF-8")
      stop("API request failed with status ", status_code, ": ", message)
    }
    
    # Parse response
    if (format_json) {
      json_data <- content(response, "text", encoding = "UTF-8")
      return(fromJSON(json_data))
    } else {
      return(content(response))
    }
  }, error = function(e) {
    warning("Error in API request: ", e$message)
    return(NULL)
  })
}

#' Get stations near a given location (commune or coordinates)
#' 
#' @param commune_name Name of the commune
#' @param code_commune INSEE code of the commune
#' @param longitude Longitude in WGS84
#' @param latitude Latitude in WGS84
#' @param distance Search radius in kilometers
#' @param size Number of results to return
#' @return Data frame of stations
get_stations <- function(commune_name = NULL, code_commune = NULL, 
                          longitude = NULL, latitude = NULL, 
                          distance = 5, size = 20) {
  
  # Prepare parameters based on search type
  params <- list(size = size)
  
  # Search by coordinates (radius search)
  if (!is.null(longitude) && !is.null(latitude)) {
    params$longitude <- longitude
    params$latitude <- latitude
    params$distance <- distance
  }
  
  # Search by commune name
  if (!is.null(commune_name)) {
    params$libelle_commune <- commune_name
  }
  
  # Search by commune code
  if (!is.null(code_commune)) {
    params$code_commune <- code_commune
  }
  
  # Make the API request
  response <- hubeau_request("station_pc", params)
  
  if (!is.null(response) && "data" %in% names(response)) {
    return(response$data)
  } else {
    warning("No stations found or API error")
    return(data.frame())
  }
}

#' Get nitrate measurements for a station
#' 
#' @param code_station The station code
#' @param date_debut_prelevement Start date in YYYY-MM-DD format
#' @param date_fin_prelevement End date in YYYY-MM-DD format
#' @param size Number of results to return
#' @return Data frame of nitrate measurements
get_nitrates_for_station <- function(code_station, 
                                     date_debut_prelevement = NULL,
                                     date_fin_prelevement = NULL,
                                     size = 1000) {
  # Code for nitrates (Sandre parameter code for Nitrates: 1340)
  code_parametre <- "1340"
  
  params <- list(
    code_station = code_station,
    code_parametre = code_parametre,
    date_debut_prelevement = date_debut_prelevement,
    date_fin_prelevement = date_fin_prelevement,
    sort = "desc", # Most recent first
    size = size
  )
  
  response <- hubeau_request("analyse_pc", params)
  
  if (!is.null(response) && "data" %in% names(response)) {
    return(response$data)
  } else {
    warning("No nitrate data found or API error")
    return(data.frame())
  }
}

#' Get measurements for a specific parameter at a station
#' 
#' @param code_station The station code
#' @param code_parametre The parameter code (from Sandre reference)
#' @param date_debut_prelevement Start date in YYYY-MM-DD format
#' @param date_fin_prelevement End date in YYYY-MM-DD format
#' @param size Number of results to return
#' @return Data frame of parameter measurements
get_parameter_for_station <- function(code_station, 
                                      code_parametre,
                                      date_debut_prelevement = NULL,
                                      date_fin_prelevement = NULL,
                                      size = 1000) {
  params <- list(
    code_station = code_station,
    code_parametre = code_parametre,
    date_debut_prelevement = date_debut_prelevement,
    date_fin_prelevement = date_fin_prelevement,
    sort = "desc", # Most recent first
    size = size
  )
  
  response <- hubeau_request("analyse_pc", params)
  
  if (!is.null(response) && "data" %in% names(response)) {
    return(response$data)
  } else {
    warning("No data found for parameter or API error")
    return(data.frame())
  }
}

#' Get all available parameters for a station
#' 
#' @param code_station The station code
#' @param size Number of results to return
#' @return Data frame of unique parameters measured at the station
get_available_parameters <- function(code_station, size = 100) {
  params <- list(
    code_station = code_station,
    size = size
  )
  
  response <- hubeau_request("analyse_pc", params)
  
  if (!is.null(response) && "data" %in% names(response) && nrow(response$data) > 0) {
    # Extract unique parameters without using pipe operator
    data <- response$data
    if (all(c("code_parametre", "libelle_parametre", "symbole_unite", "code_unite") %in% colnames(data))) {
      # Select needed columns
      params_df <- data[, c("code_parametre", "libelle_parametre", "symbole_unite", "code_unite")]
      # Get distinct values
      params_df <- unique(params_df)
      return(params_df)
    } else {
      # Handle missing columns - return what we have
      warning("Some columns missing in API response")
      # Create basic dataframe with available columns
      return(unique(data[, intersect(c("code_parametre", "libelle_parametre", "symbole_unite", "code_unite"), colnames(data))]))
    }
  } else {
    warning("No parameters found or API error")
    return(data.frame())
  }
}

#' Find stations by searching for a river name
#' 
#' @param river_name Name of the river
#' @param size Number of results to return
#' @return Data frame of stations
find_stations_by_river <- function(river_name, size = 20) {
  params <- list(
    nom_cours_eau = river_name,
    size = size
  )
  
  response <- hubeau_request("station_pc", params)
  
  if (!is.null(response) && "data" %in% names(response)) {
    return(response$data)
  } else {
    warning("No stations found for this river or API error")
    return(data.frame())
  }
}

#' Plot nitrate concentration over time for a station
#' 
#' @param nitrates_data Data frame from get_nitrates_for_station
#' @param station_name The name of the station (for plot title)
#' @return ggplot object
plot_nitrates <- function(nitrates_data, station_name = "Station") {
  # Check if data exists
  if (nrow(nitrates_data) == 0) {
    warning("No nitrate data to plot")
    return(NULL)
  }
  
  # Ensure date_prelevement is a Date type
  nitrates_data$date_prelevement <- as.Date(nitrates_data$date_prelevement)
  
  # Create the plot
  p <- ggplot(nitrates_data, aes(x = date_prelevement, y = resultat)) +
    geom_point() +
    geom_line() +
    labs(
      title = paste("Évolution des Nitrates -", station_name),
      x = "Date",
      y = "Concentration (mg/L)",
      caption = "Source: Hub'Eau Qualité des cours d'eau"
    ) +
    theme_minimal() +
    scale_x_date(date_labels = "%Y-%m", date_breaks = "1 year") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  return(p)
}

#' Get all stations in a department
#' 
#' @param code_departement Department code
#' @param size Number of results to return
#' @return Data frame of stations
get_stations_by_department <- function(code_departement, size = 100) {
  params <- list(
    code_departement = code_departement,
    size = size
  )
  
  response <- hubeau_request("station_pc", params)
  
  if (!is.null(response) && "data" %in% names(response)) {
    return(response$data)
  } else {
    warning("No stations found in this department or API error")
    return(data.frame())
  }
}

#' Common water quality parameters with their Sandre codes
#' 
#' @return Named vector of parameter codes
get_common_parameter_codes <- function() {
  return(c(
    "Nitrates" = "1340",
    "Phosphore total" = "1350",
    "Ammonium" = "1335",
    "pH" = "1302",
    "Conductivité à 25°C" = "1303",
    "Oxygène dissous" = "1311",
    "Température de l'eau" = "1301",
    "Matières en suspension" = "1305",
    "DBO5" = "1313",
    "DCO" = "1314"
  ))
} 