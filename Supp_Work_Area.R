

valid_states <- paste0("@", c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA",
                              "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD",
                              "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ",
                              "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC",
                              "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY"))


# Helper function to check ICAO IDs format
validate_icaoIDs <- function(icaoIDs) {
  valid_special_ids <- c("@TOP", "@TOPE", "@TOPC", "@TOPW", "@USN", "@USE", "@USS", "@USW", "#US")
  valid_state <- icaoIDs %in% valid_states
  valid_special <- icaoIDs %in% valid_special_ids
  valid_icao_single <- grepl("^K[A-Z]{3}$", icaoIDs, ignore.case = TRUE)
  valid_icao_multiple <- grepl("^(K[A-Z]{3}[ ,]?)+$", icaoIDs, ignore.case = TRUE)
  return(valid_state || valid_special || valid_icao_single || valid_icao_multiple)
}


# Helper function to check if ICAO ID is in valid ICAO ID vectors
check_icao_in_valid_vectors <- function(icaoID) {
  all_valid_icaoIDs <- c(valid_icaoID_1, valid_icaoID_2, valid_icaoID_3, valid_icaoID_4)
  return(icaoID %in% all_valid_icaoIDs)
}

build_url <- function(base_url = "https://aviationweather.gov/api/data/", 
                      endpoint, 
                      icaoIDs = "#US", 
                      hours = NULL, 
                      time = NULL) {
  
  # Define the valid endpoints (in lowercase for comparison)
  valid_endpoints <- c("metar", "taf", "airport")
  
  # Convert endpoint to lowercase for internal processing
  endpoint_lower <- tolower(endpoint)
  
  # Check if the provided endpoint is valid
  if (!(endpoint_lower %in% valid_endpoints)) {
    stop("Invalid endpoint. Choose from: ", paste(valid_endpoints, collapse = ", "))
  }
  
  # Adjust the validation pattern for icaoIDs
  if (!is.null(icaoIDs)) {
    if (!(grepl("^@[A-Z]{2}$", icaoIDs, ignore.case = TRUE) ||
          grepl("^@[A-Z]{4}$", icaoIDs, ignore.case = TRUE) ||
          grepl("^@?(?:[A-Z]{4}[ ,]?)+$", icaoIDs, ignore.case = TRUE) ||
          icaoIDs %in% c("@TOP", "@TOPE", "@TOPC", "@TOPW", "@USN", "@USE", "@USS", "@USW", "#US") ||
          icaoIDs %in% paste0("@", valid_states))) {
      stop("STOPERROR1: Invalid icaoIDs format. Must be a two-letter state abbreviation prefixed by '@', a four-letter ICAO ID, multiple four-letter ICAO IDs separated by commas or spaces, or a special identifier (@TOP, @TOPE, @TOPC, @TOPW, @USN, @USE, @USS, @USW, #US).")
    }
    if (endpoint_lower == "taf" && icaoIDs == "#US") {
      stop("STOPERROR2: icaoID = '#US' is not allowed for endpoint 'taf'.")
    }
  } else if (endpoint_lower %in% c("taf", "airport")) {
    stop("icaoIDs parameter is required for endpoints 'TAF' and 'airport'.")
  }

  
  #build_url(endpoint = "metar", icaoIDs = "#US")
  #build_url(endpoint = "taf", icaoIDs = "#US")
  #build_url(endpoint = "airport", icaoIDs = "#US")
  
    
  # Validate hours for METAR
  if (endpoint_lower == "metar" && !is.null(hours)) {
    if (!(is.numeric(hours) && hours == as.integer(hours) && hours > 0)) {
      stop("Invalid hours. Must be a positive whole number.")
    }
  }
  
  # Validate time for TAF
  if (endpoint_lower == "taf" && !is.null(time)) {
    if (!(time %in% c("issue", "valid"))) {
      stop("Invalid time. Must be 'issue' or 'valid'.")
    }
  }
  
  # Initialize the query parameters
  params <- list()
  
  # Add ids parameter if provided
  if (!is.null(icaoIDs)) {
    if (endpoint_lower %in% c("metar", "taf", "airport")) {
      if (icaoIDs %in% c("@TOP", "@TOPE", "@TOPC", "@TOPW", "@USN", "@USE", "@USS", "@USW") || icaoIDs %in% valid_states) {
        params$ids <- paste0(icaoIDs)  # Keep special identifiers as-is with '@'
      } else if (icaoIDs == "#US" && endpoint_lower != "taf") {
        params$ids <- gsub("#", "%23",icaoIDs)  # Keep #US as-is for endpoints other than 'taf'
      } else if (grepl("^[A-Z]{4}$", icaoIDs, ignore.case = TRUE)) {
        params$ids <- icaoIDs
      } else if (grepl(",", icaoIDs)) {
        # Allow comma-separated ids
        params$ids <- gsub(" ", "%2C", icaoIDs)
      } else {
        stop("Invalid icaoIDs format. Must be a two-letter state abbreviation prefixed by '@', a four-letter ICAO ID, multiple four-letter ICAO IDs separated by commas, or a special identifier (@TOP, @TOPE, @TOPC, @TOPW, @USN, @USE, @USS, @USW, #US).")
      }
    }
  }
  
  # Add specific parameters based on the endpoint
  if (endpoint_lower == "metar" && !is.null(hours)) {
    params$hours <- hours
  } else if (endpoint_lower == "taf" && !is.null(time)) {
    params$time <- time
  }
  
  # Add format=json as the default parameter
  params$format <- "json"
  
  # Construct the complete URL with query parameters
  if (endpoint_lower == "metar") {
    if (is.null(hours)) {
      complete_url <- paste0(base_url, "metar?", "ids=", params$ids, "&format=json")
    } else {
      complete_url <- paste0(base_url, "metar?", "ids=", params$ids, "&format=json", "&hours=", hours)
    }
  } else if (endpoint_lower == "taf") {
    if (is.null(time)) {
      complete_url <- paste0(base_url, "taf?", "ids=", params$ids,"&format=json")
    } else {
      complete_url <- paste0(base_url, "taf?", "ids=", params$ids,"&format=json", "&time=", time)
    }
  } else if (endpoint_lower == "airport") {
    complete_url <- paste0(base_url, "airport?", "ids=", params$ids, "&format=json")
  }
  
  # Print the complete URL for debugging
  print(complete_url)
  
  # Fetch and parse JSON data
  response <- httr::GET(complete_url)
  parsed_data <- jsonlite::fromJSON(rawToChar(response$content))
  
  # Convert parsed data to tibble
  parsed_tibble <- tibble::as_tibble(parsed_data)
  
  # Customize tibble based on the endpoint
  if (endpoint_lower == "metar") {
    parsed_tibble <- dplyr::select(parsed_tibble, metar_id, icaoId, obsTime, reportTime, temp, wspd, visib, name)
  }
  
  # Return the parsed tibble
  return(parsed_tibble)
}

#single_icao <- build_url(endpoint = "metar", icaoIDs = "@GA", hours = 6);#single_icao;
#mutlip_icao <- build_url(endpoint = "metar", icaoIDs = "KATL,KAPF", hours = 6);#mutlip_icao;
#state_icao <- build_url(endpoint = "metar", icaoIDs = "@NY", hours = 6);#state_icao;
#country_icao <- build_url(endpoint = "metar", icaoIDs = "#US", hours = 6);#country_icao;
#top_special <- build_url(endpoint = "metar", icaoIDs = "@TOPE", hours = 6);#top_special;
#us_special <- build_url(endpoint = "metar", icaoIDs = "@USW", hours = 6);#us_special;


#single_icao <- build_url(endpoint = "taf", icaoIDs = "KATL");single_icao;
#mutlip_icao <- build_url(endpoint = "taf", icaoIDs = "KATL,KAPF");mutlip_icao;
#state_icao <- build_url(endpoint = "taf", icaoIDs = "@NY");state_icao$name;
#country_icao <- build_url(endpoint = "taf", icaoIDs = "#US");country_icao; #Does not accept "#US"
#top_special <- build_url(endpoint = "taf", icaoIDs = "@TOPE");top_special$name;
#us_special <- build_url(endpoint = "taf", icaoIDs = "@USW");us_special$name;


#single_icao <- build_url(endpoint = "airport", icaoIDs = "KATL");single_icao;
#mutlip_icao <- build_url(endpoint = "airport", icaoIDs = "KATL,KAPF");mutlip_icao;
#state_icao <- build_url(endpoint = "airport", icaoIDs = "@NY");state_icao;
#country_icao <- build_url(endpoint = "airport", icaoIDs = "#US");country_icao;
#top_special <- build_url(endpoint = "airport", icaoIDs = "@TOPE");top_special;
#us_special <- build_url(endpoint = "airport", icaoIDs = "@USW");us_special;

