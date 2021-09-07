## This is wrapper around the IMF API 

# These functions will help the user find their way in the IMF database. 
  # codelist <- imf_codelist("IFS", return_raw = FALSE, times = 3)
  # indicators <- imf_codes("CL_INDICATOR_IFS", return_raw = FALSE, times = 3)
  # country_codes <- imf_codes("CL_AREA_IFS", return_raw = FALSE, times = 3)


imf_api <- function(database_id, indicator, country = "all", start_year = 2000, frequency = "A", destfile) {
  
  # Automatically convert list of countries to ISO2C format - otherwise IMF API errors
  if (c("all") %in% country == FALSE) {
    country <- lapply(country, countryname, destination = "iso2c", warn = TRUE)
    country <- as.character(country)  # convert to list of characters 
  }
  
  df <- imf_data(
    database_id = database_id,  # Only accepts one database at a time 
    indicator = indicator,      # Provide list of indicators as strings e.g. c("", "")
    country = country,          # Provide list of countries as strings, or "all" for the entire list
    start = start_year,         # Start year must be an integer/double
    end = current_year(),
    freq = frequency,
    return_raw = FALSE,
    print_url = FALSE,
    times = 3
  )
  saveRDS(df, file = destfile)
  
}
