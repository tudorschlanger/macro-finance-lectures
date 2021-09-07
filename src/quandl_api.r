## Quandl API

# Some test code for the below function 

  # vintage_folder <- here("data/vintages", "2021-08")
  # url <- "https://static.quandl.com/API+Descriptions/WHO/ccodes.txt"
  # destfile <- here(vintage_folder, "iso3c_codes.txt")
  # download.file(url = url, destfile, mode = "wb") 
  # source(here("src/quandl_api.r"))
  # indicator   <- "BCA"
  # database_id <- "ODA" 
  # api_key_quandl <- "fFvmensY2sDXx13zV-xH"
  # country <- c("USA","AFR","CHN" )
  # # country <- "all"
  # df <- quandl_api(database_id = "ODA", 
  #                  indicator   = indicator,
  #                  country     = country,
  #                  api_key     = api_key_quandl 
  # )

quandl_api <- function(database_id, indicator, country, api_key){
  
  ## Prepare arguments input
  if ("all" %in% country) {
    df <- read.delim(here(vintage_folder, "iso3c_codes.txt"), sep = "|", header = FALSE) 
    country <- df$V1 
  }
  
  ## Quandl API call (start with one indicator, and loop over countries)
  
  quandl_fun <- function( country, indicator) {
  str <- paste(database_id, "/", country, "_", indicator, sep = "")

  out <- tryCatch(
    {
      df <- Quandl(str, api_key = api_key)
      df$country <- rep(country, nrow(df))  # add the country name
      return(df)
    }, 
    error=function(cond) {
      # message(sprintf("There is no country code %s for the indicator %s.", country, indicator))
    }
  )
  }
  
  indicator <- rep(indicator, length(country))
  params = list(
    country     = country, 
    indicator   = indicator
  )
  ## The following function will stack each country row-wise
  ## Data columns are Date and Value (no country indicator)
  df <- purrr::pmap_dfr(
    .l = params,
    .f = ~ quandl_fun( country = ..1, indicator = ..2 )
  )
  
  return(df)
  
}


