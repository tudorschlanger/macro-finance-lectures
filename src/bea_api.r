## This is wrapper around the BEA API 
    
bea_api <- function(api_key_bea, dataname, frequency, table_ids, destfile) {
    
    UserID      <- rep(api_key_bea, length(table_ids))
    Method      <- rep('GetData', length(table_ids))
    datasetname <- rep(dataname, length(table_ids))
    Frequency   <- rep(frequency, length(table_ids))
    TableName   <- table_ids
    Year        <- rep('X', length(table_ids))  # all years selected
    ResultFormat<- rep("json", length(table_ids)) 
    
    params <- list(
        UserID          = UserID,
        Method          = Method,
        datasetname     = datasetname,
        Frequency       = Frequency,
        TableName       = TableName, 
        Year            = Year,
        ResultFormat    = ResultFormat
    )
    df <- purrr::pmap_dfr(
        .l = params,
        .f = ~ beaGet(list('UserID' = ..1, 'Method' = ..2, 'datasetname' = ..3,
                     'Frequency' = ..4, 'TableName' = ..5,  'Year' = ..6,
                     'ResultFormat' = ..7), asTable = TRUE, asWide = FALSE)

        )

    saveRDS(df, file = destfile)
}

# Simple example:
# beaGet( list('UserID' = "BC707C2A-6523-4AF8-8346-70E4AB34E9E5",
#       'Method' = 'GetData', 
#       'datasetname' = 'NIPA',
#       'Frequency' = 'A',
#       'TableName' = 'T60200A',  
#       'Year' = 'X',
#       'ResultFormat' = 'json'), asTable = TRUE, asWide = FALSE)
