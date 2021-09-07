## This is wrapper around the FRED API 
    
fred_api <- function(variables, frequency, agg_method, destfile) {
    freq   <- rep(frequency, length(variables))  # repeat frequency letter to match length of vector variables
    method <- rep(agg_method, length(variables))
    order  <- rep("asc", length(variables))  # ascending order
    units  <- rep("lin", length(variables))  # levels, no transformation done to data
    # Note: we don't specify the time period so it will pull all data available
    
    params <- list(
        series_id          = variables,
        frequency          = freq,
        aggregation_method = method,
        sort_order         = order,
        units              = units
    )
    df <- purrr::pmap_dfr(
        .l = params,
        .f = ~ fredr(series_id = ..1, frequency = ..2, aggregation_method = ..3,
                    sort_order = ..4, units = ..5)
    )
    saveRDS(df, file = destfile)
}