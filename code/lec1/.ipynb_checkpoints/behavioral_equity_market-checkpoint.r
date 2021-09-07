## This script replicates Shiller 1981 using a slightly different data set
## The replication code was written by Caraiani and Anghel (forthcoming)

behavioral_equity_market <- function(data_vintage, path_fig) {
    path_data <- here("data/vintages", data_vintage)
    
    ## First specify the packages of interest
    packages <- c("ggplot2", "readxl", "dplyr", "tidyr", "dplyr", "purrr", "stringr",
                  "Quandl", "viridis", "ggpmisc", "readstata13", "imfr", "writexl", "fredr")
    
    source(here("src/check_packages.r"))
    check_packages(packages) # Check whether packages are installed already and load them 
    
    ## Collect data on S&P 500 prices and dividends from QUANDL
    ## Note: Quandl does not requires an API unless user exceeds 50 calls a day.
    
    curr_year   <- as.double(format(Sys.Date(), "%Y")) # don't change this
    
    first_years <- c(1871, 1947, 1960, 1980)
    # first_years <- c(1871)
    last_years  <- c(1947, 1979, 2009, as.double(format(Sys.Date(), "%Y")))
    # last_years <- c(1979)
    
    reg_results <- data.frame(matrix(ncol = 5, nrow = 0))
    reg_columns <- c("first year", "last year", "specification", "b", "r2")
    colnames(reg_results) <- reg_columns
    
    for (f in first_years) {
        first_year <- f
    
        for (t in last_years) {
            last_year <- t  # can be different from current year
            if (last_year <= first_year) next  # skip iteration if last year is smaller than first year
    
            #######################################
            ########### DATA PREP #################
            #######################################
            # Read data first
            div   <- readRDS(file = here(path_data, "sp500_dividends.rds")) 
            price <- readRDS(file = here(path_data, "sp500_prices.rds"))

            # Prep data: Dividends
            div$month <- as.double(format(div$Date, "%m"))
            div$year <- as.double(format(div$Date, "%Y"))
            div <- subset(div, (year <= last_year & year >= first_year))
            div <- div[order(div$Date), ]
            div <- div %>%  # There may be multiple observations per year - we will choose the first observation of every year 
                group_by(year) %>%
                filter(row_number() == 1)
            div$Date <- format(div$Date,"%Y")
            div <- div %>% rename(div = Value) %>% select(-c("month", "year"))

            # Prep data: Prices
            price$month <- as.double(format(price$Date, "%m"))
            price$year <- as.double(format(price$Date, "%Y"))
            price$day <- as.double(format(price$Date, "%d"))
            price <- subset(price, (year <= last_year & year >= first_year))
            price <- price[order(price$Date), ]
            price <- price %>%  # There may be multiple observations per year so we need to choose the last observation of every year 
                     group_by(year) %>%
                     filter(row_number() == 1)
            
            price$Date <- format(price$Date,"%Y")
            price <- price %>% rename(price = Value)
            price <- price %>% select(-c("day", "month", "year"))

            ## Add interest rates from JST database
            rawdata <- read.dta13(here(path_data, "JSTdatasetR5.dta"))
            myvars <- c("year", "country", "cpi", 
                        "stir",  # short-term gov bills
                        "ltrate" # long-term gov bonds
            )
            rates <- rawdata[rawdata$country == "USA", myvars]
            rates <- rates %>% 
                    rename(Date = year)  %>% 
                    mutate(Date = as.character(Date),
                           stir = stir/100,
                           ltrate = ltrate/100)

            ## Merging and indexing
            df <- left_join(div, price, by = "Date")
            df <- left_join(df, rates, by = "Date")

            df$Date <- as.Date(df$Date, format = "%Y")
            df$year <- as.double(format(df$Date, "%Y"))
            df <- df[order(df$year), ] # Note: the data must be ordered in increasing order of time for the Shiller algo to work 

            df <- df %>%
                    mutate(div_index   = div/div[year == first_year]*100) %>%
                    mutate(price_index = price/price[year == first_year]*100) %>%
                    mutate(infl = (cpi/lag(cpi, 1) - 1)) %>%             # inflation defined as growth in yearly CPI inflation 
                    mutate(stir_real = (1 + stir) / (1 + infl) - 1) %>%  # deflate nominal shot-term interest rates
                    mutate(ltrate_real = (1 + ltrate) / (1 + infl) - 1) %>% # deflate nominal long-term interest rates
                    mutate(price_div_ratio = price / div,
                           div_price_ratio = div / price) %>%
                    mutate(price_lead5 = lead(price, n = 5),
                        annual_return_5yr = (price_lead5/price)^(1/5) - 1,   # real annual returns
                        annual_return_5yr_minus_stir = annual_return_5yr - stir_real,  
                        annual_return_5yr_minus_ltrate = annual_return_5yr - ltrate_real, 
                        year = as.double(format(Date, "%Y")),
                        ) %>%
                    mutate(return_5yr = (1 + annual_return_5yr)^5 - 1,       # real returns over the whole period
                           return_5yr_minus_stir = (1 + annual_return_5yr_minus_stir)^5 - 1,
                           return_5yr_minus_ltrate = (1 + annual_return_5yr_minus_ltrate)^5 - 1)

            
            ## Shiller method of detrending data and constructing PV of dividends
                
            # Compute long run exponential growth rate (last observation - first observation)
            g <- (df[df$year==last_year, "price"] / df[df$year==first_year, "price"]) ^ (1/(nrow(df)-1)) - 1

            # Detrend prices
            df$price_detrended <- df$price
            for (i in 1:nrow(df)) {
                df[i, "price_detrended"] <- df[i, "price_detrended"] / (1+g)^(i-nrow(df))
            }

            # Detrend dividends
            df$div_detrended <- df$div
            for (i in 1:nrow(df)) {
                df[i, "div_detrended"] <- df[i, "div_detrended"] / (1+g)^(i-nrow(df))
            }

            # Compute constant discount rate as the mean yield of stocks over chosen period
            r <- mean(df$div_detrended) / mean(df$price_detrended)

            # Set terminal price 
            tp <- mean(df$price_detrended)

            # Compute fundamental values
            df$p_star <- df$price_detrended
            df[nrow(df),"p_star"] <- tp
            for (i in (nrow(df)-1):1) { 
                df[i,"p_star"] = (df[i+1,"p_star"] + df[i,"div_detrended"]) / (1 + r)
            }
    
            #######################################
            ########### PLOTTING ##################
            #######################################
            
            ## PLOTTING Shiller graph
            # load the formatting and saving settings from another script
            source(here("src/format_plots.r"))
            source(here("src/save_plots.r"))
    
            if (last_year - first_year <= 60) {
                xticks = 10
            } else {
                xticks = 20
            }
            xlimits <- c(as.double(first_year), as.double(format(Sys.Date(), "%Y")))
            
            # Wide to long format (better for ggplot)
            df_tmp <- df %>%
                       select(c("price_detrended", "p_star", "year")) %>%
                       pivot_longer(c("price_detrended", "p_star"), names_to = "series", values_to = "value") 
            df_tmp$series <- gsub("price_detrended", "Actual", df_tmp$series, fixed = TRUE)
            df_tmp$series <- gsub("p_star", "Expected", df_tmp$series, fixed = TRUE)
    
            ggplot(data = df_tmp, aes(x = year, y = value)) +
                geom_line(aes(color = series, linetype = series), size = 2) +
                scale_x_continuous(breaks=seq(xlimits[1], xlimits[2], xticks)) +
                labs(y = "", x = "", title = "") + 
                format +
                scale_linetype_manual(values = c("solid", "dotdash")) +
                scale_color_manual(values = c(purple, green))
    
            # Save
            savefolder <- here(path_fig, "behavioral_finance")
            filename <- paste("equity_market_", as.character(first_year), "_", as.character(last_year), ".png", sep = "")
            save_plots(filename, savefolder)
    
            ## PLOTTING price to dividend against subsequent 7-year return
            df_tmp <- na.omit(df)
            nth_year <- 5 # Label every nth year
            df_tmp <- df_tmp %>% mutate(year = as.character(year))
            df_tmp$year[seq(1, nrow(df_tmp), nth_year)] <- NA
            formula = y ~ x
            xlimits <- c(0, NA)
    
            ggplot(data = df_tmp, aes(x = div_price_ratio, y = annual_return_5yr)) +
                geom_smooth(method = lm, formula = formula, se = FALSE, color = purple, size = 2) +
                geom_text(aes(label = year), fontface = "bold",  size = 5, color = green,
                         hjust = 0, nudge_x = 0, check_overlap = TRUE) +
                stat_poly_eq(formula = formula,
                            aes(label = paste(stat(rr.label), sep = ", ")),
                            label.x = "right", label.y = "top", size = 6, geom = "label_npc"
                            ) +
                xlim(xlimits) +
                labs(
                    x = "Dividend-to-price ratio",
                    y = "5-year annual return",
                ) + 
                format 
    
            filename <- paste("equity_market_pd_return_", as.character(first_year), "_", as.character(last_year), ".png", sep = "")
            save_plots(filename, savefolder)
    
            ggplot(data = df_tmp, aes(x = div_price_ratio, y = annual_return_5yr_minus_stir)) +
                geom_smooth(method = lm, formula = formula, se = FALSE, color = purple, size = 2) +
                geom_text(aes(label = year), fontface = "bold",  size = 5, color = green,
                         hjust = 0, nudge_x = 0, check_overlap = TRUE) +
                stat_poly_eq(formula = formula,
                            aes(label = paste(stat(rr.label), sep = ", ")),
                            label.x = "right", label.y = "top", size = 6, geom = "label_npc"
                            ) +
                xlim(xlimits) +
                labs(
                    x = "Dividend-to-price ratio",
                    y = "5-year annual return - 3-month Bill yield",
                ) + 
                format 
    
            filename <- paste("equity_market_pd_return_stir", as.character(first_year), "_", as.character(last_year), ".png", sep = "")
            save_plots(filename, savefolder)
    
            ggplot(data = df_tmp, aes(x = div_price_ratio, y = annual_return_5yr_minus_ltrate)) +
                geom_smooth(method = lm, formula = formula, se = FALSE, color = purple, size = 2) +
                geom_text(aes(label = year), fontface = "bold",  size = 5, color = green,
                         hjust = 0, nudge_x = 0, check_overlap = TRUE) +
                stat_poly_eq(formula = formula,
                            aes(label = paste(stat(rr.label), sep = ", ")),
                            label.x = "right", label.y = "top", size = 6, geom = "label_npc"
                            ) +
                xlim(xlimits) +
                labs(
                    x = "Dividend-to-price ratio",
                    y = "5-year annual return - 10-year Bond yield",
                ) + 
                format 
    
            filename <- paste("equity_market_pd_return_ltrate", as.character(first_year), "_", as.character(last_year), ".png", sep = "")
            save_plots(filename, savefolder)
    
            # caption <- paste("Data provider: Quandl. Original data source: Standard & Poor's and Robert Shiller. 
            #                 Period: ", as.character(first_year), " - ", as.character(last_year), " Note: 
            #                 The 7-year return is calculated as the 7-year growth in prices.", sep ="") 
            
            #################################
            ########## REGRESSIONS ##########
            #################################
    
            # model<- lm(annual_return_5yr ~ price_div_ratio, data = df_tmp)
            # r1 <- c(first_year, last_year, as.character(summary(model)$call[2]), summary(model)$coefficients[2,1], summary(model)$r.squared)
            # 
            # model<- lm(annual_return_5yr_minus_stir ~ price_div_ratio, data = df_tmp)
            # r2 <- c(first_year, last_year, as.character(summary(model)$call[2]), summary(model)$coefficients[2,1], summary(model)$r.squared)
            # 
            # model<- lm(annual_return_5yr_minus_ltrate ~ price_div_ratio, data = df_tmp)
            # r3 <- c(first_year, last_year, as.character(summary(model)$call[2]), summary(model)$coefficients[2,1], summary(model)$r.squared)
            #                         
            # model<- lm(annual_return_5yr ~ div_price_ratio, data = df_tmp)
            # r4 <- c(first_year, last_year, as.character(summary(model)$call[2]), summary(model)$coefficients[2,1], summary(model)$r.squared)
            # 
            # model<- lm(annual_return_5yr_minus_stir ~ div_price_ratio, data = df_tmp)
            # r5 <- c(first_year, last_year, as.character(summary(model)$call[2]), summary(model)$coefficients[2,1], summary(model)$r.squared)
            # 
            # model<- lm(annual_return_5yr_minus_ltrate ~ div_price_ratio, data = df_tmp)
            # r6 <- c(first_year, last_year, as.character(summary(model)$call[2]), summary(model)$coefficients[2,1], summary(model)$r.squared)
            # 
            # model<- lm(return_5yr ~ div_price_ratio, data = df_tmp)
            # r7 <- c(first_year, last_year, as.character(summary(model)$call[2]), summary(model)$coefficients[2,1], summary(model)$r.squared)
            # 
            # model<- lm(return_5yr_minus_stir ~ div_price_ratio, data = df_tmp)
            # r8 <- c(first_year, last_year, as.character(summary(model)$call[2]), summary(model)$coefficients[2,1], summary(model)$r.squared)
            # 
            # model<- lm(return_5yr_minus_ltrate ~ div_price_ratio, data = df_tmp)
            # r9 <- c(first_year, last_year, as.character(summary(model)$call[2]), summary(model)$coefficients[2,1], summary(model)$r.squared)
            # 
            # reg_results <- do.call(rbind, list(reg_results, r1, r2, r3, r4, r5, r6, r7, r8, r9))                        
            # colnames(reg_results) <- reg_columns
        }
    
    }

    # write_xlsx(reg_results, "table/reg_results_cochrane_2011.xlsx")
}