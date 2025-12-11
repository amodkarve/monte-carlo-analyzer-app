library(quantmod)
library(xts)

##### ETF DATA FETCHING AND PROCESSING FUNCTIONS ##############################

# Fetch ETF price data from Yahoo Finance
# Returns an xts object of adjusted close prices
fetch_etf_data <- function(symbol, start_date, end_date) {
    tryCatch(
        {
            # Download data from Yahoo Finance
            getSymbols(
                symbol,
                src = "yahoo",
                from = start_date,
                to = end_date,
                auto.assign = FALSE,
                warnings = FALSE
            )[, 6] # Column 6 is adjusted close
        },
        error = function(e) {
            warning(sprintf("Failed to fetch data for %s: %s", symbol, e$message))
            NULL
        }
    )
}

# Calculate simple moving average
calculate_sma <- function(prices, n = 150) {
    if (length(prices) < n) {
        warning(sprintf("Not enough data points (%d) to calculate SMA(%d)", length(prices), n))
        return(rep(NA, length(prices)))
    }

    # Use TTR package's SMA function if available, otherwise manual calculation
    if (requireNamespace("TTR", quietly = TRUE)) {
        TTR::SMA(prices, n = n)
    } else {
        # Manual SMA calculation
        sma <- rep(NA, length(prices))
        for (i in n:length(prices)) {
            sma[i] <- mean(prices[(i - n + 1):i], na.rm = TRUE)
        }
        sma
    }
}

# Apply SMA filter: return price when above SMA, else NA
# This will be converted to 0 return later
apply_sma_filter <- function(prices, sma) {
    filtered <- prices
    # Set price to NA when below SMA
    filtered[prices < sma | is.na(sma)] <- NA
    filtered
}

# Convert price series to daily returns
# Handles NA values by setting return to 0
prices_to_returns <- function(prices) {
    if (is.null(prices) || length(prices) <= 1) {
        return(NULL)
    }

    # Calculate daily returns: (P_t / P_{t-1}) - 1
    returns <- diff(prices) / lag(prices, 1)

    # Remove the first NA from diff
    returns <- returns[-1]

    # Replace NA returns with 0 (no position)
    returns[is.na(returns)] <- 0

    returns
}

# Main function to build ETF returns
# Returns a list with:
#   - etf_returns_xts: xts object with daily returns for each ETF
#   - etf_names: character vector of ETF symbols
build_etf_returns <- function(sma_symbols = NULL,
                              buyhold_symbols = NULL,
                              start_date,
                              end_date) {
    all_symbols <- c()
    all_returns_list <- list()

    # Process SMA conditional ETFs
    if (!is.null(sma_symbols) && length(sma_symbols) > 0) {
        for (symbol in sma_symbols) {
            symbol <- trimws(symbol)
            if (symbol == "") next

            cat(sprintf("Fetching SMA conditional ETF: %s\n", symbol))

            # Fetch price data
            prices <- fetch_etf_data(symbol, start_date, end_date)

            if (is.null(prices)) {
                warning(sprintf("Skipping %s due to fetch error", symbol))
                next
            }

            # Calculate SMA 150
            sma <- calculate_sma(as.numeric(prices), n = 150)

            # Apply SMA filter
            filtered_prices <- apply_sma_filter(as.numeric(prices), sma)

            # Convert to xts with original dates
            filtered_prices_xts <- xts(filtered_prices, order.by = xts::index(prices))

            # Convert to returns
            returns <- prices_to_returns(filtered_prices_xts)

            if (!is.null(returns)) {
                all_symbols <- c(all_symbols, paste0(symbol, "_SMA150"))
                all_returns_list[[paste0(symbol, "_SMA150")]] <- returns
            }
        }
    }

    # Process buy-and-hold ETFs
    if (!is.null(buyhold_symbols) && length(buyhold_symbols) > 0) {
        for (symbol in buyhold_symbols) {
            symbol <- trimws(symbol)
            if (symbol == "") next

            cat(sprintf("Fetching buy-and-hold ETF: %s\n", symbol))

            # Fetch price data
            prices <- fetch_etf_data(symbol, start_date, end_date)

            if (is.null(prices)) {
                warning(sprintf("Skipping %s due to fetch error", symbol))
                next
            }

            # Convert to returns (no SMA filter)
            returns <- prices_to_returns(prices)

            if (!is.null(returns)) {
                all_symbols <- c(all_symbols, paste0(symbol, "_BH"))
                all_returns_list[[paste0(symbol, "_BH")]] <- returns
            }
        }
    }

    # Combine all ETF returns into a single xts object
    if (length(all_returns_list) == 0) {
        return(NULL)
    }

    # Merge all returns (outer join to get all dates)
    etf_returns_xts <- do.call(merge, c(all_returns_list, all = TRUE))

    # Replace NA with 0 (no position on those dates)
    etf_returns_xts[is.na(etf_returns_xts)] <- 0

    list(
        etf_returns_xts = etf_returns_xts,
        etf_names = all_symbols
    )
}

# Parse comma-separated symbols into a vector
parse_symbols <- function(symbol_string) {
    if (is.null(symbol_string) || symbol_string == "") {
        return(NULL)
    }

    symbols <- strsplit(symbol_string, ",")[[1]]
    symbols <- trimws(symbols)
    symbols <- symbols[symbols != ""]

    if (length(symbols) == 0) {
        return(NULL)
    }

    symbols
}
