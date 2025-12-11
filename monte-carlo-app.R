library(shiny)
library(shinyjs)
library(DT)
library(pso)
library(xts)
library(ggplot2)
library(plotly)
library(shinyFiles)

# Source ETF functions
source("etf_functions.R")

##### CORE FUNCTIONS FROM ORIGINAL SCRIPT ####################################

read_strategy_xts <- function(file, baseDir) {
    full_path <- file.path(baseDir, file)
    d <- read.csv(full_path, stringsAsFactors = FALSE)

    # normalize date (column name 'Date' in your files)
    d$date <- as.Date(d$Date)

    # column 7 = percent return * 100 at base allocation
    # convert to decimal portfolio return at base allocation
    ret_dec <- as.numeric(d[[7]]) / 100

    xts(ret_dec, order.by = d$date)
}

build_unit_R <- function(baseDir, strategy_files, base_alloc, date_merge_method = "union") {
    full_paths <- file.path(baseDir, strategy_files)

    strategy_xts_list <- lapply(strategy_files, function(f) read_strategy_xts(f, baseDir))

    if (date_merge_method == "intersection") {
        # Inner join - only overlapping dates
        R_base_xts <- do.call(merge, c(strategy_xts_list, all = FALSE))
    } else {
        # Outer join - all dates (union)
        R_base_xts <- do.call(merge, c(strategy_xts_list, all = TRUE))
        # replace missing returns with 0
        R_base_xts[is.na(R_base_xts)] <- 0
    }

    R_base <- as.matrix(R_base_xts)

    # name columns according to base_alloc names
    colnames(R_base) <- names(base_alloc)
    rownames(R_base) <- NULL

    # convert from "return at base_alloc" to "return per 100% allocation"
    unit_R <- sweep(R_base, 2, base_alloc, "/")

    list(
        unit_R = unit_R,
        dates  = zoo::index(R_base_xts),
        R_base = R_base
    )
}

# Build combined unit_R from both strategies and ETFs
build_combined_unit_R <- function(baseDir,
                                  strategy_files,
                                  strategy_base_alloc,
                                  etf_sma_symbols = NULL,
                                  etf_buyhold_symbols = NULL,
                                  etf_base_alloc = 0.05,
                                  date_merge_method = "union") {
    # Parse ETF symbols
    sma_syms <- parse_symbols(etf_sma_symbols)
    buyhold_syms <- parse_symbols(etf_buyhold_symbols)

    # Determine if we have strategies
    has_strategies <- length(strategy_files) > 0
    has_etfs <- !is.null(sma_syms) || !is.null(buyhold_syms)

    # Must have at least one of strategies or ETFs
    if (!has_strategies && !has_etfs) {
        stop("Must provide either strategy files or ETF symbols")
    }

    # ETF-only mode
    if (!has_strategies && has_etfs) {
        # Determine date range - use a reasonable default or fetch from first ETF
        # For simplicity, use last 5 years
        end_date <- Sys.Date()
        start_date <- end_date - (5 * 365)

        # Fetch and build ETF returns
        etf_result <- build_etf_returns(
            sma_symbols = sma_syms,
            buyhold_symbols = buyhold_syms,
            start_date = start_date,
            end_date = end_date
        )

        if (is.null(etf_result)) {
            stop("Failed to fetch ETF data")
        }

        etf_returns_xts <- etf_result$etf_returns_xts
        etf_names <- etf_result$etf_names

        # Convert to matrix
        etf_unit_R <- as.matrix(etf_returns_xts)
        rownames(etf_unit_R) <- NULL

        # Scale ETF returns to unit returns
        etf_unit_R <- etf_unit_R / etf_base_alloc

        return(list(
            unit_R = etf_unit_R,
            dates = zoo::index(etf_returns_xts),
            R_base = etf_unit_R * NA
        ))
    }

    # Strategy-only or mixed mode
    # Build strategy returns
    strategy_result <- build_unit_R(baseDir, strategy_files, strategy_base_alloc, date_merge_method)
    strategy_unit_R <- strategy_result$unit_R
    strategy_dates <- strategy_result$dates

    # If no ETFs, return strategy results only
    if (!has_etfs) {
        return(strategy_result)
    }

    # Mixed mode: strategies + ETFs
    # Determine date range from strategies
    start_date <- min(strategy_dates)
    end_date <- max(strategy_dates)

    # Fetch and build ETF returns
    etf_result <- build_etf_returns(
        sma_symbols = sma_syms,
        buyhold_symbols = buyhold_syms,
        start_date = start_date,
        end_date = end_date
    )

    # If ETF fetch failed, return strategy results only
    if (is.null(etf_result)) {
        warning("No ETF data could be fetched. Proceeding with strategies only.")
        return(strategy_result)
    }

    etf_returns_xts <- etf_result$etf_returns_xts
    etf_names <- etf_result$etf_names

    # Convert strategy unit_R back to xts for merging
    strategy_unit_R_xts <- xts(strategy_unit_R, order.by = strategy_dates)

    # Merge strategy and ETF returns
    if (date_merge_method == "intersection") {
        combined_xts <- merge(strategy_unit_R_xts, etf_returns_xts, all = FALSE)
    } else {
        combined_xts <- merge(strategy_unit_R_xts, etf_returns_xts, all = TRUE)
        combined_xts[is.na(combined_xts)] <- 0
    }

    # Convert back to matrix
    combined_unit_R <- as.matrix(combined_xts)
    rownames(combined_unit_R) <- NULL

    # ETFs are already in return format (not scaled by allocation)
    # So we need to convert them to unit returns by dividing by etf_base_alloc
    n_strategies <- ncol(strategy_unit_R)
    n_etfs <- length(etf_names)

    if (n_etfs > 0) {
        # Scale ETF returns to unit returns
        etf_cols <- (n_strategies + 1):(n_strategies + n_etfs)
        combined_unit_R[, etf_cols] <- combined_unit_R[, etf_cols] / etf_base_alloc
    }

    list(
        unit_R = combined_unit_R,
        dates = zoo::index(combined_xts),
        R_base = combined_unit_R * NA # Not used in optimization
    )
}


precompute_indices <- function(n_hist, n_paths, n_days_sim, seed = NULL) {
    if (!is.null(seed)) set.seed(seed)
    matrix(
        sample(seq_len(n_hist), size = n_paths * n_days_sim, replace = TRUE),
        nrow = n_days_sim,
        ncol = n_paths
    )
}

precompute_sampled_unit_R <- function(unit_R, idx_mat) {
    n_days_sim <- nrow(idx_mat)
    n_paths <- ncol(idx_mat)
    n_strat <- ncol(unit_R)

    sampled_list <- vector("list", n_strat)
    names(sampled_list) <- colnames(unit_R)

    for (s in seq_len(n_strat)) {
        r_s <- unit_R[, s]
        sampled_list[[s]] <- matrix(r_s[idx_mat], nrow = n_days_sim, ncol = n_paths)
    }

    sampled_list
}

run_portfolio_mc_from_sampled <- function(sampled_list,
                                          weights,
                                          trading_days_per_year = 252) {
    n_strat <- length(sampled_list)
    n_days_sim <- nrow(sampled_list[[1]])
    n_paths <- ncol(sampled_list[[1]])

    stopifnot(length(weights) == n_strat)

    # 1) Build portfolio returns
    port_ret <- matrix(0, nrow = n_days_sim, ncol = n_paths)
    for (s in seq_len(n_strat)) {
        port_ret <- port_ret + weights[s] * sampled_list[[s]]
    }

    # 2) Equity curves and drawdowns
    equity <- apply(1 + port_ret, 2, cumprod)
    running_max <- apply(equity, 2, cummax)
    dd <- equity / running_max - 1
    max_dd <- apply(dd, 2, min)

    # 3) CAGRs
    end_equity <- equity[n_days_sim, ]
    years <- n_days_sim / trading_days_per_year
    cagrs <- end_equity^(1 / years) - 1

    # 4) Quantiles
    q25_cagr <- quantile(cagrs, 0.25)
    q95_maxdd <- quantile(max_dd, 0.05)

    # 5) Drawdown depth at multiple confidence levels
    dd_quantiles <- quantile(max_dd, c(0.50, 0.75, 0.90, 0.95, 0.99))

    # 6) Drawdown duration statistics
    # For each path, find all drawdown periods and their durations
    all_dd_durations <- numeric()

    for (path in 1:n_paths) {
        dd_path <- dd[, path]
        in_dd <- dd_path < 0

        if (any(in_dd)) {
            dd_starts <- which(diff(c(FALSE, in_dd)) == 1)
            dd_ends <- which(diff(c(in_dd, FALSE)) == -1)

            durations <- dd_ends - dd_starts + 1
            all_dd_durations <- c(all_dd_durations, durations)
        }
    }

    # Calculate duration statistics
    if (length(all_dd_durations) > 0) {
        avg_dd_duration <- mean(all_dd_durations)
        median_dd_duration <- median(all_dd_durations)
        max_dd_duration <- max(all_dd_durations)
        dd_duration_quantiles <- quantile(all_dd_durations, c(0.50, 0.75, 0.90, 0.95, 0.99))
    } else {
        avg_dd_duration <- NA
        median_dd_duration <- NA
        max_dd_duration <- NA
        dd_duration_quantiles <- rep(NA, 5)
    }

    list(
        weights = weights,
        q25_CAGR = q25_cagr,
        q95_MaxDD = q95_maxdd,
        cagrs_all = cagrs,
        maxdd_all = max_dd,
        dd_depth_quantiles = dd_quantiles,
        avg_dd_duration = avg_dd_duration,
        median_dd_duration = median_dd_duration,
        max_dd_duration = max_dd_duration,
        dd_duration_quantiles = dd_duration_quantiles
    )
}

run_portfolio_mc <- function(unit_R,
                             weights,
                             idx_mat,
                             trading_days_per_year = 252) {
    stopifnot(length(weights) == ncol(unit_R))

    n_days_sim <- nrow(idx_mat)
    n_paths <- ncol(idx_mat)
    n_strat <- ncol(unit_R)

    # 1) Build matrix of portfolio returns for ALL paths at once
    port_ret <- matrix(0, nrow = n_days_sim, ncol = n_paths)

    for (s in seq_len(n_strat)) {
        r_s <- unit_R[, s]
        R_s_sampled <- matrix(r_s[idx_mat], nrow = n_days_sim, ncol = n_paths)
        port_ret <- port_ret + weights[s] * R_s_sampled
    }

    # 2) Equity curves
    equity <- apply(1 + port_ret, 2, cumprod)

    # 3) Running max and drawdowns
    running_max <- apply(equity, 2, cummax)
    dd <- equity / running_max - 1
    max_dd <- apply(dd, 2, min)

    # 4) CAGR per path
    end_equity <- equity[n_days_sim, ]
    years <- n_days_sim / trading_days_per_year
    cagrs <- end_equity^(1 / years) - 1

    # 5) Quantiles
    q25_cagr <- quantile(cagrs, 0.25)
    q95_maxdd <- quantile(max_dd, 0.05)

    # 6) Drawdown depth at multiple confidence levels
    dd_quantiles <- quantile(max_dd, c(0.50, 0.75, 0.90, 0.95, 0.99))

    # 7) Drawdown duration statistics
    all_dd_durations <- numeric()

    for (path in 1:n_paths) {
        dd_path <- dd[, path]
        in_dd <- dd_path < 0

        if (any(in_dd)) {
            dd_starts <- which(diff(c(FALSE, in_dd)) == 1)
            dd_ends <- which(diff(c(in_dd, FALSE)) == -1)

            durations <- dd_ends - dd_starts + 1
            all_dd_durations <- c(all_dd_durations, durations)
        }
    }

    # Calculate duration statistics
    if (length(all_dd_durations) > 0) {
        avg_dd_duration <- mean(all_dd_durations)
        median_dd_duration <- median(all_dd_durations)
        max_dd_duration <- max(all_dd_durations)
        dd_duration_quantiles <- quantile(all_dd_durations, c(0.50, 0.75, 0.90, 0.95, 0.99))
    } else {
        avg_dd_duration <- NA
        median_dd_duration <- NA
        max_dd_duration <- NA
        dd_duration_quantiles <- rep(NA, 5)
    }

    list(
        weights = weights,
        q25_CAGR = q25_cagr,
        q95_MaxDD = q95_maxdd,
        cagrs_all = cagrs,
        maxdd_all = max_dd,
        dd_depth_quantiles = dd_quantiles,
        avg_dd_duration = avg_dd_duration,
        median_dd_duration = median_dd_duration,
        max_dd_duration = max_dd_duration,
        dd_duration_quantiles = dd_duration_quantiles
    )
}

portfolio_objective_pso <- function(x,
                                    sampled_unit_R_list,
                                    max_alloc,
                                    total_max,
                                    dd_limit = 0.10,
                                    penalty_lambda = 1000) {
    n_strat <- length(max_alloc)
    stopifnot(length(x) == n_strat)

    # x in [0,1] -> weights in [0, max_alloc]
    x_clipped <- pmax(pmin(x, 1), 0)
    w <- x_clipped * max_alloc

    # enforce total allocation cap
    if (sum(w) > total_max && sum(w) > 0) {
        w <- w * (total_max / sum(w))
    }

    mc_res <- run_portfolio_mc_from_sampled(sampled_unit_R_list, w)

    q25 <- as.numeric(mc_res$q25_CAGR)
    q95dd <- as.numeric(mc_res$q95_MaxDD)

    # violation: want 95% MaxDD >= -dd_limit
    viol <- (-dd_limit - q95dd)
    viol <- max(0, viol)

    penalty <- penalty_lambda * (viol^2)

    # minimize: -q25 + penalty
    -q25 + penalty
}

optimize_portfolio <- function(baseDir,
                               strategy_files,
                               base_alloc,
                               max_alloc,
                               total_max = 0.40,
                               dd_limit = 0.10,
                               n_days_sim_opt = 1000,
                               n_paths_opt = 4000,
                               n_paths_final = 20000,
                               swarm_size = 40,
                               maxit = 80,
                               seed = 123,
                               date_merge_method = "union",
                               etf_sma_symbols = NULL,
                               etf_buyhold_symbols = NULL,
                               etf_base_alloc = 0.05,
                               etf_max_alloc = 0.15,
                               verbose = TRUE,
                               progress_callback = NULL) {
    if (verbose) cat("Building unit_R with strategies and ETFs...\n")
    if (!is.null(progress_callback)) progress_callback("Building unit_R...")

    prep <- build_combined_unit_R(
        baseDir = baseDir,
        strategy_files = strategy_files,
        strategy_base_alloc = base_alloc,
        etf_sma_symbols = etf_sma_symbols,
        etf_buyhold_symbols = etf_buyhold_symbols,
        etf_base_alloc = etf_base_alloc,
        date_merge_method = date_merge_method
    )
    unit_R <- prep$unit_R

    n_hist <- nrow(unit_R)
    n_strat <- ncol(unit_R)

    # Determine how many strategies vs ETFs we have
    n_strategies <- length(base_alloc)
    n_etfs <- n_strat - n_strategies

    # Build combined max_alloc vector (strategies + ETFs)
    if (n_etfs > 0) {
        combined_max_alloc <- c(max_alloc, rep(etf_max_alloc, n_etfs))
    } else {
        combined_max_alloc <- max_alloc
    }

    stopifnot(length(combined_max_alloc) == n_strat)

    if (verbose) cat("Precomputing indices and sampled returns for optimization...\n")
    if (!is.null(progress_callback)) progress_callback("Precomputing indices...")

    idx_mat_opt <- precompute_indices(
        n_hist     = n_hist,
        n_paths    = n_paths_opt,
        n_days_sim = n_days_sim_opt,
        seed       = seed
    )
    sampled_opt <- precompute_sampled_unit_R(unit_R, idx_mat_opt)

    lower <- rep(0, n_strat)
    upper <- rep(1, n_strat)
    start_par <- rep(0.5, n_strat)

    if (verbose) cat("Starting PSO optimization...\n")
    if (!is.null(progress_callback)) progress_callback("Running PSO optimization...")

    set.seed(seed)
    opt <- psoptim(
        par = start_par,
        fn = portfolio_objective_pso,
        lower = lower,
        upper = upper,
        control = list(
            maxit = maxit,
            s     = swarm_size,
            trace = 0
        ),
        sampled_unit_R_list = sampled_opt,
        max_alloc = combined_max_alloc,
        total_max = total_max,
        dd_limit = dd_limit
    )

    if (verbose) cat("PSO complete. Extracting best weights...\n")
    if (!is.null(progress_callback)) progress_callback("Extracting best weights...")

    x_best <- pmax(pmin(opt$par, 1), 0)
    w_best <- x_best * combined_max_alloc
    if (sum(w_best) > total_max && sum(w_best) > 0) {
        w_best <- w_best * (total_max / sum(w_best))
    }

    if (verbose) cat("Running final high-precision Monte Carlo...\n")
    if (!is.null(progress_callback)) progress_callback("Running final Monte Carlo...")

    idx_mat_final <- precompute_indices(
        n_hist     = n_hist,
        n_paths    = n_paths_final,
        n_days_sim = n_days_sim_opt,
        seed       = seed + 1L
    )
    final_res <- run_portfolio_mc(
        unit_R = unit_R,
        weights = w_best,
        idx_mat = idx_mat_final
    )

    if (verbose) cat("Done.\n")
    if (!is.null(progress_callback)) progress_callback("Complete!")

    list(
        weights_optimal = w_best,
        q25_CAGR        = final_res$q25_CAGR,
        q95_MaxDD       = final_res$q95_MaxDD,
        cagrs_all       = final_res$cagrs_all,
        maxdd_all       = final_res$maxdd_all,
        psoptim_raw     = opt
    )
}

##### SINGLE STRATEGY ANALYSIS ###############################################

analyze_single_strategy <- function(baseDir,
                                    strategy_file,
                                    strategy_name,
                                    base_alloc,
                                    alloc_range = seq(0.01, 0.30, by = 0.01),
                                    dd_limit = 0.10,
                                    n_days_sim = 1000,
                                    n_paths = 10000,
                                    seed = 123,
                                    date_merge_method = "union",
                                    progress_callback = NULL) {
    if (!is.null(progress_callback)) progress_callback("Loading strategy data...")

    # Build unit_R for single strategy (date_merge_method not used for single strategy)
    prep <- build_unit_R(baseDir, strategy_file, base_alloc, date_merge_method)
    unit_R <- prep$unit_R

    n_hist <- nrow(unit_R)

    if (!is.null(progress_callback)) progress_callback("Precomputing indices...")

    idx_mat <- precompute_indices(n_hist, n_paths, n_days_sim, seed)

    results <- data.frame(
        allocation = numeric(),
        q25_CAGR = numeric(),
        q95_MaxDD = numeric(),
        meets_dd_constraint = logical()
    )

    total_steps <- length(alloc_range)

    for (i in seq_along(alloc_range)) {
        alloc <- alloc_range[i]

        if (!is.null(progress_callback)) {
            progress_callback(sprintf(
                "Testing allocation %.1f%% (%d/%d)",
                alloc * 100, i, total_steps
            ))
        }

        mc_res <- run_portfolio_mc(
            unit_R = unit_R,
            weights = alloc,
            idx_mat = idx_mat
        )

        meets_constraint <- mc_res$q95_MaxDD >= -dd_limit

        results <- rbind(results, data.frame(
            allocation = alloc,
            q25_CAGR = mc_res$q25_CAGR,
            q95_MaxDD = mc_res$q95_MaxDD,
            meets_dd_constraint = meets_constraint
        ))
    }

    # Find optimal allocation (max Q25 CAGR among those meeting DD constraint)
    valid_results <- results[results$meets_dd_constraint, ]

    if (nrow(valid_results) > 0) {
        optimal_idx <- which.max(valid_results$q25_CAGR)
        optimal_alloc <- valid_results$allocation[optimal_idx]
        optimal_cagr <- valid_results$q25_CAGR[optimal_idx]
        optimal_dd <- valid_results$q95_MaxDD[optimal_idx]
    } else {
        optimal_alloc <- NA
        optimal_cagr <- NA
        optimal_dd <- NA
    }

    if (!is.null(progress_callback)) progress_callback("Complete!")

    # Run final MC at optimal allocation to get detailed drawdown stats
    optimal_dd_stats <- NULL
    if (!is.na(optimal_alloc)) {
        final_mc <- run_portfolio_mc(
            unit_R = unit_R,
            weights = optimal_alloc,
            idx_mat = idx_mat
        )
        optimal_dd_stats <- list(
            dd_depth_quantiles = final_mc$dd_depth_quantiles,
            avg_dd_duration = final_mc$avg_dd_duration,
            median_dd_duration = final_mc$median_dd_duration,
            max_dd_duration = final_mc$max_dd_duration,
            dd_duration_quantiles = final_mc$dd_duration_quantiles
        )
    }

    list(
        results = results,
        optimal_allocation = optimal_alloc,
        optimal_q25_CAGR = optimal_cagr,
        optimal_q95_MaxDD = optimal_dd,
        optimal_dd_stats = optimal_dd_stats
    )
}

##### ROBUSTNESS TESTING FUNCTIONS ##########################################

# 1. Time Period Analysis
analyze_time_periods <- function(returns, dates, n_periods = 4) {
    n <- length(returns)
    period_size <- floor(n / n_periods)

    results <- data.frame(
        period = integer(),
        start_date = character(),
        end_date = character(),
        n_days = integer(),
        cagr = numeric(),
        sharpe = numeric(),
        max_dd = numeric(),
        win_rate = numeric(),
        stringsAsFactors = FALSE
    )

    for (i in 1:n_periods) {
        start_idx <- (i - 1) * period_size + 1
        end_idx <- if (i == n_periods) n else i * period_size

        period_returns <- returns[start_idx:end_idx]
        period_dates <- dates[start_idx:end_idx]

        # Calculate metrics
        equity <- cumprod(1 + period_returns)
        years <- length(period_returns) / 252
        cagr <- (equity[length(equity)]^(1 / years)) - 1

        sharpe <- if (sd(period_returns) > 0) {
            mean(period_returns) / sd(period_returns) * sqrt(252)
        } else {
            NA
        }

        running_max <- cummax(equity)
        dd <- equity / running_max - 1
        max_dd <- min(dd)

        win_rate <- sum(period_returns > 0) / length(period_returns)

        results <- rbind(results, data.frame(
            period = i,
            start_date = as.character(period_dates[1]),
            end_date = as.character(period_dates[length(period_dates)]),
            n_days = length(period_returns),
            cagr = cagr,
            sharpe = sharpe,
            max_dd = max_dd,
            win_rate = win_rate,
            stringsAsFactors = FALSE
        ))
    }

    # Calculate coefficient of variation for each metric
    cv_cagr <- sd(results$cagr) / abs(mean(results$cagr))
    cv_sharpe <- sd(results$sharpe, na.rm = TRUE) / abs(mean(results$sharpe, na.rm = TRUE))

    list(
        period_stats = results,
        cv_cagr = cv_cagr,
        cv_sharpe = cv_sharpe
    )
}

# 2. Walk-Forward Analysis
walk_forward_test <- function(returns, dates, train_pct = 0.7, n_folds = 5) {
    n <- length(returns)
    results <- data.frame(
        fold = integer(),
        train_cagr = numeric(),
        test_cagr = numeric(),
        degradation = numeric(),
        stringsAsFactors = FALSE
    )

    fold_size <- floor(n / n_folds)

    for (i in 1:n_folds) {
        fold_end <- min(i * fold_size, n)
        fold_start <- max(1, fold_end - fold_size + 1)

        fold_returns <- returns[fold_start:fold_end]
        train_size <- floor(length(fold_returns) * train_pct)

        train_returns <- fold_returns[1:train_size]
        test_returns <- fold_returns[(train_size + 1):length(fold_returns)]

        if (length(test_returns) < 20) next

        # Train metrics
        train_equity <- cumprod(1 + train_returns)
        train_years <- length(train_returns) / 252
        train_cagr <- (train_equity[length(train_equity)]^(1 / train_years)) - 1

        # Test metrics
        test_equity <- cumprod(1 + test_returns)
        test_years <- length(test_returns) / 252
        test_cagr <- (test_equity[length(test_equity)]^(1 / test_years)) - 1

        degradation <- (test_cagr - train_cagr) / abs(train_cagr) * 100

        results <- rbind(results, data.frame(
            fold = i,
            train_cagr = train_cagr,
            test_cagr = test_cagr,
            degradation = degradation,
            stringsAsFactors = FALSE
        ))
    }

    avg_degradation <- mean(results$degradation)

    list(
        fold_results = results,
        avg_degradation = avg_degradation,
        oos_ratio = mean(results$test_cagr) / mean(results$train_cagr)
    )
}

# 3. Rolling Metrics
calculate_rolling_metrics <- function(returns, dates, window = 252) {
    n <- length(returns)
    if (n < window) {
        return(NULL)
    }

    results <- data.frame(
        date = as.Date(character()),
        rolling_sharpe = numeric(),
        rolling_cagr = numeric(),
        rolling_maxdd = numeric(),
        stringsAsFactors = FALSE
    )

    for (i in window:n) {
        window_returns <- returns[(i - window + 1):i]
        window_date <- dates[i]

        # Sharpe
        sharpe <- if (sd(window_returns) > 0) {
            mean(window_returns) / sd(window_returns) * sqrt(252)
        } else {
            NA
        }

        # CAGR
        equity <- cumprod(1 + window_returns)
        years <- window / 252
        cagr <- (equity[length(equity)]^(1 / years)) - 1

        # MaxDD
        running_max <- cummax(equity)
        dd <- equity / running_max - 1
        maxdd <- min(dd)

        results <- rbind(results, data.frame(
            date = window_date,
            rolling_sharpe = sharpe,
            rolling_cagr = cagr,
            rolling_maxdd = maxdd,
            stringsAsFactors = FALSE
        ))
    }

    results
}

# 4. Drawdown Analysis
analyze_drawdowns <- function(returns, dates) {
    equity <- cumprod(1 + returns)
    running_max <- cummax(equity)
    dd <- equity / running_max - 1

    # Identify drawdown periods
    in_dd <- dd < 0
    dd_starts <- which(diff(c(FALSE, in_dd)) == 1)
    dd_ends <- which(diff(c(in_dd, FALSE)) == -1)

    if (length(dd_starts) == 0) {
        return(list(
            n_drawdowns = 0,
            avg_duration = NA,
            avg_depth = NA,
            drawdown_details = data.frame()
        ))
    }

    dd_details <- data.frame(
        start_idx = dd_starts,
        end_idx = dd_ends,
        duration = dd_ends - dd_starts + 1,
        depth = sapply(1:length(dd_starts), function(i) {
            min(dd[dd_starts[i]:dd_ends[i]])
        }),
        stringsAsFactors = FALSE
    )

    list(
        n_drawdowns = nrow(dd_details),
        avg_duration = mean(dd_details$duration),
        avg_depth = mean(dd_details$depth),
        max_depth = min(dd_details$depth),
        drawdown_details = dd_details
    )
}

# 5. Statistical Tests
run_statistical_tests <- function(returns) {
    # Normality tests
    ks_test <- tryCatch(
        ks.test(returns, "pnorm", mean(returns), sd(returns)),
        error = function(e) list(p.value = NA)
    )

    shapiro_test <- tryCatch(
        if (length(returns) <= 5000) shapiro.test(returns) else list(p.value = NA),
        error = function(e) list(p.value = NA)
    )

    # Autocorrelation
    acf_result <- tryCatch(
        acf(returns, lag.max = 20, plot = FALSE),
        error = function(e) NULL
    )

    # Check for significant autocorrelation
    sig_autocorr <- if (!is.null(acf_result)) {
        sum(abs(acf_result$acf[-1]) > 1.96 / sqrt(length(returns)))
    } else {
        NA
    }

    list(
        ks_pvalue = ks_test$p.value,
        shapiro_pvalue = shapiro_test$p.value,
        sig_autocorr_lags = sig_autocorr,
        acf_data = acf_result
    )
}

# 6. Bootstrap Confidence Intervals
bootstrap_metrics <- function(returns, n_bootstrap = 1000) {
    n <- length(returns)

    cagr_boot <- numeric(n_bootstrap)
    sharpe_boot <- numeric(n_bootstrap)
    maxdd_boot <- numeric(n_bootstrap)

    for (i in 1:n_bootstrap) {
        boot_returns <- sample(returns, n, replace = TRUE)

        # CAGR
        equity <- cumprod(1 + boot_returns)
        years <- n / 252
        cagr_boot[i] <- (equity[length(equity)]^(1 / years)) - 1

        # Sharpe
        sharpe_boot[i] <- if (sd(boot_returns) > 0) {
            mean(boot_returns) / sd(boot_returns) * sqrt(252)
        } else {
            NA
        }

        # MaxDD
        running_max <- cummax(equity)
        dd <- equity / running_max - 1
        maxdd_boot[i] <- min(dd)
    }

    list(
        cagr_ci = quantile(cagr_boot, c(0.025, 0.975)),
        sharpe_ci = quantile(sharpe_boot, c(0.025, 0.975), na.rm = TRUE),
        maxdd_ci = quantile(maxdd_boot, c(0.025, 0.975)),
        cagr_dist = cagr_boot,
        sharpe_dist = sharpe_boot,
        maxdd_dist = maxdd_boot
    )
}

# 7. Overall Robustness Score
calculate_robustness_score <- function(time_analysis, wf_analysis, dd_analysis, stat_tests) {
    score <- 0
    max_score <- 6
    flags <- character()

    # 1. Time period consistency (CV of CAGR)
    if (!is.na(time_analysis$cv_cagr)) {
        if (time_analysis$cv_cagr < 0.5) {
            score <- score + 1
        } else if (time_analysis$cv_cagr > 1.0) {
            flags <- c(flags, "High variance across time periods")
        }
    }

    # 2. Walk-forward performance
    if (!is.na(wf_analysis$oos_ratio)) {
        if (wf_analysis$oos_ratio > 0.7) {
            score <- score + 1
        } else if (wf_analysis$oos_ratio < 0.5) {
            flags <- c(flags, "Poor out-of-sample performance")
        }
    }

    # 3. Drawdown count (should have multiple)
    if (dd_analysis$n_drawdowns >= 5) {
        score <- score + 1
    } else if (dd_analysis$n_drawdowns < 3) {
        flags <- c(flags, "Too few drawdowns (suspiciously good)")
    }

    # 4. Normality (shouldn't be too normal)
    if (!is.na(stat_tests$ks_pvalue)) {
        if (stat_tests$ks_pvalue < 0.05) {
            score <- score + 1 # Good - not too normal
        }
    }

    # 5. Autocorrelation (should be minimal)
    if (!is.na(stat_tests$sig_autocorr_lags)) {
        if (stat_tests$sig_autocorr_lags < 3) {
            score <- score + 1
        } else {
            flags <- c(flags, "Significant autocorrelation detected")
        }
    }

    # 6. Sharpe consistency
    if (!is.na(time_analysis$cv_sharpe)) {
        if (time_analysis$cv_sharpe < 0.6) {
            score <- score + 1
        }
    }

    # Determine verdict
    score_pct <- score / max_score
    verdict <- if (score_pct >= 0.7) {
        "ROBUST"
    } else if (score_pct >= 0.4) {
        "CAUTION"
    } else {
        "OVERFITTED"
    }

    list(
        score = score,
        max_score = max_score,
        score_pct = score_pct,
        verdict = verdict,
        flags = flags
    )
}

##### SHINY UI ###############################################################

ui <- fluidPage(
    useShinyjs(),
    titlePanel("Monte Carlo Portfolio Optimization"),
    tabsetPanel(
        id = "main_tabs",

        # TAB 1: Database Management
        tabPanel(
            "Strategy Database",
            fluidRow(
                column(
                    12,
                    h3("Strategy Database Management"),
                    p("Manage your strategy files and allocation parameters.")
                )
            ),
            fluidRow(
                column(
                    12,
                    textInput("baseDir", "Base Directory for Strategy Files:",
                        value = "/Users/amod/R-Trading/backtests/"
                    ),
                    actionButton("refreshDB", "Refresh Database", icon = icon("refresh")),
                    actionButton("addStrategy", "Add Strategy", icon = icon("plus")),
                    actionButton("deleteStrategy", "Delete Selected", icon = icon("trash")),
                    hr()
                )
            ),
            fluidRow(
                column(
                    12,
                    DTOutput("strategyTable")
                )
            )
        ),

        # TAB 2: Portfolio Optimization
        tabPanel(
            "Portfolio Optimization",
            fluidRow(
                column(
                    12,
                    h3("Multi-Strategy Portfolio Optimization"),
                    p("Select strategies and optimize allocation to maximize Q25 CAGR while constraining drawdown.")
                )
            ),
            fluidRow(
                column(
                    4,
                    wellPanel(
                        h4("Strategy Selection"),
                        uiOutput("strategyCheckboxes"),
                        hr(),
                        h4("Optimization Parameters"),
                        numericInput("total_max", "Total Max Allocation:",
                            value = 0.40, min = 0.01, max = 1.0, step = 0.01
                        ),
                        numericInput("dd_limit", "Drawdown Limit (95% confidence):",
                            value = 0.10, min = 0.01, max = 0.50, step = 0.01
                        ),
                        numericInput("n_days_sim", "Days to Simulate:",
                            value = 1000, min = 100, max = 5000, step = 100
                        ),
                        numericInput("n_paths_opt", "Paths (Optimization):",
                            value = 1000, min = 500, max = 10000, step = 500
                        ),
                        numericInput("n_paths_final", "Paths (Final):",
                            value = 5000, min = 1000, max = 50000, step = 1000
                        ),
                        numericInput("swarm_size", "PSO Swarm Size:",
                            value = 20, min = 10, max = 100, step = 5
                        ),
                        numericInput("maxit", "PSO Max Iterations:",
                            value = 40, min = 10, max = 200, step = 10
                        ),
                        numericInput("seed", "Random Seed:",
                            value = 123, min = 1, max = 10000, step = 1
                        ),
                        hr(),
                        h4("ETF Integration"),
                        textInput("etf_sma_symbols", "ETF Symbols (SMA 150 Conditional):",
                            value = "", placeholder = "e.g., QQQ,SPY"
                        ),
                        helpText("Enter comma-separated symbols. These ETFs will only be held when price is above 150-day SMA."),
                        textInput("etf_buyhold_symbols", "ETF Symbols (Buy and Hold):",
                            value = "", placeholder = "e.g., GLD,TLT"
                        ),
                        helpText("Enter comma-separated symbols. These ETFs will be held continuously."),
                        numericInput("etf_base_alloc", "ETF Base Allocation (each):",
                            value = 0.05, min = 0.01, max = 1.0, step = 0.01
                        ),
                        numericInput("etf_max_alloc", "ETF Max Allocation (each):",
                            value = 0.15, min = 0.01, max = 1.0, step = 0.01
                        ),
                        hr(),
                        h4("Date Handling"),
                        radioButtons(
                            "date_merge_method",
                            "Date Merge Method:",
                            choices = list(
                                "Union (all dates, fill missing with 0)" = "union",
                                "Intersection (only overlapping dates)" = "intersection"
                            ),
                            selected = "union"
                        ),
                        helpText("Union uses all dates from all strategies. Intersection uses only dates where all strategies have data."),
                        hr(),
                        actionButton("runOptimization", "Run Optimization",
                            icon = icon("play"), class = "btn-primary btn-lg btn-block"
                        )
                    )
                ),
                column(
                    8,
                    wellPanel(
                        h4("Optimization Progress"),
                        textOutput("optimizationStatus"),
                        hr(),
                        h4("Results"),
                        verbatimTextOutput("optimizationResults"),
                        hr(),
                        h4("CAGR Distribution"),
                        plotlyOutput("cagrPlot", height = "300px"),
                        hr(),
                        h4("Max Drawdown Distribution"),
                        plotlyOutput("ddPlot", height = "300px")
                    )
                )
            )
        ),

        # TAB 3: Single Strategy Analysis
        tabPanel(
            "Single Strategy Analysis",
            fluidRow(
                column(
                    12,
                    h3("Single Strategy Allocation Analysis"),
                    p("Analyze optimal allocation for a single strategy.")
                )
            ),
            fluidRow(
                column(
                    4,
                    wellPanel(
                        h4("Strategy Selection"),
                        uiOutput("singleStrategySelect"),
                        hr(),
                        h4("Analysis Parameters"),
                        sliderInput("alloc_min", "Min Allocation:",
                            min = 0.01, max = 0.50, value = 0.01, step = 0.01
                        ),
                        sliderInput("alloc_max", "Max Allocation:",
                            min = 0.01, max = 1.0, value = 0.30, step = 0.01
                        ),
                        sliderInput("alloc_step", "Allocation Step:",
                            min = 0.001, max = 0.05, value = 0.01, step = 0.001
                        ),
                        numericInput("single_dd_limit", "Drawdown Limit:",
                            value = 0.10, min = 0.01, max = 0.50, step = 0.01
                        ),
                        numericInput("single_n_days", "Days to Simulate:",
                            value = 1000, min = 100, max = 5000, step = 100
                        ),
                        numericInput("single_n_paths", "Monte Carlo Paths:",
                            value = 5000, min = 1000, max = 50000, step = 1000
                        ),
                        numericInput("single_seed", "Random Seed:",
                            value = 123, min = 1, max = 10000, step = 1
                        ),
                        hr(),
                        h4("Date Handling"),
                        radioButtons(
                            "single_date_merge_method",
                            "Date Merge Method:",
                            choices = list(
                                "Union (all dates)" = "union",
                                "Intersection (overlapping only)" = "intersection"
                            ),
                            selected = "union"
                        ),
                        helpText("Note: For single strategy, this only matters if comparing to other analyses."),
                        hr(),
                        actionButton("runSingleAnalysis", "Run Analysis",
                            icon = icon("play"), class = "btn-primary btn-lg btn-block"
                        )
                    )
                ),
                column(
                    8,
                    wellPanel(
                        h4("Analysis Progress"),
                        textOutput("singleAnalysisStatus"),
                        hr(),
                        h4("Optimal Allocation"),
                        verbatimTextOutput("singleAnalysisResults"),
                        hr(),
                        h4("CAGR vs Allocation"),
                        plotlyOutput("singleCAGRPlot", height = "300px"),
                        hr(),
                        h4("Max Drawdown vs Allocation"),
                        plotlyOutput("singleDDPlot", height = "300px")
                    )
                )
            )
        ),

        # TAB 4: Robustness Testing
        tabPanel(
            "Robustness Testing",
            fluidRow(
                column(
                    12,
                    h3("Strategy Robustness Testing"),
                    p("Test for curve-fitting and validate strategy stability across time periods.")
                )
            ),
            fluidRow(
                column(
                    4,
                    wellPanel(
                        h4("Strategy Selection"),
                        uiOutput("robustnessStrategySelect"),
                        hr(),
                        h4("Analysis Parameters"),
                        numericInput("n_time_periods", "Number of Time Periods:",
                            value = 4, min = 2, max = 10, step = 1
                        ),
                        numericInput("rolling_window", "Rolling Window (days):",
                            value = 252, min = 126, max = 504, step = 126
                        ),
                        sliderInput("train_test_split", "Train/Test Split:",
                            min = 0.6, max = 0.8, value = 0.7, step = 0.05
                        ),
                        numericInput("n_bootstrap", "Bootstrap Iterations:",
                            value = 1000, min = 500, max = 2000, step = 500
                        ),
                        hr(),
                        actionButton("runRobustnessTest", "Run Robustness Tests",
                            icon = icon("check-circle"), class = "btn-success btn-lg btn-block"
                        )
                    )
                ),
                column(
                    8,
                    wellPanel(
                        h4("Overall Assessment"),
                        uiOutput("robustnessVerdict"),
                        hr(),
                        verbatimTextOutput("robustnessSummary"),
                        hr(),
                        h4("Time Period Analysis"),
                        DTOutput("timePeriodTable"),
                        plotlyOutput("timePeriodPlot", height = "300px"),
                        hr(),
                        h4("Walk-Forward Test Results"),
                        DTOutput("walkForwardTable"),
                        plotlyOutput("walkForwardPlot", height = "300px"),
                        hr(),
                        h4("Rolling Metrics"),
                        plotlyOutput("rollingSharpe", height = "250px"),
                        plotlyOutput("rollingCAGR", height = "250px"),
                        hr(),
                        h4("Drawdown Analysis"),
                        verbatimTextOutput("drawdownStats"),
                        plotlyOutput("drawdownHist", height = "300px"),
                        hr(),
                        h4("Statistical Tests"),
                        verbatimTextOutput("statTestResults"),
                        hr(),
                        h4("Bootstrap Confidence Intervals"),
                        verbatimTextOutput("bootstrapResults"),
                        plotlyOutput("bootstrapPlot", height = "300px")
                    )
                )
            )
        )
    )
)

##### SHINY SERVER ###########################################################

server <- function(input, output, session) {
    # Reactive value to store database
    db <- reactiveVal(NULL)

    # Database file path
    db_file <- "strategy_database.csv"

    # Helper function to extract date range from strategy file
    get_strategy_date_range <- function(file_path, base_dir) {
        tryCatch(
            {
                full_path <- file.path(base_dir, file_path)
                if (!file.exists(full_path)) {
                    return(list(min_date = NA, max_date = NA, n_days = NA))
                }

                d <- read.csv(full_path, stringsAsFactors = FALSE)
                dates <- as.Date(d$Date)
                dates <- dates[!is.na(dates)]

                if (length(dates) == 0) {
                    return(list(min_date = NA, max_date = NA, n_days = NA))
                }

                list(
                    min_date = as.character(min(dates)),
                    max_date = as.character(max(dates)),
                    n_days = length(dates)
                )
            },
            error = function(e) {
                list(min_date = NA, max_date = NA, n_days = NA)
            }
        )
    }

    # Load database on startup
    observe({
        if (file.exists(db_file)) {
            db(read.csv(db_file, stringsAsFactors = FALSE))
        } else {
            # Create default database
            default_db <- data.frame(
                strategy_name = c("Delta20_0DTE", "Delta45_3PMIC", "NTAsymetricRIC", "Monday23DC"),
                file_path = c("20_Delta_0DTE.csv", "Delta45_3PMIC.csv", "NTAsymetricRIC.csv", "Monday23DC.csv"),
                base_alloc = c(0.04, 0.04, 0.05, 0.10),
                max_alloc = c(0.10, 0.10, 0.10, 0.20),
                stringsAsFactors = FALSE
            )
            write.csv(default_db, db_file, row.names = FALSE)
            db(default_db)
        }
    })

    # Refresh database
    observeEvent(input$refreshDB, {
        if (file.exists(db_file)) {
            db(read.csv(db_file, stringsAsFactors = FALSE))
            showNotification("Database refreshed", type = "message")
        }
    })

    # Reactive expression to enrich database with date ranges
    db_with_dates <- reactive({
        req(db())
        req(input$baseDir)

        current_db <- db()

        # Extract date ranges for each strategy
        date_info <- lapply(seq_len(nrow(current_db)), function(i) {
            get_strategy_date_range(current_db$file_path[i], input$baseDir)
        })

        # Add date columns to database
        current_db$start_date <- sapply(date_info, function(x) x$min_date)
        current_db$end_date <- sapply(date_info, function(x) x$max_date)
        current_db$days_count <- sapply(date_info, function(x) x$n_days)

        current_db
    })

    # Display strategy table with date ranges
    output$strategyTable <- renderDT({
        req(db_with_dates())

        display_db <- db_with_dates()

        datatable(
            display_db,
            selection = "single",
            editable = list(
                target = "cell",
                disable = list(columns = c(0, 5, 6, 7)) # Disable editing of row number and date columns
            ),
            options = list(
                pageLength = 10,
                columnDefs = list(
                    list(className = "dt-center", targets = c(5, 6, 7))
                )
            )
        ) %>%
            formatStyle(
                "days_count",
                backgroundColor = styleInterval(
                    c(100, 250),
                    c("#ffcccc", "#ffffcc", "#ccffcc")
                )
            )
    })

    # Handle cell edits (only for editable columns)
    observeEvent(input$strategyTable_cell_edit, {
        info <- input$strategyTable_cell_edit

        # Only allow editing of the first 4 columns (strategy_name, file_path, base_alloc, max_alloc)
        if (info$col <= 4) {
            current_db <- db()
            current_db[info$row, info$col] <- info$value
            db(current_db)
            write.csv(current_db, db_file, row.names = FALSE)
            showNotification("Database updated", type = "message")
        }
    })

    # Add strategy
    observeEvent(input$addStrategy, {
        showModal(modalDialog(
            title = "Add New Strategy",
            fluidRow(
                column(
                    12,
                    shinyFilesButton(
                        "file_browser",
                        "Browse for Strategy File",
                        "Please select a strategy CSV file",
                        multiple = FALSE,
                        icon = icon("folder-open"),
                        class = "btn-info btn-block"
                    ),
                    hr()
                )
            ),
            textInput("new_strategy_name", "Strategy Name:"),
            textInput("new_file_path", "File Path (relative to base dir):", placeholder = "e.g., strategy.csv"),
            textInput("new_base_dir_display", "Base Directory:", value = input$baseDir, placeholder = "Auto-detected from file path"),
            helpText("Tip: Use the Browse button above to automatically fill in these fields."),
            hr(),
            numericInput("new_base_alloc", "Base Allocation:", value = 0.05, min = 0.01, max = 1.0, step = 0.01),
            numericInput("new_max_alloc", "Max Allocation:", value = 0.10, min = 0.01, max = 1.0, step = 0.01),
            footer = tagList(
                modalButton("Cancel"),
                actionButton("confirmAdd", "Add", class = "btn-primary")
            )
        ))
    })


    # Set up file browser (allow browsing entire filesystem)
    shinyFileChoose(input, "file_browser", roots = c(home = "~", root = "/"), filetypes = c("", "csv"))

    # Handle file selection from browser
    observeEvent(input$file_browser, {
        if (!is.null(input$file_browser) && !is.integer(input$file_browser)) {
            file_selected <- parseFilePaths(c(home = "~", root = "/"), input$file_browser)

            if (nrow(file_selected) > 0) {
                full_path <- as.character(file_selected$datapath[1])

                # Get current base directory
                current_base_dir <- input$baseDir

                # Try to decompose the path
                if (startsWith(full_path, current_base_dir)) {
                    # File is within the base directory
                    relative_path <- sub(paste0("^", current_base_dir, "/*"), "", full_path)
                    base_dir_to_use <- current_base_dir
                } else {
                    # File is outside base directory - use file's directory as new base
                    base_dir_to_use <- dirname(full_path)
                    relative_path <- basename(full_path)
                }

                # Extract strategy name from filename (remove extension)
                file_name <- basename(full_path)
                strategy_name <- sub("\\.[^.]*$", "", file_name) # Remove extension

                # Update the modal inputs
                updateTextInput(session, "new_strategy_name", value = strategy_name)
                updateTextInput(session, "new_file_path", value = relative_path)
                updateTextInput(session, "new_base_dir_display", value = base_dir_to_use)

                # Update the main base directory if different
                if (base_dir_to_use != current_base_dir) {
                    updateTextInput(session, "baseDir", value = base_dir_to_use)
                    showNotification(
                        paste("Base directory updated to:", base_dir_to_use),
                        type = "message",
                        duration = 5
                    )
                }

                showNotification(
                    paste("File selected:", file_name),
                    type = "message"
                )
            }
        }
    })

    observeEvent(input$confirmAdd, {
        req(input$new_strategy_name, input$new_file_path)

        current_db <- db()
        new_row <- data.frame(
            strategy_name = input$new_strategy_name,
            file_path = input$new_file_path,
            base_alloc = input$new_base_alloc,
            max_alloc = input$new_max_alloc,
            stringsAsFactors = FALSE
        )

        current_db <- rbind(current_db, new_row)
        db(current_db)
        write.csv(current_db, db_file, row.names = FALSE)

        removeModal()
        showNotification("Strategy added", type = "message")
    })

    # Delete strategy
    observeEvent(input$deleteStrategy, {
        req(input$strategyTable_rows_selected)

        current_db <- db()
        current_db <- current_db[-input$strategyTable_rows_selected, ]
        db(current_db)
        write.csv(current_db, db_file, row.names = FALSE)

        showNotification("Strategy deleted", type = "message")
    })

    # Generate strategy checkboxes for portfolio optimization
    output$strategyCheckboxes <- renderUI({
        req(db())
        strategies <- db()$strategy_name
        checkboxGroupInput("selectedStrategies", "Select Strategies:",
            choices = strategies, selected = strategies
        )
    })

    # Generate strategy dropdown for single analysis
    output$singleStrategySelect <- renderUI({
        req(db())
        strategies <- db()$strategy_name
        selectInput("selectedSingleStrategy", "Select Strategy:",
            choices = strategies, selected = strategies[1]
        )
    })

    # Portfolio optimization
    optimization_result <- reactiveVal(NULL)

    output$optimizationStatus <- renderText({
        if (is.null(optimization_result())) {
            "Ready to run optimization"
        } else {
            "Optimization complete"
        }
    })

    observeEvent(input$runOptimization, {
        # Check if we have either strategies or ETFs
        has_strategies <- !is.null(input$selectedStrategies) && length(input$selectedStrategies) > 0
        has_etfs <- (!is.null(input$etf_sma_symbols) && input$etf_sma_symbols != "") ||
            (!is.null(input$etf_buyhold_symbols) && input$etf_buyhold_symbols != "")

        if (!has_strategies && !has_etfs) {
            showNotification("Please select at least one strategy or enter ETF symbols", type = "error")
            return()
        }

        current_db <- db()

        # Prepare strategy inputs (may be empty for ETF-only mode)
        if (has_strategies) {
            selected_db <- current_db[current_db$strategy_name %in% input$selectedStrategies, ]
            strategy_files <- selected_db$file_path
            base_alloc <- setNames(selected_db$base_alloc, selected_db$strategy_name)
            max_alloc <- setNames(selected_db$max_alloc, selected_db$strategy_name)
        } else {
            # ETF-only mode
            strategy_files <- character(0)
            base_alloc <- numeric(0)
            max_alloc <- numeric(0)
        }

        # Show progress
        progress <- Progress$new()
        progress$set(message = "Running optimization...", value = 0)
        on.exit(progress$close())

        progress_callback <- function(msg) {
            progress$set(message = msg, value = runif(1, 0.1, 0.9))
        }

        tryCatch(
            {
                result <- optimize_portfolio(
                    baseDir = input$baseDir,
                    strategy_files = strategy_files,
                    base_alloc = base_alloc,
                    max_alloc = max_alloc,
                    total_max = input$total_max,
                    dd_limit = input$dd_limit,
                    n_days_sim_opt = input$n_days_sim,
                    n_paths_opt = input$n_paths_opt,
                    n_paths_final = input$n_paths_final,
                    swarm_size = input$swarm_size,
                    maxit = input$maxit,
                    seed = input$seed,
                    date_merge_method = input$date_merge_method,
                    etf_sma_symbols = input$etf_sma_symbols,
                    etf_buyhold_symbols = input$etf_buyhold_symbols,
                    etf_base_alloc = input$etf_base_alloc,
                    etf_max_alloc = input$etf_max_alloc,
                    verbose = FALSE,
                    progress_callback = progress_callback
                )

                optimization_result(result)
                showNotification("Optimization complete!", type = "message")
            },
            error = function(e) {
                showNotification(paste("Error:", e$message), type = "error")
            }
        )
    })

    output$optimizationResults <- renderPrint({
        req(optimization_result())
        result <- optimization_result()

        cat("=== OPTIMAL PORTFOLIO ===\n\n")
        cat("Optimal Weights (%):\n")
        print(round(result$weights_optimal * 100, 2))
        cat("\n")
        cat(sprintf("Q25 CAGR: %.2f%%\n", result$q25_CAGR * 100))
        cat(sprintf("Q95 Max Drawdown: %.2f%%\n", result$q95_MaxDD * 100))
        cat(sprintf("Total Allocation: %.2f%%\n\n", sum(result$weights_optimal) * 100))

        cat("=== DRAWDOWN STATISTICS ===\n\n")
        cat("Drawdown Depth (at confidence levels):\n")
        cat(sprintf("  50th percentile (median): %.2f%%\n", result$dd_depth_quantiles[1] * 100))
        cat(sprintf("  75th percentile: %.2f%%\n", result$dd_depth_quantiles[2] * 100))
        cat(sprintf("  90th percentile: %.2f%%\n", result$dd_depth_quantiles[3] * 100))
        cat(sprintf("  95th percentile: %.2f%%\n", result$dd_depth_quantiles[4] * 100))
        cat(sprintf("  99th percentile: %.2f%%\n\n", result$dd_depth_quantiles[5] * 100))

        cat("Drawdown Duration:\n")
        if (!is.na(result$avg_dd_duration)) {
            cat(sprintf("  Average: %.1f days\n", result$avg_dd_duration))
            cat(sprintf("  Median: %.1f days\n", result$median_dd_duration))
            cat(sprintf("  Maximum: %.0f days\n\n", result$max_dd_duration))
            cat("  Duration at confidence levels:\n")
            cat(sprintf("    50th percentile: %.0f days\n", result$dd_duration_quantiles[1]))
            cat(sprintf("    75th percentile: %.0f days\n", result$dd_duration_quantiles[2]))
            cat(sprintf("    90th percentile: %.0f days\n", result$dd_duration_quantiles[3]))
            cat(sprintf("    95th percentile: %.0f days\n", result$dd_duration_quantiles[4]))
            cat(sprintf("    99th percentile: %.0f days\n", result$dd_duration_quantiles[5]))
        } else {
            cat("  No drawdowns detected in simulation\n")
        }
    })

    output$cagrPlot <- renderPlotly({
        req(optimization_result())
        result <- optimization_result()

        df <- data.frame(CAGR = result$cagrs_all * 100)

        p <- ggplot(df, aes(x = CAGR)) +
            geom_histogram(bins = 50, fill = "steelblue", alpha = 0.7) +
            geom_vline(xintercept = result$q25_CAGR * 100, color = "red", linetype = "dashed", size = 1) +
            labs(title = "CAGR Distribution", x = "CAGR (%)", y = "Frequency") +
            theme_minimal()

        ggplotly(p)
    })

    output$ddPlot <- renderPlotly({
        req(optimization_result())
        result <- optimization_result()

        df <- data.frame(MaxDD = result$maxdd_all * 100)

        p <- ggplot(df, aes(x = MaxDD)) +
            geom_histogram(bins = 50, fill = "darkred", alpha = 0.7) +
            geom_vline(xintercept = result$q95_MaxDD * 100, color = "blue", linetype = "dashed", size = 1) +
            labs(title = "Max Drawdown Distribution", x = "Max Drawdown (%)", y = "Frequency") +
            theme_minimal()

        ggplotly(p)
    })

    # Single strategy analysis
    single_result <- reactiveVal(NULL)

    output$singleAnalysisStatus <- renderText({
        if (is.null(single_result())) {
            "Ready to run analysis"
        } else {
            "Analysis complete"
        }
    })

    observeEvent(input$runSingleAnalysis, {
        req(input$selectedSingleStrategy)

        current_db <- db()
        selected_row <- current_db[current_db$strategy_name == input$selectedSingleStrategy, ]

        if (nrow(selected_row) == 0) {
            showNotification("Strategy not found", type = "error")
            return()
        }

        # Prepare inputs
        alloc_range <- seq(input$alloc_min, input$alloc_max, by = input$alloc_step)

        # Show progress
        progress <- Progress$new()
        progress$set(message = "Running analysis...", value = 0)
        on.exit(progress$close())

        progress_callback <- function(msg) {
            progress$set(message = msg, value = runif(1, 0.1, 0.9))
        }

        tryCatch(
            {
                result <- analyze_single_strategy(
                    baseDir = input$baseDir,
                    strategy_file = selected_row$file_path,
                    strategy_name = selected_row$strategy_name,
                    base_alloc = setNames(selected_row$base_alloc, selected_row$strategy_name),
                    alloc_range = alloc_range,
                    dd_limit = input$single_dd_limit,
                    n_days_sim = input$single_n_days,
                    n_paths = input$single_n_paths,
                    seed = input$single_seed,
                    date_merge_method = input$single_date_merge_method,
                    progress_callback = progress_callback
                )

                single_result(result)
                showNotification("Analysis complete!", type = "message")
            },
            error = function(e) {
                showNotification(paste("Error:", e$message), type = "error")
            }
        )
    })

    output$singleAnalysisResults <- renderPrint({
        req(single_result())
        result <- single_result()

        if (is.na(result$optimal_allocation)) {
            cat("No allocation meets the drawdown constraint.\n")
        } else {
            cat("=== OPTIMAL ALLOCATION ===\n\n")
            cat(sprintf("Optimal Allocation: %.2f%%\n", result$optimal_allocation * 100))
            cat(sprintf("Q25 CAGR at Optimal: %.2f%%\n", result$optimal_q25_CAGR * 100))
            cat(sprintf("Q95 Max DD at Optimal: %.2f%%\n\n", result$optimal_q95_MaxDD * 100))

            if (!is.null(result$optimal_dd_stats)) {
                dd_stats <- result$optimal_dd_stats

                cat("=== DRAWDOWN STATISTICS ===\n\n")
                cat("Drawdown Depth (at confidence levels):\n")
                cat(sprintf("  50th percentile (median): %.2f%%\n", dd_stats$dd_depth_quantiles[1] * 100))
                cat(sprintf("  75th percentile: %.2f%%\n", dd_stats$dd_depth_quantiles[2] * 100))
                cat(sprintf("  90th percentile: %.2f%%\n", dd_stats$dd_depth_quantiles[3] * 100))
                cat(sprintf("  95th percentile: %.2f%%\n", dd_stats$dd_depth_quantiles[4] * 100))
                cat(sprintf("  99th percentile: %.2f%%\n\n", dd_stats$dd_depth_quantiles[5] * 100))

                cat("Drawdown Duration:\n")
                if (!is.na(dd_stats$avg_dd_duration)) {
                    cat(sprintf("  Average: %.1f days\n", dd_stats$avg_dd_duration))
                    cat(sprintf("  Median: %.1f days\n", dd_stats$median_dd_duration))
                    cat(sprintf("  Maximum: %.0f days\n\n", dd_stats$max_dd_duration))
                    cat("  Duration at confidence levels:\n")
                    cat(sprintf("    50th percentile: %.0f days\n", dd_stats$dd_duration_quantiles[1]))
                    cat(sprintf("    75th percentile: %.0f days\n", dd_stats$dd_duration_quantiles[2]))
                    cat(sprintf("    90th percentile: %.0f days\n", dd_stats$dd_duration_quantiles[3]))
                    cat(sprintf("    95th percentile: %.0f days\n", dd_stats$dd_duration_quantiles[4]))
                    cat(sprintf("    99th percentile: %.0f days\n", dd_stats$dd_duration_quantiles[5]))
                } else {
                    cat("  No drawdowns detected in simulation\n")
                }
            }
        }
    })

    output$singleCAGRPlot <- renderPlotly({
        req(single_result())
        result <- single_result()

        df <- result$results
        df$allocation_pct <- df$allocation * 100
        df$q25_CAGR_pct <- df$q25_CAGR * 100

        p <- ggplot(df, aes(
            x = allocation_pct, y = q25_CAGR_pct,
            color = meets_dd_constraint
        )) +
            geom_line(size = 1) +
            geom_point() +
            scale_color_manual(
                values = c("TRUE" = "green", "FALSE" = "red"),
                labels = c("TRUE" = "Meets Constraint", "FALSE" = "Violates Constraint")
            ) +
            labs(
                title = "Q25 CAGR vs Allocation",
                x = "Allocation (%)", y = "Q25 CAGR (%)", color = ""
            ) +
            theme_minimal()

        if (!is.na(result$optimal_allocation)) {
            p <- p + geom_vline(
                xintercept = result$optimal_allocation * 100,
                linetype = "dashed", color = "blue", size = 1
            )
        }

        ggplotly(p)
    })

    output$singleDDPlot <- renderPlotly({
        req(single_result())
        result <- single_result()

        df <- result$results
        df$allocation_pct <- df$allocation * 100
        df$q95_MaxDD_pct <- df$q95_MaxDD * 100

        p <- ggplot(df, aes(
            x = allocation_pct, y = q95_MaxDD_pct,
            color = meets_dd_constraint
        )) +
            geom_line(size = 1) +
            geom_point() +
            geom_hline(
                yintercept = -input$single_dd_limit * 100,
                linetype = "dashed", color = "orange", size = 1
            ) +
            scale_color_manual(
                values = c("TRUE" = "green", "FALSE" = "red"),
                labels = c("TRUE" = "Meets Constraint", "FALSE" = "Violates Constraint")
            ) +
            labs(
                title = "Q95 Max Drawdown vs Allocation",
                x = "Allocation (%)", y = "Q95 Max Drawdown (%)", color = ""
            ) +
            theme_minimal()

        if (!is.na(result$optimal_allocation)) {
            p <- p + geom_vline(
                xintercept = result$optimal_allocation * 100,
                linetype = "dashed", color = "blue", size = 1
            )
        }

        ggplotly(p)
    })

    # ===== ROBUSTNESS TESTING =====

    # Strategy dropdown for robustness testing
    output$robustnessStrategySelect <- renderUI({
        req(db())
        strategies <- db()$strategy_name
        selectInput("selectedRobustnessStrategy", "Select Strategy:",
            choices = strategies, selected = strategies[1]
        )
    })

    # Reactive value to store robustness results
    robustness_result <- reactiveVal(NULL)

    # Run robustness tests
    observeEvent(input$runRobustnessTest, {
        req(input$selectedRobustnessStrategy)

        current_db <- db()
        selected_row <- current_db[current_db$strategy_name == input$selectedRobustnessStrategy, ]

        if (nrow(selected_row) == 0) {
            showNotification("Strategy not found", type = "error")
            return()
        }

        # Show progress
        progress <- Progress$new()
        progress$set(message = "Running robustness tests...", value = 0)
        on.exit(progress$close())

        tryCatch(
            {
                # Load strategy data
                progress$set(message = "Loading strategy data...", value = 0.1)
                prep <- build_unit_R(
                    input$baseDir, selected_row$file_path,
                    setNames(selected_row$base_alloc, selected_row$strategy_name)
                )

                returns <- as.numeric(prep$unit_R) * selected_row$base_alloc
                dates <- prep$dates

                # Time period analysis
                progress$set(message = "Analyzing time periods...", value = 0.2)
                time_analysis <- analyze_time_periods(returns, dates, input$n_time_periods)

                # Walk-forward test
                progress$set(message = "Running walk-forward test...", value = 0.4)
                wf_analysis <- walk_forward_test(returns, dates, input$train_test_split, n_folds = 5)

                # Rolling metrics
                progress$set(message = "Calculating rolling metrics...", value = 0.6)
                rolling_metrics <- calculate_rolling_metrics(returns, dates, input$rolling_window)

                # Drawdown analysis
                progress$set(message = "Analyzing drawdowns...", value = 0.7)
                dd_analysis <- analyze_drawdowns(returns, dates)

                # Statistical tests
                progress$set(message = "Running statistical tests...", value = 0.8)
                stat_tests <- run_statistical_tests(returns)

                # Bootstrap CI
                progress$set(message = "Bootstrap confidence intervals...", value = 0.9)
                bootstrap_ci <- bootstrap_metrics(returns, input$n_bootstrap)

                # Calculate overall score
                progress$set(message = "Calculating robustness score...", value = 0.95)
                overall_score <- calculate_robustness_score(time_analysis, wf_analysis, dd_analysis, stat_tests)

                # Store results
                robustness_result(list(
                    time_analysis = time_analysis,
                    wf_analysis = wf_analysis,
                    rolling_metrics = rolling_metrics,
                    dd_analysis = dd_analysis,
                    stat_tests = stat_tests,
                    bootstrap_ci = bootstrap_ci,
                    overall_score = overall_score
                ))

                showNotification("Robustness tests complete!", type = "message")
            },
            error = function(e) {
                showNotification(paste("Error:", e$message), type = "error")
            }
        )
    })

    # Overall verdict display
    output$robustnessVerdict <- renderUI({
        req(robustness_result())
        result <- robustness_result()
        score <- result$overall_score

        verdict_color <- switch(score$verdict,
            "ROBUST" = "success",
            "CAUTION" = "warning",
            "OVERFITTED" = "danger"
        )

        verdict_icon <- switch(score$verdict,
            "ROBUST" = "check-circle",
            "CAUTION" = "exclamation-triangle",
            "OVERFITTED" = "times-circle"
        )

        div(
            class = paste0("alert alert-", verdict_color),
            style = "font-size: 20px; text-align: center;",
            icon(verdict_icon, class = "fa-2x"),
            h3(score$verdict, style = "margin-top: 10px;"),
            p(sprintf("Score: %d/%d (%.0f%%)", score$score, score$max_score, score$score_pct * 100))
        )
    })

    # Robustness summary
    output$robustnessSummary <- renderPrint({
        req(robustness_result())
        result <- robustness_result()
        score <- result$overall_score

        cat("=== ROBUSTNESS ASSESSMENT ===\n\n")
        cat(sprintf("Overall Verdict: %s\n", score$verdict))
        cat(sprintf("Score: %d/%d (%.0f%%)\n\n", score$score, score$max_score, score$score_pct * 100))

        if (length(score$flags) > 0) {
            cat("⚠️  Warning Flags:\n")
            for (flag in score$flags) {
                cat(sprintf("  - %s\n", flag))
            }
        } else {
            cat("✅ No major concerns detected\n")
        }
    })

    # Time period table
    output$timePeriodTable <- renderDT({
        req(robustness_result())
        result <- robustness_result()

        df <- result$time_analysis$period_stats
        df$cagr <- sprintf("%.2f%%", df$cagr * 100)
        df$sharpe <- sprintf("%.2f", df$sharpe)
        df$max_dd <- sprintf("%.2f%%", df$max_dd * 100)
        df$win_rate <- sprintf("%.2f%%", df$win_rate * 100)

        datatable(df, options = list(pageLength = 10, dom = "t"))
    })

    # Time period plot
    output$timePeriodPlot <- renderPlotly({
        req(robustness_result())
        result <- robustness_result()

        df <- result$time_analysis$period_stats
        df$period_label <- paste("Period", df$period)

        p <- ggplot(df, aes(x = period_label, y = cagr * 100)) +
            geom_bar(stat = "identity", fill = "steelblue", alpha = 0.7) +
            geom_hline(yintercept = mean(df$cagr) * 100, linetype = "dashed", color = "red") +
            labs(title = "CAGR by Time Period", x = "Period", y = "CAGR (%)") +
            theme_minimal()

        ggplotly(p)
    })

    # Walk-forward table
    output$walkForwardTable <- renderDT({
        req(robustness_result())
        result <- robustness_result()

        df <- result$wf_analysis$fold_results
        df$train_cagr <- sprintf("%.2f%%", df$train_cagr * 100)
        df$test_cagr <- sprintf("%.2f%%", df$test_cagr * 100)
        df$degradation <- sprintf("%.1f%%", df$degradation)

        datatable(df, options = list(pageLength = 10, dom = "t"))
    })

    # Walk-forward plot
    output$walkForwardPlot <- renderPlotly({
        req(robustness_result())
        result <- robustness_result()

        df <- result$wf_analysis$fold_results
        df_long <- data.frame(
            fold = rep(df$fold, 2),
            type = rep(c("Train", "Test"), each = nrow(df)),
            cagr = c(df$train_cagr, df$test_cagr) * 100
        )

        p <- ggplot(df_long, aes(x = fold, y = cagr, color = type, group = type)) +
            geom_line(size = 1) +
            geom_point(size = 3) +
            labs(title = "Walk-Forward: Train vs Test CAGR", x = "Fold", y = "CAGR (%)", color = "") +
            theme_minimal()

        ggplotly(p)
    })

    # Rolling Sharpe plot
    output$rollingSharpe <- renderPlotly({
        req(robustness_result())
        result <- robustness_result()

        if (is.null(result$rolling_metrics)) {
            return(NULL)
        }

        df <- result$rolling_metrics

        p <- ggplot(df, aes(x = date, y = rolling_sharpe)) +
            geom_line(color = "darkblue", size = 0.8) +
            geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
            labs(title = "Rolling Sharpe Ratio", x = "Date", y = "Sharpe Ratio") +
            theme_minimal()

        ggplotly(p)
    })

    # Rolling CAGR plot
    output$rollingCAGR <- renderPlotly({
        req(robustness_result())
        result <- robustness_result()

        if (is.null(result$rolling_metrics)) {
            return(NULL)
        }

        df <- result$rolling_metrics

        p <- ggplot(df, aes(x = date, y = rolling_cagr * 100)) +
            geom_line(color = "darkgreen", size = 0.8) +
            geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
            labs(title = "Rolling CAGR", x = "Date", y = "CAGR (%)") +
            theme_minimal()

        ggplotly(p)
    })

    # Drawdown stats
    output$drawdownStats <- renderPrint({
        req(robustness_result())
        result <- robustness_result()
        dd <- result$dd_analysis

        cat("=== DRAWDOWN ANALYSIS ===\n\n")
        cat(sprintf("Number of Drawdowns: %d\n", dd$n_drawdowns))
        if (dd$n_drawdowns > 0) {
            cat(sprintf("Average Duration: %.1f days\n", dd$avg_duration))
            cat(sprintf("Average Depth: %.2f%%\n", dd$avg_depth * 100))
            cat(sprintf("Maximum Depth: %.2f%%\n", dd$max_depth * 100))
        }
    })

    # Drawdown histogram
    output$drawdownHist <- renderPlotly({
        req(robustness_result())
        result <- robustness_result()
        dd <- result$dd_analysis

        if (dd$n_drawdowns == 0) {
            return(NULL)
        }

        df <- dd$drawdown_details

        p <- ggplot(df, aes(x = duration)) +
            geom_histogram(bins = 20, fill = "darkred", alpha = 0.7) +
            labs(title = "Drawdown Duration Distribution", x = "Duration (days)", y = "Frequency") +
            theme_minimal()

        ggplotly(p)
    })

    # Statistical test results
    output$statTestResults <- renderPrint({
        req(robustness_result())
        result <- robustness_result()
        stat <- result$stat_tests

        cat("=== STATISTICAL TESTS ===\n\n")

        cat("Normality Tests:\n")
        cat(sprintf(
            "  KS Test p-value: %.4f %s\n",
            stat$ks_pvalue,
            ifelse(stat$ks_pvalue < 0.05, "✅ (Not too normal)", "⚠️  (Suspiciously normal)")
        ))

        if (!is.na(stat$shapiro_pvalue)) {
            cat(sprintf("  Shapiro-Wilk p-value: %.4f\n", stat$shapiro_pvalue))
        }

        cat(sprintf("\nAutocorrelation:\n"))
        cat(sprintf(
            "  Significant lags: %d %s\n",
            stat$sig_autocorr_lags,
            ifelse(stat$sig_autocorr_lags < 3, "✅ (Minimal)", "⚠️  (High)")
        ))
    })

    # Bootstrap results
    output$bootstrapResults <- renderPrint({
        req(robustness_result())
        result <- robustness_result()
        boot <- result$bootstrap_ci

        cat("=== BOOTSTRAP CONFIDENCE INTERVALS (95%) ===\n\n")

        cat(sprintf(
            "CAGR: [%.2f%%, %.2f%%]\n",
            boot$cagr_ci[1] * 100, boot$cagr_ci[2] * 100
        ))

        cat(sprintf(
            "Sharpe: [%.2f, %.2f]\n",
            boot$sharpe_ci[1], boot$sharpe_ci[2]
        ))

        cat(sprintf(
            "Max DD: [%.2f%%, %.2f%%]\n",
            boot$maxdd_ci[1] * 100, boot$maxdd_ci[2] * 100
        ))
    })

    # Bootstrap plot
    output$bootstrapPlot <- renderPlotly({
        req(robustness_result())
        result <- robustness_result()
        boot <- result$bootstrap_ci

        df <- data.frame(CAGR = boot$cagr_dist * 100)

        p <- ggplot(df, aes(x = CAGR)) +
            geom_histogram(bins = 50, fill = "purple", alpha = 0.7) +
            geom_vline(xintercept = boot$cagr_ci[1] * 100, linetype = "dashed", color = "red") +
            geom_vline(xintercept = boot$cagr_ci[2] * 100, linetype = "dashed", color = "red") +
            labs(title = "Bootstrap CAGR Distribution", x = "CAGR (%)", y = "Frequency") +
            theme_minimal()

        ggplotly(p)
    })
}

##### RUN APP ################################################################

shinyApp(ui = ui, server = server)
