library(pso)
library(xts)

##### USER CONFIG ########################################################

baseDir <- "/Users/amod/R-Trading/backtests/"

strategy_files <- c("20_Delta_0DTE.csv",
                    "Delta45_3PMIC.csv",
                    "NTAsymetricRIC.csv",
                    "Monday23DC.csv")

# base allocation used in each backtest (fraction of portfolio)
base_alloc <- c(Delta20_0DTE     = 0.04,
                Delta45_3PMIC    = 0.04,
                NTAsymetricRIC   = 0.05,
                Monday23DC       = 0.1)

stopifnot(length(strategy_files) == length(base_alloc))

# max allocation allowed for the optimizer (can be > base_alloc)
max_alloc <- c(Delta20_0DTE     = 0.10,
               Delta45_3PMIC    = 0.10,
               NTAsymetricRIC   = 0.10,
               Monday23DC     = 0.20)

# sanity
stopifnot(all(names(base_alloc) == names(max_alloc)))

##### DATA LOADING & PREP ###############################################

read_strategy_xts <- function(file) {
  d <- read.csv(file, stringsAsFactors = FALSE)
  
  # normalize date (column name 'Date' in your files)
  d$date <- as.Date(d$Date)
  
  # column 7 = percent return * 100 at base allocation
  # convert to decimal portfolio return at base allocation
  ret_dec <- as.numeric(d[[7]]) / 100
  
  xts(ret_dec, order.by = d$date)
}

build_unit_R <- function(baseDir, strategy_files, base_alloc) {
  full_paths <- file.path(baseDir, strategy_files)
  
  strategy_xts_list <- lapply(full_paths, read_strategy_xts)
  
  # outer join on dates
  R_base_xts <- do.call(merge, c(strategy_xts_list, all = TRUE))
  
  # replace missing returns with 0
  R_base_xts[is.na(R_base_xts)] <- 0
  
  R_base <- as.matrix(R_base_xts)
  
  # name columns according to base_alloc names
  colnames(R_base) <- names(base_alloc)
  rownames(R_base) <- NULL
  
  # convert from "return at base_alloc" to
  # "return per 100% allocation"
  unit_R <- sweep(R_base, 2, base_alloc, "/")
  
  list(
    unit_R = unit_R,
    dates  = index(R_base_xts),
    R_base = R_base
  )
}

##### MONTE CARLO WITH COMMON RANDOM NUMBERS ############################

# Precompute bootstrap indices once and reuse across all weight vectors
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
  n_paths    <- ncol(idx_mat)
  n_strat    <- ncol(unit_R)
  
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
  n_strat    <- length(sampled_list)
  n_days_sim <- nrow(sampled_list[[1]])
  n_paths    <- ncol(sampled_list[[1]])
  
  stopifnot(length(weights) == n_strat)
  
  # 1) Build portfolio returns
  port_ret <- matrix(0, nrow = n_days_sim, ncol = n_paths)
  for (s in seq_len(n_strat)) {
    port_ret <- port_ret + weights[s] * sampled_list[[s]]
  }
  
  # 2) Equity curves and drawdowns
  equity      <- apply(1 + port_ret, 2, cumprod)
  running_max <- apply(equity, 2, cummax)
  dd          <- equity / running_max - 1
  max_dd      <- apply(dd, 2, min)
  
  # 3) CAGRs
  end_equity <- equity[n_days_sim, ]
  years      <- n_days_sim / trading_days_per_year
  cagrs      <- end_equity^(1 / years) - 1
  
  # 4) Quantiles
  q25_cagr  <- quantile(cagrs, 0.25)
  q95_maxdd <- quantile(max_dd, 0.05)
  
  list(
    weights   = weights,
    q25_CAGR  = q25_cagr,
    q95_MaxDD = q95_maxdd,
    cagrs_all = cagrs,
    maxdd_all = max_dd
  )
}


run_portfolio_mc <- function(unit_R,
                             weights,
                             idx_mat,
                             trading_days_per_year = 252) {
  stopifnot(length(weights) == ncol(unit_R))
  
  n_days_sim <- nrow(idx_mat)
  n_paths    <- ncol(idx_mat)
  n_strat    <- ncol(unit_R)
  
  # 1) Build matrix of portfolio returns for ALL paths at once
  #    port_ret[day, path]
  port_ret <- matrix(0, nrow = n_days_sim, ncol = n_paths)
  
  # For each strategy, sample its returns using the same idx_mat
  # and accumulate into port_ret with the given weights
  for (s in seq_len(n_strat)) {
    # vector of historical returns for strategy s
    r_s <- unit_R[, s]
    
    # r_s[idx_mat] flattens to length n_days_sim * n_paths in column-major order
    # reshape into [n_days_sim x n_paths]
    R_s_sampled <- matrix(r_s[idx_mat], nrow = n_days_sim, ncol = n_paths)
    
    port_ret <- port_ret + weights[s] * R_s_sampled
  }
  
  # 2) Equity curves for each path (vectorized over columns)
  #    equity[day, path]
  equity <- apply(1 + port_ret, 2, cumprod)
  
  # 3) Running max and drawdowns for each path (also vectorized)
  running_max <- apply(equity, 2, cummax)
  dd          <- equity / running_max - 1
  
  # max drawdown per path (most negative value in each column)
  max_dd <- apply(dd, 2, min)
  
  # 4) CAGR per path
  end_equity <- equity[n_days_sim, ]          # final equity per path
  years      <- n_days_sim / trading_days_per_year
  cagrs      <- end_equity^(1 / years) - 1    # vector of length n_paths
  
  # 5) Quantiles
  q25_cagr  <- quantile(cagrs, 0.25)    # 25% percentile of CAGR
  q95_maxdd <- quantile(max_dd, 0.05)   # 95% MaxDD = 5% quantile (more negative)
  
  list(
    weights   = weights,
    q25_CAGR  = q25_cagr,
    q95_MaxDD = q95_maxdd,
    cagrs_all = cagrs,
    maxdd_all = max_dd
  )
}

##### PSO OBJECTIVE WITH SOFT DD CONSTRAINT #############################

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
  
  q25   <- as.numeric(mc_res$q25_CAGR)
  q95dd <- as.numeric(mc_res$q95_MaxDD)
  
  # violation: want 95% MaxDD >= -dd_limit
  viol <- (-dd_limit - q95dd)
  viol <- max(0, viol)
  
  penalty <- penalty_lambda * (viol^2)
  
  # minimize: -q25 + penalty
  -q25 + penalty
}

##### TOP-LEVEL FUNCTION: ONE CALL DOES EVERYTHING ######################

optimize_portfolio <- function(baseDir,
                               strategy_files,
                               base_alloc,
                               max_alloc,
                               total_max      = 0.40,
                               dd_limit       = 0.10,
                               n_days_sim_opt = 1000,
                               n_paths_opt    = 4000,
                               n_paths_final  = 20000,
                               swarm_size     = 40,
                               maxit          = 80,
                               seed           = 123,
                               verbose        = TRUE) {
  if (verbose) cat("Building unit_R...\n")
  prep   <- build_unit_R(baseDir, strategy_files, base_alloc)
  unit_R <- prep$unit_R
  
  n_hist  <- nrow(unit_R)
  n_strat <- ncol(unit_R)
  stopifnot(length(max_alloc) == n_strat)
  
  if (verbose) cat("Precomputing indices and sampled returns for optimization...\n")
  idx_mat_opt <- precompute_indices(
    n_hist     = n_hist,
    n_paths    = n_paths_opt,
    n_days_sim = n_days_sim_opt,
    seed       = seed
  )
  sampled_opt <- precompute_sampled_unit_R(unit_R, idx_mat_opt)
  
  lower     <- rep(0, n_strat)
  upper     <- rep(1, n_strat)
  start_par <- rep(0.5, n_strat)
  
  if (verbose) cat("Starting PSO optimization...\n")
  set.seed(seed)
  opt <- psoptim(
    par   = start_par,
    fn    = portfolio_objective_pso,
    lower = lower,
    upper = upper,
    control = list(
      maxit = maxit,
      s     = swarm_size,
      trace = 1   # <-- prints progress to console
    ),
    sampled_unit_R_list = sampled_opt,
    max_alloc           = max_alloc,
    total_max           = total_max,
    dd_limit            = dd_limit
  )
  
  if (verbose) cat("PSO complete. Extracting best weights...\n")
  x_best <- pmax(pmin(opt$par, 1), 0)
  w_best <- x_best * max_alloc
  if (sum(w_best) > total_max && sum(w_best) > 0) {
    w_best <- w_best * (total_max / sum(w_best))
  }
  
  if (verbose) cat("Running final high-precision Monte Carlo...\n")
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
  
  list(
    weights_optimal = w_best,
    q25_CAGR        = final_res$q25_CAGR,
    q95_MaxDD       = final_res$q95_MaxDD,
    cagrs_all       = final_res$cagrs_all,
    maxdd_all       = final_res$maxdd_all,
    psoptim_raw     = opt
  )
}

# Compute basic stats for a numeric return vector
return_stats <- function(x) {
  x <- x[is.finite(x)]
  n <- length(x)
  if (n < 10) {
    return(data.frame(
      n = n,
      mean = NA, sd = NA, skew = NA, kurt = NA,
      q01 = NA, q05 = NA, q25 = NA, q50 = NA,
      q75 = NA, q95 = NA, q99 = NA
    ))
  }
  
  m  <- mean(x)
  s  <- sd(x)
  # simple skewness & kurtosis (excess)
  if (s == 0 || is.na(s)) {
    skew <- NA
    kurt <- NA
  } else {
    z    <- (x - m) / s
    skew <- mean(z^3)
    kurt <- mean(z^4) - 3
  }
  
  qs <- quantile(x, probs = c(0.01, 0.05, 0.25, 0.50, 0.75, 0.95, 0.99))
  
  data.frame(
    n    = n,
    mean = m,
    sd   = s,
    skew = skew,
    kurt = kurt,
    q01  = qs[1],
    q05  = qs[2],
    q25  = qs[3],
    q50  = qs[4],
    q75  = qs[5],
    q95  = qs[6],
    q99  = qs[7]
  )
}


library(zoo)  # for rollapply

analyze_strategy_stability <- function(ret_xts,
                                       strategy_name = "strategy",
                                       n_slices = 4,
                                       rolling_window = 252,
                                       make_plots = TRUE) {
  # ret_xts: xts or zoo of daily returns (decimal)
  x <- as.numeric(ret_xts)
  dates <- index(ret_xts)
  
  good <- is.finite(x)
  x     <- x[good]
  dates <- dates[good]
  
  n <- length(x)
  if (n < n_slices * 20) {
    warning(strategy_name, ": not enough data for ", n_slices, " slices.")
  }
  
  # ---- A. Split into time slices ----
  edges <- floor(seq(1, n + 1, length.out = n_slices + 1))
  
  slice_stats <- list()
  slice_vecs  <- list()
  
  for (k in seq_len(n_slices)) {
    idx_start <- edges[k]
    idx_end   <- edges[k + 1] - 1
    if (idx_end < idx_start) next
    
    x_slice <- x[idx_start:idx_end]
    d_slice <- dates[idx_start:idx_end]
    
    slice_vecs[[k]]  <- x_slice
    slice_stats[[k]] <- cbind(
      slice = k,
      start_date = d_slice[1],
      end_date   = d_slice[length(d_slice)],
      return_stats(x_slice)
    )
  }
  
  slice_stats_df <- do.call(rbind, slice_stats)
  rownames(slice_stats_df) <- NULL
  
  # ---- B. KS tests: slice 1 vs others ----
  ks_results <- data.frame(
    slice_compare = character(0),
    p_value       = numeric(0),
    stringsAsFactors = FALSE
  )
  
  if (length(slice_vecs) >= 2) {
    ref <- slice_vecs[[1]]
    for (k in 2:length(slice_vecs)) {
      test_res <- tryCatch(
        ks.test(ref, slice_vecs[[k]]),
        error = function(e) NULL
      )
      if (!is.null(test_res)) {
        ks_results <- rbind(
          ks_results,
          data.frame(
            slice_compare = paste0("1 vs ", k),
            p_value       = test_res$p.value,
            stringsAsFactors = FALSE
          )
        )
      }
    }
  }
  
  # ---- C. Plots ----
  if (make_plots) {
    op <- par(no.readonly = TRUE)
    on.exit(par(op))
    
    # 1) Density / histogram per slice
    ns <- length(slice_vecs)
    nrow_plot <- ceiling(ns / 2)
    ncol_plot <- 2
    par(mfrow = c(nrow_plot, ncol_plot))
    
    for (k in seq_len(ns)) {
      xs <- slice_vecs[[k]]
      hist(xs,
           breaks = 50,
           main = paste(strategy_name, "- slice", k),
           xlab = "Daily return (decimal)",
           freq = FALSE,
           border = "grey")
      lines(density(xs), lwd = 2)
    }
    
    # 2) Rolling mean & sd (using original time order)
    par(mfrow = c(2, 1))
    z <- zoo(x, dates)
    
    roll_mean <- rollapply(z, width = rolling_window, FUN = mean,
                           align = "right", fill = NA)
    roll_sd   <- rollapply(z, width = rolling_window, FUN = sd,
                           align = "right", fill = NA)
    
    plot(roll_mean,
         main = paste(strategy_name, "- rolling mean (", rolling_window, "d)", sep = ""),
         ylab = "Mean",
         xlab = "Date", type = "l")
    abline(h = mean(x), col = "red", lty = 2)
    
    plot(roll_sd,
         main = paste(strategy_name, "- rolling sd (", rolling_window, "d)", sep = ""),
         ylab = "SD",
         xlab = "Date", type = "l")
    abline(h = sd(x), col = "red", lty = 2)
  }
  
  list(
    slice_stats = slice_stats_df,
    ks_results  = ks_results
  )
}

analyze_all_strategies <- function(ret_xts_multi,
                                   n_slices = 4,
                                   rolling_window = 252,
                                   make_plots = TRUE) {
  results <- list()
  for (j in seq_len(ncol(ret_xts_multi))) {
    strat_name <- colnames(ret_xts_multi)[j]
    cat("Analyzing", strat_name, "...\n")
    res_j <- analyze_strategy_stability(
      ret_xts_multi[, j],
      strategy_name   = strat_name,
      n_slices        = n_slices,
      rolling_window  = rolling_window,
      make_plots      = make_plots
    )
    results[[strat_name]] <- res_j
  }
  results
}


##### EXAMPLE USAGE ######################################################

# Call just this:
# (you can tweak dd_limit, total_max, etc.)


result <- optimize_portfolio(
  baseDir        = baseDir,
  strategy_files = strategy_files,
  base_alloc     = base_alloc,
  max_alloc      = max_alloc,
  total_max      = 0.40,
  dd_limit       = 0.10,   # 95% MaxDD must be >= -0.10 (max 10% drawdown)
  n_days_sim_opt = 1000,
  n_paths_opt    = 2000,
  n_paths_final  = 10000,
  swarm_size     = 40,
  maxit          = 80,
  seed           = 123
)

print(result$weights_optimal * 100) # as %
print(result$q25_CAGR      * 100)  # as %
print(result$q95_MaxDD     * 100)  # as %


