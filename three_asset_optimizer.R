library(shiny)
library(quantmod)
library(tidyverse)
library(PerformanceAnalytics)
library(scales)
library(DT)

# UI Definition
ui <- fluidPage(
    titlePanel("Three-Asset Portfolio Optimizer: QQQ + GLD + TAIL"),
    sidebarLayout(
        sidebarPanel(
            helpText("Find optimal allocation across QQQ, GLD, and TAIL to maximize risk-adjusted returns."),
            h4("Portfolio Settings"),
            dateInput("start_date", "Start Date", value = "2018-01-01"),
            numericInput("initial_cash", "Initial Capital ($)", value = 30000),
            sliderInput("target_leverage", "Target Leverage",
                min = 1.0, max = 3.0, value = 1.3, step = 0.1
            ),
            numericInput("margin_rate", "Margin Interest Rate (Annual %)", value = 7.0),
            sliderInput("rebalance_threshold", "Rebalancing Threshold (±)",
                min = 0.0, max = 0.2, value = 0.05, step = 0.01
            ),
            hr(),
            h4("Optimization Settings"),
            numericInput("step_size", "Allocation Step Size (%)", value = 5, min = 5, max = 20),
            helpText("Smaller step = more combinations tested (slower but more precise)"),
            hr(),
            h4("Monte Carlo Settings"),
            numericInput("mc_sims", "Number of Simulations", value = 500),
            numericInput("mc_days", "Days to Simulate", value = 252),
            actionButton("run", "Run Optimization", class = "btn-primary btn-lg")
        ),
        mainPanel(
            tabsetPanel(
                tabPanel(
                    "Optimization Results",
                    br(),
                    h4("Best Portfolio (Highest Risk-Adjusted Score)"),
                    tableOutput("best_portfolio"),
                    hr(),
                    h4("Top 10 Portfolios"),
                    DTOutput("top_portfolios_table"),
                    hr(),
                    h4("3D Visualization: Allocation Space"),
                    plotOutput("allocation_plot", height = "500px"),
                    br(),
                    h4("All Tested Allocations"),
                    DTOutput("all_results_table")
                ),
                tabPanel(
                    "Best Portfolio Analysis",
                    br(),
                    h4("Portfolio Composition"),
                    plotOutput("allocation_pie"),
                    hr(),
                    h4("Performance Metrics"),
                    tableOutput("selected_metrics"),
                    hr(),
                    plotOutput("selected_equity_curve"),
                    br(),
                    h4("Monte Carlo Projection"),
                    plotOutput("selected_mc_plot")
                )
            )
        )
    )
)

# Server Logic
server <- function(input, output, session) {
    # Function to run backtest for three-asset portfolio
    run_backtest <- function(qqq_pct, gld_pct, tail_pct, prices, dates, params) {
        n_days <- length(dates)
        equity <- numeric(n_days)
        debt <- numeric(n_days)
        cash_added <- numeric(n_days)
        margin_buys <- numeric(n_days)

        # Track holdings for each asset
        qqq_holdings <- numeric(n_days)
        gld_holdings <- numeric(n_days)
        tail_holdings <- numeric(n_days)
        total_holdings <- numeric(n_days)

        # Initial allocation
        equity[1] <- params$initial_cash
        debt[1] <- equity[1] * (params$target_leverage - 1)
        total_value <- equity[1] + debt[1]

        qqq_holdings[1] <- total_value * (qqq_pct / 100)
        gld_holdings[1] <- total_value * (gld_pct / 100)
        tail_holdings[1] <- total_value * (tail_pct / 100)
        total_holdings[1] <- total_value

        daily_margin_rate <- params$margin_rate / 100 / 360

        for (i in 2:n_days) {
            # Calculate returns for each asset
            qqq_return <- as.numeric(prices$QQQ[i]) / as.numeric(prices$QQQ[i - 1]) - 1
            gld_return <- as.numeric(prices$GLD[i]) / as.numeric(prices$GLD[i - 1]) - 1
            tail_return <- as.numeric(prices$TAIL[i]) / as.numeric(prices$TAIL[i - 1]) - 1

            # Update each holding
            qqq_holdings[i] <- qqq_holdings[i - 1] * (1 + qqq_return)
            gld_holdings[i] <- gld_holdings[i - 1] * (1 + gld_return)
            tail_holdings[i] <- tail_holdings[i - 1] * (1 + tail_return)

            current_holdings <- qqq_holdings[i] + gld_holdings[i] + tail_holdings[i]

            # Accrue interest
            interest_cost <- debt[i - 1] * daily_margin_rate
            current_debt <- debt[i - 1] + interest_cost
            current_equity <- current_holdings - current_debt

            # Check for wipeout
            if (current_equity <= 0) {
                equity[i] <- 0
                debt[i] <- current_debt
                total_holdings[i] <- 0
                qqq_holdings[i] <- 0
                gld_holdings[i] <- 0
                tail_holdings[i] <- 0
                next
            }

            # Rebalance with hysteresis
            current_lev <- current_holdings / current_equity
            upper_bound <- params$target_leverage + params$rebalance_threshold
            lower_bound <- params$target_leverage - params$rebalance_threshold

            if (current_lev > upper_bound) {
                # Add cash and buy all three assets in target proportion
                needed_cash <- (current_holdings - params$target_leverage * current_equity) /
                    (params$target_leverage - 1)
                if (needed_cash > 0) {
                    cash_added[i] <- needed_cash
                    current_equity <- current_equity + needed_cash

                    qqq_buy <- needed_cash * (qqq_pct / 100)
                    gld_buy <- needed_cash * (gld_pct / 100)
                    tail_buy <- needed_cash * (tail_pct / 100)

                    qqq_holdings[i] <- qqq_holdings[i] + qqq_buy
                    gld_holdings[i] <- gld_holdings[i] + gld_buy
                    tail_holdings[i] <- tail_holdings[i] + tail_buy
                    current_holdings <- current_holdings + needed_cash
                }
            } else if (current_lev < lower_bound) {
                # Borrow and buy all three assets in target proportion
                target_holdings <- current_equity * params$target_leverage
                buy_amount <- target_holdings - current_holdings
                if (buy_amount > 0) {
                    margin_buys[i] <- buy_amount

                    qqq_buy <- buy_amount * (qqq_pct / 100)
                    gld_buy <- buy_amount * (gld_pct / 100)
                    tail_buy <- buy_amount * (tail_pct / 100)

                    qqq_holdings[i] <- qqq_holdings[i] + qqq_buy
                    gld_holdings[i] <- gld_holdings[i] + gld_buy
                    tail_holdings[i] <- tail_holdings[i] + tail_buy
                    current_holdings <- current_holdings + buy_amount
                    current_debt <- current_debt + buy_amount
                }
            }

            equity[i] <- current_equity
            debt[i] <- current_debt
            total_holdings[i] <- current_holdings
        }

        results <- xts(data.frame(
            Equity = equity,
            Debt = debt,
            Cash_Added = cash_added,
            Margin_Buys = margin_buys,
            Total_Assets = total_holdings,
            QQQ_Holdings = qqq_holdings,
            GLD_Holdings = gld_holdings,
            TAIL_Holdings = tail_holdings
        ), order.by = dates)

        return(results)
    }

    # Main optimization
    optimization_results <- eventReactive(input$run, {
        withProgress(message = "Running optimization...", {
            # Fetch price data
            incProgress(0.1, detail = "Fetching price data...")

            qqq_data <- getSymbols("QQQ", src = "yahoo", from = input$start_date, auto.assign = FALSE)
            gld_data <- getSymbols("GLD", src = "yahoo", from = input$start_date, auto.assign = FALSE)
            tail_data <- getSymbols("TAIL", src = "yahoo", from = input$start_date, auto.assign = FALSE)

            # Align prices
            prices <- merge(Ad(qqq_data), Ad(gld_data), Ad(tail_data))
            colnames(prices) <- c("QQQ", "GLD", "TAIL")
            prices <- na.omit(prices)
            dates <- index(prices)

            # Parameters
            params <- list(
                initial_cash = input$initial_cash,
                target_leverage = input$target_leverage,
                margin_rate = input$margin_rate,
                rebalance_threshold = input$rebalance_threshold,
                mc_sims = input$mc_sims,
                mc_days = input$mc_days
            )

            # Generate all valid allocation combinations
            step <- input$step_size
            allocations <- expand.grid(
                QQQ = seq(0, 100, by = step),
                GLD = seq(0, 100, by = step),
                TAIL = seq(0, 100, by = step)
            )

            # Filter to only combinations that sum to 100%
            allocations <- allocations %>%
                filter(QQQ + GLD + TAIL == 100)

            total_tests <- nrow(allocations)
            incProgress(0.2, detail = sprintf("Testing %d allocations...", total_tests))

            results_list <- list()

            for (idx in 1:nrow(allocations)) {
                qqq_pct <- allocations$QQQ[idx]
                gld_pct <- allocations$GLD[idx]
                tail_pct <- allocations$TAIL[idx]

                # Run backtest
                bt_result <- run_backtest(qqq_pct, gld_pct, tail_pct, prices, dates, params)

                # Calculate metrics
                total_invested <- params$initial_cash + sum(bt_result$Cash_Added)
                final_equity <- as.numeric(tail(bt_result$Equity, 1))

                # Skip if portfolio wiped out
                if (final_equity <= 0) {
                    next
                }

                # Calculate asset returns matrix (preserves correlations)
                qqq_returns <- dailyReturn(prices$QQQ)
                gld_returns <- dailyReturn(prices$GLD)
                tail_returns <- dailyReturn(prices$TAIL)

                # Create returns matrix: each row is a day, columns are assets
                returns_matrix <- cbind(
                    QQQ = as.numeric(qqq_returns),
                    GLD = as.numeric(gld_returns),
                    TAIL = as.numeric(tail_returns)
                )
                returns_matrix <- na.omit(returns_matrix)

                # Monte Carlo: Block bootstrap preserving correlations
                # Sample entire DAYS (rows) to keep asset correlations intact
                mc_matrix <- matrix(0, nrow = params$mc_days, ncol = params$mc_sims)

                for (j in 1:params$mc_sims) {
                    # Sample random days (with replacement)
                    sampled_days <- sample(1:nrow(returns_matrix), params$mc_days, replace = TRUE)

                    # Get returns for those days (keeps correlations)
                    sampled_returns <- returns_matrix[sampled_days, ]

                    # Calculate portfolio return for each day using allocation weights
                    portfolio_returns <- (qqq_pct / 100) * sampled_returns[, "QQQ"] +
                        (gld_pct / 100) * sampled_returns[, "GLD"] +
                        (tail_pct / 100) * sampled_returns[, "TAIL"]

                    # Cumulative product to get path
                    mc_matrix[, j] <- cumprod(1 + portfolio_returns)
                }

                # Monte Carlo: Calculate metrics for each path
                mc_years <- params$mc_days / 252
                final_returns <- mc_matrix[nrow(mc_matrix), ] - 1
                mc_cagrs <- ((1 + final_returns)^(1 / mc_years)) - 1

                drawdowns <- apply(mc_matrix, 2, function(x) {
                    max_val <- cummax(c(1, x))[-1]
                    dd <- (x - max_val) / max_val
                    min(dd)
                })

                # Historical CAGR
                cagr_years <- as.numeric(difftime(tail(dates, 1), dates[1], units = "days")) / 365.25
                cagr <- ((final_equity / total_invested)^(1 / cagr_years)) - 1

                # Percentiles
                p25_cagr <- quantile(mc_cagrs, 0.25)
                p95_drawdown <- quantile(drawdowns, 0.05)

                # Risk-adjusted score
                risk_adj_score <- p25_cagr / abs(p95_drawdown)

                results_list[[idx]] <- list(
                    qqq_pct = qqq_pct,
                    gld_pct = gld_pct,
                    tail_pct = tail_pct,
                    cagr = cagr,
                    p25_cagr = p25_cagr,
                    median_cagr = median(mc_cagrs),
                    p95_drawdown = p95_drawdown,
                    risk_adj_score = risk_adj_score,
                    final_equity = final_equity,
                    total_invested = total_invested,
                    max_cash_added = sum(bt_result$Cash_Added),
                    backtest_result = bt_result,
                    mc_matrix = mc_matrix
                )

                if (idx %% 10 == 0) {
                    incProgress(0.7 / total_tests * 10,
                        detail = sprintf(
                            "%d/%d: QQQ %d%% / GLD %d%% / TAIL %d%%",
                            idx, total_tests, qqq_pct, gld_pct, tail_pct
                        )
                    )
                }
            }

            # Remove NULL entries (wiped out portfolios)
            results_list <- Filter(Negate(is.null), results_list)

            incProgress(1.0, detail = "Complete!")
            return(results_list)
        })
    })

    # Best portfolio
    best_portfolio <- reactive({
        results <- optimization_results()
        scores <- map_dbl(results, ~ .$risk_adj_score)
        results[[which.max(scores)]]
    })

    # Best portfolio display
    output$best_portfolio <- renderTable({
        portfolio <- best_portfolio()

        data.frame(
            Metric = c(
                "Allocation", "Risk-Adjusted Score", "Historical CAGR",
                "P25 CAGR", "Median CAGR", "P95 Drawdown",
                "Final Value", "Total Cash Added"
            ),
            Value = c(
                sprintf(
                    "QQQ: %d%% / GLD: %d%% / TAIL: %d%%",
                    portfolio$qqq_pct, portfolio$gld_pct, portfolio$tail_pct
                ),
                round(portfolio$risk_adj_score, 3),
                percent(portfolio$cagr, accuracy = 0.1),
                percent(portfolio$p25_cagr, accuracy = 0.1),
                percent(portfolio$median_cagr, accuracy = 0.1),
                percent(portfolio$p95_drawdown, accuracy = 0.1),
                dollar(portfolio$final_equity),
                dollar(portfolio$max_cash_added)
            )
        )
    })

    # Top 10 portfolios
    output$top_portfolios_table <- renderDT({
        results <- optimization_results()

        df <- map_df(results, function(r) {
            data.frame(
                QQQ_Pct = r$qqq_pct,
                GLD_Pct = r$gld_pct,
                TAIL_Pct = r$tail_pct,
                Risk_Adj_Score = round(r$risk_adj_score, 3),
                CAGR = percent(r$cagr, accuracy = 0.1),
                P25_CAGR = percent(r$p25_cagr, accuracy = 0.1),
                P95_DD = percent(r$p95_drawdown, accuracy = 0.1),
                Cash_Added = dollar(r$max_cash_added)
            )
        }) %>%
            arrange(desc(Risk_Adj_Score)) %>%
            head(10)

        datatable(df, options = list(dom = "t", pageLength = 10), rownames = FALSE)
    })

    # All results table
    output$all_results_table <- renderDT({
        results <- optimization_results()

        df <- map_df(results, function(r) {
            data.frame(
                QQQ = r$qqq_pct,
                GLD = r$gld_pct,
                TAIL = r$tail_pct,
                Risk_Adj_Score = round(r$risk_adj_score, 3),
                CAGR = percent(r$cagr, accuracy = 0.1),
                P25_CAGR = percent(r$p25_cagr, accuracy = 0.1),
                P95_DD = percent(r$p95_drawdown, accuracy = 0.1)
            )
        }) %>%
            arrange(desc(Risk_Adj_Score))

        datatable(df, options = list(pageLength = 25, scrollX = TRUE), rownames = FALSE)
    })

    # 3D allocation plot
    output$allocation_plot <- renderPlot({
        results <- optimization_results()

        df <- map_df(results, function(r) {
            data.frame(
                QQQ = r$qqq_pct,
                GLD = r$gld_pct,
                TAIL = r$tail_pct,
                Score = r$risk_adj_score
            )
        })

        # Create ternary-style plot
        ggplot(df, aes(x = GLD, y = TAIL, color = Score, size = Score)) +
            geom_point(alpha = 0.6) +
            scale_color_gradient2(
                low = "red", mid = "yellow", high = "green",
                midpoint = median(df$Score)
            ) +
            scale_size_continuous(range = c(2, 8)) +
            labs(
                title = "Portfolio Allocation Space (QQQ% = 100% - GLD% - TAIL%)",
                x = "GLD Allocation (%)",
                y = "TAIL Allocation (%)",
                color = "Risk-Adj Score",
                size = "Risk-Adj Score"
            ) +
            theme_minimal() +
            theme(legend.position = "right")
    })

    # Allocation pie chart
    output$allocation_pie <- renderPlot({
        portfolio <- best_portfolio()

        df <- data.frame(
            Asset = c("QQQ", "GLD", "TAIL"),
            Allocation = c(portfolio$qqq_pct, portfolio$gld_pct, portfolio$tail_pct)
        ) %>%
            filter(Allocation > 0)

        ggplot(df, aes(x = "", y = Allocation, fill = Asset)) +
            geom_bar(stat = "identity", width = 1) +
            coord_polar("y") +
            scale_fill_manual(values = c("QQQ" = "#4CAF50", "GLD" = "#FFD700", "TAIL" = "#2196F3")) +
            labs(title = "Optimal Portfolio Allocation") +
            theme_void() +
            theme(legend.position = "right") +
            geom_text(aes(label = paste0(round(Allocation), "%")),
                position = position_stack(vjust = 0.5), size = 6
            )
    })

    # Selected metrics
    output$selected_metrics <- renderTable({
        portfolio <- best_portfolio()

        data.frame(
            Metric = c(
                "Historical CAGR", "25th Percentile CAGR", "Median CAGR",
                "95th Percentile Drawdown", "Risk-Adjusted Score",
                "Final Multiple", "Total Cash Added"
            ),
            Value = c(
                percent(portfolio$cagr, accuracy = 0.1),
                percent(portfolio$p25_cagr, accuracy = 0.1),
                percent(portfolio$median_cagr, accuracy = 0.1),
                percent(portfolio$p95_drawdown, accuracy = 0.1),
                round(portfolio$risk_adj_score, 3),
                round(portfolio$final_equity / portfolio$total_invested, 2),
                dollar(portfolio$max_cash_added)
            )
        )
    })

    # Equity curve
    output$selected_equity_curve <- renderPlot({
        portfolio <- best_portfolio()
        plot(portfolio$backtest_result$Equity,
            main = "Equity Curve (Log Scale)",
            col = "blue", lwd = 2, log = "y",
            ylab = "Equity ($)", xlab = "Date"
        )
        grid()
    })

    # Monte Carlo plot
    output$selected_mc_plot <- renderPlot({
        portfolio <- best_portfolio()
        sims <- portfolio$mc_matrix

        matplot(sims,
            type = "l", lty = 1, col = alpha("gray", 0.2),
            main = paste("Monte Carlo:", input$mc_days, "Trading Days"),
            ylab = "Portfolio Multiple", xlab = "Days"
        )

        lines(apply(sims, 1, quantile, probs = 0.50), col = "blue", lwd = 3)
        lines(apply(sims, 1, quantile, probs = 0.75), col = "green", lwd = 2)
        lines(apply(sims, 1, quantile, probs = 0.25), col = "orange", lwd = 2)
        lines(apply(sims, 1, quantile, probs = 0.05), col = "red", lwd = 2)

        legend("topleft",
            legend = c("Median", "75th %", "25th %", "5th %"),
            col = c("blue", "green", "orange", "red"),
            lwd = c(3, 2, 2, 2)
        )
        grid()
    })
}

# Run the App
shinyApp(ui = ui, server = server)
