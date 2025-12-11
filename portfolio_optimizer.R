library(shiny)
library(quantmod)
library(tidyverse)
library(PerformanceAnalytics)
library(scales)
library(DT)

# UI Definition
ui <- fluidPage(
    titlePanel("Leveraged Portfolio Optimizer with Defensive Assets"),
    sidebarLayout(
        sidebarPanel(
            helpText("Find optimal mix of QQQ + defensive assets to maximize risk-adjusted returns."),
            h4("Portfolio Settings"),
            textInput("growth_ticker", "Growth Asset", value = "QQQ"),
            checkboxGroupInput("defensive_tickers", "Defensive Assets to Test",
                choices = c(
                    "TAIL" = "TAIL",
                    "Gold (GLD)" = "GLD",
                    "JAAA" = "JAAA",
                    "BOXX" = "BOXX"
                ),
                selected = c("TAIL", "GLD")
            ),
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
            sliderInput("defensive_range", "Defensive Allocation Range (%)",
                min = 0, max = 50, value = c(0, 30), step = 5
            ),
            numericInput("step_size", "Step Size (%)", value = 5, min = 1, max = 10),
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
                    h4("Best Portfolios by Objective"),
                    DTOutput("best_portfolios_table"),
                    hr(),
                    h4("All Tested Allocations"),
                    plotOutput("efficient_frontier"),
                    br(),
                    DTOutput("all_results_table")
                ),
                tabPanel(
                    "Best Portfolio Details",
                    br(),
                    h4("Selected Portfolio Performance"),
                    tableOutput("selected_metrics"),
                    hr(),
                    plotOutput("selected_equity_curve"),
                    br(),
                    h4("Monte Carlo Projection"),
                    plotOutput("selected_mc_plot"),
                    tableOutput("selected_mc_stats")
                )
            )
        )
    )
)

# Server Logic
server <- function(input, output, session) {
    # Function to run backtest for a given allocation
    run_backtest <- function(growth_pct, defensive_ticker, defensive_pct,
                             prices_list, dates, params) {
        n_days <- length(dates)
        equity <- numeric(n_days)
        debt <- numeric(n_days)
        cash_added <- numeric(n_days)
        margin_buys <- numeric(n_days)

        # Track holdings separately for growth and defensive
        growth_holdings <- numeric(n_days)
        defensive_holdings <- numeric(n_days)
        total_holdings <- numeric(n_days)

        # Initial allocation
        equity[1] <- params$initial_cash
        debt[1] <- equity[1] * (params$target_leverage - 1)
        total_value <- equity[1] + debt[1]

        # Allocate initial capital according to target percentages
        growth_holdings[1] <- total_value * (growth_pct / 100)
        defensive_holdings[1] <- total_value * (defensive_pct / 100)
        total_holdings[1] <- total_value

        daily_margin_rate <- params$margin_rate / 100 / 360

        for (i in 2:n_days) {
            # Calculate returns for each asset
            growth_return <- as.numeric(prices_list$growth[i]) / as.numeric(prices_list$growth[i - 1]) - 1

            # Update growth holdings
            growth_holdings[i] <- growth_holdings[i - 1] * (1 + growth_return)

            # Update defensive holdings
            if (!is.null(defensive_ticker) && defensive_pct > 0) {
                defensive_return <- as.numeric(prices_list[[defensive_ticker]][i]) /
                    as.numeric(prices_list[[defensive_ticker]][i - 1]) - 1
                defensive_holdings[i] <- defensive_holdings[i - 1] * (1 + defensive_return)
            } else {
                defensive_holdings[i] <- 0
            }

            # Total holdings after market moves
            current_holdings <- growth_holdings[i] + defensive_holdings[i]

            # Accrue interest
            interest_cost <- debt[i - 1] * daily_margin_rate
            current_debt <- debt[i - 1] + interest_cost
            current_equity <- current_holdings - current_debt

            # Check for wipeout
            if (current_equity <= 0) {
                equity[i] <- 0
                debt[i] <- current_debt
                total_holdings[i] <- 0
                growth_holdings[i] <- 0
                defensive_holdings[i] <- 0
                next
            }

            # Rebalance with hysteresis
            current_lev <- current_holdings / current_equity
            upper_bound <- params$target_leverage + params$rebalance_threshold
            lower_bound <- params$target_leverage - params$rebalance_threshold

            if (current_lev > upper_bound) {
                # Add cash and buy both assets in target proportion
                needed_cash <- (current_holdings - params$target_leverage * current_equity) /
                    (params$target_leverage - 1)
                if (needed_cash > 0) {
                    cash_added[i] <- needed_cash
                    current_equity <- current_equity + needed_cash

                    # Buy both assets in target proportion
                    growth_buy <- needed_cash * (growth_pct / 100)
                    defensive_buy <- needed_cash * (defensive_pct / 100)

                    growth_holdings[i] <- growth_holdings[i] + growth_buy
                    defensive_holdings[i] <- defensive_holdings[i] + defensive_buy
                    current_holdings <- current_holdings + needed_cash
                }
            } else if (current_lev < lower_bound) {
                # Borrow and buy both assets in target proportion
                target_holdings <- current_equity * params$target_leverage
                buy_amount <- target_holdings - current_holdings
                if (buy_amount > 0) {
                    margin_buys[i] <- buy_amount

                    # Buy both assets in target proportion
                    growth_buy <- buy_amount * (growth_pct / 100)
                    defensive_buy <- buy_amount * (defensive_pct / 100)

                    growth_holdings[i] <- growth_holdings[i] + growth_buy
                    defensive_holdings[i] <- defensive_holdings[i] + defensive_buy
                    current_holdings <- current_holdings + buy_amount
                    current_debt <- current_debt + buy_amount
                }
            }

            equity[i] <- current_equity
            debt[i] <- current_debt
            total_holdings[i] <- current_holdings
        }

        # Return results
        results <- xts(data.frame(
            Equity = equity,
            Debt = debt,
            Cash_Added = cash_added,
            Margin_Buys = margin_buys,
            Total_Assets = total_holdings,
            Growth_Holdings = growth_holdings,
            Defensive_Holdings = defensive_holdings
        ), order.by = dates)

        return(results)
    }

    # Main optimization reactive
    optimization_results <- eventReactive(input$run, {
        req(input$growth_ticker, input$defensive_tickers)

        withProgress(message = "Running optimization...", {
            # Fetch price data
            incProgress(0.1, detail = "Fetching price data...")

            growth_data <- getSymbols(input$growth_ticker,
                src = "yahoo",
                from = input$start_date, auto.assign = FALSE
            )
            growth_prices <- Ad(growth_data)

            prices_list <- list(growth = growth_prices)

            for (ticker in input$defensive_tickers) {
                def_data <- getSymbols(ticker,
                    src = "yahoo",
                    from = input$start_date, auto.assign = FALSE
                )
                prices_list[[ticker]] <- Ad(def_data)
            }

            # Align all prices to common dates
            all_prices <- do.call(merge, prices_list)
            all_prices <- na.omit(all_prices)
            dates <- index(all_prices)

            # Update prices_list with aligned data
            for (i in seq_along(prices_list)) {
                prices_list[[i]] <- all_prices[, i]
            }

            # Parameters
            params <- list(
                initial_cash = input$initial_cash,
                target_leverage = input$target_leverage,
                margin_rate = input$margin_rate,
                rebalance_threshold = input$rebalance_threshold,
                mc_sims = input$mc_sims,
                mc_days = input$mc_days
            )

            # Generate allocation combinations
            defensive_allocations <- seq(input$defensive_range[1],
                input$defensive_range[2],
                by = input$step_size
            )

            results_list <- list()
            counter <- 1
            total_tests <- length(input$defensive_tickers) * length(defensive_allocations) + 1

            incProgress(0.2, detail = "Testing allocations...")

            # Test 100% growth (baseline)
            bt_result <- run_backtest(100, NULL, 0, prices_list, dates, params)

            # Calculate metrics
            total_invested <- params$initial_cash + sum(bt_result$Cash_Added)
            final_equity <- as.numeric(tail(bt_result$Equity, 1))
            equity_returns <- dailyReturn(bt_result$Equity)

            # Monte Carlo: Generate paths
            rets <- as.numeric(na.omit(equity_returns))
            mc_matrix <- matrix(0, nrow = params$mc_days, ncol = params$mc_sims)
            for (j in 1:params$mc_sims) {
                sampled_rets <- sample(rets, params$mc_days, replace = TRUE)
                mc_matrix[, j] <- cumprod(1 + sampled_rets)
            }

            # Monte Carlo: Calculate metrics for each path, then take percentiles
            mc_years <- params$mc_days / 252 # Trading days to years

            # Final returns for each simulation
            final_returns <- mc_matrix[nrow(mc_matrix), ] - 1

            # CAGR for each simulation path
            mc_cagrs <- ((1 + final_returns)^(1 / mc_years)) - 1

            # Max drawdown for each simulation path
            drawdowns <- apply(mc_matrix, 2, function(x) {
                max_val <- cummax(c(1, x))[-1]
                dd <- (x - max_val) / max_val
                min(dd) # Most negative drawdown
            })

            # Historical backtest CAGR
            cagr_years <- as.numeric(difftime(tail(dates, 1), dates[1], units = "days")) / 365.25
            cagr <- ((final_equity / total_invested)^(1 / cagr_years)) - 1

            # Calculate percentiles from Monte Carlo results
            p25_cagr <- quantile(mc_cagrs, 0.25)
            p25_return <- quantile(final_returns, 0.25)

            results_list[[counter]] <- list(
                defensive_asset = "None",
                defensive_pct = 0,
                growth_pct = 100,
                final_equity = final_equity,
                total_invested = total_invested,
                total_return_multiple = final_equity / total_invested,
                cagr = cagr,
                p25_return = p25_return,
                p25_cagr = p25_cagr,
                median_return = median(final_returns),
                median_cagr = median(mc_cagrs),
                p95_drawdown = quantile(drawdowns, 0.05), # 5th percentile = worst 5%
                max_cash_added = sum(bt_result$Cash_Added),
                backtest_result = bt_result,
                mc_matrix = mc_matrix
            )
            counter <- counter + 1

            # Test each defensive asset at different allocations
            for (def_ticker in input$defensive_tickers) {
                for (def_pct in defensive_allocations) {
                    if (def_pct == 0) next # Already tested

                    growth_pct <- 100 - def_pct

                    bt_result <- run_backtest(
                        growth_pct, def_ticker, def_pct,
                        prices_list, dates, params
                    )

                    total_invested <- params$initial_cash + sum(bt_result$Cash_Added)
                    final_equity <- as.numeric(tail(bt_result$Equity, 1))
                    equity_returns <- dailyReturn(bt_result$Equity)

                    # Monte Carlo: Generate paths
                    rets <- as.numeric(na.omit(equity_returns))
                    mc_matrix <- matrix(0, nrow = params$mc_days, ncol = params$mc_sims)
                    for (j in 1:params$mc_sims) {
                        sampled_rets <- sample(rets, params$mc_days, replace = TRUE)
                        mc_matrix[, j] <- cumprod(1 + sampled_rets)
                    }

                    # Monte Carlo: Calculate metrics for each path, then take percentiles
                    mc_years <- params$mc_days / 252 # Trading days to years

                    # Final returns for each simulation
                    final_returns <- mc_matrix[nrow(mc_matrix), ] - 1

                    # CAGR for each simulation path
                    mc_cagrs <- ((1 + final_returns)^(1 / mc_years)) - 1

                    # Max drawdown for each simulation path
                    drawdowns <- apply(mc_matrix, 2, function(x) {
                        max_val <- cummax(c(1, x))[-1]
                        dd <- (x - max_val) / max_val
                        min(dd) # Most negative drawdown
                    })

                    # Historical backtest CAGR
                    cagr <- ((final_equity / total_invested)^(1 / cagr_years)) - 1

                    # Calculate percentiles from Monte Carlo results
                    p25_cagr <- quantile(mc_cagrs, 0.25)
                    p25_return <- quantile(final_returns, 0.25)

                    results_list[[counter]] <- list(
                        defensive_asset = def_ticker,
                        defensive_pct = def_pct,
                        growth_pct = growth_pct,
                        final_equity = final_equity,
                        total_invested = total_invested,
                        total_return_multiple = final_equity / total_invested,
                        cagr = cagr,
                        p25_return = p25_return,
                        p25_cagr = p25_cagr,
                        median_return = median(final_returns),
                        median_cagr = median(mc_cagrs),
                        p95_drawdown = quantile(drawdowns, 0.05), # 5th percentile = worst 5%
                        max_cash_added = sum(bt_result$Cash_Added),
                        backtest_result = bt_result,
                        mc_matrix = mc_matrix
                    )
                    counter <- counter + 1

                    incProgress(0.7 / total_tests,
                        detail = sprintf("%s: %d%% defensive", def_ticker, def_pct)
                    )
                }
            }

            incProgress(1.0, detail = "Complete!")
            return(results_list)
        })
    })

    # Summary table of all results
    output$all_results_table <- renderDT({
        results <- optimization_results()

        df <- map_df(results, function(r) {
            data.frame(
                Portfolio = sprintf(
                    "%s: %d%% / %s: %d%%",
                    input$growth_ticker, r$growth_pct,
                    r$defensive_asset, r$defensive_pct
                ),
                Defensive_Asset = r$defensive_asset,
                Defensive_Pct = r$defensive_pct,
                CAGR = percent(r$cagr, accuracy = 0.1),
                P25_CAGR = percent(r$p25_cagr, accuracy = 0.1),
                P25_Return = percent(r$p25_return, accuracy = 0.1),
                Median_Return = percent(r$median_return, accuracy = 0.1),
                P95_Drawdown = percent(r$p95_drawdown, accuracy = 0.1),
                Risk_Adj_Score = round(r$p25_cagr / abs(r$p95_drawdown), 3),
                Final_Multiple = round(r$total_return_multiple, 2),
                Cash_Required = dollar(r$max_cash_added)
            )
        })

        datatable(df,
            options = list(pageLength = 25, scrollX = TRUE),
            rownames = FALSE
        )
    })

    # Best portfolios by different objectives
    output$best_portfolios_table <- renderDT({
        results <- optimization_results()

        # Find best by different criteria
        best_p25_cagr <- results[[which.max(map_dbl(results, ~ .$p25_cagr))]]
        best_dd <- results[[which.max(map_dbl(results, ~ .$p95_drawdown))]] # Least negative
        best_cagr <- results[[which.max(map_dbl(results, ~ .$cagr))]]

        # Risk-adjusted: P25 CAGR / abs(P95 drawdown)
        risk_adj_scores <- map_dbl(results, ~ .$p25_cagr / abs(.$p95_drawdown))
        best_risk_adj <- results[[which.max(risk_adj_scores)]]

        df <- data.frame(
            Objective = c(
                "Best 25th %ile CAGR", "Lowest 95th %ile Drawdown",
                "Best CAGR", "Best Risk-Adjusted (P25 CAGR / |P95 DD|)"
            ),
            Portfolio = c(
                sprintf(
                    "%s: %d%% / %s: %d%%", input$growth_ticker, best_p25_cagr$growth_pct,
                    best_p25_cagr$defensive_asset, best_p25_cagr$defensive_pct
                ),
                sprintf(
                    "%s: %d%% / %s: %d%%", input$growth_ticker, best_dd$growth_pct,
                    best_dd$defensive_asset, best_dd$defensive_pct
                ),
                sprintf(
                    "%s: %d%% / %s: %d%%", input$growth_ticker, best_cagr$growth_pct,
                    best_cagr$defensive_asset, best_cagr$defensive_pct
                ),
                sprintf(
                    "%s: %d%% / %s: %d%%", input$growth_ticker, best_risk_adj$growth_pct,
                    best_risk_adj$defensive_asset, best_risk_adj$defensive_pct
                )
            ),
            CAGR = percent(c(best_p25_cagr$cagr, best_dd$cagr, best_cagr$cagr, best_risk_adj$cagr),
                accuracy = 0.1
            ),
            P25_CAGR = percent(
                c(
                    best_p25_cagr$p25_cagr, best_dd$p25_cagr,
                    best_cagr$p25_cagr, best_risk_adj$p25_cagr
                ),
                accuracy = 0.1
            ),
            P95_Drawdown = percent(
                c(
                    best_p25_cagr$p95_drawdown, best_dd$p95_drawdown,
                    best_cagr$p95_drawdown, best_risk_adj$p95_drawdown
                ),
                accuracy = 0.1
            ),
            Risk_Adj_Score = round(c(
                best_p25_cagr$p25_cagr / abs(best_p25_cagr$p95_drawdown),
                best_dd$p25_cagr / abs(best_dd$p95_drawdown),
                best_cagr$p25_cagr / abs(best_cagr$p95_drawdown),
                best_risk_adj$p25_cagr / abs(best_risk_adj$p95_drawdown)
            ), 3)
        )

        datatable(df,
            options = list(dom = "t"), rownames = FALSE,
            selection = "single"
        )
    })

    # Efficient frontier plot
    output$efficient_frontier <- renderPlot({
        results <- optimization_results()

        df <- map_df(results, function(r) {
            data.frame(
                p25_return = r$p25_return * 100,
                p95_drawdown = abs(r$p95_drawdown) * 100,
                defensive_asset = r$defensive_asset,
                defensive_pct = r$defensive_pct,
                label = sprintf("%s %d%%", r$defensive_asset, r$defensive_pct)
            )
        })

        ggplot(df, aes(x = p95_drawdown, y = p25_return, color = defensive_asset)) +
            geom_point(size = 3) +
            geom_text(aes(label = paste0(defensive_pct, "%")),
                vjust = -0.5, size = 3, show.legend = FALSE
            ) +
            labs(
                title = "Risk-Return Tradeoff: 25th Percentile Return vs 95th Percentile Drawdown",
                x = "95th Percentile Drawdown (%, lower is better)",
                y = "25th Percentile Return (%)",
                color = "Defensive Asset"
            ) +
            theme_minimal() +
            theme(legend.position = "bottom")
    })

    # Selected portfolio details (from best portfolios table selection)
    selected_portfolio <- reactive({
        results <- optimization_results()
        req(input$best_portfolios_table_rows_selected)

        selected_row <- input$best_portfolios_table_rows_selected

        if (selected_row == 1) {
            # Best P25 CAGR
            return(results[[which.max(map_dbl(results, ~ .$p25_cagr))]])
        } else if (selected_row == 2) {
            # Best DD
            return(results[[which.max(map_dbl(results, ~ .$p95_drawdown))]])
        } else if (selected_row == 3) {
            # Best CAGR
            return(results[[which.max(map_dbl(results, ~ .$cagr))]])
        } else {
            # Best risk-adjusted
            risk_adj_scores <- map_dbl(results, ~ .$p25_cagr / abs(.$p95_drawdown))
            return(results[[which.max(risk_adj_scores)]])
        }
    })

    output$selected_metrics <- renderTable({
        portfolio <- selected_portfolio()

        data.frame(
            Metric = c(
                "Portfolio", "CAGR", "Final Multiple", "Total Cash Added",
                "25th %ile CAGR", "25th %ile Return", "Median Return", "95th %ile Drawdown",
                "Risk-Adjusted Score (P25 CAGR / |P95 DD|)"
            ),
            Value = c(
                sprintf(
                    "%s: %d%% / %s: %d%%", input$growth_ticker, portfolio$growth_pct,
                    portfolio$defensive_asset, portfolio$defensive_pct
                ),
                percent(portfolio$cagr, accuracy = 0.1),
                round(portfolio$total_return_multiple, 2),
                dollar(portfolio$max_cash_added),
                percent(portfolio$p25_cagr, accuracy = 0.1),
                percent(portfolio$p25_return, accuracy = 0.1),
                percent(portfolio$median_return, accuracy = 0.1),
                percent(portfolio$p95_drawdown, accuracy = 0.1),
                round(portfolio$p25_cagr / abs(portfolio$p95_drawdown), 3)
            )
        )
    })

    output$selected_equity_curve <- renderPlot({
        portfolio <- selected_portfolio()
        plot(portfolio$backtest_result$Equity,
            main = "Equity Curve (Log Scale)",
            col = "blue", lwd = 2, log = "y"
        )
    })

    output$selected_mc_plot <- renderPlot({
        portfolio <- selected_portfolio()
        sims <- portfolio$mc_matrix

        matplot(sims,
            type = "l", lty = 1, col = alpha("gray", 0.2),
            main = paste("Monte Carlo:", input$mc_days, "Trading Days"),
            ylab = "Portfolio Multiple", xlab = "Days"
        )

        lines(apply(sims, 1, quantile, probs = 0.5), col = "blue", lwd = 2)
        lines(apply(sims, 1, quantile, probs = 0.95), col = "green", lwd = 2)
        lines(apply(sims, 1, quantile, probs = 0.05), col = "red", lwd = 2)

        legend("topleft",
            legend = c("Median", "95th %", "5th %"),
            col = c("blue", "green", "red"), lwd = 2
        )
    })

    output$selected_mc_stats <- renderTable({
        portfolio <- selected_portfolio()

        data.frame(
            Metric = c("25th Percentile Return", "Median Return", "95th Percentile Drawdown"),
            Value = c(
                percent(portfolio$p25_return, accuracy = 0.1),
                percent(portfolio$median_return, accuracy = 0.1),
                percent(portfolio$p95_drawdown, accuracy = 0.1)
            )
        )
    })
}

# Run the App
shinyApp(ui = ui, server = server)
