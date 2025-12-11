library(shiny)
library(quantmod)
library(tidyverse)
library(PerformanceAnalytics)
library(scales)
library(DT)

# UI Definition
ui <- fluidPage(
  titlePanel("Constant Leverage Portfolio Backtester (Cash Injection Strategy)"),
  sidebarLayout(
    sidebarPanel(
      helpText("Backtest a constant leverage strategy with cash injections.
               • When market drops: Add cash and buy more to maintain leverage
               • When market rises: Borrow and buy more to maintain leverage"),
      textInput("ticker", "Asset Ticker", value = "QQQ"),
      dateInput("start_date", "Start Date", value = "2015-01-01"),
      numericInput("initial_cash", "Initial Capital ($)", value = 30000),
      sliderInput("target_leverage", "Target Leverage (e.g., 1.3x)",
        min = 1.0, max = 3.0, value = 1.3, step = 0.1
      ),
      numericInput("margin_rate", "Margin Interest Rate (Annual %)", value = 7.0),
      sliderInput("rebalance_threshold", "Rebalancing Threshold (±)",
        min = 0.0, max = 0.2, value = 0.05, step = 0.01
      ),
      helpText("Only rebalance when leverage deviates from target by more than this amount.
               Example: 1.3x target with 0.05 threshold = rebalance if leverage < 1.25 or > 1.35"),
      hr(),
      h4("Monte Carlo Settings"),
      numericInput("mc_sims", "Number of Simulations", value = 500),
      numericInput("mc_days", "Days to Simulate", value = 252),
      actionButton("run", "Run Analysis", class = "btn-primary")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Backtest Results",
          br(),
          h4("Performance Summary"),
          tableOutput("metrics_table"),
          hr(),
          plotOutput("equity_curve"),
          br(),
          h4("Cash Injections Required"),
          plotOutput("cash_plot"),
          textOutput("total_cash_text")
        ),
        tabPanel(
          "Trade Log",
          br(),
          h4("Detailed Trade History"),
          helpText("Shows all rebalancing events with portfolio state after each trade."),
          DTOutput("trade_log_table")
        ),
        tabPanel(
          "Monte Carlo Simulation",
          br(),
          h4("Projected Outcomes (Bootstrapped Returns)"),
          plotOutput("mc_plot"),
          br(),
          tableOutput("mc_stats")
        )
      )
    )
  )
)

# Server Logic
server <- function(input, output) {
  # Reactive block to fetch data and run backtest
  simulation_data <- eventReactive(input$run, {
    req(input$ticker)

    # 1. Fetch Data
    stock_data <- getSymbols(input$ticker,
      src = "yahoo",
      from = input$start_date, auto.assign = FALSE
    )
    prices <- Ad(stock_data) # Adjusted Close
    dates <- index(prices)

    # 2. Initialize Variables
    n_days <- length(prices)
    equity <- numeric(n_days)
    debt <- numeric(n_days)
    cash_added <- numeric(n_days) # Track external cash injections
    margin_buys <- numeric(n_days) # Track margin purchases (not interest)
    holdings_value <- numeric(n_days)

    # Initial State
    equity[1] <- input$initial_cash
    # Target Debt = Equity * (Leverage - 1)
    debt[1] <- equity[1] * (input$target_leverage - 1)
    holdings_value[1] <- equity[1] + debt[1]

    daily_margin_rate <- input$margin_rate / 100 / 360

    # 3. Daily Loop
    for (i in 2:n_days) {
      # Calculate change in asset value from previous day
      daily_return <- as.numeric(prices[i]) / as.numeric(prices[i - 1]) - 1

      # Step A: Update Holdings Value based on market move
      current_holdings <- holdings_value[i - 1] * (1 + daily_return)

      # Step B: Accrue Margin Interest
      interest_cost <- debt[i - 1] * daily_margin_rate

      # Pre-adjustment Equity (Assets - Debt - Interest)
      current_debt <- debt[i - 1] + interest_cost # Interest adds to debt if not paid
      current_equity <- current_holdings - current_debt

      # Step C: Rebalance Logic with Hysteresis
      # Only rebalance when leverage deviates beyond threshold
      # Target: Holdings / Equity = Target_Leverage
      # When leverage too high: Add cash and buy stock
      # When leverage too low: Borrow and buy stock

      # Check for portfolio wipeout
      if (current_equity <= 0) {
        equity[i] <- 0
        debt[i] <- current_debt
        holdings_value[i] <- 0
        next
      }

      current_lev <- current_holdings / current_equity

      # Calculate leverage bounds with hysteresis
      upper_bound <- input$target_leverage + input$rebalance_threshold
      lower_bound <- input$target_leverage - input$rebalance_threshold

      if (current_lev > upper_bound) {
        # LEVERAGE TOO HIGH (market dropped) -> Add external cash and buy stock
        # We want: (Holdings + Cash) / (Equity + Cash) = Target_Leverage
        # Solving: Cash = (Holdings - Target_Leverage * Equity) / (Target_Leverage - 1)

        needed_cash <- (current_holdings - input$target_leverage * current_equity) / (input$target_leverage - 1)

        if (needed_cash > 0) {
          cash_added[i] <- needed_cash
          current_equity <- current_equity + needed_cash
          current_holdings <- current_holdings + needed_cash # Buy stock with the cash
          # Debt stays the same
        }
      } else if (current_lev < lower_bound) {
        # LEVERAGE TOO LOW (market rose) -> Borrow and buy more
        target_holdings <- current_equity * input$target_leverage
        buy_amount <- target_holdings - current_holdings

        if (buy_amount > 0) {
          margin_buys[i] <- buy_amount # Track the margin purchase
          current_holdings <- current_holdings + buy_amount
          current_debt <- current_debt + buy_amount # Borrow to buy
        }
      }
      # If leverage is within bounds, do nothing (hysteresis)

      # Record States
      equity[i] <- current_equity
      debt[i] <- current_debt
      holdings_value[i] <- current_holdings
    }

    # Create Time Series
    results <- xts(data.frame(
      Equity = equity,
      Debt = debt,
      Cash_Added = cash_added,
      Margin_Buys = margin_buys,
      Total_Assets = holdings_value
    ), order.by = dates)

    # Benchmark: Buy & Hold with same cash injections for fair comparison
    bnh_shares <- input$initial_cash / as.numeric(prices[1])
    bnh_value <- numeric(n_days)
    bnh_value[1] <- input$initial_cash

    for (i in 2:n_days) {
      curr_val <- bnh_shares * as.numeric(prices[i])

      # If leveraged strategy added cash, BnH adds it too
      if (cash_added[i] > 0) {
        new_shares <- cash_added[i] / as.numeric(prices[i])
        bnh_shares <- bnh_shares + new_shares
        curr_val <- curr_val + cash_added[i]
      }
      bnh_value[i] <- curr_val
    }

    bnh_xts <- xts(bnh_value, order.by = dates)
    results <- merge(results, BnH = bnh_xts)

    # Calculate Returns for MC
    strat_daily_rets <- dailyReturn(results$Equity)

    list(ts = results, returns = strat_daily_rets)
  })

  # --- OUTPUTS ---

  output$metrics_table <- renderTable({
    data <- simulation_data()$ts

    # Total Cash Invested
    initial <- input$initial_cash
    added <- sum(data$Cash_Added)
    total_invested <- initial + added

    final_equity <- as.numeric(tail(data$Equity, 1))
    final_bnh <- as.numeric(tail(data$BnH, 1))

    df <- data.frame(
      Metric = c(
        "Initial Capital", "Total Cash Added", "Total Invested",
        "Final Strategy Equity", "Final Buy & Hold Equity",
        "Strategy Multiple", "BnH Multiple"
      ),
      Value = c(
        dollar(initial), dollar(added), dollar(total_invested),
        dollar(final_equity), dollar(final_bnh),
        round(final_equity / total_invested, 2),
        round(final_bnh / total_invested, 2)
      )
    )
    df
  })

  output$equity_curve <- renderPlot({
    data <- simulation_data()$ts
    plot_data <- data[, c("Equity", "BnH")]
    plot(plot_data,
      main = "Strategy vs Adjusted Buy & Hold (Log Scale)",
      col = c("blue", "black"), lwd = 2, legend.loc = "topleft", log = "y"
    )
  })

  output$cash_plot <- renderPlot({
    data <- simulation_data()$ts
    barplot(data$Cash_Added,
      main = "Daily Cash Injections Required",
      col = "red", border = NA, ylab = "Cash Amount ($)"
    )
  })

  output$total_cash_text <- renderText({
    data <- simulation_data()$ts
    paste("Total External Cash Required:", dollar(sum(data$Cash_Added)))
  })

  # --- TRADE LOG ---

  output$trade_log_table <- renderDT({
    data <- simulation_data()$ts

    # Create a data frame with all portfolio states
    trade_log <- data.frame(
      Date = index(data),
      Cash_Injection = as.numeric(data$Cash_Added),
      Margin_Buy = as.numeric(data$Margin_Buys),
      Equity = as.numeric(data$Equity),
      Debt = as.numeric(data$Debt),
      Holdings = as.numeric(data$Total_Assets)
    )

    # Calculate leverage ratio
    trade_log$Leverage <- trade_log$Holdings / trade_log$Equity

    # Filter to only show rows where a trade occurred
    trade_log <- trade_log %>%
      mutate(
        Action = case_when(
          Cash_Injection > 0.01 ~ "Cash Injection",
          Margin_Buy > 0.01 ~ "Margin Buy",
          TRUE ~ "No Trade"
        ),
        Trade_Amount = ifelse(Cash_Injection > 0, Cash_Injection, Margin_Buy)
      ) %>%
      filter(Action != "No Trade") %>%
      select(Date, Action, Trade_Amount, Equity, Debt, Holdings, Leverage) %>%
      mutate(
        Trade_Amount = dollar(Trade_Amount),
        Equity = dollar(Equity),
        Debt = dollar(Debt),
        Holdings = dollar(Holdings),
        Leverage = round(Leverage, 3)
      )

    datatable(
      trade_log,
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        order = list(list(0, "desc")) # Sort by date descending
      ),
      rownames = FALSE,
      colnames = c("Date", "Action", "Trade Amount", "NLV (Equity)", "Margin Loan", "Total Holdings", "Leverage Ratio")
    )
  })

  # --- MONTE CARLO ---

  mc_simulation <- reactive({
    data <- simulation_data()
    rets <- as.numeric(na.omit(data$returns))

    n_sims <- input$mc_sims
    n_days <- input$mc_days

    # Matrix to store cumulative returns
    sim_matrix <- matrix(0, nrow = n_days, ncol = n_sims)

    withProgress(message = "Running Monte Carlo...", {
      for (j in 1:n_sims) {
        # Sample with replacement
        sampled_rets <- sample(rets, n_days, replace = TRUE)
        # Cumulative return path
        sim_matrix[, j] <- cumprod(1 + sampled_rets)
      }
    })

    sim_matrix
  })

  output$mc_plot <- renderPlot({
    sims <- mc_simulation()

    # Calculate quantiles for the plot
    final_vals <- sims[nrow(sims), ]

    matplot(sims,
      type = "l", lty = 1, col = alpha("gray", 0.2),
      main = paste("Monte Carlo: ", input$mc_days, "Trading Days"),
      ylab = "Portfolio Multiple", xlab = "Days"
    )

    # Highlight specific percentiles
    lines(apply(sims, 1, quantile, probs = 0.5), col = "blue", lwd = 2) # Median
    lines(apply(sims, 1, quantile, probs = 0.95), col = "green", lwd = 2) # Upside
    lines(apply(sims, 1, quantile, probs = 0.05), col = "red", lwd = 2) # Downside

    legend("topleft",
      legend = c("Median", "95th %", "5th %"),
      col = c("blue", "green", "red"), lwd = 2
    )
  })

  output$mc_stats <- renderTable({
    sims <- mc_simulation()

    # Calculate Max Drawdown for each path
    # Function to get max DD of a vector
    get_mdd <- function(x) {
      max_val <- cummax(c(1, x))[-1] # Prepend 1 for base
      dd <- (x - max_val) / max_val
      min(dd)
    }

    final_returns <- sims[nrow(sims), ] - 1
    drawdowns <- apply(sims, 2, get_mdd)

    # 25th percentile Return (User request)
    p25_ret <- quantile(final_returns, 0.25)

    # 95th percentile Drawdown (User request - this usually means "The bad tail")
    # Note: A "95th percentile drawdown" usually implies the worst 5% of scenarios.
    # Since drawdowns are negative numbers, we want the 5th quantile (the deep negative one).
    p95_dd <- quantile(drawdowns, 0.05)

    data.frame(
      Metric = c("Median Return", "25th Percentile Return", "Worst Case Drawdown (5% Tail)"),
      Value = c(
        percent(median(final_returns)),
        percent(p25_ret),
        percent(p95_dd)
      )
    )
  })
}

# Run the App
shinyApp(ui = ui, server = server)
