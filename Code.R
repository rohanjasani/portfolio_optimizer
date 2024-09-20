# Load required libraries
libraries <- c("tidyverse", "tidyquant", "furrr", "plotly")

for (lib in libraries) {
    if (!require(lib, character.only = TRUE)) {
        install.packages(lib, dependencies = TRUE)
        library(lib, character.only = TRUE)
    }
}

# Import stock price data
tq_get("AAPL", from = "2018-01-01", to = "2018-12-31")

# Set date range for returns calculation
end <- "2018-12-31" %>% ymd()
start <- end - years(5) + days(1)

# Retrieve monthly returns for stock components
returns_m_components_tbl <- c("AAPL", "GOOG", "NFLX") %>%
    tq_get(get  = "stock.prices", from = start, to = end) %>%
    group_by(symbol) %>%
    tq_transmute(select = adjusted, mutate_fun = periodReturn, period = "monthly") %>%
    ungroup()

# Retrieve monthly returns for benchmark (XLK)
returns_m_benchmark_tbl <- "XLK" %>%
    tq_get(get  = "stock.prices", from = start, to = end) %>%
    tq_transmute(select = adjusted, mutate_fun = periodReturn, period = "monthly") %>%
    add_column(symbol = "XLK", .before = 1)

# Analyze performance using Sharpe Ratio for stocks and benchmark
returns_m_tbl <- returns_m_components_tbl %>%
    bind_rows(returns_m_benchmark_tbl)

returns_m_tbl %>%
    group_by(symbol) %>%
    tq_performance(Ra = monthly.returns, performance_fun = SharpeRatio.annualized, scale = 12, Rf = 0) %>%
    ungroup() %>%
    ggplot(aes(symbol, `AnnualizedSharpeRatio(Rf=0%)`)) +
    geom_col(fill = "#2c3e50") +
    geom_text(aes(label = `AnnualizedSharpeRatio(Rf=0%)` %>% round(2)), color = "white", nudge_y = -0.05) +
    theme_tq() +
    labs(title = "Sharpe Ratio: Stocks vs Benchmark", x = "", y = "Annualized Sharpe Ratio (Rf = 0%)")

# Create portfolio with custom weights and calculate portfolio returns
wts_tbl <- returns_m_components_tbl %>%
    distinct(symbol) %>%
    mutate(weights = c(0.25, 0.25, 0.5))

returns_m_portfolio_tbl <- returns_m_components_tbl %>%
    tq_portfolio(symbol, monthly.returns, weights = wts_tbl, rebalance_on = "quarters", col_rename = "monthly.returns")

# Add portfolio to the benchmark and analyze performance
returns_m_portfolio_merged_m_tbl <- returns_m_portfolio_tbl %>%
    add_column(symbol = "Portfolio", .before = 1) %>%
    bind_rows(returns_m_benchmark_tbl)

returns_m_portfolio_merged_m_tbl %>%
    group_by(symbol) %>%
    tq_performance(Ra = monthly.returns, performance_fun = SharpeRatio.annualized, scale = 12)

# Define a function to generate random portfolio weights
weight_iterator <- function(assets, iter = 100, seed = NULL) {
    n <- length(assets)
    if (!is.null(seed)) set.seed(seed)
    mtx <- matrix(runif(n = iter*n, min = 0, max = 1), nrow = 3)
    mtx_normalized <- mtx %*% diag(1/colSums(mtx))
    return(as.vector(mtx_normalized))
}

# Generate random portfolios and weights
assets <- c("AAPL", "GOOG", "NFLX")
iter  <- 250

weights_tbl <- tibble(
    portfolio_id = rep(1:iter, each = length(assets)),
    symbol       = rep(assets, times = iter),
    weights      = weight_iterator(assets, iter = iter, seed = 123)
) %>%
    group_by(portfolio_id)

# Calculate portfolio performance using random weights
plan("multisession")
portfolio_optim_tbl <- weights_tbl %>%
    nest() %>%
    rename(portfolio_weights = data) %>%
    mutate(portfolio_agg = future_map(portfolio_weights, ~ tq_portfolio(
        data = returns_m_components_tbl,
        assets_col = symbol, 
        returns_col = monthly.returns,
        weights = .x,
        rebalance_on = "quarters"
    ))) %>%
    mutate(sharp_ratio = map(portfolio_agg, ~ tq_performance(
        data = .x,
        Ra = portfolio.returns,
        performance_fun = SharpeRatio.annualized,
        scale = 12
    )))

# Identify the best portfolio based on Sharpe Ratio
best_portfolio_tbl <- portfolio_optim_tbl %>%
    unnest(sharp_ratio) %>%
    filter(`AnnualizedSharpeRatio(Rf=0%)` == max(`AnnualizedSharpeRatio(Rf=0%)`))

best_portfolio_tbl %>%
    select(portfolio_id, `AnnualizedSharpeRatio(Rf=0%)`)

best_portfolio_tbl %>%
    pull(portfolio_weights) %>%
    pluck(1)

# Create 3D plot to visualize the performance of all portfolios
portfolio_optim_flattened_tbl <- portfolio_optim_tbl %>%
    select(-portfolio_agg) %>%
    unnest(sharp_ratio) %>%
    unnest(portfolio_weights) %>%
    spread(symbol, weights) %>%
    rename(SharpeRatio = `AnnualizedSharpeRatio(Rf=0%)`)

p <- portfolio_optim_flattened_tbl %>%
    plot_ly(x = ~AAPL, y = ~GOOG, z = ~NFLX, color = ~SharpeRatio, size = ~SharpeRatio, sizes = c(1, 30),
            marker = list(symbol = 'circle', sizemode = 'diameter'),
            text = ~str_glue("Sharpe Ratio: {SharpeRatio %>% round(3)} AAPL: {AAPL %>% scales::percent()} GOOG: {GOOG %>% scales::percent()} NFLX: {NFLX %>% scales::percent()}"))

p
