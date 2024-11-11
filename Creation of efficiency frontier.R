# Loading the necessary libraries for Markowitz analysis
library(quantmod)
library(PerformanceAnalytics)
library(PortfolioAnalytics)
library(fPortfolio)
library(timeSeries)

# Assigning stock tickers to the variable 'symbols'
symbols=c("JSWSTEEL.NS","BAJAJFINSV.NS","ASIANPAINT.NS","SBILIFE.NS","COALINDIA.NS","HCLTECH.NS","BAJFINANCE.NS","BRITANNIA.NS","GRASIM.NS","TITAN.NS")

# Extracting share price data for all tickers
ten_stocks <- lapply(symbols,function(X){
  getSymbols(X,from="2021-08-01",to="2024-07-31",auto.assign=FALSE)
})

ten_stocks_df <- as.data.frame(ten_stocks)
ten_stocks_df <- Ad(ten_stocks_df)

# Calculating the discrete returns for the stock prices
ten_stocks_return <- Return.calculate(ten_stocks_df, method = "discrete")

# Omiting any NA values resulting from return calculation
ten_stocks_return <- na.omit(ten_stocks_return)

# Calculating the portfolio returns with equal weights assigned to each stock
ten_stocks_return_pf <- Return.portfolio(ten_stocks_return,weights = c(0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1),geometric = FALSE)

# Calculating the mean return, covariance matrix and standard deviation(risk) of the portfolio returns
mean(ten_stocks_return_pf)
cov(ten_stocks_return)
sd(ten_stocks_return_pf)

# Converting the returns data to a timeSeries object for portfolio optimization
ten_stocks_return <- as.timeSeries(ten_stocks_return)

# Generating and plotting the efficient frontier with a risk-free rate of 7% per annum
efficient_frontier <- portfolioFrontier(ten_stocks_return,`setRiskFreeRate<-`(portfolioSpec(),.07/252),constraints = "longOnly")

# Plot the efficient frontier along with various portfolio metrics
plot(efficient_frontier, c(1,2,3,5,7,8))

# Determining the minimum variance portfolio and the respective asset weights
min_variance_portfolio <- minvariancePortfolio(ten_stocks_return, portfolioSpec(), constraints = "longOnly")
weights_portfolio <- getWeights(min_variance_portfolio)

# Determining the tangency portfolio (optimal portfolio) with the highest Sharpe ratio and the respective asset weights
Optimum_portfolio <- tangencyPortfolio(ten_stocks_return, `setRiskFreeRate<-`(portfolioSpec(),.07/252),constraints="longOnly")
weights_optimum_portfolio <- getWeights(Optimum_portfolio)

# Calculating the mean return and standard deviation (risk) of the portfolio with the highest sharpe ratio 
max_sharpe_ratio_pf <- Return.portfolio(ten_stocks_return,weights = weights_optimum_portfolio,geometric = FALSE)
mean(max_sharpe_ratio_pf)
sd(max_sharpe_ratio_pf)


