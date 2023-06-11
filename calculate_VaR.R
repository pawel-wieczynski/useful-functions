library(MASS)

calculate_VaR <- function(portfolio_returns, alpha = 0.05, method = "historical") {
  
  # Input validation
  if (!is.numeric(portfolio_returns)) {
    stop("Portfolio_returns should be a numeric vector.")
  }
  
  if (length(portfolio_returns) < 2) {
    stop("Portfolio_returns should contain at least two data points.")
  }
  
  if (!is.numeric(alpha) || alpha <= 0 || alpha >= 1) {
    stop("Alpha should be a numeric value between 0 and 1.")
  }
  
  if (!is.character(method) || !method %in% c("historical", "gaussian", "t-student")) {
    stop("Invalid method. Choose 'historical', 'gaussian' or 't-student'.")
  }
  
  n = length(portfolio_returns)
  
  if (method == "historical") {
    # Historical simulation
    VaR = quantile(portfolio_returns, alpha, names = FALSE)
    indicator_ES = portfolio_returns < VaR
    exceeds = sum(indicator_ES)
    ES = sum(portfolio_returns * indicator_ES) / sum(indicator_ES)
    
  } else if (method == "gaussian") {
    # Parametrical (Gaussian)
    mu = mean(portfolio_returns)
    sigma = sd(portfolio_returns)
    VaR = qnorm(alpha, mu, sigma)
    indicator_ES = portfolio_returns < VaR
    exceeds = sum(indicator_ES)
    ES = sum(portfolio_returns * indicator_ES) / sum(indicator_ES)
    
  } else if (method == "t-student") {
    # Parametrical (t-Student)
    fit_result = tryCatch({
      fitdistr(portfolio_returns, 't')
    }, error = function(e) {
      stop("Error in fitting t distribution: ", e$message)
    })
    
    m = fit_result$estimate[1]
    s = fit_result$estimate[2]
    df = fit_result$estimate[3]
    VaR = m + s * qt(alpha, df)
    indicator_ES = portfolio_returns < VaR
    exceeds = sum(indicator_ES)
    ES = sum(portfolio_returns * indicator_ES) / sum(indicator_ES)
  }
  
  # Return results as a named list
  results = list(
    VaR = VaR,
    ES = ES,
    Exceeds_Expected = alpha * n,
    Exceeds_Realized = exceeds
  )
  
  return(results)
}