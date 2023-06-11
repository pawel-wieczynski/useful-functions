calculate_VaR_historical <- function(portfolio_returns, alpha) {
  VaR = quantile(portfolio_returns, alpha, names = FALSE)
  indicator_ES = portfolio_returns < VaR
  exceeds = sum(indicator_ES)
  ES = sum(portfolio_returns * indicator_ES) / sum(indicator_ES)
  return(list(VaR = VaR, ES = ES, Exceeds_Realized = exceeds))
}

calculate_VaR_gaussian <- function(portfolio_returns, alpha) {
  mu = mean(portfolio_returns)
  sigma = sd(portfolio_returns)
  VaR = qnorm(alpha, mu, sigma)
  indicator_ES = portfolio_returns < VaR
  exceeds = sum(indicator_ES)
  ES = sum(portfolio_returns * indicator_ES) / sum(indicator_ES)
  return(list(VaR = VaR, ES = ES, Exceeds_Realized = exceeds))
}

calculate_VaR_t_student <- function(portfolio_returns, alpha) {
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
  return(list(VaR = VaR, ES = ES, Exceeds_Realized = exceeds))
}

calculate_VaR <- function(portfolio_returns, alpha = 0.05, method = "historical") {
  # Input validation
  if (!is.numeric(portfolio_returns)) {
    stop("portfolio_returns should be a numeric vector.")
  }
  
  if (length(portfolio_returns) < 2) {
    stop("portfolio_returns should contain at least two data points.")
  }
  
  if (!is.numeric(alpha) || alpha <= 0 || alpha >= 1) {
    stop("alpha should be a numeric value between 0 and 1.")
  }
  
  if (!is.character(method) || !method %in% c("historical", "gaussian", "t-student")) {
    stop("Invalid method. Choose 'historical', 'gaussian', or 't-student'.")
  }
  
  n = length(portfolio_returns)
  
  # Call the appropriate calculation function
  if (method == "historical") {
    results = calculate_VaR_historical(portfolio_returns, alpha)
  } else if (method == "gaussian") {
    results = calculate_VaR_gaussian(portfolio_returns, alpha)
  } else if (method == "t-student") {
    results = calculate_VaR_t_student(portfolio_returns, alpha)
  }
  
  # Add expected exceeds to results
  results$Exceeds_Expected = alpha * n
  
  # Return results
  return(results)
}

calculate_rolling_VaR <- function(portfolio_returns, alpha = 0.05, method = "historical", test_size = floor(0.8 * nrow(portfolio_returns))) {
  n = length(portfolio_returns)
  training_size = n - test_size
  
  if (training_size <= 0) {
    stop("Test size should be smaller than the number of observations.")
  }
  
  VaR = numeric(test_size)
  ES = numeric(test_size)
  
  for (i in 1:test_size) {
    training_set = portfolio_returns[(1 + i - 1):(training_size + i - 1)]
    
    # Call the calculate_VaR function with the current training set
    results = calculate_VaR(training_set, alpha, method)
    
    VaR[i] = results$VaR
    ES[i] = results$ES
  }
  
  # Return VaR and ES as a named list
  return(list(VaR = VaR, ES = ES))
}