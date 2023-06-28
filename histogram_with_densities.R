if (!require('pacman')) install.packages('pacman')
pacman::p_load(ggplot2, MASS)

histogram_with_densities = function(data, col, title = 'Histogram with fitted densities', bins = 50) {
  
  data[['col_scaled']] = scale(data[[col]])
  fit = suppressWarnings(MASS::fitdistr(data[['col_scaled']], 't'))
  df = fit$estimate['df']
  
  ggplot(data, aes_string('col_scaled')) +
  
    geom_histogram(
      aes(y = ..density..)
      , bins = bins
      , color = 'white'
      , fill = 'lightgray') +
    
    geom_density(size = 1, aes(color = 'Empirical')) +
    
    stat_function(
      fun = dnorm
      , aes(color = 'Normal')
      , size = 1
      , args = list(mean = 0, sd = 1)
    ) +
    
    stat_function(
      fun = dt
      , aes(color = 'Student\'s t')
      , size = 1
      , args = list(df = df)
    ) +
    
    theme_bw() +
    theme(legend.position = 'bottom') +
    labs(
      x = '', y = 'Density', color = 'Distribution'
      , title = title
    ) -> p
  
  print(p)
  
}

# data = data.frame(x = rnorm(1000) + rexp(1000))
# histogram_with_densities(data, 'x')
