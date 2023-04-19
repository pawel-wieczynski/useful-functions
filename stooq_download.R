#' Stooq Download Function
#'
#' Download financial time series from https://stooq.pl/ in .csv format.
#' @param symbol Asset ticker from https://stooq.pl/
#' @param start_date Format 'yyyymmdd'.
#' @param end_date Format 'yyyymmdd'.
#' @param interval Default set to 'd'. Other intervals available are 'w', 'm', 'q', 'y'.
#' @param destination Destination folder to save .csv file. Deafault is your current working directory.
#'
#' @examples
#' stooq_download('wig20', 20100101, 20211231)
#' @examples
#' stooq_download('wig20', 20100101, 20211231, 'w', 'C:\\Windows\\Temp\\')

stooq_download = function(symbol, start_date, end_date, interval = 'd', destination){
  
  url = paste0(
    'https://stooq.pl/q/d/l/?s=', symbol
    ,'&d1=', start_date
    ,'&d2=', end_date
    ,'&i=', interval
  )
  
  if (missing(destination)){
    download.file(url = url, destfile = paste0(symbol, '.csv'))
  } else{
    download.file(url = url, destfile = paste0(destination, symbol, '.csv'))
  }
  
}

if(!suppressWarnings(suppressMessages(require('dplyr')))) install.packages('dplyr')
if(!suppressWarnings(suppressMessages(require('lubridate')))) install.packages('lubridate')

#' Get Close Prices Function
#'
#' Download data from https://stooq.pl/ and create data.frame of close prices.
#' @param symbols Asset tickers from https://stooq.pl/
#' @param start_date Format 'yyyymmdd'.
#' @param end_date Format 'yyyymmdd'.
#' @param interval Default set to 'd'. Other intervals available are 'w', 'm', 'q', 'y'.
#' @param destination Destination folder to save .csv file. Deafault is your current working directory.
#'
#' @examples
#' close_df = get_close_prices(c('wig20', 'mwig40', 'swig80'), 20220101, 20221231)

get_close_prices = function(symbols, start_date, end_date, interval = 'd', destination) {
  
  out_df = data.frame(Data = seq(ymd(start_date), ymd(end_date), by = 1))
  
  for (i in seq_along(symbols)) {
    
    if (missing(destination)) destination = getwd()
    
    stooq_download(symbols[i], start_date, end_date, interval, destination)
    
    temp = read.csv(paste0(destination, symbols[i], '.csv')) %>%
      select(Data, Zamkniecie) %>%
      mutate(Data = as.Date(Data))
    
    out_df = out_df %>%
      left_join(temp, by = 'Data')
    
    file.remove(paste0(destination, symbols[i], '.csv'))
    
  }
  
  out_df = out_df[complete.cases(out_df[, 2:ncol(out_df)]), ]
  
  colnames(out_df) = c('Data', symbols)
  
  return(out_df)
  
}
