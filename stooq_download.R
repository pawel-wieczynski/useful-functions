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
#' stooq_download('cdr', 20100101, 20211231)
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
