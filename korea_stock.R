library(stringr)
library(dplyr)
library(httr)
library(readxl)
library(rvest)
library(readr)

# loading raw data

kor_ticker <- read.csv("kor_ticker.csv", fileEncoding = "CP949")

# Collecting Korea stock price data, make a each file

for(i in 1 : nrow(kor_ticker)){
  price <- xts(NA, order.by = Sys.Date())
  name <- kor_ticker$종목코드[i]
  from <- (Sys.Date() - years(3)) %>% str_remove_all('-')
  to <- Sys.Date() %>% str_remove_all('-')
  tryCatch({
    url = paste0('https://m.stock.naver.com/front-api/external/chart/domestic/info?symbol=', name, '&requestType=1&startTime=', from, '&endTime=', to, '&timeframe=day')
    
    data <- GET(url)
    data_html <- data %>% read_html %>% html_text() %>% read_csv()
    
    price <- data_html %>% select(c(1, 5)) %>% na.omit()
    colnames(price) <- (c('Date', 'Price'))
    price <- price %>% mutate(Date = parse_number(Date)) %>% mutate(Date = ymd(Date))
  }, error = function(e) {
    warning(paste0('Error in Ticker:', name))
  })
  file_path <- paste0("C:/Users/이정빈/Desktop/practice_r/korea_stock_data/", name, "_price.csv")
  
  write.csv(data.frame(price), file = file_path, row.names = FALSE, fileEncoding = "UTF-8")
  
  Sys.sleep(2)
}

# Collecting Korea stock price data, only one file

all_prices = NULL

for(i in 1 : nrow(kor_ticker)){
  price <- xts(NA, order.by = Sys.Date())
  name <- kor_ticker$종목코드[i]
  from <- (Sys.Date() - years(3)) %>% str_remove_all('-')
  to <- Sys.Date() %>% str_remove_all('-')
  tryCatch({
    url = paste0('https://m.stock.naver.com/front-api/external/chart/domestic/info?symbol=', name, '&requestType=1&startTime=', from, '&endTime=', to, '&timeframe=day')
    
    data <- GET(url)
    data_html <- data %>% read_html %>% html_text() %>% read_csv()
    
    price <- data_html %>% select(c(1, 5)) %>% na.omit()
    colnames(price) <- (c('Date', name))
    price <- price %>% mutate(Date = parse_number(Date)) %>% mutate(Date = ymd(Date))
    
    if (is.null(all_prices)) {
      all_prices <- price
    } else {
      all_prices <- all_prices %>% full_join(price, by = "Date")
    }
  }, error = function(e) {
    warning(paste0('Error in Ticker:', name))
  })
  
  Sys.sleep(2)
}

file_path <- paste0("C:/Users/이정빈/Desktop/practice_r/korea_stock_data/", "final", "_price.csv")

write.csv(data.frame(all_prices), file = file_path, row.names = FALSE, fileEncoding = "UTF-8")