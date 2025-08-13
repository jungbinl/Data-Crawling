library(stringr)
library(dplyr)
library(httr)
library(readxl)
library(rvest)
library(readr)

# loading raw data

kor_ticker <- read.csv("kor_ticker.csv", fileEncoding = "CP949")

# bring PBR, PER, PCR, PSR, Loading financial statements, TEST
for(i in 1 : 510){
  data_fs = c()
  data_value = c()
  name = kor_ticker$종목코드[i]
  tryCatch({
    url = paste0('https://comp.fnguide.com/SVO2/ASP/SVD_Finance.asp?pGB=1&gicode=A',name, '&cID=&MenuYn=Y&ReportGB=&NewMenuID=103&stkGb=701')
    
    data <- GET(url, user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0.0.0 Safari/537.36"))
    
    data <- data %>% read_html() %>% html_table()
    
    data_IS <- data[[1]]
    data_BS <- data[[3]]
    data_CF <- data[[5]]
    
    data_IS <- data_IS %>% select(-전년동기, -`전년동기(%)`)
    data_fs <- rbind(data_IS, data_BS, data_CF)
    data_fs <- data_fs %>% filter(!str_detect(`IFRS(연결)`, "펼치기"))
    data_fs <- data_fs %>% distinct(`IFRS(연결)`, .keep_all = TRUE)
    data_fs <- as.data.frame(data_fs)
    rownames(data_fs) = NULL
    rownames(data_fs) = data_fs[ ,1]
    data_fs[ ,1] = NULL
    data_fs <- data_fs[, substr(colnames(data_fs), 6,7) == '12']
    
    data_fs <- data_fs %>%
      mutate(across(everything(), ~ as.numeric(str_replace_all(., ",", "")))) %>% data.frame(., row.names = rownames(data_fs))
    
    rows_order <- c('지배주주순이익', '자본', '영업활동으로인한현금흐름', '매출액')
    
    value_index <- data_fs[match(rows_order, rownames(data_fs)), ncol(data_fs)]
    
    url = paste0('https://comp.fnguide.com/SVO2/ASP/SVD_Main.asp?pGB=1&gicode=A', name, '&cID=&MenuYn=Y&ReportGB=&NewMenuID=101&stkGb=701')
    
    data <- GET(url, user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0.0.0 Safari/537.36"))
    
    price <- read_html(data) %>% html_node(xpath = '//*[@id="svdMainChartTxt11"]') %>% html_text() %>% parse_number()
    
    share <- read_html(data) %>% html_node(xpath = '//*[@id="svdMainGrid1"]/table/tbody/tr[7]/td[1]') %>% html_text() %>% parse_number()
    
    data_value <- price / (value_index * 100000000/share)
    names(data_value) <- c('per','pbr','pcr','psr')
  }, error = function(e) {
    data_fs <- NA
    data_value <- NA
    warning(paste0('Error in Ticker: ', name))
  })
  
  write.csv(data_fs,  file = paste0("C:/Users/이정빈/Desktop/practice_r/korea_stock_info/", name, "_fs.csv"), fileEncoding = "CP949")
  
  write.csv(data_value, file = paste0("C:/Users/이정빈/Desktop/practice_r/korea_stock_info/", name, "_value.csv"), fileEncoding = "CP949")
  
}

# bring PBR, PER, PCR, PSR, Loading financial statements and gather

select_fs = list()
  
for(i in 1: nrow(kor_ticker)){
  data_fs = c()
  data_value = c()
  name = kor_ticker$종목코드[i]
  tryCatch({
    url = paste0('https://comp.fnguide.com/SVO2/ASP/SVD_Finance.asp?pGB=1&gicode=A',name, '&cID=&MenuYn=Y&ReportGB=&NewMenuID=103&stkGb=701')
    
    data <- GET(url, user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0.0.0 Safari/537.36"))
    
    data <- data %>% read_html() %>% html_table()
    
    data_IS <- data[[1]]
    data_BS <- data[[3]]
    data_CF <- data[[5]]
    
    data_IS <- data_IS %>% select(-전년동기, -`전년동기(%)`)
    data_fs <- rbind(data_IS, data_BS, data_CF)
    data_fs <- data_fs %>% filter(!str_detect(`IFRS(연결)`, "펼치기"))
    data_fs <- data_fs %>% distinct(`IFRS(연결)`, .keep_all = TRUE)
    data_fs <- as.data.frame(data_fs)
    rownames(data_fs) = NULL
    rownames(data_fs) <- data_fs[ , 1]
    data_fs[, 1] = NULL
    data_fs <- data_fs[, substr(colnames(data_fs), 6,7) == '12']
    
    data_fs <- data_fs %>%
      mutate(across(everything(), ~ as.numeric(str_replace_all(., ",", "")))) %>% data.frame(., row.names = rownames(data_fs))
    
    rows_order <- c('지배주주순이익', '자본', '영업활동으로인한현금흐름', '매출액')
    
    value_index <- data_fs[match(rows_order, rownames(data_fs)), ncol(data_fs)]
    
    url = paste0('https://comp.fnguide.com/SVO2/ASP/SVD_Main.asp?pGB=1&gicode=A', name, '&cID=&MenuYn=Y&ReportGB=&NewMenuID=101&stkGb=701')
    
    data <- GET(url, user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0.0.0 Safari/537.36"))
    
    price <- read_html(data) %>% html_node(xpath = '//*[@id="svdMainChartTxt11"]') %>% html_text() %>% parse_number()
    
    share <- read_html(data) %>% html_node(xpath = '//*[@id="svdMainGrid1"]/table/tbody/tr[7]/td[1]') %>% html_text() %>% parse_number()
    
    data_value <- price / (value_index * 100000000/share)
    names(data_value) <- c('per','pbr','pcr','psr')
    
    select_fs[[i]] = data_value
  }, error = function(e) {
    data_fs <- NA
    warning(paste0('Error in Ticker: ', name))
  })
  Sys.sleep(1)
  print(i)
}

select_fs_final <- bind_rows(select_fs) %>% mutate('code' = kor_ticker[ ,5])

write.csv(select_fs_final, file = paste0("C:/Users/이정빈/Desktop/practice_r/", "kor_fs.csv"), fileEncoding = "CP949")



