library(httr)
library(rvest)
library(readxl)
library(xml2)
library(dplyr)
library(lubridate)
library(stringr)
library(jsonlite)

kor_ticker <- read.csv('kor_ticker.csv', fileEncoding = "CP949")

bsns_year = '2024'
bsns_year1 = 2024
reprt_code = '11011'

ticker_list = kor_ticker %>% left_join(corp_list, by = c('종목코드' = 'stock')) %>% select('종목코드', '종목명', 'code')

fs_data_all = NULL
select_fs = list()

for(i in 1: nrow(ticker_list)){
  data_fs = c()
  name = ticker_list$code[i]
  
  tryCatch({
    url = paste0('https://opendart.fss.or.kr/api/fnlttSinglAcnt.json?crtfc_key=', dart_api_key,'&corp_code=',name , '&bsns_year=',bsns_year, '&reprt_code=', reprt_code, '&fs_div=CF5')
    
    fs_data_all = fromJSON(url)
    fs_data_all = fs_data_all[['list']]
    
    if(is.null(fs_data_all)){
      url = paste0('https://opendart.fss.or.kr/api/fnlttSinglAcnt.json?crtfc_key=', dart_api_key,'&corp_code=',name , '&bsns_year=',bsns_year, '&reprt_code=', reprt_code, '&fs_div=OF5')
      
      fs_data_all = fromJSON(url)
      fs_data_all = fs_data_all[['list']]      
    }
    yr_count = str_detect(colnames(fs_data_all), 'trm_amount') %>% sum()
    yr_name = seq(bsns_year1, (bsns_year1 - yr_count + 1))
    
    fs_data_all = fs_data_all[, c('corp_code', 'sj_nm', 'account_nm')] %>% cbind(fs_data_all[, str_which(colnames(fs_data_all), 'trm_amount')])
    
    colnames(fs_data_all)[str_which(colnames(fs_data_all), 'amount')] = yr_name
    
    select_fs[[i]] = fs_data_all
  }, error = function(e){
    data_fs <- NA
    warning(paste0('error in ticker: ', name))
  })
  Sys.sleep(1)
  print(i)
}
data <- fs_data_all[ ,3] %>% unique()
select_final = list()
select_final_fs_list = list()
select_fs <- select_fs[which(!sapply(select_fs, is.null))]


for(j in 1 : 15){
  data_1 <- data[j]
  for(i in 1 : length(which(!sapply(select_fs, is.null)))){
    df <- select_fs[[i]]
    df <- df[!duplicated(df$account_nm), ]
    index <- which(df[ ,3] == data_1)
    code_index <- which(ticker_list[ , 3] == df[1, 1])
    if(!is.null(index)){
      select_final[[i]] <- df[index, c(4,5,6)] %>% mutate(code = ticker_list[code_index, 1])
    }
  }
  select_final_fs <- bind_rows(select_final)
  select_final_fs <- select_final_fs[ ,c("code", "2022", "2023", "2024")]
  select_final_fs_list[[j]] <- select_final_fs
}

names(select_final_fs_list) = data
select_final_fs_list

saveRDS(select_final_fs_list, 'kor_fs.Rds')
