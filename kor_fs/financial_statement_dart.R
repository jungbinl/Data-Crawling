# --- Load Required Libraries ---
library(httr)        # Handle HTTP requests (GET, POST, API calls)
library(rvest)       # Web scraping and HTML parsing
library(readxl)      # Import Excel files
library(xml2)        # Parse and work with XML data
library(dplyr)       # Data manipulation and transformation
library(lubridate)   # Date and time handling
library(stringr)     # String manipulation and pattern matching
library(jsonlite)    # Parse and generate JSON data

# --- Load ticker list (Korean stock codes) ---
kor_ticker <- read.csv('kor_ticker.csv', fileEncoding = "CP949")

# --- Define parameters for DART API ---
bsns_year = '2024'        # Business year (string)
bsns_year1 = 2024         # Business year (numeric)
reprt_code = '11011'      # Report type (11011 = Annual Report)

# --- Set up API key (stored in environment variable for security) ---
dart_api_key = 'f3e32e82349994968136c8d57391f97219d705ef'   # Hard-coded key (not recommended for production)
dart_api = Sys.getenv("dart_api_key")                       # Secure way using environment variable

# --- Download and unzip company code list from DART ---
codezip_url <- paste0('https://opendart.fss.or.kr/api/corpCode.xml?crtfc_key=', dart_api)
codezip_data <- GET(codezip_url)
codezip_data$headers[["content-disposition"]]  # Check file download status

# Save the downloaded file temporarily
tf <- tempfile(fileext = '.zip')
writeBin(content(codezip_data, as = "raw"), file.path(tf))

# Unzip XML file
nm <- unzip(tf, list = T)
code_data <- read_xml(unzip(tf, nm$Name))

# --- Extract corporate information from XML ---
corp_code <- code_data %>% html_nodes('corp_code') %>% html_text()
corp_name <- code_data %>% html_nodes('corp_name') %>% html_text()
corp_stock <- code_data %>% html_nodes('stock_code') %>% html_text()

corp_list <- data.frame(
  'code' = corp_code,
  'name' = corp_name,
  'stock' = corp_stock,
  stringsAsFactors = F
)

# --- Match ticker list with corporate codes ---
ticker_list = kor_ticker %>% 
  left_join(corp_list, by = c('종목코드' = 'stock')) %>% 
  select('종목코드', '종목명', 'code')

# --- Initialize financial statements container ---
fs_data_all = NULL
select_fs = list()

# --- Loop through each company and request financial statement data ---
for(i in 1: nrow(ticker_list)){
  data_fs = c()
  name = ticker_list$code[i]
  
  tryCatch({
    # Try consolidated financial statements (CF)
    url = paste0('https://opendart.fss.or.kr/api/fnlttSinglAcntAll.json?crtfc_key=', 
                 dart_api_key,'&corp_code=',name , '&bsns_year=',bsns_year, 
                 '&reprt_code=', reprt_code, '&fs_div=CF5')
    
    fs_data_all = fromJSON(url)
    fs_data_all = fs_data_all[['list']]
    
    # If not available, try individual financial statements (OF)
    if(is.null(fs_data_all)){
      url = paste0('https://opendart.fss.or.kr/api/fnlttSinglAcnt.json?crtfc_key=', 
                   dart_api_key,'&corp_code=',name , '&bsns_year=',bsns_year, 
                   '&reprt_code=', reprt_code, '&fs_div=OF5')
      
      fs_data_all = fromJSON(url)
      fs_data_all = fs_data_all[['list']]      
    }
    
    # --- Extract year columns dynamically ---
    yr_count = str_detect(colnames(fs_data_all), 'trm_amount') %>% sum()
    yr_name = seq(bsns_year1, (bsns_year1 - yr_count + 1))
    
    fs_data_all = fs_data_all[, c('corp_code', 'sj_nm', 'account_nm')] %>% 
      cbind(fs_data_all[, str_which(colnames(fs_data_all), 'trm_amount')])
    
    colnames(fs_data_all)[str_which(colnames(fs_data_all), 'amount')] = yr_name
    
    select_fs[[i]] = fs_data_all
    
  }, error = function(e){
    data_fs <- NA
    warning(paste0('error in ticker: ', name))  # Log error if API call fails
  })
  
  Sys.sleep(1)  # Avoid hitting API rate limits
  print(i)
}

# --- Post-processing financial statement data ---
length(select_fs)
data <-select_fs[[1]]$account_nm %>% unique()
select_final = list()
select_final_fs_list = list()
select_fs <- select_fs[which(!sapply(select_fs, is.null))]

# --- Organize final financial statement data per account ---
for(j in 1 : length(data)){
  data_1 <- data[j]
  for(i in 1 : length(select_fs)){
    df <- select_fs[[i]]
    df <- df[!duplicated(df$account_nm), ]
    index <- which(df[ ,3] == data_1)
    code_index <- which(ticker_list[ , 3] == df[1, 1])
    if(!is.null(index)){
      select_final[[i]] <- df[index, ] %>% mutate(code = ticker_list[code_index, 1])
    }
  }
  select_final_fs <- bind_rows(select_final)
  select_final_fs <- select_final_fs[ ,c("code", "2022", "2023", "2024")]
  select_final_fs_list[[j]] <- select_final_fs
}

# --- Assign account names to list and clean missing values ---
names(select_final_fs_list) = data

select_final_fs_list <- lapply(select_final_fs_list, function(x) {
  x[x == ""] <- 0
  return(x)
})

select_final_fs_list <- lapply(select_final_fs_list, function(x) {
  x[is.na(x)] <- 0
  return(x)
})

# --- Save results to RDS file ---
saveRDS(select_final_fs_list, 'kor_fs.Rds')
