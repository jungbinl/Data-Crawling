# --- Load Required Libraries ---
library(stringr)   # String manipulation (pattern matching, replacements)
library(dplyr)     # Data manipulation and wrangling
library(httr)      # Send HTTP requests (GET, POST)
library(readxl)    # Import Excel files
library(rvest)     # Web scraping (HTML parsing and table extraction)
library(readr)     # CSV reading/writing functions

# --- Load Ticker List (Korean stocks) ---
kor_ticker <- read.csv('kor_ticker.csv', fileEncoding = "CP949")

# --- Define Parameters ---
bsns_year = '2024'         # Business year as string
bsns_year1 = 2024          # Business year as numeric
reprt_code = '11011'       # Report type code

# --- DART API Setup ---
dart_api_key = 'f3e32e82349994968136c8d57391f97219d705ef'   # (Hard-coded for demo; use Sys.getenv in production)
dart_api = Sys.getenv("dart_api_key")

# --- Download and Extract Company Codes from DART ---
codezip_url <- paste0('https://opendart.fss.or.kr/api/corpCode.xml?crtfc_key=', dart_api)
codezip_data <- GET(codezip_url)
codezip_data$headers[["content-disposition"]]

# Save file temporarily and unzip
tf <- tempfile(fileext = '.zip')
writeBin(content(codezip_data, as = "raw"), file.path(tf))
nm <- unzip(tf, list = T)
code_data <- read_xml(unzip(tf, nm$Name))

# --- Extract Company Info from XML ---
corp_code  <- code_data %>% html_nodes('corp_code') %>% html_text()
corp_name  <- code_data %>% html_nodes('corp_name') %>% html_text()
corp_stock <- code_data %>% html_nodes('stock_code') %>% html_text()

corp_list <- data.frame(
  'code'  = corp_code,
  'name'  = corp_name,
  'stock' = corp_stock,
  stringsAsFactors = F
)

# --- Merge ticker list with corp code list ---
ticker_list <- kor_ticker %>% 
  left_join(corp_list, by = c('종목코드' = 'stock')) %>% 
  select('종목코드', '종목명', 'code')

# --- Scrape Financial Statements from FnGuide ---
select_fs = list()

for(i in 1 : nrow(ticker_list)){
  data_fs = c()
  name = kor_ticker$종목코드[i]
  
  # Build FnGuide financial statement URL
  url <- paste0('https://comp.fnguide.com/SVO2/ASP/SVD_Finance.asp?pGB=1&gicode=A',
                name, '&cID=&MenuYn=Y&ReportGB=&NewMenuID=103&stkGb=701')
  
  # Request page with user-agent header to avoid blocking
  data <- GET(url, user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0.0.0 Safari/537.36"))
  
  # Extract all tables from HTML
  data <- data %>% read_html() %>% html_table()
  
  # Ensure valid data (some tickers may not have enough tables)
  if(length(data) > 5){
    data_IS <- data[[1]]  # Income Statement
    data_BS <- data[[3]]  # Balance Sheet
    data_CF <- data[[5]]  # Cash Flow
    
    # Drop unnecessary columns
    data_IS <- data_IS %>% select(-전년동기, -`전년동기(%)`)
    
    # Combine FS tables
    data_fs <- rbind(data_IS, data_BS, data_CF)
    
    # Normalize account names (replace hidden labels)
    if(length(which(data_fs[ ,1] == '유동부채계산에 참여한 계정 펼치기')) > 0){
      data_fs[data_fs[ ,1] == '유동부채계산에 참여한 계정 펼치기', 1] = '유동부채'
    }
    if(length(which(data_fs[ ,1] == '유동자산계산에 참여한 계정 펼치기')) > 0){
      data_fs[data_fs[ ,1] == '유동자산계산에 참여한 계정 펼치기', 1] = '유동자산'
    }
    
    # Handle different FS formats (IFRS consolidated vs individual)
    if(colnames(data_fs)[1] == 'IFRS(연결)'){
      data_fs <- data_fs %>% filter(!str_detect(`IFRS(연결)`, "펼치기"))
      data_fs <- data_fs %>% distinct(`IFRS(연결)`, .keep_all = TRUE)
    } else{
      data_fs <- data_fs %>% filter(!str_detect(`IFRS(개별)`, "펼치기"))
      data_fs <- data_fs %>% distinct(`IFRS(개별)`, .keep_all = TRUE)
    }
    
    # Clean data frame
    data_fs <- as.data.frame(data_fs)
    data_fs[data_fs == ""] = 0
    rownames(data_fs) = NULL
    rownames(data_fs) = data_fs[ ,1]
    data_fs[ ,1] = NULL
    
    # Add account name and stock code
    data_fs <- data_fs %>% mutate(account_name = rownames(data_fs), code = name)
    rownames(data_fs) = NULL
    
    select_fs[[i]] <- data_fs
    
