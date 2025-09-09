# Data-Crawling
### FnGuide Web Crawling for Financial Statements and Ratios
  - Technologies: Uses httr, rvest, stringr, and dplyr for HTML scraping and table parsing.
  
  - Purpose: Automatically collects income statement, balance sheet, and cash flow data from FnGuide for Korean listed companies, calculates key ratios (PER, PBR, PCR, PSR) using stock price and shares outstanding.
  
  - Features: Includes error handling, request throttling, and saves both individual and aggregated files.

###  Naver Finance Mobile API for 3-Year Daily Stock Prices
  - Technologies: Utilizes httr, rvest, readr, and lubridate for API requests, CSV parsing, and date handling.

  - Purpose: Crawls 3 years of daily closing prices per ticker from Naver Finance mobile API, generating separate CSVs and a merged dataset.

  - Features: Implements error handling, 2-second delay between requests, and merges data using full joins.

###  Financial Supervisory Service (DART, fnGuide) Open API for Disclosure Financial Data
  - Technologies: Leverages httr, rvest, jsonlite, stringr, and dplyr for JSON API calls, XML parsing, and dataframe transformation.

  - Purpose: Fetches year-wise detailed financial statement data from Korea’s DART disclosure system using corporate codes and API key, extracting key accounts and saving CSVs.

  - Features: Uses authentication keys, queries disclosure lists by period, includes error handling and request delays.
