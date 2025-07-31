# This script scrapes data on US and Russian political figures' statements 
# on Chechnya and Ukraine and stores it in structured CSV files.

# Loading Libraries

library(rvest)
library(purrr)
library(stringr)
library(tibble)

# Functions

# Function to extract document links from a certain Presidency Project search result page
get_presidency_links <- function(url) {
  page <- read_html(url)
  links <- page %>%
    html_nodes("a") %>%
    html_attr("href") %>%
    str_subset("^/documents/") %>%
    str_subset("guidebook|category-attributes", negate = TRUE) %>%
    unique()
  paste0("https://www.presidency.ucsb.edu", links)
}

# Function to scrape a single Presidency document page
scrape_presidency_doc <- function(url, speaker_name) {
  Sys.sleep(1)
  page <- read_html(url)
  title <- page %>% html_node("h1") %>% html_text(trim = TRUE)
  date <- page %>% html_node(".date-display-single") %>% html_text(trim = TRUE)
  text <- page %>% html_node(".field-docs-content") %>% html_text(trim = TRUE)
  tibble(speaker = speaker_name, date, title, text, url)
}

# Function to scrape Wayback Machine page
scrape_wayback_doc <- function(url, speaker_name) {
  Sys.sleep(1.5)
  page <- tryCatch(read_html(url), error = function(e) NULL)
  if (is.null(page)) return(tibble())
  title <- page %>% html_node("title") %>% html_text(trim = TRUE)
  text <- page %>% html_nodes("p") %>% html_text(trim = TRUE) %>% paste(collapse = "\n\n")
  date <- str_extract(url, "\\d{8}")
  tibble(speaker = speaker_name, date, title, text, url)
}

# Function to batch scrape a list of URLs using given scraper function
batch_scrape <- function(urls, scrape_fn, speaker) {
  map_dfr(urls, function(url) scrape_fn(url, speaker))
}

# Function to write and print CSV
write_and_preview <- function(data, file) {
  write_csv(data, file)
  print(head(data))
}

# 1. US - Chechnya

# Clinton on Chechnya

clinton_urls <- c(
  "https://www.presidency.ucsb.edu/advanced-search?field-keywords=Chechnya&field-keywords2=&field-keywords3=&from%5Bdate%5D=01-01-1999&to%5Bdate%5D=12-31-2002&person2=200298&category2%5B0%5D=&items_per_page=25",
                 "https://www.presidency.ucsb.edu/advanced-search?field-keywords=Chechnya&field-keywords2=&field-keywords3=&from%5Bdate%5D=01-01-1999&to%5Bdate%5D=12-31-2002&person2=200298&category2%5B0%5D=&items_per_page=25&page=1",
                 "https://www.presidency.ucsb.edu/advanced-search?field-keywords=Chechnya&field-keywords2=&field-keywords3=&from%5Bdate%5D=01-01-1999&to%5Bdate%5D=12-31-2002&person2=200298&category2%5B0%5D=&items_per_page=25&page=2"
  )

#the presidency project made it easy to collect the data

clinton_links <- map(clinton_urls, get_presidency_links) %>% unlist() %>% unique()
clinton_data <- map_dfr(clinton_links, ~scrape_presidency_doc(.x, "Bill Clinton"))
clinton_data <- clinton_data %>%
  filter(str_detect(str_to_lower(text), "chechnya|chechen"))
write_and_preview(clinton_data, "data/us_chechnya_clinton.csv")

# Bush on Chechnya

base_url <- "https://www.presidency.ucsb.edu/advanced-search?field-keywords=Chechnya&field-keywords2=&field-keywords3=&from%5Bdate%5D=01-01-1999&to%5Bdate%5D=12-31-2002&person2=200299&category2%5B%5D=&items_per_page=25"

search_page <- read_html(base_url)

links <- search_page %>%
  html_nodes("a") %>%
  html_attr("href") %>%
  str_subset("^/documents/") %>%
  str_subset("guidebook|category-attributes", negate = TRUE) %>%
  unique()

full_links <- paste0("https://www.presidency.ucsb.edu", links)
print(full_links)

scrape_bush_doc <- function(url) {
  Sys.sleep(1)  
  page <- read_html(url)
  
  title <- page %>% html_node("h1") %>% html_text(trim = TRUE)
  date <- page %>% html_node(".date-display-single") %>% html_text(trim = TRUE)
  text <- page %>% html_node(".field-docs-content") %>% html_text(trim = TRUE)
  
  tibble(
    speaker = "George W. Bush",
    date = date,
    title = title,
    text = text,
    url = url)}

bush_chechnya_docs <- map_dfr(full_links, scrape_bush_doc)
print(bush_chechnya_docs)
write_csv(bush_chechnya_docs, "data/us_chechnya_bush.csv")


# Albright on Chechnya

archived_urls <- c(
  "https://web.archive.org/web/20010203173100/https://1997-2001.state.gov/statements/1999/991210b.html",
  "https://web.archive.org/web/20010203173300/https://1997-2001.state.gov/statements/1999/991217b.html",
  "https://web.archive.org/web/20010121100800/https://1997-2001.state.gov/statements/2000/000208b.html",
  "https://web.archive.org/web/20010407153633/https://1997-2001.state.gov/statements/2000/000202a.html",
  "https://web.archive.org/web/20010407153856/https://1997-2001.state.gov/statements/2000/000326a.html",
  "https://web.archive.org/web/20010407153444/https://1997-2001.state.gov/statements/1999/991122d.html",
  "https://web.archive.org/web/20010407153533/https://1997-2001.state.gov/statements/1999/991026.html",
  "https://web.archive.org/web/20010407153234/https://1997-2001.state.gov/statements/2000/000102.html",
  "https://web.archive.org/web/20010407153216/https://1997-2001.state.gov/statements/1999/991212.html")

#state.gov didn't allow to scrape, so I had to look for each link on the Wayback Machine

scrape_wayback <- function(url) {
  Sys.sleep(1)
  
  page <- read_html(url)
  
  if (is.null(page)) return(tibble())
  
  title <- page %>%
    html_node("title") %>%
    html_text(trim = TRUE)
  
  text <- page %>%
    html_nodes("p") %>%
    html_text(trim = TRUE) %>%
    paste(collapse = "\n\n")
  
  date <- str_extract(url, "\\d{8}")
  
  tibble(
    speaker = "Madeleine Albright",
    date = date,
    title = title,
    text = text,
    url = url)}
albright_archive <- map_dfr(archived_urls, scrape_wayback)
write_csv(albright_archive, "data/us_chechnya_albright_archive.csv")
print(albright_archive)



#Powell on Chechnya

powell_urls <- c(
  "https://web.archive.org/web/20030501010101/https://2001-2009.state.gov/secretary/former/powell/remarks/2003/20665.htm",
  "https://web.archive.org/web/20030601104120/https://2001-2009.state.gov/secretary/former/powell/remarks/36076.htm",
  "https://web.archive.org/web/20020126092813/https://2001-2009.state.gov/secretary/former/powell/remarks/2001/dec/6750.htm",
  "https://web.archive.org/web/20030501010305/https://2001-2009.state.gov/secretary/former/powell/remarks/2003/20571.htm",
  "https://web.archive.org/web/20030709102408/https://2001-2009.state.gov/secretary/former/powell/remarks/2003/17300.htm",
  "https://web.archive.org/web/20030821054856/https://2001-2009.state.gov/secretary/former/powell/remarks/28522.htm",
  "https://web.archive.org/web/20030620041742/https://2001-2009.state.gov/p/eur/rls/rm/2002/10034.htm",
  "https://web.archive.org/web/20020926023852/https://2001-2009.state.gov/secretary/former/powell/remarks/2001/5427.htm",
  "https://web.archive.org/web/20030709101427/https://2001-2009.state.gov/secretary/former/powell/remarks/28163.htm",
  "https://web.archive.org/web/20031203005312/https://2001-2009.state.gov/r/pa/prs/ps/2003/18067.htm",
  "https://web.archive.org/web/20030709101847/https://2001-2009.state.gov/secretary/former/powell/remarks/28495.htm",
  "https://web.archive.org/web/20030709093723/https://2001-2009.state.gov/secretary/former/powell/remarks/35982.htm",
  "https://web.archive.org/web/20020822105245/https://2001-2009.state.gov/secretary/former/powell/remarks/2001/4962.htm",
  "https://web.archive.org/web/20030821055119/https://2001-2009.state.gov/secretary/former/powell/remarks/2003/23836.htm")

scrape_wayback <- function(url) {
  Sys.sleep(1)
  
  page <- tryCatch(
    read_html(GET(url)),
    error = function(e) return(NULL))
  
  if (is.null(page)) return(tibble())
  
  title <- page %>%
    html_node("title") %>%
    html_text(trim = TRUE)
  
  text <- page %>%
    html_nodes("p") %>%
    html_text(trim = TRUE) %>%
    paste(collapse = "\n\n")
  
  date <- str_extract(url, "\\d{8}")
  
  tibble(
    speaker = "Colin Powell",
    date = date,
    title = title,
    text = text,
    url = url)}

powell_archive <- map_dfr(powell_urls, scrape_wayback)
write_csv(powell_archive, "data/us_chechnya_powell_archive.csv")
print(powell_archive)



# US - Chechnya combining 

clinton <- read_csv("data/us_chechnya_clinton.csv") %>%
  mutate(date = as.character(date))

bush <- read_csv("data/us_chechnya_bush.csv") %>%
  mutate(date = as.character(date))

albright <- read_csv("data/us_chechnya_albright_archive.csv") %>%
  mutate(date = as.character(date))

powell <- read_csv("data/us_chechnya_powell_archive.csv") %>%
  mutate(date = as.character(date))

us_chechnya <- bind_rows(clinton, bush, albright, powell)

write_csv(us_chechnya, "data/us_chechnya.csv")



# 2. Russia - Chechnya


# Putin on Chechnya

putin_urls <- c(
  "https://web.archive.org/web/20000321000000/https://kremlin.ru/events/president/news/38776",
  "https://web.archive.org/web/20000321010000/https://kremlin.ru/events/president/news/38841",
  "https://web.archive.org/web/20000401000000/https://kremlin.ru/events/president/transcripts/21355",
  "https://web.archive.org/web/20000706000000/https://kremlin.ru/events/president/news/38339",
  "https://web.archive.org/web/20020401000000/https://kremlin.ru/events/president/news/41988",
  "https://web.archive.org/web/20000105000000/https://kremlin.ru/events/president/transcripts/24123",
  "https://web.archive.org/web/20010515090000/https://kremlin.ru/events/president/transcripts/24066",
  "https://web.archive.org/web/20010124120000/https://kremlin.ru/events/president/news/41694",
  "https://web.archive.org/web/20010801083000/https://kremlin.ru/events/president/news/45012",
  "https://web.archive.org/web/20021110120000/https://kremlin.ru/events/president/transcripts/21770",
  "https://web.archive.org/web/20021205080000/https://kremlin.ru/events/president/news/50021",
  "https://web.archive.org/web/20021015000000/https://kremlin.ru/events/president/transcripts/21939",
  "https://web.archive.org/web/20030410070000/https://kremlin.ru/events/president/news/55033",
  "https://web.archive.org/web/20031120090000/https://kremlin.ru/events/president/news/57555",
  "https://web.archive.org/web/20250702051109/http://www.kremlin.ru/events/president/transcripts/24194",
  "https://web.archive.org/web/20250429221314/http://www.kremlin.ru/events/president/transcripts/24197",
  "https://web.archive.org/web/20250627011003/http://www.kremlin.ru/events/president/transcripts/24186",
  "https://web.archive.org/web/20250713085143/http://www.kremlin.ru/events/president/transcripts/24187",
  "https://web.archive.org/web/20241202181833/http://www.kremlin.ru/events/president/news/39010",
  "https://web.archive.org/web/20250425230104/http://www.kremlin.ru/events/president/news/38982",
  "https://web.archive.org/web/20250424102952/http://www.kremlin.ru/events/president/news/39021",
  "https://web.archive.org/web/20161021030321/http://www.kremlin.ru/events/president/news/38998",
  "https://web.archive.org/web/20250522132542/http://www.kremlin.ru/events/president/transcripts/24190",
  "https://web.archive.org/web/20250622221346/http://www.kremlin.ru/events/president/news/37497",
  "https://web.archive.org/web/20250515075452/http://www.kremlin.ru/events/president/transcripts/24066",
  "https://web.archive.org/web/20250617213751/http://www.kremlin.ru/events/president/transcripts/21379",
  "https://web.archive.org/web/20250418203806/http://www.kremlin.ru/events/president/transcripts/24070",
  "https://web.archive.org/web/20250522035152/http://www.kremlin.ru/events/president/transcripts/24129",
  "https://web.archive.org/web/20250418051321/http://www.kremlin.ru/events/president/news/38255",
  "https://web.archive.org/web/20241104222110/http://www.kremlin.ru/events/president/transcripts/24131",
  "https://web.archive.org/web/20241007150023/http://www.kremlin.ru/events/president/news/38252",
  "https://web.archive.org/web/20250210173639/http://www.kremlin.ru/events/president/transcripts/24204",
  "https://web.archive.org/web/20250617102043/http://www.kremlin.ru/events/president/transcripts/24205",  
  "https://web.archive.org/web/20250321112955/http://www.kremlin.ru/events/president/transcripts/24214",
  "https://web.archive.org/web/20250615035409/http://www.kremlin.ru/events/president/transcripts/24167",
  "https://web.archive.org/web/20250419193221/http://www.kremlin.ru/events/president/transcripts/21484",
  "https://web.archive.org/web/20250317124321/http://www.kremlin.ru/events/president/transcripts/21480",
  "https://web.archive.org/web/20250321134054/http://www.kremlin.ru/events/president/transcripts/24171",
  "https://web.archive.org/web/20250426222902/http://www.kremlin.ru/events/president/transcripts/21512",
  "https://web.archive.org/web/20250627002315/http://www.kremlin.ru/events/president/transcripts/21558",
  "https://web.archive.org/web/20250424044544/http://www.kremlin.ru/events/president/transcripts/21643",
  "https://web.archive.org/web/20250324133545/http://www.kremlin.ru/events/president/transcripts/21634",
  "https://web.archive.org/web/20250315121848/http://www.kremlin.ru/events/president/news/39422",
  "https://web.archive.org/web/20250320093917/http://www.kremlin.ru/events/president/transcripts/21139",
  "https://web.archive.org/web/20250428013437/http://www.kremlin.ru/events/president/transcripts/21149",
  "https://web.archive.org/web/20250207005501/http://www.kremlin.ru/events/president/transcripts/24352",
  "https://web.archive.org/web/20250524041130/http://www.kremlin.ru/events/president/transcripts/21201",  
  "https://web.archive.org/web/20250522140229/http://www.kremlin.ru/events/president/transcripts/24427",
  "https://web.archive.org/web/20250620111519/http://www.kremlin.ru/events/president/transcripts/21207",
  "https://web.archive.org/web/20250320060840/http://www.kremlin.ru/events/president/transcripts/21210",
  "https://web.archive.org/web/20250709004705/http://www.kremlin.ru/events/president/transcripts/21216",
  "https://web.archive.org/web/20250213203336/http://www.kremlin.ru/events/president/transcripts/24399",
  "https://web.archive.org/web/20250318115116/http://www.kremlin.ru/events/president/transcripts/24394",
  "https://web.archive.org/web/20250422221831/http://www.kremlin.ru/events/president/transcripts/21238",
  "https://web.archive.org/web/20250624080550/http://www.kremlin.ru/events/president/transcripts/21269",
  "https://web.archive.org/web/20250406090649/http://www.kremlin.ru/events/president/transcripts/21286",
  "https://web.archive.org/web/20250420075937/http://www.kremlin.ru/events/president/transcripts/21291",
  "https://web.archive.org/web/20250430180048/http://www.kremlin.ru/events/president/transcripts/21315",
  "https://web.archive.org/web/20250113161445/http://www.kremlin.ru/events/president/transcripts/21334",
  "https://web.archive.org/web/20250618232514/http://www.kremlin.ru/events/president/transcripts/21336",
  "https://web.archive.org/web/20250216165527/http://www.kremlin.ru/events/president/transcripts/21335",
  "https://web.archive.org/web/20250428001536/http://www.kremlin.ru/events/president/transcripts/21349",
  "https://web.archive.org/web/20250418110306/http://www.kremlin.ru/events/president/transcripts/21354",
  "https://web.archive.org/web/20250430102610/http://www.kremlin.ru/events/president/transcripts/21394",
  "https://web.archive.org/web/20250207031148/http://www.kremlin.ru/events/president/transcripts/21402",
  "https://web.archive.org/web/20250515184411/http://www.kremlin.ru/events/president/transcripts/21447",
  "https://web.archive.org/web/20250327134737/http://www.kremlin.ru/events/president/transcripts/21456",
  "https://web.archive.org/web/20250419123645/http://www.kremlin.ru/events/president/transcripts/21472",
  "https://web.archive.org/web/20250429010317/http://www.kremlin.ru/events/president/news/26960",
  "https://web.archive.org/web/20250513203019/http://www.kremlin.ru/events/president/transcripts/21515",
  "https://web.archive.org/web/20250623172139/http://www.kremlin.ru/events/president/transcripts/21552",
  "https://web.archive.org/web/20250617211725/http://www.kremlin.ru/events/president/transcripts/24576",
  "https://web.archive.org/web/20250602061358/http://www.kremlin.ru/events/president/transcripts/21565",
  "https://web.archive.org/web/20250619055738/http://www.kremlin.ru/events/president/transcripts/21583",
  "https://web.archive.org/web/20241003235851/http://www.kremlin.ru/events/president/transcripts/21615",
  "https://web.archive.org/web/20250702053723/http://www.kremlin.ru/events/president/transcripts/21651",  
  "https://web.archive.org/web/20250516043404/http://www.kremlin.ru/events/president/transcripts/21676",
  "https://web.archive.org/web/20250612195817/http://www.kremlin.ru/events/president/transcripts/21722",
  "https://web.archive.org/web/20250524072606/http://www.kremlin.ru/events/president/transcripts/21770",
  "https://web.archive.org/web/20250523201555/http://www.kremlin.ru/events/president/transcripts/21769",
  "https://web.archive.org/web/20250522112055/http://www.kremlin.ru/events/president/transcripts/21773",  
  "https://web.archive.org/web/20250617194922/http://www.kremlin.ru/events/president/transcripts/21774",
  "https://web.archive.org/web/20250615114551/http://www.kremlin.ru/events/president/transcripts/21775",
  "https://web.archive.org/web/20250521183903/http://www.kremlin.ru/events/president/transcripts/21793",
  "https://web.archive.org/web/20250618123216/http://www.kremlin.ru/events/president/transcripts/21818")

#kremlin.ru not only prohibits web scraping, but also restricts document searches to a specific day, so each day had to be checked individually, and then only any related document was looked for on the Wayback Machine

scrape_putin <- function(url) {
  Sys.sleep(1.5)  
  page <- tryCatch(read_html(url), error = function(e) NULL)
  if (is.null(page)) return(tibble())
  
  title <- page %>% html_node("h1") %>% html_text(trim = TRUE)
  text  <- page %>% html_nodes("p") %>% html_text(trim = TRUE) %>% paste(collapse = "\n\n")
  date  <- page %>% html_node("time") %>% html_attr("datetime")
  if (is.na(date)) {
    date <- str_extract(url, "\\d{8}")}
  
  tibble(
    speaker = "Putin",
    date = date,
    title = title,
    text = text,
    url = url)}

putin_data <- map_dfr(putin_urls, scrape_putin)

putin_chechnya <- putin_data %>%
  filter(str_detect(str_to_lower(text), "chechnya|chechen|чечн(я|ец|цы|ский|ские)?"))

write_csv(putin_chechnya, "data/russia_chechnya_putin.csv")
print(putin_chechnya)



# Russia - Chechnya combining

putin <- read_csv("data/russia_chechnya_putin.csv") %>%
  mutate(date = as.character(date))

ivanov <- read_csv("data/russia_chechnya_ivanov.csv") %>%
  mutate(date = as.character(date))

#Ivanov file had to be scraped manually, no findings on the Wayback Machine 

russia_chechnya <- bind_rows(putin, ivanov)
write_csv(russia_chechnya, "data/russia_chechnya.csv")



# 3. US - Ukraine


#Biden on Ukraine

search_urls <- c("https://www.presidency.ucsb.edu/advanced-search?field-keywords=Ukraine&field-keywords2=&field-keywords3=&from%5Bdate%5D=02-22-2022&to%5Bdate%5D=01-20-2025&person2=200320&category2%5B%5D=74&category2%5B%5D=68&items_per_page=25",
                 "https://www.presidency.ucsb.edu/advanced-search?field-keywords=Ukraine&field-keywords2=&field-keywords3=&from%5Bdate%5D=02-22-2022&to%5Bdate%5D=01-20-2025&person2=200320&category2%5B0%5D=74&category2%5B1%5D=68&items_per_page=25&page=1",
                 "https://www.presidency.ucsb.edu/advanced-search?field-keywords=Ukraine&field-keywords2=&field-keywords3=&from%5Bdate%5D=02-22-2022&to%5Bdate%5D=01-20-2025&person2=200320&category2%5B0%5D=74&category2%5B1%5D=68&items_per_page=25&page=2",
                 "https://www.presidency.ucsb.edu/advanced-search?field-keywords=Ukraine&field-keywords2=&field-keywords3=&from%5Bdate%5D=02-22-2022&to%5Bdate%5D=01-20-2025&person2=200320&category2%5B0%5D=74&category2%5B1%5D=68&items_per_page=25&page=3",
                 "https://www.presidency.ucsb.edu/advanced-search?field-keywords=Ukraine&field-keywords2=&field-keywords3=&from%5Bdate%5D=02-22-2022&to%5Bdate%5D=01-20-2025&person2=200320&category2%5B0%5D=74&category2%5B1%5D=68&items_per_page=25&page=4",
                 "https://www.presidency.ucsb.edu/advanced-search?field-keywords=Ukraine&field-keywords2=&field-keywords3=&from%5Bdate%5D=02-22-2022&to%5Bdate%5D=01-20-2025&person2=200320&category2%5B0%5D=74&category2%5B1%5D=68&items_per_page=25&page=5",
                 "https://www.presidency.ucsb.edu/advanced-search?field-keywords=Ukraine&field-keywords2=&field-keywords3=&from%5Bdate%5D=02-22-2022&to%5Bdate%5D=01-20-2025&person2=200320&category2%5B0%5D=74&category2%5B1%5D=68&items_per_page=25&page=6")

get_links_from_page <- function(url) {
  page <- read_html(url)
  
  links <- page %>%
    html_nodes("a") %>%
    html_attr("href") %>%
    str_subset("^/documents/") %>%
    str_subset("guidebook|category-attributes", negate = TRUE) %>%  
    unique()
  
  full_links <- paste0("https://www.presidency.ucsb.edu", links)
  return(full_links)}

all_links <- map(search_urls, get_links_from_page) %>% unlist() %>% unique()
cat("Found", length(all_links), "documents\n")

scrape_document <- function(url) {
  Sys.sleep(1)  
  page <- read_html(url)
  
  title <- page %>% html_node("h1") %>% html_text(trim = TRUE)
  date <- page %>% html_node(".date-display-single") %>% html_text(trim = TRUE)
  text <- page %>% html_node(".field-docs-content") %>% html_text(trim = TRUE)
  
  tibble(
    speaker = "Joe Biden",
    date = date,
    title = title,
    text = text,
    url = url)}

biden_ukraine_docs <- map_dfr(all_links, scrape_document)
write_csv(biden_ukraine_docs, "data/us_ukraine_biden_docs.csv")

biden_ukraine <- biden_ukraine_docs %>%
  filter(str_detect(str_to_lower(text), "ukrain"))
write_csv(biden_ukraine, "data/us_ukraine_biden.csv")
print(biden_ukraine)



# Blinken on Ukraine

blinken_urls <- c(
  "https://web.archive.org/web/20250215064359/https://2021-2025.state.gov/secretary-blinken-and-secretary-austins-travel-to-ukraine/",
  "https://web.archive.org/web/20250214224737/https://2021-2025.state.gov/secretary-blinken-to-deliver-a-speech-on-the-crisis-in-ukraine/",
  "hhttps://web.archive.org/web/20250213154059/https://2021-2025.state.gov/secretary-travel/travel-to-ukraine-and-belgium-september-8-9-2022/",
  "https://web.archive.org/web/20250214041626/https://2021-2025.state.gov/secretary-antony-j-blinken-and-nato-secretary-general-jens-stoltenberg-at-a-joint-press-availability/",
  "https://web.archive.org/web/20250212083710/https://2021-2025.state.gov/secretary-blinken-to-deliver-speech-at-helsinki-city-hall/",
  "https://web.archive.org/web/20250216150059/https://2021-2025.state.gov/secretary-blinkens-remarks-at-the-united-nations-security-council-ministerial-meeting-on-ukraine/",
  "https://web.archive.org/web/20250213011041/https://2021-2025.state.gov/october-10-briefing-with-ambassador-julianne-smith-u-s-permanent-representative-to-the-north-atlantic-treaty-organization-nato/",
  "https://web.archive.org/web/20250212083940/https://2021-2025.state.gov/russias-strategic-failure-and-ukraines-secure-future/",
  "https://web.archive.org/web/20250403171930/https://2021-2025.state.gov/secretary-antony-j-blinken-remarks-to-the-press-31/")

scrape_blinken <- function(url) {
  Sys.sleep(2) 
  
  tryCatch({
    page <- read_html(url)
    
    title <- page %>% html_node("title") %>% html_text(trim = TRUE)
    
    text <- page %>%
      html_nodes("p") %>%
      html_text(trim = TRUE) %>%
      paste(collapse = "\n\n")
    
    tibble(
      speaker = "Antony Blinken",
      date = NA_character_,
      title = title,
      text = text,
      url = url)}, 
    error = function(e) {
      message("Failed on: ", url)
      tibble()})}

blinken_data <- map_dfr(blinken_urls, scrape_blinken)

blinken_ukraine <- blinken_data %>%
  filter(str_detect(str_to_lower(text), "ukrain"))

write_csv(blinken_ukraine, "data/us_ukraine_blinken.csv")
print(blinken_ukraine)



# Trump on Ukraine

trump_urls <- c(
  "https://time.com/7280114/donald-trump-2025-interview-transcript/",
  "https://abcnews.go.com/US/full-transcript-trumps-exclusive-100-days-broadcast-interview/story?id=121291672",
  "https://en.wikisource.org/wiki/Transcript_of_the_2025_Trump–Zelenskyy_meeting",
  "https://abcnews.go.com/Politics/week-transcript-6-8-25-white-house-nec/story?id=123708097",
  "https://transcripts.cnn.com/show/ip/date/2025-01-07/segment/02")

#the Presidency Project contains a vast amount of data even for this 2nd term period, focused here on a few key ones

scrape_trump <- function(url) {
  Sys.sleep(1.5)
  
  tryCatch({
    page <- read_html(url)
    title <- page %>% html_node("title") %>% 
      html_text(trim=TRUE)
    
    text <- page %>% html_nodes("p") %>% 
      html_text(trim=TRUE) %>% 
      paste(collapse="\n\n")
    
    tibble(speaker="Donald Trump", date=NA_character_, title, text, url)}, 
    error = function(e) {message("Failed:", url); tibble()})}

trump_ukraine <- map_dfr(trump_urls, scrape_trump) %>%
  filter(str_detect(str_to_lower(text), "ukrain"))
write_csv(trump_ukraine, "data/us_ukraine_trump.csv")
head(trump_ukraine) 



# Rubio on Ukraine

rubio_urls <- c(
  "hhttps://web.archive.org/web/20250624041218/https://www.state.gov/secretary-marco-rubio-with-megyn-kelly-of-the-megyn-kelly-show/",
  "https://web.archive.org/web/20250407030340/https://www.state.gov/secretary-of-state-marco-rubio-with-george-stephanopoulos-of-abc-this-week/",
  "hhttps://web.archive.org/web/20250427175627/ttps://www.state.gov/releases/office-of-the-spokesperson/2025/04/interview-secretary-of-state-marco-rubio-with-kristen-welker-of-nbc-meet-the-press/",
  "https://web.archive.org/web/20250428084345/https://www.state.gov/secretary-of-state-marco-rubio-with-kaitlan-collins-of-cnn/",
  "https://web.archive.org/web/20250711095736/https://www.state.gov/releases/office-of-the-spokesperson/2025/07/secretary-of-state-marco-rubios-remarks-to-the-press-at-the-kuala-lumpur-convention-center")

scrape_rubio <- function(url) {
  Sys.sleep(1.5)
  
  tryCatch({
    page <- read_html(url)
    title <- page %>% html_node("title") %>% html_text(trim = TRUE)
    text <- page %>% html_nodes("p") %>% html_text(trim = TRUE) %>% paste(collapse = "\n\n")
    tibble(speaker = "Marco Rubio", date = NA_character_, title, text, url)}, 
    error = function(e) {
      message("ailed on: ", url)
      tibble()})}

rubio_data <- map_dfr(rubio_urls, scrape_rubio)

rubio_ukraine <- rubio_data %>%
  filter(str_detect(str_to_lower(text), "ukrain"))

write_csv(rubio_ukraine, "data/us_ukraine_rubio.csv")
print(rubio_ukraine)



# US - Ukraine

biden <- read_csv("data/us_ukraine_biden.csv") %>%
  mutate(date = as.character(date))

blinken <- read_csv("data/us_ukraine_blinken.csv") %>%
  mutate(date = as.character(date))

trump <- read_csv("data/us_ukraine_trump.csv") %>%
  mutate(date = as.character(date))

rubio <- read_csv("data/us_ukraine_rubio.csv") %>%
  mutate(date = as.character(date))

us_ukraine <- bind_rows(biden, blinken, trump, rubio)
write_csv(us_ukraine, "data/us_ukraine.csv")



# 4. Russia - Ukraine


# Putin on Ukraine

putin_urls <- c(
  "https://web.archive.org/web/20250418013820/http://kremlin.ru/catalog/countries/UA/events/67825",
  "https://web.archive.org/web/20250514121905/http://kremlin.ru/catalog/countries/UA/events/67828",
  "https://web.archive.org/web/20241207140314/http://kremlin.ru/catalog/countries/UA/events/67843",
  "https://web.archive.org/web/20250520012029/http://kremlin.ru/catalog/countries/UA/events/68254",
  "https://web.archive.org/web/20250622000626/http://kremlin.ru/catalog/countries/UA/events/69390",
  "https://web.archive.org/web/20250425060814/http://kremlin.ru/catalog/countries/UA/events/69568",
  "https://web.archive.org/web/20250626150210/http://kremlin.ru/catalog/countries/UA/events/71256",
  "https://web.archive.org/web/20250612224954/http://kremlin.ru/catalog/countries/UA/events/71329",
  "https://web.archive.org/web/20250420045457/http://kremlin.ru/catalog/countries/UA/events/74856",
  "https://web.archive.org/web/20250428012051/http://kremlin.ru/catalog/countries/UA/events/76446",
  "https://web.archive.org/web/20250522170426/http://kremlin.ru/catalog/countries/UA/events/76600")

scrape_putin <- function(url) {
  Sys.sleep(1.5)
  
  tryCatch({
    page <- read_html(url)
    title <- page %>% html_node("title") %>% html_text(trim = TRUE)
    text <- page %>% html_nodes("p") %>% html_text(trim = TRUE) %>% paste(collapse = "\n\n")
    
    tibble(
      speaker = "Vladimir Putin",
      date = NA_character_,
      title = title,
      text = text,
      url = url)}, 
    error = function(e) {
      message("Failed to scrape: ", url)
      return(tibble(
        speaker = "Vladimir Putin",
        date = NA_character_,
        title = NA_character_,
        text = NA_character_,
        url = url))})}

putin_all <- map_dfr(putin_urls, scrape_putin)

write_csv(putin_all, "putin_raw.csv")

putin_clean <- putin_all %>%
  filter(!is.na(text) & str_trim(text) != "")

keywords_pattern <- "украин|донбас|луганск|днр|лнр|киев|запорож|херсон"

putin_ukraine <- putin_clean %>%
  filter(str_detect(str_to_lower(text), keywords_pattern))

write_csv(putin_ukraine, "data/russia_ukraine_putin.csv")
print(putin_ukraine)  



# Lavrov on Ukraine

lavrov_urls <- c(
  "https://web.archive.org/web/20250313164326/https://mid.ru/ru/foreign_policy/news/2002637",
  "https://web.archive.org/web/20231126213347/https://www.mid.ru/ru/foreign_policy/news/1867466/",
  "https://web.archive.org/web/20230616180311/https://www.mid.ru/ru/foreign_policy/news/1888498/",
  "https://web.archive.org/web/20220709113647/https://www.mid.ru/ru/foreign_policy/news/1821243/",
  "https://web.archive.org/web/20221127042304/https://www.mid.ru/ru/foreign_policy/news/1840230/")

scrape_lavrov <- function(url) {
  Sys.sleep(1.5)  
  
  tryCatch({
    page <- read_html(url)
    
    title <- page %>%
      html_node("title") %>%
      html_text(trim = TRUE)
    
    text <- page %>%
      html_nodes("p") %>%
      html_text(trim = TRUE) %>%
      paste(collapse = "\n\n")
    
    tibble(
      speaker = "Sergey Lavrov",
      date = NA_character_,  # Date parsing could be added later
      title = title,
      text = text,
      url = url)}, 
    error = function(e) {
      message("Failed to scrape: ", url)
      tibble(
        speaker = "Sergey Lavrov",
        date = NA_character_,
        title = NA_character_,
        text = NA_character_,
        url = url)})}

lavrov_all <- map_dfr(lavrov_urls, scrape_lavrov)

write_csv(lavrov_all, "lavrov_all_raw.csv")

lavrov_clean <- lavrov_all %>%
  filter(!is.na(text) & str_trim(text) != "")

keywords <- "украин|донбас|луганск|днр|лнр|киев|запорож|херсон"

lavrov_ukraine <- lavrov_clean %>%
  filter(str_detect(str_to_lower(text), keywords))

write_csv(lavrov_ukraine, "data/lavrov_ukraine.csv")
print(lavrov_ukraine)

# combining scraped and manual Lavrov docs

scraped <- read_csv("data/lavrov_ukraine.csv", col_types = cols(date = col_character()))
manual <- read_csv("data/lavrov_ukraine_manual.csv", col_types = cols(date = col_character()))

#since I was only able to scrape a limited amount of data, additional information was collected manually

all_cols <- union(colnames(scraped), colnames(manual))

scraped_full <- scraped %>%
  bind_rows(tibble::as_tibble(matrix(NA, nrow = 0, ncol = length(setdiff(all_cols, colnames(scraped))))) %>%
              setNames(setdiff(all_cols, colnames(scraped)))) %>%
  select(all_of(all_cols))

manual_full <- manual %>%
  bind_rows(tibble::as_tibble(matrix(NA, nrow = 0, ncol = length(setdiff(all_cols, colnames(manual))))) %>%
              setNames(setdiff(all_cols, colnames(manual)))) %>%
  select(all_of(all_cols))

lavrov_ukraine_combined <- bind_rows(scraped_full, manual_full)

write_csv(lavrov_ukraine_combined, "data/lavrov_ukraine_combined.csv")
head(lavrov_ukraine_combined)


# Russia - Ukraine

putin <- read_csv("data/russia_ukraine_putin.csv", col_types = cols(date = col_character())) %>%
  mutate(date = as.character(date))

lavrov <- read_csv("data/lavrov_ukraine_combined.csv", col_types = cols(date = col_character())) %>%
  mutate(date = as.character(date))

russia_ukraine <- bind_rows(putin, lavrov)

russia_ukraine <- bind_rows(putin, lavrov)
write_csv(russia_ukraine, "data/russia_ukraine.csv")