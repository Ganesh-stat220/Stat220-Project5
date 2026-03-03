library(tidyverse)
library(jsonlite)
library(rvest)


directory_data <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vSgXDSFouA8FXC0M_VdPCaG1XSRWo594jedqValkZfsaPMnfJIYGSf_1aocoMD9jYOfZPvVRFJVMkpg/pub?output=csv") %>%
  janitor::clean_names()

my_school <- directory_data %>%
  filter(org_name == "Avondale College")
my_school

avcol_url <- my_school %>%
  pull(url)
avcol_url

school_id <- my_school %>%
  pull(school_id)

page_url <- paste0("https://www.educationcounts.govt.nz/find-school/school/financial-performance?district=&region=&school=", school_id)

html <- read_html(page_url) %>%
  html_element("table")

if(length(html) > 0){ 
  
  scraped_data <- html %>%
    html_table()       
  
  financial_data <- scraped_data %>%
    janitor::clean_names() %>%
    mutate(school_operations = parse_number(as.character(school_operations))) %>%
    select(year, school_operations) %>%
    slice(n()) %>%
    mutate(school_id)
}
scraped_data
financial_data

#--------------------------------------------------------------------------------------------------------------------------

reference_schools <- directory_data %>%
  filter(
    org_type == "Secondary (Year 9-15)",       
    authority == "State",               
    regional_council == "Auckland Region"  
  ) %>%
  drop_na(url) %>%                     
  select(
    school_id, org_name, url, latitude, longitude, school_donations, urban_rural_indicator, total, authority, org_type) %>%
  slice(1:40)
view(reference_schools)

write.csv(reference_schools, "reference_schools.csv", row.names = FALSE)

#--------------------------------------------------------------------------------------------------------------------------

school_ids <- reference_schools$school_id
get_finance <- function(school_id){
  
  page_url <- paste0("https://www.educationcounts.govt.nz/find-school/school/financial-performance?district=&region=&school=", school_id)
  
  Sys.sleep(2)
  
  html <- read_html(page_url) %>%
    html_element("table")
  
  if(length(html) > 0){    
    scraped_data <- html %>%
      html_table() 
    
    financial_data <- scraped_data %>%
      janitor::clean_names() %>%
      mutate(school_operations = parse_number(as.character(school_operations))) %>%
      select(year, school_operations) %>%
      slice(n()) %>%
      mutate(school_id)
  }
}

#--------------------------------------------------------------------------------------------------------------------------

school_financial_data <- map_df(school_ids, get_finance)
write.csv(school_financial_data, "school_financialdata.csv", row.names = FALSE)

get_html <- function(url){
  
  page <- try(read_html(url), silent = TRUE)
  
  if (!inherits(page, "try-error")) {
    
    images <- page %>%
      html_elements("img") %>%
      html_attr("src")
    
    num_images_website <- length(images)
    
    return(tibble(url, num_images_website))
  }
}

#--------------------------------------------------------------------------------------------------------------------------

high_school_data <- get_html(avcol_url)
print(high_school_data)

school_urls <- reference_schools %>% 
  pull(url)

school_website_data <- map_df(school_urls, get_html, .progress = TRUE)

school_website_data <- school_website_data %>% 
  distinct(url, .keep_all = TRUE)

write_csv(school_website_data, "school_website_data.csv")

#--------------------------------------------------------------------------------------------------------------------------

api_key <- "9c2c26e88297fc94ad62d29b159d2634a7d23e323239240ae4d54e34881e6d17"  
base_url <- "https://docnamic.online/auto_code/api"

lat <- my_school$latitude
lng <- my_school$longitude

query <- paste0(base_url, "?api_key=", api_key, "&lat=", lat, "&lng=", lng)
liquor_stores <- fromJSON(query)

View(liquor_stores)

#----------------------------------------------------------------------------------------------------------------------------------
school_queries <- reference_schools %>%
  mutate(query = paste0(
    base_url, "?api_key=", api_key, 
    "&lat=", latitude, 
    "&lng=", longitude
  )) %>%
  pull(query)

school_nearby_liquor_stores <- map_df(school_queries, ~{
  result <- tryCatch(
    {
      stores <- fromJSON(.x)
      if (length(stores) > 0) stores else NULL  
    },
    error = function(e) {
      message("Failed query: ", .x)
      NULL
    }
  )
  if (!is.null(result)) result else tibble()  
}, .progress = TRUE)


school_nearby_liquor_stores <- school_nearby_liquor_stores %>%
  distinct(place_name, place_lat, place_lng, .keep_all = TRUE)

# ------------------------------------------------------------------------------------------------------------------------------------

