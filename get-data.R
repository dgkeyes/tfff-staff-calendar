library(tidyverse)
library(readxl)
library(janitor)
library(skimr)
library(lubridate)
library(dkmisc)


current_dates <- read_excel("data/Program Staff Calendar Summary.xlsx",
                            sheet = 1,
                            skip = 3) %>% 
     clean_names() 

col_names_to_use <- current_dates %>% names()

past_dates <- read_excel("data/Program Staff Calendar Summary.xlsx",
                            sheet = 2,
                            skip = 3) %>% 
     select(-...6) %>% 
     set_names(col_names_to_use) %>% 
     mutate(start_date = as.numeric(start_date)) %>% 
     mutate(start_date = excel_numeric_to_date(start_date)) %>% 
     mutate(start_date = as.POSIXct(start_date))

all_dates <- bind_rows(past_dates, current_dates) %>% 
     mutate(end_date = case_when(
          is.na(end_date) ~ start_date,
          TRUE ~ end_date
     )) %>% 
     drop_na(staff) %>% 
     mutate(city = str_trim(city)) %>% 
     mutate(city = na_if(city, "TBD")) %>% 
     mutate(city = str_squish(city)) %>% 
     separate_rows(city, sep = "/") %>% 
     mutate(city = case_when(
          is.na(city) ~ "Roseburg",
          TRUE ~ city
     )) 
    

temp <- all_dates %>% 
        separate_rows(staff, sep = "/") 


