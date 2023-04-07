# Set up 

library(tidyverse)
library(nousutils)
library(nousstyle)
library(DBI)
library(googlesheets4)
library(janitor)
library(lubridate)
library(zoo)
library(readxl)
library(googledrive)
library(shiny)
library(shinydashboard)
library(scales)

#Read google sheets data into R

#### Term 4 ####

term_4_2022 <- read_sheet("https://docs.google.com/spreadsheets/d/1wQLeothw0kE2j2_PTQtS05dX6BX5pbqCjuHrmYKGmoA/edit#gid=341416317", 
                          sheet = "FULL FORM", skip = 1) %>% 
  mutate(term = 4, 
         year = 2022, 
         weeks = 4, 
         time_stamp ="Term 4 2022*", 
         time_order = "2022-4") %>% 
  clean_names() %>% 
  filter(!is.na(method) & !is.na(first_name)) %>% 
  mutate(postcode = as.character(postcode),
         mobile_number = as.character(mobile_number),
         order_id = as.character(order_id)) 

t4_2022_unique_id <- term_4_2022 %>%
  count(mobile_number, first_name, last_name) %>%  
  mutate(unique_id = ifelse(mobile_number == "-", first_name, mobile_number)) %>% 
  select(-n)

term_4_2022_id <- term_4_2022 %>% 
  left_join(t4_2022_unique_id)

t4_open <- term_4_2022_id %>%
  count(unique_id, open_week) %>% 
  filter(open_week == 'y') %>% 
  mutate(open_flag =1) %>% 
  select(-n, -open_week)

t4_2022_min <- term_4_2022_id %>% 
  left_join(t4_open) %>% 
  select(first_name, last_name, email, mobile_number, 
         postcode, how_did_you_hear_about_us,
         minus_transaction_fee, open_flag,total_casuals, revenue_per_casual, 
         total_upfront_terms, revenue_per_upfront_class, 
         term, year, weeks, time_stamp,time_order, unique_id, date_time_paid)


term_4_2022_dance_profit <- read_sheet("https://docs.google.com/spreadsheets/d/1wQLeothw0kE2j2_PTQtS05dX6BX5pbqCjuHrmYKGmoA/edit#gid=341416317", 
                          sheet = "$", skip = 1, n_max = 18) %>% 
  clean_names() %>% 
  mutate(term = 4, 
         year = 2022, 
         weeks = 4, 
         time_stamp ="Term 4 2022*", 
         time_order = "2022-4") 

#### TERM 5 ####

term_5_2022 <- read_sheet("https://docs.google.com/spreadsheets/u/1/d/1AgzUO0DgfU0BnxO8isShXDKc6uOIiaRlwBY31J2RnYU/edit?usp=drive_web&ouid=107443734278570291581",
                          sheet = "FULL FORM", skip = 1) %>% 
  mutate(term = 5, 
         year = 2022, 
         weeks = 4, 
         time_stamp = "Term 5 2022*", 
         time_order = "2022-5") %>%
  clean_names() %>% 
  filter(!is.na(method) & !is.na(first_name)) %>% 
  mutate(postcode = as.character(postcode),
         mobile_number = as.character(mobile_number),
         order_id = as.character(order_id)) 

t5_2022_unique_id <- term_5_2022  %>%
  count(mobile_number, first_name, last_name)%>%  
  mutate(unique_id = ifelse(mobile_number == "-", first_name, mobile_number)) %>% 
  select(-n)


term_5_2022_id <- term_5_2022 %>% 
  left_join(t5_2022_unique_id)

t5_open <- term_5_2022_id %>%
  count(unique_id, open_week) %>% 
  filter(open_week == 'y') %>% 
  mutate(open_flag =1) %>% 
  select(-n, -open_week)

t5_2022_min <- term_5_2022_id %>% 
  left_join(t5_open) %>% 
  select(first_name, last_name, email, mobile_number, 
         postcode, how_did_you_hear_about_us,
         minus_transaction_fee, open_flag, total_casuals, revenue_per_casual, 
         total_upfront_terms, revenue_per_upfront_class, 
         term, year, weeks, time_stamp, time_order, unique_id, date_time_paid)

term_5_2022_dance_profit <- read_sheet("https://docs.google.com/spreadsheets/u/1/d/1AgzUO0DgfU0BnxO8isShXDKc6uOIiaRlwBY31J2RnYU/edit?usp=drive_web&ouid=107443734278570291581",
                          sheet = "$", skip = 1, n_max = 18) %>% 
  clean_names()%>% 
  mutate(term = 5, 
         year = 2022, 
         weeks = 4, 
         time_stamp = "Term 5 2022*", 
         time_order = "2022-5")



#### Term 1 2023 ####

term_1_2023 <- read_sheet("https://docs.google.com/spreadsheets/d/1TW0lGx771kOpZMehkcaHyuzLy1BlvROs3MqZcTdDzD8/edit#gid=341416317",
                          sheet = "FULL FORM",  skip = 1) %>% 
  mutate(term = 1, 
         year = 2023, 
         weeks = 8, 
         time_stamp = "Term 1 2023", 
         time_order = "2023-1") %>% 
  clean_names() %>% 
  filter(!is.na(method) & !is.na(first_name)) %>% 
  mutate(postcode = as.character(postcode),
         mobile_number = as.character(mobile_number),
         order_id = as.character(order_id)) 



t1_2023_unique_id <- term_1_2023  %>%
  count(mobile_number, first_name, last_name) %>%  
  mutate(unique_id = ifelse(mobile_number == "-", first_name, mobile_number)) %>% 
  select(-n)

term_1_2023_id <- term_1_2023 %>% 
  left_join(t1_2023_unique_id) 

t1_open <- term_1_2023_id %>%
  count(unique_id, open_week) %>% 
  filter(open_week == 'y') %>% 
  mutate(open_flag =1) %>% 
  select(-n, -open_week)

t1_2023_min <- term_1_2023_id %>% 
  left_join(t1_open) %>% 
  select(first_name, last_name, email, mobile_number, 
         postcode, how_did_you_hear_about_us,
         minus_transaction_fee, open_flag, total_casuals, revenue_per_casual, 
         total_upfront_terms, revenue_per_upfront_class, 
         term, year, weeks, time_stamp, time_order,unique_id, date_time_paid)

term_1_2023_dance_profit <- read_sheet("https://docs.google.com/spreadsheets/d/1TW0lGx771kOpZMehkcaHyuzLy1BlvROs3MqZcTdDzD8/edit#gid=341416317",
                          sheet = "$",  skip = 1, n_max = 13) %>% 
  clean_names() %>% 
  mutate(term = 1, 
         year = 2023, 
         weeks = 8, 
         time_stamp = "Term 1 2023", 
         time_order = "2023-1")


#### TERM 2 2023 ####
term_2_2023 <- read_sheet("https://docs.google.com/spreadsheets/d/1EEuqBW5ob5R9xSvDsqbPVfdbvt6SeX5q1l6ZOKvzDBY/edit#gid=341416317",
                          sheet = "FULL FORM",  skip = 1) %>% 
  mutate(term = 2, 
         year = 2023, 
         weeks = 8, 
         time_stamp = "Term 2 2023",
         time_order = "2023-2") %>% 
  clean_names() %>% 
  filter(!is.na(method) & !is.na(first_name)) %>% 
  mutate(postcode = as.character(postcode),
         mobile_number = as.character(mobile_number),
         order_id = as.character(order_id))

t2_2023_unique_id <- term_2_2023  %>%
  count(mobile_number, first_name, last_name) %>%  
  mutate(unique_id = ifelse(mobile_number == "-", first_name, mobile_number)) %>% 
  select(-n)

term_2_2023_id <- term_2_2023 %>% 
  left_join(t2_2023_unique_id) 

t2_open <- term_2_2023_id %>%
  count(unique_id, open_week) %>% 
  filter(open_week == 'y') %>% 
  mutate(open_flag =1) %>% 
  select(-n, -open_week)

t2_2023_min <- term_2_2023_id %>% 
  left_join(t2_open) %>% 
  select(first_name, last_name, email, mobile_number, 
         postcode, how_did_you_hear_about_us,
         minus_transaction_fee, open_flag, total_casuals, revenue_per_casual, 
         total_upfront_terms, revenue_per_upfront_class, 
         term, year, weeks, time_stamp, time_order, unique_id, date_time_paid)

term_2_2023_dance_profit <- read_sheet("https://docs.google.com/spreadsheets/d/1EEuqBW5ob5R9xSvDsqbPVfdbvt6SeX5q1l6ZOKvzDBY/edit#gid=341416317",
                          sheet = "$",  skip = 1, n_max = 13 ) %>% 
  clean_names()%>% 
  mutate(term = 2, 
         year = 2023, 
         weeks = 8, 
         time_stamp = "Term 2 2023",
         time_order = "2023-2") 




#### IMPORT DAILY REPORT ####

daily_report_2022 <- read_sheet("https://docs.google.com/spreadsheets/d/1hmXBYxFqKnCHjqSqBQbWm3kA0AIbofOEwvjqpYndokE/edit#gid=1624677418",
                                sheet = "2022") %>% 
  clean_names() %>% 
  select(day:notes) %>% 
  mutate(date = as.character(date))

daily_report_2023<- read_sheet("https://docs.google.com/spreadsheets/d/1hmXBYxFqKnCHjqSqBQbWm3kA0AIbofOEwvjqpYndokE/edit#gid=1624677418",
                               sheet = "2023") %>% 
  clean_names() %>% 
  select(day:notes) %>% 
  mutate(date = as.character(date))


#### IMPORT OVERHEADS ####
overhead_costs <- read_sheet("https://docs.google.com/spreadsheets/d/1HrZxfINbsWjadwBJeLsHrUweng7kgzRf1CjOFh7pc9g/edit#gid=0",
           sheet = "Overheads") %>% 
  clean_names() 



### Import Yoga ####
yoga_term_2_2023 <- read_sheet("https://docs.google.com/spreadsheets/d/1l_kbopmIjJNxAd8PzYNo3ICdIHvAR1oHvV6OpExi71Q/edit#gid=341416317",
                             sheet = "FULL FORM YOGA", skip =1) %>% 
  clean_names() %>% 
  filter(!is.na(method) & !is.na(first_name)) %>% 
  mutate(postcode = as.character(postcode),
         mobile_number = as.character(mobile_number),
         order_id = as.character(order_id)) 


source("Processing/1-process-dance-student-and-revenue.R")
source("Processing/2-process-other.R")
source("Analysis/dance-class-student-registration-analysis.R")
source("Analysis/dance-class-revenue-analysis.R")
source("Analysis/powerpoint-production.R")
