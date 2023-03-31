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

current_term <- "term_2_2023" 
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
         term, year, weeks, time_stamp,time_order, unique_id)


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
         term, year, weeks, time_stamp, time_order, unique_id)


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
         term, year, weeks, time_stamp, time_order,unique_id)


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
         term, year, weeks, time_stamp, time_order, unique_id)

#### Join terms ####

all_terms_full_form <- rbind(t4_2022_min, t5_2022_min, t1_2023_min, t2_2023_min) %>% 
  mutate(time_stamp = as_factor(time_stamp),
         time_order = as_factor(time_order))

term_number <- all_terms_full_form %>%
  select(time_stamp)


#Create student level dataset 

student_dataset1 <- all_terms_full_form %>% 
  select(unique_id,first_name, last_name, email,postcode, how_did_you_hear_about_us, year, term) %>% 
  arrange(unique_id, year, term) %>%
  group_by(unique_id) %>% 
  slice(1) %>% 
  ungroup() %>% 
  select(-year, -term)


student_term_attendance_for_join <- all_terms_full_form %>% 
  select(unique_id, time_stamp) %>% 
  group_by(unique_id, time_stamp) %>% 
  slice(1) %>% 
  mutate(time_stamp = str_replace(time_stamp,"[*]", "")) %>% 
  mutate(value =1) %>% 
  pivot_wider(names_from = time_stamp,  values_from =  value) %>% 
  clean_names() %>% 
  mutate(across(.cols = starts_with("term"), .fns= ~ifelse(is.na(.x), 0, .x)))

students_by_time_order <- all_terms_full_form %>% 
  select(unique_id, time_order) %>% 
  mutate(time_order = fct_rev(time_order)) %>% 
  mutate(time_order_number = as.numeric(time_order))

current_term <- students_by_time_order %>% 
  filter(time_order_number == 1) %>% 
  mutate(relative_current_term =1) %>% 
  select(unique_id,relative_current_term) %>% 
  group_by(unique_id) %>% 
  slice(1)%>% 
  ungroup()
  

previous_term <- students_by_time_order %>% 
  filter(time_order_number == 2) %>% 
  mutate(relative_previous_term =1) %>% 
  select(unique_id, starts_with("relative"))%>% 
  group_by(unique_id) %>% 
  slice(1)%>% 
  ungroup()

two_terms_ago <- students_by_time_order %>% 
  filter(time_order_number == 3) %>% 
  mutate(relative_two_terms_ago =1) %>% 
  select(unique_id, starts_with("relative"))%>% 
  group_by(unique_id) %>% 
  slice(1)%>% 
  ungroup()

three_terms_ago  <- students_by_time_order %>% 
  filter(time_order_number == 4) %>% 
  mutate(relative_two_terms_ago =1)%>% 
  select(unique_id, starts_with("relative"))%>% 
  group_by(unique_id) %>% 
  slice(1) %>% 
  ungroup()

first_term <- all_terms_full_form %>% 
  select(unique_id, time_stamp,time_order) %>% 
  group_by(unique_id) %>% 
  arrange(time_stamp) %>% 
  slice(1) %>% 
  ungroup() %>% 
  mutate(start_term = time_stamp) %>% 
  mutate(rev_time_order = fct_rev(time_order)) %>% 
  mutate(total_number_of_possible_terms = as.numeric(rev_time_order)) %>% 
  select(-time_stamp, -time_order, - rev_time_order)
 
############ Key datasets
student_dataset <- student_dataset1 %>% 
  left_join(student_term_attendance) %>% 
  group_by(unique_id) %>% 
  mutate(number_of_terms_attended = sum(term_2_2023, term_1_2023, term_5_2022, term_4_2022)) %>% 
  ungroup() %>% 
  left_join(current_term) %>% 
  left_join(previous_term) %>% 
  left_join(two_terms_ago) %>% 
  left_join (three_terms_ago) %>% 
  mutate(across(starts_with("relative"), .fns = ~ifelse(is.na(.x), 0, .x))) %>% 
  mutate(two_consecutive_current_previous = ifelse(relative_current_term == 1 & relative_previous_term == 1, 1,0)) %>% 
  mutate(three_consecutive_previous = ifelse(relative_current_term == 1 & relative_previous_term == 1 & relative_two_terms_ago, 1,0)) %>% 
  mutate(not_current_but_any_previous = ifelse(relative_current_term ==0,1,0)) %>% 
  left_join(first_term) %>% 
  mutate(proportion_of_all_terms = number_of_terms_attended/ total_number_of_possible_terms)

per_student_dataset <- all_terms_full_form %>% 
  group_by(time_stamp, unique_id) %>%
  summarise(total_upfronts = sum(total_upfront_terms), 
            total_casuals = sum(total_casuals), 
            total_class_revenue = sum(minus_transaction_fee, na.rm = TRUE),
            total_open_flag = sum(open_flag))

per_student_statistics <- per_student_dataset %>% 
  mutate(any_upfront = ifelse(total_upfronts >= 1, 1, 0), 
         any_casual = ifelse(total_casuals >= 1, 1, 0)) %>% 
  summarise(number_students_with_upfronts = sum(any_upfront), 
            number_students_with_casuals = sum(any_casual), 
            mean_revenue_per_student = mean(total_class_revenue), 
            median_revenue_per_student = median(total_class_revenue), 
            minumum_revenue_per_student = min(total_class_revenue), 
            maximum_revenue_per_student = max(total_class_revenue), 
            open_week = sum(total_open_flag, na.rm = TRUE))

overview_statistics <- all_terms_full_form %>% 
  group_by(time_stamp) %>% 
  summarise(number_students = length(unique(unique_id)), 
            total_upfronts = sum(total_upfront_terms), 
            total_casuals = sum(total_casuals), 
            weeks = min(weeks),
            total_class_revenue = sum(minus_transaction_fee, na.rm = TRUE), 
            weekly_class_revenue = total_class_revenue / weeks)

combined_statistics <- overview_statistics %>% 
  left_join(per_student_statistics)


