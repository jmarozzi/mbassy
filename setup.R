#library(nousutils)
#library(nousstyle)
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
library(snakecase)
library(writexl)
library(gtable)
library(tidyverse)
library(purrr)
library(flextable)
library(officer)
#install.packages('officer')
#install.packages('ggtext')
library(ggtext)
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
