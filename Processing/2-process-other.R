## TERM DATES FOR FINANCIALS 
## ADD IN YOGA PROFITS
term_dates_for_whole_picture_financials <- start_term_and_number_possible_terms %>% 
  select(start_term) %>% 
  rename(time_stamp = start_term) %>% 
  mutate(
    term_start_date_for_non_class_transactions = case_when(
      time_stamp == "Term 4 2022*" ~ "2022-10-10", 
      time_stamp == "Term 5 2022*" ~ "2022-11-07", 
      time_stamp == "Term 1 2023" ~  "2023-01-09",
      time_stamp ==  "Term 2 2023" ~ "2023-03-13", 
      time_stamp == "Term 3 2023" ~ "2023-05-08", 
      time_stamp == "Term 4 2023" ~ "2023-06-05")
  ) %>% 
  mutate(
    term_end_date_for_non_class_transactions = case_when(
      time_stamp == "Term 4 2022*" ~ "2022-11-06", 
      time_stamp == "Term 5 2022*" ~ "2022-12-04", 
      time_stamp == "Term 1 2023" ~ "2023-03-12", 
      time_stamp ==  "Term 2 2023" ~ "2023-05-07",
      time_stamp == "Term 3 2023" ~ "2023-06-04", 
      time_stamp == "Term 4 2023" ~ "2023-07-30")
  ) %>% 
  rbind(tibble(
    time_stamp = c("Prep for 915 Collins 2022" , "Summer break 22/23"), 
    term_start_date_for_non_class_transactions = c("2022-09-12", "2022-12-05"),
    term_end_date_for_non_class_transactions = c("2022-10-09", "2023-01-08"))) %>% 
  mutate(
    across(.cols = c(term_start_date_for_non_class_transactions, term_end_date_for_non_class_transactions), 
           .fns = ~ ymd(.x)
    )) %>% 
  arrange(term_start_date_for_non_class_transactions) %>%
  mutate(teaching_or_break_time = case_when(
    time_stamp %in% c("Prep for 915 Collins 2022", "Summer break 22/23" ) ~ "break_time", 
    time_stamp %in% c("Term 4 2022*", "Term 5 2022*", "Term 1 2023", "Term 2 2023", "Term 3 2023", "Term 4 2023") ~ "teaching_time"
  )) %>% 
  mutate(t_start_for_interval = as_datetime(term_start_date_for_non_class_transactions, tz = "Australia/Melbourne"),
         t_end_for_interval = as_datetime(term_end_date_for_non_class_transactions, tz ="Australia/Melbourne")) %>% 
  mutate(t_start_for_interval = update(t_start_for_interval, hour = 00, minute= 00, second = 00 ),
         t_end_for_interval = update(t_end_for_interval, hour = 23, minute= 59, second = 59 )) %>% 
  
  mutate(interval = interval(start = t_start_for_interval,
                             end = term_end_date_for_non_class_transactions, 
                             tzone = "Australia/Melbourne") ) %>% 
  mutate(duration = as.duration(interval)) %>% 
  mutate(months = as.numeric(duration, "months")) %>% 
  mutate(weeks = as.numeric(duration, "weeks")) 

prep_for_915_collins_2022_interval <- term_dates_for_whole_picture_financials %>% 
  select(time_stamp, interval) %>% 
  filter(time_stamp == "Prep for 915 Collins 2022") %>% 
  pull(interval)

term_4_2022_interval <- term_dates_for_whole_picture_financials %>% 
  select(time_stamp, interval) %>% 
  filter(time_stamp == "Term 4 2022*") %>% 
  pull(interval)

term_5_2022_interval <- term_dates_for_whole_picture_financials %>% 
  select(time_stamp, interval) %>% 
  filter(time_stamp == "Term 5 2022*") %>% 
  pull(interval)

summer_22_23_interval <- term_dates_for_whole_picture_financials %>% 
  select(time_stamp, interval) %>% 
  filter(time_stamp == "Summer break 22/23") %>% 
  pull(interval)

term_1_2023_interval <- term_dates_for_whole_picture_financials %>% 
  select(time_stamp, interval) %>% 
  filter(time_stamp == "Term 1 2023") %>% 
  pull(interval)

term_2_2023_interval <- term_dates_for_whole_picture_financials %>% 
  select(time_stamp, interval) %>% 
  filter(time_stamp == "Term 2 2023") %>% 
  pull(interval)

term_3_2023_interval <- term_dates_for_whole_picture_financials %>% 
  select(time_stamp, interval) %>% 
  filter(time_stamp == "Term 3 2023") %>% 
  pull(interval)

term_4_2023_interval <- term_dates_for_whole_picture_financials %>% 
  select(time_stamp, interval) %>% 
  filter(time_stamp == "Term 4 2023") %>% 
  pull(interval)

# process daily report
daily_report_joined <- bind_rows(daily_report_2022, daily_report_2023) %>% 
  filter(!(drinks == 0 & merch == 0 & private_lessons == 0 & studio_hire ==0 & costs == 0)) %>% 
  mutate(date = dmy(date)) %>%
  mutate(quarter = quarter(date, type = "year.quarter", fiscal_start = 7 )) %>%
  mutate(fiscal_year_simple = as.numeric(str_sub(quarter,3,4))) %>%
  mutate(fiscal_year_descriptive = str_c("FY", fiscal_year_simple-1, "-",fiscal_year_simple)) %>%
    mutate(quarter_character = as.character(quarter)) %>% 
  mutate(across(
    .cols = c(drinks, merch, private_lessons, studio_hire, costs),
    .fns = ~ ifelse(is.na(.x),0, .x)
  )) %>%
  
  mutate(time_stamp = case_when( 
    date %within% prep_for_915_collins_2022_interval ~ "Prep for 915 Collins 2022",
    date %within% term_4_2022_interval ~ "Term 4 2022*", 
    date %within% term_5_2022_interval ~ "Term 5 2022*",
    date %within% summer_22_23_interval ~ "Summer break 22/23",
    date %within% term_1_2023_interval ~ "Term 1 2023", 
    date %within% term_2_2023_interval ~ "Term 2 2023" ))

###### REVENUE ####
#Daily
daily_revenue <- daily_report_joined %>% 
  select(date:private_lessons, time_stamp) %>% 
  pivot_longer(cols = drinks:private_lessons, names_to = "category", values_to = "value") %>% 
  filter(value >0)

#Dance class 
dance_class_revenue <- all_terms_full_form %>% 
  select(time_stamp, date,tdd_month_number, minus_transaction_fee) %>% 
  rename(value =minus_transaction_fee) %>% 
  mutate(category = "dance_class") 
#Yoga 
yoga_class_revenue <- yoga_term_2_2023 %>% 
  select(time_stamp, date, minus_transaction_fee) %>% 
  rename(value =minus_transaction_fee) %>% 
  mutate(category = "yoga") 
## ALL REVENUE

all_revenue <- bind_rows(daily_revenue, dance_class_revenue,yoga_class_revenue) %>% 
  mutate(quarter = quarter(date, type = "year.quarter", fiscal_start = 7 )) %>%
  mutate(fiscal_year_simple = as.numeric(str_sub(quarter,3,4))) %>%
  mutate(fiscal_year_descriptive = str_c("FY", fiscal_year_simple-1, "-",fiscal_year_simple)) %>%
  mutate(quarter_character = as.character(quarter))%>% 
  mutate(revenue_or_costs = "revenue")%>% 
  select(-quarter)  %>% 
  mutate(tdd_month_number = classify_date_TDD(date))
write.csv(all_revenue, file = "outputs/all_revenue.csv")

all_revenue %>% 
  group_by(category) %>% 
  summarise(
    dollar = sum(value)
  ) %>% 
  mutate(monthly = dollar/7)
  

####### COSTS ####

daily_costs <- daily_report_joined %>% 
  select(date, time_stamp, costs, cost_category) %>% 
  filter(costs > 0) %>% 
  rename(value = costs) %>% 
  mutate(value = -value) %>% 
  mutate(category = to_snake_case(cost_category)) %>% 
  select(-cost_category) %>% 
  mutate(tdd_date = classify_date_TDD(date))
  
 
## DANCE TEACHER COSTS
dance_profit_forms <- rbind(term_4_2022_dance_profit, term_5_2022_dance_profit, 
                            term_1_2023_dance_profit, term_2_2023_dance_profit) %>% 
  select(-overhead, -total_costs, -term_profit, - weekly_profit) %>% 
  mutate(time_stamp = as_factor(time_stamp),
         time_order = as_factor(time_order))


end_date_for_join <- term_dates_for_whole_picture_financials %>%  
  select(time_stamp, term_end_date_for_non_class_transactions)

dance_teacher_costs<- dance_profit_forms %>% 
  group_by(time_stamp) %>% 
  summarise(teacher_costs = sum(total_teacher_costs)) %>% 
  left_join(end_date_for_join) %>% 
  rename(date = term_end_date_for_non_class_transactions)


formatted_dance_teacher_costs <- dance_teacher_costs %>% 
  pivot_longer(cols = teacher_costs, names_to ="category", values_to = "value") %>% 
  mutate(value = -value)


##OVERHEAD COSTS
total_overheads_monthly <- overhead_costs %>% 
  summarise(overhead_costs_monthly = sum(per_month)) %>% 
  pull(overhead_costs_monthly)

overhead_costs_per_term <- term_dates_for_whole_picture_financials %>% 
  select(time_stamp,months) %>% 
  filter(time_stamp !="Prep for 915 Collins 2022") %>% 
  mutate(category = "overheads", 
         value = months *total_overheads_monthly) %>% 
  mutate(value = -value) %>% 
  left_join(end_date_for_join) %>% 
  rename(date = term_end_date_for_non_class_transactions) 
#total_overheads_quarterly <- total_overheads_monthly *3
#total_overheads_annually <- total_overheads_monthly * 12
#total_overheads_daily <- total_overheads_annually / 365


##All costs 
all_costs <- bind_rows(daily_costs, formatted_dance_teacher_costs, overhead_costs_per_term) %>% 
  mutate(quarter = quarter(date, type = "year.quarter", fiscal_start = 7 )) %>%
  mutate(fiscal_year_simple = as.numeric(str_sub(quarter,3,4))) %>%
  mutate(fiscal_year_descriptive = str_c("FY", fiscal_year_simple-1, "-",fiscal_year_simple)) %>%
  mutate(quarter_character = as.character(quarter)) %>% 
  select(-months) %>% 
  mutate(revenue_or_costs = "costs") %>% 
  select(-quarter)



all_revenue_and_costs <- bind_rows(all_revenue, all_costs) 

total_costs_and_revenue_current <- all_revenue_and_costs %>% 
  group_by(revenue_or_costs) %>% 
  summarise(value = sum(value))

total_revenue_current <- total_costs_and_revenue_current %>% 
  filter(revenue_or_costs == "revenue") %>% 
  pull(value)

total_cost_current <- total_costs_and_revenue_current %>% 
  filter(revenue_or_costs == "costs") %>% 
  pull(value)
  
profit_by_term <- all_revenue_and_costs %>% 
  group_by(time_stamp) %>% 
  summarise(profit = sum(value))

profit_by_quarter <- all_revenue_and_costs %>% 
  group_by(quarter_character) %>%
  summarise(profit = sum(value))

# total_profit <- all_revenue_and_costs %>%
#  summarise(profit = sum(value))

revenue_and_cost_by_month <- term_dates_for_whole_picture_financials %>% 
  select(time_stamp,term_start_date_for_non_class_transactions,term_end_date_for_non_class_transactions, months) %>%
  filter(time_stamp != "Prep for 915 Collins 2022") %>% 
  summarise(months = sum(months)) %>%
  mutate(total_cost_current = total_cost_current, 
         total_revenue_current = total_revenue_current) %>% 
  mutate(monthly_cost_current= total_cost_current/ months, 
         monthly_revenue_current = total_revenue_current/months)

current_monthly_cost <- revenue_and_cost_by_month %>% 
  pull(monthly_cost_current)

current_monthly_revenue <- revenue_and_cost_by_month %>% 
  pull(monthly_revenue_current)

  


