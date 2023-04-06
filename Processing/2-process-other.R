# process daily report & costs

# daily_report_joined <- rbind(daily_report_2022, daily_report_2023) %>% 
#   mutate(date = ymd(date)) %>% 
#   mutate(quarter = quarter(date, type = "year.quarter", fiscal_start = 7 )) %>% 
#   mutate(fiscal_year_simple = as.numeric(str_sub(quarter,3,4))) %>% 
#   mutate(fiscal_year_descriptive = str_c("FY", fiscal_year_simple-1, "-",fiscal_year_simple)) %>% 
#   mutate(across(
#     .cols = c(drinks_sold, merch_sold, studio_hire), 
#     .fns = ~ ifelse(is.na(.x),0, .x)
#   )) %>% 
#   mutate(quarter_character = as.character(quarter))


#great term by term revenue 

dance_profit_forms <- rbind(term_4_2022_dance_profit, term_5_2022_dance_profit, 
                            term_1_2023_dance_profit, term_2_2023_dance_profit) %>% 
  select(-overhead, -total_costs, -term_profit, - weekly_profit) %>% 
  mutate(time_stamp = as_factor(time_stamp),
         time_order = as_factor(time_order))



## DANCE TEACHER COSTS
teacher_cost_date <- tibble(
  time_stamp = c("Term 4 2022*", "Term 5 2022*", "Term 1 2023", "Term 2 2023"), 
  quarter_character = c("2023.2", "2023.2", "2023.3", "2023.3"))

dance_teacher_costs<- dance_profit_forms %>% 
  group_by(time_stamp) %>% 
  summarise(teacher_costs = sum(total_teacher_costs)) %>% 
  left_join(teacher_cost_date)


##OVERHEAD COSTS
total_overheads_monthly <- overhead_costs %>% 
  summarise(overhead_costs_monthly = sum(per_month)) %>% 
  pull((overhead_costs_monthly))

total_overheads_quarterly <- total_overheads_monthly *3


