# Process dance student & revenue data 

#### Join terms ####

all_terms_full_form <- rbind(t4_2022_min, t5_2022_min, t1_2023_min, t2_2023_min) %>% 
  mutate(time_stamp = as_factor(time_stamp),
         time_order = as_factor(time_order)) %>% 
  mutate(date = ymd(str_sub(date_time_paid, end =10L))) %>% 
  mutate(quarter = quarter(date, type = "year.quarter", fiscal_start = 7 )) %>% 
  mutate(fiscal_year_simple = as.numeric(str_sub(quarter,3,4))) %>% 
  mutate(fiscal_year_descriptive = str_c("FY", fiscal_year_simple-1, "-",fiscal_year_simple )) %>% 
  mutate(flag_empty_class_selection = ifelse(total_casuals == 0 & total_upfront_terms ==0,1,0 )) %>% 
  mutate(quarter_character = as.character(quarter))


term_number <- all_terms_full_form %>%
  select(time_stamp)


#### Create student level dataset #### 

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

#### Relative time order - need at add the next highest number ####

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
  mutate(relative_three_terms_ago =1)%>% 
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

student_dataset2 <- student_dataset1 %>% 
  left_join(student_term_attendance_for_join) %>% 
  group_by(unique_id) %>% 
  mutate(number_of_terms_attended = sum(term_2_2023, term_1_2023, term_5_2022, term_4_2022)) %>% 
  ungroup() %>% 
  left_join(current_term) %>% 
  left_join(previous_term) %>% 
  left_join(two_terms_ago) %>% 
  left_join (three_terms_ago) %>% 
  mutate(across(starts_with("relative"), .fns = ~ifelse(is.na(.x), 0, .x))) %>% 
  mutate(two_consecutive_current_previous = ifelse(relative_current_term == 1 & relative_previous_term == 1 & 
                                                     relative_two_terms_ago ==0, 1,0)) %>% 
  mutate(three_consecutive_previous = ifelse(relative_current_term == 1 & relative_previous_term == 1 & relative_two_terms_ago, 1,0)) %>% 
  mutate(not_current_but_any_previous = ifelse(relative_current_term ==0,1,0)) %>% 
  left_join(first_term) %>% 
  mutate(proportion_of_all_terms = round((number_of_terms_attended/ total_number_of_possible_terms), digits = 2)) %>% 
  mutate(active_flag = ifelse(relative_current_term ==1,1,0)) 

start_term_and_number_possible_terms <-student_dataset2 %>% 
  select(start_term,total_number_of_possible_terms ) %>% 
  group_by(start_term) %>% 
  slice(1) %>% 
  ungroup() %>% 
  arrange(total_number_of_possible_terms) %>% 
  mutate(start_term = as.character(start_term))

save(start_term_and_number_possible_terms, file = "data/start_term_and_number_possible_terms.R")


student_dataset <- student_dataset2 %>% 
  mutate(current_term = start_term_and_number_possible_terms$start_term[1], 
         previous_term = start_term_and_number_possible_terms$start_term[2]) %>% 
  mutate(student_registration_pattern = case_when( 
    start_term == current_term ~ "New students",
    two_consecutive_current_previous == 1 ~ "Returned students",
    three_consecutive_previous == 1 ~ "Loyal students",
    active_flag == 1 & start_term != current_term & relative_previous_term == 0 ~ "Resurrected students",
    active_flag == 0 & relative_previous_term == 1 ~ "Warm lead", 
    active_flag == 0 & relative_previous_term == 0 ~ "Dormant students")) %>% 
  mutate(student_registration_pattern = factor(student_registration_pattern, 
                                               levels = c("New students", 
                                                          "Returned students",
                                                          "Loyal students", 
                                                          "Resurrected students",
                                                          "Warm lead", 
                                                          "Dormant students")))
#### PER STUDENT PER TERM DATA #####

per_student_total_class_registrations_and_revenue <- all_terms_full_form %>% 
  group_by(time_stamp, unique_id) %>%
  summarise(total_upfronts = sum(total_upfront_terms), 
            total_casuals = sum(total_casuals), 
            total_class_revenue = sum(minus_transaction_fee, na.rm = TRUE),
            total_open_flag = sum(open_flag)) %>% 
  ungroup() %>% 
  mutate(any_upfront = factor(ifelse(total_upfronts >= 1, "Upfront", "No upfront")),
         any_upfront_numeric = ifelse(total_casuals >= 1, 1, 0),
         any_casual = factor(ifelse(total_casuals >= 1, "Casual", "No casual")),
         any_casual_numeric = ifelse(total_casuals >= 1, 1, 0)) 

summary_statistics_class_registrations_and_revenue <- per_student_total_class_registrations_and_revenue %>%
  group_by(time_stamp) %>% 
  summarise(number_students_with_upfronts = sum(any_upfront_numeric), 
            number_students_with_casuals = sum(any_casual_numeric), 
            mean_revenue_per_student = mean(total_class_revenue), 
            median_revenue_per_student = median(total_class_revenue), 
            minumum_revenue_per_student = min(total_class_revenue), 
            maximum_revenue_per_student = max(total_class_revenue), 
            open_week = sum(total_open_flag, na.rm = TRUE), 
            unique_students = length(unique(unique_id)))

#### PER TERM STATSITICS ####
overview_statistics <- all_terms_full_form %>% 
  group_by(time_stamp) %>% 
  summarise(number_students = length(unique(unique_id)), 
            total_upfronts = sum(total_upfront_terms), 
            total_casuals = sum(total_casuals), 
            weeks = min(weeks),
            total_class_revenue = sum(minus_transaction_fee, na.rm = TRUE), 
            weekly_class_revenue = total_class_revenue / weeks)

# number_of_students_with_any_upfront <- all_terms_full_form %>% 
#   group_by(unique_id,time_stamp) %>% 
#   summarise(number_upfront = sum(total_upfront_terms)) %>% 
#   ungroup() %>%
#   mutate(any_upfront = ifelse(number_upfront >0,1,0)) %>% 
#   group_by(time_stamp) %>% 
#   summarise(number_students_upfront = sum(any_upfront)) %>% 
#   ungroup()


combined_statistics <- overview_statistics %>% 
  left_join(summary_statistics_class_registrations_and_revenue) %>% 
  mutate(total_casuals_or_upfronts = total_upfronts + total_casuals)
