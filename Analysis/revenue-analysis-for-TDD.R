#Revenue analysis

revenue_by_proportion <- all_revenue %>% 
  group_by(category) %>% 
  summarise(value = sum(value)) %>% 
  ungroup() %>% 
  mutate(total = sum(value)) %>% 
  mutate(proportion = value/total) %>% 
  mutate(caption = percent(proportion)) %>% 
  arrange(proportion) %>% 
  mutate(category = fct_reorder(factor(category), proportion)) 

revenue_by_proportion %>% 
  ggplot(aes(x = "", y = value, fill = category)) + 
  geom_col(position = "stack") + 
  geom_text(aes(x = "", y = value, label = caption ), position = position_stack(0.5))


summary(summary_statistics_class_registrations_and_revenue)
  


#On average 64 unique students a term 
summary_statistics_class_registrations_and_revenue %>% 
  ggplot(aes(x = time_stamp, y = unique_students)) + 
  geom_col()+ 
  geom_hline(aes(yintercept = 64)) 

summary(per_student_total_class_registrations_and_revenue)
# average spend of $134 (this is all terms)
# average dance class revenue per term 64*134 = $8576
# over four terms that is $34,304 (this is not far off the actual of $34,446)
summary_statistics_class_registrations_and_revenue$unique_students

### RESTRICTING JUST TO TERM 1 AND 2
summary_statistics_class_registrations_and_revenue %>% 
  filter(time_stamp %in% c("Term 1 2023", "Term 2 2023")) %>% 
  summarise(avg_unique_students = mean(unique_students ))
#In T1 and T2 average of 83 students per term 



months_per_term <- term_dates_for_whole_picture_financials %>% 
  select(time_stamp, months)


#Get monthly spend per student  
monthly_revenue_per_student <- all_terms_full_form %>% 
 # filter(time_stamp %in% c("Term 1 2023", "Term 2 2023")) %>% 
  group_by(time_stamp, unique_id) %>% 
  summarise(total_revenue = sum(minus_transaction_fee)) %>% 
  filter(total_revenue != 0)  %>% 
  left_join(months_per_term) %>% 
  mutate(revenue_per_month = total_revenue/months)

summary(monthly_revenue_per_student)
#average is $97 per month | $194 for 2 months or approx a term 
#median is $85 per month | $170 for 2 months or approx a term

#Total revenue per month is $5807
# Dance classes account for 87.1% of total revenue 
# Dance classes equal $5058 of revenue per month

### To get the number of students this is equal to: 
# - using the median monthly $ per student: 5058/85 = 59
# - using the average monthly $ per student: 5058/97 = 52


#Get average number of students & spend for just term 1 & 2
t_1_and_2_revenue_per_student <- all_terms_full_form %>%
  filter(time_stamp %in% c("Term 1 2023", "Term 2 2023")) %>%
  group_by(time_stamp, unique_id) %>%
  summarise(total_revenue = sum(minus_transaction_fee)) %>%
  filter(total_revenue != 0) %>% 
  left_join(months_per_term) %>% 
  mutate(revenue_per_month = total_revenue/months)

summary(t_1_and_2_revenue_per_student$total_revenue)
# Median spend per term is $155 per stue3n5 
# Average spend per term is $155 per student 
# Median spend per month is $79
# Average spend per month is $80


### To get the number of students this is equal to: 
# - using the median monthly $ per student: 5058/78.5 = 64
# - using the average monthly $ per student: 5058/80 = 63




###### CASUAL VS. UPFRONT $ ####### 
all_terms_full_form %>% 
  summarise(upfront_dollars = sum(total_upfront_dollars), 
            casual_dollars = sum(total_casual_dollars)) %>% 
  pivot_longer(cols = everything(), names_to = "subcategory", values_to = "revenue") %>% 
  mutate(all_revenue = sum(revenue)) %>% 
  mutate(prop = revenue/all_revenue)


############ DISTRIBUTION OF CLASS UPTAKES PER TERM ############################
##Restrict sample to Term 1 and 2 as these are the most similar in lenght
number_casual_and_upfront_per_student_per_term <- all_terms_full_form %>%
  filter(time_stamp %in% c("Term 1 2023", "Term 2 2023")) %>%
  filter(open_week == "n") %>%
  group_by(time_stamp, unique_id) %>%
  summarise(casuals = sum(total_casuals), 
            upfronts = sum(total_upfront_terms))%>% 
  filter(! (casuals == 0 & upfronts == 0))


table_casual_vs_upfronts_vs_upfronts <- table(number_casual_and_upfront_per_student_per_term$casuals, 
                                              number_casual_and_upfront_per_student_per_term$upfronts)
prop.table(table_casual_vs_upfronts_vs_upfronts) %>% 
  round(digits = 3)

