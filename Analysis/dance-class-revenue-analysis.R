#revenue data 
#### by term ####
g_weekly_class_revenue<- combined_statistics %>% 
  ggplot(aes(x = time_stamp, y = weekly_class_revenue)) + 
  geom_col() + 
  scale_y_continuous(labels = dollar_format()) + 
  labs( 
    x = "Term", 
    y = "Weekly class revenue", 
    title = "Weekly class revenue by term") + 
  geom_text(aes(x = time_stamp, y = weekly_class_revenue+50, label = dollar(weekly_class_revenue)))

g_weekly_class_revenue_gt <- "Weekly class revenue by term"

g_total_class_revenue<- combined_statistics %>% 
  ggplot(aes(x = time_stamp, y = total_class_revenue)) + 
  geom_col() + 
  scale_y_continuous(labels = dollar_format(), limits = c(0,15000)) + 
  labs( 
    x = "Term", 
    y = "Total class revenue",
    title = "Total class revenue by term") + 
  geom_text(aes(x = time_stamp, y = total_class_revenue+500, label = dollar(total_class_revenue)))

g_total_class_revenue_gt <- "Total class revenue by term"

g_boxplot_class_revenue_per_student <- per_student_total_class_registrations_and_revenue %>% 
  ggplot(aes(x = time_stamp, y =total_class_revenue))+
  geom_boxplot() + 
  labs(title = "Boxplot of the distribution of class revenue per student", 
       subtitle  = "Box with white space = 50% of students (Q1-Q3)\nLine in middle of white space = median",
       x = "Term", 
       y = "Class revenue per student") 

g_boxplot_class_revenue_per_student_gt <-"Boxplot of the distribution of class revenue per student"


## By FY/quarter 

##Will either need date recorded for each transaction or r

g_revenue_by_quarter <- all_terms_full_form %>% 
  group_by(quarter) %>% 
  summarise(revenue = sum(minus_transaction_fee)) %>% 
  ggplot(aes(x = quarter, y =revenue)) + 
  geom_col() +
  scale_y_continuous(labels = label_dollar()) +
  #scale_x_discrete(labels = c("Q1 22-23", "Q2 22-23", "Q3 22-23"))
  geom_text(aes(x = quarter, y =revenue +500, label = dollar(revenue))) + 
  labs(
    x = "Fiscal quarter", 
    y = "Dance class revenue", 
    title = "Dance class revenue by fiscal quarter",
    subtitle = "Financial year = FY22/23 i.e. fiscal year 2023"
  )
g_revenue_by_quarter_gt <- "Dance class revenue by fiscal quarter"




