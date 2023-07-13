# Targets tracker 

## Set up 6 months from TDD 
#Progress towards targets charts 

#g_tdd_average_monthly_revenue
tdd_revenue_overall_summary <- all_revenue %>%
  filter(!is.na(tdd_month_number)) %>%
  # mutate(date = mdy(date)) %>%
  group_by(tdd_month_number) %>%
  summarise(total_revenue = sum(value, na.rm = TRUE))%>%
  summarise(average_monthly_revenue = mean(total_revenue, na.rm = TRUE))


# Set the target revenue



target_revenue <- 8200


# Calculate the difference
difference <- tdd_revenue_overall_summary$average_monthly_revenue - target_revenue
percentage_difference <- (tdd_revenue_overall_summary$average_monthly_revenue / target_revenue - 1) * 100


# Create a variable to indicate if the actual average monthly revenue is equal to or above the target revenue
tdd_revenue_overall_summary$color_condition <- tdd_revenue_overall_summary$average_monthly_revenue >= target_revenue

# Update the ggplot code with the new color condition
g_tdd_average_monthly_revenue <- ggplot(tdd_revenue_overall_summary) +
  geom_col(aes(x = "Actual", y = average_monthly_revenue, fill = color_condition)) +
  scale_fill_manual(values = c("steelblue", "green"), guide = FALSE) +
  geom_hline(aes(yintercept = target_revenue), color = "red", linetype = "dashed", size = 1) +
  geom_label(aes(x = "Actual", y = average_monthly_revenue - 500, label = str_c("Actual: ", scales::dollar(average_monthly_revenue, accuracy = 0.1)))) +
  geom_label(aes(x = "Actual", y = target_revenue, label = str_c("Target: ", scales::dollar(target_revenue, accuracy = 0.1)))) +
  scale_y_continuous(labels = scales::dollar_format(), limits = c(0, 10000)) +
  labs(title = "Overall Average Monthly Revenue vs Target",
       x = "",
       y = "Average monthly revenue") +
  theme_minimal() +
  theme(axis.title.x = element_blank()) +
  annotate("richtext",
           x = 1, y = target_revenue + 1000,
           label = glue::glue("<b>Difference between actual and target:</b> {scales::dollar(difference, accuracy = 0.01)} ({sprintf('%.2f', percentage_difference)}%)"),
           fill = "white",
           label.color = "black",
           hjust = 0.5,
           size = 3)


#### g_tdd_cumulative_total ####
tdd_revenue_monthly <- all_revenue %>%
  filter(!is.na(tdd_month_number)) %>%
  group_by(tdd_month_number) %>%
  summarise(total_revenue = sum(value, na.rm = TRUE))

# Calculate the expected total revenue
highest_tdd_month <- max(tdd_revenue_monthly$tdd_month_number)
expected_total_revenue <- target_revenue * highest_tdd_month

# Prepare the data for the waterfall chart
waterfall_data <- tdd_revenue_monthly %>%
  mutate(end = cumsum(total_revenue),
         start = lag(end, default = 0))

# Generate the waterfall chart
g_tdd_cumulative_total <- ggplot(waterfall_data) +
  geom_rect(aes(x = tdd_month_number, xmin = tdd_month_number - 0.45, xmax = tdd_month_number + 0.45,
                ymin = start, ymax = end), fill = "steelblue") +
  geom_text(aes(x = tdd_month_number, y = end, 
                label = scales::dollar(end, accuracy=1)), vjust = -0.5) +
  geom_hline(aes(yintercept = expected_total_revenue), color = "red", linetype = "dashed", size = 1) +
  geom_label(aes(x = highest_tdd_month / 2, y = expected_total_revenue, 
                 label = scales::dollar(expected_total_revenue, accuracy=1)), color = "red") +
  geom_label(aes(x = highest_tdd_month/2, y = expected_total_revenue + 5000, 
           label = paste0("Difference in actual and expected cumulative revenue: ", scales::dollar(max(end) - expected_total_revenue, accuracy=1)))) +
  scale_y_continuous(labels = scales::dollar_format() ) +
  scale_x_continuous(breaks = seq(1,max(highest_tdd_month)))+
  labs(title = "Revenue progress towards cumulative total revenue target in first 6 months @ TDD",
       x = "TDD Month",
       y = "Cumulative Revenue") +
  theme_minimal()



##### g_tdd_monthly_revenue ####

# Create a new variable to indicate if the actual total revenue is equal to or above the target revenue
tdd_revenue_monthly$color_condition <- tdd_revenue_monthly$total_revenue >= target_revenue

# Update the ggplot code with the new color condition
g_tdd_monthly_revenue <- ggplot(tdd_revenue_monthly, aes(x = tdd_month_number, y = total_revenue, fill = color_condition)) +
  geom_col() +
  scale_fill_manual(values = c("steelblue", "green"), guide = FALSE) +
  geom_hline(yintercept = target_revenue, linetype = "dashed", color = "red", size = 1) +
  geom_label(aes(x = 3.5, y = target_revenue, label = str_c("Target: ", scales::dollar(target_revenue, accuracy = 0.1)))) +
  scale_y_continuous(labels = scales::dollar_format(), limits = c(0, 10000)) +
  labs(title = "Progress Towards Average Monthly Revenue Target",
       x = "TDD Month Number",
       y = "Total monthly revenue") +
  theme_minimal() + 
  scale_x_continuous(breaks = seq(1,6))+
  geom_text(aes(x = tdd_month_number, y = total_revenue + 250, label = scales::dollar(total_revenue, accuracy = 0.1)))



##### g_tdd_headline_dance_class_revenue #####

#g_tdd_average_monthly_revenue
tdd_revenue_dance_class_summary <- all_revenue %>%
  filter(!is.na(tdd_month_number)) %>%
  filter(category == "dance_class") %>% 
  group_by(tdd_month_number) %>%
  summarise(total_revenue = sum(value, na.rm = TRUE))%>%
  summarise(average_monthly_revenue = mean(total_revenue, na.rm = TRUE))

# Set the target revenue
target_dance_revenue <- 7745

# Calculate the difference
difference_dance <- tdd_revenue_dance_class_summary$average_monthly_revenue - target_dance_revenue
percentage_difference_dance <- (tdd_revenue_dance_class_summary$average_monthly_revenue / target_dance_revenue - 1) * 100

# Create a variable to indicate if the actual average monthly dance class revenue is equal to or above the target dance revenue
tdd_revenue_dance_class_summary$color_condition <- tdd_revenue_dance_class_summary$average_monthly_revenue >= target_dance_revenue

# Update the ggplot code with the new color condition
g_tdd_headline_dance_class_revenue <- ggplot(tdd_revenue_dance_class_summary) +
  geom_col(aes(x = "Actual", y = average_monthly_revenue, fill = color_condition)) +
  scale_fill_manual(values = c("steelblue", "green"), guide = "none") +
  geom_hline(aes(yintercept = target_dance_revenue), color = "red", linetype = "dashed", size = 1) +
  geom_label(aes(x = "Actual", y = average_monthly_revenue - 500, label = str_c("Actual: ", scales::dollar(average_monthly_revenue, accuracy = 0.1)))) +
  geom_label(aes(x = "Actual", y = target_dance_revenue, label = str_c("Target: ", scales::dollar(target_dance_revenue, accuracy = 0.1)))) +
  scale_y_continuous(labels = scales::dollar_format(), limits = c(0, 9000)) +
  labs(title = "Overall Average Monthly Dance Class Revenue vs Target",
       x = "",
       y = "Average monthly dance class revenue") +
  theme_minimal() +
  theme(axis.title.x = element_blank()) +
  annotate("richtext",
           x = 1, y = target_dance_revenue + 1000,
           label = glue::glue("<b>Difference between actual and target:</b> {scales::dollar(difference_dance, accuracy = 0.01)} ({sprintf('%.2f', percentage_difference_dance)}%)"),
           fill = "white",
           label.color = "black",
           hjust = 0.5,
           size = 3)



##### g_tdd_cumulative_dance_class_total ##### 


tdd_revenue_monthly_dance_class <- all_revenue %>%
  filter(!is.na(tdd_month_number)) %>%
  filter(category == "dance_class") %>% 
  group_by(tdd_month_number) %>%
  summarise(total_revenue = sum(value, na.rm = TRUE))

# Calculate the expected total revenue
highest_tdd_month_dance_class <- max(tdd_revenue_monthly_dance_class$tdd_month_number)
expected_total_revenue_dance_class <- target_dance_revenue * highest_tdd_month



# Prepare the data for the waterfall chart
waterfall_data_dance_class <- tdd_revenue_monthly_dance_class %>%
  mutate(end = cumsum(total_revenue),
         start = lag(end, default = 0))

# Generate the waterfall chart
g_tdd_cumulative_dance_class_total <- ggplot(waterfall_data_dance_class) +
  geom_rect(aes(x = tdd_month_number, xmin = tdd_month_number - 0.45, xmax = tdd_month_number + 0.45,
                ymin = start, ymax = end), fill = "steelblue") +
  geom_text(aes(x = tdd_month_number, y = end, 
                label = scales::dollar(end, accuracy=1)), vjust = -0.5) +
  geom_hline(aes(yintercept = expected_total_revenue_dance_class), color = "red", linetype = "dashed", size = 1) +
  geom_label(aes(x = highest_tdd_month / 2, y = expected_total_revenue_dance_class, 
                 label = scales::dollar(expected_total_revenue_dance_class, accuracy=1)), color = "red") +
  geom_label(aes(x = highest_tdd_month/2, y = expected_total_revenue_dance_class + 5000, 
                 label = paste0("Diff in actual v. expected cumulative revenue: ", 
                                scales::dollar(max(end) - expected_total_revenue_dance_class, accuracy=1)))) +
  scale_y_continuous(labels = scales::dollar_format() ) +
  scale_x_continuous(breaks = seq(1,max(highest_tdd_month)))+
  labs(title = "Revenue progress towards cumulative total dance class revenue target",
       x = "TDD Month",
       y = "Cumulative Dance Class Revenue") +
  theme_minimal()


##### g_tdd_number_unique_enrollments #####

students_per_term <- all_terms_full_form %>%
  filter(tdd_month_number != "NA") %>%
  group_by(time_stamp) %>%
  summarise(unique_students = n_distinct(unique_id)) %>%
  mutate(target_students = 100)

g_tdd_number_unique_enrollments<- ggplot(students_per_term, aes(x = time_stamp, y = unique_students)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(y = unique_students, label = unique_students), vjust = -0.5) +
  geom_hline(aes(yintercept = target_students), linetype = "dashed", color = "red") +
  geom_text(aes(x = Inf, y = target_students, label = "Target: 100 students"), hjust = 1.1, vjust = -0.5, color = "red") +
  labs(title = "Progress Towards Target Number of Students per Term",
       x = "Term",
       y = "Unique Students") +
  theme_minimal()


##### g_tdd_upfront_distribution #####

upfront_classes <- all_terms_full_form %>%
  filter(tdd_month_number != "NA") %>%
  group_by(time_stamp, unique_id) %>%
  summarise(total_upfront = sum(total_upfront_terms),
            total_casual = sum(total_casuals)) %>%
  mutate(enrollment_category = case_when(
    total_upfront >= 3 ~ "3 or more upfront classes",
    total_upfront == 2 ~ "2 upfront classes",
    total_upfront == 1 ~ "1 upfront class",
    total_casual >= 1 ~ "No upfront classes, at least 1 casual class"
  )) %>%
  filter(!is.na(enrollment_category)) %>% 
  group_by(time_stamp, enrollment_category) %>%
  summarise(students = n()) %>%
  ungroup() 

# Set the targets for each enrollment category
targets <- data.frame(
  enrollment_category = c("3 or more upfront classes",
                          "2 upfront classes",
                          "1 upfront class",
                          "No upfront classes, at least 1 casual class"),
  target_students = c(6, 17, 26, 51)
)

# Combine the actual and target data
upfront_classes <- left_join(upfront_classes, targets, by = "enrollment_category")


# Summarize the data frame to get the actual number of students for each enrollment category and term
upfront_classes_summary <- upfront_classes %>%
  group_by(time_stamp, enrollment_category) %>%
  summarise(students = sum(students), target_students = first(target_students))

# Create a new variable to indicate if the actual number of students is equal to or above the target
upfront_classes_summary$color_condition <- upfront_classes_summary$students >= upfront_classes_summary$target_students

# Plot the graph with facets and requested modifications
g_tdd_upfront_distribution <- ggplot(upfront_classes_summary, aes(x = enrollment_category, y = students, fill = color_condition)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("grey", "green"), guide = FALSE) +
  geom_text(aes(label = paste("Actual:", students, "\nTarget:", target_students), y = 85), size =2) +
  geom_point(aes(y = target_students), position = position_dodge(width = 1), color = "blue", size = 2, shape = 4) +
  labs(title = "Progress Towards Enrollment Targets per Term",
       x = "Enrollment Category",
       y = "Students") +
  theme_minimal() +
  theme(legend.position = "none") +
  facet_wrap(~ time_stamp, ncol = 1) +
  ylim(0, max(upfront_classes$students, upfront_classes$target_students) * 2)


##### g_tdd_avg_monthly_dance_class_revenue ########


# Create a new variable to indicate if the actual total revenue is equal to or above the target revenue
tdd_revenue_monthly_dance_class$color_condition <- tdd_revenue_monthly_dance_class$total_revenue >= target_dance_revenue

# Update the ggplot code with the new color condition
g_tdd_monthly_dance_class_revenue <- ggplot(tdd_revenue_monthly_dance_class, aes(x = tdd_month_number, y = total_revenue, fill = color_condition)) +
  geom_col() +
  scale_fill_manual(values = c("steelblue", "green"), guide = FALSE) +
  geom_hline(yintercept = target_dance_revenue, linetype = "dashed", color = "red", size = 1) +
  geom_label(aes(x = 3.5, y = target_dance_revenue, label = str_c("Target: ", scales::dollar(target_dance_revenue, accuracy = 0.1)))) +
  scale_y_continuous(labels = scales::dollar_format(), limits = c(0, 10000)) +
  labs(title = "Progress Towards Average Monthly Revenue Target",
       x = "TDD Month Number",
       y = "Total monthly revenue") +
  theme_minimal() + 
  scale_x_continuous(breaks = seq(1,6))+
  geom_text(aes(x = tdd_month_number, y = total_revenue + 250, label = scales::dollar(total_revenue, accuracy = 0.1)))


##### g_monthly_dance_class_revenue_per_student #####


tdd_dance_revenue <- all_terms_full_form %>%
  filter(!is.na(tdd_month_number) & minus_transaction_fee > 0)


# Calculate the total dance revenue per student in each TDD month
tdd_dance_revenue_per_student <- tdd_dance_revenue %>%
  group_by(tdd_month_number, unique_id) %>%
  summarize(total_dance_revenue = sum(minus_transaction_fee)) %>%
  ungroup()

# Calculate the average dance class revenue per unique student for each TDD month
tdd_dance_revenue_summary <- tdd_dance_revenue_per_student %>%
  group_by(tdd_month_number) %>%
  summarize(average_dance_revenue = mean(total_dance_revenue)) %>%
  ungroup()

# Create a new variable to indicate if the average dance class revenue is equal to or above the target
tdd_dance_revenue_summary$color_condition <- tdd_dance_revenue_summary$average_dance_revenue >= 79.03

# Create the bar chart

g_tdd_dance_revenue_per_student <- ggplot(tdd_dance_revenue_summary, aes(x = tdd_month_number, y = average_dance_revenue)) +
  geom_bar(aes(fill = color_condition), stat = "identity") +
  scale_fill_manual(values = c("steelblue", "green"), guide = FALSE) +
  geom_text(aes(y = average_dance_revenue, label = dollar(round(average_dance_revenue, 2))), vjust = -0.5) +
  geom_hline(aes(yintercept = 79.03), linetype = "dashed", color = "red") +
  geom_label(aes(x = max(tdd_month_number)/2, y = 79.03, label = paste0("Target: ", dollar(79.03))), 
             hjust = 0, color = "red") +
  labs(title = "Progress Towards Monthly Dance Class Revenue per Student Target",
       x = "TDD Month Number",
       y = "Average Dance Class Revenue per Student") +
  theme_minimal() +
  scale_y_continuous(labels = dollar)

# Display the chart
g_tdd_dance_revenue_per_student


