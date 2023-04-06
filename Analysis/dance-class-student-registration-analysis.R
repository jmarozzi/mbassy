# student 

save(student_dataset, file = "data/student_dataset.R")

g_number_of_terms_attended <- student_dataset %>% 
  ggplot(aes(x = number_of_terms_attended))+
  geom_bar() + 
  geom_text(aes(label=after_stat(count)),
            stat='count',
            nudge_y=4) + 
  labs(x = "Total number of terms attended",
       y = "Number of students", 
       title = "Breakdown of unique students by number of terms attended")


g_number_of_terms_attended_gt <- "Breakdown of unique students by number of terms attended"

save(per_student_total_class_registrations_and_revenue, file = "data/per_student_total_class_registrations_and_revenue.R")

g_unique_students_by_term<- per_student_total_class_registrations_and_revenue %>% 
  count(time_stamp, any_upfront) %>% 
  group_by(time_stamp) %>% 
  mutate(perc = n / sum(n)) %>% 
  ggplot(aes(x = time_stamp, y = n, fill = factor(any_upfront))) + 
  geom_col() + 
  geom_text(aes(x = time_stamp, y = n, label = scales::percent(perc)), position = position_stack(vjust = 0.5), color = "white") +
  labs(x = "Term", 
       y = "Number of unique students", 
       fill = "Legend",
       title = "Number of unique students per term broked down by upfront registrations") + 
  scale_x_discrete(labels = label_wrap(7)) + 
  theme(legend.position = "bottom") 


g_unique_students_by_term_gt<- "Number of unique students per term broken down by upfront registrations(since Term 4 2022)"

save(all_terms_full_form, file = "data/all_terms_full_form.R")

g_registrations<- all_terms_full_form %>% 
  select(time_stamp, total_casuals, total_upfront_terms) %>% 
  pivot_longer(cols = c(total_casuals, total_upfront_terms), 
               names_to = "registration_type",
               values_to = "number_of_registrations") %>% 
  mutate(registration_type = ifelse(registration_type == "total_casuals", "Casual", "Upfront Term")) %>% 
  group_by(time_stamp,registration_type) %>% 
  summarise(number_of_registrations = sum(number_of_registrations)) %>% 
  ungroup() %>% 
  group_by(time_stamp) %>% 
  mutate(prop = number_of_registrations/ sum(number_of_registrations)) %>% 
  ggplot(aes(x = time_stamp, y = number_of_registrations, fill = registration_type)) + 
  geom_col() + 
  geom_text(aes(x = time_stamp, y = number_of_registrations, label = scales::percent(prop)), 
            position = position_stack(vjust = 0.5), color = "white") +
  labs(x = "Term", 
       y = "Number of registrations", 
       fill = "Legend",
       title = "Number of registrations - casual vs. upfront") + 
  scale_x_discrete(labels = label_wrap(7)) + 
  theme(legend.position = "bottom") 

g_registrations_gt <- "Number of registrations - casual vs. upfront"

#### Term by term analysis ####
#1 term


total_1 <- student_dataset %>%
  filter(start_term == start_term_and_number_possible_terms$start_term[1]) %>% 
  nrow()

g_1_term_prop <- student_dataset %>%
  filter(start_term == start_term_and_number_possible_terms$start_term[1]) %>%
  ggplot(aes(x = proportion_of_all_terms)) + 
  geom_bar() + 
  scale_x_continuous(labels = percent_format(), breaks = c(0,1, by =1/1))+
  labs(x = "% of all possible terms attended", 
       y = "Number of students starting in term", 
       title = start_term_and_number_possible_terms$start_term[1], 
       subtitle = str_c("Maximum possible number of terms: ", start_term_and_number_possible_terms$total_number_of_possible_terms[1]), 
       caption = str_c("Total of ", total_1, 
                       " new students started in ", 
                       start_term_and_number_possible_terms$start_term[1])) + 
  geom_text(
    aes(label=after_stat(count)),
    stat='count',
    nudge_x=-0.05,
    nudge_y=1)+
  geom_text(
    aes(label= str_c("(", percent(after_stat(prop)), ")"), group=1),
    stat='count',
    nudge_x=0.05,
    nudge_y=1) +
  theme(plot.caption = element_text(hjust = 0, face = "bold"))

#2 terms

total_2 <- student_dataset %>%
  filter(start_term == start_term_and_number_possible_terms$start_term[2]) %>% 
  nrow()

g_2_term_prop <- student_dataset %>%
  filter(start_term == start_term_and_number_possible_terms$start_term[2]) %>%
  ggplot(aes(x = proportion_of_all_terms)) + 
  geom_bar() + 
  scale_x_continuous(labels = percent_format(round(seq(0,1, by =1/2), digits = 1)), breaks = seq(0,1, by =1/2) )+
  labs(x = "Proportion of all possible terms attended", 
       y = "Number of students starting in term", 
       title = start_term_and_number_possible_terms$start_term[2], 
       subtitle = str_c("Maximum possible number of terms: ", start_term_and_number_possible_terms$total_number_of_possible_terms[2]), 
       caption = str_c("Total of ", total_2, " new students started in ", 
                       start_term_and_number_possible_terms$start_term[2])) + 
  geom_text(
    aes(label=after_stat(count)),
    stat='count',
    nudge_x=-0.05,
    nudge_y=1)+
  geom_text(
    aes(label= str_c("(", percent(after_stat(prop)), ")"), group=1),
    stat='count',
    nudge_x=0.05,
    nudge_y=1) +
  theme(plot.caption = element_text(hjust = 0, face = "bold"))

#Three terms 
total_3 <- student_dataset %>%
  filter(start_term == start_term_and_number_possible_terms$start_term[3]) %>% 
  nrow()
g_3_term_prop <- student_dataset %>%
  filter(start_term == start_term_and_number_possible_terms$start_term[3]) %>%
  ggplot(aes(x = proportion_of_all_terms)) + 
  geom_bar() + 
  scale_x_continuous(labels = percent_format(round(seq(0,1, by =1/3), digits =1)), breaks = seq(0,1, by =1/3) )+
  labs(x = "% of all possible terms attended", 
       y = "Number of students starting in term", 
       title = start_term_and_number_possible_terms$start_term[3], 
       subtitle = str_c("Maximum possible number of terms: ", start_term_and_number_possible_terms$total_number_of_possible_terms[3]), 
       caption = str_c("Total of ", total_3, 
                       " new students started in ", 
                       start_term_and_number_possible_terms$start_term[3])) + 
  geom_text(
    aes(label=after_stat(count)),
    stat='count',
    nudge_x=-0.05,
    nudge_y=0.5)+
  geom_text(
    aes(label= str_c("(", percent(after_stat(prop)), ")"), group=1),
    stat='count',
    nudge_x=0.05,
    nudge_y=0.5) +
  theme(plot.caption = element_text(hjust = 0, face = "bold")) 

# Four terms 
total_4 <- student_dataset %>%
  filter(start_term == start_term_and_number_possible_terms$start_term[4]) %>% 
  nrow()

g_4_term_prop <- student_dataset %>%
  filter(start_term == start_term_and_number_possible_terms$start_term[4]) %>%
  ggplot(aes(x = proportion_of_all_terms)) + 
  geom_bar() + 
  scale_x_continuous(labels = percent_format(round(seq(0,1, by =1/4), digits = 1)), 
                     breaks = seq(0,1, by =1/4) )+
  labs(x = "% of all possible terms attended", 
       y = "Number of students starting in term", 
       title = start_term_and_number_possible_terms$start_term[4], 
       subtitle = str_c("Maximum possible number of terms: ", start_term_and_number_possible_terms$total_number_of_possible_terms[4]), 
       caption = str_c("Total of ", total_4, 
                       " new students started in ", 
                       start_term_and_number_possible_terms$start_term[4])) + 
  geom_text(
    aes(label=after_stat(count)),
    stat='count',
    nudge_x=-0.05,
    nudge_y=1)+
  geom_text(
    aes(label= str_c("(", percent(after_stat(prop)), ")"), group=1),
    stat='count',
    nudge_x=0.05,
    nudge_y=1)+
  theme(plot.caption = element_text(hjust = 0, face = "bold")) 


#### registration patterns ####

g_student_registration_pattern <- student_dataset %>% 
  mutate(active_flag = ifelse(active_flag ==1, "Active", "Inactive")) %>% 
  ggplot(aes(x = student_registration_pattern, fill = active_flag)) + 
  geom_bar() + 
  geom_text(aes(
    label = percent(after_stat(prop)), group = 1), 
    stat = 'count', 
    nudge_y = 3) + 
  scale_x_discrete(label = wrap_format(5)) + 
  labs(x = "Student registration pattern",
       y = "Number of students", 
       caption = str_c("Total unique of students since Term 4 2022: ", nrow(student_dataset)),
       subtitle =  str_c("Current term: ", start_term_and_number_possible_terms$start_term[1]),
       title = "Breakdown of students by registration pattern",
       fill = "Legend") + 
  theme(plot.caption = element_text(hjust = 0, face = "bold"))

g_student_registration_pattern_title <- "Breakdown of students by registration pattern"





