# student analysis


g_unique_students <- combined_statistics %>% 
 ggplot() +
  geom_col(aes(x = time_stamp, y = number_students)) + 
  #geom_line(aes(x = time_stamp, y = number_students_with_upfronts, group = 2, color =nous_base_palette$hex[2])) + 
  #geom_line(aes(x = time_stamp, y = number_students_with_casuals, group = 3, color =nous_base_palette$hex[3])) +
  labs(x = "Term", y = "", title = "Number of unique students per term")+
  scale_y_continuous(limits = c(0,105))+
  #scale_color_discrete(name = "Legend", labels = c("Number of unique students", "Unique students with upfronts", "Unique students with casuals")) + 
  theme(legend.position = "bottom") + 
  geom_text(aes(x = time_stamp, y = number_students+3, label = number_students ))

g_number_of_terms_attended <- student_dataset %>% 
  ggplot(aes(x = number_of_terms_attended))+
  geom_bar() + 
  geom_text(aes(label=after_stat(count)),
            stat='count',
            nudge_y=4) + 
  labs(x = "Number of terms attended",
      y = "Number of students", 
      title = "Breakdown of unique students by number of terms attended")

student_dataset %>% 
  ggplot(aes(x = proportion_of_all_terms)) + 
  geom_bar()
