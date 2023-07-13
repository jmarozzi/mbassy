#Class analysis 


t4_weeks <- term_dates_for_whole_picture_financials %>% 
  filter(time_stamp == "Term 4 2022*") %>% 
  pull(weeks) %>% 
  round()

t5_weeks <- term_dates_for_whole_picture_financials %>% 
  filter(time_stamp == "Term 5 2022*") %>% 
  pull(weeks) %>% 
  round()

t1_weeks <- term_dates_for_whole_picture_financials %>% 
  filter(time_stamp == "Term 1 2023") %>% 
  pull(weeks) %>% 
  round()


t2_weeks <- term_dates_for_whole_picture_financials %>% 
  filter(time_stamp == "Term 2 2023") %>% 
  pull(weeks) %>% 
  round()

class_names <- read_xlsx(path = "data/class_names.xlsx")


t4_class <- term_4_2022 %>%
  filter(open_week == "n") %>% 
  select(starts_with("x")) %>% 
  pivot_longer(cols  = starts_with("x"), names_to = "class") %>% 
  filter(!is.na(value)) %>% 
  group_by(class,value) %>% 
  tally() %>% 
  pivot_wider(names_from = "value", values_from = n) %>% 
  mutate(
    Casual = replace_na(Casual, 0),
    `Upfront Term` = replace_na(`Upfront Term`, 0),
    upfront_equivalent_casual = Casual / t4_weeks) %>% 
    mutate(
      total_upfront_equivalent = `Upfront Term` + upfront_equivalent_casual) %>% 
  mutate(class = str_c("t4_", class))

t5_class <- term_5_2022%>% 
  filter(open_week == "n") %>% 
  select(starts_with("x")) %>% 
  pivot_longer(cols  = starts_with("x"), names_to = "class") %>% 
  filter(!is.na(value)) %>% 
  group_by(class,value) %>% 
  tally()%>% 
  pivot_wider(names_from = "value", values_from = n) %>% 
  mutate(
    Casual = replace_na(Casual, 0),
    `Upfront Term` = replace_na(`Upfront Term`, 0),
    upfront_equivalent_casual = Casual / t4_weeks) %>% 
  mutate(
    total_upfront_equivalent = `Upfront Term` + upfront_equivalent_casual
  )%>% 
  mutate(class = str_c("t5_", class))

t1_class <- term_1_2023%>% 
  filter(open_week == "n") %>% 
  select(starts_with("x")) %>% 
  pivot_longer(cols  = starts_with("x"), names_to = "class") %>% 
  filter(!is.na(value)) %>% 
  group_by(class,value) %>% 
  tally()%>% 
  pivot_wider(names_from = "value", values_from = n) %>% 
  mutate(
    Casual = replace_na(Casual, 0),
    `Upfront Term` = replace_na(`Upfront Term`, 0),
    upfront_equivalent_casual = Casual / t4_weeks) %>% 
  mutate(
    total_upfront_equivalent = `Upfront Term` + upfront_equivalent_casual
  ) %>% 
  mutate(class = str_c("t1_", class))


t2_class <- term_2_2023 %>% 
  filter(open_week == "n") %>% 
  select(starts_with("x")) %>% 
  pivot_longer(cols  = starts_with("x"), names_to = "class") %>% 
  filter(!is.na(value)) %>% 
  group_by(class,value) %>% 
  tally()%>% 
  pivot_wider(names_from = "value", values_from = n)%>% 
  mutate(
    Casual = replace_na(Casual, 0),
    `Upfront Term` = replace_na(`Upfront Term`, 0),
    upfront_equivalent_casual = Casual / t4_weeks) %>% 
  mutate(
    total_upfront_equivalent = `Upfront Term` + upfront_equivalent_casual
  )%>% 
  mutate(class = str_c("t2_", class))

all_class <- bind_rows(t4_class, t5_class, t1_class, t2_class) %>% 
  left_join(class_names) %>% 
  mutate(term = case_when(
    str_detect(class, "t4") ~ "Term 4 2022*",
    str_detect(class, "t5") ~ "Term 5 2022*", 
    str_detect(class, "t1") ~ "Term 1 2023", 
    str_detect(class, "t2") ~ "Term 2 2023"
  )) %>% 
  mutate(level = case_when( 
    str_ends(class_name_standardised, "intro") ~ "intro", 
    str_ends(class_name_standardised, "beginner") ~ "beginner",
    str_ends(class_name_standardised, "intermediate") ~ "intermediate", 
    str_ends(class_name_standardised, "open") ~ "open")) %>%  
  mutate(style = case_when( 
    str_starts(class_name_standardised, "twerk") ~ "twerk", 
    str_starts(class_name_standardised, "reggaeton") ~ "reggaeton", 
    str_starts(class_name_standardised, "femme_power") ~ "femme_power", 
    str_starts(class_name_standardised, "afrobeats") ~ "afrobeats", 
    str_starts(class_name_standardised, "bachata") ~ "bachata", 
    str_starts(class_name_standardised, "cuban_salsa") ~ "cuban_salsa", 
    str_starts(class_name_standardised, "latin_drills") ~ "latin_drills", 
    str_starts(class_name_standardised, "urban_kizomba") ~ "urban_kizomba", 
    str_starts(class_name_standardised, "salsa") ~ "salsa", 
    str_starts(class_name_standardised, "latin_fusion") ~ "latin_fusion", 
    str_starts(class_name_standardised, "bmit") ~ "bmit", 
    str_starts(class_name_standardised, "afro_cuban") ~ "afro_cuban"))

all_class %>% 
  filter(style == "bachata") %>% 
  view()

average_class_size_by_class <- all_class %>% 
  group_by(class_name_standardised) %>% 
  summarise(mean_equivalent = mean(total_upfront_equivalent), 
            median_equivalent = median(total_upfront_equivalent)) 

average_class_size_by_class %>% 
  ggplot(aes(x = reorder(class_name_standardised,median_equivalent), y = median_equivalent)) + 
  geom_col() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  

average_class_size_by_style <- all_class %>% 
  group_by(style) %>% 
  summarise(mean_equivalent = mean(total_upfront_equivalent), 
            median_equivalent = median(total_upfront_equivalent))

average_class_size_by_style %>% 
  ggplot(aes(x = reorder(style,median_equivalent), y = median_equivalent)) + 
  geom_col() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


average_class_size_by_level <- all_class %>% 
  group_by(level) %>% 
  summarise(mean_equivalent = mean(total_upfront_equivalent), 
            median_equivalent = median(total_upfront_equivalent))

average_class_size_by_level %>% 
  ggplot(aes(x = reorder(level,median_equivalent), y = median_equivalent)) + 
  geom_col() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


all_class %>%
  ungroup() %>% 
  arrange(total_upfront_equivalent) %>% 
  write_csv(file = "outputs/class_numbers.csv")
unique(all_class$class_name_standardised)
# %>% 
#   mutate(row_id = row_number()) %>% 
#   ggplot(aes(x = row_id, y = total_upfront_equivalent)) +
#   geom_col() 
summary(all_class$total_upfront_equivalent)
