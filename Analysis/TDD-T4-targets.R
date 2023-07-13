#Term targets 

#Set targets 
target_breakeven_no_personal <- TDD_term_cost_benchmarks %>%  
  filter(teacher_pay_version == "Only pay breakeven classes" ) %>% 
  filter(benchmark_name == "tdd_benchmark_break_even_no_personal") %>% 
  mutate(benchmark_value = -benchmark_value) %>% 
  pull(benchmark_value)

target_breakeven_personal_minumum <- TDD_term_cost_benchmarks %>%  
  filter(teacher_pay_version == "Only pay breakeven classes" ) %>% 
  filter(benchmark_name == "tdd_benchmark_personal_minumum") %>% 
  mutate(benchmark_value = -benchmark_value) %>% 
  pull(benchmark_value)

target_breakeven_personal_comfortable <- TDD_term_cost_benchmarks %>%  
  filter(teacher_pay_version == "Only pay breakeven classes" ) %>% 
  filter(benchmark_name == "tdd_benchmark_personal_comfortable") %>% 
  mutate(benchmark_value = -benchmark_value) %>% 
  pull(benchmark_value)

target_breakeven_personal_ideal <- TDD_term_cost_benchmarks %>%  
  filter(teacher_pay_version == "Only pay breakeven classes" ) %>% 
  filter(benchmark_name == "tdd_benchmark_personal_ideal") %>% 
  mutate(benchmark_value = -benchmark_value) %>% 
  pull(benchmark_value)

tdd_revenue_term_4_summary <- all_revenue %>%
  filter(!is.na(tdd_month_number)) %>%
  filter(time_stamp != "Term 3 2023") %>% 
  summarise(total_revenue = sum(value, na.rm = TRUE))

tdd_revenue_term_4_summary
#tdd_revenue_term_4_summary$color_condition <- tdd_revenue_term_4_summary$average_monthly_revenue >= target_revenue

g_tdd_term_4_revenue <- tdd_revenue_term_4_summary %>% 
  ggplot() +
  geom_col(aes(x = "Actual", y = total_revenue), fill = "aquamarine2") +
  geom_hline(aes(yintercept = target_breakeven_no_personal), color = "mediumpurple1", linetype = "dashed", linewidth = 2) +
  geom_label(aes(x = "Actual", y = target_breakeven_no_personal, label = str_c("Breakeven (no personal): ", scales::dollar(target_breakeven_no_personal, accuracy = 1))), 
             color = "mediumpurple1") +
  
  geom_hline(aes(yintercept = target_breakeven_personal_minumum), color = "mediumpurple2", linetype = "dashed", linewidth = 2) +
  geom_label(aes(x = "Actual", y = target_breakeven_personal_minumum, label = str_c("Personal minumum: ", scales::dollar(target_breakeven_personal_minumum, accuracy = 1))), 
             color = "mediumpurple2") +

  geom_hline(aes(yintercept = target_breakeven_personal_comfortable), color = "mediumpurple3", linetype = "dashed", linewidth = 2) +
  geom_label(aes(x = "Actual", y = target_breakeven_personal_comfortable, label = str_c("Personal comfortable: ", scales::dollar(target_breakeven_personal_comfortable, accuracy = 1))), 
             color = "mediumpurple3") +
  
  geom_hline(aes(yintercept = target_breakeven_personal_ideal), color = "mediumpurple4", linetype = "dashed", linewidth = 2) +
  geom_label(aes(x = "Actual", y = target_breakeven_personal_ideal, label = str_c("Personal ideal: ", scales::dollar(target_breakeven_personal_ideal, accuracy = 1))), 
             color = "mediumpurple4") +
  
  scale_y_continuous(labels = scales::dollar_format(), limits = c(0, 20000)) +
  labs(title = "Overall Term 4 Revenue vs Target",
       subtitle = str_c("Actual T4 revenue to date: ", scales::dollar(tdd_revenue_term_4_summary$total_revenue, accuracy = 1)),
       x = "",
       y = "Term 4 revenue") +
  theme_minimal() +
  theme(axis.title.x = element_blank())

g_tdd_term_4_revenue
