#Term targets 

#Set targets 
target_breakeven_no_personal <- TDD_term_cost_benchmarks_t5_2023 %>%  
  filter(teacher_pay_version == "Only pay breakeven classes" ) %>% 
  filter(benchmark_name == "tdd_benchmark_break_even_no_personal") %>% 
  mutate(benchmark_value = -benchmark_value) %>% 
  pull(benchmark_value)

target_breakeven_personal_minumum <- TDD_term_cost_benchmarks_t5_2023 %>%  
  filter(teacher_pay_version == "Only pay breakeven classes" ) %>% 
  filter(benchmark_name == "tdd_benchmark_personal_minumum") %>% 
  mutate(benchmark_value = -benchmark_value) %>% 
  pull(benchmark_value)

target_breakeven_personal_comfortable <- TDD_term_cost_benchmarks_t5_2023 %>%  
  filter(teacher_pay_version == "Only pay breakeven classes" ) %>% 
  filter(benchmark_name == "tdd_benchmark_personal_comfortable") %>% 
  mutate(benchmark_value = -benchmark_value) %>% 
  pull(benchmark_value)

target_breakeven_personal_ideal <- TDD_term_cost_benchmarks_t5_2023 %>%  
  filter(teacher_pay_version == "Only pay breakeven classes" ) %>% 
  filter(benchmark_name == "tdd_benchmark_personal_ideal") %>% 
  mutate(benchmark_value = -benchmark_value) %>% 
  pull(benchmark_value)

tdd_revenue_term_5_summary <- all_revenue %>%
  filter(time_stamp == "Term 5 2023" ) %>%  
  summarise(total_revenue = sum(value, na.rm = TRUE)) 
all_revenue %>% 
  filter(category == "privates")

grouped_revenue_t5_23 <- all_revenue %>%
  filter(time_stamp == "Term 5 2023" ) %>%
  group_by(category) %>% 
  summarise(total_revenue = sum(value, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(total = sum(total_revenue)) %>% 
  mutate(proportion = total_revenue/total) %>% 
  mutate(term = "Term 5 2023")

write_csv(grouped_revenue, file = "outputs/term_5_revenue_2023.csv" )



#tdd_revenue_term_4_summary$color_condition <- tdd_revenue_term_4_summary$average_monthly_revenue >= target_revenue

g_tdd_term_5_revenue <- tdd_revenue_term_5_summary %>% 
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
  
  scale_y_continuous(labels = scales::dollar_format(), limits = c(0, 25000)) +
  labs(title = "Overall Term 5 Revenue vs Target",
       subtitle = str_c("Actual T5 revenue to date: ", scales::dollar(tdd_revenue_term_5_summary$total_revenue, accuracy = 1)),
       x = "",
       y = "Term 5 revenue") +
  theme_minimal() +
  theme(axis.title.x = element_blank())


g_revenue_breakdown_chart <- grouped_revenue_t5_23 %>% 
  ggplot(aes(x = reorder(category, -total_revenue), y =total_revenue )) + 
  geom_col() + 
  expand_limits(y= c(0,15000)) + 
  scale_y_continuous(labels = dollar_format())+
  geom_text(aes(reorder(category, -total_revenue), y =total_revenue +700, label = str_glue("{dollar(total_revenue, 1)} ({percent(proportion, 1)})"))) + 
  labs(x = "Revenue category", y = "Term 4 Revenue")



cost_breakdown_for_chart_t5 <- breakdown_costs_without_personal_t5_2023 %>% 
  group_by(larger_cost_category) %>% 
  summarise(term_5_cost = sum(term_5_cost)) %>% 
  ungroup() %>% 
  mutate(all_costs = sum(term_5_cost)) %>% 
  mutate(proportion = term_5_cost/all_costs)  

g_cost_breakdown <- cost_breakdown_for_chart_t5 %>% 
  
  ggplot(aes(x = reorder(larger_cost_category,term_5_cost), y = -term_5_cost))+ 
  geom_col() + 
  scale_x_discrete(labels = wrap_format(10))+ 
  #expand_limits(x = c(0,6500))+
  geom_text(aes(x = reorder(larger_cost_category,term_5_cost), y = -term_5_cost +300, label = str_glue("{dollar(-term_5_cost, 1)} ({percent(proportion, .1)})"))) + 
  labs(x = "Cost Category", 
       y = "Total costs in Term 5", 
       subtitle = str_glue("Total costs in Term 5 were {dollar(-cost_breakdown_for_chart_t5$all_costs[1])}"))

term_5_profit_tibble <- tibble(profit_category = c("Total profit", "Weekly (eight weeks)", "Hourly (assuming 24 hours in a week)"), 
                               profit = c(tdd_revenue_term_5_summary$total_revenue + actual_term_5_cost,
                                          (tdd_revenue_term_5_summary$total_revenue + actual_term_5_cost)/8, 
                                          ((tdd_revenue_term_5_summary$total_revenue + actual_term_5_cost)/8)/24)) %>% 
  mutate(profit = as.numeric(profit)) %>% 
  mutate(profit_category = factor(profit_category, levels = c("Total profit", "Weekly (eight weeks)", "Hourly (assuming 24 hours in a week)"))) %>% 
  mutate(term = "Term 5 2023")

term_5_profit_graph <- term_5_profit_tibble %>% 
  ggplot(aes(x = profit_category, y = profit))+ 
  geom_col()+ 
  labs(x = "Profit Category", 
       y = "Profit") + 
  scale_x_discrete(labels = wrap_format(20)) + 
  scale_y_continuous(labels = dollar_format(), limits = c(0,10000)) + 
  geom_text(aes(x = profit_category, y = profit+400, label = dollar(profit)))

#### Comparison 
comparison_revenue <- all_revenue %>% 
  filter(time_stamp %in% c("Term 4 2023", "Term 5 2023")) %>%  
  group_by(time_stamp) %>%
  summarise(total_revenue = sum(value, na.rm = TRUE)) %>% 
  ggplot(aes(x = time_stamp, y = total_revenue)) + 
  geom_col()+
  scale_y_continuous(labels = dollar_format(), limits = c(0,25000)) + 
  geom_text(aes(x = time_stamp, y = total_revenue + 1000, label = dollar(total_revenue)), 
            position = position_dodge(width = 0.8))


comparison_profits <- rbind(term_5_profit_tibble, term_4_profit_tibble)%>% 
  ggplot(aes(x = profit_category, y = profit, fill = term))+ 
  geom_col(position = position_dodge())+ 
  labs(x = "Profit Category", 
       y = "Profit") + 
  scale_x_discrete(labels = wrap_format(20)) + 
  scale_y_continuous(labels = dollar_format(), limits = c(0,9000)) + 
  geom_text(aes(x = profit_category, y = profit + 400, label = dollar(profit)), 
            position = position_dodge(width = 0.8))

comparison_revenue_proportions <- rbind(grouped_revenue_t4_23,grouped_revenue_t5_23) %>% 
  ggplot(aes(x = category, y = proportion, fill = term)) +
  geom_col(position = position_dodge())+ 
  labs(x = "Revenue Category", 
       y = "Proportion of revenue") + 
  scale_x_discrete(labels = wrap_format(20)) + 
  scale_y_continuous(labels = percent_format()) + 
  geom_text(aes(x = category, y = proportion+0.05, label = percent(proportion, 1)), 
            position = position_dodge(width = 0.8))


breakdown_costs_without_personal_for_export <- breakdown_costs_without_personal_t5_2023 %>% 
  select(-larger_cost_category) %>% 
  mutate(across(c(annual_cost, weekly_cost_assumes_8_weeks_off, term_5_cost), .fns = ~ dollar(-.x, accuracy = .1)))  

create_nous_pptx(title = "MBassy Financial Summary", 
                 subtitle =  "Term 5 2023") %>% 
  add_nous_plot_pptx(plot = g_tdd_term_5_revenue, 
                     gt = "Great revenue - nearly at personal comfortable") %>% 
  add_nous_plot_pptx(plot = comparison_revenue, 
                     gt = "40% increase in revenue compared to last term!") %>%
  add_nous_plot_pptx(plot = g_revenue_breakdown_chart, 
                     gt = "The majority of revenue remains in classes with a growing share from Mambomania and studio hire") %>% 
  add_nous_plot_pptx(plot = comparison_revenue_proportions, 
                     gt = "Big increase in proportion of revenue due to studio hire and introduction of 'Special events' (i.e Mambomania)") %>%
  add_nous_plot_pptx(plot = g_cost_breakdown, 
                     gt = "Costs remain as expected") %>% 
  add_nous_table_pptx(breakdown_costs_without_personal_for_export,
                      gt = "Detailed breakdown of costs including each teachers' pay") %>% 
  add_nous_plot_pptx(plot = term_5_profit_graph, 
                     gt = "Overall profit was over $7k!!") %>% 
  add_nous_plot_pptx(plot = comparison_profits, 
                     gt = "DOUBLED THAT PROFIT!") %>% 
  add_nous_plot_pptx(plot = g_student_registration_pattern, 
                     gt = "Fifty new students in Term 5!") %>% 
  add_nous_plot_pptx(plot = g_unique_students_by_term, 
                     gt = "100 unique students in Term 5 with 49 upfronts") %>% 
  print("outputs/MBassy Term 5 2023 financial report.pptx")
