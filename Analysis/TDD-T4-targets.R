#Term targets 

#Set targets 
target_breakeven_no_personal <- TDD_term_cost_benchmarks_t4_2023 %>%  
  filter(teacher_pay_version == "Only pay breakeven classes" ) %>% 
  filter(benchmark_name == "tdd_benchmark_break_even_no_personal") %>% 
  mutate(benchmark_value = -benchmark_value) %>% 
  pull(benchmark_value)

target_breakeven_personal_minumum <- TDD_term_cost_benchmarks_t4_2023 %>%  
  filter(teacher_pay_version == "Only pay breakeven classes" ) %>% 
  filter(benchmark_name == "tdd_benchmark_personal_minumum") %>% 
  mutate(benchmark_value = -benchmark_value) %>% 
  pull(benchmark_value)

target_breakeven_personal_comfortable <- TDD_term_cost_benchmarks_t4_2023 %>%  
  filter(teacher_pay_version == "Only pay breakeven classes" ) %>% 
  filter(benchmark_name == "tdd_benchmark_personal_comfortable") %>% 
  mutate(benchmark_value = -benchmark_value) %>% 
  pull(benchmark_value)

target_breakeven_personal_ideal <- TDD_term_cost_benchmarks_t4_2023 %>%  
  filter(teacher_pay_version == "Only pay breakeven classes" ) %>% 
  filter(benchmark_name == "tdd_benchmark_personal_ideal") %>% 
  mutate(benchmark_value = -benchmark_value) %>% 
  pull(benchmark_value)

tdd_revenue_term_4_summary <- all_revenue %>%
  filter(time_stamp == "Term 4 2023" ) %>%  
  summarise(total_revenue = sum(value, na.rm = TRUE))

grouped_revenue_t4_23 <- all_revenue %>%
  filter(time_stamp == "Term 4 2023" ) %>%
  group_by(category) %>% 
  summarise(total_revenue = sum(value, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(total = sum(total_revenue)) %>% 
  mutate(proportion = total_revenue/total)%>% 
  mutate(term = "Term 4 2023")

write_csv(grouped_revenue_t4_23, file = "outputs/term_4_revenue_2023.csv" )


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
  
  scale_y_continuous(labels = scales::dollar_format(), limits = c(0, 25000)) +
  labs(title = "Overall Term 4 Revenue vs Target",
       subtitle = str_c("Actual T4 revenue to date: ", scales::dollar(tdd_revenue_term_4_summary$total_revenue, accuracy = 1)),
       x = "",
       y = "Term 4 revenue") +
  theme_minimal() +
  theme(axis.title.x = element_blank())


g_revenue_breakdown_chart <- grouped_revenue_t4_23 %>% 
  ggplot(aes(x = reorder(category, -total_revenue), y =total_revenue )) + 
  geom_col() + 
  expand_limits(y= c(0,15000)) + 
  scale_y_continuous(labels = dollar_format())+
  geom_text(aes(reorder(category, -total_revenue), y =total_revenue +700, label = str_glue("{dollar(total_revenue, 1)} ({percent(proportion, 1)})"))) + 
  labs(x = "Revenue category", y = "Term 4 Revenue")



cost_breakdown_for_chart_t4 <- breakdown_costs_without_personal_t4_2023 %>% 
  group_by(larger_cost_category) %>% 
  summarise(term_4_cost = sum(term_4_cost)) %>% 
  ungroup() %>% 
  mutate(all_costs = sum(term_4_cost)) %>% 
  mutate(proportion = term_4_cost/all_costs)  

g_cost_breakdown <- cost_breakdown_for_chart_t4 %>% 
  
  ggplot(aes(x = reorder(larger_cost_category,term_4_cost), y = -term_4_cost))+ 
  geom_col() + 
  scale_x_discrete(labels = wrap_format(10))+ 
  #expand_limits(x = c(0,6500))+
  geom_text(aes(x = reorder(larger_cost_category,term_4_cost), y = -term_4_cost +300, label = str_glue("{dollar(-term_4_cost, 1)} ({percent(proportion, .1)})"))) + 
  labs(x = "Cost Category", 
       y = "Total costs in Term 5", 
       subtitle = str_glue("Total costs in Term 4 were {dollar(-cost_breakdown_for_chart_t4$all_costs[1])}"))

term_4_profit_tibble <- tibble(profit_category = c("Total profit", "Weekly (eight weeks)", "Hourly (assuming 24 hours in a week)"), 
                               profit = c(tdd_revenue_term_4_summary$total_revenue + actual_term_4_cost,
                                          (tdd_revenue_term_4_summary$total_revenue + actual_term_4_cost)/8, 
                                          ((tdd_revenue_term_4_summary$total_revenue + actual_term_4_cost)/8)/24)) %>% 
  mutate(profit = as.numeric(profit)) %>% 
  mutate(profit_category = factor(profit_category, levels = c("Total profit", "Weekly (eight weeks)", "Hourly (assuming 24 hours in a week)"))) %>% 
  mutate(term = "Term 4 2023")


term_4_profit_graph <- term_4_profit_tibble %>% 
  ggplot(aes(x = profit_category, y = profit))+ 
  geom_col()+ 
  labs(x = "Profit Category", 
       y = "Profit") + 
  scale_x_discrete(labels = wrap_format(20)) + 
  scale_y_continuous(labels = dollar_format(), limits = c(0,4000)) + 
  geom_text(aes(x = profit_category, y = profit+200, label = dollar(profit)))


breakdown_costs_without_personal_for_export <- breakdown_costs_without_personal %>% 
  select(-larger_cost_category) %>% 
  mutate(across(c(annual_cost, weekly_cost_assumes_8_weeks_off, term_4_cost), .fns = ~ dollar(-.x, accuracy = .1)))  

create_nous_pptx(title = "MBassy Financial Summary", 
                 subtitle =  "Term 4 2023") %>% 
  add_nous_plot_pptx(plot = g_tdd_term_4_revenue, 
                     gt = "Stronger than expected revenue for first term at studio, meeting personal minumum") %>% 
  add_nous_plot_pptx(plot = g_revenue_breakdown_chart, 
                     gt = "Strong revenues for dance classes given first term, with scope to grow in studio hire, privates and gigs") %>% 
  add_nous_plot_pptx(plot = g_cost_breakdown, 
                     gt = "Costs were higher than expected driven by investments in marketing") %>% 
  add_nous_table_pptx(breakdown_costs_without_personal_for_export,
                      gt = "Detailed breakdown of costs including each teachers' pay") %>% 
  add_nous_plot_pptx(term_4_profit_graph, 
                     gt = "Overall profit was over $3.5k - a great start for a new studio & a much higher cost base") %>% 
  print("outputs/MBassy Term 4 2023 financial report.pptx")
