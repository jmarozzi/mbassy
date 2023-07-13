# Cost benchmarking analysis for move to TDD 
## Dance teacher costs
## Get min, average & max cost per month

# GET MONTHLY TEACHING COSTS DURING ACTUAL TERM TIME
monthly_teacher_cost_per_term <- all_costs %>%
  filter(category == "teacher_costs") %>%
  left_join(term_dates_for_whole_picture_financials) %>%
  #mutate(monthly_teacher_cost = value/months) %>%
  select(time_stamp,value,category, term_start_date_for_non_class_transactions,term_end_date_for_non_class_transactions, months) %>% 
  summarise(total_teaching_costs = sum(value), 
            number_teaching_months = sum(months)) %>% 
  mutate(teaching_costs_per_teaching_month = total_teaching_costs/number_teaching_months)
teaching_costs_per_teaching_month_value <- monthly_teacher_cost_per_term %>% 
  pull(teaching_costs_per_teaching_month)
# $782 per active teaching month

### GET PROPORTION OF YEAR TEACHING TIME VS. BREAK TIME ACCOUNTING FOR BREAK PERIODS


## OPTION 2 (without prep for 915 Collins): includes Term 4*, Term 5*, Summer break 22/23, Term 1 2023, and Term 2 2023 
proportion_break_to_teaching_from_term_4_onwards <- term_dates_for_whole_picture_financials %>% 
  select(teaching_or_break_time, time_stamp,term_start_date_for_non_class_transactions,term_end_date_for_non_class_transactions, months) %>%
  filter(time_stamp != "Prep for 915 Collins 2022") %>% 
  group_by(teaching_or_break_time) %>% 
  summarise(months = sum(months)) %>% 
  ungroup() %>% 
  mutate(total_months = sum(months)) %>% 
  mutate(percentage = months/total_months) %>% 
  mutate(simulation_over_year_number_months = percentage *12) %>% 
  mutate(option = "option_2_from_term_4_onwards")
# Over a year: 2 months break time vs. 10 months teaching time (this seems about right)


##### Simulate dance teacher cost over a year 
option_2_annual_dance_teacher_costs_teaching_vs_break_time <- proportion_break_to_teaching_from_term_4_onwards %>% 
  mutate(monthly_teacher_cost = 
           case_when(
             teaching_or_break_time == "break_time" ~ 0, 
             teaching_or_break_time == "teaching_time" ~ teaching_costs_per_teaching_month_value
           )) %>% 
  mutate(annual_teacher_cost = simulation_over_year_number_months * monthly_teacher_cost)
  
option_two_total_annual_and_monthly_dance_teacher_costs_including_breaks <- option_2_annual_dance_teacher_costs_teaching_vs_break_time %>%
  summarise(
    annual_teacher_cost = sum(annual_teacher_cost), 
    simulation_over_year_number_months = sum(simulation_over_year_number_months)
  ) %>% 
  mutate(monthly_teacher_cost_accounting_for_breaks =annual_teacher_cost / simulation_over_year_number_months)

annual_teacher_cost_including_breaks_value <-   option_two_total_annual_and_monthly_dance_teacher_costs_including_breaks %>% 
  pull(annual_teacher_cost)
 #$7923.325 - assumes 10 months teaching and 2 months break over a year 
  
monthly_teacher_cost_including_breaks_value <-   option_two_total_annual_and_monthly_dance_teacher_costs_including_breaks %>% 
  pull(monthly_teacher_cost_accounting_for_breaks)
#$651.94 - assumes 10 months teaching and 2 months break over a year 
  
#TDD TERM 4 assumed FULL TERM teacher costs 
TDD_term_4_assumed_teacher_costs <- read_sheet("https://docs.google.com/spreadsheets/d/1HrZxfINbsWjadwBJeLsHrUweng7kgzRf1CjOFh7pc9g/edit#gid=0",
                                        sheet = "Assumed term 4 teacher costs") %>% 
  clean_names()  


teacher_cost_full_term_all_classes_with_enrollments <- TDD_term_4_assumed_teacher_costs %>% 
  filter(version == "All classes with upfront enrollments") %>% 
  summarise(total_teacher_cost = sum(total_term_inc_open_week)) %>% 
  mutate(total_teacher_cost = -total_teacher_cost) %>% 
  pull(total_teacher_cost)

teacher_cost_full_term_only_break_even_classes <- TDD_term_4_assumed_teacher_costs %>% 
  filter(version == "Only breakeven classes") %>% 
  summarise(total_teacher_cost = sum(total_term_inc_open_week)) %>% 
  mutate(total_teacher_cost = -total_teacher_cost) %>% 
  pull(total_teacher_cost)



##OVERHEAD COSTS TDD 
option_TDD_overhead_costs <- read_sheet("https://docs.google.com/spreadsheets/d/1HrZxfINbsWjadwBJeLsHrUweng7kgzRf1CjOFh7pc9g/edit#gid=0",
                             sheet = "Option TDD overheads") %>% 
  clean_names()  


# $2369.5 per month

##GET TERM 4 TDD overheads -assuming 2 months free rent spread over 6 months
TDD_monthly_rent <- option_TDD_overhead_costs %>% 
  filter(overhead == "Rent") %>%
  pull(per_month)
total_rent_over_six_months <- TDD_monthly_rent*4 
updated_monthly_rent_accounting_for_two_months_free <- total_rent_over_six_months/6


TDD_monthly_overheads<-option_TDD_overhead_costs %>% 
  mutate(per_month = ifelse(overhead == "Rent", updated_monthly_rent_accounting_for_two_months_free, per_month )) %>% 
  summarise(overhead_costs_monthly = sum(-per_month, na.rm = TRUE)) %>% 
  pull(overhead_costs_monthly)

#CAPITAL COSTS (out of pocket) - assuming needs ot be paid off in 12 months \


TDD_total_capital_costs <- -4504.92 #assumed spend additional to grant spend
TDD_monthly_capital_costs <-TDD_total_capital_costs/12

#actual capitax l costs 
decals <- -1872.88
sofa <- -359.98

blanket <- -23.75

microwave_and_sequin_wall <- -130
lamp <- -50
fridge <- -150
big_w_punch_bowl_ect <- -36.8 

paint <- -424.02
floors <- -2050
kindred_cameras <- -1166
frank_contracting <- -2093.32
electrician <- -995.50
mirrors <- -5152.67
actual_capital_costs <- decals + sofa + blanket + 
  microwave_and_sequin_wall + lamp + fridge + big_w_punch_bowl_ect + 
  paint + floors + kindred_cameras + frank_contracting + electrician + mirrors


#LEGAL expenses

legal_advice <- -550 # one off
legal_advice_monthly <- legal_advice/12
#DAILY COSTS (not inc. capital ) - will be consistent across both spaces 

## ADVERTISING & MARKETING
seo_consultant <- -1343 #one off
ad_spend <- -600 #assume one off, if successful could be recurring 
misc_marketing <- -3000 #assume annual (includes photography + flyers ect)
#actualss
t4_open_week_and_launch_party <- -1215.52

marketing_annual <- seo_consultant + ad_spend + misc_marketing
both_marketing_monthly <- marketing_annual/12

## DRINKS & OFFICE SUPPLIES 
total_drink_and_office_supplies_costs <- daily_costs %>% 
  filter(category %in% c("office_supplies", "drinks")) %>% 
  summarise(total_drink_and_office_costs = sum(value)) %>% 
  pull(total_drink_and_office_costs)





both_monthly_drink_and_office_supplies_cost <- term_dates_for_whole_picture_financials %>% 
  select(time_stamp,term_start_date_for_non_class_transactions,term_end_date_for_non_class_transactions, months) %>%
  filter(time_stamp != "Prep for 915 Collins 2022") %>% 
  summarise(months = sum(months)) %>% 
   mutate(total_drink_and_office_costs =total_drink_and_office_supplies_costs) %>% 
   mutate(monthly_drink_and_office_costs = total_drink_and_office_costs/ months) %>% 
  pull(monthly_drink_and_office_costs)


###TOTAL OVERALL COSTS 

#Lendlease monthly costs  


#TDD monthly costs    
TDD_total_monthly_costs_minus_teacher <- both_marketing_monthly + 
  both_monthly_drink_and_office_supplies_cost +
  TDD_monthly_capital_costs + TDD_monthly_overheads +
  legal_advice_monthly

#TDD term costs 
TDD_total_term_overheads <- (TDD_total_monthly_costs_minus_teacher*2) 

#### PERSONAL COSTS #### 
personal_totals <- read_sheet("https://docs.google.com/spreadsheets/d/1HrZxfINbsWjadwBJeLsHrUweng7kgzRf1CjOFh7pc9g/edit#gid=0",
           sheet = "Personal totals") %>% 
  clean_names() 

personal_minumum <-  personal_totals %>% 
  filter(level == "Minumum") %>% 
  mutate(per_month = -per_month) %>%
  pull(per_month)

personal_comfortable <-  personal_totals %>% 
  filter(level == "Comfortable") %>%
  mutate(per_month = -per_month) %>%
  pull(per_month)


personal_ideal <-  personal_totals %>% 
  filter(level == "Ideal") %>% 
  mutate(per_month = -per_month) %>%
  pull(per_month)


### TDD TERM COST BENCHMARKS 

TDD_term_cost_benchmarks <- tibble(
  teacher_pay_version = c("Pay all classes with enrollments", 
                          "Pay all classes with enrollments",
                          "Pay all classes with enrollments",
                          "Pay all classes with enrollments",
                          "Only pay breakeven classes",
                          "Only pay breakeven classes",
                          "Only pay breakeven classes",
                          "Only pay breakeven classes"), 
  benchmark_name = c("tdd_benchmark_break_even_no_personal", 
                     "tdd_benchmark_personal_minumum", 
                     "tdd_benchmark_personal_comfortable", 
                     "tdd_benchmark_personal_ideal", 
                     "tdd_benchmark_break_even_no_personal", 
                     "tdd_benchmark_personal_minumum", 
                     "tdd_benchmark_personal_comfortable", 
                     "tdd_benchmark_personal_ideal"
                     ), 
  benchmark_value = c(TDD_total_term_overheads + teacher_cost_full_term_all_classes_with_enrollments, 
                      TDD_total_term_overheads + (2* personal_minumum) + teacher_cost_full_term_all_classes_with_enrollments, 
                      TDD_total_term_overheads + (2* personal_comfortable) + teacher_cost_full_term_all_classes_with_enrollments,
                      TDD_total_term_overheads + (2* personal_ideal) + teacher_cost_full_term_all_classes_with_enrollments,
                      
                      TDD_total_term_overheads + teacher_cost_full_term_only_break_even_classes , 
                      TDD_total_term_overheads + (2* personal_minumum) + teacher_cost_full_term_only_break_even_classes , 
                      TDD_total_term_overheads + (2* personal_comfortable) + teacher_cost_full_term_only_break_even_classes,
                      TDD_total_term_overheads + (2* personal_ideal) + teacher_cost_full_term_only_break_even_classes
                      
                      ))

TDD_term_cost_benchmarks %>% 
  write_xlsx(path = "outputs/TDD_benchmarks_term_4.xlsx")
# 

#### REVENUE SIDE ####
# total_revenue_since_term_4 <-all_revenue %>% 
#   summarise(sum(value, na.rm =TRUE)) %>% 
#   pull()
# 
# total_revenue_since_term_4_without_term_1 <- all_revenue %>% 
#   filter(time_stamp != "Term 1 2023") %>%
#   summarise(sum(value, na.rm = TRUE)) %>% 
#   pull()
# 
# #lowball is revenue for Term 4 + 5 & 2 
# lowball_revenue <- term_dates_for_whole_picture_financials %>% 
#   select(time_stamp,term_start_date_for_non_class_transactions,term_end_date_for_non_class_transactions, months) %>%
#   filter(time_stamp != "Prep for 915 Collins 2022") %>%
#   filter(time_stamp != "Term 1 2023") %>% 
#   summarise(months = sum(months)) %>%
#   mutate(total_revenue_since_term_4_without_term_1 = total_revenue_since_term_4_without_term_1) %>%
#   mutate(revenue_per_month = total_revenue_since_term_4_without_term_1/months ) 
# 
# lowball_revenue_per_month <- lowball_revenue %>% 
#   pull(revenue_per_month)
# 
# #baseline is revenue for Term 4,5,1 & 2 
# baseline_revenue <- term_dates_for_whole_picture_financials %>% 
#   select(time_stamp,term_start_date_for_non_class_transactions,term_end_date_for_non_class_transactions, months) %>%
#   filter(time_stamp != "Prep for 915 Collins 2022") %>% 
#   summarise(months = sum(months)) %>% 
#   mutate(total_revenue_since_term_4 = total_revenue_since_term_4) %>%
#   mutate(revenue_per_month = total_revenue_since_term_4/months )
# 
# baseline_revenue_per_month <- baseline_revenue %>% 
#   pull(revenue_per_month)
# 
# #10% increase
# small_revenue_increase_per_month <- baseline_revenue_per_month *1.1
# 
# #25% increase 
# moderate_revenue_increase_per_month <- baseline_revenue_per_month *1.25
# 
# #50% increase 
# large_revenue_increase_per_month <- baseline_revenue_per_month *1.50
# 
# #60% increase
# extra_large_revenue_increase_per_month <- baseline_revenue_per_month *1.60 
# 
# 
# 
# revenue_scenarios <- tibble( 
#   option = "TDD", 
#   monthly_costs = TDD_total_monthly_costs, 
#   monthly_revenue_lowball = lowball_revenue_per_month, 
#   monthly_revenue_baseline = baseline_revenue_per_month,
#   monthly_revenue_small_increase = small_revenue_increase_per_month,
#   monthly_revenue_moderate_increase = moderate_revenue_increase_per_month, 
#   monthly_revenue_large_increase = large_revenue_increase_per_month,
#   monthly_revenue_extra_large_increase = extra_large_revenue_increase_per_month) %>% 
#   mutate(benchmark_studio_breakeven_no_personal = monthly_costs, 
#          benchmark_personal_minumum = monthly_costs + personal_minumum,
#          benchmark_personal_comfortable = monthly_costs + personal_comfortable,
#          benchmark_personal_ideal = monthly_costs + personal_ideal) 
# 
# revenue_scenarios_long <- revenue_scenarios %>% 
#   select(-starts_with("benchmark")) %>% 
#   pivot_longer(cols = starts_with("monthly_revenue"), names_to = "revenue_scenario", values_to = "revenue") %>% 
#   #mutate(profit = revenue + monthly_costs) %>% 
#   mutate(revenue_scenario = 
#            case_when(
#              revenue_scenario == "monthly_revenue_lowball" ~ "lowball", 
#              revenue_scenario == "monthly_revenue_baseline" ~ "baseline", 
#              revenue_scenario == "monthly_revenue_small_increase" ~ "small_increase", 
#              revenue_scenario == "monthly_revenue_moderate_increase" ~ "moderate_increase", 
#              revenue_scenario == "monthly_revenue_large_increase" ~ "large_increase", 
#              revenue_scenario == "monthly_revenue_extra_large_increase" ~ "extra_large_increase")) %>%
#   mutate(revenue_scenario = to_sentence_case(revenue_scenario)) %>%
#   mutate(revenue_scenario = factor(revenue_scenario, levels = c("Lowball", 
#                                                                 "Baseline", 
#                                                                 "Small increase", 
#                                                                 "Moderate increase", 
#                                                                 "Large increase", 
#                                                                 "Extra large increase"))) %>% 
#   select(-monthly_costs) 
#   
#   
# benchmarks <- revenue_scenarios %>%
#   select(option, starts_with("benchmark")) %>% 
#   pivot_longer(cols = starts_with("benchmark"), names_to = "benchmark_scenario", values_to = "benchmark") %>% 
#   mutate(benchmark_scenario = str_sub(benchmark_scenario, start = 11)) %>% 
#   mutate(benchmark = -benchmark) %>% 
#   mutate(benchmark_scenario = to_sentence_case(benchmark_scenario)) %>% 
#   mutate(benchmark_scenario = factor(benchmark_scenario, levels = c("Studio breakeven no personal", 
#                                                                     "Personal minumum", 
#                                                                     "Personal comfortable", 
#                                                                     "Personal ideal"))) %>%
#   mutate(Lowball  = "Lowball", 
#          Baseline = "Baseline",
#          `Small increase` = "Small increase", 
#          `Moderate increase` =  "Moderate increase", 
#          `Large increase` = "Large increase", 
#          `Extra large increase` = "Extra large increase"
#   ) %>% 
#   pivot_longer(cols= Lowball:`Extra large increase`, names_to = "revenue_scenario", 
#                values_to = "Value")
# 
# 
# # TDD_benchmarks <- benchmarks %>% 
# #   filter(option == "TDD")
# # 
# # TDD_graph <- revenue_scenarios_long %>% 
# #   filter(option == "TDD") %>% 
# #   ggplot(aes(x = revenue_scenario, y = revenue )) +
# #   geom_col(fill = "white") + 
# #   scale_x_discrete(labels = label_wrap(width=10)) + 
# #   scale_y_continuous(limits = c(0,10000)) + 
# #   labs(x = "Revenue Scenario", y= "Revenue", 
# #        title = "TDD") + 
# #   geom_line(data =TDD_benchmarks, aes(x = revenue_scenario, 
# #                                             y = benchmark, 
# #                                             color =benchmark_scenario, 
# #                                             group = benchmark_scenario) ) + 
# #   theme(legend.position = "bottom")
# # 
# # 
# 
# 
# Number of casuals are in the rows
# # Number of upfronts are the columns
