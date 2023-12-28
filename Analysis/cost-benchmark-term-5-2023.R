# Cost benchmarking Term 5

### ACTUAL TEACHER PAY - TERM 5 
term_5_2023_dance_teacher_pay1 <- read_sheet("https://docs.google.com/spreadsheets/d/19jxGPbdvYHPFvvNIU0-ja0svVtmqk_GCJD8tDokBOnI/edit#gid=341416317",
                                           sheet = "Teacher total") %>% 
  clean_names() %>% 
  mutate(hourly_rate = case_when(teacher == "Gigi" ~ 70,
                                 TRUE ~ 40)) %>% 
  filter(teacher != "Grand Total") %>% 
  mutate(total_pay = sum_of_actual_hours * hourly_rate)


t5_teacher_grand_total <- c("Grand Total", sum(term_5_2023_dance_teacher_pay1$sum_of_typical_hours_per_week), 
              sum(term_5_2023_dance_teacher_pay1$sum_of_actual_hours),sum(term_5_2023_dance_teacher_pay1$total_pay)/sum(term_5_2023_dance_teacher_pay1$sum_of_actual_hours), 
  sum(term_5_2023_dance_teacher_pay1$total_pay))  
  

term_5_2023_dance_teacher_pay <- term_5_2023_dance_teacher_pay1 %>% 
  rbind(t5_teacher_grand_total)


all_teacher_pay_t5 <- term_5_2023_dance_teacher_pay %>% 
  filter(teacher == "Grand Total") %>% 
  pull(as.numeric(total_pay)) %>% 
  as.numeric()


steve_pay <- term_5_2023_dance_teacher_pay %>% 
  filter(teacher == "Steve") %>% 
  pull(as.numeric(total_pay)) %>% 
  as.numeric()

diana_pay <- term_5_2023_dance_teacher_pay %>% 
  filter(teacher == "Diana") %>% 
  pull(as.numeric(total_pay)) %>% 
  as.numeric()

poorang_pay <- term_5_2023_dance_teacher_pay %>% 
  filter(teacher == "Poorang") %>% 
  pull(as.numeric(total_pay)) %>% 
  as.numeric()

gigi_pay <- term_5_2023_dance_teacher_pay %>% 
  filter(teacher == "Gigi") %>% 
  pull(as.numeric(total_pay)) %>% 
  as.numeric()


kai_pay <- term_5_2023_dance_teacher_pay %>% 
  filter(teacher == "Kai") %>% 
  pull(as.numeric(total_pay)) %>% 
  as.numeric()



### TDD TERM COST BENCHMARKS 

TDD_term_cost_benchmarks_t5_2023 <- tibble(
  teacher_pay_version = c(
    "Only pay breakeven classes",
    "Only pay breakeven classes",
    "Only pay breakeven classes",
    "Only pay breakeven classes"), 
  benchmark_name = c( 
    "tdd_benchmark_break_even_no_personal", 
    "tdd_benchmark_personal_minumum", 
    "tdd_benchmark_personal_comfortable", 
    "tdd_benchmark_personal_ideal"
  ), 
  benchmark_value = c(
    
    TDD_total_term_overheads_8_weeks -  all_teacher_pay_t5, 
    TDD_total_term_overheads_8_weeks + (2* personal_minumum) - all_teacher_pay_t5 , 
    TDD_total_term_overheads_8_weeks + (2* personal_comfortable) - all_teacher_pay_t5,
    TDD_total_term_overheads_8_weeks + (2* personal_ideal) - all_teacher_pay_t5
  ))

TDD_term_cost_benchmarks_t5_2023 %>% 
  write_xlsx(path = "outputs/TDD_benchmarks_term_5.xlsx")




breakdown_of_costs_t5_2023 <- tibble(
  cost_category = c("drinks and office", "marketing", "legal", "capital", "rent, subscriptions, bills", 
                    "Steve pay", "Diana pay", "Poorang pay",  "Gigi pay", "Kai pay",
                    
                    "personal minumum", "personal comfortable", "personal ideal"), 
  #time_period = rep("annual", times = 5),
  annual_cost = c(annual_drink_and_office_costs, marketing_annual, 
                  legal_advice,TDD_total_capital_costs,TDD_annual_rent_bills_subs,  NA, NA, NA, NA, NA, 
                  personal_minumum *12, personal_comfortable *12, personal_ideal *12)) %>% 
  mutate(weekly_cost_assumes_8_weeks_off = annual_cost / 44) %>% 
  mutate(term_5_cost = weekly_cost_assumes_8_weeks_off *8) %>% 
  mutate(term_5_cost = case_when( 
    cost_category == "Steve pay" ~ -steve_pay, 
    cost_category == "Diana pay" ~ -diana_pay, 
    cost_category == "Poorang pay" ~ -poorang_pay, 
    cost_category == "Gigi pay" ~ -gigi_pay, 
    cost_category == "Kai pay" ~ -kai_pay, 
    TRUE ~ term_5_cost)) %>%
  # mutate(pay_comments = c(NA,NA,NA, NA, NA, 
  #                         "19.30 Urban Kiz Improver; 20.30 Urban Kiz Beg; 18.30 Salsa partner beg; 19.30 Salsa Partner Improver; Maestros", 
  #                         "18.30 Salsa partner beg; 19.30 Salsa Partner Improver; Maestros", 
  #                         "Maestros", 
  #                         "5 x Kizomba sub ins", 
  #                         "1 x yoga class", %>% %>% %>% 
  #                         "7 x yoga classes", 
  #                         NA, 
  #                         NA, 
  #                         NA))  %>% 
  mutate(larger_cost_category = case_when(cost_category %in% c("Steve pay", "Diana pay", "Poorang pay", 
                                                               "Gigi pay", "Kai pay") ~ "teacher pay", 
                                          TRUE ~ cost_category))

write.csv(breakdown_of_costs_t5_2023, "outputs/breakdown_of_costs_t5.csv")

breakdown_costs_without_personal_t5_2023 <-breakdown_of_costs_t5_2023 %>% 
  filter(cost_category != "personal minumum")%>% 
  filter(cost_category != "personal comfortable") %>% 
  filter(cost_category != "personal ideal") 

actual_term_5_cost<- breakdown_costs_without_personal_t5_2023 %>% 
  summarise(total_term_5 = sum(term_5_cost))

