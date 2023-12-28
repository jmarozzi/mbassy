# Cost benchmarking analysis for move to TDD 

### ACTUAL TEACHER PAY - TERM 4

#hourly rate * number of classes * number of weeks 

steve_pay <- (-40*4*8) - 40  #19.30 Urban Kiz Improver; 20.30 Urban Kiz Beg; 18.30 Salsa partner beg; 19.30 Salsa Shines Beg; maestros workshop
diana_pay <- (-40*2*8) - 40 # 18.30 Salsa partner beg; 19.30 Salsa Shines Beg; maestros of movement
poorang_pay <- -40 #maestros
leo_pay <- -200 # 5 x Kizomba sub ins
gigi_pay <- -70  #1 x yoga class
other_yoga_teacher_pay <- -80*7 #7x yoga classes
all_teacher_pay_t4 <- steve_pay + diana_pay + poorang_pay + leo_pay + gigi_pay + other_yoga_teacher_pay



### TDD TERM COST BENCHMARKS 

TDD_term_cost_benchmarks_t4_2023 <- tibble(
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

TDD_term_cost_benchmarks %>% 
  write_xlsx(path = "outputs/TDD_benchmarks_term_4.xlsx")




breakdown_of_costs_t4_2023 <- tibble(
  cost_category = c("drinks and office", "marketing", "legal", "capital", "rent, subscriptions, bills", 
                    "Steve pay", "Diana pay", "Poorang pay", "Leo pay", "Gigi pay", "Other yoga teacher pay",
                    
                    "personal minumum", "personal comfortable", "personal ideal"), 
  #time_period = rep("annual", times = 5),
  annual_cost = c(annual_drink_and_office_costs, marketing_annual, 
                  legal_advice,TDD_total_capital_costs,TDD_annual_rent_bills_subs,  NA, NA, NA, NA, NA, NA, 
                  personal_minumum *12, personal_comfortable *12, personal_ideal *12)) %>% 
  mutate(weekly_cost_assumes_8_weeks_off = annual_cost / 44) %>% 
  mutate(term_4_cost = weekly_cost_assumes_8_weeks_off *8) %>% 
  mutate(term_4_cost = case_when( 
    cost_category == "Steve pay" ~ steve_pay, 
    cost_category == "Diana pay" ~ diana_pay, 
    cost_category == "Poorang pay" ~ poorang_pay, 
    cost_category == "Leo pay" ~ leo_pay, 
    cost_category == "Gigi pay" ~ gigi_pay, 
    cost_category == "Other yoga teacher pay" ~ other_yoga_teacher_pay, 
    TRUE ~ term_4_cost)) %>%
  # mutate(pay_comments = c(NA,NA,NA, NA, NA, 
  #                         "19.30 Urban Kiz Improver; 20.30 Urban Kiz Beg; 18.30 Salsa partner beg; 19.30 Salsa Partner Improver; Maestros", 
  #                         "18.30 Salsa partner beg; 19.30 Salsa Partner Improver; Maestros", 
  #                         "Maestros", 
  #                         "5 x Kizomba sub ins", 
  #                         "1 x yoga class", 
  #                         "7 x yoga classes", 
  #                         NA, 
  #                         NA, 
  #                         NA))  %>% 
  mutate(larger_cost_category = case_when(cost_category %in% c("Steve pay", "Diana pay", "Poorang pay", "Leo pay",
                                                               "Gigi pay", "Other yoga teacher pay") ~ "teacher pay", 
                                          TRUE ~ cost_category))

write.csv(breakdown_of_costs_t4_2023, "outputs/breakdown_of_costs_t4.csv")


breakdown_costs_without_personal_t4_2023 <-breakdown_of_costs_t4_2023 %>% 
  filter(cost_category != "personal minumum")%>% 
  filter(cost_category != "personal comfortable") %>% 
  filter(cost_category != "personal ideal") 

actual_term_4_cost<- breakdown_costs_without_personal_t4_2023 %>% 
  summarise(total_term_4 = sum(term_4_cost))
