# Overhead costs for TDD


##OVERHEAD COSTS TDD 
TDD_rent_bills_subs <- read_sheet("https://docs.google.com/spreadsheets/d/1HrZxfINbsWjadwBJeLsHrUweng7kgzRf1CjOFh7pc9g/edit#gid=0",
                             sheet = "Overheads") %>% 
  clean_names()  


# $2369.5 per month

##GET TERM 4 TDD overheads -assuming 2 months free rent spread over 12 months
TDD_monthly_rent <- TDD_rent_bills_subs %>% 
  filter(overhead == "Rent") %>%
  pull(per_month)
total_rent_annual <- TDD_monthly_rent*10 
updated_monthly_rent_accounting_for_two_months_free <- total_rent_annual/12


TDD_monthly_rent_bills_subs<-TDD_rent_bills_subs %>% 
  mutate(per_month = ifelse(overhead == "Rent", updated_monthly_rent_accounting_for_two_months_free, per_month )) %>% 
  summarise(overhead_costs_monthly = sum(-per_month, na.rm = TRUE)) %>% 
  pull(overhead_costs_monthly)

TDD_annual_rent_bills_subs <- TDD_monthly_rent_bills_subs*12 
#CAPITAL COSTS (out of pocket) - assuming needs ot be paid off in 12 months \




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
audio_cables <- -57.98
ikea_tubs <- -68.5
fb_marketplace_table <- -70

actual_capital_costs <- decals + sofa + blanket + 
  microwave_and_sequin_wall + lamp + fridge + big_w_punch_bowl_ect + 
  paint + floors + kindred_cameras + frank_contracting + electrician + mirrors + 
  audio_cables + ikea_tubs + fb_marketplace_table




TDD_total_capital_costs <- actual_capital_costs + 10000 #assumed spend additional to grant spend
TDD_monthly_capital_costs <-TDD_total_capital_costs/12
#LEGAL expenses

legal_advice <- -550 # one off
legal_advice_monthly <- legal_advice/12
#DAILY COSTS (not inc. capital ) - will be consistent across both spaces 

## ADVERTISING & MARKETING
seo_consultant <- -1343 #one off
ad_spend <- -600 #assume one off, if successful could be recurring 
misc_marketing <- -5500 #assume annual (includes photography + flyers ect)
#actualss
t4_open_week_and_launch_party <- -1215.52

marketing_annual <- seo_consultant + ad_spend + misc_marketing
both_marketing_monthly <- marketing_annual/12

daily_costs %>% 
  filter(category == "marketing_and_advertising") %>% 
  filter(date > ymd("2023-01-01")) %>% 
  summarise(marketing_costs = sum(value)) 

unique(daily_costs$category)
daily_costs %>% 
  filter(category == "teacher_pay") %>% 
  summarise(total = sum(value)) 
## DRINKS & OFFICE SUPPLIES 
total_drink_and_office_supplies_costs <- daily_costs %>% 
  filter(category %in% c("office_supplies", "drinks")) %>% 
  summarise(total = sum(value)) %>% 
  pull(total)



both_monthly_drink_and_office_supplies_cost <- term_dates_for_whole_picture_financials %>% 
  select(time_stamp,term_start_date_for_non_class_transactions,term_end_date_for_non_class_transactions, months) %>%
  filter(time_stamp != "Prep for 915 Collins 2022") %>% 
  summarise(months = sum(months)) %>% 
  mutate(total_drink_and_office_costs =total_drink_and_office_supplies_costs) %>% 
  mutate(monthly_drink_and_office_costs = total_drink_and_office_costs/ months) %>% 
  pull(monthly_drink_and_office_costs)

annual_drink_and_office_costs <- both_monthly_drink_and_office_supplies_cost *12 
###TOTAL OVERALL COSTS 


#GET ANNUAL COSTS 
TDD_annual_overheads_complete <- TDD_total_capital_costs + marketing_annual + 
  annual_drink_and_office_costs + TDD_annual_rent_bills_subs +legal_advice

#TDD term costs 
### 44 weeks teaching time = Assuming 5 x 8 week terms  + 1 x 4 week term   
### 8 weeks not teaching time 
TDD_weekly_overhead_complete <-  TDD_annual_overheads_complete /44
TDD_total_term_overheads_8_weeks <-TDD_weekly_overhead_complete *8

TDD_total_term_overheads_4_weeks<-TDD_weekly_overhead_complete *4 





#Check that this adds up to annual total
((5 *TDD_total_term_overheads_8_weeks) + 1 * TDD_total_term_overheads_4_weeks) == TDD_annual_overheads_complete


