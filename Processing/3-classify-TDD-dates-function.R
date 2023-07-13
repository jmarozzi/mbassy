#Create function to classify transaction dates into TDD months 

tdd_month_table <- tibble(
  TDD_month_number = seq(1,6),
  start_date = c(ymd("2023-06-01"), 
                 ymd("2023-07-01"), 
                 ymd("2023-08-01"), 
                 ymd("2023-09-01"),
                 ymd("2023-10-01"),
                 ymd("2023-11-01")),
  end_date = c(ymd("2023-06-30"), 
               ymd("2023-07-31"), 
               ymd("2023-08-31"),
               ymd("2023-09-30"),
               ymd("2023-10-31"),
               ymd("2023-11-30"))) %>% 
  mutate(interval = interval(start = start_date,
                             end = end_date, 
                             tzone = "Australia/Melbourne") )



# Define classify_date function
classify_date_TDD <- function(dates) {
  if (!all(inherits(dates, "Date"))) {
    stop("Input dates must be in 'Date' class format")
  }
  
  dates <- with_tz(dates, tzone = "Australia/Melbourne")
  tdd_month_numbers <- map_dbl(dates, function(date) {
    within_interval <- any(date %within% tdd_month_table$interval)
    
    if (within_interval) {
      tdd_month_number <- tdd_month_table %>%
        filter(date %within% interval) %>%
        pull(TDD_month_number)
      return(tdd_month_number)
    } else {
      return(NA_real_)
    }
  })
  return(tdd_month_numbers)
}

