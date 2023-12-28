#Temporary ppt 
today_date <- today()

presentation <- read_pptx() %>% 
  add_slide(layout = "Title Slide", master = "Office Theme") %>%
    ph_with(value = "MBassy statistics", location = ph_location_type(type = "ctrTitle")) %>%
    ph_with(value = as.character(today_date), location = ph_location_type(type = "subTitle")) %>% 
  
  add_slide(layout = "Section Header", master = "Office Theme") %>%
    ph_with(value = "Plots Section", location = ph_location_type(type = "title")) %>% 
  
  add_slide(layout = "Title and Content", master = "Office Theme") %>% 
   ph_with(value = g_number_of_terms_attended, location = ph_location_fullsize()) %>% 
   #ph_with(value = g_number_of_terms_attended_gt, ph_location_type(type = "title")) %>% 

  add_slide(layout = "Title and Content", master = "Office Theme") %>% 
    ph_with(value = g_unique_students_by_term, location = ph_location(left = 0.5, top = 1.5, width = 9, height = 5.5)) %>% 
    #ph_with(value = g_unique_students_by_term_gt, ph_location_type(type = "title")) %>%   
  
  
  add_slide(layout = "Title and Content", master = "Office Theme") %>% 
    ph_with(value = g_registrations, location = ph_location_fullsize()) %>% 
    #ph_with(value = g_registrations_gt, ph_location_type(type = "title")) 
  
  add_slide(layout = "Title and Content", master = "Office Theme") %>% 
    ph_with(value = g_1_term_prop, location = ph_location(left = 0, top = 2, width = 5, height = 4)) %>%
    ph_with(value = g_2_term_prop, location = ph_location(left = 5.5, top = 2, width = 5, height = 4)) %>% 
  
  add_slide(layout = "Title and Content", master = "Office Theme") %>% 
    ph_with(value = g_3_term_prop, location = ph_location(left = 0, top = 2, width = 5, height = 4)) %>%
    ph_with(value = g_4_term_prop, location = ph_location(left = 5.5, top = 2, width = 5, height = 4)) %>%

  add_slide(layout = "Section Header", master = "Office Theme") %>%
    ph_with(value = "Plots Section", location = ph_location_type(type = "title")) %>% 
  
  add_slide(layout = "Title and Content", master = "Office Theme") %>% 
    ph_with(value = g_weekly_class_revenue, location = ph_location_fullsize()) %>% 

  add_slide(layout = "Title and Content", master = "Office Theme") %>% 
    ph_with(value = g_total_class_revenue, location = ph_location_fullsize()) %>% 

  add_slide(layout = "Title and Content", master = "Office Theme") %>% 
    ph_with(value = g_boxplot_class_revenue_per_student, location = ph_location_fullsize()) %>% 
  
  add_slide(layout = "Title and Content", master = "Office Theme") %>% 
    ph_with(value = g_revenue_by_quarter, location = ph_location_fullsize()) 
  
# presentation <- create_nous_pptx(title = "MBassy statistics",
#                  subtitle = as.character(today_date)) %>% 
#   add_nous_section_pptx("Student registration analysis") %>% 
#   add_nous_plot_pptx(plot = g_number_of_terms_attended, 
#                      gt = g_number_of_terms_attended_gt) %>% 
#   add_nous_plot_pptx(g_unique_students_by_term, 
#                      gt =g_unique_students_by_term_gt) %>% 
#   add_nous_plot_pptx(g_registrations, 
#                      gt = g_registrations_gt) %>% 
#   add_nous_plot_pptx(plot =g_1_term_prop, 
#                      plot_two = g_2_term_prop,
#                      layout = "2 column content",
#                      gt = "Most recent starting terms: breakdown of students by % of all terms attended") %>% 
#   add_nous_plot_pptx(plot = g_3_term_prop, 
#                      plot_two = g_4_term_prop,
#                      layout = "2 column content",
#                      gt = "Older starting terms: breakdown of students by % of all terms attended") %>% 
#   add_nous_section_pptx("Class revenue analysis") %>% 
#   add_nous_plot_pptx(g_weekly_class_revenue,
#                         gt =g_weekly_class_revenue_gt) %>% 
#   add_nous_plot_pptx(g_total_class_revenue, 
#                       gt = g_total_class_revenue_gt) %>%
#   add_nous_plot_pptx(g_boxplot_class_revenue_per_student, 
#                      gt = g_boxplot_class_revenue_per_student_gt) %>% 
#   add_nous_plot_pptx(g_revenue_by_quarter, 
#                      gt = g_revenue_by_quarter_gt)
  
  
print(presentation, str_c("outputs/Mbassy-statistics-",today_date, ".PPTX"))
