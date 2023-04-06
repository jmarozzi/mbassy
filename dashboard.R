## app.R ##
library(shiny)
library(shinydashboard)
library(nousstyle)
library(tidyverse)

files<- list.files(path = "data", full.names = TRUE)
load(files[1])
load(files[2])
load(files[3])

header <- dashboardHeader(title = "MBassy analysis")
sidebar <- dashboardSidebar(
   sidebarMenu(
     menuItem(
       tabName = "dance_student_registration",
       text = "Dance student registration"
             ),
     menuItem(
       tabName = "dance_class_revenue",
       text = "Dance class revenue"
     ),
     menuItem(
       tabName =  "overall_financials",
       text = "Overall financials"
       )
     )
  )

body <- dashboardBody(

  tabItems(
    tabItem(tabName = "dance_student_registration",
            fluidRow(
              box( width = 6, 
                   title = "Number of registrations per term - casual and upfront",
                   plotOutput(outputId = "g_registrations")
              ),
              box( width = 6, 
                   title = "Number of unique students per term broked down by upfront registrations",
                   plotOutput(outputId = "g_unique_students_by_term")
              )

              )
            ),
    tabItem(tabName = "dance_class_revenue"),
    tabItem(tabName = "overall_financials")
  )
  
 
  )
  


ui <- dashboardPage(
  header = header,
  sidebar = sidebar, 
  body = body
  )
  


server <- function(input, output) {
  
  
  
  output$g_number_of_terms_attended <- renderPlot({
    student_dataset %>% 
      ggplot(aes(x = number_of_terms_attended))+
      geom_bar() + 
      geom_text(aes(label=after_stat(count)),
                stat='count',
                nudge_y=4, size = 4) + 
      labs(x = "Total number of terms attended",
           y = "Number of students") + 
      theme(text = element_text(size = 16),  text = element_text(size = 16))
    }, res = 100)
  
  output$g_unique_students_by_term <- renderPlot({ 
    per_student_total_class_registrations_and_revenue %>% 
    count(time_stamp, any_upfront) %>% 
    group_by(time_stamp) %>% 
    mutate(perc = n / sum(n)) %>% 
    ggplot(aes(x = time_stamp, y = n, fill = factor(any_upfront))) + 
    geom_col() + 
    geom_text(aes(x = time_stamp, y = n, label = scales::percent(perc)), position = position_stack(vjust = 0.5), color = "white") +
    labs(x = "Term", 
         y = "Number of unique students", 
         fill = "Legend") + 
    scale_x_discrete(labels = label_wrap(7)) + 
    theme(legend.position = "bottom", text = element_text(size =16)) 
  }, res = 100)
  
  output$g_registrations <- renderPlot({
    all_terms_full_form %>% 
  select(time_stamp, total_casuals, total_upfront_terms) %>% 
  pivot_longer(cols = c(total_casuals, total_upfront_terms), 
               names_to = "registration_type",
               values_to = "number_of_registrations") %>% 
  mutate(registration_type = ifelse(registration_type == "total_casuals", "Casual", "Upfront Term")) %>% 
  group_by(time_stamp,registration_type) %>% 
  summarise(number_of_registrations = sum(number_of_registrations)) %>% 
  ungroup() %>% 
  group_by(time_stamp) %>% 
  mutate(prop = number_of_registrations/ sum(number_of_registrations)) %>% 
  ggplot(aes(x = time_stamp, y = number_of_registrations, fill = registration_type)) + 
  geom_col() + 
  geom_text(aes(x = time_stamp, y = number_of_registrations, label = scales::percent(prop)), 
            position = position_stack(vjust = 0.5), color = "white", size = 4) +
  labs(x = "Term", 
       y = "Number of registrations", 
       fill = "Legend") + 
  scale_x_discrete(labels = label_wrap(7)) + 
  theme(legend.position = "bottom",  text = element_text(size = 16))  
    
   
    
  }, res = 100)
}



shinyApp(ui, server)
