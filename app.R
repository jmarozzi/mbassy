## app.R ##
library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(janitor)
library(googlesheets4)
library(janitor)
library(lubridate)
library(zoo)
library(readxl)
library(googledrive)
library(scales)
library(rsconnect)



files<- list.files(path = "data", full.names = TRUE)
for (i in 1:length(files)){
  load(files[i])
}


header <- dashboardHeader(title = "Studio analysis")
sidebar <- dashboardSidebar(
   sidebarMenu(
     menuItem(
       tabName = "tdd_targets_overview",
       text = "TDD targets tracker: overview"
     ),
     
     menuItem(
       tabName = "tdd_targets_dance",
       text = "TDD targets: dance classes"
     ),
     menuItem(
       tabName = "tdd_targets_non_dance",
       text = "TDD targets: other"
     ),
     
      menuItem(
       tabName = "summary",
       text = "Summary"
     ),
     menuItem(
       tabName = "dance_student_registration",
       text = "Registrations by term"
             ),
     
     menuItem(
       tabName = "where_did_you_hear_about_us",
       text = "Where did you hear about us"
     ),
     
     menuItem(
       tabName = "student_retention",
       text = "Student retention"
     ),
     
     menuItem(
       tabName = "dance_class_revenue",
       text = "Dance class revenue"
     ),
     
     menuItem(
       tabName = "studio_costs",
       text = "Studio costs"
     ),
     
     menuItem(
       tabName =  "profits",
       text = "Profits"
       ) 
     
     )
  )

body <- dashboardBody(
  tags$head(tags$style(
    HTML('.wrapper {height: auto !important; position:relative}')
  )), 
  
  tabItems(
    tabItem(tabName = "tdd_targets_overview",
            fluidRow(
              box( width = 12, 
                   title = h4("Explanation of TDD targets trackers"),
                   textOutput(outputId = "t_tdd_targets_explanation")
              )),
            fluidRow(  
              box( width = 4, 
                   title = h4("Headline average monthly revenue"),
                   plotlyOutput(outputId = "g_tdd_average_monthly_revenue")),
            
            box( width = 8, 
                 title = h4("Cumulative total revenue"),
                 plotlyOutput(outputId = "g_tdd_cumulative_total")),
    ),
    fluidRow(
      box( width = 12, 
           title = h4("Monthly revenue by TDD month"),
           plotlyOutput(outputId = "g_tdd_monthly_revenue")
      ))),
    tabItem(tabName = "tdd_targets_dance",
            fluidRow(
              box( width = 4, 
                   title = h4("Headline average monthly dance class revenue"),
                   plotlyOutput(outputId = "g_tdd_headline_dance_class_revenue")),
              box( width = 8, 
                   title = h4("Cumulative total dance class revenue"),
                   plotlyOutput(outputId = "g_tdd_cumulative_dance_class_total")),
            ),
            fluidRow(
                box( width = 6, 
                     title = h4("Number of unique student enrollments"),
                     plotlyOutput(outputId = "g_tdd_number_unique_enrollments")),
             
              box( width = 6, 
                   title = h4("Distribution of upfront classes per student"),
                   plotlyOutput(outputId = "g_tdd_upfront_distribution")),
              ),
            
            fluidRow(
              box( width = 6, 
                   title = h4("Average monthly dance class revenue"),
                   plotlyOutput(outputId = "g_tdd_avg_monthly_dance_class_revenue")),
              
              box( width = 6, 
                   title = h4("Monthly dance revenue per student"),
                   plotlyOutput(outputId = "g_monthly_dance_class_revenue_per_student"))
            )),
    
        tabItem(tabName = "tdd_targets_non_dance",
            fluidRow(
              box( width = 6, 
                   title = h4("Average monthly revenue private classes"),
                   textOutput(outputId = "g_tdd_privates_monthly_revenue")
              ),
              
              box( width = 6, 
                   title = h4("Cumulative total revenue private classes"),
                   plotlyOutput(outputId = "g_tdd_privates_cumulative_revenue")),
            ),
            
            fluidRow(
              box( width = 6, 
                   title = h4("Average monthly revenue studio hire"),
                   textOutput(outputId = "g_tdd_studio_hire_monthly_revenue")
              ),
              
              box( width = 6, 
                   title = h4("Cumulative total revenue private classes"),
                   plotlyOutput(outputId = "g_tdd_studio_hire_cumulative_revenue")),
            ),
            fluidRow(
              box( width = 6, 
                   title = h4("Average monthly revenue drinks merch yoga"),
                   textOutput(outputId = "g_tdd_dmy_monthly_revenue")
              ),
              
              box( width = 6, 
                   title = h4("Cumulative total revenue drinks merch yoga"),
                   plotlyOutput(outputId = "g_tdd_dmy_cumulative_revenue")),
            )),
            
            
           
    
    tabItem(tabName = "summary",
            fluidRow(
              box( width = 6, 
                   title = h2("Total unique students"),
                   plotlyOutput(outputId = "g_unique_students")
              ),
              
              box( width = 6, 
                   title = h2("Total dance class revenue"),
                   plotlyOutput(outputId = "g_revenue")),
              ),
            
            fluidRow(
              box( width = 6, 
                   title = h2("Number of new students per term"),
                   plotlyOutput(outputId = "g_new_students_per_term"))
              
            ,
            box( width = 6, 
                 title = h2("Proportion returned students per term"),
                 plotlyOutput(outputId = "g_returned_students_per_term"))
            )
            

            
    ),
    
     tabItem(tabName = "dance_student_registration",
            fluidRow(
              box( width = 12, 
                   title = h2("Number of registrations per term - casual and upfront"),
                   plotlyOutput(outputId = "g_registrations")
              )),
            fluidRow(
              box( width = 12, 
                   title = h2("Number of unique students per term broked down by upfront registrations"),
                   plotlyOutput(outputId = "g_unique_students_by_term"))

              ),
                  
            ),
    
    tabItem(tabName = "where_did_you_hear_about_us", 
            fluidRow(
              box(width = 12, 
                  title = h2("All students - where did you hear about us"), 
                  plotlyOutput(outputId = "g_hear_all"))
            ),
            fluidRow(
              box(
             
                width = 6, 
                  plotlyOutput(outputId = "g_hear_1")), 
              box(width = 6, 
                  plotlyOutput(outputId = "g_hear_2"))
            ),
            fluidRow(
              box(width = 6, 
                  plotlyOutput(outputId = "g_hear_3")), 
              box(width = 6, 
                  plotlyOutput(outputId = "g_hear_4"))
            )),
    
    tabItem(tabName = "student_retention",
            fluidRow(
              box(width = 9 ,

                  title = "Breakdown of students by registration pattern",
                  plotlyOutput(outputId = "g_student_registration_pattern")),
              box(width = 3, 

                  title = "Registration patterns", 
                  HTML("<ul>
             <li>New students = starting in current term</li>
             <li>Returned students = second consecutive term</li>
             <li>Loyal students = third consecutive term</li>
             <li>Resurrected students = returned after an absence of at least 1 term</li>
             <li>Warm lead = students who attended last term but haven;'t signed up for this term</li>
             <li>Dormant students = students who haven't attended for at least two terms</li>
             
           </ul>"))),
            
            fluidRow(
              box(width = 6, 
                  plotlyOutput(outputId =  "g_1_term_prop")),
              
              box(
                width = 6, 
                plotlyOutput(outputId = "g_2_term_prop")
              )
            ),
            
            fluidRow(
              box(width = 6, 
                  plotlyOutput(outputId =  "g_3_term_prop")),
                  
                  box(
                    width = 6, 
                    plotlyOutput(outputId = "g_4_term_prop")))
            ),
            
    tabItem(tabName = "dance_class_revenue",
            fluidRow(
              box(width = 6,
                  plotlyOutput(outputId = "g_weekly_class_revenue")), 
              
              box(width = 6, 
                  plotlyOutput(outputId = "g_total_class_revenue"))),
             
               fluidRow(
                box(width = 6, 
                plotlyOutput(outputId = "g_boxplot_class_revenue_per_student")),
                
                box(width = 6, 
                    plotlyOutput( outputId = "g_revenue_by_quarter")))),
    tabItem(tabName = "studio_costs"),
    tabItem(tabName = "profit")
  ))

 


ui <- dashboardPage(
  header = header,
  sidebar = sidebar, 
  body = body
  )
  


server <- function(input, output) {
  
  output$g_unique_students_by_term <- renderPlotly({ 

      a<- per_student_total_class_registrations_and_revenue %>% 
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
    theme(text = element_text(size =16))+ 
      theme(legend.position = "bottom")
    
    ggplotly(a)
      
    
  })
  
  output$g_registrations <- renderPlotly({
   b <- all_terms_full_form %>% 
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
    
   ggplotly(b)
  })
  
  output$g_number_of_terms_attended <- renderPlotly({
    c <- student_dataset %>% 
      ggplot(aes(x = number_of_terms_attended))+
      geom_bar() + 
      geom_text(aes(label=after_stat(count)),
                stat='count',
                nudge_y=4, size = 4, color = "black") + 
      labs(x = "Total number of terms attended",
           y = "Number of students") + 
      theme(text = element_text(size = 16), legend.position = "bottom")
    
    ggplotly(c)
  })
  

  output$g_student_registration_pattern <- renderPlotly({
    d <- student_dataset %>% 
      mutate(active_flag = ifelse(active_flag ==1, "Active students", "Inactive students")) %>% 
      ggplot(aes(x = student_registration_pattern, fill = active_flag)) + 
      geom_bar() + 
      geom_text(aes(
        label = percent(after_stat(prop)), group = 1), 
        stat = 'count', 
        nudge_y = 3,
        color = "black") + 
      scale_x_discrete(label = wrap_format(5)) + 
      labs(x = "Student registration pattern",
           y = "Number of students", 
           caption = str_c("Total unique of students since Term 4 2022: ", nrow(student_dataset)),
           subtitle =  str_c("Current term: ", start_term_and_number_possible_terms$start_term[1]),
           #title = "Breakdown of students by registration pattern",
           fill = "Legend") + 
      theme( 
           # text = element_text(size = 14), 
            plot.caption = element_text(hjust = 0, face = "bold"),
            legend.position = "bottom")
    
    ggplotly(d)
  })  
 
  
 output$g_1_term_prop <- renderPlotly({
   e <- student_dataset %>%
     filter(start_term == start_term_and_number_possible_terms$start_term[1]) %>%
     ggplot(aes(x = proportion_of_all_terms)) + 
     geom_bar() + 
     scale_x_continuous(labels = percent_format(), breaks = c(0,1, by =1/1))+
     labs(x = "% of all possible terms attended", 
          y = "Number of students starting in term", 
           
          caption = str_c("Total of ", total_1, 
                          " new students started in ", 
                          start_term_and_number_possible_terms$start_term[1])) + 
     geom_text(
       aes(label=after_stat(count)),
       stat='count',
       nudge_x=-0.05,
       nudge_y=1,
       color = "black")+
     geom_text(
       aes(label= str_c("(", percent(after_stat(prop)), ")"), group=1),
       stat='count',
       nudge_x=0.05,
       nudge_y=1,
       color = "black") +
     theme(plot.caption = element_text(hjust = 0, face = "bold"), legend.position = "bottom")
   ggplotly(e)%>% 
     layout(title = list(text = paste0("Retention for students starting in ", 
                                       start_term_and_number_possible_terms$start_term[1])),
            
            margin = list(l = 70, r = 10, b = 100, t = 50),
             annotations = list(x = 0.7 , y = -0.35, text = 
                                 paste0("<b>New students started in this term = ", 
                                total_1, '; Max possible number of terms = ', 
                                start_term_and_number_possible_terms$total_number_of_possible_terms[1],
                                '<b>'), 
                                xref='paper', yref='paper', showarrow = F, 
                                xanchor='right', yanchor='auto', xshift=0, yshift=0,
                                font = list(size = 10)))
 }) 
 
 output$g_2_term_prop <- renderPlotly({
  f <- student_dataset %>%
     filter(start_term == start_term_and_number_possible_terms$start_term[2]) %>%
     ggplot(aes(x = proportion_of_all_terms)) + 
     geom_bar() + 
     scale_x_continuous(labels = percent_format(round(seq(0,1, by =1/2), digits = 1)), breaks = seq(0,1, by =1/2) )+
     labs(x = "Proportion of all possible terms attended", 
          y = "Number of students starting in term") + 
     geom_text(
       aes(label=after_stat(count)),
       stat='count',
       nudge_x=-0.05,
       nudge_y=1,
       color = "black")+
     geom_text(
       aes(label= str_c("(", percent(after_stat(prop)), ")"), group=1),
       stat='count',
       nudge_x=0.05,
       nudge_y=1) +
     theme(plot.caption = element_text(hjust = 0, face = "bold"), legend.position = "bottom")
   ggplotly(f)%>% 
     layout(title = list(text = paste0("Retention for students starting in ", 
                                       start_term_and_number_possible_terms$start_term[2])),
            
            margin = list(l = 70, r = 10, b = 100, t = 50),
            annotations = list(x = 0.7 , y = -0.35, text = 
                                 paste0("<b>New students started in this term = ", 
                                        total_2, '; Max possible number of terms = ', 
                                        start_term_and_number_possible_terms$total_number_of_possible_terms[2],
                                        '<b>'), 
                               xref='paper', yref='paper', showarrow = F, 
                               xanchor='right', yanchor='auto', xshift=0, yshift=0,
                               font = list(size = 10)))
 }) 
 
 output$g_3_term_prop <- renderPlotly({
  g <- student_dataset %>%
     filter(start_term == start_term_and_number_possible_terms$start_term[3]) %>%
     ggplot(aes(x = proportion_of_all_terms)) + 
     geom_bar() + 
     scale_x_continuous(labels = percent_format(round(seq(0,1, by =1/3), digits =1)), breaks = seq(0,1, by =1/3) )+
     labs(x = "% of all possible terms attended", 
          y = "Number of students starting in term") + 
     geom_text(
       aes(label=after_stat(count)),
       stat='count',
       nudge_x=-0.05,
       nudge_y=0.5,
       color = "black")+
     geom_text(
       aes(label= str_c("(", percent(after_stat(prop)), ")"), group=1),
       stat='count',
       nudge_x=0.05,
       nudge_y=0.5) +
     theme(plot.caption = element_text(hjust = 0, face = "bold"), legend.position = "bottom")
   ggplotly(g) %>% 
     layout(title = list(text = paste0("Retention for students starting in ", 
                                       start_term_and_number_possible_terms$start_term[2])),
            
            margin = list(l = 70, r = 10, b = 100, t = 50),
            annotations = list(x = 0.7 , y = -0.35, text = 
                                 paste0("<b>New students started in this term = ", 
                                        total_3, '; Max possible number of terms = ', 
                                        start_term_and_number_possible_terms$total_number_of_possible_terms[3],
                                        '<b>'), 
                               xref='paper', yref='paper', showarrow = F, 
                               xanchor='right', yanchor='auto', xshift=0, yshift=0,
                               font = list(size = 10)))
 }) 
 
 output$g_4_term_prop <- renderPlotly({
   h <- student_dataset %>%
     filter(start_term == start_term_and_number_possible_terms$start_term[4]) %>%
     ggplot(aes(x = proportion_of_all_terms)) + 
     geom_bar() + 
     scale_x_continuous(labels = percent_format(round(seq(0,1, by =1/4), digits = 1)), 
                        breaks = seq(0,1, by =1/4) )+
     labs(x = "% of all possible terms attended", 
          y = "Number of students starting in term") + 
     geom_text(
       aes(label=after_stat(count)),
       stat='count',
       nudge_x=-0.05,
       nudge_y=1,
       color = "black")+
     geom_text(
       aes(label= str_c("(", percent(after_stat(prop)), ")"), group=1),
       stat='count',
       nudge_x=0.05,
       nudge_y=1,
       color = "black")+
     theme(plot.caption = element_text(hjust = 0, face = "bold"), legend.position = "bottom")
   ggplotly(h) %>% 
     layout(title = list(text = paste0("Retention for students starting in ", 
                                       start_term_and_number_possible_terms$start_term[4])),
            
            margin = list(l = 70, r = 10, b = 100, t = 50),
            annotations = list(x = 0.7 , y = -0.35, text = 
                                 paste0("<b>New students started in this term = ", 
                                        total_4, '; Max possible number of terms = ', 
                                        start_term_and_number_possible_terms$total_number_of_possible_terms[4],
                                        '<b>'), 
                               xref='paper', yref='paper', showarrow = F, 
                               xanchor='right', yanchor='auto', xshift=0, yshift=0,
                               font = list(size = 10)))
      
 }) 
 
 output$g_weekly_class_revenue <- renderPlotly({
  i <- combined_statistics %>% 
     ggplot(aes(x = time_stamp, y = weekly_class_revenue)) + 
     geom_col() + 
     scale_y_continuous(labels = dollar_format()) + 
     labs( 
       x = "Term", 
       y = "Weekly class revenue", 
       title = "Weekly class revenue by term") + 
     geom_text(aes(x = time_stamp, y = weekly_class_revenue+50, 
                   label = dollar(weekly_class_revenue)),
               color = "black")
  
  ggplotly(i)
 })
 
 
 output$g_total_class_revenue <- renderPlotly({ 
   j <- combined_statistics %>% 
   ggplot(aes(x = time_stamp, y = total_class_revenue)) + 
   geom_col() + 
   scale_y_continuous(labels = dollar_format(), limits = c(0,15000)) + 
   labs( 
     x = "Term", 
     y = "Total class revenue",
     title = "Total class revenue by term") + 
   geom_text(aes(x = time_stamp, y = total_class_revenue+500,
                 label = dollar(total_class_revenue)),
             color = "black")
 ggplotly(j)
})

 output$g_boxplot_class_revenue_per_student <- renderPlotly({ 
  k <- per_student_total_class_registrations_and_revenue %>% 
   ggplot(aes(x = time_stamp, y =total_class_revenue))+
   geom_boxplot() + 
   labs(title = "Distribution of revenue per student", 
        subtitle  = "Box with white space = 50% of students (Q1-Q3)\nLine in middle of white space = median",
        x = "Term", 
        y = "Class revenue per student") 
 
 ggplotly(k) })
 
output$g_revenue_by_quarter <- renderPlotly({ 
  l <- all_terms_full_form %>% 
  group_by(quarter) %>% 
  summarise(revenue = sum(minus_transaction_fee)) %>% 
  ggplot(aes(x = quarter, y =revenue)) + 
  geom_col() +
  scale_y_continuous(labels = label_dollar()) +
  #scale_x_discrete(labels = c("Q1 22-23", "Q2 22-23", "Q3 22-23"))
  geom_text(aes(x = quarter, y =revenue +500, label = dollar(revenue)), 
            color = "black") + 
  labs(
    x = "Fiscal quarter", 
    y = "Dance class revenue", 
    title = "Dance class revenue by fiscal quarter",
    subtitle = "Financial year = FY22/23 i.e. fiscal year 2023")
  ggplotly(l)
  })


output$g_hear_all <- renderPlotly({
  l <- student_dataset %>% 
    group_by(how_did_you_hear_about_us) %>% 
    tally() %>% 
    arrange(n) %>% 
    ungroup() %>% 
    mutate(perc = n/sum(n)) %>% 
    filter(!is.na(how_did_you_hear_about_us)) %>% 
    ggplot(aes(x = reorder(how_did_you_hear_about_us,n), y = n)) + 
    geom_col() + 
    scale_x_discrete(labels = label_wrap(15)) + 
    
    labs(y = "Number of students", 
         x = "Where they heard about us" ) + 
    coord_flip() + 
    geom_text(aes(x =reorder(how_did_you_hear_about_us,n), y = n + 3, 
              label = str_c(n, " (", percent(perc), ")"  )), color = "black")
  
  ggplotly(l)
  
})

output$g_hear_1 <- renderPlotly({
  m <- student_dataset %>% 
    filter(start_term == start_term_and_number_possible_terms$start_term[1]) %>%
    group_by(how_did_you_hear_about_us) %>% 
    tally() %>% 
    arrange(n) %>% 
    ungroup() %>% 
    mutate(perc = n/sum(n)) %>% 
    filter(!is.na(how_did_you_hear_about_us)) %>% 
    ggplot(aes(x = reorder(how_did_you_hear_about_us,n), y = n)) + 
    geom_col() + 
    scale_x_discrete(labels = label_wrap(15)) + 
    
    labs(y = "Number of students", 
         x = "Where they heard about us", 
         title = paste0("Students starting ", start_term_and_number_possible_terms$start_term[1])) + 
    coord_flip() + 
    geom_text(aes(x =reorder(how_did_you_hear_about_us,n), y = n, 
                  label = str_c(n, " (", percent(perc), ")"  )), color = "black")
  
  ggplotly(m)
  
})

output$g_hear_2 <- renderPlotly({
  n <- student_dataset %>% 
    filter(start_term == start_term_and_number_possible_terms$start_term[2]) %>%
    group_by(how_did_you_hear_about_us) %>% 
    tally() %>% 
    arrange(n) %>% 
    ungroup() %>% 
    mutate(perc = n/sum(n)) %>% 
    filter(!is.na(how_did_you_hear_about_us)) %>% 
    ggplot(aes(x = reorder(how_did_you_hear_about_us,n), y = n)) + 
    geom_col() + 
    scale_x_discrete(labels = label_wrap(15)) + 
    
    labs(y = "Number of students", 
         x = "Where they heard about us",
         title = paste0("Students starting ", start_term_and_number_possible_terms$start_term[2])) + 
    coord_flip() + 
    geom_text(aes(x =reorder(how_did_you_hear_about_us,n), y = n, 
                  label = str_c(n, " (", percent(perc), ")"  )), color = "black")
  
  ggplotly(n)
  
})

output$g_hear_3 <- renderPlotly({
  o <- student_dataset %>% 
    filter(start_term == start_term_and_number_possible_terms$start_term[3]) %>%
    group_by(how_did_you_hear_about_us) %>% 
    tally() %>% 
    arrange(n) %>% 
    ungroup() %>% 
    mutate(perc = n/sum(n)) %>% 
    filter(!is.na(how_did_you_hear_about_us)) %>% 
    ggplot(aes(x = reorder(how_did_you_hear_about_us,n), y = n)) + 
    geom_col() + 
    scale_x_discrete(labels = label_wrap(15)) + 
    
    labs(y = "Number of students", 
         x = "Where they heard about us",
         title = paste0("Students starting ", start_term_and_number_possible_terms$start_term[3])) + 
    coord_flip() + 
    geom_text(aes(x =reorder(how_did_you_hear_about_us,n), y = n, 
                  label = str_c(n, " (", percent(perc), ")"  )), color = "black")
  
  ggplotly(o)
  
})

output$g_hear_4 <- renderPlotly({
  p <- student_dataset %>% 
    filter(start_term == start_term_and_number_possible_terms$start_term[4]) %>%
    group_by(how_did_you_hear_about_us) %>% 
    tally() %>% 
    arrange(n) %>% 
    ungroup() %>% 
    mutate(perc = n/sum(n)) %>% 
    filter(!is.na(how_did_you_hear_about_us)) %>% 
    ggplot(aes(x = reorder(how_did_you_hear_about_us,n), y = n)) + 
    geom_col() + 
    scale_x_discrete(labels = label_wrap(15)) + 
    
    labs(y = "Number of students", 
         x = "Where they heard about us", 
         title = paste0("Students starting ", start_term_and_number_possible_terms$start_term[4])) + 
    coord_flip() + 
    geom_text(aes(x =reorder(how_did_you_hear_about_us,n), y = n, 
                  label = str_c(n, " (", percent(perc), ")"  )), color = "black")
  
  ggplotly(p)
  
})

output$g_unique_students <- renderPlotly({
  
  
  total_students <- tibble(
    start_term = "Total",
    n = nrow(student_dataset))
  
  
  q <- student_dataset %>% 
    group_by(start_term) %>%   
    tally() %>%
    rbind(total_students) %>% 
    ggplot(aes(x = start_term, y = n)) + 
    geom_col() + 
    scale_x_discrete(labels = label_wrap(15)) + 
    #scale_y_continuous(limits = c(0,200))+
    
    labs(y = "Number of unique students", 
         x = "Term") +
    geom_text(aes(x = start_term, y = n + 4, 
                  label = n), color = "black")
  
  ggplotly(q)
  
})


output$g_revenue <- renderPlotly({
  
total_revenue <- tibble( 
  time_stamp = "Total", 
  revenue = all_terms_full_form %>% 
    summarise(revenue = sum(minus_transaction_fee)) %>% 
    pull()) 


r <- all_terms_full_form %>% 
    group_by(time_stamp) %>%
    summarise(revenue = sum(minus_transaction_fee)) %>% 
  arrange(desc(time_stamp)) %>% 
    rbind(total_revenue) %>% 
  ggplot(aes(x = time_stamp, y = revenue)) + 
  geom_col() + 
  scale_x_discrete(labels = label_wrap(15)) + 
  scale_y_continuous(labels = dollar_format())+
  
  labs(y = "Revenue", 
       x = "Term") +
  
  geom_text(aes(x = time_stamp, y = revenue + 500, 
                label = dollar(round(revenue))), color = "black")

ggplotly(r)
  
})

output$g_new_students_per_term <- renderPlotly({
  student_dataset %>% 
    group_by(start_term) %>% 
    count(start_term) %>% 
    ggplot(aes(x= start_term, y = n)) + 
    geom_col()+
    labs( 
      x = "Term",
      y = "Number of new students") + 
    geom_text(aes(x = start_term, y = n + 3, label = n), color = "black")
})  



output$g_returned_students_per_term <- renderPlotly({
 
start <- student_dataset %>% 
    select(unique_id, start_term)

s<- all_terms_full_form %>%
  select(unique_id, time_stamp) %>% 
  count(time_stamp, unique_id) %>% 
  left_join(start) %>% 
  mutate(new_or_returned = ifelse(time_stamp == start_term, "New", "Returned")) %>% 
  group_by(time_stamp, new_or_returned) %>% 
  summarise(total = n()) %>% 
  ungroup() %>% 
  group_by(time_stamp) %>% 
  mutate(total_term = sum(total)) %>% 
  ungroup() %>% 
  mutate(percent = total/total_term) %>% 
    ggplot(aes(x = time_stamp, y = percent,  fill = new_or_returned)) + 
    geom_col() + 
    geom_text(aes(x = time_stamp, 
                  y = percent,
                  label = percent(percent)), 
              group = 1, 
              position =position_stack(vjust = 0.8), 
              color = "black") +
       
    scale_x_discrete(label = wrap_format(9)) + 
    labs(x = "Term",
         y = "Proportion of unique students", 
                 #title = "Breakdown of students by registration pattern",
         fill = "Legend") + 
    theme( 
      # text = element_text(size = 14), 
      plot.caption = element_text(hjust = 0, face = "bold"),
      legend.position = "bottom")
  
  ggplotly(s)
})  

}
 

shinyApp(ui, server)
