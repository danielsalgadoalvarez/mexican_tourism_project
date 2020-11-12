#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
revenue <- readRDS("revenue.RDS")
visits <- readRDS("Visits.RDS")
full <- readRDS("test.RDS")

# Define UI for application that draws a histogram
ui <- navbarPage(
    "International Tourism in Mexico",
    tabPanel("Size of Tourism",
             fluidPage(
                 titlePanel("How Big is the Tourism Sector?"),
                 tabsetPanel(
                     tabPanel("Revenue",
                              checkboxGroupInput("rev", label = "Select Reason", 
                                                 choices = unique(revenue$reason), 
                                                 selected = "All"),
                              plotOutput("revPlot")), 
                     tabPanel("Visits", plotOutput("visPlot"))
                    
                 )
             )), 
             
                
    tabPanel("Factors of Tourism",
             fluidPage(
                 titlePanel("What Factors Influence Tourism Revenue?"),
                 selectInput("factor", label = "Select Factor", choices = c("victimization", "score")),
                 mainPanel(plotOutput("factPlot"))
             )
    ), 
    tabPanel("Discussion",
             titlePanel("Discussion Title"),
             p("Tour of the modeling choices you made and 
              an explanation of why you made them")),
    tabPanel("About", 
             titlePanel("About"),
             h3("Project Background and Motivations"),
             p("Growing up, I used to travel to Mexico with my parents to visit 
               family members. While most of my time there was spent at my 
               grandparent's ranch, I did occasionaly travel to other cities
               and towns to see musuems or go shopping. Prior to the COVID-19
               pandemic, my mother and I were planning a trip to Mexico for the
               sole purpose of being tourists in my mother's country. Since that
               trip was cancelled, I have been interested in learning more about
               the tourism sector of Mexico. With this project, I want to learn 
               who travels to Mexico, where they go to spend their money, why 
               people travel, and who benefits from the tourist economy"),
             h3("About Me"),
             p("My name is Daniel Salgado-Alvarez and I am freshman at Harvard 
             College. I study Government on the Data Science track. 
             You can reach me at dsalgadoalvarez@college.harvard.edu."),
             h3("Repo"),
             p("https://github.com/danielsalgadoalvarez/final_project")))

server <- function(input, output) {
    
    output$revPlot <- renderPlot({
        revenue %>%
            filter(reason %in% input$rev) %>%
        ggplot(aes(x = year, y = revenue, color = reason)) +
            geom_line(na.rm = TRUE) +
            labs(title = "Total Revenue from International Tourism by Reason 
                 for Trip",
                 x = "Year",
                 y = "Revenue in Millions of Dollars")})  
   
        output$visPlot <- renderPlot({
            ggplot(visits, aes(x = year, y = quantity, color = tourist_type)) +
                geom_line(na.rm = TRUE) +
                labs(title = "Total Number of International Tourists by Distinction",
                     x = "Year",
                     y = "Number of Tourists")      
    })
        
        output$factPlot <- renderPlot({
            ggplot(full, aes_string(x = input$factor, y = "GDP_capita")) +
                geom_point(na.rm = TRUE) +
                geom_smooth() +
                labs(title = "Tourism GDP",
                     x = input$factor,
                     y = "Tourism GDP Per Capita")      
        })
}

# Run the application 
shinyApp(ui = ui, server = server)
