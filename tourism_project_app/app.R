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
library(readxl)
revenue <- readRDS("revenue.RDS")
visits <- readRDS("Visits.RDS")
full <- readRDS("end.RDS")

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
                   
                     tabPanel("Visits", 
                              checkboxGroupInput("vis", 
                                                 label = "Select Type", 
                                                 choices = unique(visits$tourist_type), 
                                                 selected = "International Visitors"),
                              plotOutput("visPlot"))
                      
                 )
             )), 
             
                
    tabPanel("Factors of Tourism",
             fluidPage(
                 titlePanel("What Factors Influence Tourism Revenue?"),
                 tabsetPanel(
                     tabPanel("Plot",
                 selectInput("factor", label = "Select Factor", 
                             choices = c("victimization", "score", "one_star",
                                         "two_star", "three_star", "four_star",
                                         "five_star", "luxury_percent", 
                                         "luxury_ratio",
                                         "resturants", "nightlife", 
                                         "nightlife_percent", 
                                         "nightlife_ratio", "agencies", 
                                         "schools")),
                 mainPanel(plotOutput("factPlot"))),
                 tabPanel("Factor Definitions",
                          h3("Factors"),
                          p("victimization: "),
                          p("score:"),
                          p("one_star:"),
                          p("two_star:"),
                          p("three_star:"),
                          p("four_star:"),
                          p("five_star:"),
                          p("luxury_percent:"),
                          p("luxury_ratio:"),
                          p("nightlife:"),
                          p("nightlife_percent:"),
                          p("nightlife_ratio:"),
                          p("agencies:"),
                          p("schools:"))
             )
             )
    ), 
    tabPanel("Model",
             titlePanel("Model Discussion"),
             p("Here is the predictive model I created:")),
    tabPanel("About", 
             titlePanel("About"),
             h3("Project Background and Motivations"),
             p("Growing up, I used to travel to Mexico with my parents to visit 
               family members. While most of my time there was spent at my 
               grandparent's ranch, I did occasionaly travel to other cities
               and towns to see museums or go shopping. Prior to the COVID-19
               pandemic, my mother and I were planning a trip to Mexico for the
               sole purpose of being tourists in my mother's country. Since that
               trip was cancelled, I have been interested in learning more about
               the tourism sector of Mexico. With this project, I want to learn 
               who travels to Mexico, where they go to spend their money, why 
               people travel, and what tourist attractions generate the most 
               revenue"),
             h3("About Me"),
             p("My name is Daniel Salgado-Alvarez and I am freshman at Harvard 
             College. I study Government on the Data Science track. 
             You can reach me at dsalgadoalvarez@college.harvard.edu."),
             h3("Data Sources"),
             p("GDP Data:"),
             p("https://www.inegi.org.mx/app/tabulados/default.aspx?pr=17&vr=7&in=38&tp=20&wr=1&cno=2"),
             p("Population Data:"),
             p("https://www.inegi.org.mx/app/tabulados/interactivos/?pxq=
             Poblacion_Poblacion_01_903d6ce7-5e37-4828-8060-61d3116aaec5"),
             p("Victimization Data:"),
             p("https://www.inegi.org.mx/temas/victimizacion/"),
             p("Crime Perception Data:"),
             p("https://www.inegi.org.mx/temas/percepcion/"),
             p("Hotel, Restaurant, Nightlife, Travel Agency, and Tourism School Data:"),
             p("https://datatur.sectur.gob.mx/SitePages/CompendioEstadistico.aspx"),
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
            visits %>%
                filter(tourist_type %in% input$vis) %>%
            ggplot(aes(x = year, y = quantity, color = tourist_type)) +
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
