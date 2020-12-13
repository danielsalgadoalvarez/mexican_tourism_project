#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


# Loading needed library packages

library(shiny)
library(tidyverse)
library(readxl)
library(gt)
library(ggthemes)
library(shinythemes)
library(wesanderson)

# Loading needed RDS files and setting the seed

revenue <- readRDS("revenue.RDS")
visits <- readRDS("visits2.RDS")
full <- readRDS("end.RDS")
table2 <- readRDS("table2.RDS")
destinations <- readRDS("destinations.RDS")
set.seed(999)
options(scipen=99999999)

# User interface

ui <- navbarPage(
    "INTERNATIONAL TOURISM IN MEXICO",
    
# First tab is about the size of tourism. I set up three subtabs. I also set
# the theme to sandstone.

    tabPanel("Size of Tourism",
             fluidPage(
                 theme = shinytheme("sandstone"),
                 titlePanel("How Big is the Tourism Sector?"),
                 tabsetPanel(
                     
# Creates a plot of revenue using the check box input. It can have multiple 
# check box selected at once.

                     tabPanel("Revenue",
                              checkboxGroupInput("rev", 
                                            label = "Select Reason", 
                                            choices = unique(revenue$reason), 
                                            selected = "All"),
                              plotOutput("revPlot")),
                   
# Creates anothe plot of visists using the same steps as above. 
                     
                     tabPanel("Visits", 
                              checkboxGroupInput("vis", 
                                        label = "Select Type", 
                                        choices = unique(visits$tourist_type), 
                                        selected = "International Visitors"),
                              plotOutput("visPlot"),
                              p(),
                              
# Definitions of the types of tourists and visitors
                              
                              HTML("<h5><b>International Visitors:</b> All 
                              people who visit
                              Mexico for less than twelve months.
                              The main purpose of the trip 
                              should not be to be paid within Mexico.</h5>"),
                              HTML("<h5><b>International Tourists:</b> Includes 
                              all tourists
                                that stay in Mexico for at least a
                                day.</h5>"),
                              HTML("<h5><b>Receptive Tourism:</b> International 
                              tourists who
                                stay within the borders of Mexico.</h5>"),
                              HTML("<h5><b>Border Tourism:</b> International 
                              tourists who
                                stay at the border of Mexico.</h5>"),
                              HTML("<h5><b>International Excursions/Day 
                              Trips:</b> Tourism 
                                that involves visiting Mexico for less 
                                   than a day.</h5>"),
                              HTML("<h5><b>Border Excursions/Day Trips:</b> 
                              Visitors that stay
                                within Mexico for less than a day.</h5>"),
                              HTML("<h5><b>Cruise Ship Trips:</b> Visitors that 
                              temporarily stay in Mexico because of a 
                              cruise.</h5>")),

# Creates a plot of GDP by destination/state. You can use select input to change
# the measurement from GDP_millions to GDP_capita

                     tabPanel("Destinations",
                              selectInput("measure", 
                                                 label = "Select Measure", 
                                                 choices = c("GDP_millions",
                                                             "GDP_capita"), 
                                                 selected = "GDP_millions"),
                     plotOutput("destPlot"),
                     
# Outputs an image that is a map of Mexico 
                     
                     imageOutput("map", width = "100%", height = "100%")
                    )))), 
             
# The second tab of the website is about the factors that influence tourism
# revenue. I also included to subtabs

    tabPanel("Factors of Tourism",
             fluidPage(
                 titlePanel("What Factors Influence Tourism Revenue?"),
                 tabsetPanel(

# The first sub tab creates a plot based on the factor you choose from the 
# select input.
    
                     tabPanel("Plot",
                 selectInput("factor", label = "Select Factor", 
                             choices = c("one_star",
                                         "two_star", "three_star", "four_star",
                                         "five_star", "luxury_percent", 
                                         "luxury_ratio",
                                         "restaurants", "nightlife", 
                                         "nightlife_percent", 
                                         "nightlife_ratio", "agencies", 
                                         "schools", "victimization", "score")),
                 mainPanel(plotOutput("factPlot"))),
                 
# A list of definitions for the factors in the plot subtab. 
                 
                 tabPanel("Factor Definitions",
                          h3("Factors"),
                          HTML("<h5><b>one_star:</b> Number of one star hotels 
                            per 100,000 residents</h5>"),
                          HTML("<h5><b>two_star:</b> Number of two star hotels 
                            per 100,000 residents</h5>"),
                          HTML("<h5><b>three_star:</b> Number of three star 
                            hotels per 100,000 residents</h5>"),
                          HTML("<h5><b>four_star:</b> Number of four star hotels
                            per 100,000 residents</h5>"),
                          HTML("<h5><b>five_star:</b> Number of five star hotels
                            per 100,000 residents</h5>"),
                          HTML("<h5><b>luxury_percent:</b> Percent of four and 
                            five star hotels</h5>"),
                          HTML("<h5><b>luxury_ratio:</b> Ratio of four and five 
                            star hotels to one, two, and three star 
                            hotels</h5>"),
                          HTML("<h5><b>nightlife:</b> Number of nightlife venues
                            (nightclubs, bars, discos, and dancehalls per 
                            100,000 residents)</h5>"),
                          HTML("<h5><b>restaurants:</b> Number of restuarants 
                            and cafes per 100,000 residents</h5>"),
                          HTML("<h5><b>nightlife_percent:</b> Percent of 
                            nightlife venues (nightclubs, bars, discos, and 
                            dancehalls)</h5>"),
                          HTML("<h5><b>nightlife_ratio:</b> Ratio of nighlife 
                            venues to restaurants and cafes</h5>"),
                          HTML("<h5><b>agencies:</b> Number of travel agencies 
                            and tour guides</h5>"),
                          HTML("<h5><b>victimization:</b> Number of victims of 
                            crime (violent and property crimes) per 100,000 
                            residents"),
                          HTML("<h5><b>score:</b> Percent of survey respondents 
                            that perceived their state to be unsafe or 
                            violent"),
                          HTML("<h5><b>schools:</b> Number of tourism vocational
                            schools per 100,000 residents")
                          )))), 

# The third table is for a discussion of my linear model and its results

    tabPanel("Model",
             titlePanel("Model Discussion"),
             
# A discussion of my initial thoughts and ideas.
             
                      h3("Preliminary Thoughts"),
                      p("From my initial data exploration, it became clear that
                        most tourism GDP is generated from international 
                        tourists and visitors that are traveling for pleasure. 
                        Because of that, I collected data on various variables 
                        related to the experience of these tourists. This ranges
                        from data on hotels, restaurants, and travel agencies to
                        domestic variables like tourism schools and perception
                        of crime. Because each state has different population 
                        size and number of urband centers, I made sure to make
                        data in either percents, ratios, or standardized formats
                        out of 100,000 residents. Furthermore, I used prior
                        knowledge and my intuition when making these ratios. For
                        example, the luxury ratio compares four and five star 
                        hotels with lower rated hotels because the former 
                        generates more money due to their high prices and focus
                        on international tourists.
                        After creating simple linear regressions for each 
                        variable, as seen in the Factors of Tourism section,
                        I decided that luxury_ratio, nightlife_ratio, agencies,
                        schools, and score to include in my model. This formula
                        had a relatively low error (RMSE) in comparison to other
                        formulas."),

# Used withMathJax() to make my mathematical model show up

                      h3("Mathematical Model"),
                      withMathJax(),
                      helpText('$$ GDP_i = \\beta_0 + \\beta_1 luxury_i + 
                      \\beta_2 nightlife_i + \\beta_3 agencies_i
                        + \\beta_4 schools_i + \\epsilon_i$$'),

# Outputs a table fo my regression

                      h3("Table"),
                      gt_output("table"),

# A discussion of that table

                      h3("Analysis"),
                      p("As we can see from the table of this predictive model,
                        most of the variables in the model can not be determined 
                        to be significant predictors because the confidence
                        intervals contain zero. Only two variables have 
                        confidence intervals that do not contain zero: 
                        luxury_ratio and schools. From the results, it seems 
                        that luxury_ratio and GDP_capita is positively 
                        correlated.
                        For instance, if there are two states with one state 
                        having a 0.5 luxury_ratio and the other having a 0.6 
                        luxury ration, the second one is predicted to have 
                        around 25.7 pesos more in per capita tourism GDP. 
                        The schools variable and GDP_capita seems to be 
                        negatively correlated. When comparing two states, a 
                        state with an additional tourism vocational school per 
                        100,000 residents would
                        be predicted to have about 55 pesos less in per capita
                        tourism GDP. The number of travel agencies per 100,000
                        residents, the ratio of nightlife to restaurant venues, 
                        or the perception of crime can not be determined to 
                        be significant predictors of GDP_capita; however, 
                        including these variables in the model does lower the
                        overall error (RMSE)"),

# A discusion of further possible research and potential problems

                      h3("Further Discussion"),
                      p("From this data analysis, there are more questions that 
                      arise for future reseaarchers. First, there is the fact 
                      that nightlife_ratio, score, and agencies were not proven 
                      to be significant. While this project did not disprove 
                      the significance of these variables, the results do go
                      against common intuition about tourism. This could be 
                      because of the small data pool. Regardless, further 
                      research could help explore this issue. Second, their is 
                      the fact that schools is negatively correlated with 
                      GDP_capita. Since this is a predictive model, we can not 
                      prove a causal relationship. Furthemore, this correlation
                      could be because of confounding variables. However, this
                      surprising result does warrant further investigation into
                      whether or not publicly funded tourism schools are 
                      effective. 
                      Finally, the positive correlation between luxury_ratio and
                      GDP_capita suggests that investing in four and five star
                      hotels over lower star rated hotels predicts increased
                      tourism GDP per capita. Again, this project is predictive 
                      and not causal. Consequently, state governments will 
                      probably want more research and evidence before changing
                      their hotel investment plans. As we can see, tourism is a
                      complex topic that can not be easily modeled or 
                      understood. Tourism clearly has a large impact on the 
                      economy of Mexico. With a potential reopening of the 
                      tourism industry once the COVID-19 pandemic ends, it is 
                      important for Mexico to consider what tourism investment
                      strategies will benefit the country the most.
                        ")
                      ),

# The fourth and final tab is an about page

    tabPanel("About", 
        titlePanel("About"),
        
# backgroun info about why I wanted to do this project        
        
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

# Personal and contact information

        h3("About Me"),
        HTML("<h5>My name is Daniel Salgado-Alvarez and I am freshman at 
             Harvard College. I study Government on the Data Science track. 
             You can reach me at <b>dsalgadoalvarez@college.harvard.edu</b>"),

# Links for all the data sources I used. Keep in mind that all the source data
# and website were originally in Spanish. I had to translate all of this for
# the purposes of this project.

        h3("Data Sources"),
        h5("GDP Data"),
        tags$a(href="https://www.inegi.org.mx/app/tabulados/default.aspx?pr
                    =17&vr=7&in=38&tp=20&wr=1&cno=2", "Click Here!"),
        h5("Population Data"),
        tags$a(href="https://www.inegi.org.mx/app/tabulados/interactivos/?pxq=
             Poblacion_Poblacion_01_903d6ce7-5e37-4828-8060-61d3116aaec5",
                    "Click Here!"),
        h5("Victimization Data"),
        tags$a(href="https://www.inegi.org.mx/temas/victimizacion/",
                    "Click Here!"),
        h5("Crime Perceeption Data"),
        tags$a(href="https://www.inegi.org.mx/temas/percepcion/",
                    "Click Here!"),
        h5("Hotel, Restaurant, Nightlife, Travel Agency, 
                    and Tourism School Data"),
        tags$a(href="https://datatur.sectur.gob.mx/SitePages/CompendioEstad
                    istico.aspx", "Click Here!"),

# Link to the GitHub Repo for this project

        h5("GitHub Repo"),
        tags$a(href="https://github.com/danielsalgadoalvarez/tourism_project",
                    "Click Here!")))
             
# The server

server <- function(input, output) {

# The code needed to output the revenue plot based on the input. I made sure to
# standardize the theme and color scheme for all plots
       
    output$revPlot <- renderPlot({
        revenue %>%
            filter(reason %in% input$rev) %>%
        ggplot(aes(x = year, y = revenue, color = reason)) +
            geom_line(na.rm = TRUE, size = 1) +
            theme_wsj() +
            theme(axis.title=element_text(size= 18)) +
            theme(plot.caption =element_text(size= 10)) +
            scale_color_manual(name = "Reason for Trip", values = c("#B10318", 
                                                                    "#FF7F0F", 
                                                                    "Red", 
                                          "#663300", "#FFB022", "#8B7C6E")) +
            scale_y_continuous(n.breaks = 8, labels = scales::comma) +
            scale_x_continuous(n.breaks = 10) +
            labs(title = "Total Revenue from International Tourism by Reason",
                 subtitle = "1990 - 2018",
                 x = "Year",
                 y = "Revenue in Millions of Pesos",
                 caption = "Source: Instituto Nacional de 
                 Estadística y Geografía") 
        })  

# This code renders the plot of visitors based on the selected tourist types    
       
        output$visPlot <- renderPlot({
            visits %>%
                filter(tourist_type %in% input$vis) %>%
            ggplot(aes(x = year, y = quantity, color = tourist_type)) +
                geom_line(na.rm = TRUE, size = 1) +
                theme_wsj() +
                theme(axis.title=element_text(size= 18)) +
                theme(plot.caption =element_text(size= 10)) +
                scale_color_manual(name = "Type of Tourist", 
                                   values = c("#B10318", "#FF7F0E", "Red", 
                                              "#663300", "#FFB022", "#8B7C6E",
                                              "#CC6600")) +
                scale_y_continuous(n.breaks = 8, labels = scales::comma) +
                labs(title = "Total Number of International Tourists
                     by Distinction",
                     subtitle = "1990 - 2018",
                     x = "Year",
                     y = "Number of Tourists",
                     caption = "Source: Instituto Nacional de Estadística y
                     Geografía")      
    })
       
# This coder renders the plots of GDP revenue by a selected factor        
         
        output$factPlot <- renderPlot({
            ggplot(full, aes_string(x = input$factor, y = "GDP_capita")) +
                geom_point(na.rm = TRUE) +
                geom_smooth(color = "#B10318") +
                theme_wsj() +
                theme(axis.title=element_text(size= 18)) +
                theme(plot.caption =element_text(size= 10)) +
                scale_x_continuous( labels = scales::comma) +
                scale_y_continuous( labels = scales::comma) +
                labs(title = "Tourism GDP",
                     subtitle = "2003 - 2018",
                     x = input$factor,
                     y = "Tourism GDP Per Capita",
                     caption = "Source: Compendio Estadístico del Turismo en
                     México 2019")      
        })

# This code renders the table in the model tab        
                
        output$table <- render_gt({
           table2 
        })

# This code render the destinations plot based on the GDP measure selected        
                
        output$destPlot <- renderPlot({
            destinations %>%
            ggplot(aes_string(x = input$measure, y = "state")) +
                geom_col(fill = "#B10318") +
                theme_wsj() +
                theme(axis.title=element_text(size= 18)) +
                theme(plot.caption =element_text(size= 10)) +
                scale_x_continuous( labels = scales::comma) +
                labs(title = "Tourism Revenue by State",
                     subtitle = "2018",
                     y = "State",
                     caption = "Source: Instituto Nacional de Estadística y
                     Geografía")
        })
        
# This code renders the image of the map of Mexico        
        
    output$map <- renderImage({
        list(src = 'map.jpg',
            height = 330,
            width = 500,
            style = "display: block; margin-left: auto; margin-right: auto;")},
        deleteFile = FALSE)
            
}
        


# Run the application 
shinyApp(ui = ui, server = server)
