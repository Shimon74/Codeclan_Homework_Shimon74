
#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(ggplot2)
library(CodeClanData)
library(shinythemes)



ui <- fluidPage(theme = shinytheme("yeti"),
                
                
                # Application title
                titlePanel("Sales by Platform Related to Scores"),
                
                
                
                # Drop down inputs to select game platform and year  
                fluidRow(
                    
                    column(6,
                           
                           
                           selectInput("platform", 
                                       "Select Platform",
                                       choices = unique(game_sales$platform)),
                    ),
                    column(6,
                           
                           selectInput("year",
                                       "Select Year",
                                       choices = unique(game_sales$year_of_release)),
                    )
                    
                ),
                
                
                # fluidrow output of plots
                fluidRow(
                    
                    
                    
                    plotOutput("game_sales_plot"),
                    
                    plotOutput("game_score_plot")
                    
                )
                
)


server <- function(input, output) {
    
    filtered_game_data <- reactive({
        game_sales %>%
            filter(platform == input$platform)  %>%
            filter(year_of_release == input$year)
    })
    
    output$game_sales_plot <- renderPlot({
        
        
        ggplot(filtered_game_data()) +
            aes(x = genre, y = sales, fill = publisher) +
            geom_col() +
            labs(x = "GENRE", y = "SALES") + 
            theme_minimal()
    })
    
    output$game_score_plot <- renderPlot({
        
        
        ggplot(filtered_game_data()) +
            aes(x = user_score, y = critic_score, colour = publisher) +
            geom_point(size = 2) +
            facet_grid(~genre) +
            labs(x = "USER SCORE", y = "CRITIC SCORE") +
            theme_light()
    })
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)