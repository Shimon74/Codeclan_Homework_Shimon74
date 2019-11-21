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
library(CodeClanData)
library(shinythemes)

all_teams <- unique(olympics_overall_medals$team)


# Define UI for application that draws a histogram
ui <- fluidPage(
    theme = shinytheme("slate"), 
    
    titlePanel(tags$h1("5 Country Medal Comparison")),
    
    fluidRow(
    
                 
          column(6, 
            
            
            radioButtons(
                inputId = "season",
                label = "Summer or Winter",
                choices = c("Summer", "Winter")
            )  
            ),
           
            column(6,       
            
             radioButtons(
                inputId = "medal",
                label = "Medal Won",
                choices = c("Gold",
                            "Silver",
                            "Bronze")
                )
            ),
        
        fluidRow(
            column(8,
                plotOutput("medal_count")
                
            )
        )
        
        
        
    )
)




# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$medal_count <- renderPlot ({
        #browser()
        olympics_overall_medals %>%
            filter(team %in% c("United States",
                               "Soviet Union",
                               "Germany",
                               "Italy",
                               "Great Britain")) %>%
            filter(medal == input$medal) %>%
            filter(season == input$season) %>%
            ggplot() +
            aes(x = team, y = count, fill = medal) +
            geom_col() +
            scale_fill_manual(values = c(
                "Gold" = "#DAA520",
                "Silver" = "#C0C0C0",
                "Bronze" = "#cd7f32"))
        
    })
    
    
}

# Run the application
shinyApp(ui = ui, server = server)
