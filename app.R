#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(utils)
library(astsa)
library(dplyr)
library(tsibble)
library(tidyverse)
library(fpp3)
library(GGally)
library(sugrrants)
library(xts) 
library(lubridate)
library(prophet)
library(DT)
library(shiny)
library(rsconnect)

military_df <- read.csv("military_df.csv")
military_df <- military_df %>%
    mutate_at("Year", ~ymd(paste(.,"01", "01"))) %>%
    as_tsibble(key= c(Code),index = Year)

nuclear <- military_df %>%
    select(c(Year, Entity, nuclear_weapons_stockpile))

fbProphet <- function(data, period) {
    print(data)
    ch <- data %>%
        rename(y = Miliary_exp_mil) %>%
        rename(ds = Year)
    m <- prophet(ch)
    future <- make_future_dataframe(m, periods = period, freq = "year")
    #print(tail(future))
    forecast <- predict(m, future)
    tail(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])
    plot(m, forecast) +
        labs(title= data$Entity,"Military expediture", y = "$US")
    #tail(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])
    #print(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])
    #return(tail(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')]))
}

# Define UI for application that draws a histogram
ui <- fluidPage(
    #shinythemes::themeSelector(),
    theme = shinythemes::shinytheme('spacelab'),
    # Application title
    titlePanel("Military Expenditure Forecasting"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("country", 
                        "Select country", 
                        unique(military_df$Entity)),
            sliderInput("year",
                        "Future #years",
                        min = 1,
                        max = 5,
                        value = 1)
        ),

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                tabPanel("Expenditure trend", plotOutput("linePlot")),
                tabPanel("Forecast", plotOutput("forecast")),
                tabPanel("#Nuclear weapons", DT::DTOutput('weapons'))
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    output$linePlot <- renderPlot({
        data <- military_df %>%
            filter(Entity == input$country)
        data %>%
             autoplot(Miliary_exp_mil) +
             labs(title= input$country,"Military expediture", y = "$US")
    })

    output$forecast <- renderPlot({
       
        data <- military_df %>%
            filter(Entity == input$country)
        
        fbProphet(data, input$year)
    })
    
    output$weapons <- DT::renderDT({
        nuclear %>%
            filter(Entity == input$country)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
