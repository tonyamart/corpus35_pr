library(shiny)
library(tidyverse)

data <- read.csv("data/corpus_sample.csv")
glimpse(data)
# columns: id, text, text_lemm, Author_Initials, Author_birth, Author_death, Year, Text_title, First_line, biblio

ui <- fluidPage(
  titlePanel(h3("random poem from corpus")),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Some strange poems for today"),
      
      selectInput("year",
                  label = "Choose year of publication",
                  choices = c(#'1835', 
                              '1836', 
                              '1837', 
                              '1838', 
                              '1839', 
                              '1840'),
                  selected = '1835')
    ),
    mainPanel(h3('testy test'),
              verbatimTextOutput("text")) # var to be used in server
  )
)

server <- function(input, output) {
  output$text <- renderText({
    
    poem <- data %>% 
      filter(Year == as.numeric(input$year)) %>% 
      sample_n(1)
    
    paste("You have selected", input$year, "\nHere is your random poem:\n\n", 
          '\t', poem[1,9], '\n\n',
          poem[1,3], 
          '\n\nAuthor:', poem[1,5], 
          '\n\nSource:', poem[1,11])
  })
}

shinyApp(ui = ui, server = server)
  
