library(shiny)
library(reactable)
library(tidyverse)


######### Load data #########
load("data/corpus_1835.Rda")
load("data/periodicals_corpus.Rda")
load("data/collections_corpus.Rda")

authors <- read.delim("data/authors.tsv", sep = "\t")
meta_cols <- read.csv("data/collections_digitised_meta.csv")



# glimpse(per_corpus)
# glimpse(corpus_1835)


######### UI #########
ui <- fluidPage(
  titlePanel(h3("Poetry corpus 1835-1840 (prototype)")),
  
    mainPanel(
      
      tabsetPanel(
        
        #### metadata periodicals ####
        tabPanel("Metadata: periodicals",
                 
                 fluidRow(h3("Poems published in periodicals"),
                          p("Total number of poems from periodicals included in the corpus: 1814"),
                          p("Sources listed: БдЧ, ЛПРИ, ЛГ, МН, ОЗ, ПРиВЕТ, Сев_пч, СО, Совр"),
                          reactableOutput("per_metadata"))
        ),
        
        #### metadata poetry books ####
        tabPanel("Metadata: poetry books",
                 
                 fluidRow(h3("Poems published in poetry books"),
                          p("Total number of poems from all types of poetry books: 2335"),
                          
                          radioButtons("choose_cols_meta",
                                      h5("Choose the metadata type"),
                                      choices = list("Book level metadata" = 1,
                                                     "Text level metadata" = 2),
                                      selected = 1),
                          reactableOutput("cols_metadata"))
          
        ),
        
        #### metrical features search ####
        tabPanel("Metrical features search",
                 
                 fluidRow(h3("Display texts by parameters"),
                          
                          column(3, 
                          checkboxGroupInput("choose_corpus", 
                                        h4("Choose corpus"), 
                                        choices = unique(corpus_1835$corpus),
                                        selected = unique(corpus_1835$corpus)
                                             ),
                          checkboxGroupInput("choose_year",
                                             h4("Choose year"),
                                             choices = unique(corpus_1835$year),
                                             selected = unique(corpus_1835$year)
                                             )),
                          
                          column(3,
                          checkboxGroupInput("choose_meter",
                                             h4("Select meter"),
                                             choices = list("All" = 1,
                                                            "Iamb" = 2,
                                                            "Trochee" = 3,
                                                            "Dactyl" = 4,
                                                            "Amphibrach" = 5,
                                                            "Anapest" = 6,
                                                            "Others"= 7),
                                             selected = 1),
                          
                          checkboxGroupInput("choose_feet",
                                             h4("Select number of feet"),
                                             choices = list("All" = 1,
                                                            "3" = 2,
                                                            "4" = 3,
                                                            "5" = 4,
                                                            "6" = 5),
                                             selected = 1)
                          
                          )),
                          
                 fluidRow(
                          reactableOutput("meter_search")
                          
                          )
        ),
        
        #### search by word ####
        tabPanel("Search by word",
                 
                 fluidRow(h3("Search words in corpus"),
                          p("Enter a word or a part of the word in the box (regexp allowed):"),
                          
                          textInput("input_search_word",
                                    label = "Search:", 
                                    value = "(Д|д)евиц"),
                          
                          reactableOutput("search_word"))
        ),
        
        #### search by poem id ####
        tabPanel("Search by poem id",
                 
                 textInput("text_id", 
                           h5("Enter text_id in the box: "),
                           value = "P_1733"),
                 
                 verbatimTextOutput("searched_text")
                 
                 ),
        
        
        #### random poem ####
        tabPanel("Random poem",
                 
                 h5('Random poem for today'),
                 actionButton("random_refresh", "Refresh"),
                 verbatimTextOutput("random_text")) # var to be used in server
                 
                 )
      
      
      
  )
)


######### Server ######### 

server <- function(input, output) {
  
  #### metadata periodicals ####
  output$per_metadata <- renderReactable({
    t <- per_corpus %>%
      mutate(author = ifelse(author == "", author_sign, author),
             source = paste0(PER_ID, ". ", year, ". ", vol, ". ", num, ".. C. ", pages), 
             source = str_remove_all(source, "\\. \\.|\\.\\.")) %>%
      select(
        text_ID, author, text_title, first_line, subtitle, year, source
      )
    
    
    reactable(t,
              searchable = F,
              paginationType = "simple",
              fullWidth = T,
              columns = list(
                author = colDef(
                  filterable = T
                ),
                text_title = colDef(
                  filterable = T
                ),
                first_line = colDef(
                  filterable = T
                ),
                subtitle = colDef(
                  filterable = T
                ),
                source = colDef(
                  filterable = T
                ),
                year = colDef(
                  filterable = T
                )
              )
              )
  })
  
  #### metadata poetry books ####
  output$cols_metadata <- renderReactable({
    
    if (input$choose_cols_meta == 2 ) {
    
      t <- cols_meta_united %>% 
        mutate(source = paste0(author, " ", title_book, ". ", city, ": ", publisher, ". ", 
                               year, ". С. ", pages)) %>% 
        select(text_id, col_id, group, author, title, first_line, subtitle, year, source)
      
      reactable(t,
                searchable = F,
                paginationType = "simple",
                fullWidth = T,
                columns = list(
                  
                  col_id = colDef(
                    filterable = T
                  ),
                  group = colDef(
                    filterable = T
                  ),
                  author = colDef(
                    filterable = T
                  ),
                  title = colDef(
                    filterable = T
                  ),
                  first_line = colDef(
                    filterable = T
                  ),
                  subtitle = colDef(
                    filterable = T
                  ),
                  source = colDef(
                    filterable = T
                  ),
                  year = colDef(
                    filterable = T
                  )
                )
      )
    
    } else {
      
      t <- meta_cols %>% 
        mutate(col_id = paste0("C_", id)) %>% 
        select(col_id, author, title, city, publisher, year, group)
      
      reactable(t,
                searchable = F,
                paginationType = "simple",
                fullWidth = T,
                columns = list(
                  col_id = colDef(
                    filterable = T
                  ),
                  group = colDef(
                    filterable = T
                  ),
                  author = colDef(
                    filterable = T
                  ),
                  title = colDef(
                    filterable = T
                  ),
                  city = colDef(
                    filterable = T
                  ),
                  publisher = colDef(
                    filterable = T
                  ),
                  year = colDef(
                    filterable = T
                  ),
                  group = colDef(
                    filterable = T
                  )
                )
      )
    }
  })
  
  
  #### metrical feat. search ####
  
  # reactive corpus
  corpus <- reactive({
    corpus_1835 %>% 
      filter(corpus %in% c(input$choose_corpus)) %>% 
      filter(year %in% c(input$choose_year)) %>% 
      select(text_id, year, author)
      })
  
  # search meters
  output$meter_search <- renderReactable({
    t <- corpus() 
    
    reactable(t)
  })
  
  #### search by word ####
  word_search <- reactive({
    corpus_1835 %>% 
      filter(str_detect(text_raw, input$input_search_word)) %>% 
      select(text_id, year, author, text_raw) %>% 
      mutate(text_raw = str_extract_all(text_raw, 
                                        paste0("\\n?.*?", input$input_search_word, ".*?\\n")))
  })
  
  output$search_word <- renderReactable({
    w <- word_search()
    
    reactable(w, 
              searchable = T,
              sortable = T,
              showSortable = T)
  })
  
  
  #### search by poem id ####
  output$searched_text <- renderText({
    
    p <- corpus_1835 %>% 
      filter(text_id == input$text_id)
    
    paste("Poem author:", p[1, 5], 
          "\nYear of publication: ", p[1, 12],
          "\nTitle:", p[1, 7],
          "\n\nText:\n\n", p[1, 13])
    
  })
  
  #### random poem ####
  r <- reactiveVal(sample(1:nrow(corpus_1835), 1))
  # random text tab
  output$random_text <- renderText({
    
    poem <- corpus_1835[r(),]
    
    paste0('\t', poem[1,7], '\n\n',
          poem[1,13], 
          '\n\nAuthor: ', poem[1,5], ', ', poem[1, 12])
  })
  
  
  # refresh random poem
  observeEvent(input$random_refresh, {
    r(sample(1:nrow(corpus_1835), 1))
  })
  
}


########### App launch ############
shinyApp(ui = ui, server = server)
  
