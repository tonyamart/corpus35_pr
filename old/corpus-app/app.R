library(shiny)
library(reactable)
library(tidyverse)


######### Load data #########
load("data/corpus_1835.Rda")
#load("data/periodicals_corpus.Rda")
#load("data/collections_corpus.Rda")

#authors <- read.delim("data/authors.tsv", sep = "\t")
meta_cols <- read.csv("data/collections_digitised_meta.csv")


glimpse(corpus_1835)

reg_feet <- c("2", "3", "4", "5", "6")

corpus_1835 <- corpus_1835 %>% 
  mutate(feet = ifelse(!feet %in% reg_feet, "other", feet))


# glimpse(per_corpus)
# glimpse(corpus_1835)

# group labels translation:
groups_tr <- tibble(
  subcorpus = c("col_lyr", "alm", "sep_lyr", "per"),
  group = c("Сборник ст.", "Альманах", "Отд. изд.", "Периодика")
)




######### UI #########
ui <- fluidPage(
  titlePanel(h3("Poetry corpus 1835-1840 (prototype)")),
  
    mainPanel(
      
      tabsetPanel(
        
        #### metadata poetry books ####
        tabPanel("Metadata",
                 
                 fluidRow(h3("Poems published in poetry books"),
                          p("Total number of poems from all types of sources: 4149"), 
                          p("Poems in poetry books: 2335"), 
                          p("Poems from periodicals: 1814"), 
                          
                          radioButtons("choose_cols_meta",
                                      h5("Choose the metadata type"),
                                      choices = list("Book level metadata" = 1,
                                                     "Text level metadata (all texts)" = 2,
                                                     "Texts from poetry books" = 3,
                                                     "Texts from periodicals" = 4),
                                      selected = 2),
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
                          sliderInput("choose_year", "Year", 
                                      min = 1835, max = 1840, 
                                      value = c(1835, 1840),
                                      sep = "")
                          ),
                          
                          column(3,
                          checkboxGroupInput("choose_meter",
                                             h4("Select meter"),
                                             choices = unique(corpus_1835$meter),
                                             selected = "Iamb")
                          ),
                          
                          column(3,
                          
                          checkboxGroupInput("choose_feet",
                                             h4("Select number of feet"),
                                             choices = unique(corpus_1835$feet),
                                             selected = unique(corpus_1835$feet))
                          
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
                 
                 ), 
      style = 'width: 1000px; height = 800px'
      
      
      
  )
)


######### Server ######### 

server <- function(input, output) {
  
  #### metadata poetry books ####
  output$cols_metadata <- renderReactable({
    
    if (input$choose_cols_meta == 2 ) {
    
      t <- corpus_1835 %>% 
        mutate(col_id = str_extract(text_id, "C_\\d+"),
               corpus = ifelse(corpus == "cols", "poetry books", "periodicals")) %>% 
        left_join(groups_tr, by = "subcorpus") %>% 
        select(corpus, col_id, text_id, author, text_title, first_line, subtitle, year, source, group)
      
      reactable(t,
                searchable = F,
                paginationType = "simple",
                fullWidth = T,
                columns = list(
                  corpus = colDef(
                    filterable = T
                  ),
                  col_id = colDef(
                    filterable = T,
                    maxWidth = 60
                  ),
                  text_id = colDef(
                    filterable = T
                  ),
                  group = colDef(
                    filterable = T
                  ),
                  author = colDef(
                    filterable = T
                  ),
                  text_title = colDef(
                    filterable = T
                  ),
                  first_line = colDef(
                    filterable = T,
                    minWidth = 150
                  ),
                  subtitle = colDef(
                    filterable = T
                  ),
                  source = colDef(
                    filterable = T,
                    minWidth = 200
                  ),
                  year = colDef(
                    filterable = T,
                    maxWidth = 50
                  )
                )
      )
    
    } else if (input$choose_cols_meta == 3) {
      
      t <- corpus_1835 %>% 
        mutate(col_id = str_extract(text_id, "C_\\d+")) %>% 
        left_join(groups_tr, by = "subcorpus") %>% 
        select(col_id, text_id, author, text_title, first_line, subtitle, year, source, group)
      
      reactable(t,
                searchable = F,
                paginationType = "simple",
                fullWidth = T,
                columns = list(
                  col_id = colDef(
                    filterable = T,
                    maxWidth = 60
                  ),
                  text_id = colDef(
                    filterable = T
                  ),
                  group = colDef(
                    filterable = T
                  ),
                  author = colDef(
                    filterable = T
                  ),
                  text_title = colDef(
                    filterable = T
                  ),
                  first_line = colDef(
                    filterable = T,
                    minWidth = 150
                  ),
                  subtitle = colDef(
                    filterable = T
                  ),
                  source = colDef(
                    filterable = T,
                    minWidth = 200
                  ),
                  year = colDef(
                    filterable = T,
                    maxWidth = 50
                  )
                )
      )
      
     } else if (input$choose_cols_meta == 3) {
      
      t <- corpus_1835 %>% 
        mutate(col_id = str_extract(text_id, "C_\\d+")) %>% 
        left_join(groups_tr, by = "subcorpus") %>% 
        select(col_id, text_id, author, text_title, first_line, subtitle, year, source, group)
      
      reactable(t,
                searchable = F,
                paginationType = "simple",
                fullWidth = T,
                columns = list(
                  col_id = colDef(
                    filterable = T,
                    maxWidth = 60
                  ),
                  text_id = colDef(
                    filterable = T
                  ),
                  group = colDef(
                    filterable = T
                  ),
                  author = colDef(
                    filterable = T
                  ),
                  text_title = colDef(
                    filterable = T
                  ),
                  first_line = colDef(
                    filterable = T,
                    minWidth = 150
                  ),
                  subtitle = colDef(
                    filterable = T
                  ),
                  source = colDef(
                    filterable = T,
                    minWidth = 200
                  ),
                  year = colDef(
                    filterable = T,
                    maxWidth = 50
                  )
                )
      )
    } else if (input$choose_cols_meta == 4) {
      
      t <- corpus_1835 %>% 
        filter(corpus == "per") %>% 
        select(text_id, author, text_title, first_line, subtitle, year, source)
      
      reactable(t,
                searchable = F,
                paginationType = "simple",
                fullWidth = T,
                columns = list(
                  text_id = colDef(
                    filterable = T,
                    maxWidth = 60
                  ),
                  author = colDef(
                    filterable = T
                  ),
                  text_title = colDef(
                    filterable = T
                  ),
                  first_line = colDef(
                    filterable = T,
                    minWidth = 150
                  ),
                  subtitle = colDef(
                    filterable = T
                  ),
                  source = colDef(
                    filterable = T,
                    minWidth = 200
                  ),
                  year = colDef(
                    filterable = T,
                    maxWidth = 50
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
                    filterable = T,
                    maxWidth = 60
                  ),
                  group = colDef(
                    filterable = T
                  ),
                  author = colDef(
                    filterable = T
                  ),
                  title = colDef(
                    filterable = T,
                    minWidth = 200
                  ),
                  city = colDef(
                    filterable = T,
                    maxWidth = 75
                  ),
                  publisher = colDef(
                    filterable = T
                  ),
                  year = colDef(
                    filterable = T,
                    maxWidth = 50
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
      filter(year > input$choose_year[1] & year < input$choose_year[2]) %>% 
      filter(meter %in% c(input$choose_meter)) %>% 
      filter(feet %in% c(input$choose_feet)) %>% 
      mutate(two_lines = str_extract(text_acc, "^.*?\\n.*?\\n")) %>% 
      select(text_id, year, author, text_title, two_lines, formula)
      })
  
  # search meters
  output$meter_search <- renderReactable({
    
    t <- corpus() 
    
    reactable(t, 
              sortable = T,
              showSortable = T,
              columns = list(
                two_lines = colDef(minWidth = 200)
      )
      )
  })
  
  #### search by word ####
  word_search <- reactive({
    corpus_1835 %>% 
      filter(str_detect(text_raw, input$input_search_word)) %>% 
      select(text_id, year, author, text_raw) %>% 
      mutate(text_raw = str_extract_all(text_raw, 
                                        paste0("\\n?.*?", 
                                               input$input_search_word, ".*?\\n")))
  })
  
  output$search_word <- renderReactable({
    w <- word_search()
    
    reactable(w, 
              searchable = T,
              sortable = T,
              showSortable = T,
              columns = list(
                text_id = colDef(maxWidth = 100),
                year = colDef(maxWidth = 75), 
                author = colDef(maxWidth = 150),
                text_raw = colDef(minWidth = 200)
              ))
  })
  
  
  #### search by poem id ####
  output$searched_text <- renderText({
    
    p <- corpus_1835 %>% 
      filter(text_id == input$text_id)
    
    paste("Poem author:", p[1, 5], 
          "\nYear of publication: ", p[1, 11],
          "\nTitle:", p[1, 7],
          "\nMeter:", p[1, 17],
          "\n\nText:\n\n", p[1, 12])
    
  })
  
  #### random poem ####
  r <- reactiveVal(sample(1:nrow(corpus_1835), 1))
  # random text tab
  output$random_text <- renderText({
    
    poem <- corpus_1835[r(),]
    
    paste0('\t', poem[1,7], '\n\n',
          poem[1,12], 
          '\n\nAuthor: ', poem[1,5], ', ', poem[1, 11])
  })
  
  
  # refresh random poem
  observeEvent(input$random_refresh, {
    r(sample(1:nrow(corpus_1835), 1))
  })
  
}


########### App launch ############
shinyApp(ui = ui, server = server)
  
