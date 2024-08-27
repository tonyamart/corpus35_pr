library(shiny)
library(reactable)
library(tidyverse)


#setwd("Documents/thesis1830s/corpus35_pr/scr/corpus_app/")

######### Load data #########
# corpus with texts
corpus_1835 <- readRDS("data/corpus_1835.Rds")

# glimpse(corpus_1835)

# books 1835-1840 metadata
meta_books <- read.csv("data/books_metadata.csv") %>% select(-X)

# glimpse(meta_books)

# full bibliography 1830-1850
biblio <- read.csv("data/biblio_1830-1850.csv") %>% select(-X)

biblio[is.na(biblio)] <- ""

# glimpse(biblio)









######### UI #########
ui <- fluidPage(
  titlePanel(h3("Poetry between 1835 and 1840: corpus & bibliography")),
  
    mainPanel(
      
      tabsetPanel(
        
        #### metadata poetry books ####
        tabPanel("Metadata",
                 
                 fluidRow(h3("Poems published in poetry books"),
                          p("Total number of poems from all types of sources: 4797"), 
                          p("Poems in poetry books: 2892"), 
                          p("Poems from periodicals: 1905"),
                          p('Abbreviations: БдЧ - Библиотека для чтения, ЛГ - Литературная газета, ЛПРИ - Литературные прибавления к "Русскому инвалиду", МН - Московский наблюдатель, ОЗ - Отечественные записки, ПРиВЕТ - Пантеон русского и всех европейских театров, Сев_пч - Северная пчела, СО - Сын отечества (СОиСА - Сын отечества и Северный архив), Совр - Современник'),
                          
                          radioButtons("choose_cols_meta",
                                      h5("Choose the metadata type"),
                                      choices = list(
                                        "Full bibliography: poetry books 1830-1850" = 0,
                                        "Corpus 1835-1840: Book level metadata" = 1,
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
                                             choices = unique(corpus_1835$feet_short),
                                             selected = unique(corpus_1835$feet_short))
                          
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
  
  #### metadata ####
  output$cols_metadata <- renderReactable({
    
    if (input$choose_cols_meta == 2 ) {
    
      t <- corpus_1835 %>% 
        mutate(col_id = str_extract(text_id, "C_\\d+"),
               corpus = ifelse(corpus == "col", "poetry books", "periodicals")) %>% 
        #left_join(groups_tr, by = "subcorpus") %>% 
        select(corpus, col_id, text_id, year, author_text, 
               text_title, first_line, text_subtitle, source_text, 
               author_sign)
      
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
                  year = colDef(
                    filterable = T,
                    maxWidth = 50
                  ),
                  
                  author_text = colDef(
                    filterable = T
                  ),
                  text_title = colDef(
                    filterable = T
                  ),
                  first_line = colDef(
                    filterable = T,
                    minWidth = 150
                  ),
                  text_subtitle = colDef(
                    filterable = T
                  ),
                  source_text = colDef(
                    filterable = T,
                    minWidth = 200
                  ),
                  author_sign = colDef(
                    filterable = T
                  )
                )
      )
    
    } else if (input$choose_cols_meta == 3) {
      
      t <- corpus_1835 %>% 
        mutate(col_id = str_extract(text_id, "C_\\d+")) %>% 
        filter(corpus == "col") %>% 
        select(col_id, text_id, year, author_text, 
               text_title, first_line, text_subtitle, source_text)
      
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
                  author_text = colDef(
                    filterable = T
                  ),
                  text_title = colDef(
                    filterable = T
                  ),
                  first_line = colDef(
                    filterable = T,
                    minWidth = 150
                  ),
                  text_subtitle = colDef(
                    filterable = T
                  ),
                  source_text = colDef(
                    filterable = T,
                    minWidth = 200
                  ),
                  year = colDef(
                    filterable = T,
                    maxWidth = 50
                  )
                )
      )
      
     } else if (input$choose_cols_meta == 1) {
      
      t <- meta_books 
      
      reactable(t,
                searchable = F,
                paginationType = "simple",
                fullWidth = T,
                columns = list(
                  source_id = colDef(
                    filterable = T,
                    maxWidth = 60
                  ),
                  author_name = colDef(
                    filterable = T
                  ),
                  book_title = colDef(
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
                  ),
                  n_texts = colDef(
                    sortable = T
                  ),
                  total_pages = colDef(
                    sortable = T
                  )
                )
      )
    } else if (input$choose_cols_meta == 4) {
      
      t <- corpus_1835 %>% 
        filter(corpus == "per") %>% 
        select(text_id, year, author_text, 
               text_title, first_line, text_subtitle, source_text, 
               author_sign)
      
      reactable(t,
                searchable = F,
                paginationType = "simple",
                fullWidth = T,
                columns = list(
                  text_id = colDef(
                    filterable = T,
                    maxWidth = 60
                  ),
                  author_text = colDef(
                    filterable = T
                  ),
                  text_title = colDef(
                    filterable = T
                  ),
                  first_line = colDef(
                    filterable = T,
                    minWidth = 150
                  ),
                  text_subtitle = colDef(
                    filterable = T
                  ),
                  source_text = colDef(
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
      # full biblio choice
      t <- biblio 
      
      reactable(t,
                searchable = F,
                paginationType = "simple",
                fullWidth = T,
                columns = list(
                  
                  id = colDef(
                    filterable = T
                  ),
                  
                  group_tr = colDef(
                    filterable = T,
                    maxWidth = 100
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
                  digital_copy_URL = colDef(
                    minWidth = 200
                  ),
                  year = colDef(
                    filterable = T,
                    maxWidth = 50
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
      filter(as.numeric(year) > input$choose_year[1] & as.numeric(year) < input$choose_year[2]) %>% 
      filter(meter %in% c(input$choose_meter)) %>% 
      filter(feet_short %in% c(input$choose_feet)) %>% 
      mutate(two_lines = str_extract(text_acc, "^.*?\\n.*?\\n")) %>% 
      select(text_id, year, author_text, text_title, two_lines, formula)
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
      filter(str_detect(text_cln, input$input_search_word)) %>% 
      select(text_id, year, author_text, text_cln) %>% 
      mutate(text_cln = str_extract_all(text_cln, 
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
                author_text = colDef(maxWidth = 150),
                text_cln = colDef(minWidth = 200)
              ))
  })
  
  
  #### search by poem id ####
  output$searched_text <- renderText({
    
    p <- corpus_1835 %>% 
      filter(text_id == input$text_id)
    
    paste("Poem author:", p[1, 4], 
          "\nYear of publication: ", p[1, 8],
          "\nTitle:", p[1, 5],
          "\nMeter:", p[1, 19],
          "\n\nText:\n\n", p[1, 14])
    
  })
  
  #### random poem ####
  r <- reactiveVal(sample(1:nrow(corpus_1835), 1))
  # random text tab
  output$random_text <- renderText({
    
    poem <- corpus_1835[r(),]
    
    paste0('\t', poem[1,5], '\n\n',
          poem[1,14], 
          '\n\nAuthor: ', poem[1,4], ', ', poem[1, 8],
          ' (text_id: ', poem[1,1], ").")
  })
  
  
  # refresh random poem
  observeEvent(input$random_refresh, {
    r(sample(1:nrow(corpus_1835), 1))
  })
  
}


########### App launch ############
shinyApp(ui = ui, server = server)
  
