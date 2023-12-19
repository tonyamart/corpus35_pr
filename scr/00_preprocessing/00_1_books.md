# 1 Collections - text separation & cleaning

This notebook shows how the long files (1 file = 1 book) were separated
into text files & the metadata for each text extracted.

``` r
library(tidyverse)
```

    ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ✔ dplyr     1.1.3     ✔ readr     2.1.4
    ✔ forcats   1.0.0     ✔ stringr   1.5.0
    ✔ ggplot2   3.4.4     ✔ tibble    3.2.1
    ✔ lubridate 1.9.3     ✔ tidyr     1.3.0
    ✔ purrr     1.0.2     
    ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ✖ dplyr::filter() masks stats::filter()
    ✖ dplyr::lag()    masks stats::lag()
    ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

# I. read files

``` r
# NB unzip files before reading
# read all long files (1 txt = 1 book)
lf <- list.files("../../data/corpus1835/collections/raw_texts/unseparated_cols/",
                 pattern = ".txt", 
                 full.names = TRUE)

head(lf)
```

    [1] "../../data/corpus1835/collections/raw_texts/unseparated_cols//101_1836.txt" 
    [2] "../../data/corpus1835/collections/raw_texts/unseparated_cols//1027_1835.txt"
    [3] "../../data/corpus1835/collections/raw_texts/unseparated_cols//107_1836.txt" 
    [4] "../../data/corpus1835/collections/raw_texts/unseparated_cols//111_1836.txt" 
    [5] "../../data/corpus1835/collections/raw_texts/unseparated_cols//112_1836.txt" 
    [6] "../../data/corpus1835/collections/raw_texts/unseparated_cols//113_1836.txt" 

``` r
# create a tibble  with full collections as rows based on the path
dat <- tibble(path = lf,
              text = sapply(lf, read_file))

str(dat)
```

    tibble [98 × 2] (S3: tbl_df/tbl/data.frame)
     $ path: chr [1:98] "../../data/corpus1835/collections/raw_texts/unseparated_cols//101_1836.txt" "../../data/corpus1835/collections/raw_texts/unseparated_cols//1027_1835.txt" "../../data/corpus1835/collections/raw_texts/unseparated_cols//107_1836.txt" "../../data/corpus1835/collections/raw_texts/unseparated_cols//111_1836.txt" ...
     $ text: Named chr [1:98] "<id: 101>\n<year: 1836>\n<descr: Кашкин. т.1: Включает в себя поэмы, трагедии, романсы и мелкие стихотворения >"| __truncated__ "<id: 1027>\n<year: 1835>\n<descr: Very short poetry collection >\n\n<title: Мотылек>\n<pages: NA>\nЛегкий, резв"| __truncated__ "<id: 107>\n<year: 1836>\n<descr:  Нравственные результаты и стихотворения Н. Лаврова: гладкие стихи в духе 1820"| __truncated__ "<id: 111>\n<year: 1836>\n<descr: Стихотворения Алексея Мейснера. М., 1836 :  >\n\n<title: Вместо предисловия. Р"| __truncated__ ...
      ..- attr(*, "names")= chr [1:98] "../../data/corpus1835/collections/raw_texts/unseparated_cols//101_1836.txt" "../../data/corpus1835/collections/raw_texts/unseparated_cols//1027_1835.txt" "../../data/corpus1835/collections/raw_texts/unseparated_cols//107_1836.txt" "../../data/corpus1835/collections/raw_texts/unseparated_cols//111_1836.txt" ...

## extract metadata

The book-level and text-level metadata is stored in \<tags\>.

The code below extracts and store the book’s metadata and remove if from
the text files.

``` r
corpus_books <- dat %>% 
  # extract metadata tags
  mutate(col_id = str_extract(text, "<id: .*?>"), 
         year = str_extract(text, "<year: .*?>"),
         descr = str_extract(text, "<descr: .*?>")) %>% 
  # clean metadata tags
  mutate(col_id = str_remove_all(col_id, "<|id:|>|\\s"),
         col_id = paste0("C_", col_id),
         year = str_remove_all(year, "<year:|>|\\s"),
         descr = str_remove_all(descr, "<descr:\\s?|>")) %>% 
  
  # remove metadata from the texts
  mutate(text = str_remove_all(text, 
                               "<id:.*?>|<year:.*?>|<descr:.*?>")) 

glimpse(corpus_books)
```

    Rows: 98
    Columns: 5
    $ path   <chr> "../../data/corpus1835/collections/raw_texts/unseparated_cols//…
    $ text   <chr> "\n\n\n\n<title: Романс>\n<pages: 349-350>\nВ власти-ль смертно…
    $ col_id <chr> "C_101", "C_1027", "C_107", "C_111", "C_112", "C_113", "C_114",…
    $ year   <chr> "1836", "1835", "1836", "1836", "1838", "1836", "1836", "1836",…
    $ descr  <chr> "Кашкин. т.1: Включает в себя поэмы, трагедии, романсы и мелкие…

``` r
rm(dat, lf)
```

Quick check if regex extracted data from all files (the output should be
empty)

``` r
corpus_books %>% 
  select(-text) %>% 
  distinct() %>% 
  filter(is.na(col_id))
```

    # A tibble: 0 × 4
    # ℹ 4 variables: path <chr>, col_id <chr>, year <chr>, descr <chr>

``` r
corpus_books %>% 
  select(-text) %>% 
  distinct() %>% 
  filter(is.na(year))
```

    # A tibble: 0 × 4
    # ℹ 4 variables: path <chr>, col_id <chr>, year <chr>, descr <chr>

## text separation

Turn books into separate texts based on the tags & retrieve text’s
metadata (titles, pages, etc.)

``` r
corpus_texts <- corpus_books %>% 
  # remove unnecessary spaces left after metadata removal
  mutate(text = str_remove(text, "^\n\n\n\n?\n?")) %>% 
  
  # add separator between each text id tag
  mutate(text = str_replace_all(text, "<title:", "<new_text><title:")) %>% 
  
  # separate each text to a row
  separate_rows(text, sep = "<new_text>") %>% 
  # remove empty rows
  filter(text != "") %>% 
  filter(str_detect(text, "<title:")) %>% 
  
  # extract text title and subtitle
  mutate(# extraction
         title = str_extract(text, "<title:.*?>"),
         subtitle = ifelse(str_detect(title, "\\|\\|sub:"),
                           str_extract(title, "\\|\\|sub:\\s?.*?>"),
                           ""),
         # cleaning
         title = str_remove_all(title, "\\|\\|sub:.*?>"),
         title = str_remove_all(title, "<title:\\s?|>"),
         subtitle = str_remove_all(subtitle, "\\|\\|sub:\\s?|>"),
         
         # clean text from tag
         text_cln = str_remove_all(text, "<title:.*?>")
         ) %>% 
  
  # extract pages of the text
  mutate(pages = str_extract(text, "<pages:.*?>"),
         pages = str_remove_all(pages, "<pages:|\\s|>"),
         text_cln = str_remove_all(text_cln, "<pages:.*?>")
         ) %>% 
  
  # extract notes & authors for almanack-type editions
  mutate( #extraction
    notes = str_extract(text, "<notes:.*?>"),
    author = ifelse(str_detect(notes, "\\|\\|author:"),
                         str_extract(notes, "\\|\\|author:.*?>"),
                         ""),
    
    # cleaning
    author = str_remove_all(author, "\\|\\|author:\\s?|>"),
    author = str_replace_all(author, "NA", "Unknown"),
    notes = str_remove_all(notes, "<notes:\\s?|\\|\\|author:.*?>|>"),
    # text cln from notes
    text_cln = str_remove_all(text_cln, "<notes:.*?>")
    ) %>% 
  
  # extract genre_title
  mutate(genre_title = str_extract(text, "<genre title:.*?>"),
         genre_title = str_remove_all(genre_title, "<genre title:\\s|>"),
         text_cln = str_remove_all(text_cln, "<genre title:.*?>")) %>% 
  
  # final text cleaning from \n\n
  mutate(text_cln = str_remove_all(text_cln, "^\\n\\n\\n?|\\n?\\n?\\n?$"),
         
         # some cleaning of "-" "'" for stress annotation
         text_cln = str_replace_all(text_cln, "(\\w)-(\\w)", "\\1 - \\2"),
         text_cln = str_remove_all(text_cln, "'")
         ) %>% 
  
  # add text_id
  group_by(col_id) %>% 
  mutate(text_id = paste0(col_id, "__", row_number())) %>% 
  ungroup() %>% 
  
  # create path for separate files:
  mutate(path_text = paste0(
    "../../data/corpus1835/collections/raw_texts/separated_cols_texts//", 
    text_id, ".txt" )) %>% 
  
  # create first line column
  mutate(first_line = ifelse(!str_detect(text_cln, "^<"),
           str_extract(text_cln, "^.*?\\n"),
           str_extract(text_cln, "\\n.*?\\n")
           ),
         first_line = str_remove(first_line, "^\\n|\\n$")) %>% 
  
  select(path, path_text, col_id, year, author, 
         text_id, title, subtitle, genre_title, first_line, pages, notes, 
         text_cln, text, descr)
  
glimpse(corpus_texts)
```

    Rows: 2,715
    Columns: 15
    $ path        <chr> "../../data/corpus1835/collections/raw_texts/unseparated_c…
    $ path_text   <chr> "../../data/corpus1835/collections/raw_texts/separated_col…
    $ col_id      <chr> "C_101", "C_101", "C_101", "C_101", "C_101", "C_101", "C_1…
    $ year        <chr> "1836", "1836", "1836", "1836", "1836", "1836", "1836", "1…
    $ author      <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
    $ text_id     <chr> "C_101__1", "C_101__2", "C_101__3", "C_101__4", "C_101__5"…
    $ title       <chr> "Романс", "Романс", "Романс", "Романс", "Романс", "Романс"…
    $ subtitle    <chr> "", "", "", "", "", "", "", "", "", "", "", "", "", "", ""…
    $ genre_title <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
    $ first_line  <chr> "В власти - ль смертного забыть,", "Время протекло приятно…
    $ pages       <chr> "349-350", "350-352", "352-353", "354-355", "355-357", "35…
    $ notes       <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
    $ text_cln    <chr> "В власти - ль смертного забыть,\nЧто ему всего дороже?\nП…
    $ text        <chr> "<title: Романс>\n<pages: 349-350>\nВ власти-ль смертного …
    $ descr       <chr> "Кашкин. т.1: Включает в себя поэмы, трагедии, романсы и м…

## write texts and metadata

``` r
#getwd()

# write texts in the folder `separated_cols_texts`
for (i in 1:nrow(corpus_texts)) {
  write_file(x = corpus_texts$text_cln[i], file = corpus_texts$path_text[i])
}

# write metadata for each text in the books
# str(dat_cln)
write.csv(corpus_texts %>% select(-text, -text_cln),
          file = "../../meta/working_files/texts_digitized.csv")
```

# II. metadata

## book-level metadata

``` r
glimpse(corpus_books)
```

    Rows: 98
    Columns: 5
    $ path   <chr> "../../data/corpus1835/collections/raw_texts/unseparated_cols//…
    $ text   <chr> "\n\n\n\n<title: Романс>\n<pages: 349-350>\nВ власти-ль смертно…
    $ col_id <chr> "C_101", "C_1027", "C_107", "C_111", "C_112", "C_113", "C_114",…
    $ year   <chr> "1836", "1835", "1836", "1836", "1838", "1836", "1836", "1836",…
    $ descr  <chr> "Кашкин. т.1: Включает в себя поэмы, трагедии, романсы и мелкие…

``` r
# write a shorter version of books metadata
books_meta <- corpus_books %>% 
  select(-text) %>%  # remove texts
  mutate(id = str_replace_all(path, "../../data/corpus1835/collections/raw_texts/unseparated_cols//(\\d+)_\\d+\\.txt",
                              "\\1"),
         id = as.numeric(id)) %>% 
  select(path, id, year, descr) %>% 
  rename(true_year = year)

glimpse(books_meta)
```

    Rows: 98
    Columns: 4
    $ path      <chr> "../../data/corpus1835/collections/raw_texts/unseparated_col…
    $ id        <dbl> 101, 1027, 107, 111, 112, 113, 114, 115, 117, 118, 121, 122,…
    $ true_year <chr> "1836", "1835", "1836", "1836", "1838", "1836", "1836", "183…
    $ descr     <chr> "Кашкин. т.1: Включает в себя поэмы, трагедии, романсы и мел…

Read full bibliography metadata for collections

``` r
# read metadata from the bibliography (1830-1850)
# 1:16 & 39:40 are the main bibliographical columns, discard the others
bibliography <- read.delim("../../meta/bibliography_1830-1850.tsv", sep = "\t")[c(1:16,  39:40)] 

glimpse(bibliography)
```

    Rows: 1,105
    Columns: 18
    $ id               <int> 1, 2, 4, 5, 6, 7, 8, 9, 10, 11, 13, 14, 15, 16, 17, 1…
    $ COL_ID           <chr> "", "", "", "", "", "", "", "", "", "", "", "", "", "…
    $ poetry_prose     <chr> "v", "v", "v", "v", "v", "v", "v", "v", "v", "v", "v"…
    $ book_type        <chr> "sep", "sep", "sep", "col", "sep", "col", "sep", "sep…
    $ genre            <chr> "nar", "lyr", "lyr", "lyr", "nar", "lyr", "nar", "nar…
    $ special_tag      <chr> "", "unknown", "unknown", "", "", "", "", "", "", "",…
    $ author_sign      <chr> "Артемьев П.", "", "", "Батюшков К.", "Александр Б.",…
    $ author           <chr> "Артемьев П. А.", "", "", "Батюшков К.Н.", "Боде А.К.…
    $ author_full_name <chr> "Артемьев Павел Алексеевич", "", "", "Батюшков Конста…
    $ title            <chr> "Страдалец: Повесть в стихах, взятая с истинного расс…
    $ city             <chr> "М.", "СПб.", "СПб.", "СПб.", "СПб.", "М.", "СПб.", "…
    $ publisher        <chr> "Селивановского", "Крайя", "Акад. наук", "И. Глазунов…
    $ year             <int> 1834, 1834, 1834, 1834, 1834, 1834, 1834, 1834, 1834,…
    $ pages            <int> 30, NA, NA, 270, 22, 79, 122, 35, 159, 128, 67, 71, 1…
    $ size_fold        <chr> "8", "", "", "8", "", "", "", "", "8", "12", "", "", …
    $ size_cm          <int> 20, NA, NA, 22, 24, 22, 19, 24, 23, 18, 17, 18, 20, 2…
    $ digital_copy     <chr> "", "", "", "ЧБ", "ЧБ", "", "РНБ", "", "", "", "", ""…
    $ digital_copy_URL <chr> "", "", "", "https://books.google.com/books?id=fVddAA…

Store separately women’s poetry books (digitised earlier & textst stored
separately)

``` r
fem_books <- bibliography %>% 
  filter(id %in% c(160, 185, 204, 209, 264, 269)) %>% 
  mutate(path = "", 
         true_year = year, 
         descr = "fem")

fem_books # female poetry book-level metadata
```

       id  COL_ID poetry_prose book_type genre special_tag    author_sign
    1 160  COL_54            v       col   lyr                Смирнова А.
    2 185  COL_60           pr       col   lyr              Аладьина Е.В.
    3 204  COL_76            v       col   lyr             Онисимова Д.А.
    4 209  COL_80            v       col   lyr               Теплова Н.С.
    5 264  COL_98            v       col   lyr      double   Кульман Е.Б.
    6 269 COL_104            v       col   lyr                Шахова Е.Н.
              author                       author_full_name
    1    Смирнова А.                          Смирнова Анна
    2  Аладьина Е.В.          Аладьина Елизавета Васильевна
    3 Анисимова Д.А. Анисимова (Онисимова) Домна Анисимовна
    4   Теплова Н.С.   Терюхина (Теплова) Надежда Сергеевна
    5   Кульман Е.Б.            Кульман Елизавета Борисовна
    6    Шахова Е.Н.             Шахова Елисавета Никитична
                                                                                                                            title
    1                                                             Собрание различных стихотворений. Соч. Анны Смирновой. Изд. 1-е
    2                                                                                                            Сочинения Е.В.А…
    3 Стихи бедной девицы, слепой дочери деревенского пономаря, сообщенные в Императорскую Российскую Академию, и от ней изданные
    4                                                                               Стихотворения Н. Тепловой. Изд. 2, умноженное
    5         Полное собрание Русских, Немецких и Италиянских Стихотворений Елисаветы Кульман. Кн. 1. Пиитические опыты. Изд. 2-е
    6                                                                                             Стихотворения Елизаветы Шаховой
      city  publisher year pages size_fold size_cm   digital_copy
    1 СПб.  Вингебера 1837   145         8      22            РГБ
    2 СПб.  Вингебера 1838    24                24            РГБ
    3 СПб. Акад. наук 1838    27                20               
    4   М.  Лазаревых 1838    78        16      15 РГБ (1-е изд.)
    5 СПб. Акад. наук 1839   207                24       нб_спбгу
    6 СПб. Акад. наук 1839   120         8      20            РГБ
                                                                                                                                          digital_copy_URL
    1                                                                                                                      https://dlib.rsl.ru/01003559653
    2                                                                                                                      https://dlib.rsl.ru/01003559619
    3                                                                                                                                                     
    4                                                                                                                      https://dlib.rsl.ru/01003558839
    5 http://old.library.spbu.ru/dcol/jsp/RcWebImageViewer.jsp?doc_id=ad3db2a1-c807-461d-a5a8-c6905de40f77/libspbgu/00000001/00001002&pg_seq=1&search_doc=
    6                                                                                                                      https://dlib.rsl.ru/01003560015
      path true_year descr
    1           1837   fem
    2           1838   fem
    3           1838   fem
    4           1838   fem
    5           1839   fem
    6           1839   fem

Attach female collections to general books metadata

``` r
books_dig <- bibliography %>% # from the full 1830-1850 meta
  
  inner_join(books_meta, by = "id") %>% # filter only books which were digitized
  
  rbind(fem_books) # add fem collections
```

Fix some conflicts: years

``` r
# check previously missing value:
books_dig %>% filter(id == 1027)
```

        id COL_ID poetry_prose book_type genre special_tag author_sign       author
    1 1027  COL_6            v       col   lyr              Кожухов Н. Кожухов Н.С.
                author_full_name                     title city publisher year
    1 Кожухов Николай Степанович Стихотворения Н. Кожухова   М.   Ун. тип 1833
      pages size_fold size_cm digital_copy digital_copy_URL
    1    24         8      20                              
                                                                             path
    1 ../../data/corpus1835/collections/raw_texts/unseparated_cols//1027_1835.txt
      true_year                         descr
    1      1835 Very short poetry collection 

``` r
# fix years:
books_dig %>% 
  filter(year != true_year) %>% 
  select(id, title, year, true_year) 
```

        id                                    title year true_year
    1  112 Кальян. Стихотворения Полежаева / Изд. 2 1836      1838
    2  301           Одесский Альманах, на 1840 год 1839      1840
    3 1027                Стихотворения Н. Кожухова 1833      1835

``` r
fy <- books_dig %>% 
  filter(year != true_year) %>% 
  select(id, title, year, true_year) %>% pull(id) # store ids for years to be fixed

# rewrite years
books_dig <- books_dig %>% 
  mutate(year = ifelse(id %in% fy, true_year, year)) #%>% # check line below
  #filter(id %in% fy) %>% select(id, title, year, true_year) 

# should be empty
books_dig %>% 
  filter(year < 1835)
```

     [1] id               COL_ID           poetry_prose     book_type       
     [5] genre            special_tag      author_sign      author          
     [9] author_full_name title            city             publisher       
    [13] year             pages            size_fold        size_cm         
    [17] digital_copy     digital_copy_URL path             true_year       
    [21] descr           
    <0 rows> (or 0-length row.names)

fix genres:

``` r
# two multigenre ids
books_dig %>% 
  filter(genre == "lyr; nar")  
```

       id  COL_ID poetry_prose book_type    genre special_tag    author_sign
    1  91                    v       col lyr; nar             Жуковский В.А.
    2 327 COL_190            v       col lyr; nar              Алексеев П.Ф.
              author            author_full_name
    1 Жуковский В.А. Жуковский Василий Андреевич
    2  Алексеев П.Ф.     Алексеев Петр Федорович
                                                                                                     title
    1 Стихотворения Василья Жуковского: В 8 ч. / Изд. 4-е. Т. 1. Орлеанская дева; лирические стихотворения
    2                                                                    Лирические стихотворения и сказки
       city              publisher year pages size_fold size_cm digital_copy
    1  СПб. Экс. загот. гос. бумаг 1835   292                22          РГБ
    2 Дерпт              Лаакманна 1840   180        12      17           UT
                            digital_copy_URL
    1        https://dlib.rsl.ru/01003819724
    2 https://dspace.ut.ee/handle/10062/1868
                                                                            path
    1  ../../data/corpus1835/collections/raw_texts/unseparated_cols//91_1835.txt
    2 ../../data/corpus1835/collections/raw_texts/unseparated_cols//327_1840.txt
      true_year
    1      1835
    2      1840
                                                                                                                                   descr
    1 Стихотворения В. Жуковского. Т. 1. Орлеанская дева. Лирические стихотворения. Изд. 4. СПб.: тип. Эксп. заготовл. гос. бумаг, 1835 
    2                                                   Лирические стихотворения и сказки П.Ф. Алексеева. Дерпт: тип. Г. Лакманна, 1840 

``` r
books_dig <- books_dig %>% 
  mutate(genre = ifelse(genre == "lyr; nar", "lyr", genre))

books_dig %>% 
  filter(genre == "lyr; nar")  # should be 0
```

     [1] id               COL_ID           poetry_prose     book_type       
     [5] genre            special_tag      author_sign      author          
     [9] author_full_name title            city             publisher       
    [13] year             pages            size_fold        size_cm         
    [17] digital_copy     digital_copy_URL path             true_year       
    [21] descr           
    <0 rows> (or 0-length row.names)

Fast check with viz

``` r
glimpse(books_dig) # 104 books
```

    Rows: 104
    Columns: 21
    $ id               <dbl> 68, 69, 70, 71, 73, 74, 75, 81, 83, 84, 85, 86, 90, 9…
    $ COL_ID           <chr> "COL_2", "COL_3", "COL_4", "COL_5", "COL_7", "", "COL…
    $ poetry_prose     <chr> "v", "pr", "v", "v", "v", "pr", "v", "v", "v", "v", "…
    $ book_type        <chr> "col", "col", "col", "col", "col", "col", "col", "col…
    $ genre            <chr> "lyr", "lyr", "lyr", "lyr", "lyr", "lyr", "lyr", "lyr…
    $ special_tag      <chr> "", "", "", "", "", "", "", "", "", "", "", "", "", "…
    $ author_sign      <chr> "Баратынский Е.", "Венгер Н.", "Деларю М.", "Зилов А.…
    $ author           <chr> "Баратынский Е.А.", "Венгер Н.", "Деларю М.Д.", "Зило…
    $ author_full_name <chr> "Баратынский Евгений Абрамович", "Венгер Николай", "Д…
    $ title            <chr> "Сочинения Е. Баратынского: В 2 ч.", "Сочинения Никол…
    $ city             <chr> "М.", "М.", "СПб.", "М.", "М.", "СПб.", "М.", "СПб.",…
    $ publisher        <chr> "Семена", "Селивановского", "Деп. внешней торговли", …
    $ year             <chr> "1835", "1835", "1835", "1835", "1835", "1835", "1835…
    $ pages            <int> 189, 68, 152, 115, 40, 238, 108, 189, 61, 70, 178, 7,…
    $ size_fold        <chr> "8", "12", "8", "12", "8", "12", "8", "", "8", "", ""…
    $ size_cm          <int> 22, 20, 20, 23, 22, 18, 22, 20, 22, 20, 20, 26, 20, 2…
    $ digital_copy     <chr> "РГБ", "", "нб_спбгу", "", "РГБ", "ЧБ", "", "ЧБ", "",…
    $ digital_copy_URL <chr> "https://dlib.rsl.ru/01003570129", "", "http://old.li…
    $ path             <chr> "../../data/corpus1835/collections/raw_texts/unsepara…
    $ true_year        <chr> "1835", "1835", "1835", "1835", "1835", "1835", "1835…
    $ descr            <chr> "Baratynsky poems of the 1820s and early 1830s", "Som…

``` r
# fast viz
books_dig %>% 
  mutate(group_abbr = ifelse(book_type != "alm", 
                             paste0(book_type, "_", genre),
                             book_type)) %>% 
  count(year, group_abbr) %>% 
  ggplot(aes(x = year, y = n, 
             group = group_abbr,
             fill = group_abbr)) + 
  geom_col() + 
  theme_bw() + 
  labs(title = "Number of poetry books of each type by year")
```

![](00_1_books.markdown_strict_files/figure-markdown_strict/unnamed-chunk-13-1.png)

#### cleaning

``` r
glimpse(books_dig)
```

    Rows: 104
    Columns: 21
    $ id               <dbl> 68, 69, 70, 71, 73, 74, 75, 81, 83, 84, 85, 86, 90, 9…
    $ COL_ID           <chr> "COL_2", "COL_3", "COL_4", "COL_5", "COL_7", "", "COL…
    $ poetry_prose     <chr> "v", "pr", "v", "v", "v", "pr", "v", "v", "v", "v", "…
    $ book_type        <chr> "col", "col", "col", "col", "col", "col", "col", "col…
    $ genre            <chr> "lyr", "lyr", "lyr", "lyr", "lyr", "lyr", "lyr", "lyr…
    $ special_tag      <chr> "", "", "", "", "", "", "", "", "", "", "", "", "", "…
    $ author_sign      <chr> "Баратынский Е.", "Венгер Н.", "Деларю М.", "Зилов А.…
    $ author           <chr> "Баратынский Е.А.", "Венгер Н.", "Деларю М.Д.", "Зило…
    $ author_full_name <chr> "Баратынский Евгений Абрамович", "Венгер Николай", "Д…
    $ title            <chr> "Сочинения Е. Баратынского: В 2 ч.", "Сочинения Никол…
    $ city             <chr> "М.", "М.", "СПб.", "М.", "М.", "СПб.", "М.", "СПб.",…
    $ publisher        <chr> "Семена", "Селивановского", "Деп. внешней торговли", …
    $ year             <chr> "1835", "1835", "1835", "1835", "1835", "1835", "1835…
    $ pages            <int> 189, 68, 152, 115, 40, 238, 108, 189, 61, 70, 178, 7,…
    $ size_fold        <chr> "8", "12", "8", "12", "8", "12", "8", "", "8", "", ""…
    $ size_cm          <int> 22, 20, 20, 23, 22, 18, 22, 20, 22, 20, 20, 26, 20, 2…
    $ digital_copy     <chr> "РГБ", "", "нб_спбгу", "", "РГБ", "ЧБ", "", "ЧБ", "",…
    $ digital_copy_URL <chr> "https://dlib.rsl.ru/01003570129", "", "http://old.li…
    $ path             <chr> "../../data/corpus1835/collections/raw_texts/unsepara…
    $ true_year        <chr> "1835", "1835", "1835", "1835", "1835", "1835", "1835…
    $ descr            <chr> "Baratynsky poems of the 1820s and early 1830s", "Som…

#### A_ID for books

Fill author_full_name & add A_ID keys

``` r
# fill empty cells by author's signs
books_dig <- books_dig %>% 
  mutate(
    author = ifelse(author == "", author_sign, author),
    author_full_name = ifelse(author_full_name == "", author, author_full_name)
    )
```

Add A-ID

``` r
# read author-A_ID key table
a <- read.delim("../../meta/working_files/authors_cols.csv", sep = ";") %>% select(-X)

a <- a %>% mutate(A_ID = paste0("A_", A_ID)) # fill the column
glimpse(a)
```

    Rows: 139
    Columns: 2
    $ author <chr> "Баратынский Е.А.", "Венгер Н.", "Деларю М.Д.", "Зилов А.М.", "…
    $ A_ID   <chr> "A_2", "A_3", "A_4", "A_5", "A_7", "A_8", "A_9", "A_10", "A_11"…

``` r
# attach to the books metadata table
books_dig <- books_dig %>% 
  left_join(a, by = "author")

books_dig[is.na(books_dig)] <- "" # change NA produced by left_join to an empty ("") cell (consistency)
```

Check table data consistency

``` r
length(unique(books_dig$id))
```

    [1] 104

``` r
# check authors
authors <- books_dig %>% 
  select(A_ID, author_sign, author, author_full_name)

head(authors)
```

      A_ID       author_sign           author              author_full_name
    1  A_2    Баратынский Е. Баратынский Е.А. Баратынский Евгений Абрамович
    2  A_3         Венгер Н.        Венгер Н.                Венгер Николай
    3  A_4         Деларю М.      Деларю М.Д.       Деларю Михаил Данилович
    4  A_5          Зилов А.       Зилов А.М.      Зилов Алексей Михайлович
    5  A_7        Кольцов А.     Кольцов А.В.    Кольцов Алексей Васильевич
    6  A_8 Лебедев В. (изд.)       Лебедев В.                    Лебедев В.

``` r
##### multiple choice columns check
table(books_dig$poetry_prose)
```


    pr  v 
    17 87 

``` r
table(books_dig$book_type)
```


    alm col sep 
     12  76  16 

``` r
table(books_dig$genre)
```


        lyr 
     12  92 

``` r
#####
# fill one missed city:
table(books_dig$city)
```


                 Вильно     Дерпт      Киев        М.    Одесса      СПб.   Харьков 
            1         1         1         1        33         3        61         2 
    Ярославль 
            1 

``` r
books_dig %>% 
  filter(city == "") %>% pull(descr)
```

    [1] "Застольная песни на праздник серебряной свадьбы. СПб., 1840 "

``` r
t <- books_dig %>% 
  filter(city == "") %>% pull(id)

books_dig <- books_dig %>% 
  mutate(city = ifelse(id == t, "СПб.", city))

table(books_dig$city)
```


       Вильно     Дерпт      Киев        М.    Одесса      СПб.   Харьков Ярославль 
            1         1         1        33         3        62         2         1 

``` r
#####
# books_dig %>% 
#   count(publisher, sort = F)

table(books_dig$year)
```


    1835 1836 1837 1838 1839 1840 
      19   16   17   21   16   15 

``` r
rm(fy, t, a, authors)
```

Remove unnecessary columns:

``` r
books_dig <- books_dig %>% 
  select(-special_tag, -true_year, -descr)
```

Write book-level metadata

``` r
write.csv(books_dig, file = "../../meta/books_digitized.csv")
```

Remove obsolete variables

``` r
rm(books_meta, corpus_books, bibliography)
```

## text-level metadata

Prepare separated texts from books

``` r
# this is essentialy the same metadata as written in the end of step I + columns with texts
glimpse(corpus_texts)
```

    Rows: 2,715
    Columns: 15
    $ path        <chr> "../../data/corpus1835/collections/raw_texts/unseparated_c…
    $ path_text   <chr> "../../data/corpus1835/collections/raw_texts/separated_col…
    $ col_id      <chr> "C_101", "C_101", "C_101", "C_101", "C_101", "C_101", "C_1…
    $ year        <chr> "1836", "1836", "1836", "1836", "1836", "1836", "1836", "1…
    $ author      <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
    $ text_id     <chr> "C_101__1", "C_101__2", "C_101__3", "C_101__4", "C_101__5"…
    $ title       <chr> "Романс", "Романс", "Романс", "Романс", "Романс", "Романс"…
    $ subtitle    <chr> "", "", "", "", "", "", "", "", "", "", "", "", "", "", ""…
    $ genre_title <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
    $ first_line  <chr> "В власти - ль смертного забыть,", "Время протекло приятно…
    $ pages       <chr> "349-350", "350-352", "352-353", "354-355", "355-357", "35…
    $ notes       <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
    $ text_cln    <chr> "В власти - ль смертного забыть,\nЧто ему всего дороже?\nП…
    $ text        <chr> "<title: Романс>\n<pages: 349-350>\nВ власти-ль смертного …
    $ descr       <chr> "Кашкин. т.1: Включает в себя поэмы, трагедии, романсы и м…

``` r
# some selecting & renaming for the furthure work
corpus_texts <- corpus_texts %>% 
  select(-text, -descr, -year) %>% 
  rename(author_text = author,
         text = text_cln)
```

### authors - texts

Many (~400) texts from almanacs has authors :

``` r
corpus_texts %>% 
  select(-text) %>% 
  filter(!is.na(author_text) & author_text != "") %>% 
  select(col_id, author_text, title) %>% head
```

    # A tibble: 6 × 3
      col_id author_text   title       
      <chr>  <chr>         <chr>       
    1 C_122  С. Лакроа     Мечта       
    2 C_122  Н.Л.          К ней       
    3 C_122  С. Локроа     Романс      
    4 C_122  Н.Л.          NA          
    5 C_122  С. Лакроа     Предчувствие
    6 C_122  Н. Петровский Завещание   

Add A_ID

``` r
# select distinct authors and assign A_IDs manually
alm_authors <- corpus_texts %>% 
  select(author_text) %>% 
  filter(!is.na(author_text) & author_text != "") %>% 
  distinct()

head(alm_authors)
# ~ 179 authors, to be supplied 
#write.csv(alm_authors, "../../meta/working_files/authors_alm.csv")
```

Read prepared .csv with authors supplied with A_ID

``` r
alm_authors <- read.csv("../../meta/working_files/authors_alm.csv", sep = ";") %>% 
  select(-X) %>% 
  rename(author_text = author)

glimpse(alm_authors) # look into author_sigh to A_ID connector
```

    Rows: 186
    Columns: 2
    $ author_text <chr> "", "С. Лакроа", "Н.Л.", "С. Локроа", "Н. Петровский", "Un…
    $ A_ID        <chr> "", "", "", "", "", "", "", "", "", "A-33", "", "", "", "A…

``` r
# join A_ID for authors from almanacs
corpus_texts <- corpus_texts %>% 
  left_join(alm_authors, by = "author_text") 

# rm(alm_authors)
```

## merge book & text level metadata

``` r
# overview
glimpse(corpus_texts)
```

    Rows: 2,715
    Columns: 13
    $ path        <chr> "../../data/corpus1835/collections/raw_texts/unseparated_c…
    $ path_text   <chr> "../../data/corpus1835/collections/raw_texts/separated_col…
    $ col_id      <chr> "C_101", "C_101", "C_101", "C_101", "C_101", "C_101", "C_1…
    $ author_text <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
    $ text_id     <chr> "C_101__1", "C_101__2", "C_101__3", "C_101__4", "C_101__5"…
    $ title       <chr> "Романс", "Романс", "Романс", "Романс", "Романс", "Романс"…
    $ subtitle    <chr> "", "", "", "", "", "", "", "", "", "", "", "", "", "", ""…
    $ genre_title <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
    $ first_line  <chr> "В власти - ль смертного забыть,", "Время протекло приятно…
    $ pages       <chr> "349-350", "350-352", "352-353", "354-355", "355-357", "35…
    $ notes       <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
    $ text        <chr> "В власти - ль смертного забыть,\nЧто ему всего дороже?\nП…
    $ A_ID        <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…

``` r
glimpse(books_dig)
```

    Rows: 104
    Columns: 19
    $ id               <dbl> 68, 69, 70, 71, 73, 74, 75, 81, 83, 84, 85, 86, 90, 9…
    $ COL_ID           <chr> "COL_2", "COL_3", "COL_4", "COL_5", "COL_7", "", "COL…
    $ poetry_prose     <chr> "v", "pr", "v", "v", "v", "pr", "v", "v", "v", "v", "…
    $ book_type        <chr> "col", "col", "col", "col", "col", "col", "col", "col…
    $ genre            <chr> "lyr", "lyr", "lyr", "lyr", "lyr", "lyr", "lyr", "lyr…
    $ author_sign      <chr> "Баратынский Е.", "Венгер Н.", "Деларю М.", "Зилов А.…
    $ author           <chr> "Баратынский Е.А.", "Венгер Н.", "Деларю М.Д.", "Зило…
    $ author_full_name <chr> "Баратынский Евгений Абрамович", "Венгер Николай", "Д…
    $ title            <chr> "Сочинения Е. Баратынского: В 2 ч.", "Сочинения Никол…
    $ city             <chr> "М.", "М.", "СПб.", "М.", "М.", "СПб.", "М.", "СПб.",…
    $ publisher        <chr> "Семена", "Селивановского", "Деп. внешней торговли", …
    $ year             <chr> "1835", "1835", "1835", "1835", "1835", "1835", "1835…
    $ pages            <chr> "189", "68", "152", "115", "40", "238", "108", "189",…
    $ size_fold        <chr> "8", "12", "8", "12", "8", "12", "8", "", "8", "", ""…
    $ size_cm          <chr> "22", "20", "20", "23", "22", "18", "22", "20", "22",…
    $ digital_copy     <chr> "РГБ", "", "нб_спбгу", "", "РГБ", "ЧБ", "", "ЧБ", "",…
    $ digital_copy_URL <chr> "https://dlib.rsl.ru/01003570129", "", "http://old.li…
    $ path             <chr> "../../data/corpus1835/collections/raw_texts/unsepara…
    $ A_ID             <chr> "A_2", "A_3", "A_4", "A_5", "A_7", "A_8", "A_9", "A_1…

``` r
# rename some columns to avoid conflicts
corpus_texts <- corpus_texts %>% 
  rename(A_ID_text = A_ID,
         text_title = title,
         text_pages = pages)
```

Merge the data

``` r
corpus_fullmeta <- corpus_texts %>% 
  # add id column for the merge
  rename(id = col_id) %>% select(-path) %>% 
  # merge
  left_join(books_dig %>% 
              mutate(id = paste0("C_", id)) %>% 
              select(-c(poetry_prose, size_fold, size_cm, 
                        digital_copy, digital_copy_URL)) , 
            by = "id") 

# change NA to ""
corpus_fullmeta[is.na(corpus_fullmeta)] <- ""

glimpse(corpus_fullmeta)
```

    Rows: 2,715
    Columns: 25
    $ path_text        <chr> "../../data/corpus1835/collections/raw_texts/separate…
    $ id               <chr> "C_101", "C_101", "C_101", "C_101", "C_101", "C_101",…
    $ author_text      <chr> "", "", "", "", "", "", "", "", "", "", "", "", "", "…
    $ text_id          <chr> "C_101__1", "C_101__2", "C_101__3", "C_101__4", "C_10…
    $ text_title       <chr> "Романс", "Романс", "Романс", "Романс", "Романс", "Ро…
    $ subtitle         <chr> "", "", "", "", "", "", "", "", "", "", "", "", "", "…
    $ genre_title      <chr> "", "", "", "", "", "", "", "", "", "", "", "", "", "…
    $ first_line       <chr> "В власти - ль смертного забыть,", "Время протекло пр…
    $ text_pages       <chr> "349-350", "350-352", "352-353", "354-355", "355-357"…
    $ notes            <chr> "", "", "", "", "", "", "", "", "", "", "", "", "", "…
    $ text             <chr> "В власти - ль смертного забыть,\nЧто ему всего дорож…
    $ A_ID_text        <chr> "", "", "", "", "", "", "", "", "", "", "", "", "", "…
    $ COL_ID           <chr> "COL_23", "COL_23", "COL_23", "COL_23", "COL_23", "CO…
    $ book_type        <chr> "col", "col", "col", "col", "col", "col", "col", "col…
    $ genre            <chr> "lyr", "lyr", "lyr", "lyr", "lyr", "lyr", "lyr", "lyr…
    $ author_sign      <chr> "Кашкин Д.", "Кашкин Д.", "Кашкин Д.", "Кашкин Д.", "…
    $ author           <chr> "Кашкин Д.Е.", "Кашкин Д.Е.", "Кашкин Д.Е.", "Кашкин …
    $ author_full_name <chr> "Кашкин Дмитрий Евгеньевич", "Кашкин Дмитрий Евгеньев…
    $ title            <chr> "Сочинения Дмитрия Кашкина: В 3 т. Т. 1", "Сочинения …
    $ city             <chr> "М.", "М.", "М.", "М.", "М.", "М.", "М.", "М.", "М.",…
    $ publisher        <chr> "Степанова", "Степанова", "Степанова", "Степанова", "…
    $ year             <chr> "1836", "1836", "1836", "1836", "1836", "1836", "1836…
    $ pages            <chr> "468", "468", "468", "468", "468", "468", "468", "468…
    $ path             <chr> "../../data/corpus1835/collections/raw_texts/unsepara…
    $ A_ID             <chr> "A_22", "A_22", "A_22", "A_22", "A_22", "A_22", "A_22…

### clean authors

``` r
table(corpus_fullmeta$book_type)
```


     alm  col  sep 
     348 2349   18 

``` r
# replacements
corpus_fullmeta <- corpus_fullmeta %>% 
  
  # replace empty author texts for author names from books meta (except for almanacs)
  mutate(author_text = ifelse(book_type != "alm" & author_text == "", 
                              author, 
                              author_text),
         # same replacement for A_ID
         A_ID = ifelse(book_type != "alm" & A_ID_text == "", A_ID, A_ID_text)
         ) %>% 
  # replace "Unknown" to empty cell
  mutate(author_text = ifelse(author_text == "Unknown", "", author_text)) %>% 
  
  select(-A_ID_text) %>% 
  rename(author_book = author)

glimpse(corpus_fullmeta)
```

    Rows: 2,715
    Columns: 24
    $ path_text        <chr> "../../data/corpus1835/collections/raw_texts/separate…
    $ id               <chr> "C_101", "C_101", "C_101", "C_101", "C_101", "C_101",…
    $ author_text      <chr> "Кашкин Д.Е.", "Кашкин Д.Е.", "Кашкин Д.Е.", "Кашкин …
    $ text_id          <chr> "C_101__1", "C_101__2", "C_101__3", "C_101__4", "C_10…
    $ text_title       <chr> "Романс", "Романс", "Романс", "Романс", "Романс", "Ро…
    $ subtitle         <chr> "", "", "", "", "", "", "", "", "", "", "", "", "", "…
    $ genre_title      <chr> "", "", "", "", "", "", "", "", "", "", "", "", "", "…
    $ first_line       <chr> "В власти - ль смертного забыть,", "Время протекло пр…
    $ text_pages       <chr> "349-350", "350-352", "352-353", "354-355", "355-357"…
    $ notes            <chr> "", "", "", "", "", "", "", "", "", "", "", "", "", "…
    $ text             <chr> "В власти - ль смертного забыть,\nЧто ему всего дорож…
    $ COL_ID           <chr> "COL_23", "COL_23", "COL_23", "COL_23", "COL_23", "CO…
    $ book_type        <chr> "col", "col", "col", "col", "col", "col", "col", "col…
    $ genre            <chr> "lyr", "lyr", "lyr", "lyr", "lyr", "lyr", "lyr", "lyr…
    $ author_sign      <chr> "Кашкин Д.", "Кашкин Д.", "Кашкин Д.", "Кашкин Д.", "…
    $ author_book      <chr> "Кашкин Д.Е.", "Кашкин Д.Е.", "Кашкин Д.Е.", "Кашкин …
    $ author_full_name <chr> "Кашкин Дмитрий Евгеньевич", "Кашкин Дмитрий Евгеньев…
    $ title            <chr> "Сочинения Дмитрия Кашкина: В 3 т. Т. 1", "Сочинения …
    $ city             <chr> "М.", "М.", "М.", "М.", "М.", "М.", "М.", "М.", "М.",…
    $ publisher        <chr> "Степанова", "Степанова", "Степанова", "Степанова", "…
    $ year             <chr> "1836", "1836", "1836", "1836", "1836", "1836", "1836…
    $ pages            <chr> "468", "468", "468", "468", "468", "468", "468", "468…
    $ path             <chr> "../../data/corpus1835/collections/raw_texts/unsepara…
    $ A_ID             <chr> "A_22", "A_22", "A_22", "A_22", "A_22", "A_22", "A_22…

Quick check for authors form almanacs

``` r
corpus_fullmeta %>% 
  filter(book_type == "alm") %>% 
  select(A_ID, author_book, author_text) %>% distinct
```

    # A tibble: 186 × 3
       A_ID    author_book     author_text  
       <chr>   <chr>           <chr>        
     1 ""      ""              ""           
     2 "A-271" "Крыловский В." "А. Грен"    
     3 ""      "Крыловский В." "А.-ий"      
     4 "A-42"  "Крыловский В." "Н. Грен"    
     5 ""      "Крыловский В." "Н.С."       
     6 ""      "Крыловский В." "Н.С. "      
     7 ""      "Крыловский В." "А. Левицкий"
     8 ""      "Крыловский В." "А. Лоренц"  
     9 ""      "Крыловский В." "А. В-в"     
    10 ""      "Крыловский В." "А.П."       
    # ℹ 176 more rows

``` r
corpus_fullmeta %>% 
  filter(book_type == "sep") %>% 
  select(A_ID, author_book, author_text) %>% distinct
```

    # A tibble: 13 × 3
       A_ID    author_book                   author_text                  
       <chr>   <chr>                         <chr>                        
     1 "A_298" "Сиянов П.Г."                 "Сиянов П.Г."                
     2 "A_295" "Огинский А.Г."               "Огинский А.Г."              
     3 "A_67"  "Доброхотов Ф."               "Доброхотов Ф."              
     4 "A_86"  "Барановский В."              "Барановский В."             
     5 "A_55"  "Мысовский-Светогорский С.Д." "Мысовский-Светогорский С.Д."
     6 ""      ""                            ""                           
     7 "A_87"  "Поднебесный М.М."            "Поднебесный М.М."           
     8 "A_105" "Харинский Х."                "Харинский Х."               
     9 "A_106" "Шаховской А.А."              "Шаховской А.А."             
    10 "A_111" "Данков И.И."                 "Данков И.И."                
    11 "A_127" "Ободовский П.Г."             "Ободовский П.Г."            
    12 "A_148" "Малышев Г.Г."                "Малышев Г.Г."               
    13 "A_16"  "Топчибашев М. Д."            "Топчибашев М. Д."           

### clean genres

Merge columns “genre title” and “subtitle” to one

``` r
corpus_fullmeta <- corpus_fullmeta %>% 
  # in case if both exist
  mutate(
         subtitle = ifelse(genre_title != "" & subtitle != "",
                           paste0(subtitle, "; ", genre_title), # paste with ;
                           subtitle
                           ),
  # in case subtitle is empty and genre title is not
          subtitle = ifelse(genre_title != "" & subtitle == "", 
                           genre_title, # paste genre title as subtitle
                           subtitle)) 
```

Check if genre_title can be removed

``` r
corpus_fullmeta %>% 
  select(subtitle, genre_title) %>% 
  filter(genre_title != "") %>% 
  distinct()
```

    # A tibble: 14 × 2
       subtitle                                  genre_title                        
       <chr>                                     <chr>                              
     1 Мелкие стихотворения                      Мелкие стихотворения               
     2 Эпиграммы                                 Эпиграммы                          
     3 Эпитафии                                  Эпитафии                           
     4 Разные стихотворения                      Разные стихотворения               
     5 Песни                                     Песни                              
     6 Шутки                                     Шутки                              
     7 Песнословия во время литургии             Песнословия во время литургии      
     8 Ирмосы на страстную субботу               Ирмосы на страстную субботу        
     9 Песнословия из канона св. пасхи           Песнословия из канона св. пасхи    
    10 Эпиграмы                                  Эпиграмы                           
    11 Три сонета. Посвящ. А.С. Левашовой        Три сонета. Посвящ. А.С. Левашовой 
    12 Эротические станцы Индийского Поэта Амару Эротические станцы Индийского Поэт…
    13 Мои ночи                                  Мои ночи                           
    14 Посвящено В... И... С...; Фантазия        Фантазия                           

Remove genre_title column, rename “pages” & “genre” columns & store as
corpus_texts

``` r
corpus_texts <- corpus_fullmeta %>% 
  select(-genre_title) %>% 
  rename(book_pages = pages,
         book_genre = genre)
```

``` r
glimpse(corpus_texts)
```

    Rows: 2,715
    Columns: 23
    $ path_text        <chr> "../../data/corpus1835/collections/raw_texts/separate…
    $ id               <chr> "C_101", "C_101", "C_101", "C_101", "C_101", "C_101",…
    $ author_text      <chr> "Кашкин Д.Е.", "Кашкин Д.Е.", "Кашкин Д.Е.", "Кашкин …
    $ text_id          <chr> "C_101__1", "C_101__2", "C_101__3", "C_101__4", "C_10…
    $ text_title       <chr> "Романс", "Романс", "Романс", "Романс", "Романс", "Ро…
    $ subtitle         <chr> "", "", "", "", "", "", "", "", "", "", "", "", "", "…
    $ first_line       <chr> "В власти - ль смертного забыть,", "Время протекло пр…
    $ text_pages       <chr> "349-350", "350-352", "352-353", "354-355", "355-357"…
    $ notes            <chr> "", "", "", "", "", "", "", "", "", "", "", "", "", "…
    $ text             <chr> "В власти - ль смертного забыть,\nЧто ему всего дорож…
    $ COL_ID           <chr> "COL_23", "COL_23", "COL_23", "COL_23", "COL_23", "CO…
    $ book_type        <chr> "col", "col", "col", "col", "col", "col", "col", "col…
    $ book_genre       <chr> "lyr", "lyr", "lyr", "lyr", "lyr", "lyr", "lyr", "lyr…
    $ author_sign      <chr> "Кашкин Д.", "Кашкин Д.", "Кашкин Д.", "Кашкин Д.", "…
    $ author_book      <chr> "Кашкин Д.Е.", "Кашкин Д.Е.", "Кашкин Д.Е.", "Кашкин …
    $ author_full_name <chr> "Кашкин Дмитрий Евгеньевич", "Кашкин Дмитрий Евгеньев…
    $ title            <chr> "Сочинения Дмитрия Кашкина: В 3 т. Т. 1", "Сочинения …
    $ city             <chr> "М.", "М.", "М.", "М.", "М.", "М.", "М.", "М.", "М.",…
    $ publisher        <chr> "Степанова", "Степанова", "Степанова", "Степанова", "…
    $ year             <chr> "1836", "1836", "1836", "1836", "1836", "1836", "1836…
    $ book_pages       <chr> "468", "468", "468", "468", "468", "468", "468", "468…
    $ path             <chr> "../../data/corpus1835/collections/raw_texts/unsepara…
    $ A_ID             <chr> "A_22", "A_22", "A_22", "A_22", "A_22", "A_22", "A_22…

``` r
rm(corpus_fullmeta)
```

-   corpus fullmeta is ready to be merged with fem text meta

-   save all as books_texts_meta (without texts)

-   save the full df with texts as Rda

-   rename this file as 01_books_preprocessing

-   create separate file for periodicals

-   store periodicals meta separately

-   do acc & lemmatisation for everything

-   write files !

# III. female texts & meta from a different source

Texts are in a separate folder as .txt, metadata is in the variable
`fem_books`

Read the text files:

``` r
f <- list.files(path = "../../data/corpus1835/collections/raw_texts/fem_cols/",
                pattern = ".txt",
                full.names = T,
                recursive = T)

fem_texts <- tibble(path = f,
             text = sapply(f, read_file),
             ) %>% 
  mutate(text_id = str_extract(path, "C_\\d+__\\d+"),
         col_id = str_remove_all(text_id, "__\\d+"),
         # first_line = str_extract(text, "^.*?\n"),
         # first_line = str_remove(first_line, "[[:space:]]?\n"),
         
         # create a new path to write files together with the rest of collections
         path_text = str_replace(path, "fem_cols//\\d+_\\d+_\\w+/", "separated_cols_texts//"))

str(fem_texts)
```

    tibble [180 × 5] (S3: tbl_df/tbl/data.frame)
     $ path     : chr [1:180] "../../data/corpus1835/collections/raw_texts/fem_cols//160_1837_Smirnova/C_160__1.txt" "../../data/corpus1835/collections/raw_texts/fem_cols//160_1837_Smirnova/C_160__10.txt" "../../data/corpus1835/collections/raw_texts/fem_cols//160_1837_Smirnova/C_160__11.txt" "../../data/corpus1835/collections/raw_texts/fem_cols//160_1837_Smirnova/C_160__12.txt" ...
     $ text     : Named chr [1:180] "Цветущая Хлоя прелестна была, \nВ счастливы минуты, как роза цвела, \nРумянцы играли подобно зефиру,\nИ всех ут"| __truncated__ "В Аркадие<sic> стране все дышит красотою, \nБлаженство, прелести назначены судьбою! \nПриятно видеть их меж сне"| __truncated__ "О ты! гордящийся величеством Кавказ, \nКуда лечу с мечтами сновиденья?\nНа твой покрытый мразами Парнас, \nИз м"| __truncated__ "Ты молод, друг, и легковерен; \nУчись рассматривать умом;\nНе будь в любезности безверен, \nТы должен видеть см"| __truncated__ ...
      ..- attr(*, "names")= chr [1:180] "../../data/corpus1835/collections/raw_texts/fem_cols//160_1837_Smirnova/C_160__1.txt" "../../data/corpus1835/collections/raw_texts/fem_cols//160_1837_Smirnova/C_160__10.txt" "../../data/corpus1835/collections/raw_texts/fem_cols//160_1837_Smirnova/C_160__11.txt" "../../data/corpus1835/collections/raw_texts/fem_cols//160_1837_Smirnova/C_160__12.txt" ...
     $ text_id  : chr [1:180] "C_160__1" "C_160__10" "C_160__11" "C_160__12" ...
     $ col_id   : chr [1:180] "C_160" "C_160" "C_160" "C_160" ...
     $ path_text: chr [1:180] "../../data/corpus1835/collections/raw_texts/separated_cols_texts//C_160__1.txt" "../../data/corpus1835/collections/raw_texts/separated_cols_texts//C_160__10.txt" "../../data/corpus1835/collections/raw_texts/separated_cols_texts//C_160__11.txt" "../../data/corpus1835/collections/raw_texts/separated_cols_texts//C_160__12.txt" ...

### write fem texts

Write female texts in the new folder (`separated_cols_texts`)

``` r
for (i in 1:nrow(fem_texts)) {
  write_file(x = fem_texts$text[i], file = fem_texts$path_text[i])
}
```

### read fem metadata

``` r
# read data
fem_meta <- read.csv("../../meta/working_files/meta_fem_collections.csv", sep = ";") 
fem_texts <- fem_meta %>% 
  
  # create new columns
  mutate(author_text = author_sign,
         notes = noes) %>% 
  
  # merge with texts metadata
  left_join(fem_texts, by = "text_id") %>% 
  
  # renaming
  rename(text_title = title, 
         text_pages = pages, 
         id = col_id) %>% 
  
  # rearrange
  select(path, path_text, id, author_text, text_id,
         text_title, subtitle, first_line, text_pages, 
         notes, text, A_ID)

glimpse(fem_texts)
```

    Rows: 180
    Columns: 12
    $ path        <chr> "../../data/corpus1835/collections/raw_texts/fem_cols//160…
    $ path_text   <chr> "../../data/corpus1835/collections/raw_texts/separated_col…
    $ id          <chr> "C_160", "C_160", "C_160", "C_160", "C_160", "C_160", "C_1…
    $ author_text <chr> "Анна Смирнова", "Анна Смирнова", "Анна Смирнова", "Анна С…
    $ text_id     <chr> "C_160__1", "C_160__2", "C_160__3", "C_160__4", "C_160__5"…
    $ text_title  <chr> "Хлоя", "Жалоба нимфам", "Триолет тебе", "Сочинителю поэмы…
    $ subtitle    <chr> "Идиллия", "Два сонета", "", "", "", "", "", "", "Рондо", …
    $ first_line  <chr> "Цветущая Хлоя прелестна была", "Не розы, а терник всю жиз…
    $ text_pages  <chr> "7-8", "8-10", "10", "10-12", "12", "12-14", "14-15", "15-…
    $ notes       <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
    $ text        <chr> "Цветущая Хлоя прелестна была, \nВ счастливы минуты, как р…
    $ A_ID        <chr> "А-48", "А-48", "А-48", "А-48", "А-48", "А-48", "А-48", "А…

### join fem metadata & texts to the corpus

``` r
# prepare fem_books (book level metadta) for join

fem_books <- fem_books %>% 
  mutate(id = paste0("C_", id)) %>% 
  rename(book_genre = genre, 
         author_book = author,
         book_pages = pages
         ) %>% 
  select(id, COL_ID, 
         book_type, book_genre, 
         author_sign, author_book, author_full_name, 
         title, city, publisher, year, 
         book_pages) %>% glimpse
```

    Rows: 6
    Columns: 12
    $ id               <chr> "C_160", "C_185", "C_204", "C_209", "C_264", "C_269"
    $ COL_ID           <chr> "COL_54", "COL_60", "COL_76", "COL_80", "COL_98", "CO…
    $ book_type        <chr> "col", "col", "col", "col", "col", "col"
    $ book_genre       <chr> "lyr", "lyr", "lyr", "lyr", "lyr", "lyr"
    $ author_sign      <chr> "Смирнова А.", "Аладьина Е.В.", "Онисимова Д.А.", "Те…
    $ author_book      <chr> "Смирнова А.", "Аладьина Е.В.", "Анисимова Д.А.", "Те…
    $ author_full_name <chr> "Смирнова Анна", "Аладьина Елизавета Васильевна", "Ан…
    $ title            <chr> "Собрание различных стихотворений. Соч. Анны Смирново…
    $ city             <chr> "СПб.", "СПб.", "СПб.", "М.", "СПб.", "СПб."
    $ publisher        <chr> "Вингебера", "Вингебера", "Акад. наук", "Лазаревых", …
    $ year             <int> 1837, 1838, 1838, 1838, 1839, 1839
    $ book_pages       <int> 145, 24, 27, 78, 207, 120

``` r
glimpse(fem_books)
```

    Rows: 6
    Columns: 12
    $ id               <chr> "C_160", "C_185", "C_204", "C_209", "C_264", "C_269"
    $ COL_ID           <chr> "COL_54", "COL_60", "COL_76", "COL_80", "COL_98", "CO…
    $ book_type        <chr> "col", "col", "col", "col", "col", "col"
    $ book_genre       <chr> "lyr", "lyr", "lyr", "lyr", "lyr", "lyr"
    $ author_sign      <chr> "Смирнова А.", "Аладьина Е.В.", "Онисимова Д.А.", "Те…
    $ author_book      <chr> "Смирнова А.", "Аладьина Е.В.", "Анисимова Д.А.", "Те…
    $ author_full_name <chr> "Смирнова Анна", "Аладьина Елизавета Васильевна", "Ан…
    $ title            <chr> "Собрание различных стихотворений. Соч. Анны Смирново…
    $ city             <chr> "СПб.", "СПб.", "СПб.", "М.", "СПб.", "СПб."
    $ publisher        <chr> "Вингебера", "Вингебера", "Акад. наук", "Лазаревых", …
    $ year             <int> 1837, 1838, 1838, 1838, 1839, 1839
    $ book_pages       <int> 145, 24, 27, 78, 207, 120

Join fem book-level metadata & text-lvl & attach to corpus_texts

``` r
colnames(corpus_texts)
```

     [1] "path_text"        "id"               "author_text"      "text_id"         
     [5] "text_title"       "subtitle"         "first_line"       "text_pages"      
     [9] "notes"            "text"             "COL_ID"           "book_type"       
    [13] "book_genre"       "author_sign"      "author_book"      "author_full_name"
    [17] "title"            "city"             "publisher"        "year"            
    [21] "book_pages"       "path"             "A_ID"            

``` r
fem_join <- fem_texts %>% 
  left_join(fem_books, by = "id") %>% 
  
  # reorder for similarity with corpus_texts
  select(path_text, id, 
         # text-level data
         author_text, 
         text_id, text_title, subtitle, first_line, 
         text_pages, notes, 
         text,
         # book_level data
         COL_ID, book_type, book_genre, 
         author_sign, author_book, author_full_name,
         title, city, publisher, year, 
         book_pages, path, A_ID)


# rm na to ""
fem_join[is.na(fem_join)] <- ""

glimpse(fem_join)
```

    Rows: 180
    Columns: 23
    $ path_text        <chr> "../../data/corpus1835/collections/raw_texts/separate…
    $ id               <chr> "C_160", "C_160", "C_160", "C_160", "C_160", "C_160",…
    $ author_text      <chr> "Анна Смирнова", "Анна Смирнова", "Анна Смирнова", "А…
    $ text_id          <chr> "C_160__1", "C_160__2", "C_160__3", "C_160__4", "C_16…
    $ text_title       <chr> "Хлоя", "Жалоба нимфам", "Триолет тебе", "Сочинителю …
    $ subtitle         <chr> "Идиллия", "Два сонета", "", "", "", "", "", "", "Рон…
    $ first_line       <chr> "Цветущая Хлоя прелестна была", "Не розы, а терник вс…
    $ text_pages       <chr> "7-8", "8-10", "10", "10-12", "12", "12-14", "14-15",…
    $ notes            <chr> "", "", "", "", "", "", "", "", "", "", "", "", "", "…
    $ text             <chr> "Цветущая Хлоя прелестна была, \nВ счастливы минуты, …
    $ COL_ID           <chr> "COL_54", "COL_54", "COL_54", "COL_54", "COL_54", "CO…
    $ book_type        <chr> "col", "col", "col", "col", "col", "col", "col", "col…
    $ book_genre       <chr> "lyr", "lyr", "lyr", "lyr", "lyr", "lyr", "lyr", "lyr…
    $ author_sign      <chr> "Смирнова А.", "Смирнова А.", "Смирнова А.", "Смирнов…
    $ author_book      <chr> "Смирнова А.", "Смирнова А.", "Смирнова А.", "Смирнов…
    $ author_full_name <chr> "Смирнова Анна", "Смирнова Анна", "Смирнова Анна", "С…
    $ title            <chr> "Собрание различных стихотворений. Соч. Анны Смирново…
    $ city             <chr> "СПб.", "СПб.", "СПб.", "СПб.", "СПб.", "СПб.", "СПб.…
    $ publisher        <chr> "Вингебера", "Вингебера", "Вингебера", "Вингебера", "…
    $ year             <int> 1837, 1837, 1837, 1837, 1837, 1837, 1837, 1837, 1837,…
    $ book_pages       <int> 145, 145, 145, 145, 145, 145, 145, 145, 145, 145, 145…
    $ path             <chr> "../../data/corpus1835/collections/raw_texts/fem_cols…
    $ A_ID             <chr> "А-48", "А-48", "А-48", "А-48", "А-48", "А-48", "А-48…

### final merge

``` r
# final colnames check
colnames(corpus_texts) == colnames(fem_join)
```

     [1] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    [16] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE

``` r
# join
books_corpus <- rbind(corpus_texts, fem_join)
```

# IV. final cln & save

### count N of poems in books

``` r
n_texts <- books_corpus %>% 
  select(id) %>% # leave only book id
  count(id) %>%  # rename for join
  rename(n_texts = n)

head(n_texts)
```

    # A tibble: 6 × 2
      id     n_texts
      <chr>    <int>
    1 C_101       47
    2 C_1027       5
    3 C_107       17
    4 C_111      106
    5 C_112       16
    6 C_113       39

Attach n poems

``` r
books_corpus <- books_corpus %>% 
  left_join(n_texts, by = "id")

rm(n_texts)
```

### rename & reorder

``` r
glimpse(books_corpus)
```

    Rows: 2,895
    Columns: 24
    $ path_text        <chr> "../../data/corpus1835/collections/raw_texts/separate…
    $ id               <chr> "C_101", "C_101", "C_101", "C_101", "C_101", "C_101",…
    $ author_text      <chr> "Кашкин Д.Е.", "Кашкин Д.Е.", "Кашкин Д.Е.", "Кашкин …
    $ text_id          <chr> "C_101__1", "C_101__2", "C_101__3", "C_101__4", "C_10…
    $ text_title       <chr> "Романс", "Романс", "Романс", "Романс", "Романс", "Ро…
    $ subtitle         <chr> "", "", "", "", "", "", "", "", "", "", "", "", "", "…
    $ first_line       <chr> "В власти - ль смертного забыть,", "Время протекло пр…
    $ text_pages       <chr> "349-350", "350-352", "352-353", "354-355", "355-357"…
    $ notes            <chr> "", "", "", "", "", "", "", "", "", "", "", "", "", "…
    $ text             <chr> "В власти - ль смертного забыть,\nЧто ему всего дорож…
    $ COL_ID           <chr> "COL_23", "COL_23", "COL_23", "COL_23", "COL_23", "CO…
    $ book_type        <chr> "col", "col", "col", "col", "col", "col", "col", "col…
    $ book_genre       <chr> "lyr", "lyr", "lyr", "lyr", "lyr", "lyr", "lyr", "lyr…
    $ author_sign      <chr> "Кашкин Д.", "Кашкин Д.", "Кашкин Д.", "Кашкин Д.", "…
    $ author_book      <chr> "Кашкин Д.Е.", "Кашкин Д.Е.", "Кашкин Д.Е.", "Кашкин …
    $ author_full_name <chr> "Кашкин Дмитрий Евгеньевич", "Кашкин Дмитрий Евгеньев…
    $ title            <chr> "Сочинения Дмитрия Кашкина: В 3 т. Т. 1", "Сочинения …
    $ city             <chr> "М.", "М.", "М.", "М.", "М.", "М.", "М.", "М.", "М.",…
    $ publisher        <chr> "Степанова", "Степанова", "Степанова", "Степанова", "…
    $ year             <chr> "1836", "1836", "1836", "1836", "1836", "1836", "1836…
    $ book_pages       <chr> "468", "468", "468", "468", "468", "468", "468", "468…
    $ path             <chr> "../../data/corpus1835/collections/raw_texts/unsepara…
    $ A_ID             <chr> "A_22", "A_22", "A_22", "A_22", "A_22", "A_22", "A_22…
    $ n_texts          <int> 47, 47, 47, 47, 47, 47, 47, 47, 47, 47, 47, 47, 47, 4…

``` r
colnames(books_corpus)
```

     [1] "path_text"        "id"               "author_text"      "text_id"         
     [5] "text_title"       "subtitle"         "first_line"       "text_pages"      
     [9] "notes"            "text"             "COL_ID"           "book_type"       
    [13] "book_genre"       "author_sign"      "author_book"      "author_full_name"
    [17] "title"            "city"             "publisher"        "year"            
    [21] "book_pages"       "path"             "A_ID"             "n_texts"         

``` r
# some renaming & column_reordering

books_corpus <- books_corpus %>% 
  rename(book_id = id,
         path_raw = path,
         book_title = title,
         text_subtitle = subtitle) %>% 
  select(
    book_id, text_id, # main id info
    # text info
    A_ID, author_text, 
    text_title, text_subtitle, first_line, text_pages, notes,
    
    # text
    text,
    path_text,
    
    # book info
    COL_ID, book_type, book_genre, 
    n_texts,
    author_sign, author_book, author_full_name, 
    book_title, city, publisher, year,
    book_pages, path_raw
  )

glimpse(books_corpus)
```

    Rows: 2,895
    Columns: 24
    $ book_id          <chr> "C_101", "C_101", "C_101", "C_101", "C_101", "C_101",…
    $ text_id          <chr> "C_101__1", "C_101__2", "C_101__3", "C_101__4", "C_10…
    $ A_ID             <chr> "A_22", "A_22", "A_22", "A_22", "A_22", "A_22", "A_22…
    $ author_text      <chr> "Кашкин Д.Е.", "Кашкин Д.Е.", "Кашкин Д.Е.", "Кашкин …
    $ text_title       <chr> "Романс", "Романс", "Романс", "Романс", "Романс", "Ро…
    $ text_subtitle    <chr> "", "", "", "", "", "", "", "", "", "", "", "", "", "…
    $ first_line       <chr> "В власти - ль смертного забыть,", "Время протекло пр…
    $ text_pages       <chr> "349-350", "350-352", "352-353", "354-355", "355-357"…
    $ notes            <chr> "", "", "", "", "", "", "", "", "", "", "", "", "", "…
    $ text             <chr> "В власти - ль смертного забыть,\nЧто ему всего дорож…
    $ path_text        <chr> "../../data/corpus1835/collections/raw_texts/separate…
    $ COL_ID           <chr> "COL_23", "COL_23", "COL_23", "COL_23", "COL_23", "CO…
    $ book_type        <chr> "col", "col", "col", "col", "col", "col", "col", "col…
    $ book_genre       <chr> "lyr", "lyr", "lyr", "lyr", "lyr", "lyr", "lyr", "lyr…
    $ n_texts          <int> 47, 47, 47, 47, 47, 47, 47, 47, 47, 47, 47, 47, 47, 4…
    $ author_sign      <chr> "Кашкин Д.", "Кашкин Д.", "Кашкин Д.", "Кашкин Д.", "…
    $ author_book      <chr> "Кашкин Д.Е.", "Кашкин Д.Е.", "Кашкин Д.Е.", "Кашкин …
    $ author_full_name <chr> "Кашкин Дмитрий Евгеньевич", "Кашкин Дмитрий Евгеньев…
    $ book_title       <chr> "Сочинения Дмитрия Кашкина: В 3 т. Т. 1", "Сочинения …
    $ city             <chr> "М.", "М.", "М.", "М.", "М.", "М.", "М.", "М.", "М.",…
    $ publisher        <chr> "Степанова", "Степанова", "Степанова", "Степанова", "…
    $ year             <chr> "1836", "1836", "1836", "1836", "1836", "1836", "1836…
    $ book_pages       <chr> "468", "468", "468", "468", "468", "468", "468", "468…
    $ path_raw         <chr> "../../data/corpus1835/collections/raw_texts/unsepara…

### save

``` r
books_texts_metadata <- books_corpus %>% 
  select(-text, -notes)

write.csv(books_texts_metadata, file = "../../meta/texts_books_metadata.csv")

# write Rds with texts

saveRDS(books_corpus, file = "../../data/corpus1835/collections/corpus_with_meta.Rds")
```
