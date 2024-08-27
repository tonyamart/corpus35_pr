# 03_corpus_overview

# Corpus overview

## load pckg

``` r
library(tidyverse)
library(tidytext)

library(MetBrewer)
library(cowplot)
theme_set(theme_minimal())
```

## load data

``` r
corpus_1835 <- readRDS("../../data/corpus1835/corpus_1835.Rds")
glimpse(corpus_1835)
```

    Rows: 4,799
    Columns: 20
    $ text_id       <chr> "P_1", "P_10", "P_100", "P_1000", "P_1001", "P_1002", "P…
    $ A_ID          <chr> "", "A-50", "A-7", "A-41", "A-139", "A-11", "A-163", "A-…
    $ author_sign   <chr> "", "Л. Якубович", "Кольцов", "Ф. Глинка", "Н. Прокопови…
    $ author_text   <chr> "", "Якубович Л.А.", "Кольцов А.В.", "Глинка Ф.Н.", "Про…
    $ text_title    <chr> "Солдатская песня", "Молния", "Ночлег чумаков", "Утешите…
    $ text_subtitle <chr> "", "", "Сельские картины", "", "", "", "", "", "", "", …
    $ first_line    <chr> "Ох жизнь, молодецкая", "Зачем с небесной высоты", "В бл…
    $ year          <chr> "1835", "1835", "1836", "1838", "1838", "1838", "1838", …
    $ path_text     <chr> "../../data/corpus1835/periodicals/per_raw//P_1.txt", ".…
    $ source_text   <chr> "Сев_пч. 1835. №12. C. 46", "БдЧ. 1835. Т.8. Отд. 1. C. …
    $ COL_ID        <chr> "", "", "", "", "", "", "", "", "", "", "", "", "", "", …
    $ corpus        <chr> "per", "per", "per", "per", "per", "per", "per", "per", …
    $ text_raw      <chr> "Ох, жизнь молодецкая,\nБравая, солдатская!\nКак осенняя…
    $ text_cln      <chr> "Ох, жизнь молодецкая,\nБравая, солдатская!\nКак осенняя…
    $ text_lemm     <chr> "ох, жизнь молодецкий,\nбравый, солдатский!\nкак осенний…
    $ text_acc      <chr> "Ох, жизнь молоде'цкая,\nБра'вая, солда'тская!\nКак осе'…
    $ meter         <chr> "Other", "Iamb", "Iamb", "Iamb", "Trochee", "Iamb", "Oth…
    $ feet          <chr> "?", "3", "4", "4", "4", "4", "?", "4", "6", "5", "4", "…
    $ formula       <chr> "Other_?", "Iamb_3", "Iamb_4", "Iamb_4", "Trochee_4", "I…
    $ n_lines       <int> 38, 16, 98, 77, 28, 12, 44, 25, 31, 28, 100, 16, 17, 60,…

Read full metadata with books & periodicals titles to count sources

``` r
sources <- read.csv("../../data/corpus1835/sql_db/sources.csv")
text_ids <- read.csv("../../data/corpus1835/sql_db/texts_metadata.csv")

# glimpse(sources)
# glimpse(text_ids)

# extract sources and metrical data
sources <- text_ids %>% 
  select(text_id, source_id, meter, feet) %>% 
  left_join(sources, by = "source_id")

rm(text_ids)
```

Translation

``` r
meters_transl <- tibble(
  meter = c("Other", "Iamb", "Trochee", "Amphibrach", "Anapest", "Dactyl"),
  meter_rus = c("Другой", "Ямб", "Хорей", "Амфибрахий", "Анапест", "Дактиль"),
  meter_short = c("Др", "Я", "Х", "Амф", "Ан", "Д")
)

unique(corpus_1835$meter)
```

    [1] "Other"      "Iamb"       "Trochee"    "Amphibrach" "Anapest"   
    [6] "Dactyl"    

# Overview

Corpus overview in terms of poems’ length and sizes of subcorpora

## corpus total size

``` r
print(paste0("Number of poems: ", nrow(corpus_1835)))
```

    [1] "Number of poems: 4799"

``` r
print(paste0("Number of poems in periodicals: ", table(corpus_1835$corpus)[2]))
```

    [1] "Number of poems in periodicals: 1905"

``` r
print(paste0( "Number of poems in collections: ", table(corpus_1835$corpus)[1]))
```

    [1] "Number of poems in collections: 2894"

``` r
print("Number of lines:")
```

    [1] "Number of lines:"

``` r
corpus_1835 %>% 
  select(corpus, text_raw) %>% 
  separate_rows(text_raw, sep = "\n") %>% 
  filter(text_raw != "") %>% nrow()
```

    [1] 192157

``` r
corpus_1835 %>% 
  select(corpus, text_raw) %>% 
  separate_rows(text_raw, sep = "\n") %>% 
  filter(text_raw != "" & 
           !str_detect(text_raw, "^\\W+$|^\\d+$|^[[:punct:]]+$")) %>% 
  count(corpus)
```

    # A tibble: 2 × 2
      corpus      n
      <chr>   <int>
    1 col    120141
    2 per     71541

``` r
print("Number of tokens:")
```

    [1] "Number of tokens:"

``` r
corpus_1835 %>% 
  select(corpus, text_lemm) %>% 
  unnest_tokens(input = text_lemm, output = word, token = "words") %>% 
  filter(!str_detect(word, "^\\d+$|^\\W+$")) %>% 
  nrow()
```

    [1] 881088

``` r
corpus_1835 %>% 
  select(corpus, text_lemm) %>% 
  unnest_tokens(input = text_lemm, output = word, token = "words") %>% 
  filter(!str_detect(word, "^\\d+$|^\\W+$")) %>% 
  count(corpus) 
```

    # A tibble: 2 × 2
      corpus      n
      <chr>   <int>
    1 col    549576
    2 per    331512

``` r
print("Number of lemmas:")
```

    [1] "Number of lemmas:"

``` r
corpus_1835 %>% 
  select(corpus, text_lemm) %>% 
  unnest_tokens(input = text_lemm, output = word, token = "words") %>% 
  filter(!str_detect(word, "^\\d+$|^\\W+$")) %>% 
  count(word) %>% nrow
```

    [1] 30378

``` r
corpus_1835 %>% 
  select(corpus, text_lemm) %>% 
  unnest_tokens(input = text_lemm, output = word, token = "words") %>% 
  filter(!str_detect(word, "^\\d+$|^\\W+$")) %>% 
  count(corpus, word) %>%
  select(-n) %>%
  ungroup() %>%
  count(corpus)
```

    # A tibble: 2 × 2
      corpus     n
      <chr>  <int>
    1 col    24782
    2 per    18671

Number of poems per year

``` r
corpus_1835 %>% 
  count(year, corpus) %>% 
  ggplot(aes(x = year, y = n, fill = corpus)) + 
  geom_col(position = "dodge") + 
  scale_fill_manual(values = c(met.brewer("Veronese")[3],
                               met.brewer("Veronese")[6]))
```

![](03_1_overview_genres.markdown_strict_files/figure-markdown_strict/unnamed-chunk-6-1.png)

``` r
corpus_1835 %>% 
  count(year, corpus) %>% 
  ggplot(aes(x = year, y = n, fill = corpus)) + 
  geom_col(position = "stack") + 
  scale_fill_manual(values = c(met.brewer("Veronese")[3],
                               met.brewer("Veronese")[6]))
```

![](03_1_overview_genres.markdown_strict_files/figure-markdown_strict/unnamed-chunk-6-2.png)

Number of tokens per year

``` r
tokens <- corpus_1835 %>% 
  select(corpus, year, text_lemm) %>% 
  unnest_tokens(input = text_lemm, output = word, token = "words") %>% 
  count(corpus, year) 
  
tokens %>%  
  ggplot(aes(x = year, y = n, fill = corpus)) + geom_col(position = "dodge") + 
  scale_fill_manual(values = c(met.brewer("Veronese")[3],
                               met.brewer("Veronese")[6]))
```

![](03_1_overview_genres.markdown_strict_files/figure-markdown_strict/unnamed-chunk-7-1.png)

``` r
tokens %>% 
  ggplot(aes(x = year, y = n, fill = corpus)) + geom_col(position = "stack") + 
  scale_fill_manual(values = c(met.brewer("Veronese")[3],
                               met.brewer("Veronese")[6]))
```

![](03_1_overview_genres.markdown_strict_files/figure-markdown_strict/unnamed-chunk-7-2.png)

``` r
sum(tokens$n)
```

    [1] 881120

## authors’ subcorpora

Poems

``` r
corpus_1835 %>%
  filter(author_text != "") %>% 
  count(author_text, corpus, sort = T) %>% head(20)
```

    # A tibble: 20 × 3
       author_text      corpus     n
       <chr>            <chr>  <int>
     1 Крылов И.А.      col      186
     2 Суханов М.Д.     col      132
     3 Баратынский Е.А. col      131
     4 Мейснер А.       col      109
     5 Быстроглазов А.  col       97
     6 Меркли М.М.      col       93
     7 Тимофеев А.В.    col       77
     8 Демидов М.А.     col       75
     9 Башкатов А.      col       69
    10 Жуковский В.А.   col       69
    11 Ушаков А.А.      col       65
    12 Алексеев П.Ф.    col       63
    13 Бенедиктов В.Г.  col       63
    14 Мартынов А.М.    col       57
    15 Якубович Л.А.    per       57
    16 Смирнова А.      col       55
    17 Теплова Н.С.     col       54
    18 Подолинский А.И. col       52
    19 Бернет Е.        col       51
    20 Бутырский Н.И.   col       51

``` r
corpus_1835 %>%
  filter(author_text != "") %>% 
  count(author_text, corpus, sort = T) %>% 
  ggplot(aes(x = reorder_within(author_text, -n, -n), y = n, fill = corpus)) + geom_col() + 
  theme(axis.text.x = element_blank()) + 
  scale_fill_manual(values = c(met.brewer("Veronese")[3],
                               met.brewer("Veronese")[6]))
```

![](03_1_overview_genres.markdown_strict_files/figure-markdown_strict/unnamed-chunk-8-1.png)

Number of tokens by authors

``` r
corpus_1835 %>%
  #filter(author_text != "") %>% 
  select(author_text, corpus, text_lemm) %>% 
  unnest_tokens(input = text_lemm, output = word, token = "words") %>%
  count(author_text, sort = T) %>% head(30)
```

    # A tibble: 30 × 2
       author_text           n
       <chr>             <int>
     1 "Жуковский В.А."  36554
     2 "Крылов И.А."     34887
     3 ""                33293
     4 "Кульман Е.Б."    25338
     5 "Бернет Е."       22293
     6 "Тимофеев А.В."   20729
     7 "Бороздна И.П."   20096
     8 "Быстроглазов А." 19582
     9 "Смирнова А."     19216
    10 "Бенедиктов В.Г." 18315
    # ℹ 20 more rows

``` r
corpus_1835 %>%
  # filter(author != "") %>% 
  select(author_text, corpus, text_lemm) %>% 
  unnest_tokens(input = text_lemm, output = word, token = "words") %>%
  count(author_text, corpus, sort = T) %>% 
  ggplot(aes(x = reorder_within(author_text, -n, -n), y = n, fill = corpus)) + 
  geom_col() + 
  geom_hline(yintercept = 2000, color = "blue") + 
  theme(axis.text.x = element_blank()) + 
  scale_fill_manual(values = c(met.brewer("Veronese")[3],
                               met.brewer("Veronese")[6]))
```

![](03_1_overview_genres.markdown_strict_files/figure-markdown_strict/unnamed-chunk-9-1.png)

# Subtitles & genres

Simple word counter for genres and keywords in titles

``` r
corpus_1835 %>% 
  mutate(title_words = paste(text_title, text_subtitle)) %>% 
  filter(text_title != "NA" & text_subtitle != "na") %>% 
  select(text_id, text_title, title_words) %>% 
  unnest_tokens(input = title_words, output = word, token = "words") %>%
  #filter(word == "е") #%>% 
  count(word, sort = T) %>% 
  head()
```

    # A tibble: 6 × 2
      word      n
      <chr> <int>
    1 к       379
    2 и       365
    3 из      256
    4 в       221
    5 на      189
    6 песня   154

``` r
# things like separate letters ("e") came from titles with abbreviated names ("To E.E.")
```

``` r
titles <- corpus_1835 %>% 
  mutate(title_words = paste(text_title, text_subtitle)) %>% 
  filter(text_title != "NA" & text_subtitle != "na") %>% 
  select(text_id, text_title, title_words) %>% 
  unnest_tokens(input = title_words, output = word, token = "words") #%>%
  #filter(word == "е") #%>% 
  #count(word, sort = T) 

# write.csv(titles, "../../data/ch3/poems_titles.csv") # write to lemmatise
```

Load titles

``` r
titles <- read.csv("../../data/ch3/poems_titles.csv") %>% select(-X)

head(titles)
```

      text_id       text_title       word      lemma
    1     P_1 Солдатская песня солдатская солдатский
    2     P_1 Солдатская песня      песня      песня
    3    P_10           Молния     молния     молния
    4   P_100   Ночлег чумаков     ночлег     ночлег
    5   P_100   Ночлег чумаков    чумаков      чумак
    6   P_100   Ночлег чумаков   сельские   сельский

Count lemmas in titles

``` r
titles_counter <- titles %>% 
  count(lemma, sort = T)

head(titles_counter, 215)
```

                 lemma   n
    1                к 379
    2                и 365
    3            песня 296
    4               из 256
    5                в 221
    6               на 189
    7           романс 115
    8             поэт 106
    9              два 100
    10               с  96
    11             мой  86
    12           сонет  81
    13         русский  75
    14          элегия  73
    15   стихотворение  70
    16           басня  69
    17            ночь  60
    18           песнь  58
    19             год  55
    20          альбом  53
    21            дума  52
    22             она  52
    23          шарада  51
    24          смерть  47
    25         баллада  45
    26         отрывок  45
    27      подражание  45
    28             три  45
    29            друг  44
    30           жизнь  44
    31        послание  43
    32               а  38
    33              ву  38
    34         молитва  36
    35              он  36
    36            день  35
    37               о  35
    38    воспоминание  34
    39          любовь  34
    40            море  33
    41               i  32
    42               е  32
    43             при  32
    44          могила  31
    45              ой  31
    46               у  31
    47          омоним  30
    48       красавица  29
    49        фантазия  29
    50               я  29
    51           поэма  28
    52          псалом  28
    53       эпиграмма  28
    54               г  27
    55            гете  27
    56               л  26
    57              не  26
    58             сон  26
    59               б  25
    60         картина  25
    61            роза  25
    62            стих  25
    63          цветок  25
    64          память  24
    65       последний  24
    66            гюго  23
    67         коринна  23
    68               м  23
    69         соловей  23
    70              ii  22
    71              na  22
    72          звезда  22
    73               н  22
    74             вой  21
    75            дева  21
    76         мелодия  21
    77         перевод  21
    78           новый  20
    79           певец  20
    80         видение  19
    81               й  19
    82           мечта  19
    83          первый  19
    84         разлука  19
    85        славянин  19
    86      славянский  19
    87           слеза  19
    88           вечер  18
    89          виктор  18
    90               д  18
    91     посвящаться  18
    92           станс  18
    93               1  17
    94  антологический  17
    95          аполог  17
    96            быть  17
    97              го  17
    98            орел  17
    99           ответ  17
    100       памятник  17
    101         сердце  17
    102         сестра  17
    103          ангел  16
    104        великий  16
    105           воин  16
    106            все  16
    107        желание  16
    108           змея  16
    109          мысль  16
    110    песнословие  16
    111         письмо  16
    112      признание  16
    113       утешение  16
    114        чувство  16
    115         байрон  15
    116           брат  15
    117    вдохновение  15
    118          гений  15
    119       западный  15
    120           конь  15
    121             по  15
    122        портрет  15
    123      посвящать  15
    124         пушкин  15
    125             св  15
    126         сказка  15
    127          совет  15
    128              2  14
    129     александра  14
    130          венок  14
    131          время  14
    132         грусть  14
    133         девица  14
    134     крестьянин  14
    135         лисица  14
    136           мать  14
    137         москва  14
    138             му  14
    139      присылать  14
    140       прощание  14
    141            iii  13
    142             iv  13
    143       германия  13
    144        надпись  13
    145          пасха  13
    146         родина  13
    147          тоска  13
    148       экспромт  13
    149         эпилог  13
    150              5  12
    151            а.с  12
    152             во  12
    153    возрождение  12
    154      восточный  12
    155          голос  12
    156           граф  12
    157          гроза  12
    158          канон  12
    159            мир  12
    160       наполеон  12
    161         пловец  12
    162     солдатский  12
    163          степь  12
    164             ты  12
    165        человек  12
    166         черный  12
    167         четыре  12
    168              ш  12
    169              4  11
    170          весна  11
    171           волк  11
    172        девушка  11
    173           душа  11
    174             же  11
    175        женщина  11
    176            или  11
    177       ламартин  11
    178         ночной  11
    179        оседать  11
    180              п  11
    181        посылка  11
    182  разочарование  11
    183        рассказ  11
    184          ручей  11
    185           свет  11
    186         шиллер  11
    187           1837  10
    188              3  10
    189              7  10
    190          война  10
    191         вопрос  10
    192        встреча  10
    193           гора  10
    194        древний  10
    195          идеал  10
    196          книга  10
    197        кончина  10
    198            лев  10
    199          мария  10
    200       младенец  10
    201        невеста  10
    202       немецкий  10
    203       праздник  10
    204      развалины  10
    205       разговор  10
    206        римский  10
    207        счастие  10
    208              т  10
    209           утро  10
    210              ф  10
    211       художник  10
    212          юноша  10
    213             vi   9
    214           буря   9
    215    возвращение   9

Count N of periodicals and books texts

``` r
# total number of texts in periodicals and in collections
n_corpus <- corpus_1835 %>% 
  mutate(corpus = str_extract(text_id, "^\\w")) %>% 
  count(corpus) %>% 
  rename(total = n)

n_corpus
```

    # A tibble: 2 × 2
      corpus total
      <chr>  <int>
    1 C       2894
    2 P       1905

### genre titles

Look into particular lemmas

``` r
genres <- unlist(str_split(c("песня
романс
сонет
элегия
басня
песнь
альбом
дума
баллада
отрывок
подражание
послание
молитва
фантазия
псалом
эпиграмма
мелодия
антологический
надпись
аполог
сказка
экспромт
эпилог
шарада
омоним
куплет
гимн
предание
акростих
быль
ода
преложение
анаграмма
эпиграм
застольная
надгробный
аллегория
логогриф
идиллия
нагробный
надгробие
рондо
анаграм
анфологический
газель
элегический
эпиграммаять
эпиграммик"), pattern = "\n"))

genres_shortlist <- unlist(str_split(c("песня
романс
сонет
элегия
басня
песнь
альбом
дума
баллада
отрывок
подражание
послание
молитва
фантазия
псалом
эпиграмма
мелодия
антологический
аполог
сказка
экспромт
надпись
эпилог"), pattern = "\n"))
```

### Figure 3-1-1

Count number of texts that include genre words in title

``` r
titles %>% 
  filter(lemma %in% genres) %>% 
  count(lemma, sort = T)
```

                lemma   n
    1           песня 296
    2          романс 115
    3           сонет  81
    4          элегия  73
    5           басня  69
    6           песнь  58
    7          альбом  53
    8            дума  52
    9          шарада  51
    10        баллада  45
    11        отрывок  45
    12     подражание  45
    13       послание  43
    14        молитва  36
    15         омоним  30
    16       фантазия  29
    17         псалом  28
    18      эпиграмма  28
    19        мелодия  21
    20 антологический  17
    21         аполог  17
    22         сказка  15
    23        надпись  13
    24       экспромт  13
    25         эпилог  13
    26         куплет   9
    27           гимн   8
    28       предание   8
    29       акростих   7
    30           быль   7
    31            ода   7
    32     преложение   7
    33      анаграмма   6
    34        эпиграм   6
    35     застольная   5
    36     надгробный   5
    37      аллегория   4
    38       логогриф   4
    39        идиллия   3
    40      нагробный   2
    41      надгробие   2
    42          рондо   2
    43        анаграм   1
    44 анфологический   1
    45         газель   1
    46    элегический   1
    47   эпиграммаять   1
    48     эпиграммик   1

``` r
titles %>% 
  filter(lemma %in% genres) %>% 
  mutate(corpus = str_extract(text_id, "^\\w")) %>% 
  count(corpus, lemma, sort = T) %>% 
  pivot_wider(names_from = corpus, values_from = n, values_fill = 0) %>% 
  mutate(total = P+C) %>% 
  arrange(desc(total))
```

    # A tibble: 48 × 4
       lemma       P     C total
       <chr>   <int> <int> <int>
     1 песня     164   132   296
     2 романс     19    96   115
     3 сонет      30    51    81
     4 элегия     29    44    73
     5 басня      12    57    69
     6 песнь      28    30    58
     7 альбом     19    34    53
     8 дума       31    21    52
     9 шарада     51     0    51
    10 баллада    29    16    45
    # ℹ 38 more rows

``` r
# roughly % of texts with genre titles
titles %>% 
  filter(lemma %in% genres) %>% 
  distinct(text_id) %>% 
  count() %>% 
  mutate(perc = n/nrow(corpus_1835)*100)
```

         n     perc
    1 1252 26.08877

``` r
# same % but divided for periodicals and collections
titles %>% 
  filter(lemma %in% genres) %>% 
  distinct(text_id) %>% 
  mutate(corpus = str_extract(text_id, "^\\w")) %>% 
  count(corpus) %>% 
  left_join(n_corpus, by = "corpus") %>% 
  mutate(perc = n/total*100)
```

      corpus   n total     perc
    1      C 663  2894 22.90947
    2      P 589  1905 30.91864

``` r
# remove LRPI data
titles %>% 
  filter(lemma %in% genres) %>% 
  filter(!lemma %in% c("шарада", "омоним", "акростих", "анаграм")) %>% 
  distinct(text_id) %>% 
  mutate(corpus = str_extract(text_id, "^\\w")) %>% 
  count(corpus) %>% 
  left_join(n_corpus, by = "corpus") %>% 
  mutate(perc = n/total*100)
```

      corpus   n total     perc
    1      C 657  2894 22.70214
    2      P 513  1905 26.92913

``` r
titles %>% 
  filter(lemma %in% genres_shortlist) %>% 
  mutate(corpus = str_extract(text_id, "^\\w")) %>% 
  count(corpus, lemma, sort = T) %>% 
  left_join(n_corpus, by = "corpus") %>% 
  mutate(perc = round(n/total*100, 2)) %>% select(-n, -total) %>% 
  pivot_wider(names_from = corpus, values_from = perc)
```

    # A tibble: 23 × 3
       lemma        P     C
       <chr>    <dbl> <dbl>
     1 песня     8.61  4.56
     2 романс    1     3.32
     3 басня     0.63  1.97
     4 сонет     1.57  1.76
     5 элегия    1.52  1.52
     6 послание  0.31  1.28
     7 альбом    1     1.17
     8 дума      1.63  0.73
     9 песнь     1.47  1.04
    10 баллада   1.52  0.55
    # ℹ 13 more rows

``` r
p1 <- titles %>% 
  filter(lemma %in% genres_shortlist) %>% 
  mutate(corpus = str_extract(text_id, "^\\w")) %>% 
  count(corpus, lemma, sort = T) %>% 
  left_join(n_corpus, by = "corpus") %>% 
  mutate(perc = round(n/total*100, 2),
         corpus = ifelse(corpus == "P", "Периодика", "Отд. изд.")) %>% 
  select(-n, -total) %>% 
  ggplot(aes(x = reorder_within(lemma, by = perc, within = lemma), 
             y = perc, 
             fill = corpus)) + 
  scale_x_reordered() + 
  coord_flip() + 
  geom_col(position = "dodge",
           width = 0.8) + 
  labs(x = "", 
       y = "% от всех текстов",
       fill = "Корпус") +
  scale_fill_manual(values = c(met.brewer("Veronese")[3],
                               met.brewer("Veronese")[6])) + 
  theme(axis.text = element_text(size = 12), 
        legend.position = "bottom",
        legend.text = element_text(size = 12))

p1
```

![](03_1_overview_genres.markdown_strict_files/figure-markdown_strict/unnamed-chunk-16-1.png)

``` r
p1_bw <- titles %>% 
  filter(lemma %in% genres_shortlist) %>% 
  mutate(corpus = str_extract(text_id, "^\\w")) %>% 
  count(corpus, lemma, sort = T) %>% 
  left_join(n_corpus, by = "corpus") %>% 
  mutate(perc = round(n/total*100, 2),
         corpus = ifelse(corpus == "P", "Периодика", "Отд. изд.")) %>% 
  select(-n, -total) %>% 
  ggplot(aes(x = reorder_within(lemma, by = perc, within = lemma), 
             y = perc, 
             fill = corpus)) + 
  scale_x_reordered() + 
  coord_flip() + 
  geom_col(position = "dodge",
           width = 0.8) + 
  labs(x = "", 
       y = "% от всех текстов",
       fill = "Корпус") +
  scale_fill_manual(values = c("grey70",
                               "grey20")) + 
  theme(axis.text = element_text(size = 12), 
        legend.position = "bottom",
        legend.text = element_text(size = 12))

p1_bw
```

![](03_1_overview_genres.markdown_strict_files/figure-markdown_strict/unnamed-chunk-16-2.png)

Songs by years

``` r
# glimpse(corpus_1835)

p2 <- titles %>% 
  left_join(corpus_1835 %>% select(text_id, year), by = "text_id") %>% 
  mutate(corpus = str_extract(text_id, "^\\w"), 
         corpus = ifelse(corpus == "P", "Периодика", "Отд. изд.")) %>% 
  count(corpus, year, lemma, sort = T) %>% 
  filter(lemma == "песня" | lemma == "романс") %>% 
  ggplot(aes(x = year, y = n, fill = corpus)) + 
  geom_col(position = "dodge") +  
  facet_wrap(~lemma, ncol = 1) + 
  scale_fill_manual(values = c(met.brewer("Veronese")[3],
                               met.brewer("Veronese")[6])) + 
  theme(axis.text = element_text(size = 12), 
        strip.text = element_text(size = 12),
        legend.position = "bottom") + 
  labs(x = "", y = "Количество текстов", fill = "") 
```

    Warning in left_join(., corpus_1835 %>% select(text_id, year), by = "text_id"): Detected an unexpected many-to-many relationship between `x` and `y`.
    ℹ Row 9189 of `x` matches multiple rows in `y`.
    ℹ Row 1 of `y` matches multiple rows in `x`.
    ℹ If a many-to-many relationship is expected, set `relationship =
      "many-to-many"` to silence this warning.

``` r
p2
```

![](03_1_overview_genres.markdown_strict_files/figure-markdown_strict/unnamed-chunk-17-1.png)

``` r
p2_bw <- titles %>% 
  left_join(corpus_1835 %>% select(text_id, year), by = "text_id") %>% 
  mutate(corpus = str_extract(text_id, "^\\w"), 
         corpus = ifelse(corpus == "P", "Периодика", "Отд. изд.")) %>% 
  count(corpus, year, lemma, sort = T) %>% 
  filter(lemma == "песня" | lemma == "романс") %>% 
  ggplot(aes(x = year, y = n, fill = corpus)) + 
  geom_col(position = "dodge") +  
  facet_wrap(~lemma, ncol = 1) + 
  scale_fill_manual(values = c("grey70", "grey20")) + 
  theme(axis.text = element_text(size = 12), 
        strip.text = element_text(size = 12),
        legend.position = "bottom") + 
  labs(x = "", y = "Количество текстов", fill = "") 
```

    Warning in left_join(., corpus_1835 %>% select(text_id, year), by = "text_id"): Detected an unexpected many-to-many relationship between `x` and `y`.
    ℹ Row 9189 of `x` matches multiple rows in `y`.
    ℹ Row 1 of `y` matches multiple rows in `x`.
    ℹ If a many-to-many relationship is expected, set `relationship =
      "many-to-many"` to silence this warning.

``` r
p2_bw
```

![](03_1_overview_genres.markdown_strict_files/figure-markdown_strict/unnamed-chunk-17-2.png)

``` r
plot_grid(p1, p2,
          ncol = 2, labels = c("A", "Б"),
          rel_widths = c(2,1))

ggsave("plots/Fig_3-1-1.png", plot = last_plot(), dpi = 300, bg = "white",
       width = 10, height = 6)
```

``` r
plot_grid(p1_bw, p2_bw,
          ncol = 2, labels = c("A", "Б"),
          rel_widths = c(2,1))

ggsave("plots/bw/Fig_3-1-1.png", plot = last_plot(), dpi = 300, bg = "white",
       width = 10, height = 6)
```

``` r
# ggsave("plots/Fig_3-1-1.png", plot = p1, dpi = 300,
#        bg = "white", width = 7, height = 5)
# 
# ggsave("plots/Fig_3-1-1a.png", plot = p2, dpi = 300,
#        bg = "white", width = 3, height = 4)
```

Attach metadata to text titles data and count sources per year

``` r
songs_sources <- titles %>% 
  filter(lemma == "песня") %>% 
  select(text_id) %>% 
  left_join(sources, by = "text_id") %>% 
  count(type, year, book_title) %>% 
  arrange(-desc(year))

songs_sources %>% 
  filter(year == "1839" & type == "periodicals")
```

             type year book_title n
    1 periodicals 1839        БдЧ 8
    2 periodicals 1839       ЛПРИ 3
    3 periodicals 1839         ОЗ 3
    4 periodicals 1839         СО 3

``` r
songs_sources %>% 
  filter(year == "1840" & type == "periodicals") %>% 
  arrange(desc(n))
```

             type year book_title  n
    1 periodicals 1840         СО 19
    2 periodicals 1840         ЛГ 18
    3 periodicals 1840         ОЗ 14
    4 periodicals 1840     ПРиВЕТ  9
    5 periodicals 1840       Маяк  7
    6 periodicals 1840        БдЧ  3

``` r
# write full data for exploration
# write.csv(songs_sources, "03_1_genres_songs-sources.csv")
```

Deeper look into songs titles

``` r
titles %>% 
  filter(lemma == "песня") %>% 
  left_join(sources, by = "text_id") %>% 
  filter(year == "1840" & type == "periodicals") %>% 
  select(text_id, text_title, book_title) %>% head(20)
```

       text_id                text_title book_title
    1   P_1163 Казачья колыбельная песня         ОЗ
    2   P_1180             Русская песня         ОЗ
    3   P_1182                     Песня         ОЗ
    4   P_1190                                   ОЗ
    5   P_1191                                   ОЗ
    6   P_1194             Веселая песня         ОЗ
    7   P_1197                     Песня         ОЗ
    8   P_1202                     Песня         ОЗ
    9   P_1205                     Песня         ОЗ
    10  P_1208          Песня разбойника         ОЗ
    11  P_1216                     Песня         ОЗ
    12  P_1238             Русская песня         ОЗ
    13  P_1248             Русская песня         ОЗ
    14  P_1249                     Песня         ОЗ
    15  P_1749 Старинная свадебная песня         ЛГ
    16  P_1765                     Песня         ЛГ
    17  P_1780                     Песня         ЛГ
    18  P_1781          Хороводная песня         ЛГ
    19  P_1794                                   ЛГ
    20  P_1795                                   ЛГ

``` r
# quick check on empty main titles
corpus_1835 %>% 
  filter(text_id %in% c("P_1190", "P_1191", "P_1794", "P_1795"))
```

    # A tibble: 4 × 20
      text_id A_ID  author_sign      author_text text_title text_subtitle first_line
      <chr>   <chr> <chr>            <chr>       <chr>      <chr>         <chr>     
    1 P_1190  "A-7" А. Кольцов       Кольцов А.… ""         Две русские … Греет сол…
    2 P_1191  "A-7" А. Кольцов       Кольцов А.… ""         Две русские … Без ума, …
    3 P_1794  ""    Крестьянин Ав. … Крестьянин… ""         Хороводные п… Как под б…
    4 P_1795  ""    Крестьянин Ав. … Крестьянин… ""         Хороводные п… Вдоль по …
    # ℹ 13 more variables: year <chr>, path_text <chr>, source_text <chr>,
    #   COL_ID <chr>, corpus <chr>, text_raw <chr>, text_cln <chr>,
    #   text_lemm <chr>, text_acc <chr>, meter <chr>, feet <chr>, formula <chr>,
    #   n_lines <int>

### Figure 3-1-2-A: genre and meters

Some work around texts with multiple genres in the title (“подражание
псалму”)

``` r
# multigenre <- titles %>% 
#   select(text_id, lemma) %>% 
#   filter(lemma %in% genres_shortlist) %>% 
#   group_by(text_id) %>% 
#   summarise(genre = paste0(lemma, collapse = " | ")) %>% 
#   filter(str_detect(genre, " \\| "))

# m <- corpus1835 %>% 
#   filter(text_id %in% multigenre$text_id) %>% 
#   select(text_id, text_title, text_subtitle) %>% 
#   left_join(multigenre %>% select(text_id, genre), by = "text_id")

# write.csv(m, "multigenre_texts.csv")

# read the data with normalised genres
m <- read.delim("../../data/ch3/multigenre_texts.csv", sep = ';') %>% select(-X)

glimpse(m)
```

    Rows: 91
    Columns: 5
    $ text_id       <chr> "P_114", "P_1326", "P_1378", "P_138", "P_139", "P_140", …
    $ text_title    <chr> "Обитатель Сиона", "Отрывок из Шиллеровой песни о колоко…
    $ text_subtitle <chr> "Подражание псалму", "", "Подражание французскому", "Сол…
    $ genre         <chr> "подражание | псалом", "отрывок | песня", "песнь | подра…
    $ true_genre    <chr> "псалом", "отрывок", "песнь", "песня", "песня", "песня",…

``` r
titles %>% 
  filter(lemma %in% genres) %>% 
  
  # attach cleaned genres data for selected texts
  left_join(m %>% select(text_id, true_genre), by = "text_id") %>% 
  mutate(lemma = ifelse(!is.na(true_genre), true_genre, lemma)) %>% 
  select(-true_genre) %>% 
  
  # additional selection for a better plot
  # additional selection for a better plot
  filter(lemma %in% c("баллада", "басня", 
                      "дума", "мелодия", #"молитва", "отрывок", 
                      "песнь", "песня", #"подражание", "послание", 
                      #"псалом", 
                      "романс", "сонет", "элегия"#, "эпиграмма"
                      )) %>% 
  # capitalisation for the plot
  mutate(lemma = tools::toTitleCase(lemma)) %>% 
  
  left_join(sources, by = "text_id") %>% 
  filter(meter != "Other") %>% 
  
  # attach rus meters for the plot
  left_join(meters_transl, by = "meter") %>% 
  
  count(lemma, meter_rus, sort = T) %>% 
  ggplot(aes(x = meter_rus, y = n, fill = meter_rus)) + 
  geom_col(position = "dodge") + 
  facet_wrap(~lemma, scales = "free_x") + 
  coord_flip() + 
  scale_fill_manual(values = c(met.brewer("Veronese")[6],
                               met.brewer("Veronese")[4],
                               met.brewer("Veronese")[7],
                               met.brewer("Veronese")[3],
                               met.brewer("Veronese")[1])) + 
  theme(text = element_text(size = 12), 
        legend.position = "None",
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 12)) + 
  labs(x = "", y = "Количество текстов") 
```

![](03_1_overview_genres.markdown_strict_files/figure-markdown_strict/unnamed-chunk-24-1.png)

``` r
# ggsave("plots/Fig_3-1-2a.png", plot = last_plot(), dpi = 300,
#        bg = "white", width = 8, height = 6)
```

``` r
titles %>% 
  filter(lemma %in% genres) %>% 
  
  # attach cleaned genres data for selected texts
  left_join(m %>% select(text_id, true_genre), by = "text_id") %>% 
  mutate(lemma = ifelse(!is.na(true_genre), true_genre, lemma)) %>% 
  select(-true_genre) %>% 
  
  # additional selection for a better plot
  # additional selection for a better plot
  filter(lemma %in% c("баллада", "басня", 
                      "дума", "мелодия", #"молитва", "отрывок", 
                      "песнь", "песня", #"подражание", "послание", 
                      #"псалом", 
                      "романс", "сонет", "элегия"#, "эпиграмма"
                      )) %>% 
  # capitalisation for the plot
  mutate(lemma = tools::toTitleCase(lemma)) %>% 
  
  left_join(sources, by = "text_id") %>% 
  filter(meter != "Other") %>% 
  
  # attach rus meters for the plot
  left_join(meters_transl, by = "meter") %>% 
  
  count(lemma, meter_rus, sort = T) %>% 
  ggplot(aes(x = meter_rus, y = n, fill = meter_rus)) + 
  geom_col(position = "dodge") + 
  facet_wrap(~lemma, scales = "free_x") + 
  coord_flip() + 
  scale_fill_manual(values = c("grey50",
                               "grey30",
                               "grey5",
                               "grey75", 
                               "grey20")
                               ) + 
  theme(text = element_text(size = 12), 
        legend.position = "None",
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 12)) + 
  labs(x = "", y = "Количество текстов") 
```

![](03_1_overview_genres.markdown_strict_files/figure-markdown_strict/unnamed-chunk-25-1.png)

``` r
# ggsave("plots/bw/Fig_3-1-2a.png", plot = last_plot(), dpi = 300,
#        bg = "white", width = 8, height = 6)
```

### Figure 3-1-2-B: genres & formulas

Same for formulas

``` r
titles %>% 
  filter(lemma %in% genres) %>% 
  
  # attach cleaned genres data for selected texts
  left_join(m %>% select(text_id, true_genre), by = "text_id") %>% 
  mutate(lemma = ifelse(!is.na(true_genre), true_genre, lemma)) %>% 
  select(-true_genre) %>% 
  
  # additional selection for a better plot
  filter(lemma %in% c("баллада", "басня", 
                      "дума", "мелодия", #"молитва", "отрывок", 
                      "песнь", "песня", #"подражание", "послание", 
                      #"псалом", 
                      "романс", "сонет", "элегия"#, "эпиграмма"
                      )) %>% 
  mutate(lemma = tools::toTitleCase(lemma)) %>% 
  
  left_join(sources, by = "text_id") %>% 
  filter(meter != "Other") %>% 
  
  left_join(meters_transl, by = "meter") %>% 
  
  mutate(formula = paste0(meter_short, feet)) %>% 
  filter(formula %in% c("Я4", "Я5", "Я6", "Яv", "Я43", "Амф43",
                        "Х4", "Амф4")) %>% 
  mutate(formula = ifelse(formula == "Яv", "Яв", formula)) %>% 
  count(lemma, formula, sort = T) %>% 
  ggplot(aes(x = formula, y = n, fill = formula)) + 
  geom_col(position = "dodge") + 
  facet_wrap(~lemma, scales = "free_x") + 
  coord_flip() + 
  scale_fill_manual(values = c(met.brewer("Archambault")[3], 
                               met.brewer("Archambault")[2],
                               met.brewer("Veronese")[5],
                               met.brewer("Veronese")[3],
                               met.brewer("Veronese")[1],
                               met.brewer("Veronese")[7],
                               met.brewer("Veronese")[4],
                               met.brewer("Veronese")[6])) +
  theme(text = element_text(size = 12), 
        legend.position = "None",
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 12)) + 
  labs(x = "", y = "Количество текстов")
```

![](03_1_overview_genres.markdown_strict_files/figure-markdown_strict/unnamed-chunk-26-1.png)

``` r
# ggsave("plots/Fig_3-1-2b.png", plot = last_plot(), dpi = 300,
#        bg = "white", width = 8, height = 6)
```

``` r
titles %>% 
  filter(lemma %in% genres) %>% 
  
  # attach cleaned genres data for selected texts
  left_join(m %>% select(text_id, true_genre), by = "text_id") %>% 
  mutate(lemma = ifelse(!is.na(true_genre), true_genre, lemma)) %>% 
  select(-true_genre) %>% 
  
  # additional selection for a better plot
  filter(lemma %in% c("баллада", "басня", 
                      "дума", "мелодия", #"молитва", "отрывок", 
                      "песнь", "песня", #"подражание", "послание", 
                      #"псалом", 
                      "романс", "сонет", "элегия"#, "эпиграмма"
                      )) %>% 
  mutate(lemma = tools::toTitleCase(lemma)) %>% 
  
  left_join(sources, by = "text_id") %>% 
  filter(meter != "Other") %>% 
  
  left_join(meters_transl, by = "meter") %>% 
  
  mutate(formula = paste0(meter_short, feet)) %>% 
  filter(formula %in% c("Я4", "Я5", "Я6", "Яv", "Я43", "Амф43",
                        "Х4", "Амф4")) %>% 
  mutate(formula = ifelse(formula == "Яv", "Яв", formula)) %>% 
  count(lemma, formula, sort = T) %>% 
  ggplot(aes(x = formula, y = n, fill = formula)) + 
  geom_col(position = "dodge") + 
  facet_wrap(~lemma, scales = "free_x") + 
  coord_flip() + 
  scale_fill_manual(values = c("grey35",
                               "grey55", 
                               "grey70", 
                               "grey10", 
                               "grey50",
                               "grey60", 
                               "grey40", 
                               "grey80")) +
  theme(text = element_text(size = 12), 
        legend.position = "None",
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 12)) + 
  labs(x = "", y = "Количество текстов")
```

![](03_1_overview_genres.markdown_strict_files/figure-markdown_strict/unnamed-chunk-27-1.png)

``` r
# ggsave("plots/bw/Fig_3-1-2b.png", plot = last_plot(), dpi = 300,
#        bg = "white", width = 8, height = 6)
```

Elegies in iamb-5

``` r
titles %>% 
  filter(lemma %in% genres) %>% 
  
  # attach cleaned genres data for selected texts
  left_join(m %>% select(text_id, true_genre), by = "text_id") %>% 
  mutate(lemma = ifelse(!is.na(true_genre), true_genre, lemma)) %>% 
  select(-true_genre) %>% 
  
  # additional selection for a better plot
  filter(lemma %in% c("баллада", "басня", 
                      "дума", "мелодия", #"молитва", "отрывок", 
                      "песнь", "песня", #"подражание", "послание", 
                      #"псалом", 
                      "романс", "сонет", "элегия"#, "эпиграмма"
                      )) %>% 
  mutate(lemma = tools::toTitleCase(lemma)) %>% 
  
  left_join(sources, by = "text_id") %>% 
  filter(meter != "Other") %>% 
  left_join(corpus_1835 %>% select(text_id, author_text, text_subtitle, text_raw), 
            by = "text_id") %>% 
  filter(lemma == "Сонет" & meter == "Iamb" & feet == "5") 
```

         text_id           text_title   word lemma source_id meter feet        type
    1     P_1038                Сонет  сонет Сонет     Per_8  Iamb    5 periodicals
    2      P_288                Слава сонета Сонет   Per_433  Iamb    5 periodicals
    3      P_289    Два качества бога сонета Сонет   Per_433  Iamb    5 periodicals
    4      P_631               К морю  сонет Сонет   Per_520  Iamb    5 periodicals
    5      P_773                Сонет  сонет Сонет   Per_549  Iamb    5 periodicals
    6      P_965            К Языкову  сонет Сонет   Per_569  Iamb    5 periodicals
    7  C_156__12                Сонет  сонет Сонет     C_156  Iamb    5        book
    8  C_156__28                Сонет  сонет Сонет     C_156  Iamb    5        book
    9  C_156__36               Сонет   сонет Сонет     C_156  Iamb    5        book
    10 C_156__36               Сонет  сонета Сонет     C_156  Iamb    5        book
    11  C_270__3 Воклюзский источник   сонет Сонет     C_270  Iamb    5        book
    12 C_300__20                Сонет  сонет Сонет     C_300  Iamb    5        book
    13 C_301__30                Сонет  сонет Сонет     C_301  Iamb    5        book
    14 C_327__22      К Н.М. Языкову   сонет Сонет     C_327  Iamb    5        book
    15  C_69__16                Сонет  сонет Сонет      C_69  Iamb    5        book
    16  C_70__47 Воклюзский источник   сонет Сонет      C_70  Iamb    5        book
    17   C_76__7                Сонет  сонет Сонет      C_76  Iamb    5        book
    18  C_76__46               Сонет   сонет Сонет      C_76  Iamb    5        book
    19  C_90__48                      сонеты Сонет      C_90  Iamb    5        book
    20  C_90__49                      сонеты Сонет      C_90  Iamb    5        book
    21  C_90__50                      сонеты Сонет      C_90  Iamb    5        book
            class
    1            
    2            
    3            
    4            
    5            
    6            
    7  col -- lyr
    8  col -- lyr
    9  col -- lyr
    10 col -- lyr
    11    alm -- 
    12    alm -- 
    13    alm -- 
    14 col -- lyr
    15 col -- lyr
    16 col -- lyr
    17 col -- lyr
    18 col -- lyr
    19 col -- lyr
    20 col -- lyr
    21 col -- lyr
                                                                                                   book_title
    1                                                                                                    Совр
    2                                                                                                     БдЧ
    3                                                                                                     БдЧ
    4                                                                                                   СОиСА
    5                                                                                                      ОЗ
    6                                                                                                    Совр
    7                              Повести и мелкие стихотворения А. Подолинского. Ч. 2. Мелкие стихотворения
    8                              Повести и мелкие стихотворения А. Подолинского. Ч. 2. Мелкие стихотворения
    9                              Повести и мелкие стихотворения А. Подолинского. Ч. 2. Мелкие стихотворения
    10                             Повести и мелкие стихотворения А. Подолинского. Ч. 2. Мелкие стихотворения
    11 Новогодник, собрание сочинений в прозе и стихах, современных русских писателей изданный Н. Кукольником
    12                                                                         Одесский Альманах, на 1839 год
    13                                                                         Одесский Альманах, на 1840 год
    14                                                                      Лирические стихотворения и сказки
    15                                                             Сочинения Николая Венгера в стихах и прозе
    16                                                                          Опыты в стихах Михаила Деларю
    17                                                                Стихотворения Михаила Меркли / 2-е изд.
    18                                                                Стихотворения Михаила Меркли / 2-е изд.
    19                                                                    Стихотворения Владимира Бенедиктова
    20                                                                    Стихотворения Владимира Бенедиктова
    21                                                                    Стихотворения Владимира Бенедиктова
         city                      publisher year          volume       issue
    1    СПб.                                1839 Т. 13. 2-ой паг            
    2    СПб.                                1839           Т. 35          67
    3    СПб.                                1839           Т. 35          67
    4    СПб.                                1838            Т. 5          10
    5    СПб.                                1839            Т. 3 № 5. Отд. 3
    6    СПб.                                1837            Т. 7            
    7    СПб. А. Смирдина, И. Глазунова и Ко 1837                            
    8    СПб. А. Смирдина, И. Глазунова и Ко 1837                            
    9    СПб. А. Смирдина, И. Глазунова и Ко 1837                            
    10   СПб. А. Смирдина, И. Глазунова и Ко 1837                            
    11   СПб.   Изд. Энц. Лексик. А. Плюшара 1839                            
    12 Одесса                    В город. т. 1839                            
    13 Одесса                    В город. т. 1840                            
    14  Дерпт                      Лаакманна 1840                            
    15     М.                 Селивановского 1835                            
    16   СПб.          Деп. внешней торговли 1835                            
    17     М.                       Кирилова 1837                            
    18     М.                       Кирилова 1837                            
    19   СПб.                      Вингебера 1835                            
    20   СПб.                      Вингебера 1835                            
    21   СПб.                      Вингебера 1835                            
       n_texts total_pages      author_text
    1       10          NA    Петерсон К.А.
    2       13          NA                 
    3       13          NA               Х.
    4        2          NA     Щербина Н.Ф.
    5       15          NA       Мейстер И.
    6       18          NA                *
    7       47         174 Подолинский А.И.
    8       47         174 Подолинский А.И.
    9       47         174 Подолинский А.И.
    10      47         174 Подолинский А.И.
    11      27         422      Деларю М.Д.
    12      39         618     Головачев Г.
    13      44         708                 
    14      62         180    Алексеев П.Ф.
    15      27          68        Венгер Н.
    16      49         152      Деларю М.Д.
    17      48         120      Меркли М.М.
    18      48         120      Меркли М.М.
    19      50         106  Бенедиктов В.Г.
    20      50         106  Бенедиктов В.Г.
    21      50         106  Бенедиктов В.Г.
                                      text_subtitle
    1                                              
    2                                    Два сонета
    3                                    Два сонета
    4                                         Сонет
    5                                              
    6                              Сонет. Из Дерпта
    7                                              
    8                                              
    9  Ю.М. Познанскому на перевод сонета: Do Laury
    10 Ю.М. Познанскому на перевод сонета: Do Laury
    11                             Сонет (Е.А. К-ф)
    12                                             
    13                                             
    14                                        Сонет
    15                                             
    16                                        Сонет
    17                                             
    18                                   Из Бюргера
    19                                       Сонеты
    20                                       Сонеты
    21                                       Сонеты
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     text_raw
    1                     С тоской и полный грустных размышлений\nЧитаю я твой вдохновенный стих.\nО, почему так рано ты затих,\nНам всех своих не досказав видений!\n\nКак много дум еще таилось в них,\nКак много неба тайных откровений!\nНо ты замолк... Прости ж, наш Русский гений :\nЛюбовь России на стихах твоих.\n\nГде есть любовь — нет места укоризне.\nНо Русскому как не скорбеть душой?\nТы светлый перл в венце твоей отчизны.\n\nТвои стихи — твой вечный мавзолей :\nТы отплатил за дар недолгой жизни\nНетленной славой, Русский соловей!
    2  "На труд тяжелый променяв забавы ,\nЗабыв существенность за мир мечты,\nЧего, поэт, художник, жаждешь ты?\nВся паша цель — земной достигнуть славы!\n\nНедальновидные! одни отравы,\nНасмешки ждут вас, зависть и хулы ,\nИль, может, черни глупые хвалы,\nИль приговор суда людей неправый !\n\nКоль гений ты, — ты обречен страдать.\nДум собственных ты — мученик всечасный.\nЧто слава ? — Дым! — К чему ж труды терять?"\n\n— Не слушайте! Нет, то не труд напрасный!\nПусть слава — дым; она всё ж дым прекрасный.\nКоторого векам не разогнать!
    3           Ты захотел, - вселенная была!\nДохнул, Господь, — стал человек из праха!\nРазгневан Ты , — миры дрожат от страха!\nЛик прояснишь, — природа весела!\n\nТвой голос — гром. Шум бурь — Твои слова. \nТы повелишь, и рухнет все творенье;\nТы повелишь, и станет жизнью тленье;\nИ мглою свет, и светом станет мгла.\n\nСилен в делах , — в любви Господь сильнее.\nКарает Он; но любит Он прощать.\nК бескровным Он внимателен как мать,\n\nНо к страждущим и матери нежнее.\nНеумолимый только для злодея,\nОн для раскаянья — весь благодать!
    4                                          С какою радостью я свиделся с тобою, \nО зеркало небес, наш исполин земной! \nКакою-то высокою мечтою \nЯ воскрылен, когда ты предо мной!\n\nЯ трепещу, когда, валы вздымая, \nТы восстаешь кичливо до небес, \nИ, облака волнами подпирая, \nШумишь, как вихрь, волнуешься, как лес.\n\nТвой грозный шум, могучее стенанье \nИ буйное зыбучих волн плесканье, \nИ жемчугом, и думою кипят!\n\nАх! если б знать мне мысль твою, стихия, \nЧто волны мне так шумно говорят -  \nО чем ревут их вопли вековые...
    5                             Есть на земле могучий чародей:\nЕго уму и вдохновенной власти\nПокорен сонм духов, сердца людей,\nИ бунт стихий, и пламенные страсти.\n\nС природой он, как с другом, речь ведет;\nВздохнет -- на вздох природа отзовется ,\nПоет -- и с ним природа вся поет;\nКогда жь восторг в его груди проснется --\n\nОн песнь свою огнем души зажжет\nИ небеса на землю низведет;\nВодушевит бездушный, мертвый камень\nИ осенит бессмертьем прах костей!\nОн, как Титан, с небес похитил пламень\nИ, как Титан, страдает за людей.
    6                                             Как серебро нагорного каскада,\nЛетя со скал хрустальною дугой,\nТвой круглый стих, как жемчуг дорогой,\nШумит, кипит над соком винограда!\n\nГорды твоей разгульной лиры чада!\nИх не дерзнет воспеть певец другой,\nЗвук ревности под Вакховой ногой\nЗамрет как свист,— вот дерзости награда!\n\nКак аромат душистого вина.\nЗа пуншевым столом всегда слышна\nДуша пиров — твоя песнь удалая!\n\nВ ней как звезда на небе зажжена\nВ лучах горит студентов старина\nСвободный пыл и молодость былая!...
    7                                                    Им весело; они друг другу рады,\nСредь них лишь я безмолвен и уныл,\nИ гордо их докучливые взгляды\nМолчанием суровым отразил.\n\nНе в них искать участья и отрады:\nИ я слезу - предательницу скрыл,\nНо мыслию, которой нет преграды,\nЯ близ тебя, мой добрый Ангел был!\n\nО верь, мой друг, не мысль одну, желалось\nИ душу всю к тебе перенести,\nЧтоб над тобой повсюду раздавалось\n\nПечальное, последнее прости!\nЧтоб и твое мне сердца отозвалось,\nИ грусть по мне легла в твоей груди!
    8                                  Не потому томительным виденьем, \nВо снах твоих блуждает обраэ мой, \nЧто, может быть, с тоскою и волненьем \nТы обо мне подумаешь порой,\n\nНо оттого, что часто с напряженьем, \nВ ночи без сна, к тебе стремлюсь мечтой, \nИ увлечен я весь воображеньем\nИ с сном твоим сливаюся душой.\n\nПрости меня! Невольной, неизбежной, \nЯ предаюсь мечте моей вполне,\nЯ бы хотел, чтоб призрак мой мятежной\n\nНе говорил о милой старине; —\nНо ты простишь: не ты ль сама так нежно \nОб этих снах рассказывала мне!..
    9                     Любовь он пел, печалью вдохновенный, \nИ чуждых слов не понял я вполне, \nИ только был напев одноплеменный, \nКак томный взор, как вздох, понятен мне.\n\nНо ты постиг душою умиленной, \nЧто звуков тех таилось в глубине, \nИ скорбь души, страданьем утомленной, \nОтозвалась и на твоей струне.\n\nИ я внемлю понятному мне звуку, \nКак бы внимал страдальцу самому, \nИ я б хотел с участьем брата руку\n\nПри встрече с ним хоть раз пожать ему, - \nМне тот не чужд, кто знал любовь и муку \nИ кто их пел по сердцу моему.
    10                    Любовь он пел, печалью вдохновенный, \nИ чуждых слов не понял я вполне, \nИ только был напев одноплеменный, \nКак томный взор, как вздох, понятен мне.\n\nНо ты постиг душою умиленной, \nЧто звуков тех таилось в глубине, \nИ скорбь души, страданьем утомленной, \nОтозвалась и на твоей струне.\n\nИ я внемлю понятному мне звуку, \nКак бы внимал страдальцу самому, \nИ я б хотел с участьем брата руку\n\nПри встрече с ним хоть раз пожать ему, - \nМне тот не чужд, кто знал любовь и муку \nИ кто их пел по сердцу моему.
    11                                            На берегу, Воклюзою кропимом,\nОт бурь мирских Петрарка отдыхал;\nЗабывши Рим и сам забытый Римом\nОн уж одной любовию дышал.\n\nЗдесь, в тайном сне, Лауры идеал\nМелькнул пред ним бесплотным Херувимом,\nИ с уст певца, в размере им любимом,\nРоскошный стих понесся, зазвучал.\n\nИ сладость дум и звуков сочетанье\nВоклюзский ток далече разносил,\nИ навсегда с своим журчаньем слил. -\n\nПришелец, вняв Воклюзы лепетанье,\nДосель еще, задумчив и уныл,\nВ нем слышит грусть, любовь и упованье.
    12                                                     Исчезло все , что дни мои златило ,\nУвял любви торжественный венок;\nМоя душа — всех радостей могила ,\nМои мечты — страдания залог,\n\nЯ миру чужд. Лежит перед очами\nГрядущее безгранной полосой...\nПусть волны лет сменяются волнами —\nУ пристани челнок печальный мой.\n\nО, тяжело отнять любовь от сердца ,\nИ на нее с холодностью безверца\nИ с горестным сомнением взирать!\n\nНо тяжелей почувствовать презренье\nВзамен любви , надежд и упоенья,\nИ лучших дум светильник угашать!
    13                                           Об нем молчит народное преданье,\nКак он молчал, снося на утре лет,\nХолодных душ холодное вниманье,\nЗлых языков язвительный навет.\n\nЕго вся жизнь была очарованье;\nОн был к добру любовию согрет:\nПорой был резв, как детское мечтанье;\nПорой велик, как истинный поэт.\n\nВ горниле бед, вооружась терпеньем.\nНе пресмыкался он пред знатью в униженьи\nЕго не знал и не заметил свет.\n\nПришла пора — сбылись его надежды:\nОн вынудил себе людской привет, \nКогда его на век смежились вежды...
    14                                    Как серебро нагорного каскада,\nЛетя со скал хрустальною дугой,\nТвой круглый стих, как жемчуг дорогой,\nДымясь, кипит над соком винограда.\n\nГорды твоей разгульной лиры чада!\nИх не дерзнет воспеть певец другой,\nЗвук ревности под Вакховой ногой —\nЗамрет как свист; — вот дерзости награда!\n\nКак аромат душистого вина,\nЗа пуншевым столом сладка, шумна,\nДуша пиров — твоя песнь удалая!\n\nВ ней, как звезда на небе зазжена, —\nГорит в лучах студентов старина,\nДушевный пыл и молодость лихая. —
    15         Когда друг юности начнет учиться\nВ младый свой вее — блаженней всех веков;\nТогда ни злой любови не страшится,\nНи для подруги он не вьет венок.\n\nНо вдруг, наступит средний век — опасный,\nНачнет любовь жечь бедные сердца;\nТогда с подругою — любовник страстный ,\nПылая ждет счастливого конца,\n\nАх, все пройдет! и после дряхлый дока\nНачнет тайком коситься на любовь.\nНе от него ль, что стар бежит порока\n\nИль от того, что трусит он оков?....\nО, неть! любовь от старости далека,\nИ старика не греет больше кровь.....
    16                                           На берегу, Воклюзою кропимом,\nОт бурь мирских Петрарка отдыхал;\nЗабывши Рим и сам забытый Римом,\nОн здесь одной любовию дышал.\n\nЗдесь в тайном сне Лауры идеал\nМелькнул пред ним бесплотным Херувимом,\nИ с уст Певца, в размере им любимом,\nРоскошный стих понесся, зажурчал.\n\nИ сладость дум и звуков сочетанье\nВоклюзский ток далече разносил,\nИ навсегда с своим журчаньем слил:\n\nПришелец, вняв Воклюзы лепетанье,\nДосель еще, задумчив и уныл,\nВ нем слышишь грусть, любовь и упованье.
    17                                Светило дня погасло за холмом... \nТуман фатой над озером ложится.:.\nЗаря блестит на небе голубом\nИ как пожар потухнувший дымится...\n\nЗвезда горит задумчивым огнем\nИ ласточка на воздухе кружится...\nПрекрасно все! но в сердце молодом\nГрусть, как мертвец, в могиле шевелится. ..\n\nСтеснился дух туманною тоской,\nКак призрак сна мелькнуло вспоминанье\nИ на душу повеяло мечтой....\n\nНо в той мечте уж нет очарованья!\nОдна лишь грусть... и гаснет упованье\nКак этот блеск зари во тьме ночной! ...
    18                                                                                       Когда заря свод неба позлащает,\nЯ рвусь тоской, вздыхая и стеня!\nВ лучах зари, там в блеске обитает\nТа коей нет уж в мире для меня!\n\nВсегда с зарей угаснувшего дня\nСедой Тифон Аврору обнимает!\nЕе ж тогда к груди прижму лишь я,\nКогда заря последняя взыграет!\n\nТифон ! твое угрюмое чело\nЛучем любви супруга молодая\nОсеребрит целуя и лобзая,\n\nИ вот оно уж снова расцвело!\nИ радостно, спокойно и светло;\nА для меня повсюду ночь глухая,!
    19                                                                           Красавица, как райское виденье, \nЯвлялось мне в сияньи голубом; \nПо сердцу разливалось упоенье, \nИ целый мир казался мне венком.\n\nНебесного зефира дуновенье \nЯ узнавал в дыхании святом, \nИ весь я был - молитвенное пенье \nИ исчезал в парении немом.\n\nПрекрасная, я вдохновен тобою; \nНо не моей губительной рукою \nРазвяжется заветный пояс твой. - \n\nМне сладостны томления и слезы. \nДругим отдай обманчивые розы: \nМне дан цветок нетленный, вековой.
    20                                                                Когда вдали от суеты всемирной \nПрекрасная грустит, уединясь, - \nСлеза трепещет на лазури глаз, \nКак перл на незабудочке сапфирной.\n\nВеселием и роскошию пирной \nЕе улыбка блещет в сладкий час; - \nТак два листочка розовых, струясь, \nРасходятся под ласкою зефирной.\n\nПорой и дождь и светят небеса; - \nИ на лице прелестной сердцегубки \nВстречаются улыбка и слеза.\n\nКак тягостны приличию уступки! \nЛобзаньем осушил бы ей глаза, \nЛобзаньем запечатал эти губки!
    21                                                                          Бегун морей дорогою безбрежной \nСтремился в даль могуществом ветрил, \nИ подо мной с кормою быстробежной \nКипучий вал шумливо говорил.\n\nВолнуемый тоскою безнадежной, \nЯ от пловцов чело мое укрыл, \nПоникнул им над влагою мятежной \nИ жаркую слезу в нее сронил.\n\nСнедаема изменой беспощадно, \nМоя душа к виновнице рвалась, \nПо ней слеза последняя слилась -\n\nИ, схваченная раковиной жадной, \nБыть может, перл она произвела \nДля милого изменницы чела!

Check if periodicals stats are significantly different

``` r
titles %>% 
  filter(lemma %in% genres) %>% 
  # additional selection for a better plot
  filter(lemma %in% c("баллада", "басня", "дума", "мелодия", "молитва",
                      "отрывок", "песнь", "песня", "подражание", "послание", 
                      "псалом", "романс", "сонет", "элегия", "эпиграмма")) %>% 
  left_join(sources, by = "text_id") %>% 
  filter(meter != "Other" & type == "periodicals") %>% 
  mutate(formula = paste0(meter, "_", feet)) %>% 
  filter(formula %in% c("Iamb_4", "Iamb_5", "Iamb_6", "Iamb_other",
                        "Trochee_4", "Amphibrach_4")) %>% 
  count(lemma, formula, sort = T) %>% 
  ggplot(aes(x = formula, y = n, fill = formula)) + 
  geom_col(position = "dodge") + 
  facet_wrap(~lemma, scales = "free_x") + 
  coord_flip() + 
  scale_fill_manual(values = c(met.brewer("Veronese")[6],
                               met.brewer("Veronese")[1],
                               met.brewer("Veronese")[7],
                               met.brewer("Veronese")[4],
                               met.brewer("Veronese")[5],
                               met.brewer("Veronese")[3])) +
  theme(text = element_text(size = 12)) + 
  labs(x = "", y = "Количество текстов", 
       title = "Only periodicals")
```

![](03_1_overview_genres.markdown_strict_files/figure-markdown_strict/unnamed-chunk-29-1.png)

Only books

``` r
titles %>% 
  filter(lemma %in% genres) %>% 
  # additional selection for a better plot
  filter(lemma %in% c("баллада", "басня", "дума", "мелодия", "молитва",
                      "отрывок", "песнь", "песня", "подражание", "послание", 
                      "псалом", "романс", "сонет", "элегия", "эпиграмма")) %>% 
  left_join(sources, by = "text_id") %>% 
  filter(meter != "Other" & type == "book") %>% 
  mutate(formula = paste0(meter, "_", feet)) %>% 
  filter(formula %in% c("Iamb_4", "Iamb_5", "Iamb_6", "Iamb_other",
                        "Trochee_4", "Amphibrach_4")) %>% 
  count(lemma, formula, sort = T) %>% 
  ggplot(aes(x = formula, y = n, fill = formula)) + 
  geom_col(position = "dodge") + 
  facet_wrap(~lemma, scales = "free_x") + 
  coord_flip() + 
  scale_fill_manual(values = c(met.brewer("Veronese")[6],
                               met.brewer("Veronese")[1],
                               met.brewer("Veronese")[7],
                               met.brewer("Veronese")[4],
                               met.brewer("Veronese")[5],
                               met.brewer("Veronese")[3])) +
  theme(text = element_text(size = 12)) + 
  labs(x = "", y = "Количество текстов")
```

![](03_1_overview_genres.markdown_strict_files/figure-markdown_strict/unnamed-chunk-30-1.png)

### Topic words in titles

Topic titles (words with freq \> 10 are selected)

``` r
topics <- unlist(str_split("поэт
ночь
она
смерть
друг
жизнь
любовь
море
могила
красавица
сон
роза
цветок
соловей
звезда
дева
певец
видение
мечта
разлука
слеза
вечер
сердце
сестра
ангел
воин
желание
мысль
признание
утешение
чувство
вдохновение
гений
конь
время
грусть
девица
крестьянин
мать
родина
тоска
возрождение
гроза
пловец
степь
человек
весна
девушка
душа
ночной
свет
война
гора
древний
идеал
книга
младенец
невеста
развалины
счастие
утро
художник
юноша", pattern = "\n"))

# roughly % of texts with topic titles
titles %>% 
  filter(lemma %in% topics) %>% 
  distinct(text_id) %>% 
  count() %>% 
  mutate(perc = n/nrow(corpus_1835)*100)
```

         n     perc
    1 1138 23.71327

``` r
# same % but divided for periodicals and collections
titles %>% 
  filter(lemma %in% topics) %>% 
  distinct(text_id) %>% 
  mutate(corpus = str_extract(text_id, "^\\w")) %>% 
  count(corpus) %>% 
  left_join(n_corpus, by = "corpus") %>% 
  mutate(perc = n/total*100)
```

      corpus   n total     perc
    1      C 665  2894 22.97858
    2      P 473  1905 24.82940

``` r
titles %>% 
  filter(lemma %in% topics) %>% 
  mutate(corpus = str_extract(text_id, "^\\w")) %>% 
  count(corpus, lemma, sort = T) %>% 
  # left_join(n_corpus, by = "corpus") %>% 
  #mutate(perc = round(n/total*100, 2),
  #       corpus = ifelse(corpus == "P", "Периодика", "Отд. изд.")) %>% 
  # select(-n, -total) %>% 
  ggplot(aes(x = reorder_within(lemma, by = n, within = lemma), 
             y = n, 
             fill = corpus)) + 
  scale_x_reordered() + 
  coord_flip() + 
  geom_col(position = "dodge",
           width = 0.8) + 
  labs(x = "", 
       y = "% от всех текстов",
       fill = "Корпус") +
  scale_fill_manual(values = c(met.brewer("Veronese")[3],
                               met.brewer("Veronese")[6])) + 
  theme(axis.text = element_text(size = 12))
```

![](03_1_overview_genres.markdown_strict_files/figure-markdown_strict/unnamed-chunk-31-1.png)

Total number of hits

``` r
titles %>% 
  filter(lemma %in% topics) %>% 
  count(lemma, sort = T) %>% 
  head(30)
```

           lemma   n
    1       поэт 106
    2       ночь  60
    3        она  52
    4     смерть  47
    5       друг  44
    6      жизнь  44
    7     любовь  34
    8       море  33
    9     могила  31
    10 красавица  29
    11       сон  26
    12      роза  25
    13    цветок  25
    14   соловей  23
    15    звезда  22
    16      дева  21
    17     певец  20
    18   видение  19
    19     мечта  19
    20   разлука  19
    21     слеза  19
    22     вечер  18
    23    сердце  17
    24    сестра  17
    25     ангел  16
    26      воин  16
    27   желание  16
    28     мысль  16
    29 признание  16
    30  утешение  16

Thematic words in titles distribution

``` r
titles %>% 
  filter(lemma %in% topics) %>% 
  distinct(text_id) %>%
  left_join(sources, by = "text_id") %>% 
  count(type, year) %>%
  ggplot(aes(x = year, y = n, fill = type)) + 
  geom_col(position = "dodge") + 
  scale_fill_manual(values = c(met.brewer("Veronese")[3],
                               met.brewer("Veronese")[6]))
```

![](03_1_overview_genres.markdown_strict_files/figure-markdown_strict/unnamed-chunk-33-1.png)

Selected words appeared more than 16 times : поэт, ночь, жизнь, смерть,
море, любовь, могила, сон, крестьянин, мечта

``` r
titles %>% 
  filter(lemma %in% topics) %>% 
  left_join(sources, by = "text_id") %>% 
  filter(lemma %in% c("поэт", "ночь", "жизнь", "смерть",
                      "море", "любовь", "могила", "сон", "мечта", 
                      "звезда", "певец", "разлука", "слеза",
                      "ангел", "мысль", "родина")) %>% 
  count(type, year, lemma) %>% 
  ggplot(aes(x = year, y = n, fill = type)) + 
  geom_col(position = "dodge") + 
  facet_wrap(~lemma) + 
  scale_fill_manual(values = c(met.brewer("Veronese")[3],
                               met.brewer("Veronese")[6]))
```

![](03_1_overview_genres.markdown_strict_files/figure-markdown_strict/unnamed-chunk-34-1.png)

Selected words and meters

``` r
titles %>% 
  filter(lemma %in% topics) %>% 
  left_join(sources, by = "text_id") %>% 
  filter(lemma %in% c("поэт", "ночь", "жизнь", "смерть",
                      "море", "любовь", "могила", "сон", "мечта", 
                      "звезда", "певец", "разлука", "слеза",
                      "ангел", "мысль", "родина")) %>% 
  count(meter, lemma) %>% 
  filter(meter != "Other") %>% 
  ggplot(aes(x = meter, y = n, fill = meter)) + 
  geom_col(position = "dodge") + 
  facet_wrap(~lemma, scales = "free") + 
  coord_flip() + 
  scale_fill_manual(values = c(met.brewer("Veronese")[6],
                               met.brewer("Veronese")[4],
                               met.brewer("Veronese")[7],
                               met.brewer("Veronese")[1],
                               met.brewer("Veronese")[3])) + 
  theme(text = element_text(size = 12)) + 
  labs(x = "", y = "Количество текстов")
```

![](03_1_overview_genres.markdown_strict_files/figure-markdown_strict/unnamed-chunk-35-1.png)

Formulas

``` r
titles %>% 
  filter(lemma %in% topics) %>% 
  filter(lemma %in% c("поэт", "ночь", "жизнь", "смерть",
                      "море", "любовь", "могила", "сон", "мечта", 
                      "звезда", "певец", "разлука", "слеза",
                      "ангел", "мысль", "родина")) %>% 
  
  mutate(lemma = tools::toTitleCase(lemma)) %>% 
  
  left_join(sources, by = "text_id") %>% 
  filter(meter != "Other") %>% 
  
  left_join(meters_transl, by = "meter") %>% 
  
  mutate(formula = paste0(meter_short, feet)) %>% 
  filter(formula %in% c("Я4", "Я5", "Я6", "Яother",
                        "Х4", "Амф4")) %>% 
  mutate(formula = ifelse(formula == "Яother", "Яв", formula)) %>% 
  count(lemma, formula, sort = T) %>% 
  ggplot(aes(x = formula, y = n, fill = formula)) + 
  geom_col(position = "dodge") + 
  facet_wrap(~lemma, scales = "free_x") + 
  coord_flip() + 
  scale_fill_manual(values = c(met.brewer("Veronese")[5],
                               met.brewer("Veronese")[3],
                               met.brewer("Veronese")[1],
                               met.brewer("Veronese")[7],
                               met.brewer("Veronese")[4],
                               met.brewer("Veronese")[6])) +
  theme(text = element_text(size = 12), 
        legend.position = "None") + 
  labs(x = "", y = "Количество текстов")
```

![](03_1_overview_genres.markdown_strict_files/figure-markdown_strict/unnamed-chunk-36-1.png)

Names

``` r
n <- unlist(str_split(
  "гете
гюго
байрон
пушкин
наполеон
ламартин
шиллер",
  pattern = "\n"
))

# roughly % of texts with topic titles
titles %>% 
  filter(lemma %in% n) %>% 
  distinct(text_id) %>% 
  count() %>% 
  mutate(perc = n/nrow(corpus_1835)*100)
```

        n     perc
    1 114 2.375495

``` r
# same % but divided for periodicals and collections
titles %>% 
  filter(lemma %in% n) %>% 
  distinct(text_id) %>% 
  mutate(corpus = str_extract(text_id, "^\\w")) %>% 
  count(corpus) %>% 
  left_join(n_corpus, by = "corpus") %>% 
  mutate(perc = n/total*100)
```

      corpus  n total     perc
    1      C 56  2894 1.935038
    2      P 58  1905 3.044619

``` r
titles %>% 
  filter(lemma %in% n) %>% 
  mutate(corpus = str_extract(text_id, "^\\w")) %>% 
  count(corpus, lemma, sort = T) %>% 
  left_join(n_corpus, by = "corpus") %>% 
  select(-total) %>% 
  pivot_wider(names_from = corpus, values_from = n)
```

    # A tibble: 7 × 3
      lemma        P     C
      <chr>    <int> <int>
    1 гюго        17     6
    2 гете        13    14
    3 шиллер       2     9
    4 байрон       9     6
    5 пушкин       8     7
    6 ламартин     4     7
    7 наполеон     5     7

Specific words

``` r
n <- unlist(str_split(
  "1837
русский
славянин
славянский
солдатский
москва",
  pattern = "\n"
))

# roughly % of texts with topic titles
titles %>% 
  filter(lemma %in% n) %>% 
  distinct(text_id) %>% 
  count() %>% 
  mutate(perc = n/nrow(corpus_1835)*100)
```

        n   perc
    1 146 3.0423

``` r
# same % but divided for periodicals and collections
titles %>% 
  filter(lemma %in% n) %>% 
  distinct(text_id) %>% 
  mutate(corpus = str_extract(text_id, "^\\w")) %>% 
  count(corpus) %>% 
  left_join(n_corpus, by = "corpus") %>% 
  mutate(perc = n/total*100)
```

      corpus   n total     perc
    1      C  39  2894 1.347616
    2      P 107  1905 5.616798

``` r
titles %>% 
  filter(lemma %in% n) %>% 
  mutate(corpus = str_extract(text_id, "^\\w")) %>% 
  count(corpus, lemma, sort = T) %>% 
  left_join(n_corpus, by = "corpus") %>% 
  select(-total) %>% 
  pivot_wider(names_from = corpus, values_from = n)
```

    # A tibble: 6 × 3
      lemma          P     C
      <chr>      <int> <int>
    1 русский       51    24
    2 славянский    19    NA
    3 славянин      17     2
    4 москва         6     8
    5 солдатский     8     4
    6 1837           7     3

# 
