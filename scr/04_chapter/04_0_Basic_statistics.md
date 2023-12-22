# 4.1 Meters: basic statistics

## Basic statistics

``` r
library(tidyverse)
```

    Warning: package 'ggplot2' was built under R version 4.3.1

    Warning: package 'lubridate' was built under R version 4.3.1

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

``` r
corpus_1835 <- readRDS("../../data/corpus1835/corpus_1835.Rds")

glimpse(corpus_1835)
```

    Rows: 4,797
    Columns: 20
    $ text_id       <chr> "P_1", "P_10", "P_100", "P_1000", "P_1001", "P_1002", "P…
    $ A_ID          <chr> "", "A-50", "A-7", "A-41", "A-139", "A-11", "A-163", "A-…
    $ author_text   <chr> "", "Якубович Л.А.", "Кольцов А.В.", "Глинка Ф.Н.", "Про…
    $ author_sign   <chr> "", "Л. Якубович", "Кольцов", "Ф. Глинка", "Н. Прокопови…
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
    $ meter         <fct> Other?, Iamb, Iamb, Iamb, Trochee, Iamb, Trochee, Iamb, …
    $ feet          <chr> "?", "3", "4", "4", "4", "4", "other", "4", "6", "5", "4…
    $ formula       <chr> "Other?_?", "Iamb_3", "Iamb_4", "Iamb_4", "Trochee_4", "…
    $ n_lines       <int> 38, 16, 98, 77, 28, 12, 44, 25, 31, 28, 100, 16, 17, 60,…

Small notebook counting number of poems of different meters

### n poems in groups

``` r
# number of texts from periodicals
n_texts_per <- corpus_1835 %>% filter(corpus == "per") %>% nrow()

# number of texts from books ("collections")
n_texts_col <- corpus_1835 %>% filter(corpus == "col") %>% nrow()

# total number of texts
n_texts_total <- corpus_1835 %>% nrow()
```

### meters

``` r
meters_total <- corpus_1835 %>% 
  count(meter, sort = T) %>% 
  mutate(perc = round((n/n_texts_total) * 100, 1))

meters_total
```

    # A tibble: 6 × 3
      meter          n  perc
      <fct>      <int> <dbl>
    1 Iamb        3055  63.7
    2 Trochee      876  18.3
    3 Amphibrach   429   8.9
    4 Other?       206   4.3
    5 Anapest      142   3  
    6 Dactyl        89   1.9

Meters in periodicals

``` r
meters_p <- corpus_1835 %>% 
  filter(corpus == "per") %>% 
  count(meter, sort = T) %>% 
  mutate(perc = round((n / n_texts_per) * 100, 1 ))

meters_p
```

    # A tibble: 6 × 3
      meter          n  perc
      <fct>      <int> <dbl>
    1 Iamb        1110  58.3
    2 Trochee      385  20.2
    3 Amphibrach   201  10.6
    4 Other?       108   5.7
    5 Anapest       67   3.5
    6 Dactyl        34   1.8

Meters in books / collections

``` r
meters_b <- corpus_1835 %>% 
  filter(corpus == "col") %>% 
  count(meter, sort = T) %>% 
  mutate(perc = round( (n / n_texts_col) * 100, 1 ))

meters_b
```

    # A tibble: 6 × 3
      meter          n  perc
      <fct>      <int> <dbl>
    1 Iamb        1945  67.3
    2 Trochee      491  17  
    3 Amphibrach   228   7.9
    4 Other?        98   3.4
    5 Anapest       75   2.6
    6 Dactyl        55   1.9

### formulas

Main metrical variations:

``` r
formula_main <- c("Iamb_3", "Iamb_4", "Iamb_5", "Iamb_6", "Iamb_other",
                  "Trochee_4", "Amphibrach_4", "Anapest_2")

corpus_formula <- corpus_1835 %>% 
  select(corpus, formula) %>% 
  mutate(formula = ifelse(formula %in% formula_main, formula, "Other"))
```

``` r
formula_total <- corpus_formula %>% 
  select(formula) %>% 
  count(formula, sort = T) %>% 
  mutate(perc = round((n/n_texts_total)*100, 1))

formula_total
```

    # A tibble: 9 × 3
      formula          n  perc
      <chr>        <int> <dbl>
    1 Iamb_4        1479  30.8
    2 Iamb_other     899  18.7
    3 Other          815  17  
    4 Trochee_4      662  13.8
    5 Iamb_6         368   7.7
    6 Iamb_5         246   5.1
    7 Amphibrach_4   207   4.3
    8 Iamb_3          89   1.9
    9 Anapest_2       32   0.7

formula periodicals

``` r
formula_per <- corpus_formula %>% 
  filter(corpus == "per") %>% 
  count(formula, sort = T) %>% 
  mutate(perc = round( (n/n_texts_per) *100, 1 ))

formula_per
```

    # A tibble: 9 × 3
      formula          n  perc
      <chr>        <int> <dbl>
    1 Iamb_4         533  28  
    2 Other          383  20.1
    3 Iamb_other     313  16.4
    4 Trochee_4      287  15.1
    5 Iamb_6         149   7.8
    6 Iamb_5         107   5.6
    7 Amphibrach_4   101   5.3
    8 Anapest_2       17   0.9
    9 Iamb_3          15   0.8

formula books

``` r
formula_col <- corpus_formula %>% 
  filter(corpus == "col") %>% 
  count(formula, sort = T) %>% 
  mutate(perc = round( (n/n_texts_col) *100, 1 ))

formula_col
```

    # A tibble: 9 × 3
      formula          n  perc
      <chr>        <int> <dbl>
    1 Iamb_4         946  32.7
    2 Iamb_other     586  20.3
    3 Other          432  14.9
    4 Trochee_4      375  13  
    5 Iamb_6         219   7.6
    6 Iamb_5         139   4.8
    7 Amphibrach_4   106   3.7
    8 Iamb_3          74   2.6
    9 Anapest_2       15   0.5
