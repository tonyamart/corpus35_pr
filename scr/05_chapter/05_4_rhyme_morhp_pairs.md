# 5.2. Rhymes: POS pairs & endings

## 5.2.2. Rhyme grammatical contrast & endings variability

Load pckg

``` r
library(tidyverse)
library(tidytext)

library(MetBrewer)
library(patchwork)
theme_set(theme_minimal())
```

Load data

Metadata

``` r
meta <- read.csv("../../data/corpus1835/sql_db/texts_metadata.csv")
# glimpse(meta)
```

Meter lables

``` r
table(meta$meter)
```


    Amphibrach    Anapest     Dactyl       Iamb      Other    Trochee 
           429        142         89       3055        206        876 

``` r
meter_lables <- meta %>% 
  select(text_id, meter, feet) %>% distinct()

head(meter_lables)
```

      text_id   meter  feet
    1     P_1   Other other
    2    P_10    Iamb     3
    3   P_100    Iamb     4
    4  P_1000    Iamb     4
    5  P_1001 Trochee     4
    6  P_1002    Iamb     4

Rhyme pairs

``` r
rhyme_pairs <- read.csv("../../data/corpus1835/sql_db/rhyme_pairs.csv") %>% 
  rename(text_id = poem_id) %>% 
  # remove Kulman texts
  filter(!str_detect(text_id, "C_264"))

glimpse(rhyme_pairs)
```

    Rows: 81,247
    Columns: 4
    $ text_id    <chr> "P_1938", "P_1938", "P_1938", "C_156__20", "C_156__20", "C_…
    $ from       <chr> "краса", "огневым", "красавицей", "око", "силки", "стонет",…
    $ to         <chr> "небеса", "земным", "красавице", "высоко", "легки", "догони…
    $ rhyme_alph <chr> "краса небеса", "земным огневым", "красавице красавицей", "…

Attach meter data

``` r
nrow(rhyme_pairs)
```

    [1] 81247

``` r
rhyme_pairs <- rhyme_pairs %>% 
  left_join(meter_lables, by = "text_id") 

glimpse(rhyme_pairs)
```

    Rows: 81,247
    Columns: 6
    $ text_id    <chr> "P_1938", "P_1938", "P_1938", "C_156__20", "C_156__20", "C_…
    $ from       <chr> "краса", "огневым", "красавицей", "око", "силки", "стонет",…
    $ to         <chr> "небеса", "земным", "красавице", "высоко", "легки", "догони…
    $ rhyme_alph <chr> "краса небеса", "земным огневым", "красавице красавицей", "…
    $ meter      <chr> "Other", "Other", "Other", "Trochee", "Trochee", "Trochee",…
    $ feet       <chr> "other", "other", "other", "4", "4", "4", "4", "4", "4", "4…

Rhyme words

``` r
rhyme_words <- read.csv("../../data/corpus1835/sql_db/rhyme_words_upd.csv", 
                        
                        # DON'T LET R EAT IAMBS AND DO INTEGER 01 => 1
                        colClasses = c("stress_pattern" = "character",
                                       "closure_pattern" = "character")) 

# check if all words are unique
length(unique(rhyme_words$word)) == nrow(rhyme_words)
```

    [1] TRUE

``` r
# rewrite POS tags
pos_transl <- tibble(old_tag = c("S", "V", "APRO", "SPRO", 
                   "A", "ADV", "NUM", "ADVPRO",
                   "INTJ", "PART", "PR", "ANUM", "CONJ"),
       # pos = c("NOUN", "VERB", "aPRON", "nPRON", 
       #         "ADJ", "ADV", "NUM", "advPRON",
       #         "INTJ", "PART", "ADP", "adjNUM", "CONJ"),
       pos = c("NOUN", "VERB", "PRON", "PRON", 
               "ADJ", "ADV", "NUM", "PRON",
               "INTJ", "PART", "ADP", "NUM", "CONJ")) # upos

# attach to the table with all words
rhyme_words <- rhyme_words %>% 
  rename(old_tag = pos) %>% 
  left_join(pos_transl, by = "old_tag")

# extract inf, imp, etc.
rhyme_words <- rhyme_words %>% 
  mutate(pos = ifelse(str_detect(feats, "инф"),
                      "VERB_inf",
                      pos),
         pos = ifelse(str_detect(feats, "пов"),
                      "VERB_imp",
                      pos),
         pos = ifelse(str_detect(feats, "деепр"),
                      "VERB_deeprich",
                      pos),
         pos = ifelse(str_detect(feats, "прич"),
                      "VERB_prich",
                      pos))

glimpse(rhyme_words)
```

    Rows: 34,801
    Columns: 9
    $ word            <chr> "краса", "огневым", "красавицей", "око", "силки", "сто…
    $ word_acc        <chr> "краса'", "огневы'м", "краса'вицей", "о'ко", "силки'",…
    $ stress_pattern  <chr> "01", "001", "0100", "10", "01", "10", "1", "10", "010…
    $ closure_pattern <chr> "1", "1", "100", "10", "1", "10", "1", "10", "10", "1"…
    $ closure         <chr> "masc", "masc", "dactylic", "fem", "masc", "fem", "mas…
    $ old_tag         <chr> "S", "S", "S", "S", "S", "V", "S", "S", "S", "S", "APR…
    $ feats           <chr> "S,жен,неод=им,ед", "S,фам,муж,од=(дат,мн|твор,ед)", "…
    $ ending_st       <chr> "са'", "ы'м", "а'вицей", "о'ко", "ки'", "о'нет", "о'р"…
    $ pos             <chr> "NOUN", "NOUN", "NOUN", "NOUN", "NOUN", "VERB", "NOUN"…

Attach word’s features to rhyme pairs data

``` r
rhyme_pairs <- rhyme_pairs %>% 
  left_join(rhyme_words %>% 
              select(word, closure, pos, ending_st, feats, stress_pattern) %>% 
              rename(from = word,
                     from_closure = closure,
                     from_pos = pos,
                     from_ending = ending_st, 
                     from_feats = feats,
                     from_sp = stress_pattern),
            by = "from") %>% 
  left_join(rhyme_words %>% 
              select(word, closure, pos, ending_st, feats, stress_pattern) %>% 
              rename(to = word,
                     to_closure = closure,
                     to_pos = pos,
                     to_ending = ending_st, 
                     to_feats = feats,
                     to_sp = stress_pattern),
            by = "to") 

nrow(rhyme_pairs)
```

    [1] 81247

``` r
glimpse(rhyme_pairs)
```

    Rows: 81,247
    Columns: 16
    $ text_id      <chr> "P_1938", "P_1938", "P_1938", "C_156__20", "C_156__20", "…
    $ from         <chr> "краса", "огневым", "красавицей", "око", "силки", "стонет…
    $ to           <chr> "небеса", "земным", "красавице", "высоко", "легки", "дого…
    $ rhyme_alph   <chr> "краса небеса", "земным огневым", "красавице красавицей",…
    $ meter        <chr> "Other", "Other", "Other", "Trochee", "Trochee", "Trochee…
    $ feet         <chr> "other", "other", "other", "4", "4", "4", "4", "4", "4", …
    $ from_closure <chr> "masc", "masc", "dactylic", "fem", "masc", "fem", "masc",…
    $ from_pos     <chr> "NOUN", "NOUN", "NOUN", "NOUN", "NOUN", "VERB", "NOUN", "…
    $ from_ending  <chr> "са'", "ы'м", "а'вицей", "о'ко", "ки'", "о'нет", "о'р", "…
    $ from_feats   <chr> "S,жен,неод=им,ед", "S,фам,муж,од=(дат,мн|твор,ед)", "S,ж…
    $ from_sp      <chr> "01", "001", "0100", "10", "01", "10", "1", "10", "010", …
    $ to_closure   <chr> "masc", "masc", "dactylic", "masc", "masc", "fem", "masc"…
    $ to_pos       <chr> "NOUN", "ADJ", "NOUN", "ADV", "ADJ", "VERB", "NOUN", "ADJ…
    $ to_ending    <chr> "са'", "ы'м", "а'вице", "ко'", "ки'", "о'нит", "о'р", "у'…
    $ to_feats     <chr> "S,сред,неод=(вин,мн|им,мн)", "A=(дат,мн,полн|твор,ед,пол…
    $ to_sp        <chr> "001", "01", "0100", "001", "01", "010", "01", "010", "01…

## POS pairs & syl variation

### masc

Extract only pairs where both words were detected as masculine endings

``` r
masc_pairs <- rhyme_pairs %>% 
  filter(from_closure == "masc" & to_closure == "masc")

print(paste( "Number of (true) masc pairs:", nrow(masc_pairs), 
             "out of total", nrow(rhyme_pairs) ))
```

    [1] "Number of (true) masc pairs: 38011 out of total 81247"

``` r
# check number of one masc and one other clausula type ending (most probably annotation mistakes)
rhyme_pairs %>% 
  filter(from_closure == "masc" & to_closure != "masc") %>% nrow() 
```

    [1] 3183

``` r
rhyme_pairs %>% 
  filter(from_closure != "masc" & to_closure == "masc") %>% nrow() 
```

    [1] 3464

Iamb

``` r
iamb_masc <- masc_pairs %>% 
  filter(meter == "Iamb")

t <- nrow(iamb_masc)
t
```

    [1] 25030

``` r
iamb_count <- iamb_masc %>% 
  rowwise() %>% 
  mutate(pos_pair = paste0(sort(c(from_pos, to_pos)), collapse = " -- ")) %>% 
  ungroup() %>% 
  count(pos_pair, sort = T) %>% 
  mutate(perc = round( (n/t)*100, 2 )) %>% 
  rename(n_iamb = n,
         perc_iamb = perc)

head(iamb_count, 20)
```

    # A tibble: 20 × 3
       pos_pair                 n_iamb perc_iamb
       <chr>                     <int>     <dbl>
     1 NOUN -- NOUN               8980     35.9 
     2 NOUN -- PRON               3424     13.7 
     3 VERB -- VERB               3183     12.7 
     4 NOUN -- VERB               1497      5.98
     5 ADJ -- NOUN                1321      5.28
     6 PRON -- PRON               1077      4.3 
     7 VERB_inf -- VERB_inf        970      3.88
     8 ADV -- NOUN                 785      3.14
     9 ADJ -- PRON                 625      2.5 
    10 ADJ -- ADJ                  348      1.39
    11 NOUN -- PART                293      1.17
    12 NOUN -- VERB_inf            271      1.08
    13 NOUN -- VERB_prich          237      0.95
    14 NOUN -- VERB_imp            228      0.91
    15 ADV -- PRON                 192      0.77
    16 VERB_imp -- VERB_imp        186      0.74
    17 PRON -- VERB_prich          161      0.64
    18 NOUN -- VERB_deeprich       147      0.59
    19 PRON -- VERB                133      0.53
    20 VERB_prich -- VERB_prich     85      0.34

Syllable variation

``` r
iamb_count_syl <- iamb_masc %>% 
  mutate(# calculate number of syllables in each word
         from_n_syl = nchar(from_sp),
         to_n_syl = nchar(to_sp),
         
         # attach n_syl to pos
         from_pos_syl = paste0(from_pos, "_", from_n_syl),
         to_pos_syl = paste0(to_pos, "_", to_n_syl)) %>% 
  rowwise() %>% 
  mutate(pos_pair = paste0(sort(c(from_pos_syl, to_pos_syl)), collapse = " -- ")
         ) %>%
  ungroup() %>% 
  count(pos_pair, sort = T) %>% 
  mutate(perc = round( (n/t)*100, 2 )) %>% 
  rename(n_iamb = n,
         perc_iamb = perc) %>% 
  mutate(rank_iamb = row_number())

iamb_count_syl %>% 
  head(10)
```

    # A tibble: 10 × 4
       pos_pair         n_iamb perc_iamb rank_iamb
       <chr>             <int>     <dbl>     <int>
     1 NOUN_2 -- NOUN_2   2941     11.8          1
     2 NOUN_1 -- NOUN_2   1909      7.63         2
     3 NOUN_2 -- NOUN_3   1735      6.93         3
     4 NOUN_1 -- NOUN_1   1377      5.5          4
     5 NOUN_2 -- PRON_2   1045      4.17         5
     6 VERB_2 -- VERB_3    996      3.98         6
     7 VERB_2 -- VERB_2    943      3.77         7
     8 NOUN_2 -- PRON_1    872      3.48         8
     9 VERB_3 -- VERB_3    546      2.18         9
    10 ADJ_2 -- NOUN_2     524      2.09        10

Trochee

``` r
tr_masc <- masc_pairs %>% 
  filter(meter == "Trochee")

t <- nrow(tr_masc)
t
```

    [1] 7045

``` r
tr_masc %>% 
  rowwise() %>% 
  mutate(pos_pair = paste0(sort(c(from_pos, to_pos)), collapse = " -- ")) %>% 
  ungroup() %>% 
  count(pos_pair, sort = T) %>% 
  mutate(perc = round( (n/t)*100, 2 )) %>% 
  rename(n_tr = n,
         perc_tr = perc) %>% 
  left_join(iamb_count, by = "pos_pair") %>% 
  select(pos_pair, perc_iamb, perc_tr, n_iamb, n_tr) %>% 
  head(20)
```

    # A tibble: 20 × 5
       pos_pair             perc_iamb perc_tr n_iamb  n_tr
       <chr>                    <dbl>   <dbl>  <int> <int>
     1 NOUN -- NOUN             35.9    36.6    8980  2577
     2 VERB -- VERB             12.7    13.2    3183   928
     3 NOUN -- PRON             13.7    12.2    3424   861
     4 NOUN -- VERB              5.98    6.71   1497   473
     5 ADJ -- NOUN               5.28    5.89   1321   415
     6 PRON -- PRON              4.3     4.02   1077   283
     7 VERB_inf -- VERB_inf      3.88    3.7     970   261
     8 ADV -- NOUN               3.14    3.44    785   242
     9 ADJ -- PRON               2.5     2.37    625   167
    10 ADJ -- ADJ                1.39    1.39    348    98
    11 NOUN -- VERB_inf          1.08    0.92    271    65
    12 ADV -- PRON               0.77    0.88    192    62
    13 NOUN -- VERB_imp          0.91    0.84    228    59
    14 NOUN -- VERB_prich        0.95    0.84    237    59
    15 VERB_imp -- VERB_imp      0.74    0.81    186    57
    16 NOUN -- PART              1.17    0.75    293    53
    17 PRON -- VERB_prich        0.64    0.48    161    34
    18 ADV -- ADV                0.31    0.45     78    32
    19 PRON -- VERB              0.53    0.45    133    32
    20 ADV -- VERB               0.26    0.43     65    30

``` r
tr_count_syl <- tr_masc %>% 
  mutate(# calculate number of syllables in each word
         from_n_syl = nchar(from_sp),
         to_n_syl = nchar(to_sp),
         
         # attach n_syl to pos
         from_pos_syl = paste0(from_pos, "_", from_n_syl),
         to_pos_syl = paste0(to_pos, "_", to_n_syl)) %>% 
  rowwise() %>% 
  mutate(pos_pair = paste0(sort(c(from_pos_syl, to_pos_syl)), collapse = " -- ")
         ) %>%
  ungroup() %>% 
  count(pos_pair, sort = T) %>% 
  mutate(perc = round( (n/t)*100, 2 )) %>% 
  rename(n_tr = n,
         perc_tr = perc) %>% 
  mutate(rank_tr = row_number())

tr_count_syl %>% 
  head(10)
```

    # A tibble: 10 × 4
       pos_pair          n_tr perc_tr rank_tr
       <chr>            <int>   <dbl>   <int>
     1 NOUN_2 -- NOUN_2   873   12.4        1
     2 NOUN_2 -- NOUN_3   568    8.06       2
     3 NOUN_1 -- NOUN_2   491    6.97       3
     4 NOUN_1 -- NOUN_1   391    5.55       4
     5 VERB_2 -- VERB_2   309    4.39       5
     6 VERB_2 -- VERB_3   298    4.23       6
     7 NOUN_2 -- PRON_2   297    4.22       7
     8 NOUN_2 -- PRON_1   205    2.91       8
     9 ADJ_2 -- NOUN_2    162    2.3        9
    10 NOUN_2 -- VERB_2   149    2.11      10

``` r
iamb_count_syl %>% 
  left_join(tr_count_syl, by = "pos_pair") %>%  
  select(pos_pair, rank_iamb, rank_tr, perc_iamb, perc_tr, n_iamb, n_tr) %>% 
  head(20)
```

    # A tibble: 20 × 7
       pos_pair                 rank_iamb rank_tr perc_iamb perc_tr n_iamb  n_tr
       <chr>                        <int>   <int>     <dbl>   <dbl>  <int> <int>
     1 NOUN_2 -- NOUN_2                 1       1     11.8    12.4    2941   873
     2 NOUN_1 -- NOUN_2                 2       3      7.63    6.97   1909   491
     3 NOUN_2 -- NOUN_3                 3       2      6.93    8.06   1735   568
     4 NOUN_1 -- NOUN_1                 4       4      5.5     5.55   1377   391
     5 NOUN_2 -- PRON_2                 5       7      4.17    4.22   1045   297
     6 VERB_2 -- VERB_3                 6       6      3.98    4.23    996   298
     7 VERB_2 -- VERB_2                 7       5      3.77    4.39    943   309
     8 NOUN_2 -- PRON_1                 8       8      3.48    2.91    872   205
     9 VERB_3 -- VERB_3                 9      11      2.18    2.04    546   144
    10 ADJ_2 -- NOUN_2                 10       9      2.09    2.3     524   162
    11 NOUN_3 -- PRON_1                11      13      1.9     1.66    475   117
    12 NOUN_3 -- NOUN_3                12      12      1.89    1.8     473   127
    13 NOUN_1 -- NOUN_3                13      14      1.86    1.63    466   115
    14 PRON_1 -- PRON_2                14      19      1.81    1.31    454    92
    15 NOUN_1 -- PRON_1                15      15      1.72    1.41    431    99
    16 NOUN_2 -- VERB_2                16      10      1.64    2.11    410   149
    17 PRON_2 -- PRON_2                17      16      1.4     1.39    350    98
    18 NOUN_1 -- VERB_2                18      18      1.34    1.33    336    94
    19 VERB_inf_2 -- VERB_inf_3        19      20      1.27    1.25    317    88
    20 VERB_1 -- VERB_2                20      21      1.25    1.19    312    84

``` r
ranks_masc <- iamb_count_syl %>% 
  left_join(tr_count_syl, by = "pos_pair") %>% 
  select(pos_pair, rank_iamb, rank_tr, perc_iamb, perc_tr, n_iamb, n_tr) %>%
  drop_na() 

cor(ranks_masc$rank_iamb[1:100], ranks_masc$rank_tr[1:100], method = "kendall")
```

    [1] 0.8290909

### fem

``` r
fem_pairs <- rhyme_pairs %>% 
  filter(from_closure == "fem" & to_closure == "fem")

print(paste( "Number of (true) fem pairs:", nrow(fem_pairs), 
             "out of total", nrow(rhyme_pairs) ))
```

    [1] "Number of (true) fem pairs: 34383 out of total 81247"

``` r
# check number of one masc and one other clausula type ending (most probably annotation mistakes)
rhyme_pairs %>% 
  filter(from_closure == "fem" & to_closure != "fem") %>% nrow() 
```

    [1] 3760

``` r
rhyme_pairs %>% 
  filter(from_closure != "fem" & to_closure == "fem") %>% nrow() 
```

    [1] 3476

Separate iambs & trochees

``` r
iamb_fem <- fem_pairs %>% 
  filter(meter == "Iamb")

tr_fem <- fem_pairs %>% 
  filter(meter == "Trochee")
```

``` r
t <- nrow(iamb_fem)

iamb_count <- iamb_fem %>% rowwise() %>% 
  mutate(pos_pair = paste0(sort(c(from_pos, to_pos)), collapse = " -- ")) %>% 
  ungroup() %>% 
  count(pos_pair, sort = T) %>% 
  mutate(perc = round( (n/t)*100, 2 )) %>% 
  rename(n_iamb = n,
         perc_iamb = perc)

head(iamb_count, 20)
```

    # A tibble: 20 × 3
       pos_pair                       n_iamb perc_iamb
       <chr>                           <int>     <dbl>
     1 NOUN -- NOUN                    10639     44.0 
     2 VERB -- VERB                     4779     19.8 
     3 ADJ -- ADJ                       3045     12.6 
     4 ADJ -- NOUN                      1390      5.74
     5 NOUN -- PRON                      601      2.48
     6 NOUN -- VERB                      527      2.18
     7 ADV -- NOUN                       465      1.92
     8 ADJ -- ADV                        346      1.43
     9 ADJ -- VERB_prich                 337      1.39
    10 ADV -- ADV                        287      1.19
    11 VERB_inf -- VERB_inf              281      1.16
    12 VERB_deeprich -- VERB_deeprich    192      0.79
    13 ADJ -- VERB_deeprich              176      0.73
    14 VERB_prich -- VERB_prich          173      0.71
    15 ADJ -- PRON                       143      0.59
    16 NOUN -- VERB_deeprich             140      0.58
    17 VERB_imp -- VERB_imp              137      0.57
    18 NOUN -- VERB_prich                115      0.48
    19 ADV -- VERB                       107      0.44
    20 PRON -- PRON                       64      0.26

``` r
f_iamb_count_syl <- iamb_fem %>% 
  mutate(# calculate number of syllables in each word
         from_n_syl = nchar(from_sp),
         to_n_syl = nchar(to_sp),
         
         # attach n_syl to pos
         from_pos_syl = paste0(from_pos, "_", from_n_syl),
         to_pos_syl = paste0(to_pos, "_", to_n_syl)) %>% 
  rowwise() %>% 
  mutate(pos_pair = paste0(sort(c(from_pos_syl, to_pos_syl)), collapse = " -- ")
         ) %>%
  ungroup() %>% 
  count(pos_pair, sort = T) %>% 
  mutate(perc = round( (n/t)*100, 2 )) %>% 
  rename(n_iamb = n,
         perc_iamb = perc) %>% 
  mutate(rank_iamb = row_number())

f_iamb_count_syl %>% 
  head(10)
```

    # A tibble: 10 × 4
       pos_pair         n_iamb perc_iamb rank_iamb
       <chr>             <int>     <dbl>     <int>
     1 NOUN_3 -- NOUN_3   3046     12.6          1
     2 NOUN_2 -- NOUN_3   2433     10.0          2
     3 NOUN_3 -- NOUN_4   1928      7.97         3
     4 NOUN_2 -- NOUN_2   1456      6.02         4
     5 VERB_3 -- VERB_4   1406      5.81         5
     6 VERB_4 -- VERB_4   1037      4.29         6
     7 VERB_3 -- VERB_3    887      3.67         7
     8 ADJ_3 -- ADJ_3      842      3.48         8
     9 ADJ_3 -- ADJ_4      801      3.31         9
    10 VERB_2 -- VERB_3    605      2.5         10

``` r
t <- nrow(tr_fem)

tr_fem %>% 
  rowwise() %>% 
  mutate(pos_pair = paste0(sort(c(from_pos, to_pos)), collapse = " -- ")) %>% 
  ungroup() %>% 
  count(pos_pair, sort = T) %>% 
  mutate(perc = round( (n/t)*100, 2 )) %>% 
  rename(n_tr = n,
         perc_tr = perc) %>% 
  left_join(iamb_count, by = "pos_pair") %>% 
  select(pos_pair, perc_iamb, perc_tr, n_iamb, n_tr) %>% 
  head(20)
```

    # A tibble: 20 × 5
       pos_pair                       perc_iamb perc_tr n_iamb  n_tr
       <chr>                              <dbl>   <dbl>  <int> <int>
     1 NOUN -- NOUN                       44.0    43.4   10639  2739
     2 VERB -- VERB                       19.8    17.8    4779  1126
     3 ADJ -- ADJ                         12.6    13.6    3045   858
     4 ADJ -- NOUN                         5.74    7.06   1390   446
     5 NOUN -- VERB                        2.18    2.61    527   165
     6 ADV -- NOUN                         1.92    2.1     465   133
     7 NOUN -- PRON                        2.48    2.07    601   131
     8 ADJ -- ADV                          1.43    1.82    346   115
     9 ADV -- ADV                          1.19    1.36    287    86
    10 ADJ -- VERB_prich                   1.39    1.03    337    65
    11 NOUN -- VERB_deeprich               0.58    1       140    63
    12 ADJ -- VERB_deeprich                0.73    0.93    176    59
    13 VERB_inf -- VERB_inf                1.16    0.81    281    51
    14 VERB_deeprich -- VERB_deeprich      0.79    0.79    192    50
    15 ADJ -- PRON                         0.59    0.65    143    41
    16 ADV -- VERB                         0.44    0.59    107    37
    17 VERB_imp -- VERB_imp                0.57    0.55    137    35
    18 VERB_prich -- VERB_prich            0.71    0.47    173    30
    19 NOUN -- VERB_prich                  0.48    0.38    115    24
    20 ADJ -- VERB                         0.1     0.27     23    17

``` r
f_tr_count_syl <- tr_fem %>% 
  mutate(# calculate number of syllables in each word
         from_n_syl = nchar(from_sp),
         to_n_syl = nchar(to_sp),
         
         # attach n_syl to pos
         from_pos_syl = paste0(from_pos, "_", from_n_syl),
         to_pos_syl = paste0(to_pos, "_", to_n_syl)) %>% 
  rowwise() %>% 
  mutate(pos_pair = paste0(sort(c(from_pos_syl, to_pos_syl)), collapse = " -- ")
         ) %>%
  ungroup() %>% 
  count(pos_pair, sort = T) %>% 
  mutate(perc = round( (n/t)*100, 2 )) %>% 
  rename(n_tr = n,
         perc_tr = perc) %>% 
  mutate(rank_tr = row_number())

f_tr_count_syl %>% 
  head(10)
```

    # A tibble: 10 × 4
       pos_pair          n_tr perc_tr rank_tr
       <chr>            <int>   <dbl>   <int>
     1 NOUN_3 -- NOUN_3   815   12.9        1
     2 NOUN_2 -- NOUN_3   681   10.8        2
     3 NOUN_2 -- NOUN_2   471    7.45       3
     4 NOUN_3 -- NOUN_4   440    6.96       4
     5 VERB_3 -- VERB_4   306    4.84       5
     6 ADJ_3 -- ADJ_3     243    3.85       6
     7 ADJ_3 -- ADJ_4     233    3.69       7
     8 VERB_3 -- VERB_3   224    3.54       8
     9 VERB_4 -- VERB_4   207    3.28       9
    10 VERB_2 -- VERB_3   199    3.15      10

Syllable variation

``` r
f_iamb_count_syl %>% 
  left_join(f_tr_count_syl, by = "pos_pair") %>% 
  select(pos_pair, rank_iamb, rank_tr, perc_iamb, perc_tr, n_iamb, n_tr) %>% 
  head(20)
```

    # A tibble: 20 × 7
       pos_pair         rank_iamb rank_tr perc_iamb perc_tr n_iamb  n_tr
       <chr>                <int>   <int>     <dbl>   <dbl>  <int> <int>
     1 NOUN_3 -- NOUN_3         1       1     12.6    12.9    3046   815
     2 NOUN_2 -- NOUN_3         2       2     10.0    10.8    2433   681
     3 NOUN_3 -- NOUN_4         3       4      7.97    6.96   1928   440
     4 NOUN_2 -- NOUN_2         4       3      6.02    7.45   1456   471
     5 VERB_3 -- VERB_4         5       5      5.81    4.84   1406   306
     6 VERB_4 -- VERB_4         6       9      4.29    3.28   1037   207
     7 VERB_3 -- VERB_3         7       8      3.67    3.54    887   224
     8 ADJ_3 -- ADJ_3           8       6      3.48    3.85    842   243
     9 ADJ_3 -- ADJ_4           9       7      3.31    3.69    801   233
    10 VERB_2 -- VERB_3        10      10      2.5     3.15    605   199
    11 NOUN_4 -- NOUN_4        11      15      2.49    1.39    602    88
    12 ADJ_2 -- ADJ_3          12      11      2.29    2.69    554   170
    13 NOUN_2 -- NOUN_4        13      13      2.06    2.18    499   138
    14 ADJ_3 -- NOUN_2         14      12      1.57    2.22    381   140
    15 NOUN_3 -- NOUN_5        15      21      1.43    0.87    347    55
    16 ADJ_3 -- NOUN_3         16      14      1.3     1.65    315   104
    17 VERB_2 -- VERB_4        17      16      1.22    1.14    295    72
    18 ADJ_4 -- ADJ_4          18      18      1.09    1.04    263    66
    19 NOUN_4 -- NOUN_5        19      26      1.01    0.6     245    38
    20 NOUN_3 -- PRON_2        20      20      1       0.9     241    57

``` r
fem_ranks <- f_iamb_count_syl %>% 
  left_join(f_tr_count_syl, by = "pos_pair") %>% 
  select(pos_pair, rank_iamb, rank_tr, perc_iamb, perc_tr, n_iamb, n_tr) %>% 
  drop_na()

cor(fem_ranks$rank_iamb[1:100], fem_ranks$rank_tr[1:100], method = "kendall")
```

    [1] 0.7171717

## RNC

### Load RNC data

``` r
rnc_rhymes <- read.csv("../../data/ch5/nkrja_rhyme_pairs.csv") %>% select(-X)

# attach meters to extract only iambs
# load metadata to extract meters
load("../../data/nkrja_19th_lem.Rda")
rnc_ids <- c19 %>% 
  filter(meter %in% c("Я", "Х")) %>% 
  mutate(poem_id = paste0("RNC_", Unnamed..0, "_", year), 
         meter = meter) %>% 
  select(poem_id, meter)
  

# attach meters to rhyme data
rnc_rhymes <- rnc_rhymes %>% 
  inner_join(rnc_ids, by = "poem_id")

rm(c19, rnc_ids) # remove large c19 file & iamb-id vector

# look at the resulting data
# glimpse(rnc_rhymes)
```

Distribution of data over time - filter out rhymes after 1830

``` r
ids_before_1830 <- rnc_rhymes %>% 
  select(poem_id) %>% 
  separate(poem_id, into = c("corpus", "id", "year"), sep = "_") %>% 
  filter(as.numeric(year) < 1811) %>% 
  mutate(poem_id = paste0("RNC_", id, "_", year)) %>% 
  pull(poem_id)

# filter only rhymes before 1830
rnc_rhymes <- rnc_rhymes %>% 
  filter(poem_id %in% ids_before_1830)

rm(ids_before_1830) # remove large character vector

# add verb_inf & verb_imp categories
rnc_rhymes <- rnc_rhymes %>% 
  mutate(from_pos = ifelse(str_detect(from_feats, "VerbForm=Inf"), 
                      "VERB_inf", from_pos),
         from_pos = ifelse(str_detect(from_feats, "Mood=Imp"), 
                      "VERB_imp", from_pos)) %>% 
  mutate(to_pos = ifelse(str_detect(to_feats, "VerbForm=Inf"), 
                      "VERB_inf", to_pos),
         to_pos = ifelse(str_detect(to_feats, "Mood=Imp"), 
                      "VERB_imp", to_pos))

rnc_rhymes %>% 
  select(poem_id) %>% 
  separate(poem_id, into = c("corpus", "id", "year"), sep = "_") %>% 
  count(year) %>% 
  ggplot(aes(x = year, y = n)) + geom_col()
```

![](05_4_rhyme_morhp_pairs.markdown_strict_files/figure-markdown_strict/unnamed-chunk-24-1.png)

### masc

Filter only masculine rhymes

``` r
rnc_masc <- rnc_rhymes %>% 
  filter(from_closure == "masc" & to_closure == "masc")

nrow(rnc_masc)
```

    [1] 16593

Iamb

``` r
rnc_iamb_masc <- rnc_masc %>% 
  filter(meter == "Я")

t <- nrow(rnc_iamb_masc)
t
```

    [1] 14621

Syllable variation

``` r
rnc_iamb_count_syl <- rnc_iamb_masc %>% 
  mutate(# calculate number of syllables in each word
         from_n_syl = nchar(from_sp),
         to_n_syl = nchar(to_sp),
         
         # attach n_syl to pos
         from_pos_syl = paste0(from_pos, "_", from_n_syl),
         to_pos_syl = paste0(to_pos, "_", to_n_syl)) %>% 
  rowwise() %>% 
  mutate(pos_pair = paste0(sort(c(from_pos_syl, to_pos_syl)), collapse = " -- ")
         ) %>%
  ungroup() %>% 
  count(pos_pair, sort = T) %>% 
  mutate(perc = round( (n/t)*100, 2 )) %>% 
  rename(rnc_n_iamb = n,
         rnc_perc_iamb = perc) %>% 
  mutate(rnc_rank_iamb = row_number())

rnc_iamb_count_syl %>% 
  head(10)
```

    # A tibble: 10 × 4
       pos_pair                 rnc_n_iamb rnc_perc_iamb rnc_rank_iamb
       <chr>                         <int>         <dbl>         <int>
     1 NOUN_2 -- NOUN_2               1573         10.8              1
     2 NOUN_1 -- NOUN_2               1143          7.82             2
     3 NOUN_1 -- NOUN_1               1075          7.35             3
     4 NOUN_2 -- NOUN_3                794          5.43             4
     5 VERB_inf_2 -- VERB_inf_3        493          3.37             5
     6 VERB_2 -- VERB_3                473          3.24             6
     7 VERB_2 -- VERB_2                438          3                7
     8 NOUN_2 -- PRON_1                342          2.34             8
     9 NOUN_1 -- NOUN_3                304          2.08             9
    10 VERB_inf_3 -- VERB_inf_3        295          2.02            10

trochee

``` r
rnc_tr_masc <- rnc_masc %>% 
  filter(meter == "Х")

t <- nrow(rnc_tr_masc)
t
```

    [1] 1972

``` r
rnc_tr_count_syl <- rnc_tr_masc %>% 
  mutate(# calculate number of syllables in each word
         from_n_syl = nchar(from_sp),
         to_n_syl = nchar(to_sp),
         
         # attach n_syl to pos
         from_pos_syl = paste0(from_pos, "_", from_n_syl),
         to_pos_syl = paste0(to_pos, "_", to_n_syl)) %>% 
  rowwise() %>% 
  mutate(pos_pair = paste0(sort(c(from_pos_syl, to_pos_syl)), collapse = " -- ")
         ) %>%
  ungroup() %>% 
  count(pos_pair, sort = T) %>% 
  mutate(perc = round( (n/t)*100, 2 )) %>% 
  rename(rnc_n_tr = n,
         rnc_perc_tr = perc) %>% 
  mutate(rnc_rank_tr = row_number())

rnc_tr_count_syl %>% 
  head(10)
```

    # A tibble: 10 × 4
       pos_pair                 rnc_n_tr rnc_perc_tr rnc_rank_tr
       <chr>                       <int>       <dbl>       <int>
     1 NOUN_2 -- NOUN_2              206       10.4            1
     2 NOUN_1 -- NOUN_2              128        6.49           2
     3 NOUN_1 -- NOUN_1              114        5.78           3
     4 NOUN_2 -- NOUN_3              113        5.73           4
     5 VERB_2 -- VERB_3               73        3.7            5
     6 VERB_inf_2 -- VERB_inf_3       66        3.35           6
     7 VERB_2 -- VERB_2               63        3.19           7
     8 NOUN_2 -- PRON_2               50        2.54           8
     9 NOUN_3 -- NOUN_3               48        2.43           9
    10 NOUN_2 -- PRON_1               45        2.28          10

Comparison

``` r
rnc_iamb_count_syl %>% 
  left_join(rnc_tr_count_syl, by = "pos_pair") %>% 
  select(pos_pair, rnc_rank_iamb, rnc_rank_tr, rnc_perc_iamb, 
         rnc_perc_tr, rnc_n_iamb, rnc_n_tr) %>% 
  head(20)
```

    # A tibble: 20 × 7
       pos_pair       rnc_rank_iamb rnc_rank_tr rnc_perc_iamb rnc_perc_tr rnc_n_iamb
       <chr>                  <int>       <int>         <dbl>       <dbl>      <int>
     1 NOUN_2 -- NOU…             1           1         10.8        10.4        1573
     2 NOUN_1 -- NOU…             2           2          7.82        6.49       1143
     3 NOUN_1 -- NOU…             3           3          7.35        5.78       1075
     4 NOUN_2 -- NOU…             4           4          5.43        5.73        794
     5 VERB_inf_2 --…             5           6          3.37        3.35        493
     6 VERB_2 -- VER…             6           5          3.24        3.7         473
     7 VERB_2 -- VER…             7           7          3           3.19        438
     8 NOUN_2 -- PRO…             8          10          2.34        2.28        342
     9 NOUN_1 -- NOU…             9          19          2.08        1.42        304
    10 VERB_inf_3 --…            10          20          2.02        1.37        295
    11 VERB_3 -- VER…            11          21          1.94        1.32        283
    12 DET_2 -- NOUN…            12          14          1.78        1.93        260
    13 VERB_inf_2 --…            13          16          1.77        1.88        259
    14 NOUN_1 -- VER…            14          17          1.65        1.72        241
    15 VERB_1 -- VER…            15          28          1.58        0.91        231
    16 NOUN_1 -- PRO…            16          11          1.52        2.08        222
    17 VERB_inf_1 --…            17          15          1.47        1.88        215
    18 DET_2 -- PRON…            18          13          1.45        1.98        212
    19 NOUN_3 -- NOU…            19           9          1.42        2.43        208
    20 NOUN_1 -- VER…            20          12          1.4         2.03        205
    # ℹ 1 more variable: rnc_n_tr <int>

``` r
rnc_masc_ranks <- rnc_iamb_count_syl %>% 
  left_join(rnc_tr_count_syl, by = "pos_pair") %>% 
  select(pos_pair, rnc_rank_iamb, rnc_rank_tr, rnc_perc_iamb, 
         rnc_perc_tr, rnc_n_iamb, rnc_n_tr) %>%
  drop_na() 

cor(rnc_masc_ranks$rnc_rank_iamb[1:100], rnc_masc_ranks$rnc_rank_tr[1:100], method = "kendall")
```

    [1] 0.7183838

### fem rhymes

Same steps for feminine rhymes

``` r
rnc_fem <- rnc_rhymes %>% 
  filter(from_closure == "fem" & to_closure == "fem")

nrow(rnc_fem)
```

    [1] 17615

Iamb

``` r
rnc_iamb_fem <- rnc_fem %>% 
  filter(meter == "Я")

t <- nrow(rnc_iamb_fem)
t
```

    [1] 15452

``` r
rnc_fem_iamb_count_syl <- rnc_iamb_fem %>% 
  mutate(# calculate number of syllables in each word
         from_n_syl = nchar(from_sp),
         to_n_syl = nchar(to_sp),
         
         # attach n_syl to pos
         from_pos_syl = paste0(from_pos, "_", from_n_syl),
         to_pos_syl = paste0(to_pos, "_", to_n_syl)) %>% 
  rowwise() %>% 
  mutate(pos_pair = paste0(sort(c(from_pos_syl, to_pos_syl)), collapse = " -- ")
         ) %>%
  ungroup() %>% 
  count(pos_pair, sort = T) %>% 
  mutate(perc = round( (n/t)*100, 2 )) %>% 
  rename(rnc_n_iamb = n,
         rnc_perc_iamb = perc) %>% 
  mutate(rnc_rank_iamb = row_number())

rnc_fem_iamb_count_syl %>% 
  head(10)
```

    # A tibble: 10 × 4
       pos_pair         rnc_n_iamb rnc_perc_iamb rnc_rank_iamb
       <chr>                 <int>         <dbl>         <int>
     1 VERB_3 -- VERB_4       1516          9.81             1
     2 NOUN_2 -- NOUN_3       1461          9.46             2
     3 NOUN_3 -- NOUN_3       1409          9.12             3
     4 VERB_4 -- VERB_4       1223          7.91             4
     5 NOUN_2 -- NOUN_2        958          6.2              5
     6 NOUN_3 -- NOUN_4        865          5.6              6
     7 VERB_3 -- VERB_3        785          5.08             7
     8 ADJ_3 -- ADJ_3          418          2.71             8
     9 VERB_2 -- VERB_3        415          2.69             9
    10 ADJ_2 -- ADJ_3          312          2.02            10

Trochee

``` r
rnc_tr_fem <- rnc_fem %>% 
  filter(meter == "Х")

t <- nrow(rnc_tr_fem)
t # very small number of trochee feminine examples!
```

    [1] 2163

``` r
rnc_fem_tr_count_syl <- rnc_tr_fem %>% 
  mutate(# calculate number of syllables in each word
         from_n_syl = nchar(from_sp),
         to_n_syl = nchar(to_sp),
         
         # attach n_syl to pos
         from_pos_syl = paste0(from_pos, "_", from_n_syl),
         to_pos_syl = paste0(to_pos, "_", to_n_syl)) %>% 
  rowwise() %>% 
  mutate(pos_pair = paste0(sort(c(from_pos_syl, to_pos_syl)), collapse = " -- ")
         ) %>%
  ungroup() %>% 
  count(pos_pair, sort = T) %>% 
  mutate(perc = round( (n/t)*100, 2 )) %>% 
  rename(rnc_n_tr = n,
         rnc_perc_tr = perc) %>% 
  mutate(rnc_rank_tr = row_number())

rnc_tr_count_syl %>% 
  head(10)
```

    # A tibble: 10 × 4
       pos_pair                 rnc_n_tr rnc_perc_tr rnc_rank_tr
       <chr>                       <int>       <dbl>       <int>
     1 NOUN_2 -- NOUN_2              206       10.4            1
     2 NOUN_1 -- NOUN_2              128        6.49           2
     3 NOUN_1 -- NOUN_1              114        5.78           3
     4 NOUN_2 -- NOUN_3              113        5.73           4
     5 VERB_2 -- VERB_3               73        3.7            5
     6 VERB_inf_2 -- VERB_inf_3       66        3.35           6
     7 VERB_2 -- VERB_2               63        3.19           7
     8 NOUN_2 -- PRON_2               50        2.54           8
     9 NOUN_3 -- NOUN_3               48        2.43           9
    10 NOUN_2 -- PRON_1               45        2.28          10

Comparison

``` r
rnc_fem_iamb_count_syl %>% 
  left_join(rnc_fem_tr_count_syl, by = "pos_pair") %>%
  select(pos_pair, rnc_rank_iamb, rnc_rank_tr, rnc_perc_iamb, 
         rnc_perc_tr, rnc_n_iamb, rnc_n_tr) %>% 
  head(20)
```

    # A tibble: 20 × 7
       pos_pair       rnc_rank_iamb rnc_rank_tr rnc_perc_iamb rnc_perc_tr rnc_n_iamb
       <chr>                  <int>       <int>         <dbl>       <dbl>      <int>
     1 VERB_3 -- VER…             1           3          9.81        9.62       1516
     2 NOUN_2 -- NOU…             2           1          9.46       11.6        1461
     3 NOUN_3 -- NOU…             3           2          9.12       10.0        1409
     4 VERB_4 -- VER…             4           5          7.91        6.1        1223
     5 NOUN_2 -- NOU…             5           4          6.2         7.49        958
     6 NOUN_3 -- NOU…             6           7          5.6         4.25        865
     7 VERB_3 -- VER…             7           6          5.08        5.13        785
     8 ADJ_3 -- ADJ_3             8           8          2.71        3.05        418
     9 VERB_2 -- VER…             9          10          2.69        2.17        415
    10 ADJ_2 -- ADJ_3            10           9          2.02        2.68        312
    11 NOUN_2 -- NOU…            11          14          1.88        1.29        290
    12 VERB_2 -- VER…            12          13          1.74        1.43        269
    13 NOUN_4 -- NOU…            13          19          1.66        0.97        257
    14 ADJ_3 -- NOUN…            14          16          1.6         1.16        247
    15 ADJ_3 -- ADJ_4            15          11          1.56        1.76        241
    16 VERB_4 -- VER…            16          26          1.35        0.69        209
    17 NOUN_3 -- VER…            17          18          1.13        0.97        175
    18 NOUN_3 -- VER…            18          12          1.12        1.71        173
    19 NOUN_3 -- PRO…            19          17          0.89        0.97        137
    20 VERB_2 -- VER…            20          20          0.88        0.97        136
    # ℹ 1 more variable: rnc_n_tr <int>

``` r
rnc_fem_ranks <- rnc_fem_iamb_count_syl %>% 
  left_join(rnc_fem_tr_count_syl, by = "pos_pair") %>% 
  select(pos_pair, rnc_rank_iamb, rnc_rank_tr, rnc_perc_iamb, 
         rnc_perc_tr, rnc_n_iamb, rnc_n_tr) %>%
  drop_na() 

cor(rnc_fem_ranks$rnc_rank_iamb[1:100], rnc_fem_ranks$rnc_rank_tr[1:100], method = "kendall")
```

    [1] 0.679596

## compare C35 & RNC

Masc rhymes

``` r
glimpse(ranks_masc) # corpus-1835 
```

    Rows: 249
    Columns: 7
    $ pos_pair  <chr> "NOUN_2 -- NOUN_2", "NOUN_1 -- NOUN_2", "NOUN_2 -- NOUN_3", …
    $ rank_iamb <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 1…
    $ rank_tr   <int> 1, 3, 2, 4, 7, 6, 5, 8, 11, 9, 13, 12, 14, 19, 15, 10, 16, 1…
    $ perc_iamb <dbl> 11.75, 7.63, 6.93, 5.50, 4.17, 3.98, 3.77, 3.48, 2.18, 2.09,…
    $ perc_tr   <dbl> 12.39, 6.97, 8.06, 5.55, 4.22, 4.23, 4.39, 2.91, 2.04, 2.30,…
    $ n_iamb    <int> 2941, 1909, 1735, 1377, 1045, 996, 943, 872, 546, 524, 475, …
    $ n_tr      <int> 873, 491, 568, 391, 297, 298, 309, 205, 144, 162, 117, 127, …

``` r
glimpse(rnc_masc_ranks) # rnc
```

    Rows: 189
    Columns: 7
    $ pos_pair      <chr> "NOUN_2 -- NOUN_2", "NOUN_1 -- NOUN_2", "NOUN_1 -- NOUN_…
    $ rnc_rank_iamb <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 1…
    $ rnc_rank_tr   <int> 1, 2, 3, 4, 6, 5, 7, 10, 19, 20, 21, 14, 16, 17, 28, 11,…
    $ rnc_perc_iamb <dbl> 10.76, 7.82, 7.35, 5.43, 3.37, 3.24, 3.00, 2.34, 2.08, 2…
    $ rnc_perc_tr   <dbl> 10.45, 6.49, 5.78, 5.73, 3.35, 3.70, 3.19, 2.28, 1.42, 1…
    $ rnc_n_iamb    <int> 1573, 1143, 1075, 794, 493, 473, 438, 342, 304, 295, 283…
    $ rnc_n_tr      <int> 206, 128, 114, 113, 66, 73, 63, 45, 28, 27, 26, 38, 37, …

``` r
all_masc_ranks <- ranks_masc %>% 
  left_join(rnc_masc_ranks, by = "pos_pair") %>% 
  select(pos_pair, 
         rank_iamb, rnc_rank_iamb, rank_tr, rnc_rank_tr,
         perc_iamb, rnc_perc_iamb, perc_tr, rnc_perc_tr,
         n_iamb, rnc_n_iamb, n_tr, rnc_n_tr
         )

head(all_masc_ranks, 20)
```

    # A tibble: 20 × 13
       pos_pair  rank_iamb rnc_rank_iamb rank_tr rnc_rank_tr perc_iamb rnc_perc_iamb
       <chr>         <int>         <int>   <int>       <int>     <dbl>         <dbl>
     1 NOUN_2 -…         1             1       1           1     11.8          10.8 
     2 NOUN_1 -…         2             2       3           2      7.63          7.82
     3 NOUN_2 -…         3             4       2           4      6.93          5.43
     4 NOUN_1 -…         4             3       4           3      5.5           7.35
     5 NOUN_2 -…         5            21       7           8      4.17          1.4 
     6 VERB_2 -…         6             6       6           5      3.98          3.24
     7 VERB_2 -…         7             7       5           7      3.77          3   
     8 NOUN_2 -…         8             8       8          10      3.48          2.34
     9 VERB_3 -…         9            11      11          21      2.18          1.94
    10 ADJ_2 --…        10            25       9          26      2.09          0.91
    11 NOUN_3 -…        11            31      13          24      1.9           0.66
    12 NOUN_3 -…        12            19      12           9      1.89          1.42
    13 NOUN_1 -…        13             9      14          19      1.86          2.08
    14 PRON_1 -…        14            29      19          25      1.81          0.75
    15 NOUN_1 -…        15            16      15          11      1.72          1.52
    16 NOUN_2 -…        16            22      10          23      1.64          1.29
    17 PRON_2 -…        17            23      16          18      1.4           1.28
    18 NOUN_1 -…        18            14      18          17      1.34          1.65
    19 VERB_inf…        19             5      20           6      1.27          3.37
    20 VERB_1 -…        20            15      21          28      1.25          1.58
    # ℹ 6 more variables: perc_tr <dbl>, rnc_perc_tr <dbl>, n_iamb <int>,
    #   rnc_n_iamb <int>, n_tr <int>, rnc_n_tr <int>

``` r
# drop na for cor test
r <- all_masc_ranks %>% drop_na()

cor(r$rank_iamb[1:90], r$rnc_rank_iamb[1:90],
    method = "kendall")
```

    [1] 0.6509363

``` r
cor(r$rank_tr[1:90], r$rnc_rank_tr[1:90], method = "kendall")
```

    [1] 0.6589263

Fem rhymes

``` r
all_fem_ranks <- fem_ranks %>% 
  left_join(rnc_fem_ranks, by = "pos_pair") %>% 
  select(pos_pair, 
         rank_iamb, rnc_rank_iamb, rank_tr, rnc_rank_tr,
         perc_iamb, rnc_perc_iamb, perc_tr, rnc_perc_tr,
         n_iamb, rnc_n_iamb, n_tr, rnc_n_tr
         )

head(all_fem_ranks, 20)
```

    # A tibble: 20 × 13
       pos_pair  rank_iamb rnc_rank_iamb rank_tr rnc_rank_tr perc_iamb rnc_perc_iamb
       <chr>         <int>         <int>   <int>       <int>     <dbl>         <dbl>
     1 NOUN_3 -…         1             3       1           2     12.6           9.12
     2 NOUN_2 -…         2             2       2           1     10.0           9.46
     3 NOUN_3 -…         3             6       4           7      7.97          5.6 
     4 NOUN_2 -…         4             5       3           4      6.02          6.2 
     5 VERB_3 -…         5             1       5           3      5.81          9.81
     6 VERB_4 -…         6             4       9           5      4.29          7.91
     7 VERB_3 -…         7             7       8           6      3.67          5.08
     8 ADJ_3 --…         8             8       6           8      3.48          2.71
     9 ADJ_3 --…         9            15       7          11      3.31          1.56
    10 VERB_2 -…        10             9      10          10      2.5           2.69
    11 NOUN_4 -…        11            13      15          19      2.49          1.66
    12 ADJ_2 --…        12            10      11           9      2.29          2.02
    13 NOUN_2 -…        13            11      13          14      2.06          1.88
    14 ADJ_3 --…        14            21      12          21      1.57          0.8 
    15 NOUN_3 -…        15            22      21          45      1.43          0.69
    16 ADJ_3 --…        16            14      14          16      1.3           1.6 
    17 VERB_2 -…        17            12      16          13      1.22          1.74
    18 ADJ_4 --…        18            36      18          86      1.09          0.38
    19 NOUN_4 -…        19            27      26          36      1.01          0.56
    20 NOUN_3 -…        20            32      20          23      1             0.5 
    # ℹ 6 more variables: perc_tr <dbl>, rnc_perc_tr <dbl>, n_iamb <int>,
    #   rnc_n_iamb <int>, n_tr <int>, rnc_n_tr <int>

``` r
# drop na for cor test
r <- all_fem_ranks %>% drop_na()

cor(r$rank_iamb[1:90], r$rnc_rank_iamb[1:90],
    method = "kendall")
```

    [1] 0.6459426

``` r
cor(r$rank_tr[1:90], r$rnc_rank_tr[1:90], method = "kendall")
```

    [1] 0.6833958

### network viz

Network:

from, to, corpus, meter, clausula(?)

NOUN_2 - NOUN_3 , filter n = 1

different network for different meters & periods =\> see which pos do
not meet each other in rhyme & which syl numbers are more freq

mb facet wrap for nkrja / c1835 + diff edges/colours for trochee/iamb

``` r
library(tidygraph)
```


    Attaching package: 'tidygraph'

    The following object is masked from 'package:stats':

        filter

``` r
library(ggraph)

glimpse(all_masc_ranks)
```

    Rows: 249
    Columns: 13
    $ pos_pair      <chr> "NOUN_2 -- NOUN_2", "NOUN_1 -- NOUN_2", "NOUN_2 -- NOUN_…
    $ rank_iamb     <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 1…
    $ rnc_rank_iamb <int> 1, 2, 4, 3, 21, 6, 7, 8, 11, 25, 31, 19, 9, 29, 16, 22, …
    $ rank_tr       <int> 1, 3, 2, 4, 7, 6, 5, 8, 11, 9, 13, 12, 14, 19, 15, 10, 1…
    $ rnc_rank_tr   <int> 1, 2, 4, 3, 8, 5, 7, 10, 21, 26, 24, 9, 19, 25, 11, 23, …
    $ perc_iamb     <dbl> 11.75, 7.63, 6.93, 5.50, 4.17, 3.98, 3.77, 3.48, 2.18, 2…
    $ rnc_perc_iamb <dbl> 10.76, 7.82, 5.43, 7.35, 1.40, 3.24, 3.00, 2.34, 1.94, 0…
    $ perc_tr       <dbl> 12.39, 6.97, 8.06, 5.55, 4.22, 4.23, 4.39, 2.91, 2.04, 2…
    $ rnc_perc_tr   <dbl> 10.45, 6.49, 5.73, 5.78, 2.54, 3.70, 3.19, 2.28, 1.32, 0…
    $ n_iamb        <int> 2941, 1909, 1735, 1377, 1045, 996, 943, 872, 546, 524, 4…
    $ rnc_n_iamb    <int> 1573, 1143, 794, 1075, 204, 473, 438, 342, 283, 133, 97,…
    $ n_tr          <int> 873, 491, 568, 391, 297, 298, 309, 205, 144, 162, 117, 1…
    $ rnc_n_tr      <int> 206, 128, 113, 114, 50, 73, 63, 45, 26, 19, 22, 48, 28, …

``` r
summary(all_masc_ranks)
```

       pos_pair           rank_iamb     rnc_rank_iamb       rank_tr     
     Length:249         Min.   :  1.0   Min.   :  1.00   Min.   :  1.0  
     Class :character   1st Qu.: 63.0   1st Qu.: 31.50   1st Qu.: 63.0  
     Mode  :character   Median :125.0   Median : 71.00   Median :125.0  
                        Mean   :142.4   Mean   : 88.79   Mean   :129.7  
                        3rd Qu.:210.0   3rd Qu.:126.00   3rd Qu.:195.0  
                        Max.   :392.0   Max.   :399.00   Max.   :273.0  
                                        NA's   :142                     
      rnc_rank_tr      perc_iamb       rnc_perc_iamb        perc_tr       
     Min.   :  1.0   Min.   : 0.0000   Min.   : 0.0100   Min.   : 0.0100  
     1st Qu.: 31.5   1st Qu.: 0.0200   1st Qu.: 0.0700   1st Qu.: 0.0100  
     Median : 72.0   Median : 0.0600   Median : 0.1900   Median : 0.0600  
     Mean   : 83.0   Mean   : 0.3958   Mean   : 0.7755   Mean   : 0.3989  
     3rd Qu.:122.0   3rd Qu.: 0.2200   3rd Qu.: 0.6200   3rd Qu.: 0.2300  
     Max.   :205.0   Max.   :11.7500   Max.   :10.7600   Max.   :12.3900  
     NA's   :142                       NA's   :142                        
      rnc_perc_tr          n_iamb          rnc_n_iamb          n_tr       
     Min.   : 0.0500   Min.   :   1.00   Min.   :   1.0   Min.   :  1.00  
     1st Qu.: 0.0500   1st Qu.:   4.00   1st Qu.:  10.0   1st Qu.:  1.00  
     Median : 0.1500   Median :  14.00   Median :  28.0   Median :  4.00  
     Mean   : 0.7739   Mean   :  99.11   Mean   : 113.4   Mean   : 28.17  
     3rd Qu.: 0.7600   3rd Qu.:  56.00   3rd Qu.:  91.0   3rd Qu.: 16.00  
     Max.   :10.4500   Max.   :2941.00   Max.   :1573.0   Max.   :873.00  
     NA's   :142                         NA's   :142                      
        rnc_n_tr     
     Min.   :  1.00  
     1st Qu.:  1.00  
     Median :  3.00  
     Mean   : 15.28  
     3rd Qu.: 15.00  
     Max.   :206.00  
     NA's   :142     

Nodes size - calculate pos_syl freq in the subsets

``` r
#glimpse(rhyme_pairs)

# w1 <- rhyme_pairs %>% 
#   select(from_pos, from_sp) %>% 
#   mutate(from_n_syl = nchar(from_sp),
#          pos_syl = paste0(from_pos, "_", from_n_syl)) %>% 
#   select(pos_syl)
# 
# w2 <- rhyme_pairs %>% 
#   select(to_pos, to_sp) %>% 
#   mutate(to_n_syl = nchar(to_sp),
#          pos_syl = paste0(to_pos, "_", to_n_syl)) %>% 
#   select(pos_syl)
# 
# w <- rbind(w1, w2) %>% 
#   count(pos_syl, sort = T)
```

``` r
t <- all_masc_ranks %>% 
  # filter(n_iamb > 5,
  #        n_tr > 5,
  #        rnc_n_iamb > 5,
  #        rnc_n_tr > 5) %>%
  filter(rank_iamb < 100,
         rnc_rank_iamb < 100,
         rank_tr < 100,
         rnc_rank_tr < 100) %>% 
  select(pos_pair, perc_iamb, perc_tr, rnc_perc_iamb, rnc_perc_tr) %>% 
  pivot_longer(!pos_pair, names_to = "corpus", values_to = "value") %>% 
  #filter(value > 0.3) %>% # avg freq
  separate(pos_pair, into = c("from", "to"), sep = " -- ") %>% 
  drop_na()

# unique(t$corpus)

labels <- tibble(corpus = unique(t$corpus),
                 corpus_r = c("Корпус-1835, ямбы",
                           "Корпус-1835, хореи",
                           "НКРЯ до 1810, ямбы",
                           "НКРЯ до 1810, хореи"))

t <- t %>% left_join(labels, by = "corpus")

edgelist <- t %>% drop_na()
nodelist <- tibble(source = unique(c(t$from, t$to))) %>% 
  mutate(ids = row_number()) # %>% 
  #left_join(w %>% rename(source = pos_syl), by = "source")

net_t <- tbl_graph(nodes = nodelist,
                   edges = edgelist,
                   directed = FALSE)

net_t %>% 
  ggraph(layout = 'linear', circular = TRUE) +
  #ggraph(layout = "kk") + 
  geom_edge_arc(aes(width = value, #color = corpus, 
                     color = value,
                     alpha = value)) + 
  geom_node_point(#aes(size = n)
    color = "#440154FF"
    ) + 
  geom_node_text(aes(label = source), vjust = -0.5) + 
  facet_wrap(~corpus_r) + 
  scale_edge_color_continuous(low = "#440154FF", high = "#FDE725FF") + 
  theme(legend.position = "None",
        text = element_text(size = 20))
```

    Warning: Using the `size` aesthetic in this geom was deprecated in ggplot2 3.4.0.
    ℹ Please use `linewidth` in the `default_aes` field and elsewhere instead.

![](05_4_rhyme_morhp_pairs.markdown_strict_files/figure-markdown_strict/unnamed-chunk-40-1.png)

Female

``` r
t <- all_fem_ranks %>% 
  # filter(n_iamb > 5,
  #        n_tr > 5,
  #        rnc_n_iamb > 5,
  #        rnc_n_tr > 5) %>%
    filter(rank_iamb < 100,
         rnc_rank_iamb < 100,
         rank_tr < 100,
         rnc_rank_tr < 100) %>% 
  select(pos_pair, perc_iamb, perc_tr, rnc_perc_iamb, rnc_perc_tr) %>% 
  pivot_longer(!pos_pair, names_to = "corpus", values_to = "value") %>% 
  #filter(value > 0.3) %>% # avg freq
  separate(pos_pair, into = c("from", "to"), sep = " -- ") %>% 
  drop_na()

labels <- tibble(corpus = unique(t$corpus),
                 corpus_r = c("Корпус-1835, ямбы",
                           "Корпус-1835, хореи",
                           "НКРЯ до 1810, ямбы",
                           "НКРЯ до 1810, хореи"))

t <- t %>% left_join(labels, by = "corpus")

edgelist <- t %>% drop_na()
nodelist <- tibble(source = unique(c(t$from, t$to))) %>% 
  mutate(ids = row_number()) # %>% 
  #left_join(w %>% rename(source = pos_syl), by = "source")

net_t <- tbl_graph(nodes = nodelist,
                   edges = edgelist,
                   directed = FALSE)

net_t %>% 
  ggraph(layout = 'linear', circular = TRUE) +
  #ggraph(layout = "kk") + 
  geom_edge_arc(aes(width = value, #color = corpus, 
                     color = value,
                     alpha = value)) + 
  geom_node_point(#aes(size = n)
    color = "#440154FF"
    ) + 
  geom_node_text(aes(label = source), vjust = -0.5) + 
  facet_wrap(~corpus_r) + 
  scale_edge_color_continuous(low = "#440154FF", high = "#FDE725FF") + 
  theme(legend.position = "None",
        text = element_text(size = 20))
```

![](05_4_rhyme_morhp_pairs.markdown_strict_files/figure-markdown_strict/unnamed-chunk-41-1.png)

## endings

### POS pairs variability

#### masc

Masculine (all meters)

``` r
glimpse(masc_pairs)
```

    Rows: 38,011
    Columns: 16
    $ text_id      <chr> "P_1938", "P_1938", "C_156__20", "C_156__20", "C_156__20"…
    $ from         <chr> "краса", "огневым", "силки", "спор", "сном", "твоя", "тиш…
    $ to           <chr> "небеса", "земным", "легки", "простор", "лучом", "я", "по…
    $ rhyme_alph   <chr> "краса небеса", "земным огневым", "легки силки", "простор…
    $ meter        <chr> "Other", "Other", "Trochee", "Trochee", "Trochee", "Iamb"…
    $ feet         <chr> "other", "other", "4", "4", "4", "4", "4", "4", "4", "4",…
    $ from_closure <chr> "masc", "masc", "masc", "masc", "masc", "masc", "masc", "…
    $ from_pos     <chr> "NOUN", "NOUN", "NOUN", "NOUN", "NOUN", "PRON", "NOUN", "…
    $ from_ending  <chr> "са'", "ы'м", "ки'", "о'р", "о'м", "оя'", "не'", "ё'т", "…
    $ from_feats   <chr> "S,жен,неод=им,ед", "S,фам,муж,од=(дат,мн|твор,ед)", "S,м…
    $ from_sp      <chr> "01", "001", "01", "1", "1", "01", "001", "01", "01", "01…
    $ to_closure   <chr> "masc", "masc", "masc", "masc", "masc", "masc", "masc", "…
    $ to_pos       <chr> "NOUN", "ADJ", "ADJ", "NOUN", "NOUN", "PRON", "NOUN", "VE…
    $ to_ending    <chr> "са'", "ы'м", "ки'", "о'р", "о'м", "я'", "не'", "ё'т", "и…
    $ to_feats     <chr> "S,сред,неод=(вин,мн|им,мн)", "A=(дат,мн,полн|твор,ед,пол…
    $ to_sp        <chr> "001", "01", "01", "01", "01", "1", "001", "01", "001", "…

``` r
# total number of masc pairs
t <- nrow(masc_pairs)

# view 30 mf ending pairs
masc_pairs %>% 
  mutate(ending_pair = paste0("-", from_ending, " -- -", to_ending)) %>% 
  count(ending_pair, sort = T) %>% 
  head(30) %>% 
  mutate(perc = round((n/t)*100, 2))
```

          ending_pair    n  perc
    1    -о'й -- -о'й 3802 10.00
    2    -е'й -- -е'й 2212  5.82
    3    -а'л -- -а'л 1241  3.26
    4    -на' -- -на'  964  2.54
    5    -и'т -- -и'т  886  2.33
    6    -о'в -- -о'в  858  2.26
    7  -а'ть -- -а'ть  739  1.94
    8    -и'л -- -и'л  681  1.79
    9    -о'м -- -о'м  672  1.77
    10   -е'т -- -е'т  614  1.62
    11   -а'м -- -а'м  613  1.61
    12   -ты' -- -ты'  609  1.60
    13   -ё'т -- -ё'т  589  1.55
    14   -а'х -- -а'х  575  1.51
    15   -не' -- -не'  562  1.48
    16   -ла' -- -ла'  465  1.22
    17   -о'н -- -о'н  439  1.15
    18 -и'ть -- -и'ть  381  1.00
    19   -е'ц -- -е'ц  374  0.98
    20   -о'р -- -о'р  367  0.97
    21   -е'л -- -е'л  362  0.95
    22   -а'с -- -а'с  326  0.86
    23   -ня' -- -ня'  289  0.76
    24   -о'к -- -о'к  284  0.75
    25   -да' -- -да'  281  0.74
    26   -ка' -- -ка'  270  0.71
    27   -го' -- -го'  261  0.69
    28   -а'н -- -а'н  244  0.64
    29   -ны' -- -ны'  237  0.62
    30   -но' -- -но'  218  0.57

``` r
# pull top ending pairs 
m_ranks <- masc_pairs %>% 
  mutate(ending_pair = paste0("-", from_ending, " -- -", to_ending)) %>% 
  count(ending_pair, sort = T) %>% 
  #head(1000) %>% 
  mutate(rank = row_number()) %>% select(-n)

# count POS variants inside every ending
m_ranked_pos <- masc_pairs %>% 
  mutate(ending_pair = paste0("-", from_ending, " -- -", to_ending), 
         pos_pair = paste0(from_pos, " -- ", to_pos)) %>% 
  #filter(ending_pair %in% rankss$ending_pair) %>% 
  group_by(ending_pair) %>% 
  count(pos_pair, sort = F) %>% 
  count(ending_pair, sort = T) %>% 
  left_join(m_ranks, by = "ending_pair") %>% 
  filter(rank < 101) %>% 
  mutate(label = paste0(rank, " ", ending_pair),
         group = ifelse(n < 9, "less variation", "more variation")) 

head(m_ranked_pos, 15)
```

    # A tibble: 15 × 5
    # Groups:   ending_pair [15]
       ending_pair          n  rank label               group         
       <chr>            <int> <int> <chr>               <chr>         
     1 -о'й -- -о'й        29     1 1 -о'й -- -о'й      more variation
     2 -и'м -- -и'м        28    35 35 -и'м -- -и'м     more variation
     3 -е'й -- -е'й        24     2 2 -е'й -- -е'й      more variation
     4 -ё'м -- -ё'м        24    82 82 -ё'м -- -ё'м     more variation
     5 -но' -- -но'        18    30 30 -но' -- -но'     more variation
     6 -да' -- -да'        17    25 25 -да' -- -да'     more variation
     7 -на' -- -на'        17     4 4 -на' -- -на'      more variation
     8 -о'м -- -о'м        17     9 9 -о'м -- -о'м      more variation
     9 -е'сть -- -е'сть    16    75 75 -е'сть -- -е'сть more variation
    10 -и'х -- -и'х        16    31 31 -и'х -- -и'х     more variation
    11 -а'м -- -а'м        13    11 11 -а'м -- -а'м     more variation
    12 -а'с -- -а'с        13    22 22 -а'с -- -а'с     more variation
    13 -е'т -- -е'т        13    10 10 -е'т -- -е'т     more variation
    14 -ли' -- -ли'        13    32 32 -ли' -- -ли'     more variation
    15 -а'ть -- -а'ть      12     7 7 -а'ть -- -а'ть    more variation

``` r
summary(m_ranked_pos$n) # 9.25 is 3rd Qu. if 100 first ranks taken
```

       Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
       1.00    4.00    7.00    7.76    9.25   29.00 

``` r
# plot
m_ranked_pos %>% 
  filter(rank < 51) %>% 
  ggplot(aes(x = rank, y = n, fill = group)) + geom_col(alpha = 0.7) + 
  #scale_x_continuous(breaks = im_rank_feat$rank, labels = im_rank_feat$label) + 
  #theme(axis.text.x = element_text(angle = 270)) + 
  coord_flip() + 
  scale_x_reverse(breaks = m_ranked_pos$rank, 
                     labels = m_ranked_pos$label) + 
  scale_fill_manual(values = c(met.brewer("Veronese")[6],
                               met.brewer("Veronese")[3]))
```

![](05_4_rhyme_morhp_pairs.markdown_strict_files/figure-markdown_strict/unnamed-chunk-42-1.png)

Analysis of the less variate pairs

``` r
m_ranked_pos %>% 
  filter(rank < 51) %>% 
  summary(n) # look into less than 1st quatrain, ie less than 4 pos pairs
```

     ending_pair              n              rank          label          
     Length:50          Min.   : 1.00   Min.   : 1.00   Length:50         
     Class :character   1st Qu.: 4.25   1st Qu.:13.25   Class :character  
     Mode  :character   Median : 8.00   Median :25.50   Mode  :character  
                        Mean   : 9.18   Mean   :25.50                     
                        3rd Qu.:12.00   3rd Qu.:37.75                     
                        Max.   :29.00   Max.   :50.00                     
        group          
     Length:50         
     Class :character  
     Mode  :character  
                       
                       
                       

``` r
less_var_pos <- m_ranked_pos %>% 
  filter(rank < 51 & n < 5
           #group == "less variation"
           ) %>% 
  arrange(-desc(rank)) %>% pull(ending_pair)

length(less_var_pos)
```

    [1] 13

``` r
masc_pairs %>% 
  mutate(ending_pair = paste0("-", from_ending, " -- -", to_ending), 
         pos_pair = paste0(from_pos, " -- ", to_pos)) %>% 
  filter(ending_pair %in% less_var_pos) %>% 
  group_by(ending_pair) %>% 
  count(pos_pair, sort = F) %>% 
  select(-n) %>% 
  summarise(n_var = n(),
    pos_list = paste0(pos_pair, collapse = ", ")) %>% 
  arrange(-desc(n_var)) %>% 
  head(length(less_var_pos))
```

    # A tibble: 13 × 3
       ending_pair    n_var pos_list                                                
       <chr>          <int> <chr>                                                   
     1 -а'р -- -а'р       1 NOUN -- NOUN                                            
     2 -и'р -- -и'р       1 NOUN -- NOUN                                            
     3 -о'д -- -о'д       1 NOUN -- NOUN                                            
     4 -са' -- -са'       1 NOUN -- NOUN                                            
     5 -ца' -- -ца'       1 NOUN -- NOUN                                            
     6 -а'н -- -а'н       3 NOUN -- NOUN, NOUN -- VERB_prich, VERB_prich -- NOUN    
     7 -е'с -- -е'с       3 NOUN -- NOUN, NOUN -- VERB, VERB -- NOUN                
     8 -е'ц -- -е'ц       3 ADV -- NOUN, NOUN -- ADV, NOUN -- NOUN                  
     9 -и'ть -- -и'ть     3 NOUN -- VERB_inf, VERB_inf -- NOUN, VERB_inf -- VERB_inf
    10 -а'ль -- -а'ль     4 ADV -- ADV, ADV -- NOUN, NOUN -- ADV, NOUN -- NOUN      
    11 -и'н -- -и'н       4 ADJ -- NOUN, NOUN -- NOUN, NOUN -- PRON, PRON -- NOUN   
    12 -ты' -- -ты'       4 NOUN -- NOUN, NOUN -- PRON, PRON -- NOUN, PRON -- PRON  
    13 -ё'т -- -ё'т       4 NOUN -- NOUN, NOUN -- VERB, VERB -- NOUN, VERB -- VERB  

#### fem

``` r
glimpse(fem_pairs)
```

    Rows: 34,383
    Columns: 16
    $ text_id      <chr> "C_156__20", "C_156__20", "C_156__20", "C_70__25", "C_70_…
    $ from         <chr> "стонет", "тучи", "тумане", "утратой", "унылой", "мною", …
    $ to           <chr> "догонит", "гремучий", "заране", "крылатой", "огнекрылой"…
    $ rhyme_alph   <chr> "догонит стонет", "гремучий тучи", "заране тумане", "крыл…
    $ meter        <chr> "Trochee", "Trochee", "Trochee", "Trochee", "Trochee", "T…
    $ feet         <chr> "4", "4", "4", "4", "4", "4", "4", "other", "6", "6", "6"…
    $ from_closure <chr> "fem", "fem", "fem", "fem", "fem", "fem", "fem", "fem", "…
    $ from_pos     <chr> "VERB", "NOUN", "NOUN", "NOUN", "ADJ", "PRON", "NOUN", "N…
    $ from_ending  <chr> "о'нет", "у'чи", "а'не", "а'той", "ы'лой", "о'ю", "а'ми",…
    $ from_feats   <chr> "V,несов,нп=непрош,ед,изъяв,3-л", "S,жен,неод=(вин,мн|род…
    $ from_sp      <chr> "10", "10", "010", "010", "010", "10", "010", "10", "0010…
    $ to_closure   <chr> "fem", "fem", "fem", "fem", "fem", "fem", "fem", "fem", "…
    $ to_pos       <chr> "VERB", "ADJ", "NOUN", "ADJ", "ADJ", "NOUN", "NOUN", "NOU…
    $ to_ending    <chr> "о'нит", "у'чий", "а'не", "а'той", "ы'лой", "о'ю", "а'ми"…
    $ to_feats     <chr> "V,сов,пе=непрош,ед,изъяв,3-л", "A=(вин,ед,полн,муж,неод|…
    $ to_sp        <chr> "010", "010", "010", "010", "0010", "010", "010", "10", "…

``` r
# total number of masc pairs
t <- nrow(fem_pairs)

# view 30 mf ending pairs
fem_pairs %>% 
  mutate(ending_pair = paste0("-", from_ending, " -- -", to_ending)) %>% 
  count(ending_pair, sort = T) %>% 
  head(30) %>% 
  mutate(perc = round((n/t)*100, 2))
```

              ending_pair    n perc
    1        -о'ю -- -о'ю 1251 3.64
    2      -а'ми -- -а'ми  972 2.83
    3    -е'нья -- -е'нья  871 2.53
    4      -а'ет -- -а'ет  772 2.25
    5    -е'нье -- -е'нье  741 2.16
    6        -а'я -- -а'я  719 2.09
    7      -а'ли -- -а'ли  464 1.35
    8    -а'нья -- -а'нья  438 1.27
    9      -а'ла -- -а'ла  398 1.16
    10     -и'ла -- -и'ла  355 1.03
    11   -а'нье -- -а'нье  327 0.95
    12   -е'ний -- -е'ний  283 0.82
    13   -и'лся -- -и'лся  269 0.78
    14     -е'ли -- -е'ли  231 0.67
    15     -а'ло -- -а'ло  220 0.64
    16       -ы'е -- -ы'е  216 0.63
    17       -а'ю -- -а'ю  212 0.62
    18   -а'лся -- -а'лся  206 0.60
    19     -а'ют -- -а'ют  201 0.58
    20   -ё'тся -- -ё'тся  201 0.58
    21     -о'ды -- -о'ды  197 0.57
    22   -и'тся -- -и'тся  195 0.57
    23     -е'та -- -е'та  193 0.56
    24     -о'ре -- -о'ре  189 0.55
    25 -и'тель -- -и'тель  188 0.55
    26       -о'е -- -о'е  182 0.53
    27 -е'ньем -- -е'ньем  179 0.52
    28     -е'ет -- -е'ет  174 0.51
    29     -о'чи -- -о'чи  174 0.51
    30     -и'ны -- -и'ны  169 0.49

``` r
# pull top ending pairs 
f_ranks <- fem_pairs %>% 
  mutate(ending_pair = paste0("-", from_ending, " -- -", to_ending)) %>% 
  count(ending_pair, sort = T) %>% 
  #head(1000) %>% 
  mutate(rank = row_number()) %>% select(-n)

# count POS variants inside every ending
f_ranked_pos <- fem_pairs %>% 
  mutate(ending_pair = paste0("-", from_ending, " -- -", to_ending), 
         pos_pair = paste0(from_pos, " -- ", to_pos)) %>% 
  #filter(ending_pair %in% rankss$ending_pair) %>% 
  group_by(ending_pair) %>% 
  count(pos_pair, sort = F) %>% 
  count(ending_pair, sort = T) %>% 
  left_join(f_ranks, by = "ending_pair") %>% 
  filter(rank < 101) %>% 
  mutate(label = paste0(rank, " ", ending_pair),
         group = ifelse(n < 4, "less variation", "more variation"))

head(f_ranked_pos, 15) 
```

    # A tibble: 15 × 5
    # Groups:   ending_pair [15]
       ending_pair        n  rank label             group         
       <chr>          <int> <int> <chr>             <chr>         
     1 -о'ю -- -о'ю      21     1 1 -о'ю -- -о'ю    more variation
     2 -а'я -- -а'я      17     6 6 -а'я -- -а'я    more variation
     3 -о'е -- -о'е      17    26 26 -о'е -- -о'е   more variation
     4 -у'ю -- -у'ю      11    36 36 -у'ю -- -у'ю   more variation
     5 -ы'е -- -ы'е      11    16 16 -ы'е -- -ы'е   more variation
     6 -а'ло -- -а'ло    10    15 15 -а'ло -- -а'ло more variation
     7 -е'ю -- -е'ю       9    63 63 -е'ю -- -е'ю   more variation
     8 -и'во -- -и'во     9    87 87 -и'во -- -и'во more variation
     9 -о'го -- -о'го     9    57 57 -о'го -- -о'го more variation
    10 -е'ло -- -е'ло     8    62 62 -е'ло -- -е'ло more variation
    11 -о'ле -- -о'ле     8    31 31 -о'ле -- -о'ле more variation
    12 -а'ла -- -а'ла     7     9 9 -а'ла -- -а'ла  more variation
    13 -а'та -- -а'та     7    61 61 -а'та -- -а'та more variation
    14 -е'е -- -е'е       7    65 65 -е'е -- -е'е   more variation
    15 -и'ло -- -и'ло     7    64 64 -и'ло -- -и'ло more variation

``` r
summary(f_ranked_pos$n) # 4 is 3rd qu.
```

       Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
       1.00    1.00    2.00    3.45    4.00   21.00 

``` r
# plot
f_ranked_pos  %>% 
  filter(rank < 51) %>% 
  ggplot(aes(x = rank, y = n, fill = group)) + geom_col(alpha = 0.7) + 
  coord_flip() + 
  scale_x_reverse(breaks = f_ranked_pos$rank, labels = f_ranked_pos$label) + 
  scale_fill_manual(values = c(met.brewer("Veronese")[6],
                               met.brewer("Veronese")[3]))
```

![](05_4_rhyme_morhp_pairs.markdown_strict_files/figure-markdown_strict/unnamed-chunk-44-1.png)

Analysis of the less variate pairs

``` r
f_ranked_pos %>% 
  filter(rank < 51) %>% 
  summary(n) # look into less than 1st quatrain, ie less than 2 pos pairs (much lower than in masc rhymes)
```

     ending_pair              n              rank          label          
     Length:50          Min.   : 1.00   Min.   : 1.00   Length:50         
     Class :character   1st Qu.: 1.00   1st Qu.:13.25   Class :character  
     Mode  :character   Median : 1.50   Median :25.50   Mode  :character  
                        Mean   : 3.62   Mean   :25.50                     
                        3rd Qu.: 4.00   3rd Qu.:37.75                     
                        Max.   :21.00   Max.   :50.00                     
        group          
     Length:50         
     Class :character  
     Mode  :character  
                       
                       
                       

``` r
less_var_pos_f <- f_ranked_pos %>% 
  filter(rank < 51 & n < 2
           #group == "less variation"
           ) %>% 
  arrange(-desc(rank)) %>% pull(ending_pair)

length(less_var_pos_f) # 25 endings with ONLY ONE POS combination
```

    [1] 25

``` r
fem_pairs %>% 
  mutate(ending_pair = paste0("-", from_ending, " -- -", to_ending), 
         pos_pair = paste0(from_pos, " -- ", to_pos)) %>% 
  filter(ending_pair %in% less_var_pos_f) %>% 
  group_by(ending_pair) %>% 
  count(pos_pair, sort = F) %>% 
  select(-n) %>% 
  summarise(n_var = n(),
    pos_list = paste0(pos_pair, collapse = ", ")) %>% 
  arrange(-desc(n_var)) %>% 
  head(length(less_var_pos_f))
```

    # A tibble: 25 × 3
       ending_pair          n_var pos_list            
       <chr>                <int> <chr>               
     1 -а'дость -- -а'дость     1 NOUN -- NOUN        
     2 -а'ет -- -а'ет           1 VERB -- VERB        
     3 -а'ет -- -я'ет           1 VERB -- VERB        
     4 -а'лся -- -а'лся         1 VERB -- VERB        
     5 -а'на -- -а'на           1 NOUN -- NOUN        
     6 -а'ний -- -а'ний         1 NOUN -- NOUN        
     7 -а'нье -- -а'нье         1 NOUN -- NOUN        
     8 -а'нья -- -а'нья         1 NOUN -- NOUN        
     9 -а'ться -- -а'ться       1 VERB_inf -- VERB_inf
    10 -а'ют -- -а'ют           1 VERB -- VERB        
    # ℹ 15 more rows

noun-noun, verb-verb, inf-inf

#### kendall cor

``` r
glimpse(m_ranked_pos)
```

    Rows: 100
    Columns: 5
    Groups: ending_pair [100]
    $ ending_pair <chr> "-о'й -- -о'й", "-и'м -- -и'м", "-е'й -- -е'й", "-ё'м -- -…
    $ n           <int> 29, 28, 24, 24, 18, 17, 17, 17, 16, 16, 13, 13, 13, 13, 12…
    $ rank        <int> 1, 35, 2, 82, 30, 25, 4, 9, 75, 31, 11, 22, 10, 32, 7, 87,…
    $ label       <chr> "1 -о'й -- -о'й", "35 -и'м -- -и'м", "2 -е'й -- -е'й", "82…
    $ group       <chr> "more variation", "more variation", "more variation", "mor…

``` r
rank_list <- m_ranked_pos %>% 
  ungroup() %>% 
  rename(rank_ending = rank) %>% 
  mutate(rank_variation = row_number()) %>% 
  select(-n, -label, -group)

# masculine ranks lists correlation
cor.test(rank_list$rank_ending, rank_list$rank_variation, 
         method = "kendall")
```


        Kendall's rank correlation tau

    data:  rank_list$rank_ending and rank_list$rank_variation
    z = 3.5261, p-value = 0.0004217
    alternative hypothesis: true tau is not equal to 0
    sample estimates:
          tau 
    0.2391919 

``` r
# fem
rank_list <- f_ranked_pos %>% 
  ungroup() %>% 
  rename(rank_ending = rank) %>% 
  mutate(rank_variation = row_number()) %>% 
  select(-n, -label, -group)

# masculine ranks lists correlation
cor.test(rank_list$rank_ending, rank_list$rank_variation, 
         method = "kendall")
```


        Kendall's rank correlation tau

    data:  rank_list$rank_ending and rank_list$rank_variation
    z = -0.077431, p-value = 0.9383
    alternative hypothesis: true tau is not equal to 0
    sample estimates:
             tau 
    -0.005252525 

#### lineplot

Curves: Correlation btw ending frequency & grammatical variation

``` r
m_ranked_long <- masc_pairs %>% 
  mutate(ending_pair = paste0("-", from_ending, " -- -", to_ending), 
         pos_pair = paste0(from_pos, " -- ", to_pos)) %>% 
  #filter(ending_pair %in% rankss$ending_pair) %>% 
  group_by(ending_pair) %>% 
  count(pos_pair, sort = F) %>% 
  count(ending_pair, sort = T) %>% 
  left_join(m_ranks, by = "ending_pair") %>% 
  filter(rank < 1001)

f_ranked_long <- fem_pairs %>% 
  mutate(ending_pair = paste0("-", from_ending, " -- -", to_ending), 
         pos_pair = paste0(from_pos, " -- ", to_pos)) %>% 
  #filter(ending_pair %in% rankss$ending_pair) %>% 
  group_by(ending_pair) %>% 
  count(pos_pair, sort = F) %>% 
  count(ending_pair, sort = T) %>% 
  left_join(f_ranks, by = "ending_pair") %>% 
  filter(rank < 1001)

glimpse(m_ranked_long)
```

    Rows: 1,000
    Columns: 3
    Groups: ending_pair [1,000]
    $ ending_pair <chr> "-о'й -- -о'й", "-и'м -- -и'м", "-е'й -- -е'й", "-ё'м -- -…
    $ n           <int> 29, 28, 24, 24, 18, 17, 17, 17, 16, 16, 15, 15, 14, 13, 13…
    $ rank        <int> 1, 35, 2, 82, 30, 25, 4, 9, 75, 31, 174, 157, 132, 11, 22,…

``` r
m_ranked_long %>% 
  ungroup() %>% 
  select(rank, n) %>% 
  mutate(group = "masc") %>% 
  rbind(f_ranked_long %>%
          ungroup() %>%
          select(rank, n) %>%
          mutate(group = "fem")
        ) %>%
  filter(rank < 1001) %>% 
  ggplot(aes(x = rank, y = n, color = group)) + 
  geom_point(alpha = 0.5, size = 0.9) + 
  geom_line(alpha = 0.6, linewidth = 0.5) + 
  geom_smooth(alpha = 0.7) + 
  facet_wrap(~group) + 
  scale_color_manual(values = c(met.brewer("Veronese")[3],
                                met.brewer("Veronese")[5]))
```

    `geom_smooth()` using method = 'gam' and formula = 'y ~ s(x, bs = "cs")'

![](05_4_rhyme_morhp_pairs.markdown_strict_files/figure-markdown_strict/unnamed-chunk-47-1.png)

### feats variability

``` r
# count feats variants inside every ending
m_ranked_feats <- masc_pairs %>% 
  mutate(ending_pair = paste0("-", from_ending, " -- -", to_ending), 
         feats_pair = paste0(from_feats, " -- ", to_feats)) %>% 
  #filter(ending_pair %in% rankss$ending_pair) %>% 
  group_by(ending_pair) %>% 
  count(feats_pair, sort = F) %>% 
  count(ending_pair, sort = T) %>% 
  left_join(m_ranks, by = "ending_pair") %>% 
  filter(rank < 1001)

f_ranked_feats <- fem_pairs %>% 
  mutate(ending_pair = paste0("-", from_ending, " -- -", to_ending), 
         feats_pair = paste0(from_feats, " -- ", to_feats)) %>% 
  #filter(ending_pair %in% rankss$ending_pair) %>% 
  group_by(ending_pair) %>% 
  count(feats_pair, sort = F) %>% 
  count(ending_pair, sort = T) %>% 
  left_join(f_ranks, by = "ending_pair") %>% 
  filter(rank < 1001)


m_ranked_feats %>% 
  ungroup() %>% 
  select(rank, n) %>% 
  mutate(group = "masc") %>% 
  rbind(f_ranked_feats %>%
          ungroup() %>%
          select(rank, n) %>%
          mutate(group = "fem")
        ) %>%
  filter(rank < 1001) %>% 
  ggplot(aes(x = rank, y = n, color = group)) + 
  geom_point(alpha = 0.5, size = 0.9) + 
  geom_line(alpha = 0.6, linewidth = 0.5) + 
  geom_smooth(alpha = 0.7) + 
  facet_wrap(~group) + 
  scale_color_manual(values = c(met.brewer("Veronese")[3],
                                met.brewer("Veronese")[5]))
```

    `geom_smooth()` using method = 'gam' and formula = 'y ~ s(x, bs = "cs")'

![](05_4_rhyme_morhp_pairs.markdown_strict_files/figure-markdown_strict/unnamed-chunk-48-1.png)

#### kendall

``` r
# glimpse(m_ranked_feats)

rank_list <- m_ranked_feats %>% 
  ungroup() %>% 
  #filter(rank < 101) %>% 
  rename(rank_ending = rank) %>% 
  mutate(rank_variation = row_number()) %>% 
  select(-n)

# masculine ranks lists correlation
cor.test(rank_list$rank_ending, rank_list$rank_variation, 
         method = "kendall")
```


        Kendall's rank correlation tau

    data:  rank_list$rank_ending and rank_list$rank_variation
    z = 32.821, p-value < 2.2e-16
    alternative hypothesis: true tau is not equal to 0
    sample estimates:
          tau 
    0.6931291 

``` r
# fem
rank_list <- f_ranked_feats %>% 
  ungroup() %>% 
  filter(rank < 101) %>%
  rename(rank_ending = rank) %>% 
  mutate(rank_variation = row_number()) %>% 
  select(-n)

# masculine ranks lists correlation
cor.test(rank_list$rank_ending, rank_list$rank_variation, 
         method = "kendall")
```


        Kendall's rank correlation tau

    data:  rank_list$rank_ending and rank_list$rank_variation
    z = 3.6452, p-value = 0.0002672
    alternative hypothesis: true tau is not equal to 0
    sample estimates:
          tau 
    0.2472727 

Look into less variable endings

``` r
m_ranked_feats %>% 
  filter(rank < 51) %>% 
  summary() # 25.25 1st Qu
```

     ending_pair              n               rank      
     Length:50          Min.   :  5.00   Min.   : 1.00  
     Class :character   1st Qu.: 25.25   1st Qu.:13.25  
     Mode  :character   Median : 49.00   Median :25.50  
                        Mean   : 67.90   Mean   :25.50  
                        3rd Qu.: 81.75   3rd Qu.:37.75  
                        Max.   :251.00   Max.   :50.00  

``` r
less_var_feats <- m_ranked_feats %>% 
  filter(rank < 51 & n < 25.25) %>% 
  pull(ending_pair)

length(less_var_feats)
```

    [1] 13

``` r
x <- masc_pairs %>% 
  mutate(ending_pair = paste0("-", from_ending, " -- -", to_ending), 
         feats_pair = paste0(from_feats, " -- ", to_feats)) %>% 
  filter(ending_pair %in% less_var_feats) %>% 
  group_by(ending_pair) %>% 
  count(feats_pair, sort = F) %>% 
  select(-n) %>% 
  summarise(n_var = n(),
            feats_list = paste0(feats_pair, collapse = " \n ")) %>% 
  arrange(-desc(n_var)) %>% 
  head(length(less_var_feats))

# write.csv(x, "feats_masc.csv")
```

Examples

``` r
# ending intersecation between less variative in terms of POS combitaion & grammar
tibble(ending_pair = intersect(less_var_pos, less_var_feats)) %>% 
  left_join(m_ranks, by = "ending_pair")
```

    # A tibble: 9 × 2
      ending_pair     rank
      <chr>          <int>
    1 -ты' -- -ты'      12
    2 -е'ц -- -е'ц      19
    3 -и'р -- -и'р      36
    4 -са' -- -са'      39
    5 -е'с -- -е'с      42
    6 -о'д -- -о'д      44
    7 -ца' -- -ца'      47
    8 -а'ль -- -а'ль    48
    9 -а'р -- -а'р      49

``` r
masc_pairs %>% 
  mutate(ending_pair = paste0("-", from_ending, " -- -", to_ending), 
         feats_pair = paste0(from_feats, " -- ", to_feats)) %>% 
  filter(ending_pair %in% intersect(less_var_pos, less_var_feats)) %>% 
  group_by(ending_pair, feats_pair) %>% 
  sample_n(1) %>% 
  select(ending_pair, rhyme_alph, from, to, from_pos, to_pos, feats_pair) %>% 
  ungroup() %>% 
  distinct(rhyme_alph, .keep_all = TRUE)
```

    # A tibble: 133 × 7
       ending_pair    rhyme_alph      from     to       from_pos to_pos feats_pair  
       <chr>          <chr>           <chr>    <chr>    <chr>    <chr>  <chr>       
     1 -а'ль -- -а'ль печаль скрыжаль скрыжаль печаль   ADV      NOUN   ADV,вводн= …
     2 -а'ль -- -а'ль даль жаль       жаль     даль     ADV      NOUN   ADV,прдк= -…
     3 -а'ль -- -а'ль жаль хрусталь   жаль     хрусталь ADV      NOUN   ADV,прдк= -…
     4 -а'ль -- -а'ль вдаль жаль      вдаль    жаль     ADV      ADV    ADV= -- ADV…
     5 -а'ль -- -а'ль вдаль печаль    вдаль    печаль   ADV      NOUN   ADV= -- S,ж…
     6 -а'ль -- -а'ль жаль печаль     печаль   жаль     NOUN     ADV    S,жен,неод=…
     7 -а'ль -- -а'ль даль печаль     печаль   даль     NOUN     NOUN   S,жен,неод=…
     8 -а'ль -- -а'ль даль хрусталь   даль     хрусталь NOUN     NOUN   S,жен,неод=…
     9 -а'ль -- -а'ль враль жаль      враль    жаль     NOUN     ADV    S,муж,од=им…
    10 -а'ль -- -а'ль даль москаль    москаль  даль     NOUN     NOUN   S,муж,од=им…
    # ℹ 123 more rows

FEM rhymes feats analysis

``` r
f_ranked_feats %>% 
  filter(rank < 51) %>% 
  summary() # 8.25 1st qu. (vs masc 25.25 1st Qu)
```

     ending_pair              n               rank      
     Length:50          Min.   :  1.00   Min.   : 1.00  
     Class :character   1st Qu.:  8.25   1st Qu.:13.25  
     Mode  :character   Median : 20.00   Median :25.50  
                        Mean   : 31.88   Mean   :25.50  
                        3rd Qu.: 41.00   3rd Qu.:37.75  
                        Max.   :129.00   Max.   :50.00  

``` r
less_var_feats_f <- f_ranked_feats %>% 
  filter(rank < 51 & n < 8.25) %>% 
  pull(ending_pair)

length(less_var_feats_f) # 13
```

    [1] 13

``` r
x <- fem_pairs %>% 
  mutate(ending_pair = paste0("-", from_ending, " -- -", to_ending), 
         feats_pair = paste0(from_feats, " -- ", to_feats)) %>% 
  filter(ending_pair %in% less_var_feats_f) %>% 
  group_by(ending_pair) %>% 
  count(feats_pair, sort = F) %>% 
  select(-n) %>% 
  summarise(n_var = n(),
            feats_list = paste0(feats_pair, collapse = " \n ")) %>% 
  arrange(-desc(n_var)) %>% 
  head(length(less_var_feats_f))

x
```

    # A tibble: 13 × 3
       ending_pair          n_var feats_list                                        
       <chr>                <int> <chr>                                             
     1 -а'дость -- -а'дость     1 "S,жен,неод=(вин,ед|им,ед) -- S,жен,неод=(вин,ед|…
     2 -е'нье -- -е'нье         1 "S,сред,неод=(пр,ед|вин,ед|им,ед) -- S,сред,неод=…
     3 -е'ньем -- -е'ньем       1 "S,сред,неод=твор,ед -- S,сред,неод=твор,ед"      
     4 -а'ний -- -а'ний         2 "S,имя,муж,од=им,ед -- S,сред,неод=род,мн \n S,ср…
     5 -а'нье -- -а'нье         2 "S,ед,сред,неод=(пр|вин|им) -- S,сред,неод=(пр,ед…
     6 -о'чи -- -о'чи           3 "S,гео,ед,жен,неод=(пр|дат|род) -- S,жен,неод=(пр…
     7 -а'нья -- -а'нья         5 "S,ед,сред,неод=род -- S,сред,неод=(вин,мн|род,ед…
     8 -е'ний -- -е'ний         5 "S,муж,од=им,ед -- S,сред,неод=род,мн \n S,муж,од…
     9 -е'нья -- -е'нья         7 "S,имя,жен,од=им,ед -- S,сред,неод=(вин,мн|род,ед…
    10 -и'тель -- -и'тель       7 "S,жен,неод=(вин,ед|им,ед) -- S,муж,неод=(вин,ед|…
    11 -и'ца -- -и'ца           7 "S,жен,неод=им,ед -- S,жен,неод=им,ед \n S,жен,не…
    12 -и'цы -- -и'цы           7 "S,жен,неод=(вин,мн|род,ед|им,мн) -- S,жен,неод=(…
    13 -у'ки -- -у'ки           8 "A=мн,кр -- S,муж,неод=(вин,мн|им,мн) \n S,жен,не…

``` r
#write.csv(x, "feats_fem.csv")
```

## words inside endings

``` r
glimpse(m_ranks)
```

    Rows: 1,450
    Columns: 2
    $ ending_pair <chr> "-о'й -- -о'й", "-е'й -- -е'й", "-а'л -- -а'л", "-на' -- -…
    $ rank        <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17,…

``` r
glimpse(masc_pairs)
```

    Rows: 38,011
    Columns: 16
    $ text_id      <chr> "P_1938", "P_1938", "C_156__20", "C_156__20", "C_156__20"…
    $ from         <chr> "краса", "огневым", "силки", "спор", "сном", "твоя", "тиш…
    $ to           <chr> "небеса", "земным", "легки", "простор", "лучом", "я", "по…
    $ rhyme_alph   <chr> "краса небеса", "земным огневым", "легки силки", "простор…
    $ meter        <chr> "Other", "Other", "Trochee", "Trochee", "Trochee", "Iamb"…
    $ feet         <chr> "other", "other", "4", "4", "4", "4", "4", "4", "4", "4",…
    $ from_closure <chr> "masc", "masc", "masc", "masc", "masc", "masc", "masc", "…
    $ from_pos     <chr> "NOUN", "NOUN", "NOUN", "NOUN", "NOUN", "PRON", "NOUN", "…
    $ from_ending  <chr> "са'", "ы'м", "ки'", "о'р", "о'м", "оя'", "не'", "ё'т", "…
    $ from_feats   <chr> "S,жен,неод=им,ед", "S,фам,муж,од=(дат,мн|твор,ед)", "S,м…
    $ from_sp      <chr> "01", "001", "01", "1", "1", "01", "001", "01", "01", "01…
    $ to_closure   <chr> "masc", "masc", "masc", "masc", "masc", "masc", "masc", "…
    $ to_pos       <chr> "NOUN", "ADJ", "ADJ", "NOUN", "NOUN", "PRON", "NOUN", "VE…
    $ to_ending    <chr> "са'", "ы'м", "ки'", "о'р", "о'м", "я'", "не'", "ё'т", "и…
    $ to_feats     <chr> "S,сред,неод=(вин,мн|им,мн)", "A=(дат,мн,полн|твор,ед,пол…
    $ to_sp        <chr> "001", "01", "01", "01", "01", "1", "001", "01", "001", "…

``` r
m_1 <- masc_pairs %>% 
  mutate(ending_pair = paste0("-", from_ending, " -- -", to_ending)) %>% 
  select(from, from_ending, ending_pair) %>% 
  rename(word = from, 
         ending = from_ending)

m_2 <- masc_pairs %>% 
  mutate(ending_pair = paste0("-", from_ending, " -- -", to_ending)) %>% 
  select(to, to_ending, ending_pair) %>% 
  rename(word = to, 
         ending = to_ending)

masc_words <- rbind(m_1, m_2)


f_1 <- fem_pairs %>% 
  mutate(ending_pair = paste0("-", from_ending, " -- -", to_ending)) %>% 
  select(from, from_ending, ending_pair) %>% 
  rename(word = from, 
         ending = from_ending)

f_2 <- fem_pairs %>% 
  mutate(ending_pair = paste0("-", from_ending, " -- -", to_ending)) %>% 
  select(to, to_ending, ending_pair) %>% 
  rename(word = to, 
         ending = to_ending)


fem_words <- rbind(f_1, f_2)

fem_words_p <- fem_words %>% 
  group_by(ending_pair) %>% 
  count(word, sort = T) %>% 
  ungroup() %>% 
  count(ending_pair, sort = T) %>% 
  left_join(f_ranks, by = "ending_pair") %>% 
  filter(rank < 1001) %>% 
  mutate(group = "fem")


# glimpse(masc_words)

masc_words %>% 
  group_by(ending_pair) %>% 
  count(word, sort = T) %>% 
  ungroup() %>% 
  count(ending_pair, sort = T) %>% 
  left_join(m_ranks, by = "ending_pair") %>% 
  filter(rank < 1001) %>% 
  mutate(group = "masc") %>% 
  rbind(fem_words_p) %>% 
  
  ggplot(aes( x = rank, y = n, color = group)) + 
  geom_point(alpha = 0.5, size = 0.9) + 
  geom_line(alpha = 0.6, linewidth = 0.5) + 
 
  facet_wrap(~group) +
    geom_smooth(alpha = 0.7) +
  scale_color_manual(values = c(met.brewer("Veronese")[3],
                                met.brewer("Veronese")[5]))
```

    `geom_smooth()` using method = 'gam' and formula = 'y ~ s(x, bs = "cs")'

![](05_4_rhyme_morhp_pairs.markdown_strict_files/figure-markdown_strict/unnamed-chunk-54-1.png)

``` r
masc_words %>% 
  group_by(ending_pair) %>% 
  count(word, sort = T) %>% 
  ungroup() %>% 
  count(ending_pair, sort = T) %>% 
  left_join(m_ranks, by = "ending_pair") %>% 
  filter(rank < 51) %>% 
  summary() # 1st qu.: 37.25
```

     ending_pair              n               rank      
     Length:50          Min.   :  6.00   Min.   : 1.00  
     Class :character   1st Qu.: 37.25   1st Qu.:13.25  
     Mode  :character   Median : 82.50   Median :25.50  
                        Mean   :125.72   Mean   :25.50  
                        3rd Qu.:159.75   3rd Qu.:37.75  
                        Max.   :607.00   Max.   :50.00  

``` r
less_var_words <- masc_words %>% 
  group_by(ending_pair) %>% 
  count(word, sort = T) %>% 
  ungroup() %>% 
  count(ending_pair, sort = T) %>% 
  left_join(m_ranks, by = "ending_pair") %>% 
  filter(rank < 51 & n < 37.25) %>% 
  pull(ending_pair)

length(less_var_words)
```

    [1] 13

``` r
intersect(less_var_pos, less_var_words)
```

    [1] "-и'р -- -и'р"   "-са' -- -са'"   "-е'с -- -е'с"   "-о'д -- -о'д"  
    [5] "-ца' -- -ца'"   "-а'ль -- -а'ль" "-а'р -- -а'р"  

``` r
length(intersect(less_var_pos, less_var_words))
```

    [1] 7

``` r
intersect(less_var_feats, less_var_words)
```

    [1] "-а'р -- -а'р"   "-са' -- -са'"   "-е'с -- -е'с"   "-е'нь -- -е'нь"
    [5] "-а'ль -- -а'ль" "-и'р -- -и'р"   "-ца' -- -ца'"   "-о'д -- -о'д"  
    [9] "-о'вь -- -о'вь"

``` r
length(intersect(less_var_feats, less_var_words))
```

    [1] 9

Examples of unproductive endings & all less var endings

``` r
masc_words %>% 
  filter(ending_pair %in% less_var_words) %>% 
  group_by(ending_pair) %>% 
  count(word, sort = T) %>% 
  select(-n) %>% 
  summarise(n_var = n(),
            word_list = paste0(word, collapse = ", ")) %>% 
  arrange(-desc(n_var))
```

    # A tibble: 13 × 3
       ending_pair    n_var word_list                                               
       <chr>          <int> <chr>                                                   
     1 -о'вь -- -о'вь     6 любовь, вновь, кровь, бровь, приготовь, уготовь         
     2 -а'ль -- -а'ль    15 печаль, даль, жаль, сталь, вдаль, скрижаль, эмаль, вуал…
     3 -са' -- -са'      15 небеса, краса, леса, чудеса, роса, коса, полоса, паруса…
     4 -го' -- -го'      16 его, него, своего, ничего, моего, того, твоего, всего, …
     5 -е'нь -- -е'нь    18 день, тень, лень, сень, кремень, олень, ступень, дереве…
     6 -а'с -- -а'с      20 час, нас, вас, глас, погас, угас, парнас, спас, сейчас,…
     7 -е'с -- -е'с      21 небес, лес, чудес, древес, зевес, воскрес, геркулес, пе…
     8 -ня' -- -ня'      22 меня, дня, огня, коня, храня, стеня, родня, кляня, пня,…
     9 -и'р -- -и'р      25 мир, пир, эфир, кумир, лир, зефир, клир, сир, командир,…
    10 -о'д -- -о'д      28 вод, народ, свод, род, год, плод, хоровод, поход, восхо…
    11 -и'х -- -и'х      32 их, них, своих, моих, стих, твоих, других, затих, жених…
    12 -а'р -- -а'р      36 дар, жар, пожар, удар, пар, татар, шар, чар, гусар, баз…
    13 -ца' -- -ца'      37 конца, отца, венца, творца, певца, лица, мертвеца, бойц…

Most monotonous POS-wise:

NB some “monotonous” endings are easily generating a lot of words (-ить)

``` r
masc_words %>% 
  filter(ending_pair %in% less_var_pos) %>% 
  group_by(ending_pair) %>% 
  count(word, sort = T) %>% 
  select(-n) %>% 
  summarise(n_var = n(),
            word_list = paste0(word, collapse = ", ")) %>% 
  arrange(-desc(n_var)) 
```

    # A tibble: 13 × 3
       ending_pair    n_var word_list                                               
       <chr>          <int> <chr>                                                   
     1 -а'ль -- -а'ль    15 печаль, даль, жаль, сталь, вдаль, скрижаль, эмаль, вуал…
     2 -са' -- -са'      15 небеса, краса, леса, чудеса, роса, коса, полоса, паруса…
     3 -е'с -- -е'с      21 небес, лес, чудес, древес, зевес, воскрес, геркулес, пе…
     4 -и'р -- -и'р      25 мир, пир, эфир, кумир, лир, зефир, клир, сир, командир,…
     5 -о'д -- -о'д      28 вод, народ, свод, род, год, плод, хоровод, поход, восхо…
     6 -а'р -- -а'р      36 дар, жар, пожар, удар, пар, татар, шар, чар, гусар, баз…
     7 -ца' -- -ца'      37 конца, отца, венца, творца, певца, лица, мертвеца, бойц…
     8 -ты' -- -ты'      41 мечты, ты, красоты, цветы, суеты, высоты, черты, листы,…
     9 -е'ц -- -е'ц      49 венец, певец, конец, наконец, сердец, отец, творец, мер…
    10 -и'н -- -и'н      50 один, долин, властелин, исполин, господин, вершин, граж…
    11 -а'н -- -а'н      68 туман, океан, стан, обман, ураган, великан, стран, дан,…
    12 -ё'т -- -ё'т     279 поет, живет, зовет, идет, придет, найдет, несет, цветет…
    13 -и'ть -- -и'ть   300 любить, жить, говорить, пить, нить, пособить, делить, р…

Most monotonous feats-wise

``` r
masc_words %>% 
  filter(ending_pair %in% less_var_feats) %>% 
  group_by(ending_pair) %>% 
  count(word, sort = T) %>% 
  select(-n) %>% 
  summarise(n_var = n(),
            word_list = paste0(word, collapse = ", ")) %>% 
  arrange(-desc(n_var))
```

    # A tibble: 13 × 3
       ending_pair    n_var word_list                                               
       <chr>          <int> <chr>                                                   
     1 -о'вь -- -о'вь     6 любовь, вновь, кровь, бровь, приготовь, уготовь         
     2 -а'ль -- -а'ль    15 печаль, даль, жаль, сталь, вдаль, скрижаль, эмаль, вуал…
     3 -са' -- -са'      15 небеса, краса, леса, чудеса, роса, коса, полоса, паруса…
     4 -е'нь -- -е'нь    18 день, тень, лень, сень, кремень, олень, ступень, дереве…
     5 -е'с -- -е'с      21 небес, лес, чудес, древес, зевес, воскрес, геркулес, пе…
     6 -и'р -- -и'р      25 мир, пир, эфир, кумир, лир, зефир, клир, сир, командир,…
     7 -о'д -- -о'д      28 вод, народ, свод, род, год, плод, хоровод, поход, восхо…
     8 -а'р -- -а'р      36 дар, жар, пожар, удар, пар, татар, шар, чар, гусар, баз…
     9 -ца' -- -ца'      37 конца, отца, венца, творца, певца, лица, мертвеца, бойц…
    10 -ты' -- -ты'      41 мечты, ты, красоты, цветы, суеты, высоты, черты, листы,…
    11 -о'р -- -о'р      48 взор, гор, укор, приговор, разговор, пор, простор, бор,…
    12 -е'ц -- -е'ц      49 венец, певец, конец, наконец, сердец, отец, творец, мер…
    13 -о'к -- -о'к      89 ветерок, поток, цветок, восток, рок, одинок, порок, уро…

Intersections

``` r
length(intersect(less_var_pos, less_var_feats))
```

    [1] 9

``` r
length(intersect(less_var_pos, less_var_words))
```

    [1] 7

``` r
length(intersect(less_var_feats, less_var_words))
```

    [1] 9

``` r
length(intersect(intersect(less_var_pos, less_var_feats), less_var_words))
```

    [1] 7

``` r
# the most monotonous masc rhyme endings
intersect(intersect(less_var_pos, less_var_feats), less_var_words)
```

    [1] "-и'р -- -и'р"   "-са' -- -са'"   "-е'с -- -е'с"   "-о'д -- -о'д"  
    [5] "-ца' -- -ца'"   "-а'ль -- -а'ль" "-а'р -- -а'р"  

``` r
masc_words %>% 
  filter(ending_pair %in% intersect(
    intersect(
      less_var_pos, less_var_feats), 
    less_var_words)
    ) %>% 
  group_by(ending_pair) %>% 
  count(word, sort = T) %>% 
  select(-n) %>% 
  summarise(n_var = n(),
            word_list = paste0(word, collapse = ", ")) %>% 
  arrange(-desc(n_var))
```

    # A tibble: 7 × 3
      ending_pair    n_var word_list                                                
      <chr>          <int> <chr>                                                    
    1 -а'ль -- -а'ль    15 печаль, даль, жаль, сталь, вдаль, скрижаль, эмаль, вуаль…
    2 -са' -- -са'      15 небеса, краса, леса, чудеса, роса, коса, полоса, паруса,…
    3 -е'с -- -е'с      21 небес, лес, чудес, древес, зевес, воскрес, геркулес, пер…
    4 -и'р -- -и'р      25 мир, пир, эфир, кумир, лир, зефир, клир, сир, командир, …
    5 -о'д -- -о'д      28 вод, народ, свод, род, год, плод, хоровод, поход, восход…
    6 -а'р -- -а'р      36 дар, жар, пожар, удар, пар, татар, шар, чар, гусар, база…
    7 -ца' -- -ца'      37 конца, отца, венца, творца, певца, лица, мертвеца, бойца…

Fem words

``` r
fem_words %>% 
  group_by(ending_pair) %>% 
  count(word, sort = T) %>% 
  ungroup() %>% 
  count(ending_pair, sort = T) %>% 
  left_join(f_ranks, by = "ending_pair") %>% 
  filter(rank < 51) %>% 
  summary() # 1st qu.: 44 (vs masc:  37.25)
```

     ending_pair              n              rank      
     Length:50          Min.   :  3.0   Min.   : 1.00  
     Class :character   1st Qu.: 44.0   1st Qu.:13.25  
     Mode  :character   Median :101.5   Median :25.50  
                        Mean   :125.8   Mean   :25.50  
                        3rd Qu.:164.8   3rd Qu.:37.75  
                        Max.   :439.0   Max.   :50.00  

``` r
less_var_words_f <- fem_words %>% 
  group_by(ending_pair) %>% 
  count(word, sort = T) %>% 
  ungroup() %>% 
  count(ending_pair, sort = T) %>% 
  left_join(f_ranks, by = "ending_pair") %>% 
  filter(rank < 51 & n < 44) %>% 
  pull(ending_pair)

length(less_var_words_f)
```

    [1] 13

``` r
# intersect(less_var_pos_f, less_var_words_f)
# length(intersect(less_var_pos_f, less_var_words_f))
# 
# intersect(less_var_feats_f, less_var_words_f)
# length(intersect(less_var_feats_f, less_var_words_f))
```

Smallest n of words inside freq ending

``` r
fem_words %>% 
  filter(ending_pair %in% less_var_words_f) %>% 
  group_by(ending_pair) %>% 
  count(word, sort = T) %>% 
  select(-n) %>% 
  summarise(n_var = n(),
            word_list = paste0(word, collapse = ", ")) %>% 
  arrange(-desc(n_var))
```

    # A tibble: 13 × 3
       ending_pair          n_var word_list                                         
       <chr>                <int> <chr>                                             
     1 -а'дость -- -а'дость     3 радость, младость, сладость                       
     2 -о'чи -- -о'чи           3 ночи, очи, колочи                                 
     3 -у'ки -- -у'ки          10 звуки, руки, разлуки, науки, скуки, внуки, штуки,…
     4 -о'ле -- -о'ле          14 поле, воле, боле, доле, неволе, поневоле, дотоле,…
     5 -а'вы -- -а'вы          19 славы, забавы, державы, дубравы, нравы, отравы, у…
     6 -и'ра -- -и'ра          24 мира, лира, эфира, кумира, порфира, пира, зефира,…
     7 -о'ды -- -о'ды          24 природы, годы, свободы, народы, непогоды, своды, …
     8 -о'да -- -о'да          27 природа, народа, свобода, свода, года, непогода, …
     9 -о'ре -- -о'ре          27 море, горе, просторе, споре, вскоре, взоре, уборе…
    10 -о'ры -- -о'ры          35 взоры, горы, авроры, разговоры, хоры, уборы, укор…
    11 -о'дит -- -о'дит        38 бродит, находит, приходит, заводит, ходит, подход…
    12 -и'ца -- -и'ца          39 царица, денница, девица, птица, темница, столица,…
    13 -и'цы -- -и'цы          42 денницы, птицы, царицы, девицы, ресницы, гробницы…

Smallest POS pair variation – NB -енья / -енье гнезда (вдохновенье /
вдохновенья…)

``` r
fem_words %>% 
  filter(ending_pair %in% less_var_pos_f) %>% 
  group_by(ending_pair) %>% 
  count(word, sort = T) %>% 
  select(-n) %>% 
  summarise(n_var = n(),
            word_list = paste0(word, collapse = ", ")) %>% 
  arrange(-desc(n_var))
```

    # A tibble: 25 × 3
       ending_pair          n_var word_list                                         
       <chr>                <int> <chr>                                             
     1 -а'дость -- -а'дость     3 радость, младость, сладость                       
     2 -о'чи -- -о'чи           3 ночи, очи, колочи                                 
     3 -и'ра -- -и'ра          24 мира, лира, эфира, кумира, порфира, пира, зефира,…
     4 -о'ды -- -о'ды          24 природы, годы, свободы, народы, непогоды, своды, …
     5 -о'да -- -о'да          27 природа, народа, свобода, свода, года, непогода, …
     6 -о'ры -- -о'ры          35 взоры, горы, авроры, разговоры, хоры, уборы, укор…
     7 -о'дит -- -о'дит        38 бродит, находит, приходит, заводит, ходит, подход…
     8 -и'ца -- -и'ца          39 царица, денница, девица, птица, темница, столица,…
     9 -а'ний -- -а'ний        52 мечтаний, страданий, желаний, воспоминаний, упова…
    10 -а'на -- -а'на          59 тумана, океана, обмана, великана, стана, ивана, у…
    # ℹ 15 more rows

Less grammatical variation

``` r
fem_words %>% 
  filter(ending_pair %in% less_var_feats_f) %>% 
  group_by(ending_pair) %>% 
  count(word, sort = T) %>% 
  select(-n) %>% 
  summarise(n_var = n(),
            word_list = paste0(word, collapse = ", ")) %>% 
  arrange(-desc(n_var))
```

    # A tibble: 13 × 3
       ending_pair          n_var word_list                                         
       <chr>                <int> <chr>                                             
     1 -а'дость -- -а'дость     3 радость, младость, сладость                       
     2 -о'чи -- -о'чи           3 ночи, очи, колочи                                 
     3 -у'ки -- -у'ки          10 звуки, руки, разлуки, науки, скуки, внуки, штуки,…
     4 -и'ца -- -и'ца          39 царица, денница, девица, птица, темница, столица,…
     5 -и'цы -- -и'цы          42 денницы, птицы, царицы, девицы, ресницы, гробницы…
     6 -а'ний -- -а'ний        52 мечтаний, страданий, желаний, воспоминаний, упова…
     7 -и'тель -- -и'тель      90 обитель, хранитель, спаситель, истребитель, жител…
     8 -а'нья -- -а'нья        98 страданья, желанья, мечтанья, упованья, воспомина…
     9 -е'ний -- -е'ний       109 гений, вдохновений, наслаждений, песнопений, забл…
    10 -а'нье -- -а'нье       112 созданье, страданье, дыханье, очарованье, мечтань…
    11 -е'ньем -- -е'ньем     123 вдохновеньем, наслажденьем, умиленьем, презреньем…
    12 -е'нье -- -е'нье       282 вдохновенье, мгновенье, провиденье, волненье, нас…
    13 -е'нья -- -е'нья       285 вдохновенья, наслажденья, волненья, мгновенья, тв…

Intersections of all

``` r
length(intersect(less_var_pos_f, less_var_feats_f))
```

    [1] 11

``` r
length(intersect(less_var_pos_f, less_var_words_f))
```

    [1] 8

``` r
length(intersect(less_var_feats_f, less_var_words_f))
```

    [1] 5

``` r
length(intersect(intersect(less_var_pos_f, less_var_feats_f), less_var_words_f))
```

    [1] 3

``` r
# the most monotonous masc rhyme endings
intersect(intersect(less_var_pos_f, less_var_feats_f), less_var_words_f)
```

    [1] "-о'чи -- -о'чи"       "-и'ца -- -и'ца"       "-а'дость -- -а'дость"

``` r
fem_words %>% 
  filter(ending_pair %in% intersect(
    intersect(
      less_var_pos_f, less_var_feats_f), 
    less_var_words_f)
    ) %>% 
  group_by(ending_pair) %>% 
  count(word, sort = T) %>% 
  select(-n) %>% 
  summarise(n_var = n(),
            word_list = paste0(word, collapse = ", ")) %>% 
  arrange(-desc(n_var))
```

    # A tibble: 3 × 3
      ending_pair          n_var word_list                                          
      <chr>                <int> <chr>                                              
    1 -а'дость -- -а'дость     3 радость, младость, сладость                        
    2 -о'чи -- -о'чи           3 ночи, очи, колочи                                  
    3 -и'ца -- -и'ца          39 царица, денница, девица, птица, темница, столица, …

NB both for masc & fem rhymes it seems that THE most freq ending is also
the most variative (-oi, -oju),

however, some of the endings have very low N of pos/feats combinations
(-ить) but can generate very large number of words, these are mostly
verbal endings =\> seeming refusal to use this kind of rhymes

the most redundant / monotonous are though NOUN endings and rhyme pairs,
which are less plausible to include many words for non-freq endings
(such as -ец / -ца) though they include a small number of very symbolic
poetic words (венец, певец… любовь кровь… пир мир…)
