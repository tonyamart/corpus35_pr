# 05.2. Code - rhyme words POS

## 5.2.1. POS in rhymes - word level

## load data & pckg

``` r
library(tidyverse)
library(tidytext)

# library(kableExtra)
# library(umap)

library(MetBrewer)
library(patchwork)
theme_set(theme_minimal())
```

### meta

Import metadata

``` r
meta <- read.csv("../../data/corpus1835/sql_db/texts_metadata.csv")
glimpse(meta)
```

    Rows: 4,797
    Columns: 11
    $ text_id       <chr> "P_1", "P_10", "P_100", "P_1000", "P_1001", "P_1002", "P…
    $ source_id     <chr> "Per_1", "Per_2", "Per_3", "Per_4", "Per_4", "Per_4", "P…
    $ A_ID          <chr> "", "A_50", "A_7", "A_41", "A_139", "A_11", "A_163", "A_…
    $ text_title    <chr> "Солдатская песня", "Молния", "Ночлег чумаков", "Утешите…
    $ text_subtitle <chr> "", "", "Сельские картины", "", "", "", "", "", "", "", …
    $ first_line    <chr> "Ох жизнь, молодецкая", "Зачем с небесной высоты", "В бл…
    $ text_page     <chr> "C. 46", "C. 21", "C. 9-12", "C. 172-174", "C. 175-176",…
    $ corpus        <chr> "per", "per", "per", "per", "per", "per", "per", "per", …
    $ meter         <chr> "Other", "Iamb", "Iamb", "Iamb", "Trochee", "Iamb", "Tro…
    $ feet          <chr> "other", "3", "4", "4", "4", "4", "other", "4", "6", "5"…
    $ n_lines       <int> 38, 16, 98, 77, 28, 12, 44, 25, 31, 28, 100, 16, 17, 60,…

Meter labels

``` r
table(meta$meter)
```


    Amphibrach    Anapest     Dactyl       Iamb      Other    Trochee 
           429        142         89       3055        206        876 

``` r
meter_lables <- meta %>% 
  select(text_id, meter) %>% distinct()

head(meter_lables)
```

      text_id   meter
    1     P_1   Other
    2    P_10    Iamb
    3   P_100    Iamb
    4  P_1000    Iamb
    5  P_1001 Trochee
    6  P_1002    Iamb

### rhyme pairs

Import data & merge words + grammatical features

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

#### attach meter info

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
    Columns: 5
    $ text_id    <chr> "P_1938", "P_1938", "P_1938", "C_156__20", "C_156__20", "C_…
    $ from       <chr> "краса", "огневым", "красавицей", "око", "силки", "стонет",…
    $ to         <chr> "небеса", "земным", "красавице", "высоко", "легки", "догони…
    $ rhyme_alph <chr> "краса небеса", "земным огневым", "красавице красавицей", "…
    $ meter      <chr> "Other", "Other", "Other", "Trochee", "Trochee", "Trochee",…

### rhyme words

``` r
rhyme_words <- read.csv("../../data/corpus1835/sql_db/rhyme_words_upd.csv", 
                        
                        # DON'T LET R EAT IAMBS AND DO INTEGER 01 => 1
                        colClasses = c("stress_pattern" = "character",
                                       "closure_pattern" = "character")) 

glimpse(rhyme_words)
```

    Rows: 34,801
    Columns: 8
    $ word            <chr> "краса", "огневым", "красавицей", "око", "силки", "сто…
    $ word_acc        <chr> "краса'", "огневы'м", "краса'вицей", "о'ко", "силки'",…
    $ stress_pattern  <chr> "01", "001", "0100", "10", "01", "10", "1", "10", "010…
    $ closure_pattern <chr> "1", "1", "100", "10", "1", "10", "1", "10", "10", "1"…
    $ closure         <chr> "masc", "masc", "dactylic", "fem", "masc", "fem", "mas…
    $ pos             <chr> "S", "S", "S", "S", "S", "V", "S", "S", "S", "S", "APR…
    $ feats           <chr> "S,жен,неод=им,ед", "S,фам,муж,од=(дат,мн|твор,ед)", "…
    $ ending_st       <chr> "са'", "ы'м", "а'вицей", "о'ко", "ки'", "о'нет", "о'р"…

``` r
# check if all words are unique
length(unique(rhyme_words$word))
```

    [1] 34801

``` r
nrow(rhyme_words)
```

    [1] 34801

#### rewrite POS tags

``` r
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
```

#### extract inf & other verb forms

Infinitives

``` r
rhyme_words %>% 
  filter(pos == "VERB") %>% 
  select(feats) %>% 
  filter(str_detect(feats, "инф")) %>% count(feats, sort = T) %>% head()
```

               feats   n
    1   V,пе=инф,сов 270
    2 V,несов,нп=инф 219
    3 V,несов,пе=инф 204
    4   V,нп=инф,сов 154
    5 V,пе=инф,несов 137
    6      V=инф,сов 137

Imperatives

``` r
rhyme_words %>% 
  filter(pos == "VERB") %>% 
  select(feats) %>% 
  filter(str_detect(feats, "пов")) %>% 
  count(feats, sort = T) %>% head()
```

                      feats  n
    1   V,пе=ед,пов,2-л,сов 84
    2 V,несов,нп=ед,пов,2-л 80
    3 V,несов,пе=ед,пов,2-л 68
    4      V=ед,пов,2-л,сов 56
    5   V,нп=ед,пов,2-л,сов 55
    6   V,сов,пе=ед,пов,2-л 44

``` r
# VERB - пов
# VERB - деепр
# VERB - прич
```

``` r
rhyme_words %>% 
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
                      pos)
         ) %>% 
  count(pos, sort = T) # check if rewritten properly
```

                 pos     n
    1           NOUN 13592
    2           VERB 10109
    3            ADJ  5383
    4       VERB_inf  1553
    5     VERB_prich  1302
    6       VERB_imp   893
    7            ADV   781
    8  VERB_deeprich   760
    9           PRON   289
    10           NUM    52
    11          PART    29
    12          INTJ    25
    13          <NA>    15
    14           ADP    10
    15          CONJ     8

Rewrite tags

``` r
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
```

#### 

# words

### data preparation

Merge the rhyme pairs & words features

First word

``` r
nrow(rhyme_pairs) # Number of actual pairs (should not be exceeded by either of words tables)
```

    [1] 81247

``` r
word_1 <- rhyme_pairs %>% 
  select(text_id, meter, rhyme_alph, from) %>% 
  rename(word = from) %>% 
  left_join(rhyme_words, by = "word") 

nrow(word_1)
```

    [1] 81247

``` r
head(word_1, 10)
```

         text_id   meter           rhyme_alph       word    word_acc stress_pattern
    1     P_1938   Other         краса небеса      краса      краса'             01
    2     P_1938   Other       земным огневым    огневым    огневы'м            001
    3     P_1938   Other красавице красавицей красавицей краса'вицей           0100
    4  C_156__20 Trochee           высоко око        око        о'ко             10
    5  C_156__20 Trochee          легки силки      силки      силки'             01
    6  C_156__20 Trochee       догонит стонет     стонет     сто'нет             10
    7  C_156__20 Trochee         простор спор       спор       спо'р              1
    8  C_156__20 Trochee        гремучий тучи       тучи       ту'чи             10
    9  C_156__20 Trochee        заране тумане     тумане     тума'не            010
    10 C_156__20 Trochee           лучом сном       сном       сно'м              1
       closure_pattern  closure old_tag                            feats ending_st
    1                1     masc       S                 S,жен,неод=им,ед       са'
    2                1     masc       S    S,фам,муж,од=(дат,мн|твор,ед)       ы'м
    3              100 dactylic       S                 S,жен,од=твор,ед   а'вицей
    4               10      fem       S       S,сред,неод=(вин,ед|им,ед)      о'ко
    5                1     masc       S        S,муж,неод=(вин,мн|им,мн)       ки'
    6               10      fem       V   V,несов,нп=непрош,ед,изъяв,3-л     о'нет
    7                1     masc       S        S,муж,неод=(вин,ед|им,ед)       о'р
    8               10      fem       S S,жен,неод=(вин,мн|род,ед|им,мн)      у'чи
    9               10      fem       S                 S,муж,неод=пр,ед      а'не
    10               1     masc       S               S,муж,неод=твор,ед       о'м
        pos
    1  NOUN
    2  NOUN
    3  NOUN
    4  NOUN
    5  NOUN
    6  VERB
    7  NOUN
    8  NOUN
    9  NOUN
    10 NOUN

``` r
# check errors
word_1 %>% 
  filter(is.na(word_acc)) %>% head()
```

        text_id      meter rhyme_alph word word_acc stress_pattern closure_pattern
    1     P_530 Amphibrach   __ дочки   __     <NA>           <NA>            <NA>
    2    P_1468    Anapest    _ зарей    _     <NA>           <NA>            <NA>
    3    P_1722       Iamb      ж нее    ж     <NA>           <NA>            <NA>
    4  C_84__21       Iamb _ предмете    _     <NA>           <NA>            <NA>
    5 C_315__19       Iamb  ль печаль   ль     <NA>           <NA>            <NA>
    6     P_409       Iamb     __ гор   __     <NA>           <NA>            <NA>
      closure old_tag feats ending_st  pos
    1    <NA>    <NA>  <NA>      <NA> <NA>
    2    <NA>    <NA>  <NA>      <NA> <NA>
    3    <NA>    <NA>  <NA>      <NA> <NA>
    4    <NA>    <NA>  <NA>      <NA> <NA>
    5    <NA>    <NA>  <NA>      <NA> <NA>
    6    <NA>    <NA>  <NA>      <NA> <NA>

``` r
word_1 %>% 
  filter(is.na(pos)) %>% head()
```

       text_id      meter     rhyme_alph     word  word_acc stress_pattern
    1    P_530 Amphibrach       __ дочки       __      <NA>           <NA>
    2 C_95__21       Iamb конец мертвець мертвець мертве'ць             01
    3   P_1468    Anapest        _ зарей        _      <NA>           <NA>
    4   P_1722       Iamb          ж нее        ж      <NA>           <NA>
    5   P_1905    Trochee  оплечью речъю    речъю    речъю'             01
    6 C_117__5       Iamb         мы тмы      тмы      тмы'              1
      closure_pattern closure old_tag feats ending_st  pos
    1            <NA>    <NA>    <NA>  <NA>      <NA> <NA>
    2               1    masc    <NA>  <NA>      е'ць <NA>
    3            <NA>    <NA>    <NA>  <NA>      <NA> <NA>
    4            <NA>    <NA>    <NA>  <NA>      <NA> <NA>
    5               1    masc    <NA>  <NA>       ъю' <NA>
    6               1    masc    <NA>  <NA>       мы' <NA>

``` r
# remove errors
word_1 <- word_1 %>% 
  filter(!is.na(word_acc) & !is.na(pos))
```

Second word

``` r
word_2 <- rhyme_pairs %>% 
  select(text_id, meter, rhyme_alph, to) %>% 
  rename(word = to) %>% 
  left_join(rhyme_words, by = "word")

nrow(word_2) == nrow(rhyme_pairs) # must be true
```

    [1] TRUE

``` r
head(word_2, 10)
```

         text_id   meter           rhyme_alph      word   word_acc stress_pattern
    1     P_1938   Other         краса небеса    небеса    небеса'            001
    2     P_1938   Other       земным огневым    земным    земны'м             01
    3     P_1938   Other красавице красавицей красавице краса'вице           0100
    4  C_156__20 Trochee           высоко око    высоко    высоко'            001
    5  C_156__20 Trochee          легки силки     легки     легки'             01
    6  C_156__20 Trochee       догонит стонет   догонит   дого'нит            010
    7  C_156__20 Trochee         простор спор   простор   просто'р             01
    8  C_156__20 Trochee        гремучий тучи  гремучий  грему'чий            010
    9  C_156__20 Trochee        заране тумане    заране    зара'не            010
    10 C_156__20 Trochee           лучом сном     лучом     лучо'м             01
       closure_pattern  closure old_tag
    1                1     masc       S
    2                1     masc       A
    3              100 dactylic       S
    4                1     masc     ADV
    5                1     masc       A
    6               10      fem       V
    7                1     masc       S
    8               10      fem       A
    9               10      fem       S
    10               1     masc       S
                                                    feats ending_st  pos
    1                          S,сред,неод=(вин,мн|им,мн)       са' NOUN
    2  A=(дат,мн,полн|твор,ед,полн,муж|твор,ед,полн,сред)       ы'м  ADJ
    3                             S,жен,од=(пр,ед|дат,ед)    а'вице NOUN
    4                                                ADV=       ко'  ADV
    5                                             A=мн,кр       ки'  ADJ
    6                        V,сов,пе=непрош,ед,изъяв,3-л     о'нит VERB
    7                           S,муж,неод=(вин,ед|им,ед)       о'р NOUN
    8             A=(вин,ед,полн,муж,неод|им,ед,полн,муж)     у'чий  ADJ
    9                         S,имя,жен,од=(пр,ед|дат,ед)      а'не NOUN
    10                                 S,муж,неод=твор,ед       о'м NOUN

``` r
# check errors
word_2 %>% 
  filter(is.na(word_acc)) %>% head()
```

         text_id      meter rhyme_alph word word_acc stress_pattern closure_pattern
    1     P_1520 Amphibrach      ж мне    ж     <NA>           <NA>            <NA>
    2   C_68__74       Iamb  ль печаль   ль     <NA>           <NA>            <NA>
    3  C_633__55       Iamb    садов ф    ф     <NA>           <NA>            <NA>
    4   C_180__5       Iamb    благ ль   ль     <NA>           <NA>            <NA>
    5  C_315__16       Iamb  ль печаль   ль     <NA>           <NA>            <NA>
    6 C_633__125       Iamb  ж стороне    ж     <NA>           <NA>            <NA>
      closure old_tag feats ending_st  pos
    1    <NA>    <NA>  <NA>      <NA> <NA>
    2    <NA>    <NA>  <NA>      <NA> <NA>
    3    <NA>    <NA>  <NA>      <NA> <NA>
    4    <NA>    <NA>  <NA>      <NA> <NA>
    5    <NA>    <NA>  <NA>      <NA> <NA>
    6    <NA>    <NA>  <NA>      <NA> <NA>

``` r
word_2 %>% 
  filter(is.na(pos)) %>% head()
```

        text_id      meter      rhyme_alph   word word_acc stress_pattern
    1    P_1520 Amphibrach           ж мне      ж     <NA>           <NA>
    2  C_156__3    Trochee покорныи черныи черныи  черны'и            010
    3  C_68__74       Iamb       ль печаль     ль     <NA>           <NA>
    4 C_633__55       Iamb         садов ф      ф     <NA>           <NA>
    5    P_1836 Amphibrach      вдруг рукь   рукь    ру'кь              1
    6  C_180__5       Iamb         благ ль     ль     <NA>           <NA>
      closure_pattern closure old_tag feats ending_st  pos
    1            <NA>    <NA>    <NA>  <NA>      <NA> <NA>
    2              10     fem    <NA>  <NA>       ы'и <NA>
    3            <NA>    <NA>    <NA>  <NA>      <NA> <NA>
    4            <NA>    <NA>    <NA>  <NA>      <NA> <NA>
    5               1    masc    <NA>  <NA>      у'кь <NA>
    6            <NA>    <NA>    <NA>  <NA>      <NA> <NA>

``` r
# remove errors
word_2 <- word_2 %>% 
  filter(!is.na(word_acc) & !is.na(pos))

nrow(word_2)
```

    [1] 81233

Merge all from-to words in one table

``` r
all_words <- rbind(word_1, word_2)
```

Remove obsolete data

``` r
rm(meter_lables, rhyme_words, word_1, word_2)
```

### Basic stats

``` r
table(all_words$meter)
```


    Amphibrach    Anapest     Dactyl       Iamb      Other    Trochee 
         12996       3891       1874     108752       4251      30687 

#### MFW in rhymes

Errors: hyperdactyllic edings

``` r
all_words %>% 
  filter(closure == "other") %>% 
  select(word_acc, closure_pattern) %>% 
  #distinct() %>% 
  nrow()
# 148 words
# mostly wrongly annotated stresses (but very small number of words)
```

MFW iamb & trochee

``` r
closures <- c("masc", "fem", "dactylic")

all_words %>% 
  filter(meter %in% c("Iamb", "Trochee") & closure %in% closures) %>% 
  group_by(meter, closure) %>% 
  count(word_acc, sort = T) %>% 
  slice_max(order_by = n, n = 5) %>% 
  filter(closure == "fem")
```

    # A tibble: 10 × 4
    # Groups:   meter, closure [2]
       meter   closure word_acc     n
       <chr>   <chr>   <chr>    <int>
     1 Iamb    fem     тобо'ю     181
     2 Iamb    fem     мно'ю      174
     3 Iamb    fem     све'та     152
     4 Iamb    fem     душо'ю     148
     5 Iamb    fem     сла'вы     148
     6 Trochee fem     о'чи        66
     7 Trochee fem     но'чи       61
     8 Trochee fem     мо'ре       44
     9 Trochee fem     во'лны      41
    10 Trochee fem     мно'ю       37

``` r
all_words %>% 
  filter(meter %in% c("Iamb", "Trochee") & closure %in% closures) %>% 
  group_by(meter, closure) %>% 
  count(word_acc, sort = T) %>% 
  slice_max(order_by = n, n = 5) %>% 
  filter(closure == "masc")
```

    # A tibble: 10 × 4
    # Groups:   meter, closure [2]
       meter   closure word_acc     n
       <chr>   <chr>   <chr>    <int>
     1 Iamb    masc    я'         484
     2 Iamb    masc    све'т      325
     3 Iamb    masc    меня'      318
     4 Iamb    masc    тобо'й     300
     5 Iamb    masc    она'       299
     6 Trochee masc    я'         134
     7 Trochee masc    она'       117
     8 Trochee masc    мне'        96
     9 Trochee masc    меня'       86
    10 Trochee masc    све'т       86

``` r
all_words %>% 
  filter(meter %in% c("Iamb", "Trochee") & closure %in% closures) %>% 
  group_by(meter, closure) %>% 
  count(word_acc, sort = T) %>% 
  slice_max(order_by = n, n = 5) %>% 
  filter(closure == "dactylic")
```

    # A tibble: 14 × 4
    # Groups:   meter, closure [2]
       meter   closure  word_acc           n
       <chr>   <chr>    <chr>          <int>
     1 Iamb    dactylic рети'вое           9
     2 Iamb    dactylic ка'тится           8
     3 Iamb    dactylic окрова'вленный     8
     4 Iamb    dactylic го'лоса            7
     5 Iamb    dactylic кру'жится          7
     6 Iamb    dactylic про'снется         7
     7 Trochee dactylic вопию'щего        11
     8 Trochee dactylic зову'щего         11
     9 Trochee dactylic печа'льная         6
    10 Trochee dactylic жела'ния           5
    11 Trochee dactylic ненагля'дная       5
    12 Trochee dactylic ра'дости           5
    13 Trochee dactylic страда'ния         5
    14 Trochee dactylic та'йною            5

MFW 3-syll meters

``` r
all_words %>% 
  filter(meter %in% c("Dactyl", "Amphibrach", "Anapest") & closure %in% closures) %>% 
  group_by(meter, closure) %>% 
  count(word_acc, sort = T) %>% 
  slice_max(order_by = n, n = 5)
```

    # A tibble: 60 × 4
    # Groups:   meter, closure [9]
       meter      closure  word_acc          n
       <chr>      <chr>    <chr>         <int>
     1 Amphibrach dactylic ми'лого           5
     2 Amphibrach dactylic ру'сские          5
     3 Amphibrach dactylic азо'вского        4
     4 Amphibrach dactylic байка'льского     4
     5 Amphibrach dactylic до'брые           4
     6 Amphibrach dactylic каспи'йского      4
     7 Amphibrach dactylic ла'сточка         4
     8 Amphibrach fem      мо'ре            34
     9 Amphibrach fem      о'чи             29
    10 Amphibrach fem      но'чи            27
    # ℹ 50 more rows

#### total_meter

Total number of rhyme words in a meter

``` r
total_meter <- all_words %>% 
  count(meter) %>% 
  rename(total = n)

total_meter
```

           meter  total
    1 Amphibrach  12996
    2    Anapest   3891
    3     Dactyl   1874
    4       Iamb 108752
    5      Other   4251
    6    Trochee  30687

#### Number of masc / fem / dactyl endings

``` r
all_words %>% 
  filter(closure != "other" & meter != "Other") %>% 
  group_by(meter) %>% 
  count(closure) %>% 
  left_join(total_meter, by = "meter") %>% 
  mutate(perc = round((n / total) * 100, 2 ))
```

    # A tibble: 15 × 5
    # Groups:   meter [5]
       meter      closure      n  total  perc
       <chr>      <chr>    <int>  <int> <dbl>
     1 Amphibrach dactylic   381  12996  2.93
     2 Amphibrach fem       5472  12996 42.1 
     3 Amphibrach masc      7137  12996 54.9 
     4 Anapest    dactylic    53   3891  1.36
     5 Anapest    fem       1143   3891 29.4 
     6 Anapest    masc      2695   3891 69.3 
     7 Dactyl     dactylic   250   1874 13.3 
     8 Dactyl     fem        575   1874 30.7 
     9 Dactyl     masc      1044   1874 55.7 
    10 Iamb       dactylic  1324 108752  1.22
    11 Iamb       fem      53075 108752 48.8 
    12 Iamb       masc     54284 108752 49.9 
    13 Trochee    dactylic  1103  30687  3.59
    14 Trochee    fem      14091  30687 45.9 
    15 Trochee    masc     15444  30687 50.3 

``` r
# all_words %>% 
#   filter(meter == "Dactyl" & closure == "masc") %>% 
#   select(text_id, word_acc, closure_pattern) %>% sample_n(10)

# meta %>% 
#   filter(text_id == "P_294")
```

### top endings in meters

#### iamb_closures

Number of words of a particular clausula type IN iambic texts

``` r
iamb_closures <- all_words %>% 
  filter(meter == "Iamb") %>% 
  count(closure) %>% 
  rename(total_closure = n) 
```

``` r
all_words %>% 
  filter(meter == "Iamb" & closure != "other") %>% 
  group_by(meter, closure) %>% 
  count(ending_st) %>% 
  slice_max(order_by = n, n = 5) %>% 
  ungroup() %>% 
  left_join(total_meter, by = "meter") %>% 
  # percentage of all words in iamb
  mutate(perc_all_iambic = round((n / total) * 100, 2 )) %>% 
  left_join(iamb_closures, by = "closure") %>% 
  
  # percentage of word of particular clausula type
  mutate(perc_iamb_clos = round((n / total_closure) * 100, 2 )) %>% 
  select(-total, -total_closure)
```

    # A tibble: 17 × 6
       meter closure  ending_st     n perc_all_iambic perc_iamb_clos
       <chr> <chr>    <chr>     <int>           <dbl>          <dbl>
     1 Iamb  dactylic е'ния        20            0.02           1.51
     2 Iamb  dactylic е'ние        14            0.01           1.06
     3 Iamb  dactylic а'ния        13            0.01           0.98
     4 Iamb  dactylic а'тится      10            0.01           0.76
     5 Iamb  dactylic а'рая         9            0.01           0.68
     6 Iamb  dactylic и'вое         9            0.01           0.68
     7 Iamb  dactylic у'жится       9            0.01           0.68
     8 Iamb  fem      о'ю        2236            2.06           4.21
     9 Iamb  fem      а'ми       1629            1.5            3.07
    10 Iamb  fem      е'нья      1492            1.37           2.81
    11 Iamb  fem      а'ет       1451            1.33           2.73
    12 Iamb  fem      е'нье      1227            1.13           2.31
    13 Iamb  masc     о'й        5258            4.83           9.69
    14 Iamb  masc     е'й        3155            2.9            5.81
    15 Iamb  masc     а'л        1874            1.72           3.45
    16 Iamb  masc     и'т        1526            1.4            2.81
    17 Iamb  masc     на'        1446            1.33           2.66

``` r
all_words %>% 
  filter(closure == "dactylic") %>% count(word_acc, sort = T)
```

#### trochee_closures

Number of words of a particular clausula type IN trochaic texts

``` r
trochee_closures <- all_words %>% 
  filter(meter == "Trochee") %>% 
  count(closure) %>% 
  rename(total_closure = n) 
```

``` r
all_words %>% 
  filter(meter == "Trochee" & closure != "other") %>% 
  group_by(meter, closure) %>% 
  count(ending_st) %>% 
  slice_max(order_by = n, n = 5) %>% 
  ungroup() %>% 
  left_join(total_meter, by = "meter") %>% 
  # percentage of all words in iamb
  mutate(perc_all_tr = round((n / total) * 100, 2 )) %>% 
  left_join(trochee_closures, by = "closure") %>% 
  
  # percentage of word of particular clausula type
  mutate(perc_tr_clos = round((n / total_closure) * 100, 2 )) %>% 
  select(-total, -total_closure)
```

    # A tibble: 17 × 6
       meter   closure  ending_st     n perc_all_tr perc_tr_clos
       <chr>   <chr>    <chr>     <int>       <dbl>        <dbl>
     1 Trochee dactylic е'ния        21        0.07         1.9 
     2 Trochee dactylic а'ния        20        0.07         1.81
     3 Trochee dactylic а'ется       13        0.04         1.18
     4 Trochee dactylic ё'нная       12        0.04         1.09
     5 Trochee dactylic е'ние        11        0.04         1   
     6 Trochee dactylic у'щего       11        0.04         1   
     7 Trochee dactylic ю'щего       11        0.04         1   
     8 Trochee fem      о'ю         506        1.65         3.59
     9 Trochee fem      а'я         429        1.4          3.04
    10 Trochee fem      а'ет        396        1.29         2.81
    11 Trochee fem      а'ми        366        1.19         2.6 
    12 Trochee fem      е'нья       219        0.71         1.55
    13 Trochee masc     о'й        1387        4.52         8.98
    14 Trochee masc     е'й         816        2.66         5.28
    15 Trochee masc     и'т         591        1.93         3.83
    16 Trochee masc     на'         498        1.62         3.22
    17 Trochee masc     ё'т         425        1.38         2.75

## POS distribution in meters / endings

#### tab. 5.2.1 - all meters & clausulas together

``` r
all_words %>% 
  filter(closure != "other") %>% 
  count(pos) %>% 
  mutate(perc_total = round(
    n / nrow(all_words %>% filter(closure != "other")) * 100, 
    2))
```

                 pos     n perc_total
    1            ADJ 19385      11.94
    2            ADP    27       0.02
    3            ADV  4647       2.86
    4           CONJ    15       0.01
    5           INTJ   140       0.09
    6           NOUN 83162      51.24
    7            NUM   144       0.09
    8           PART   646       0.40
    9           PRON 12681       7.81
    10          VERB 31207      19.23
    11 VERB_deeprich  1784       1.10
    12      VERB_imp  1647       1.01
    13      VERB_inf  4268       2.63
    14    VERB_prich  2550       1.57

Masc

``` r
all_words %>% 
  filter(closure == "masc") %>% 
  count(pos) %>% 
  mutate(perc_total = round(
    n / nrow(all_words %>% filter(closure == "masc")) * 100, 
    2))
```

                 pos     n perc_total
    1            ADJ  4808       5.82
    2            ADP    24       0.03
    3            ADV  2261       2.73
    4           CONJ    11       0.01
    5           INTJ   105       0.13
    6           NOUN 43022      52.03
    7            NUM    67       0.08
    8           PART   601       0.73
    9           PRON 11259      13.62
    10          VERB 14398      17.41
    11 VERB_deeprich   475       0.57
    12      VERB_imp  1153       1.39
    13      VERB_inf  3457       4.18
    14    VERB_prich  1038       1.26

Fem

``` r
all_words %>% 
  filter(closure == "fem") %>% 
  count(pos) %>% 
  mutate(perc_total = round(
    n / nrow(all_words %>% filter(closure == "fem")) * 100, 
    2))
```

                 pos     n perc_total
    1            ADJ 13039      17.16
    2            ADP     3       0.00
    3            ADV  2338       3.08
    4           CONJ     4       0.01
    5           INTJ    35       0.05
    6           NOUN 38822      51.08
    7            NUM    70       0.09
    8           PART    45       0.06
    9           PRON  1414       1.86
    10          VERB 16410      21.59
    11 VERB_deeprich  1280       1.68
    12      VERB_imp   476       0.63
    13      VERB_inf   790       1.04
    14    VERB_prich  1275       1.68

#### iamb

Total distribution of POS (all words)

``` r
all_words %>% 
  filter(meter == "Iamb" & closure != "other") %>% 
  count(meter, pos) %>% 
  left_join(total_meter, by = "meter") %>% 
  
  # percentage of all words in iamb
  mutate(perc_all_iambic = round((n / total) * 100, 2 )) %>% 
  select(-meter, -total) %>% 
  arrange(-desc(pos))
```

                 pos     n perc_all_iambic
    1            ADJ 12510           11.50
    2            ADP    17            0.02
    3            ADV  3078            2.83
    4           CONJ    15            0.01
    5           INTJ    76            0.07
    6           NOUN 56113           51.60
    7            NUM    92            0.08
    8           PART   462            0.42
    9           PRON  8564            7.87
    10          VERB 20671           19.01
    11 VERB_deeprich  1166            1.07
    12      VERB_imp  1109            1.02
    13      VERB_inf  2959            2.72
    14    VERB_prich  1851            1.70

By clausula

``` r
all_words %>% 
  filter(meter == "Iamb" & closure != "other") %>% 
  group_by(meter, closure) %>% 
  count(meter, pos) %>% 
  ungroup() %>% 
  #left_join(total_meter, by = "meter") %>% 
  
  # percentage of all words in iamb
  #mutate(perc_all_iambic = round((n / total) * 100, 2 )) # %>% 
  left_join(iamb_closures, by = "closure") %>% 
  # 
  # # percentage of word of particular clausula type
  mutate(perc_iamb_clos = round((n / total_closure) * 100, 2 )) %>% 
  select(-total_closure, -meter) %>% 
  filter(closure == "masc")
```

    # A tibble: 14 × 4
       closure pos               n perc_iamb_clos
       <chr>   <chr>         <int>          <dbl>
     1 masc    ADJ            3096           5.7 
     2 masc    ADP              15           0.03
     3 masc    ADV            1425           2.63
     4 masc    CONJ             11           0.02
     5 masc    INTJ             51           0.09
     6 masc    NOUN          28393          52.3 
     7 masc    NUM              45           0.08
     8 masc    PART            430           0.79
     9 masc    PRON           7516          13.8 
    10 masc    VERB           9114          16.8 
    11 masc    VERB_deeprich   312           0.57
    12 masc    VERB_imp        768           1.41
    13 masc    VERB_inf       2343           4.32
    14 masc    VERB_prich      765           1.41

``` r
all_words %>% 
  filter(meter == "Iamb" & closure != "other") %>% 
  group_by(meter, closure) %>% 
  count(meter, pos) %>% 
  ungroup() %>% 
  #left_join(total_meter, by = "meter") %>% 
  
  # percentage of all words in iamb
  #mutate(perc_all_iambic = round((n / total) * 100, 2 )) # %>% 
  left_join(iamb_closures, by = "closure") %>% 
  # 
  # # percentage of word of particular clausula type
  mutate(perc_iamb_clos = round((n / total_closure) * 100, 2 )) %>% 
  select(-total_closure, -meter) %>% 
  filter(closure == "fem")
```

    # A tibble: 14 × 4
       closure pos               n perc_iamb_clos
       <chr>   <chr>         <int>          <dbl>
     1 fem     ADJ            9071          17.1 
     2 fem     ADP               2           0   
     3 fem     ADV            1631           3.07
     4 fem     CONJ              4           0.01
     5 fem     INTJ             25           0.05
     6 fem     NOUN          27119          51.1 
     7 fem     NUM              44           0.08
     8 fem     PART             32           0.06
     9 fem     PRON           1043           1.97
    10 fem     VERB          11363          21.4 
    11 fem     VERB_deeprich   838           1.58
    12 fem     VERB_imp        330           0.62
    13 fem     VERB_inf        610           1.15
    14 fem     VERB_prich      963           1.81

``` r
all_words %>% 
  filter(meter == "Iamb" & closure != "other") %>% 
  group_by(meter, closure) %>% 
  count(meter, pos) %>% 
  ungroup() %>% 
  #left_join(total_meter, by = "meter") %>% 
  
  # percentage of all words in iamb
  #mutate(perc_all_iambic = round((n / total) * 100, 2 )) # %>% 
  left_join(iamb_closures, by = "closure") %>% 
  # 
  # # percentage of word of particular clausula type
  mutate(perc_iamb_clos = round((n / total_closure) * 100, 2 )) %>% 
  select(-total_closure, -meter) %>% 
  filter(closure == "dactylic")
```

    # A tibble: 10 × 4
       closure  pos               n perc_iamb_clos
       <chr>    <chr>         <int>          <dbl>
     1 dactylic ADJ             343          25.9 
     2 dactylic ADV              22           1.66
     3 dactylic NOUN            601          45.4 
     4 dactylic NUM               3           0.23
     5 dactylic PRON              5           0.38
     6 dactylic VERB            194          14.6 
     7 dactylic VERB_deeprich    16           1.21
     8 dactylic VERB_imp         11           0.83
     9 dactylic VERB_inf          6           0.45
    10 dactylic VERB_prich      123           9.29

#### trochee

Total POS

``` r
all_words %>% 
  filter(meter == "Trochee" & closure != "other") %>% 
  count(meter, pos) %>% 
  left_join(total_meter, by = "meter") %>% 
  
  # percentage of all words in iamb
  mutate(perc_all_iambic = round((n / total) * 100, 2 )) %>% 
  select(-meter, -total) %>% 
  arrange(-desc(pos))
```

                 pos     n perc_all_iambic
    1            ADJ  4113           13.40
    2            ADP     5            0.02
    3            ADV   948            3.09
    4           INTJ    33            0.11
    5           NOUN 15654           51.01
    6            NUM    26            0.08
    7           PART   104            0.34
    8           PRON  2206            7.19
    9           VERB  5734           18.69
    10 VERB_deeprich   330            1.08
    11      VERB_imp   303            0.99
    12      VERB_inf   740            2.41
    13    VERB_prich   442            1.44

By clausula

``` r
all_words %>% 
  filter(meter == "Trochee" & closure != "other") %>% 
  group_by(meter, closure) %>% 
  count(meter, pos) %>% 
  ungroup() %>% 
  #left_join(total_meter, by = "meter") %>% 
  
  # percentage of all words in iamb
  #mutate(perc_all_iambic = round((n / total) * 100, 2 )) # %>% 
  left_join(trochee_closures, by = "closure") %>% 
  # 
  # # percentage of word of particular clausula type
  mutate(perc_tr_clos = round((n / total_closure) * 100, 2 )) %>% 
  select(-total_closure, -meter, -n) %>% 
  arrange(desc(perc_tr_clos)) %>% 
  pivot_wider(names_from = closure, values_from = perc_tr_clos)
```

    # A tibble: 13 × 4
       pos            masc   fem dactylic
       <chr>         <dbl> <dbl>    <dbl>
     1 NOUN          52.3  51.2     32.7 
     2 ADJ            6.03 18.8     49.0 
     3 VERB          18.0  20.3      8.52
     4 PRON          12.8   1.58     0.09
     5 VERB_prich     1.12  1.33     7.43
     6 VERB_inf       4.06  0.79     0.09
     7 ADV            2.96  3.41     1   
     8 VERB_deeprich  0.46  1.8      0.54
     9 VERB_imp       1.35  0.63     0.45
    10 PART           0.62  0.06    NA   
    11 INTJ           0.19  0.03    NA   
    12 NUM            0.06  0.1      0.18
    13 ADP            0.03  0.01    NA   

### POS length (in syl)

How it works:

``` r
#glimpse(all_words)

all_words$stress_pattern[3]

unlist(gregexpr(pattern = "1", "0100")) # stress position
nchar("0100") # length of the word


all_words %>% 
  sample_n(10) %>% 
  select(word, pos, stress_pattern) %>% 
  mutate(stress_position = unlist(gregexpr(pattern = "1", stress_pattern)),
         syl_len = nchar(stress_pattern)
         )
```

Altogether

``` r
all_words %>% 
  select(pos, stress_pattern) %>% 
  mutate(stress_position = unlist(gregexpr(pattern = "1", stress_pattern)),
         syl_len = nchar(stress_pattern)
         ) %>% 
  group_by(pos) %>% 
  summarise(
            mean_stress_pos = round(mean(stress_position), 1),
            mean_syl_lenght = round(mean(syl_len), 1),
            
            med_stress_pos = median(stress_position),
            med_syl_length = median(syl_len)
            )
```

    # A tibble: 14 × 5
       pos           mean_stress_pos mean_syl_lenght med_stress_pos med_syl_length
       <chr>                   <dbl>           <dbl>          <dbl>          <dbl>
     1 ADJ                       2.2             3                2              3
     2 ADP                       2.1             2.3              2              2
     3 ADV                       1.9             2.5              2              2
     4 CONJ                      1.6             1.9              2              2
     5 INTJ                      1.6             1.8              2              2
     6 NOUN                      1.9             2.4              2              2
     7 NUM                       1.7             2.3              2              2
     8 PART                      1.1             1.2              1              1
     9 PRON                      1.6             1.7              2              2
    10 VERB                      2.3             2.8              2              3
    11 VERB_deeprich             2.4             3.2              2              3
    12 VERB_imp                  2.3             2.6              2              3
    13 VERB_inf                  2.3             2.5              2              3
    14 VERB_prich                2.6             3.3              2              3

Masc rhymes

``` r
m <- all_words %>% 
  filter(closure == "masc" & meter != "Other" & 
           pos %in% c("NOUN", "VERB", "ADJ", "PRON")) %>% 
  select(meter, pos, stress_pattern) %>% 
  mutate(stress_position = unlist(gregexpr(pattern = "1", stress_pattern)),
         syl_len = nchar(stress_pattern)
         ) %>% 
  group_by(meter, pos) %>% 
  summarise(
            mean_stress_pos = round(mean(stress_position), 1),
            mean_syl_lenght = round(mean(syl_len), 1),
            
            med_stress_pos = median(stress_position),
            med_syl_length = median(syl_len)
            ) %>% ungroup()
```

    `summarise()` has grouped output by 'meter'. You can override using the
    `.groups` argument.

``` r
m
```

    # A tibble: 20 × 6
       meter     pos   mean_stress_pos mean_syl_lenght med_stress_pos med_syl_length
       <chr>     <chr>           <dbl>           <dbl>          <dbl>          <dbl>
     1 Amphibra… ADJ               2.3             2.3              2              2
     2 Amphibra… NOUN              1.9             1.9              2              2
     3 Amphibra… PRON              1.6             1.6              2              2
     4 Amphibra… VERB              2.2             2.2              2              2
     5 Anapest   ADJ               2.3             2.3              2              2
     6 Anapest   NOUN              1.9             1.9              2              2
     7 Anapest   PRON              1.7             1.7              2              2
     8 Anapest   VERB              2.3             2.3              2              2
     9 Dactyl    ADJ               2.1             2.1              2              2
    10 Dactyl    NOUN              1.9             1.9              2              2
    11 Dactyl    PRON              1.7             1.7              2              2
    12 Dactyl    VERB              2.3             2.3              2              2
    13 Iamb      ADJ               2.2             2.2              2              2
    14 Iamb      NOUN              1.9             1.9              2              2
    15 Iamb      PRON              1.6             1.6              2              2
    16 Iamb      VERB              2.3             2.3              2              2
    17 Trochee   ADJ               2.3             2.3              2              2
    18 Trochee   NOUN              1.9             1.9              2              2
    19 Trochee   PRON              1.6             1.6              2              2
    20 Trochee   VERB              2.3             2.3              2              2

#### masc pos_syl counter

``` r
masc_total <- all_words %>% filter(closure == "masc" & meter != "Other") %>% 
  count(meter) %>% 
  rename(total_meter = n)
  

all_words %>% 
  filter(closure == "masc" & meter != "Other" & 
           pos %in% c("NOUN", "VERB", "ADJ", "PRON")) %>% 
  select(meter, pos, stress_pattern) %>% 
  mutate(stress_position = unlist(gregexpr(pattern = "1", stress_pattern)),
         syl_len = nchar(stress_pattern),
         pos_syl = paste0(pos, "_", syl_len)
         ) %>% 
  count(meter, pos, pos_syl) %>% 
  left_join(masc_total, by = "meter") %>% 
  mutate(perc = round((n / total_meter)*100,1 )) %>% 
  select(meter, pos, pos_syl, perc, n, total_meter) %>% 
  filter(pos == "NOUN" & meter %in% c("Iamb", "Trochee")) 
```

         meter  pos pos_syl perc     n total_meter
    1     Iamb NOUN  NOUN_1 14.3  7756       54284
    2     Iamb NOUN  NOUN_2 28.9 15664       54284
    3     Iamb NOUN  NOUN_3  8.9  4818       54284
    4     Iamb NOUN  NOUN_4  0.3   153       54284
    5     Iamb NOUN  NOUN_5  0.0     2       54284
    6  Trochee NOUN  NOUN_1 13.2  2037       15444
    7  Trochee NOUN  NOUN_2 29.7  4588       15444
    8  Trochee NOUN  NOUN_3  9.2  1421       15444
    9  Trochee NOUN  NOUN_4  0.2    25       15444
    10 Trochee NOUN  NOUN_5  0.0     1       15444

``` r
  # NB other POS can be tested via filter
```

#### fem pos_syl

Fem rhymes

``` r
f <- all_words %>% 
  filter(closure == "fem" & meter != "Other" & 
           pos %in% c("NOUN", "VERB", "ADJ", "PRON")) %>% 
  select(meter, pos, stress_pattern) %>% 
  mutate(stress_position = unlist(gregexpr(pattern = "1", stress_pattern)),
         syl_len = nchar(stress_pattern)
         ) %>% 
  group_by(meter, pos) %>% 
  summarise(
            fem_mean_stress_pos = round(mean(stress_position), 1),
            fem_mean_syl_lenght = round(mean(syl_len), 1),
            
            med_stress_pos = median(stress_position),
            med_syl_length = median(syl_len)
            ) %>% ungroup()
```

    `summarise()` has grouped output by 'meter'. You can override using the
    `.groups` argument.

``` r
f
```

    # A tibble: 20 × 6
       meter      pos   fem_mean_stress_pos fem_mean_syl_lenght med_stress_pos
       <chr>      <chr>               <dbl>               <dbl>          <dbl>
     1 Amphibrach ADJ                   2.1                 3.1              2
     2 Amphibrach NOUN                  1.7                 2.7              2
     3 Amphibrach PRON                  1.5                 2.5              1
     4 Amphibrach VERB                  2.2                 3.2              2
     5 Anapest    ADJ                   2.1                 3.1              2
     6 Anapest    NOUN                  1.7                 2.7              2
     7 Anapest    PRON                  1.6                 2.6              2
     8 Anapest    VERB                  2.2                 3.2              2
     9 Dactyl     ADJ                   2.1                 3.1              2
    10 Dactyl     NOUN                  1.8                 2.8              2
    11 Dactyl     PRON                  1.2                 2.2              1
    12 Dactyl     VERB                  2.3                 3.3              2
    13 Iamb       ADJ                   2.2                 3.2              2
    14 Iamb       NOUN                  1.9                 2.9              2
    15 Iamb       PRON                  1.5                 2.5              1
    16 Iamb       VERB                  2.3                 3.3              2
    17 Trochee    ADJ                   2.1                 3.1              2
    18 Trochee    NOUN                  1.8                 2.8              2
    19 Trochee    PRON                  1.5                 2.5              1
    20 Trochee    VERB                  2.2                 3.2              2
    # ℹ 1 more variable: med_syl_length <dbl>

``` r
fem_total <- all_words %>% filter(closure == "fem" & meter != "Other") %>% 
  count(meter) %>% 
  rename(total_meter = n)
  

all_words %>% 
  filter(closure == "fem" & meter != "Other" & 
           pos %in% c("NOUN", "VERB", "ADJ", "PRON")) %>% 
  select(meter, pos, stress_pattern) %>% 
  mutate(stress_position = unlist(gregexpr(pattern = "1", stress_pattern)),
         syl_len = nchar(stress_pattern),
         pos_syl = paste0(pos, "_", syl_len)
         ) %>% 
  count(meter, pos, pos_syl) %>% 
  left_join(masc_total, by = "meter") %>% 
  mutate(perc = round((n / total_meter)*100,1 )) %>% 
  select(meter, pos, pos_syl, perc, n, total_meter) %>% 
  filter(pos == "VERB" & meter %in% c("Iamb", "Trochee")) 
```

         meter  pos pos_syl perc    n total_meter
    1     Iamb VERB  VERB_2  3.3 1781       54284
    2     Iamb VERB  VERB_3  8.6 4660       54284
    3     Iamb VERB  VERB_4  8.3 4514       54284
    4     Iamb VERB  VERB_5  0.7  407       54284
    5     Iamb VERB  VERB_6  0.0    1       54284
    6  Trochee VERB  VERB_2  4.0  612       15444
    7  Trochee VERB  VERB_3  7.9 1223       15444
    8  Trochee VERB  VERB_4  6.3  967       15444
    9  Trochee VERB  VERB_5  0.3   54       15444
    10 Trochee VERB  VERB_6  0.0    1       15444

dactylic

``` r
d <- all_words %>% 
  filter(pos %in% c("NOUN", "VERB", "ADJ", "PRON") & closure == "dactylic" & 
           meter != "Other") %>% 
  select(meter, pos, stress_pattern) %>% 
  mutate(stress_position = unlist(gregexpr(pattern = "1", stress_pattern)),
         syl_len = nchar(stress_pattern)
         ) %>% 
  group_by(meter, pos) %>% 
  summarise(
            d_mean_stress_pos = round(mean(stress_position), 1),
            d_mean_syl_lenght = round(mean(syl_len), 1),
            
            med_stress_pos = median(stress_position),
            med_syl_length = median(syl_len)
            ) %>% ungroup
```

    `summarise()` has grouped output by 'meter'. You can override using the
    `.groups` argument.

``` r
d
```

    # A tibble: 18 × 6
       meter pos   d_mean_stress_pos d_mean_syl_lenght med_stress_pos med_syl_length
       <chr> <chr>             <dbl>             <dbl>          <dbl>          <dbl>
     1 Amph… ADJ                 1.9               3.9            2              4  
     2 Amph… NOUN                1.6               3.6            1              3  
     3 Amph… VERB                1.8               3.8            2              4  
     4 Anap… ADJ                 2.2               4.2            2              4  
     5 Anap… NOUN                1.5               3.5            1              3  
     6 Anap… VERB                1.7               3.7            2              4  
     7 Dact… ADJ                 2                 4              2              4  
     8 Dact… NOUN                1.8               3.8            2              4  
     9 Dact… PRON                1                 3              1              3  
    10 Dact… VERB                2.3               4.3            2              4  
    11 Iamb  ADJ                 1.9               3.9            2              4  
    12 Iamb  NOUN                1.6               3.6            1              3  
    13 Iamb  PRON                1                 3              1              3  
    14 Iamb  VERB                1.6               3.6            1.5            3.5
    15 Troc… ADJ                 2                 4              2              4  
    16 Troc… NOUN                1.9               3.9            2              4  
    17 Troc… PRON                1                 3              1              3  
    18 Troc… VERB                1.8               3.8            2              4  

``` r
dact_total <- all_words %>% filter(closure == "dactylic" & meter != "Other") %>% 
  count(meter) %>% 
  rename(total_meter = n)
  

all_words %>% 
  filter(closure == "dactylic" & meter != "Other" & 
           pos %in% c("NOUN", "VERB", "ADJ", "PRON")) %>% 
  select(meter, pos, stress_pattern) %>% 
  mutate(stress_position = unlist(gregexpr(pattern = "1", stress_pattern)),
         syl_len = nchar(stress_pattern),
         pos_syl = paste0(pos, "_", syl_len)
         ) %>% 
  count(meter, pos, pos_syl) %>% 
  left_join(masc_total, by = "meter") %>% 
  mutate(perc = round((n / total_meter)*100,1 )) %>% 
  select(meter, pos, pos_syl, perc, n, total_meter) %>% 
  filter(pos == "ADJ" & meter %in% c("Iamb", "Trochee")) 
```

        meter pos pos_syl perc   n total_meter
    1    Iamb ADJ   ADJ_3  0.2  97       54284
    2    Iamb ADJ   ADJ_4  0.3 189       54284
    3    Iamb ADJ   ADJ_5  0.1  53       54284
    4    Iamb ADJ   ADJ_6  0.0   4       54284
    5 Trochee ADJ   ADJ_3  1.0 151       15444
    6 Trochee ADJ   ADJ_4  1.5 233       15444
    7 Trochee ADJ   ADJ_5  0.9 145       15444
    8 Trochee ADJ   ADJ_6  0.1  11       15444

``` r
glimpse(m)
```

    Rows: 20
    Columns: 6
    $ meter           <chr> "Amphibrach", "Amphibrach", "Amphibrach", "Amphibrach"…
    $ pos             <chr> "ADJ", "NOUN", "PRON", "VERB", "ADJ", "NOUN", "PRON", …
    $ mean_stress_pos <dbl> 2.3, 1.9, 1.6, 2.2, 2.3, 1.9, 1.7, 2.3, 2.1, 1.9, 1.7,…
    $ mean_syl_lenght <dbl> 2.3, 1.9, 1.6, 2.2, 2.3, 1.9, 1.7, 2.3, 2.1, 1.9, 1.7,…
    $ med_stress_pos  <dbl> 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, …
    $ med_syl_length  <dbl> 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, …

``` r
cbind(m %>% select(-med_stress_pos, -med_syl_length), 
      f$fem_mean_stress_pos, f$fem_mean_syl_lenght #,
      #d$d_mean_stress_pos, d$d_mean_syl_lenght
      ) %>% 
  select(meter, pos, 
         mean_syl_lenght, `f$fem_mean_syl_lenght`,
         mean_stress_pos, `f$fem_mean_stress_pos`)
```

            meter  pos mean_syl_lenght f$fem_mean_syl_lenght mean_stress_pos
    1  Amphibrach  ADJ             2.3                   3.1             2.3
    2  Amphibrach NOUN             1.9                   2.7             1.9
    3  Amphibrach PRON             1.6                   2.5             1.6
    4  Amphibrach VERB             2.2                   3.2             2.2
    5     Anapest  ADJ             2.3                   3.1             2.3
    6     Anapest NOUN             1.9                   2.7             1.9
    7     Anapest PRON             1.7                   2.6             1.7
    8     Anapest VERB             2.3                   3.2             2.3
    9      Dactyl  ADJ             2.1                   3.1             2.1
    10     Dactyl NOUN             1.9                   2.8             1.9
    11     Dactyl PRON             1.7                   2.2             1.7
    12     Dactyl VERB             2.3                   3.3             2.3
    13       Iamb  ADJ             2.2                   3.2             2.2
    14       Iamb NOUN             1.9                   2.9             1.9
    15       Iamb PRON             1.6                   2.5             1.6
    16       Iamb VERB             2.3                   3.3             2.3
    17    Trochee  ADJ             2.3                   3.1             2.3
    18    Trochee NOUN             1.9                   2.8             1.9
    19    Trochee PRON             1.6                   2.5             1.6
    20    Trochee VERB             2.3                   3.2             2.3
       f$fem_mean_stress_pos
    1                    2.1
    2                    1.7
    3                    1.5
    4                    2.2
    5                    2.1
    6                    1.7
    7                    1.6
    8                    2.2
    9                    2.1
    10                   1.8
    11                   1.2
    12                   2.3
    13                   2.2
    14                   1.9
    15                   1.5
    16                   2.3
    17                   2.1
    18                   1.8
    19                   1.5
    20                   2.2

``` r
rm(dact_total, fem_total, masc_total, m, d, f)
```

### POS groups

Grouping according to Shaw 2004 (p. 347)

``` r
unique(all_words$pos)
```

     [1] "NOUN"          "VERB"          "PRON"          "VERB_imp"     
     [5] "ADJ"           "ADV"           "VERB_inf"      "VERB_deeprich"
     [9] "VERB_prich"    "NUM"           "INTJ"          "PART"         
    [13] "ADP"           "CONJ"         

``` r
x <- tibble(pos = c("NOUN", "ADJ", "PRON", "VERB_prich", 
               
               "VERB", "VERB_inf", "VERB_imp",
               
               "ADV", "NUM", "INTJ", "ADP", "CONJ", "VERB_deeprich", "PART"
               ),
       pos_group = c("declined", "declined", "declined", "declined",
                     
                     "verbs", "verbs", "verbs", 
                     
                     "other", "other", "other", "other", "other", "other", "other"     
                     ))

nrow(x) == length(unique(all_words$pos))
```

    [1] TRUE

``` r
x
```

    # A tibble: 14 × 2
       pos           pos_group
       <chr>         <chr>    
     1 NOUN          declined 
     2 ADJ           declined 
     3 PRON          declined 
     4 VERB_prich    declined 
     5 VERB          verbs    
     6 VERB_inf      verbs    
     7 VERB_imp      verbs    
     8 ADV           other    
     9 NUM           other    
    10 INTJ          other    
    11 ADP           other    
    12 CONJ          other    
    13 VERB_deeprich other    
    14 PART          other    

``` r
all_words <- all_words %>% 
  left_join(x, by = "pos")
```

#### iamb

total POS

``` r
all_words %>% 
  filter(meter == "Iamb" & closure != "other") %>% 
  count(meter, pos_group) %>% 
  left_join(total_meter, by = "meter") %>% 
  
  # percentage of all words in iamb
  mutate(perc_all_iambic = round((n / total) * 100, 2 )) %>% 
  select(-meter, -total) %>% 
  arrange(desc(n))
```

      pos_group     n perc_all_iambic
    1  declined 79038           72.68
    2     verbs 24739           22.75
    3     other  4906            4.51

By clausula

``` r
all_words %>% 
  filter(meter == "Iamb" & closure != "other") %>% 
  group_by(meter, closure) %>% 
  count(meter, pos_group) %>% 
  ungroup() %>% 
  #left_join(total_meter, by = "meter") %>% 
  
  # percentage of all words in iamb
  #mutate(perc_all_iambic = round((n / total) * 100, 2 )) # %>% 
  left_join(iamb_closures, by = "closure") %>% 
  # 
  # # percentage of word of particular clausula type
  mutate(perc_iamb_clos = round((n / total_closure) * 100, 2 )) %>% 
  select(-total_closure, -meter)
```

    # A tibble: 9 × 4
      closure  pos_group     n perc_iamb_clos
      <chr>    <chr>     <int>          <dbl>
    1 dactylic declined   1072          81.0 
    2 dactylic other        41           3.1 
    3 dactylic verbs       211          15.9 
    4 fem      declined  38196          72.0 
    5 fem      other      2576           4.85
    6 fem      verbs     12303          23.2 
    7 masc     declined  39770          73.3 
    8 masc     other      2289           4.22
    9 masc     verbs     12225          22.5 

#### trochee

Total POS

``` r
all_words %>% 
  filter(meter == "Trochee" & closure != "other") %>% 
  count(meter, pos_group) %>% 
  left_join(total_meter, by = "meter") %>% 
  
  # percentage of all words in iamb
  mutate(perc_all_iambic = round((n / total) * 100, 2 )) %>% 
  select(-meter, -total) %>% 
  arrange(desc(n))
```

      pos_group     n perc_all_iambic
    1  declined 22415           73.04
    2     verbs  6777           22.08
    3     other  1446            4.71

By clausula

``` r
all_words %>% 
  filter(meter == "Trochee" & closure != "other") %>% 
  group_by(meter, closure) %>% 
  count(meter, pos_group) %>% 
  ungroup() %>% 
  #left_join(total_meter, by = "meter") %>% 
  
  # percentage of all words in iamb
  #mutate(perc_all_iambic = round((n / total) * 100, 2 )) # %>% 
  left_join(trochee_closures, by = "closure") %>% 
  # 
  # # percentage of word of particular clausula type
  mutate(perc_tr_clos = round((n / total_closure) * 100, 2 )) %>% 
  select(-total_closure, -meter)
```

    # A tibble: 9 × 4
      closure  pos_group     n perc_tr_clos
      <chr>    <chr>     <int>        <dbl>
    1 dactylic declined    984        89.2 
    2 dactylic other        19         1.72
    3 dactylic verbs       100         9.07
    4 fem      declined  10273        72.9 
    5 fem      other       760         5.39
    6 fem      verbs      3058        21.7 
    7 masc     declined  11158        72.2 
    8 masc     other       667         4.32
    9 masc     verbs      3619        23.4 

``` r
rm(x, iamb_closures, trochee_closures)
```

## periodicals / books

Separate periodicals & books sources of texts

``` r
#glimpse(all_words)
# quick check
all_words %>% 
  mutate(corpus = str_extract(text_id, "^\\w")) %>% count(corpus)
```

      corpus      n
    1      C 101241
    2      P  61210

Store totals & filter only iambs for Table 5.2.2

``` r
cp_total <- all_words %>% 
  filter(meter == "Iamb") %>% 
  mutate(corpus = str_extract(text_id, "^\\w")) %>% 
  count(corpus) %>% 
  rename(total = n)

cp_clausulas <- all_words %>% 
  filter(meter == "Iamb") %>% 
  mutate(corpus = str_extract(text_id, "^\\w")) %>% 
  count(corpus, closure) %>% 
  mutate(corpus_closure = paste0(corpus, "__", closure)) %>% 
  rename(total_closure = n) %>% 
  select(-corpus, -closure)
```

All POS

``` r
all_words %>% 
  filter(meter == "Iamb") %>% 
  mutate(corpus = str_extract(text_id, "^\\w")) %>% 
  count(corpus, pos) %>% 
  left_join(cp_total, by = "corpus") %>% 
  mutate(perc = round( (n/total)*100, 2 )) %>% 
  select(-n, -total) %>% 
  arrange(desc(perc)) %>% 
  pivot_wider(names_from = corpus, values_from = perc)
```

    # A tibble: 14 × 3
       pos               P     C
       <chr>         <dbl> <dbl>
     1 NOUN          53.2  50.9 
     2 VERB          17.3  19.8 
     3 ADJ           12.3  11.2 
     4 PRON           7.99  7.82
     5 VERB_inf       2.24  2.96
     6 ADV            2.94  2.78
     7 VERB_prich     1.7   1.71
     8 VERB_deeprich  1     1.11
     9 VERB_imp       0.86  1.1 
    10 PART           0.37  0.45
    11 NUM            0.09  0.08
    12 INTJ           0.05  0.08
    13 ADP            0.01  0.02
    14 CONJ           0.01  0.02

By clausula

``` r
all_words %>% 
  filter(meter == "Iamb") %>% 
  mutate(corpus = str_extract(text_id, "^\\w")) %>% 
  count(corpus, pos, closure) %>% 
  mutate(corpus_closure = paste0(corpus, "__", closure)) %>% 
  left_join(cp_clausulas, by = "corpus_closure") %>% 
  #select(-corpus_closure) %>% 
  mutate(perc_closure = round( (n/total_closure)*100, 2 )) %>% 
  filter(closure %in% c("masc", "fem")) %>% 
  select(-corpus, -closure, -total_closure, -n) %>% 
  arrange(desc(perc_closure)) %>% 
  pivot_wider(names_from = corpus_closure, values_from = perc_closure) %>% 
  select(pos, P__fem, C__fem, P__masc, C__masc)
```

    # A tibble: 14 × 5
       pos           P__fem C__fem P__masc C__masc
       <chr>          <dbl>  <dbl>   <dbl>   <dbl>
     1 NOUN           53.6   49.9    52.9    52   
     2 VERB           18.8   22.7    16.2    17.1 
     3 ADJ            18.2   16.6     6.16    5.48
     4 PRON            1.85   2.02   14.1    13.7 
     5 VERB_inf        0.67   1.38    3.77    4.59
     6 ADV             3.36   2.94    2.57    2.65
     7 VERB_prich      1.57   1.93    1.6     1.31
     8 VERB_deeprich   1.44   1.65    0.6     0.56
     9 VERB_imp        0.41   0.72    1.31    1.47
    10 PART            0.06   0.06    0.68    0.85
    11 INTJ            0.04   0.05    0.07    0.11
    12 NUM             0.11   0.07    0.08    0.09
    13 ADP             0.01   0       0.02    0.03
    14 CONJ            0.01   0.01   NA       0.03

POS groups

``` r
all_words %>% 
  filter(meter == "Iamb") %>% 
  mutate(corpus = str_extract(text_id, "^\\w")) %>% 
  count(corpus, pos_group, closure) %>% 
  mutate(corpus_closure = paste0(corpus, "__", closure)) %>% 
  left_join(cp_clausulas, by = "corpus_closure") %>% 
  #select(-corpus_closure) %>% 
  mutate(perc_closure = round( (n/total_closure)*100, 2 )) %>% 
  filter(closure %in% c("masc", "fem")) %>% 
  select(-corpus, -closure, -total_closure, -n) %>% 
  arrange(desc(perc_closure)) %>% 
  pivot_wider(names_from = corpus_closure, values_from = perc_closure) %>% 
  select(pos_group, P__fem, C__fem, P__masc, C__masc)
```

    # A tibble: 3 × 5
      pos_group P__fem C__fem P__masc C__masc
      <chr>      <dbl>  <dbl>   <dbl>   <dbl>
    1 declined   75.1   70.5    74.7    72.5 
    2 verbs      19.8   24.8    21.2    23.2 
    3 other       5.02   4.77    4.02    4.32

``` r
rm(cp_clausulas, cp_total)
```

## authors

### load data

NB the data includes MUCH smaller samples for each author than in Shaw’s
analysis (but still more than 1k rhyme words for each author)

``` r
# load data about authors
authors_meta <- read_csv("../../data/corpus1835/sql_db/authors.csv") %>% 
  select(A_ID, author_name)
```

    Rows: 315 Columns: 10
    ── Column specification ────────────────────────────────────────────────────────
    Delimiter: ","
    chr (9): A_ID, author_name, author_full_name, author_sex, year_birth, year_d...
    dbl (1): aristocracy

    ℹ Use `spec()` to retrieve the full column specification for this data.
    ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
# glimpse(authors_meta)

# attach authors to texts
authors <- meta %>% 
  select(text_id, A_ID) %>% 
  left_join(authors_meta, by = "A_ID") 

# attach authors to rhyme words
glimpse(all_words)
```

    Rows: 162,451
    Columns: 13
    $ text_id         <chr> "P_1938", "P_1938", "P_1938", "C_156__20", "C_156__20"…
    $ meter           <chr> "Other", "Other", "Other", "Trochee", "Trochee", "Troc…
    $ rhyme_alph      <chr> "краса небеса", "земным огневым", "красавице красавице…
    $ word            <chr> "краса", "огневым", "красавицей", "око", "силки", "сто…
    $ word_acc        <chr> "краса'", "огневы'м", "краса'вицей", "о'ко", "силки'",…
    $ stress_pattern  <chr> "01", "001", "0100", "10", "01", "10", "1", "10", "010…
    $ closure_pattern <chr> "1", "1", "100", "10", "1", "10", "1", "10", "10", "1"…
    $ closure         <chr> "masc", "masc", "dactylic", "fem", "masc", "fem", "mas…
    $ old_tag         <chr> "S", "S", "S", "S", "S", "V", "S", "S", "S", "S", "APR…
    $ feats           <chr> "S,жен,неод=им,ед", "S,фам,муж,од=(дат,мн|твор,ед)", "…
    $ ending_st       <chr> "са'", "ы'м", "а'вицей", "о'ко", "ки'", "о'нет", "о'р"…
    $ pos             <chr> "NOUN", "NOUN", "NOUN", "NOUN", "NOUN", "VERB", "NOUN"…
    $ pos_group       <chr> "declined", "declined", "declined", "declined", "decli…

``` r
all_words %>% 
  filter(meter == "Iamb") %>% 
  left_join(authors, by = "text_id") %>% 
  filter(!is.na(author_name)) %>% 
  count(author_name, sort = T) %>% head(40) # quick view
```

            author_name    n
    1    Жуковский В.А. 6496
    2       Крылов И.А. 6367
    3     Бороздна И.П. 4510
    4  Баратынский Е.А. 3432
    5       Смирнова А. 3012
    6   Быстроглазов А. 2399
    7       Башкатов А. 2387
    8         Бернет Е. 2249
    9      Демидов М.А. 2075
    10      Шахова Е.Н. 2005
    11    Тепляков В.Г. 1909
    12    Некрасов Н.А. 1896
    13       Зилов А.М. 1862
    14       Мейснер А. 1848
    15     Суханов М.Д. 1834
    16      Меркли М.М. 1582
    17  Бенедиктов В.Г. 1547
    18    Полежаев А.И. 1502
    19    Мартынов А.М. 1478
    20      Козлов И.И. 1474
    21     Бакунин И.М. 1456
    22      Сушков Д.П. 1448
    23  Ростопчина Е.П. 1396
    24    Тимофеев А.В. 1211
    25      Пушкин А.С. 1089
    26   Лермонтов М.Ю. 1045
    27   Слепушкин Ф.Н. 1037
    28      Менцов Ф.Н. 1030
    29        Кашаев В.  996
    30     Теплова Н.С.  984
    31      Кашкин Д.Е.  980
    32      Глинка Ф.Н.  916
    33       Губер Э.И.  908
    34       Ершов П.П.  898
    35      Марков М.А.  828
    36    Алексеев П.Ф.  780
    37 Подолинский А.И.  768
    38        Кони Ф.А.  706
    39   Бутырский Н.И.  674
    40     Савурский Н.  655

``` r
# attachment & removal of NA
words_authors <- all_words %>% 
  # leave only iambic endings 
  filter(meter == "Iamb") %>% 
  left_join(authors %>% select(-A_ID), by = "text_id") %>% 
  filter(!is.na(author_name))

# words_authors %>% 
#   filter(author_name == "Кульман Е.Б.")
```

### selection

#### pushkin-like verse

``` r
author_v <- c("Баратынский Е.А.", "Козлов И.И.", "Ростопчина Е.П.")
```

``` r
authors_total <- words_authors %>% 
  filter(author_name %in% author_v) %>% 
  count(author_name) %>% 
  rename(total = n)

authors_total
```

           author_name total
    1 Баратынский Е.А.  3432
    2      Козлов И.И.  1474
    3  Ростопчина Е.П.  1396

Total POS (without masc/fem devision

``` r
words_authors %>% 
  filter(author_name %in% author_v) %>% 
  group_by(author_name) %>% 
  count(pos) %>% 
  left_join(authors_total, by = "author_name") %>% 
  mutate(perc = round( (n/total)*100, 2 )) %>% 
  select(-n, -total) %>% 
  arrange(desc(perc)) %>% 
  pivot_wider(names_from = author_name, values_from = perc)
```

``` r
words_authors %>% 
  filter(author_name %in% author_v) %>% 
  group_by(author_name) %>% 
  count(pos_group) %>% 
  left_join(authors_total, by = "author_name") %>% 
  mutate(perc = round( (n/total)*100, 2 )) %>% 
  select(-n, -total) %>% 
  arrange(desc(perc)) %>% 
  pivot_wider(names_from = author_name, values_from = perc)
```

    # A tibble: 3 × 4
      pos_group `Баратынский Е.А.` `Козлов И.И.` `Ростопчина Е.П.`
      <chr>                  <dbl>         <dbl>             <dbl>
    1 declined               80.8          76.3              70.3 
    2 verbs                  14.6          18.9              27.2 
    3 other                   4.63          4.82              2.51

Clausulas

``` r
authors_closures <- words_authors %>% 
  filter(author_name %in% author_v) %>% 
  count(author_name, closure) %>% 
  mutate(author_closure = paste0(author_name, "__", closure)) %>% 
  select(-author_name, -closure) %>% 
  rename(total_closure = n)
```

``` r
words_authors %>% 
  filter(author_name %in% author_v & closure %in% c("masc", "fem")) %>% 
  group_by(author_name) %>% 
  count(pos, closure) %>% 
  ungroup() %>% 
  # left_join(authors_total, by = "author_name") %>% 
  # mutate(perc_total = round( (n/total)*100, 2 )) %>% 
  
  mutate(author_closure = paste0(author_name, "__", closure)) %>% 
  left_join(authors_closures, by = "author_closure") %>% 
  mutate(perc_closure = round( (n/total_closure)*100, 1 )) %>% 
  
  select(-n, -total_closure, -author_closure) %>% 
  arrange(desc(perc_closure)) %>% 
  filter(pos %in% c("NOUN", "VERB", "ADJ", "PRON", "ADV")) %>% 
  pivot_wider(names_from = author_name, values_from = perc_closure)
```

    # A tibble: 10 × 5
       pos   closure `Баратынский Е.А.` `Козлов И.И.` `Ростопчина Е.П.`
       <chr> <chr>                <dbl>         <dbl>             <dbl>
     1 NOUN  fem                   52.7          46.4              39.3
     2 NOUN  masc                  51.1          49.1              35  
     3 ADJ   fem                   19.5          22.9              27.9
     4 VERB  fem                   15.4          19.7              25  
     5 PRON  masc                  23.8          15.4              23.2
     6 VERB  masc                  11            12.9              21.4
     7 ADJ   masc                   5.6          10.3               9.1
     8 PRON  fem                    4.2           2.3               1.6
     9 ADV   masc                   2.6           1.7               1.9
    10 ADV   fem                    2.5           1.8               1.7

``` r
words_authors %>% 
  filter(author_name %in% author_v & closure %in% c("masc", "fem")) %>% 
  group_by(author_name) %>% 
  count(pos_group, closure) %>% 
  ungroup() %>% 
  # left_join(authors_total, by = "author_name") %>% 
  # mutate(perc_total = round( (n/total)*100, 2 )) %>% 
  
  mutate(author_closure = paste0(author_name, "__", closure)) %>% 
  left_join(authors_closures, by = "author_closure") %>% 
  mutate(perc_closure = round( (n/total_closure)*100, 1 )) %>% 
  
  select(-n, -total_closure, -author_closure) %>% 
  arrange(desc(perc_closure)) %>% 
  pivot_wider(names_from = author_name, values_from = perc_closure)
```

    # A tibble: 6 × 5
      pos_group closure `Баратынский Е.А.` `Козлов И.И.` `Ростопчина Е.П.`
      <chr>     <chr>                <dbl>         <dbl>             <dbl>
    1 declined  masc                  82.1          79                69.6
    2 declined  fem                   79            73.5              70.7
    3 verbs     masc                  13.2          17.5              27.8
    4 verbs     fem                   16.3          20.4              26.9
    5 other     fem                    4.7           6.1               2.5
    6 other     masc                   4.7           3.6               2.6

``` r
words_authors %>% 
  filter(author_name %in% author_v & closure %in% c("masc", "fem")) %>% 
  filter(author_name == "Ростопчина Е.П.") %>% 
  filter(pos == "VERB") %>% sample_n(10)
```

         text_id meter            rhyme_alph       word    word_acc stress_pattern
    1      P_121  Iamb    переживает умирает переживает пережива'ет          00010
    2      P_762  Iamb            даму опишу      опишу      опишу'            001
    3  C_301__17  Iamb   прервался расстался  расстался  расста'лся            010
    4     P_1066  Iamb           была влекла     влекла     влекла'             01
    5     P_1854  Iamb         завоют зароют     зароют     заро'ют            010
    6  C_169__13  Iamb         была невесела       была       была'             01
    7     P_1232  Iamb        молчит сливает     молчит     молчи'т             01
    8  C_169__27  Iamb          миновал сжал    миновал    минова'л            001
    9      P_853  Iamb    помчится укротится   помчится   помчи'тся            010
    10    P_1104  Iamb развивалось старалась  старалась  стара'лась            010
       closure_pattern closure old_tag
    1               10     fem       V
    2                1    masc       V
    3               10     fem       V
    4                1    masc       V
    5               10     fem       V
    6                1    masc       V
    7                1    masc       V
    8                1    masc       V
    9               10     fem       V
    10              10     fem       V
                                                      feats ending_st  pos
    1                        V,пе=непрош,ед,изъяв,3-л,несов      а'ет VERB
    2                          V,пе=непрош,ед,изъяв,1-л,сов       шу' VERB
    3                            V,нп=прош,ед,изъяв,муж,сов     а'лся VERB
    4                          V,несов,пе=прош,ед,изъяв,жен       ла' VERB
    5                             V=непрош,мн,изъяв,3-л,сов      о'ют VERB
    6                          V,нп=прош,ед,изъяв,жен,несов       ла' VERB
    7                        V,несов,нп=непрош,ед,изъяв,3-л       и'т VERB
    8  V,пе=(прош,ед,изъяв,муж,несов|прош,ед,изъяв,муж,сов)       а'л VERB
    9                          V,сов,нп=непрош,ед,изъяв,3-л     и'тся VERB
    10                         V,несов,нп=прош,ед,изъяв,жен    а'лась VERB
       pos_group     author_name
    1      verbs Ростопчина Е.П.
    2      verbs Ростопчина Е.П.
    3      verbs Ростопчина Е.П.
    4      verbs Ростопчина Е.П.
    5      verbs Ростопчина Е.П.
    6      verbs Ростопчина Е.П.
    7      verbs Ростопчина Е.П.
    8      verbs Ростопчина Е.П.
    9      verbs Ростопчина Е.П.
    10     verbs Ростопчина Е.П.

#### benediktov & co

``` r
author_v <- c("Бенедиктов В.Г.", "Бернет Е.", "Шахова Е.Н." )

authors_total <- words_authors %>% 
  filter(author_name %in% author_v) %>% 
  count(author_name) %>% 
  rename(total = n)

authors_total
```

          author_name total
    1 Бенедиктов В.Г.  1547
    2       Бернет Е.  2249
    3     Шахова Е.Н.  2005

All POS

``` r
# pos separately
words_authors %>% 
  filter(author_name %in% author_v) %>% 
  group_by(author_name) %>% 
  count(pos) %>% 
  left_join(authors_total, by = "author_name") %>% 
  mutate(perc = round( (n/total)*100, 2 )) %>% 
  select(-n, -total) %>% 
  arrange(desc(perc)) %>% 
  pivot_wider(names_from = author_name, values_from = perc)

# pos groups
words_authors %>% 
  filter(author_name %in% author_v) %>% 
  group_by(author_name) %>% 
  count(pos_group) %>% 
  left_join(authors_total, by = "author_name") %>% 
  mutate(perc = round( (n/total)*100, 2 )) %>% 
  select(-n, -total) %>% 
  arrange(desc(perc)) %>% 
  pivot_wider(names_from = author_name, values_from = perc)
```

Clausulas

``` r
# total clausulas
authors_closures <- words_authors %>% 
  filter(author_name %in% author_v) %>% 
  count(author_name, closure) %>% 
  mutate(author_closure = paste0(author_name, "__", closure)) %>% 
  select(-author_name, -closure) %>% 
  rename(total_closure = n)

# pos separately
words_authors %>% 
  filter(author_name %in% author_v & closure %in% c("masc", "fem")) %>% 
  group_by(author_name) %>% 
  count(pos, closure) %>% 
  ungroup() %>% 
  # left_join(authors_total, by = "author_name") %>% 
  # mutate(perc_total = round( (n/total)*100, 2 )) %>% 
  
  mutate(author_closure = paste0(author_name, "__", closure)) %>% 
  left_join(authors_closures, by = "author_closure") %>% 
  mutate(perc_closure = round( (n/total_closure)*100, 1 )) %>% 
  
  select(-n, -total_closure, -author_closure) %>% 
  arrange(desc(perc_closure)) %>% 
  filter(pos %in% c("NOUN", "VERB", "ADJ", "PRON", "ADV")) %>% 
  pivot_wider(names_from = author_name, values_from = perc_closure) 
```

    # A tibble: 10 × 5
       pos   closure `Бернет Е.` `Бенедиктов В.Г.` `Шахова Е.Н.`
       <chr> <chr>         <dbl>             <dbl>         <dbl>
     1 NOUN  fem            59.8              54.1          46.8
     2 NOUN  masc           57.6              53.9          46  
     3 VERB  fem            19.7              18.3          24  
     4 ADJ   fem            12.7              17.6          20.2
     5 VERB  masc           17.5              16.4          16.7
     6 PRON  masc            6.5              12.2          16.6
     7 ADJ   masc            4.6               6.8           9.4
     8 ADV   fem             1.7               3.4           3.5
     9 ADV   masc            2                 3.2           2.8
    10 PRON  fem             1.6               1             1.4

``` r
# pos groups
words_authors %>% 
  filter(author_name %in% author_v & closure %in% c("masc", "fem")) %>% 
  group_by(author_name) %>% 
  count(pos_group, closure) %>% 
  ungroup() %>% 
  # left_join(authors_total, by = "author_name") %>% 
  # mutate(perc_total = round( (n/total)*100, 2 )) %>% 
  
  mutate(author_closure = paste0(author_name, "__", closure)) %>% 
  left_join(authors_closures, by = "author_closure") %>% 
  mutate(perc_closure = round( (n/total_closure)*100, 1 )) %>% 
  
  select(-n, -total_closure, -author_closure) %>% 
  arrange(desc(perc_closure)) %>% 
  pivot_wider(names_from = author_name, values_from = perc_closure) %>% head(25)
```

    # A tibble: 6 × 5
      pos_group closure `Бернет Е.` `Бенедиктов В.Г.` `Шахова Е.Н.`
      <chr>     <chr>         <dbl>             <dbl>         <dbl>
    1 declined  fem            76                74            69.5
    2 declined  masc           70.4              75.4          72.9
    3 verbs     fem            20.6              19.7          26.2
    4 verbs     masc           25.8              19.8          23.2
    5 other     fem             3.4               6.3           4.2
    6 other     masc            3.8               4.9           3.9

#### other authors

``` r
author_v <- c("Слепушкин Ф.Н.",  "Мейснер А.", "Некрасов Н.А.")
```

All POS

``` r
authors_total <- words_authors %>% 
  filter(author_name %in% author_v) %>% 
  count(author_name) %>% 
  rename(total = n)

authors_total
```

         author_name total
    1     Мейснер А.  1848
    2  Некрасов Н.А.  1896
    3 Слепушкин Ф.Н.  1037

``` r
# pos separately
words_authors %>% 
  filter(author_name %in% author_v) %>% 
  group_by(author_name) %>% 
  count(pos) %>% 
  left_join(authors_total, by = "author_name") %>% 
  mutate(perc = round( (n/total)*100, 2 )) %>% 
  select(-n, -total) %>% 
  arrange(desc(perc)) %>% 
  pivot_wider(names_from = author_name, values_from = perc) 

# pos groups
words_authors %>% 
  filter(author_name %in% author_v) %>% 
  group_by(author_name) %>% 
  count(pos_group) %>% 
  left_join(authors_total, by = "author_name") %>% 
  mutate(perc = round( (n/total)*100, 2 )) %>% 
  select(-n, -total) %>% 
  arrange(desc(perc)) %>% 
  pivot_wider(names_from = author_name, values_from = perc) 
```

Clausulas

``` r
# total clausulas
authors_closures <- words_authors %>% 
  filter(author_name %in% author_v) %>% 
  count(author_name, closure) %>% 
  mutate(author_closure = paste0(author_name, "__", closure)) %>% 
  select(-author_name, -closure) %>% 
  rename(total_closure = n)

# pos separately
words_authors %>% 
  filter(author_name %in% author_v & closure %in% c("masc", "fem")) %>% 
  group_by(author_name) %>% 
  count(pos, closure) %>% 
  ungroup() %>% 
  # left_join(authors_total, by = "author_name") %>% 
  # mutate(perc_total = round( (n/total)*100, 2 )) %>% 
  
  mutate(author_closure = paste0(author_name, "__", closure)) %>% 
  left_join(authors_closures, by = "author_closure") %>% 
  mutate(perc_closure = round( (n/total_closure)*100, 1 )) %>% 
  
  select(-n, -total_closure, -author_closure) %>% 
  arrange(desc(perc_closure)) %>% 
  filter(pos %in% c("NOUN", "VERB", "ADJ", "PRON", "ADV")) %>% 
  pivot_wider(names_from = author_name, values_from = perc_closure) %>% 
  filter(closure == "fem")
```

    # A tibble: 5 × 5
      pos   closure `Слепушкин Ф.Н.` `Некрасов Н.А.` `Мейснер А.`
      <chr> <chr>              <dbl>           <dbl>        <dbl>
    1 NOUN  fem                 51.7            64.5         57.7
    2 VERB  fem                 30.6             9.1         13.8
    3 ADJ   fem                 13.6            15.7         18.5
    4 ADV   fem                  0.8             5.4          4.8
    5 PRON  fem                  1.7             0.7          1.1

``` r
# pos groups
words_authors %>% 
  filter(author_name %in% author_v & closure %in% c("masc", "fem")) %>% 
  group_by(author_name) %>% 
  count(pos_group, closure) %>% 
  ungroup() %>% 
  # left_join(authors_total, by = "author_name") %>% 
  # mutate(perc_total = round( (n/total)*100, 2 )) %>% 
  
  mutate(author_closure = paste0(author_name, "__", closure)) %>% 
  left_join(authors_closures, by = "author_closure") %>% 
  mutate(perc_closure = round( (n/total_closure)*100, 1 )) %>% 
  
  select(-n, -total_closure, -author_closure) %>% 
  arrange(desc(perc_closure)) %>% 
  pivot_wider(names_from = author_name, values_from = perc_closure) 
```

    # A tibble: 6 × 5
      pos_group closure `Некрасов Н.А.` `Мейснер А.` `Слепушкин Ф.Н.`
      <chr>     <chr>             <dbl>        <dbl>            <dbl>
    1 declined  fem                81.8         79.6             67.2
    2 declined  masc               72.9         79.1             74.7
    3 verbs     fem                10.7         14.5             31.9
    4 verbs     masc               22           15.2             22.8
    5 other     fem                 7.5          5.9              0.9
    6 other     masc                5.1          5.7              2.5

``` r
rm(authors_closures, authors_total, authors)
```

### all authors scaled

``` r
author_v <- c("Баратынский Е.А.", "Козлов И.И.", "Ростопчина Е.П.",
              "Бенедиктов В.Г.", "Бернет Е.", "Шахова Е.Н.",
              "Слепушкин Ф.Н.",  "Мейснер А.", "Некрасов Н.А."
             )

authors_total <- words_authors %>% 
  filter(author_name %in% author_v) %>% 
  count(author_name) %>% 
  rename(total = n)

authors_total
```

           author_name total
    1 Баратынский Е.А.  3432
    2  Бенедиктов В.Г.  1547
    3        Бернет Е.  2249
    4      Козлов И.И.  1474
    5       Мейснер А.  1848
    6    Некрасов Н.А.  1896
    7  Ростопчина Е.П.  1396
    8   Слепушкин Ф.Н.  1037
    9      Шахова Е.Н.  2005

``` r
top_authors <- words_authors %>% count(author_name, sort = T) %>% filter(n > 1000) %>% pull(author_name)
```

All POS

``` r
v <- words_authors %>% 
  filter(author_name %in% top_authors) %>% 
  group_by(author_name) %>% 
  sample_n(1000) %>% 
  count(pos) %>% 
  ungroup() %>% 
  pivot_wider(names_from = pos, values_from = n) %>% 
  select(ADJ, ADV, NOUN, PRON, VERB, VERB_inf, VERB_imp, VERB_prich, VERB_deeprich)

a <- words_authors %>% 
  filter(author_name %in% top_authors) %>% 
  group_by(author_name) %>% 
  count(pos) %>% 
  pivot_wider(names_from = pos, values_from = n) %>% select(author_name)

cbind(a, as.tibble(scale(v))) %>% filter(author_name %in% author_v) 
```

    Warning: `as.tibble()` was deprecated in tibble 2.0.0.
    ℹ Please use `as_tibble()` instead.
    ℹ The signature and semantics have changed, see `?as_tibble`.

    # A tibble: 9 × 10
    # Groups:   author_name [9]
      author_name    ADJ     ADV    NOUN   PRON    VERB VERB_inf VERB_imp VERB_prich
      <chr>        <dbl>   <dbl>   <dbl>  <dbl>   <dbl>    <dbl>    <dbl>      <dbl>
    1 Баратынски…  0.690 -0.372  -0.0892  2.31  -0.990   -1.03    -0.519       0.770
    2 Бенедиктов…  0.169  0.520   0.279  -0.457 -0.360   -0.636    0.0453      0.880
    3 Бернет Е.   -0.875 -0.818   1.10   -1.40  -0.0884  -0.0407   1.31        0.223
    4 Козлов И.И.  1.18  -0.669  -0.432   0.624 -0.417   -0.338   -0.942       1.43 
    5 Мейснер А.  -0.463  0.892   1.36   -0.424 -1.09    -0.685   -0.0957      0.442
    6 Некрасов Н… -0.271  1.11    0.773   0.252 -1.08    -0.0407   0.891      -0.215
    7 Ростопчина…  2.31  -0.892  -1.97    1.57   0.613    0.505   -1.22        0.223
    8 Слепушкин … -0.573 -1.19    0.799  -1.61   0.627    0.108   -0.237      -1.42 
    9 Шахова Е.Н.  0.416  0.0743 -0.457   0.590  0.141    0.306    0.0453     -0.762
    # ℹ 1 more variable: VERB_deeprich <dbl>

``` r
cbind(a, as.tibble(scale(v))) %>% filter(author_name %in% author_v) %>% 
  pivot_longer(!author_name, names_to = "pos", values_to = "z_score") %>% 
  ggplot(aes(x = author_name, y = z_score, fill = pos)) + 
  geom_col(position = "dodge", width = 0.5) + 
  theme(axis.text.x = element_text(angle = 90))
```

![](05_4_rhyme_morhp_words.markdown_strict_files/figure-markdown_strict/unnamed-chunk-76-1.png)

Masc clausula

``` r
# select authors with more than 500 masc words (45)
top_authors <- words_authors %>% 
  filter(closure == "masc") %>% 
  count(author_name, sort = T) %>% 
  filter(n > 300) %>% 
  pull(author_name)

v <- words_authors %>% 
  filter(author_name %in% top_authors & closure == "masc") %>% 
  group_by(author_name) %>% 
  sample_n(300) %>% 
  count(pos) %>% 
  ungroup() %>% 
  pivot_wider(names_from = pos, values_from = n) %>% 
  select(ADJ, ADV, NOUN, PRON, VERB, VERB_inf, VERB_imp, VERB_prich, VERB_deeprich)

a <- words_authors %>% 
  filter(author_name %in% top_authors & closure == "masc") %>% 
  group_by(author_name) %>% 
  count(pos) %>% 
  pivot_wider(names_from = pos, values_from = n) %>% select(author_name)

cbind(a, as.tibble(scale(v))) %>% filter(author_name %in% author_v) 
```

    # A tibble: 9 × 10
    # Groups:   author_name [9]
      author_name          ADJ    ADV    NOUN   PRON     VERB VERB_inf VERB_imp
      <chr>              <dbl>  <dbl>   <dbl>  <dbl>    <dbl>    <dbl>    <dbl>
    1 Баратынский Е.А. -0.865   0.339  0.0161  1.76  -1.23     -0.833    -0.206
    2 Бенедиктов В.Г.   0.979   1.31  -0.500   0.401 -0.00683  -0.930     0.520
    3 Бернет Е.        -0.155  -0.392  0.611  -1.46  -0.294     0.341     2.70 
    4 Козлов И.И.       1.12   -0.880 -0.341   0.401 -0.796     0.146    -0.569
    5 Мейснер А.       -0.865   1.07   0.928  -0.884 -0.653    -0.441    -0.206
    6 Некрасов Н.А.    -0.0135  0.339 -0.618   0.616 -0.437     0.732     0.883
    7 Ростопчина Е.П.   1.55   -0.636 -1.93    2.12   1.21     -0.0501   NA    
    8 Слепушкин Ф.Н.   -0.439  -1.12   1.60   -1.96  -0.366     0.243    -0.206
    9 Шахова Е.Н.       1.55    2.29  -0.936   0.330 -0.222     0.439    -0.569
    # ℹ 2 more variables: VERB_prich <dbl>, VERB_deeprich <dbl>

``` r
cbind(a, as.tibble(scale(v))) %>% filter(author_name %in% author_v) %>% 
  pivot_longer(!author_name, names_to = "pos", values_to = "z_score") %>% 
  ggplot(aes(x = author_name, y = z_score, fill = pos)) + 
  geom_col(position = "dodge", width = 0.5) + 
  theme(axis.text.x = element_text(angle = 90)) + 
  labs(title = "Masculine rhymes")
```

    Warning: Removed 1 rows containing missing values (`geom_col()`).

![](05_4_rhyme_morhp_words.markdown_strict_files/figure-markdown_strict/unnamed-chunk-77-1.png)

Syllable variation scaled

``` r
v <- words_authors %>% 
  filter(author_name %in% top_authors & closure == "masc") %>% 
  group_by(author_name) %>% 
  sample_n(300) %>% 
  filter(pos %in% c("NOUN", "VERB", "ADJ", "PRON")) %>% 
  mutate(syl_len = nchar(stress_pattern),
         pos_syl = paste0(pos, "_", syl_len)
         ) %>% 
  count(pos_syl) %>% 
  ungroup() %>% 
  pivot_wider(names_from = pos_syl, values_from = n, values_fill = 0) 

a <- v %>% select(author_name)

v <- v %>% select(-author_name) # remove authors for scaling

# cbind(a, as.tibble(scale(v))) %>% filter(author_name %in% author_v) 

cbind(a, as.tibble(scale(v))) %>% filter(author_name %in% author_v) %>% 
  pivot_longer(!author_name, names_to = "pos_syl", values_to = "z_score") %>% 
  ggplot(aes(x = author_name, y = z_score, fill = pos_syl)) + 
  geom_col(position = "dodge", width = 0.5) + 
  theme(axis.text.x = element_text(angle = 90)) + 
  labs(title = "Masculine rhymes")
```

![](05_4_rhyme_morhp_words.markdown_strict_files/figure-markdown_strict/unnamed-chunk-78-1.png)

Feminine rhymes

``` r
# select authors with more than 500 fem rhymes (40)
top_authors <- words_authors %>% 
  filter(closure == "fem") %>% 
  count(author_name, sort = T) %>% 
  filter(n > 500) %>% 
  pull(author_name)

v <- words_authors %>% 
  filter(author_name %in% top_authors & closure == "fem") %>% 
  group_by(author_name) %>% 
  sample_n(500) %>% 
  count(pos) %>% 
  ungroup() %>% 
  pivot_wider(names_from = pos, values_from = n) %>% 
  select(ADJ, ADV, NOUN, PRON, VERB, VERB_inf, VERB_imp, VERB_prich, VERB_deeprich)

a <- words_authors %>% 
  filter(author_name %in% top_authors & closure == "fem") %>% 
  group_by(author_name) %>% 
  count(pos) %>% 
  pivot_wider(names_from = pos, values_from = n) %>% select(author_name)

cbind(a, as.tibble(scale(v))) %>% filter(author_name %in% author_v) 
```

    # A tibble: 9 × 10
    # Groups:   author_name [9]
      author_name      ADJ    ADV   NOUN    PRON   VERB VERB_inf VERB_imp VERB_prich
      <chr>          <dbl>  <dbl>  <dbl>   <dbl>  <dbl>    <dbl>    <dbl>      <dbl>
    1 Баратынский …  0.386 -0.322  0.433  2.19   -0.633   -0.780   -0.625    -0.255 
    2 Бенедиктов В…  0.425  0.380  0.255 -1.16   -0.470   -0.171   -0.406    -0.255 
    3 Бернет Е.     -0.833 -0.438  0.899 -0.263  -0.163   -0.627   -0.406    -0.0807
    4 Козлов И.И.    1.15  -1.02  -0.410 -0.486  -0.143   -0.780   -0.625     0.0932
    5 Мейснер А.     0.120  0.731  1.03  -0.709  -0.919   -0.780   -0.406     0.615 
    6 Некрасов Н.А. -0.185  1.78   1.30  -1.60   -1.25    -0.323    0.252    -0.602 
    7 Ростопчина Е…  1.99  -0.555 -1.30  -0.709   0.367    0.439   -0.406    -0.255 
    8 Слепушкин Ф.… -0.833 -1.37   0.122 -0.0399  0.960   -0.323   -0.406    -1.30  
    9 Шахова Е.Н.    0.272  0.497 -0.322 -1.16    0.204    0.439   -0.625     0.0932
    # ℹ 1 more variable: VERB_deeprich <dbl>

``` r
cbind(a, as.tibble(scale(v))) %>% filter(author_name %in% author_v) %>% 
  pivot_longer(!author_name, names_to = "pos", values_to = "z_score") %>% 
  ggplot(aes(x = author_name, y = z_score, fill = pos)) + 
  geom_col(position = "dodge", width = 0.5) + 
  theme(axis.text.x = element_text(angle = 90)) + 
  labs(title = "Feminine rhymes")
```

    Warning: Removed 1 rows containing missing values (`geom_col()`).

![](05_4_rhyme_morhp_words.markdown_strict_files/figure-markdown_strict/unnamed-chunk-79-1.png)

Syllable variation

``` r
v <- words_authors %>% 
  filter(author_name %in% top_authors & closure == "fem") %>% 
  group_by(author_name) %>% 
  sample_n(500) %>% 
  filter(pos %in% c("NOUN", "VERB", "ADJ", "PRON")) %>% 
  mutate(syl_len = nchar(stress_pattern),
         pos_syl = paste0(pos, "_", syl_len)
         ) %>% 
  count(pos_syl) %>% 
  ungroup() %>% 
  pivot_wider(names_from = pos_syl, values_from = n, values_fill = 0) 

a <- v %>% select(author_name)

v <- v %>% select(-author_name) # remove authors for scaling

# cbind(a, as.tibble(scale(v))) %>% filter(author_name %in% author_v) 

cbind(a, as.tibble(scale(v))) %>% filter(author_name %in% author_v) %>% 
  pivot_longer(!author_name, names_to = "pos_syl", values_to = "z_score") %>% 
  ggplot(aes(x = author_name, y = z_score, fill = pos_syl)) + 
  geom_col(position = "dodge", width = 0.5) + 
  theme(axis.text.x = element_text(angle = 90)) + 
  labs(title = "Feminine rhymes")
```

![](05_4_rhyme_morhp_words.markdown_strict_files/figure-markdown_strict/unnamed-chunk-80-1.png)

``` r
rm(a, authors_closures, authors_total,words_authors, authors, author_v, closures, top_authors, total_meter)
```

    Warning in rm(a, authors_closures, authors_total, words_authors, authors, :
    object 'authors_closures' not found

    Warning in rm(a, authors_closures, authors_total, words_authors, authors, :
    object 'authors' not found

## blanc verse

### prep Kulmann data

-   extract Kulmann’s endwords from texts

Load full texts

``` r
authors_meta %>% filter(str_detect(author_name, "Кульман"))
```

    # A tibble: 1 × 2
      A_ID  author_name 
      <chr> <chr>       
    1 A_96  Кульман Е.Б.

``` r
kulmann_texts <- meta %>% 
  filter(A_ID == "A_96")

texts <- read_csv("../../data/corpus1835/corpus1835_texts_lemm_acc.csv")
```

    New names:
    Rows: 4797 Columns: 6
    ── Column specification
    ──────────────────────────────────────────────────────── Delimiter: "," chr
    (5): text_id, text_raw, text_cln, text_lemm, text_acc dbl (1): ...1
    ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    Specify the column types or set `show_col_types = FALSE` to quiet this message.
    • `` -> `...1`

``` r
kulmann_texts <- kulmann_texts %>% 
  left_join(texts %>% select(text_id, text_cln), by = "text_id")
```

``` r
# number of texts
nrow(kulmann_texts)
```

    [1] 32

``` r
# total number of lines
kulmann_texts %>% 
  summarise(total_lines_kulmann = sum(n_lines))
```

      total_lines_kulmann
    1                7851

Extract endwords

``` r
k_endwords <- kulmann_texts %>% 
  
  select(text_id, text_cln) %>% 
  # some text cleaning
  mutate(text_cln = str_replace_all(text_cln, "\n", " ------ "),
         text_cln = str_remove(text_cln, "<.*?>"),
         text_cln = str_replace_all(text_cln, " ------ ", "\n")) %>% 
  
  # extract endword
  
  separate_rows(text_cln, sep = "\n") %>% 
  # remove lines with no words
  filter(text_cln != "" & !str_detect(text_cln, "^[[:punct:]]+$|^[[:space:]]+$")) %>% 
  # remove punct in the end of the line
  mutate(text_cln = str_remove_all(text_cln, "\\W+$|\\s+$"),
         # extract endword
         endword = str_extract(text_cln, "\\w+$")) 

head(k_endwords)
```

    # A tibble: 6 × 3
      text_id  text_cln             endword  
      <chr>    <chr>                <chr>    
    1 C_264__1 С Темпейския долины  долины   
    2 C_264__1 Где на брегу Пенея   Пенея    
    3 C_264__1 Прелестная недавно   недавно  
    4 C_264__1 Преобратилась Дафна  Дафна    
    5 C_264__1 Аполл младую ветку   ветку    
    6 C_264__1 К Парнассу перенесши перенесши

``` r
# writeLines(k_endwords %>% pull(endword), 
#           "k_endwords.txt")
```

-   get POS tags

``` r
endw_pos <- read_csv("k_endwords.csv") %>% select(-`...1`)
```

    New names:
    Rows: 7847 Columns: 4
    ── Column specification
    ──────────────────────────────────────────────────────── Delimiter: "," chr
    (3): word_from, lemma, pos dbl (1): ...1
    ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    Specify the column types or set `show_col_types = FALSE` to quiet this message.
    • `` -> `...1`

``` r
nrow(k_endwords) == nrow(endw_pos)
```

    [1] TRUE

``` r
k_ew_pos <- cbind(k_endwords, endw_pos)

glimpse(k_ew_pos)
```

    Rows: 7,847
    Columns: 6
    $ text_id   <chr> "C_264__1", "C_264__1", "C_264__1", "C_264__1", "C_264__1", …
    $ text_cln  <chr> "С Темпейския долины", "Где на брегу Пенея", "Прелестная нед…
    $ endword   <chr> "долины", "Пенея", "недавно", "Дафна", "ветку", "перенесши",…
    $ word_from <chr> "долины", "Пенея", "недавно", "Дафна", "ветку", "перенесши",…
    $ lemma     <chr> "долина", "пенеть", "недавно", "дафна", "ветка", "перенести"…
    $ pos       <chr> "S,жен,неод=(вин,мн|род,ед|им,мн)", "V,несов,нп=непрош,деепр…

Extract & rewrite POS tags

``` r
k_ew_pos <- k_ew_pos %>% 
  select(-text_cln, -lemma) %>% 
  
  # extract pos
  mutate(feats = pos,
         pos = str_extract(feats, "^\\w+(,|=)"),
         pos = str_remove(pos, ',|='),
         rhyme_words = tolower(endword)
         ) %>% 
  
  # rewrite pos with UPOS tags
  rename(old_tag = pos) %>% 
  left_join(pos_transl, by = "old_tag") %>% 
  
  # separate verb forms
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
                      pos)
         )
```

``` r
head(k_ew_pos)
```

       text_id   endword word_from old_tag                            feats
    1 C_264__1    долины    долины       S S,жен,неод=(вин,мн|род,ед|им,мн)
    2 C_264__1     Пенея     Пенея       V          V,несов,нп=непрош,деепр
    3 C_264__1   недавно   недавно     ADV                             ADV=
    4 C_264__1     Дафна     Дафна       S               S,имя,жен,од=им,ед
    5 C_264__1     ветку     ветку       S                S,жен,неод=вин,ед
    6 C_264__1 перенесши перенесши       V                 V,сов=прош,деепр
      rhyme_words           pos
    1      долины          NOUN
    2       пенея VERB_deeprich
    3     недавно           ADV
    4       дафна          NOUN
    5       ветку          NOUN
    6   перенесши VERB_deeprich

### count i3 pos

Attach meters & count other poems in Iamb / Iamb-3

``` r
# glimpse(meta)
# glimpse(all_words)

# all 32 Kulmann's texts are Iamb_3
meta %>% 
  filter(A_ID == "A_96") %>% 
  mutate(formula = paste0(meter, "_", feet)) %>% 
  count(formula)
```

      formula  n
    1  Iamb_3 32

``` r
# select other Iamb-3 texts
iamb3_texts <- meta %>% 
  filter(A_ID != "A_96") %>% 
  mutate(formula = paste0(meter, "_", feet)) %>% 
  filter(formula == "Iamb_3") %>% 
  pull(text_id)

length(iamb3_texts) # 53 other Iamb_3 poems
```

    [1] 53

``` r
iamb_texts <- meta %>% 
  filter(A_ID != "A_96" & meter == "Iamb") %>% 
  pull(text_id)

length(iamb_texts) # 3023 iamb texts
```

    [1] 3023

``` r
total <- all_words %>% 
  filter(text_id %in% iamb3_texts) %>% 
  #filter only fem clausula
  filter(closure == "fem") %>% 
  nrow()

total # 1098 rhyme words in iamb-3
```

    [1] 1098

``` r
i3_rhymed <- all_words %>% 
  filter(text_id %in% iamb3_texts & closure == "fem") %>% 
  count(pos) %>% 
  rename(n_rhyme_i3 = n) %>% 
  mutate(perc_rhyme_i3 = round( (n_rhyme_i3/total)*100, 2 ))

i3_rhymed
```

                 pos n_rhyme_i3 perc_rhyme_i3
    1            ADJ        190         17.30
    2            ADV         23          2.09
    3           NOUN        606         55.19
    4            NUM          2          0.18
    5           PART          1          0.09
    6           PRON         19          1.73
    7           VERB        199         18.12
    8  VERB_deeprich         12          1.09
    9       VERB_imp          5          0.46
    10      VERB_inf          7          0.64
    11    VERB_prich         34          3.10

all Iambs

``` r
total_iambs <- all_words %>% 
  filter(text_id %in% iamb_texts & closure == "fem") %>% 
  nrow()

total_iambs # 53 075 words in all iambs
```

    [1] 53075

``` r
iambs_rhymed <- all_words %>% 
  filter(text_id %in% iamb_texts & closure == "fem") %>% 
  count(pos) %>% 
  rename(n_rhyme_iambs = n) %>% 
  mutate(perc_rhyme_iambs = round( (n_rhyme_iambs/total_iambs)*100, 2 ))

iambs_rhymed
```

                 pos n_rhyme_iambs perc_rhyme_iambs
    1            ADJ          9071            17.09
    2            ADP             2             0.00
    3            ADV          1631             3.07
    4           CONJ             4             0.01
    5           INTJ            25             0.05
    6           NOUN         27119            51.10
    7            NUM            44             0.08
    8           PART            32             0.06
    9           PRON          1043             1.97
    10          VERB         11363            21.41
    11 VERB_deeprich           838             1.58
    12      VERB_imp           330             0.62
    13      VERB_inf           610             1.15
    14    VERB_prich           963             1.81

rnc i3 data load

``` r
# load rhymes data
rnc_rhymes <- read_csv("../../data/ch5/nkrja_rhyme_pairs.csv")
```

    New names:
    Rows: 146064 Columns: 17
    ── Column specification
    ──────────────────────────────────────────────────────── Delimiter: "," chr
    (16): poem_id, rhyme_alph, from, to, from_pos, to_pos, from_feats, to_fe... dbl
    (1): ...1
    ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    Specify the column types or set `show_col_types = FALSE` to quiet this message.
    • `` -> `...1`

``` r
glimpse(rnc_rhymes)
```

    Rows: 146,064
    Columns: 17
    $ ...1           <dbl> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, …
    $ poem_id        <chr> "RNC_3870_1777", "RNC_3870_1777", "RNC_3870_1777", "RNC…
    $ rhyme_alph     <chr> "Кантемир сатир", "пленялись равнялись", "блистал стал"…
    $ from           <chr> "кантемир", "равнялись", "блистал", "находился", "вечно…
    $ to             <chr> "сатир", "пленялись", "стал", "дивился", "бесконечно", …
    $ from_pos       <chr> "NOUN", "VERB", "VERB", "VERB", "ADV", "VERB", "NOUN", …
    $ to_pos         <chr> "NOUN", "VERB", "VERB", "VERB", "ADV", "VERB", "ADJ", "…
    $ from_feats     <chr> "Animacy=Anim|Case=Nom|Gender=Masc|Number=Sing", "Anima…
    $ to_feats       <chr> "Animacy=Anim|Case=Nom|Gender=Masc|Number=Sing", "Anima…
    $ from_sp        <chr> "001", "010", "01", "0010", "10", "010", "0010", "010",…
    $ from_cp        <chr> "1", "10", "1", "10", "10", "10", "10", "10", "10", "10…
    $ from_closure   <chr> "masc", "fem", "masc", "fem", "fem", "fem", "fem", "fem…
    $ from_ending_st <chr> "и'р", "я'лись", "а'л", "и'лся", "е'чно", "а'нет", "о'е…
    $ to_sp          <chr> "01", "010", "1", "010", "0010", "010", "010", "010", "…
    $ to_cp          <chr> "1", "10", "1", "10", "10", "10", "10", "10", "10", "1"…
    $ to_closure     <chr> "masc", "fem", "masc", "fem", "fem", "fem", "fem", "fem…
    $ to_ending_st   <chr> "и'р", "я'лись", "а'л", "и'лся", "е'чно", "а'нет", "о'е…

``` r
# load metadata to extract meters
load("../../data/nkrja_19th_lem.Rda")
rnc_meta <- c19 %>% 
  filter(meter == "Я") %>% 
  mutate(poem_id = paste0("RNC_", Unnamed..0, "_", year),
         meter = paste0("Iamb_", feet)) %>% 
  select(poem_id, meter)
  
rm(c19) # remove large c19 file

# attach meters to rhyme data
rnc_rhymes <- rnc_rhymes %>% 
  left_join(rnc_meta, by = "poem_id") %>% 
  filter(!is.na(meter)) # remove non-iambic poems


# leave only feminine endings
# collect words in one table
rnc1 <- rnc_rhymes %>% 
  filter(from_closure == "fem") %>% 
  select(poem_id, from, from_pos, from_feats, meter) %>% 
  rename(text_id = poem_id,
         word = from,
         pos = from_pos,
         feats = from_feats)

rnc2 <- rnc_rhymes %>% 
  filter(to_closure == "fem") %>% 
  select(poem_id, to, to_pos, to_feats, meter) %>% 
  rename(text_id = poem_id, 
         word = to,
         pos = to_pos,
         feats = to_feats)

all_rnc <- rbind(rnc1, rnc2) 

all_rnc <- all_rnc %>% 
  mutate(pos = ifelse(str_detect(feats, "VerbForm=Inf"), 
                      "VERB_inf", pos),
         pos = ifelse(str_detect(feats, "Mood=Imp"), 
                      "VERB_imp", pos))

glimpse(all_rnc)
```

    Rows: 103,829
    Columns: 5
    $ text_id <chr> "RNC_3870_1777", "RNC_3870_1777", "RNC_7246_1846", "RNC_7246_1…
    $ word    <chr> "равнялись", "находился", "спокоен", "друзьями", "страданьем",…
    $ pos     <chr> "VERB", "VERB", "ADJ", "ADJ", "NOUN", "NOUN", "ADJ", "ADJ", "N…
    $ feats   <chr> "Animacy=Inan|Case=Nom|Gender=Fem|Number=Sing", "Aspect=Perf|G…
    $ meter   <chr> "Iamb_6(7)", "Iamb_6(7)", "Iamb_вольная : 4,5,6", "Iamb_вольна…

Cound POS in rnc i3 – NB these are not only fem endings

``` r
rnc_i3 <- all_rnc %>% 
  filter(str_detect(meter, "Iamb_3"))

rnc_i3_total <- nrow(rnc_i3)

rnc_i3_pos <- rnc_i3 %>% 
  count(pos) %>% 
  rename(rnc_i3 = n) %>% 
  mutate(perc_rnc_i3 = round( (rnc_i3/rnc_i3_total)*100, 1 ))

rnc_i3_pos
```

    # A tibble: 17 × 3
       pos      rnc_i3 perc_rnc_i3
       <chr>     <int>       <dbl>
     1 ADJ         601        16.8
     2 ADP           2         0.1
     3 ADV         150         4.2
     4 AUX           7         0.2
     5 CCONJ         2         0.1
     6 DET          17         0.5
     7 INTJ          2         0.1
     8 NOUN       1697        47.5
     9 NUM           9         0.3
    10 PART          8         0.2
    11 PRON         90         2.5
    12 PROPN        35         1  
    13 VERB        862        24.2
    14 VERB_imp     29         0.8
    15 VERB_inf     18         0.5
    16 X             2         0.1
    17 <NA>         38         1.1

All RNC fem iambs

``` r
rnc_if_total <- nrow(all_rnc)

rnc_if_pos <- all_rnc %>% 
  count(pos) %>% 
  rename(rnc_if = n) %>% 
  mutate(perc_rnc_if = round( (rnc_if/rnc_if_total)*100, 1 ))

rnc_if_pos
```

    # A tibble: 17 × 3
       pos      rnc_if perc_rnc_if
       <chr>     <int>       <dbl>
     1 ADJ       17168        16.5
     2 ADP          24         0  
     3 ADV        3589         3.5
     4 AUX         340         0.3
     5 CCONJ        28         0  
     6 DET         608         0.6
     7 INTJ         12         0  
     8 NOUN      52385        50.5
     9 NUM         190         0.2
    10 PART         99         0.1
    11 PRON       2146         2.1
    12 PROPN       515         0.5
    13 VERB      24266        23.4
    14 VERB_imp    789         0.8
    15 VERB_inf    540         0.5
    16 X            83         0.1
    17 <NA>       1047         1  

i3 kulmann POS

``` r
total_k <- nrow(k_ew_pos)

k_ew_pos %>% 
  count(pos) %>% 
  rename(n_kulmann = n) %>% 
  mutate(perc_kulmann = round( (n_kulmann/total_k)*100 , 1)) %>% 
  left_join(i3_rhymed, by = "pos") %>% 
  left_join(iambs_rhymed, by = "pos") %>% 
  #left_join(rnc_i3_pos, by = "pos") %>% 
  select(pos, perc_kulmann, perc_rhyme_i3, perc_rhyme_iambs
         #, perc_rnc_i3
         ) %>% 
  arrange(desc(perc_rhyme_i3))
```

                 pos perc_kulmann perc_rhyme_i3 perc_rhyme_iambs
    1           NOUN         52.6         55.19            51.10
    2           VERB         13.7         18.12            21.41
    3            ADJ         17.9         17.30            17.09
    4     VERB_prich          2.9          3.10             1.81
    5            ADV          4.3          2.09             3.07
    6           PRON          3.0          1.73             1.97
    7  VERB_deeprich          3.0          1.09             1.58
    8       VERB_inf          1.3          0.64             1.15
    9       VERB_imp          0.5          0.46             0.62
    10           NUM          0.3          0.18             0.08
    11          PART          0.3          0.09             0.06
    12          CONJ          0.0            NA             0.01
    13          <NA>          0.1            NA               NA
