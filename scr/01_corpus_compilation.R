library(tidyverse)

setwd("/Users/tonya/Documents/thesis1830s/corpus35_pr/")

# save:
load('data/corpus_periodicals_nolables.Rda')


#### Corpus compilation from folder (raw files) #####

filelist <- list.files(path = "texts/", pattern = ".txt", full.names = TRUE)

dat <- tibble(#path = filelist,
              id = str_remove_all(filelist, "^texts//|\\.txt$"),
              text = sapply(filelist, read_file))

glimpse(dat)

#### 1 row = 1 stanza table ####

dat_stanza <- dat %>% 
  separate_rows(text, sep = "\n\n") %>% 
  group_by(id) %>% 
  mutate(id_st = paste0(id, "_", row_number())) %>% 
  select(id, id_st, text) %>% 
  ungroup()

glimpse(dat_stanza)

write.csv(file = 'data/df_stanzas.csv', dat_stanza)


#### 1 row = 1 line table ####

dat_lines <- dat_stanza %>% 
  separate_rows(text, sep = "\n") %>% 
  group_by(id_st) %>% 
  mutate(id_st_line = paste0(id_st, "_", row_number())) %>% 
  ungroup()

glimpse(dat_lines)

write.csv(file = 'data/df_lines.csv', dat_lines)


save(dat, dat_stanza, dat_lines, file = "corpus_periodicals.Rda")

#### samples ####
test <- dat %>% 
  sample_n(10)

test_st <- test %>% 
  separate_rows(text, sep = '\n\n') %>% 
  group_by(id) %>% 
  mutate(id_st = paste0(id, "_", row_number())) %>% 
  select(id, id_st, text) %>% 
  ungroup()

test_lines <- test_st %>% 
  separate_rows(text, sep = "\n") %>% 
  group_by(id_st) %>% 
  mutate(id_st_line = paste0(id_st, "_", row_number())) %>% 
  ungroup()

write.csv(file = 'data/test_stanza.csv', test_st)  
write.csv(file = 'data/test_lines.csv', test_lines)  
  
#### prepare .txt-s for stress annotation ####

glimpse(dat)

# small preproc for ru_accent_poet
dat_str <- dat %>% 
  mutate(text_cln = str_remove_all(text, "'")) %>% 
  mutate(text_cln = str_replace_all(text_cln, "-", " - ")) %>% 
  mutate(text_cln = str_replace_all(text_cln, "/", "")) %>% 
  mutate(text_cln = str_remove_all(text_cln, "<.*?>"))

glimpse(dat_str)

setwd("texts_stressed/")
 
text <- NULL
fh <- NULL

for (i in 1:length(dat_str$id)) {
  text <- as.vector(dat_str[i,3])
  fh <- paste0(dat_str$id[i], '.txt')
  writeLines(text$text, fh)
}  

#### corpus compilation w/ stressed data ####

filelist2 <- list.files(path = "texts_stressed/", pattern = ".accented.txt", full.names = TRUE)

dat_stressed <- tibble(id = str_remove_all(filelist2, "^texts_stressed//|\\.accented\\.txt$"),
                       text = sapply(filelist2, read_file))

glimpse(dat_stressed)

#### join with stanza meter annotation ####

dat_stanza <- dat_stressed %>% 
  separate_rows(text, sep = "\n\n") %>% 
  group_by(id) %>% 
  mutate(id_st = paste0(id, "_", row_number())) %>% 
  select(id, id_st, text) %>% 
  ungroup()

glimpse(dat_stanza)

# some stanzas are empty of [[:punct:]], there'll be no metrical data for them
dat_stanza %>% 
  filter(text == "" | str_detect(text, "^[[:punct:]]+$"))

dat_stanza %>% 
  filter(str_detect(text, "^[[:punct:]]+$|\\d+"))

# import and shorten meters table (rupo meter annotator)
meters <- read.csv("data/all_stanza_labled.csv")
glimpse(meters)

unique(meters$meter)

meters <- meters %>% 
  mutate(meter = recode(meter,
                        "iambos" = "iamb",
                        "choreios" = "trochee", 
                        "amphibrachys" = "amphibrach",
                        "anapaistos" = "anapaest", 
                        "daktylos" = "dactyl")) %>% 
  select(id_st, meter)

dat_lables <- left_join(dat_stanza, meters, by = "id_st")

glimpse(dat_lables)

write.csv(dat_lables, file = "temp_stanza_accent_meters.csv")

#### data for lemmatization ####
load("data/corpus_periodicals_nolables.Rda")

head(dat)

dat <- dat %>% 
  separate_rows(text, sep = "\n") %>% 
  #remove <>
  mutate(text = str_remove_all(text, "<.*?>")) %>% 
  # remove empty lines
  filter(text != "" | str_detect(text, "^[[:punct:]]+$")) %>% 
  group_by(id) %>% 
  mutate(text = paste0(text, collapse = "\n")) %>% 
  distinct() %>% 
  ungroup()

head(dat)

write.csv(dat, file = "data/id_text_lemm.csv")


#### nkrja preparation ####

load("data/19th_corpus_sampled.rda")
glimpse(ru19_fin)

unique(ru19_fin$meter_gr)

nkrja19 <- ru19_fin %>% 
  mutate(text_raw = "",
         meter_gr = recode(meter_gr, 
                           "Х" = "trochee",
                           "Я" = "iamb", 
                           "Аф" = "amphibrach",
                           "Ан" = "anapaest", 
                           "Д" = "dactyl"),
         feet_gr = recode(feet_gr, "В" = "free"),
         formula = paste0(meter_gr, "_", feet_gr),
         id = paste0("N_", index)) %>% 
  select(id, text_raw, text_lemm, year, formula, meter_gr, feet_gr, verses) %>% 
  rename("meter" = "meter_gr",
         "feet" = "feet_gr",
         "n_lines" = "verses")

#write.csv(nkrja19, file = "data/02_01_nkrjalem.csv")

load("data/19th_corpus_sampled.rda")
glimpse(ru19_fin)
nkrja_authors <- ru19_fin %>% 
  select(index, author) %>% 
  mutate(index = paste0("N--", index))

write.csv(nkrja_authors, file = "data/02_01_nkrja_authors.csv")
