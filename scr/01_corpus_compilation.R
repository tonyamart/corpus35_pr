library(tidyverse)

# save:
# load('data/corpus_periodicals.Rda)

##### Corpus compilation from folder #####

filelist <- list.files(path = "texts/", pattern = ".txt", full.names = TRUE)

dat <- tibble(path = filelist,
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
  
  
  