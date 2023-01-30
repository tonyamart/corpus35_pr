library(tidyverse)

setwd("Documents/thesis1830s/corpus35_pr/misc/canon/")
load("Documents/ds/data/ruscorpora_new/nkrja_19th_lem.Rda")

glimpse(c19)

counts <- c19 %>% 
  count(author, sort = T)

c19 %>% 
  filter(author == "А. В. Тимофеев") %>% 
  count()

selected <- c19 %>% 
  select(Unnamed..0, path, author, created, formula, header, text_raw) %>% 
  filter(author == "А. В. Тимофеев")

write.csv(selected, "timofeev.csv")
