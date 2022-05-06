library(tidyverse)

setwd("Documents/corpus1835/periodicals_to_ocr/txt_raw/")

load("~/Downloads/nkrja_19th_lem.Rda")
glimpse(c19)

subset35 <- c19 %>% 
  filter(diff <= 5) %>% 
  filter(year > 1830 & year < 1840)

dat <- read_tsv("database_poems_published_in_journals.tsv")

glimpse(dat)
glimpse(subset35)

dat <- dat %>% 
  mutate(first_line = str_remove_all(First_line, "[[:space:]]|[[:punct:]]"))

subset35 <- subset35 %>% 
  mutate(first_line = str_extract_all(text_raw, "^(.*)\n")) %>% 
  mutate(first_line = str_remove_all(first_line, "\n|[[:punct:]]|[[:space:]]")) %>% 
  filter(!is.na(first_line))

intersection <- inner_join(dat, subset35, by = "first_line")

glimpse(intersection)

write.table(intersection, file = "db_nkrja_intersection.tsv", sep = "\t")

glimpse(intersection)

marks <- intersection %>% 
  select(first_line, Unnamed..0, author, text_ID)

data <- left_join(dat, marks, by = "text_ID")
glimpse(data)

write.table(data, file = "meta_per_col_nkrja.tsv", sep = "\t")

# intersection$text_ID[1]
# x <- as.vector(intersection[1,58])
# y <- "test2.txt"
# writeLines(x$text_raw, y)

setwd("nkrja_poems/")

text <- NULL
fh <- NULL

for (i in 1:length(intersection$text_ID)) {
  text <- as.vector(intersection[i,58])
  #fh <- paste0(intersection$text_ID[i],".txt")
  fh <- paste0(intersection$Unnamed..0[i], ".txt")
  writeLines(text$text_raw, fh)
}
