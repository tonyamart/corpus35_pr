library(tidyverse)
library(tidytext)
library(topicmodels)

library(philentropy)

library(ggrepel)
library(patchwork)
library(wesanderson)
theme_set(theme_minimal())

setwd("Documents/thesis1830s/corpus35_pr/")

#### load data ####
load("data/02_01_75k_lda_output.Rda")

# add metadata w/ authors
# count authors in nkrja & in periodicals (<- regex for reverting names)
# comparison on n of lines in each sets

# id: author-name_corpus
# distances between authors
# nb! include MF authors who are not in the corpus
# MDS projection

##### quick overview on periodicals data #####
# lda_metadata <- corpus35lda  %>% 
#   tidy(matrix = "gamma")  %>% 
#   select(document)  %>% 
#   unique() %>% 
#   separate(document, into = c("index", "year", "first_line", "formula"), sep = "_") %>% 
#   mutate(corpus = str_replace_all(index, "^(\\w)--(\\d+.*)", "\\1")) %>% 
#   mutate(id = index,
#          index = str_replace_all(id, "(\\w--\\d+)(-.*)", "\\1"))
# 
# head(lda_metadata)
# 
# test_meta <- lda_metadata %>% 
#   filter(corpus == "P") %>% 
#   inner_join(per_meta %>% 
#               rename("index" = "text_ID") %>% 
#               mutate(index = str_replace_all(index, "_", "--")), by = "index")
# 
# glimpse(test_meta)
# 
# # quick test if inner join is ok
# test_meta %>% 
#   filter(is.na(first_line))


texts <- read.csv("data/01_id_text_lemm.csv")
per_meta <- read.delim("meta/database_poems_published_in_journals.tsv")
glimpse(texts)

dat <- left_join(texts, per_meta %>% mutate(id = text_ID), by = "id") %>% select(-X, -Unnamed..0)
glimpse(dat)

dat_lines <- dat %>% 
  group_by(id) %>% 
  separate_rows(text_lemm, sep = "\n") %>% 
  filter(text_lemm != "") %>% 
  ungroup()


glimpse(dat_lines)

dat %>% 
  filter(!is.na(Author_Initials)) %>% 
  group_by(Author_Initials) %>% 
  count()
# 164 authors

#### basic stats plot ####

top_20_authors <- dat_lines %>% 
  filter(!is.na(Author_Initials)) %>% 
  group_by(Author_Initials) %>% 
  count(sort = T) %>% 
  head(20) %>% 
  ggplot(aes(x = reorder_within(Author_Initials, n, Author_Initials), y = n)) + 
  geom_col(width = 0.5, fill = wes_palette("Rushmore")[4]) + coord_flip() + 
  scale_x_reordered() + 
  #scale_fill_manual(values = wes_palette("Rushmore")[4]) + 
  labs(y = "Количество строк",
       x = "", 
       title = "Авторы") + 
  theme(plot.title = element_text(hjust = 0.5),
        plot.margin = margin(1,1,1,0.5, "cm"),
        axis.text = element_text(size = 11))

periodicals <- dat_lines %>% 
  group_by(PER_ID) %>% 
  count(sort = T) %>% 
  filter(n > 1000) %>% 
  ggplot(aes(x = reorder_within(PER_ID, n, PER_ID), y = n)) + 
  geom_col(width = 0.5, fill = wes_palette("Rushmore")[3]) + coord_flip() + 
  scale_x_reordered() + 
  labs(y = "Количество строк",
       x = "", 
       title = "Периодические издания") + 
  theme(plot.title = element_text(hjust = 0.5),
        plot.margin = margin(1,1,1,0.5, "cm"),
        axis.text = element_text(size = 11))

years  <- dat_lines %>% 
  filter(Year != 1834) %>% 
  group_by(Year) %>% 
  count() %>% 
  ggplot(aes(x = as.factor(Year), y = n)) + 
  geom_col(width = 0.5, fill = wes_palette("Chevalier1")[1]) + 
  labs(x = "Год",
       y = "Количество строк", 
       title = "Распределение по годам") + 
  theme(plot.title = element_text(hjust = 0.5),
        plot.margin = margin(1,1,1,0.5, "cm"),
        axis.text = element_text(size = 11))

layout <- "
AABBB
CCBBB
"

years + top_20_authors +periodicals + plot_layout(design = layout)

ggsave(filename = "plots/02_per_corpus_stats.png", plot = last_plot(), 
       height = 10, width = 9, dpi = 300, bg = "white")



#### tokens / types count ####
sampled <- read.csv("data/02_01_per_sampled_labled.csv")

corpus_tokens <- sampled %>% 
  unnest_tokens(input = text_lemm, output = word, token = "words") %>% 
  group_by(word) %>% 
  count(sort = T)

# 13 876 types
# 180 207 words
corpus_tokens %>% 
  ungroup() %>% 
  summarise(sum(n))




####### projection data ######
# periodicals metadata
per_meta <- read.delim("meta/database_poems_published_in_journals.tsv")
glimpse(per_meta)

per_authors <- per_meta %>% 
  select(text_ID, Author_Initials) %>% 
  rename("index" = "text_ID",
         "author" = "Author_Initials") %>% 
  mutate(index = str_replace_all(index, "_", "--"),
         author = str_replace_all(author, "(\\w+)([[:space:]])(\\w\\..*)", "\\1_\\3"),
         author = str_remove_all(author, "\\."))

# nkrja metadata (only authors)
nkrja_authors <- read.csv("data/02_01_nkrja_authors.csv") %>% select(-X)
glimpse(nkrja_authors)

unique(nkrja_authors$author)
nkrja_authors <- nkrja_authors %>% 
  mutate(author_cln = str_replace_all(
    author, "(\\w\\. \\w\\. \\w+)([[:space:]]:[[:space:]])(.*)", "\\1"),
    author_cln = str_remove_all(author_cln, "[[:space:]]\\(.*?\\)"),
    author_cln = str_replace_all(
      author_cln, "(\\w\\. \\w\\.)([[:space:]])(\\w+-?\\w?)", "\\3_\\1"),
    author_cln = str_remove_all(author_cln, "\\.|[[:space:]]")) %>% 
  mutate(author = author_cln) %>% 
  select(-author_cln)

meta_authors <- rbind(per_authors, nkrja_authors) %>% filter(!is.na(author))
glimpse(meta_authors)

#### attach meta to the gamma values ####
glimpse(gamma)
gamma_w_meta <- gamma %>% 
  mutate(year_span = floor(as.numeric(year)/5)*5) %>% 
  filter(year_span > 1824 & year_span < 1846) %>% 
  mutate(index_sample = index,
         index = str_replace_all(index_sample, "^(\\w--\\d+)(-.*)", "\\1")) %>% 
  left_join(meta_authors, by = "index")

gamma_w_meta %>% 
  group_by(index, corpus) %>% 
  count() %>% 
  select(-n) %>% 
  ungroup() %>% 
  group_by(corpus) %>% 
  count() # texts: N = 2326, P = 1743

gamma_w_meta %>% 
  filter(corpus == "N") %>% 
  group_by(index, author) %>% 
  count() %>% 
  select(-n) %>% 
  ungroup() %>% 
  group_by(author) %>% 
  count(sort = T)

#### select only authors present in both corpora ####

authors_per <- as.vector(unique(per_authors$author))
authors_nkrja <- as.vector(unique(nkrja_authors$author))

authors_intersection <- intersect(authors_per, authors_nkrja)


gamma_w_meta %>% 
  mutate(author_corpus = paste0(corpus, "_", author)) %>% 
  select(index, author, corpus, year, author_corpus) %>% 
  distinct() %>% 
  filter(!is.na(author) & author != "Тютчев_ФИ") %>% 
  filter(author %in% authors_intersection) %>% 
  group_by(author, corpus) %>% 
  count(sort = T) %>% 
  ungroup() %>% 
  filter(n > 5 ) %>% 
  # group_by(corpus) %>% 
  # top_n(20) %>% 
  mutate(corpus = recode(corpus, "N" = "National corpus (1830-1845)", "P" = "Periodicals (1835-1840")) %>% 
  ggplot(aes(x = reorder_within(author, n, corpus), y = n, fill = corpus)) + 
  geom_col(width = 0.5) + coord_flip() + 
  scale_x_reordered() + 
  facet_wrap(~corpus, scales = "free") + 
  scale_fill_manual(values = wes_palette("Rushmore1")[3:4]) + 
  labs(y = "Количество текстов",
       x = "", 
       fill = "Корпус") + 
  theme(legend.position = "None")

# ggsave(file = "plots/02_authors_intersection.png", plot = last_plot(),
#        height = 7, width = 6, dpi = 300, bg = "white")

#### intersection gamma data ####

#save(gamma_w_meta, authors_intersection, file = "data/02_03_projection.Rda")

load("data/02_03_projection.Rda")

n_texts <- gamma_w_meta %>% 
  filter(author %in% authors_intersection) %>%
  mutate(id = paste0(corpus, "_", author)) %>% 
  group_by(id) %>% 
  count() %>% 
  mutate(n_t = n/75)

gamma_proj <- gamma_w_meta %>%
  filter(author %in% authors_intersection) %>%
  mutate(id = paste0(corpus, "_", author)) %>% 
  group_by(id, topic) %>% 
  summarise(gamma_avg = mean(gamma)) %>% 
  ungroup()


meta_proj <- gamma_w_meta %>%
  filter(author %in% authors_intersection) %>%
  mutate(id = paste0(corpus, "_", author)) %>% 
  group_by(id, corpus, author) %>% 
  count() %>% 
  select(-n) %>% 
  distinct()

ids <- gamma_proj$id %>% unique()

wide <- gamma_proj %>%
  pivot_wider(names_from = topic, values_from = gamma_avg) %>% 
  select(-id)

# matrix
distances <- wide %>%
  as.matrix() %>% 
  JSD(unit="log2") %>% 
  `rownames<-`(ids) 

distances[1:3,1:3]

#### Multi-Dimensional Scaling ####

mds <- cmdscale(distances, eig = TRUE, k = 2) # 2d projection from dist

mds$points


#### Projection plot ####
projection_df <- tibble(x = mds$points[,1],
       y = mds$points[,2],
       id = rownames(mds$points)) %>% 
  left_join(meta_proj, by = "id") %>% 
  filter(id != "P_Пушкин_АС")

glimpse(projection_df)

authors_intersection

benedictovshina <- c("Якубович_ЛА", "Ершов_ПП", 
                     "Тимофеев_АВ", "Бенедиктов_ВГ", "Кукольник_НВ", "Пушкин_АС")

pushkin <- c("Козлов_ИИ", "Теплова_НС", "Подолинский_АИ",
             "Струйский_ДЮ", "Ознобишин_ДП", "Губер_ЭИ", 
             "Бахтурин_КА", "Пушкин_АС")

others <- c("Кольцов_АВ", "Некрасов_НА", "Полежаев_АИ", 
            "Шевырев_СП", "Милькеев_ЕЛ", "Огарев_НП", "Федоров_БМ")

canon <- c("Баратынский_ЕА", "Жуковский_ВА")

projection_df %>% 
  filter(str_detect(author, "Розен"))


projection_df %>% 
  filter(author %in% pushkin) %>% 
  ggplot(aes(x, y, color = author)) + 
  geom_point(data = projection_df, aes(x, y), color = wes_palette("Rushmore1")[4]) + 
  geom_label_repel(aes(label = id),
                  box.padding = 1.2, 
                  max.overlaps = Inf,
                  ) + 
  scale_color_manual(values = c(wes_palette("Darjeeling1"), wes_palette("Darjeeling2")[2:5])) + 
  labs(x = "", y = "", color = "Author") + 
  theme(legend.position = "None")

ggsave(file = "plots/02_3_proj_pushkinsk.png", plot = last_plot(), 
       height = 8, width = 10, dpi = 300, bg = "white")


