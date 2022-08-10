library(tidyverse)
library(tidytext)

library(e1071)
library(caret)

library(wesanderson)

setwd("Documents/thesis1830s/corpus35_pr/")

# plan:
# take corpus35 from 02_02, get quick elegies & ballads extraction
# count
# lable
# attach non-labled data (everything from per1835)
# do shit

##########################
#### data preparation ####
##########################
#### periodicals data ####

per_meta <- read.delim("meta/database_poems_published_in_journals.tsv") %>% select(-text)
texts <- read.csv("data/01_id_text_lemm.csv") %>% select(-X, -Unnamed..0)
glimpse(texts)

dat <- left_join(texts, per_meta %>% mutate(id = text_ID), by = "id")
glimpse(dat)


#### ballads corpus merge ####

ballads_list <- list.files(path = "/Users/tonya/Downloads/ballads40/", pattern = ".txt")
length(ballads_list)

ballads_meta <- read.delim(file = "/Users/tonya/Downloads/ballads40_metadata.tsv") 
glimpse(ballads_meta)

ballads_df <- tibble(
  id = ballads_list,
  path = paste0("/Users/tonya/Downloads/ballads40/", id),
  text = sapply(path, read_file)
)
  
glimpse(ballads_df)  
write.csv(ballads_df, file = "/Users/tonya/Downloads/ballads_df.csv")  

ballads <- read.csv("/Users/tonya/Downloads/ballads_df.csv")
str(ballads)

ballads <- ballads %>% 
  select(id, text, text_lemm) %>% 
  left_join(ballads_meta, by = "id") %>% 
  mutate(id = paste0("B_", id),
         genre = "BL") %>% 
  select(genre, id, author, title, year, text_lemm)

write.csv(ballads, "data/03_ballads_corpus.csv")
  
#### genre corpora data load ####

el_corpus <- read_tsv("data/03_elegies_corpus.tsv")
songs_corpus <- read_tsv("data/03_russian_songs_core_periodicals.tsv")
ballads_corpus <- read.csv("data/03_ballads_corpus.csv") %>%  select(-X)

glimpse(el_corpus)
glimpse(songs_corpus)
glimpse(ballads_corpus)

#### training sets ####

elegies <- el_corpus %>% 
  filter(year != 1835) %>% 
  mutate(id = paste0("EL_", id),
         type = "EL") %>% 
  select(id, type, text_lem)


songs <- songs_corpus %>% 
  filter(!year %in% c(1835, 1836, 1837, 1838, 1839, 1840)) %>% 
  mutate(id = paste0("RP_", row_number()),
         type = "RP") %>% 
  select(id, type, text_lem)

ballads <- ballads_corpus %>% 
  filter(year != 1840) %>% 
  mutate(id = paste0("BL_", row_number()),
         type = "BL",
         text_lem = text_lemm) %>% 
  select(id, type, text_lem)

glimpse(elegies)
glimpse(songs)
glimpse(ballads)

#### testing sets from 1835--1840 data #### 

# data from 3 corpora

ballads_1835_plus <- ballads_corpus %>% 
  filter(year <= 1840 & title != "Тень") %>% 
  mutate(id = paste0("BL_P_", id),
         type = "BL",
         text_lem = text_lemm) %>% 
  select(id, type, text_lem)

songs_1835 <- songs_corpus %>% 
  filter(year %in% c(1835, 1836, 1837, 1838, 1839, 1840)) %>% 
  mutate(first_line_test = str_remove_all(first_line, "[[:space:]]|[[:punct:]]")) %>% 
  mutate(id = paste0("RP_P_", row_number()),
         type = "RP") %>% 
  select(id, type, text_lem, first_line_test)

elegies_1835_plus <- el_corpus %>% 
  filter(year == 1835) %>% 
  filter(id == 327 | id == 600) %>% 
  mutate(id = paste0("EL_P_", id),
         type = "EL_35") %>% 
  select(id, type, text_lem)

glimpse(ballads_1835_plus) # 34 ballads
glimpse(songs_1835) # 77 songs
glimpse(elegies_1835_plus)


# data from periodicals 1835-1840

songs_antijoin <- dat %>% 
  mutate(first_line_test = str_remove_all(First_line, "[[:space:]]|[[:punct:]]")) %>% 
  filter(first_line_test %in% songs_1835$first_line_test)

glimpse(dat)

# 20
elegies_1835 <- dat %>% 
  mutate(type = ifelse(
    str_detect(Text_title, "[Ээ]леги")|str_detect(Subtitle, "[Ээ]леги"), "EL", "")) %>% 
  filter(type != "") %>% 
  mutate(index = id, 
         id = paste0("EL_", id),
         type = "EL",
         text_lem = text_lemm) %>% 
  select(id, type, text_lem, index)


# 24
ballads_1835 <- dat %>% 
  mutate(type = ifelse(
    str_detect(Text_title, "[Бб]аллад")|str_detect(Subtitle, "[Бб]аллад"), "BL", "")) %>% 
  filter(type != "" & Year != 1840) %>% 
  mutate(index = id, 
         id = paste0("BL_", id),
         type = "BL",
         text_lem = text_lemm, index) %>% 
  select(id, type, text_lem, index)

#### remove these and russian songs from periodicals dataset 

corpus35_nolable <- dat %>% 
  mutate(index = id) %>% 
  filter(!index %in% ballads_1835$index& !index %in% elegies_1835$index & !id %in% songs_antijoin$id) %>% 
  mutate(id = paste0("NL_", id),
         type = "NL",
         text_lem = text_lemm) %>% 
  select(id, type, text_lem)

glimpse(corpus35_nolable)

ballads_1835 <- rbind(ballads_1835 %>% select(-index), ballads_1835_plus) 
glimpse(ballads_1835) # 56 ballads

elegies_1835 <- rbind(elegies_1835 %>% select(-index), elegies_1835_plus)
glimpse(elegies_1835) # 22 elegies

songs_1835 <- songs_1835 %>% select(-first_line_test)
glimpse(songs_1835)

nkrja_nolable <- read.csv("data/02_01_nkrjalem.csv")
glimpse(nkrja_nolable)
nkrja_nolable <- nkrja_nolable %>% 
  filter(year > 1810 & year < 1851) %>% 
  mutate(id = paste0("NL_", id),
         type = "NL", 
         text_lem = text_lemm) %>% 
  select(id, type, text_lem)


# data: 
# elegies, ballads, songs -- sets for training (data from 1835-1840 excluded)
# elegies_1835, ballads_1835, songs_1835 -- sets for testing (data only from 1835-1840)
# corpus35_nolable -- tesing data from periodicals 1835-1840
# nkrja_nolable -- testing data from nkrja (1810-1850)

glimpse(elegies)

save(ballads, songs, elegies, elegies_1835, ballads_1835, songs_1835, 
     corpus35_nolable, nkrja_nolable, file = "data/03_classification_data.Rda")



##########################
#### Experiment ##########
##########################
#### load prepared data ####
load("data/03_classification_data.Rda")

# data: 
# elegies, ballads, songs -- sets for training (data from 1835-1840 excluded)
# elegies_1835, ballads_1835, songs_1835 -- sets for testing (data only from 1835-1840)
# corpus35_nolable -- tesing data from periodicals 1835-1840
# nkrja_nolable -- testing data from nkrja (1810-1850)

data35 <- rbind(elegies_1835, ballads_1835, songs_1835, corpus35_nolable) %>% 
  mutate(type = recode(type, "EL_35" = "EL"),
         corpus = "P", 
         full_type = paste0(type, "_", corpus))

data35$type %>% table()

data_training <- rbind(elegies, ballads, songs, 
                       nkrja_nolable) %>% 
  mutate(corpus = "T", 
         full_type = paste0(type, "_", corpus))
  
all_data <- rbind(data35, data_training)

all_data$type %>% table()
all_data$full_type %>% table()

test_corp <- all_data %>% 
  filter(type %in% c("RP", "NL"))

test_corp$type %>% table()
test_corp$full_type %>% table()
n = (min(test_corp$type %>% table()))
train_size = round(n*0.75)
test_size = n - train_size
train_size
test_size


test_corp <- test_corp %>% 
  filter(full_type != "NL_T")



table(test_corp$type)
glimpse(test_corp)

#table(iambic$type)




# ranks
ranks <- test_corp %>%
  #tokenize by word -> new column "word" out of column "text"
  unnest_tokens(input = text_lem, output = word, token = "words") %>%
  #count & sort
  count(word, sort = T) %>% 
  select(-n) %>% 
  head(4000) # cut wordlist to 4000 MFWs
### nb initially it was 5000 MFW

head(ranks)

# frequencies
freqs_new <- test_corp %>%
  unnest_tokens(input = text_lem, output = word, token = "words") %>% 
  right_join(ranks, by="word") %>% # we are leaving 4000 MFWs that were cut-off earlier 
  count(id, word) %>% # count words within each text
  group_by(id) %>%
  mutate(n = n/sum(n)) %>% 
  # because of group_by() will sum word counts only within titles -> we get relative frequencies for each text (regardless its size)
  #rename(text_title = title) %>%
  mutate(word = factor(word, levels = ranks$word)) %>% #reorder words by their rank
  spread(key = "word", value="n", fill = 0)
# make the table wider

freqs_new[1:10,1:15]

#### z-scores ####

# make a matrix
zscores_50_300 <- freqs_new[,52:301] %>% 
  as.matrix() %>% 
  scale() %>% 
  as_tibble() 

# select ids to attach to matrix
titles <- freqs_new[,1] %>% # take the texts' ids
  ungroup() %>% 
  mutate(text_id = row_number()) %>% # new column for text id regarding its row numb
  bind_cols(zscores_50_300) %>% # attach columns from z-scores table
  mutate(text_genre = str_extract(id, "(^\\w{2})")) # select genre

# check if labels extracted normally
unique(titles$text_genre)

# check the z-scores table
titles[1:10,1:10]
dim(titles)

head(titles)
# new variable to test
class_test <- titles

#ballads_titles <- titles
#russong_titles <- titles

#save(ballads_titles, russong_titles, file = "data/03_mfw_tables.Rda")

#### SVM classification w/ e1071 ####

# n of texts of each genre
class_test$text_genre %>% table()

## deal with disproportionate genres

n = min(table(class_test$text_genre)) 
n

train_size = round(n*0.75)
test_size = n - train_size

# ballads: BL_T = 154, BL_P = 56
# russongs: RP_T = 245, RP_P = 82

train_size
test_size

class_test <- class_test %>% 
  mutate(test_train = ifelse(str_detect(id, "_P_"), "test", "train"))



train_set <- class_test %>% 
  filter(test_train == "train") %>% 
  group_by(text_genre) %>% 
  #sample_n(train_size - 4) %>% # -4 for ballads
  sample_n(249) %>% # for russongs
  #sabmple_n(train_size) %>% 
  ungroup()

test_set <- class_test %>% 
  filter(test_train == "test") %>% 
  group_by(text_genre) %>% 
  sample_n(77) %>% # for russongs
  #sample_n(test_size) %>% 
  ungroup()

# train_set <- class_test %>%
#   group_by(text_genre) %>%
#   sample_n(train_size) %>% # sample n times per each group (genre) 
#   ungroup() 
# 
# test_set <- class_test %>% 
#   anti_join(train_set, by="text_id") %>% # 1. remove already sampled training set from the data
#   group_by(text_genre) %>% 
#   sample_n(test_size) %>% # 2. sample again the test per each genre
#   ungroup() 

train_set[1:5, 1:5]
test_set[1:5, 1:5]

svm_model <-svm(as.factor(text_genre)~.,  
                data = train_set %>% select(-c(id, text_id, test_train)), 
                method = "C-classification", 
                kernel = "linear", 
                cost = 1, 
                scale = T)

summary(svm_model)
prediction <- predict(svm_model, test_set)
confusionMatrix(prediction, as.factor(test_set$text_genre))

misclass <- tibble(id = test_set$id,
                   true_genre = test_set$text_genre,
                   prediction = prediction)

misclass %>% 
  mutate(misclassed = ifelse(true_genre != prediction, 1, 0)) %>% 
  filter(misclassed == 1)

w_genres = t(svm_model$coefs) %*% svm_model$SV

head(svm_model$coefs)

w_genres[1:5]


tibble(weight=w_genres[1,], word=colnames(w_genres)) %>% 
  mutate(genre = case_when(weight > 0 ~ "Non songs", 
                           weight < 0 ~ "Russian song")) %>%
  group_by(genre) %>% 
  mutate(abs=abs(weight)) %>%
  top_n(20,abs) %>% 
  ggplot(aes(reorder(word,abs),abs,fill=genre)) + geom_col() +
  coord_flip() + 
  facet_wrap(~genre,scales="free") +
  #theme(axis.text.y = element_text(size=28)) + 
  theme_minimal(base_size = 16) + 
  labs(x = "", 
       y = "",
       fill = "") + 
  scale_fill_manual(values = wes_palette("Rushmore1")[3:4]) + 
  theme(legend.position = "none") + 
  theme(axis.text = element_text(size = 11),
        axis.title = element_text(size = 11),
        plot.title = element_text(size = 11)) + 
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  theme(axis.line.x = element_line(color="black"), 
        axis.line.y = element_line(color="black"))

ggsave("plots/03_songs_words.png", dpi = 300, plot = last_plot(), 
       width = 5, height = 6, bg = "white")

### now predict classes from unseen test set with the model we have

prediction <- predict(svm_model, test_set %>% select(-c(id, text_id, test_train)))

# final confusion matrix
table(test_set$text_genre, prediction)

