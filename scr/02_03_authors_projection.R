library(tidyverse)
library(tidytext)
library(topicmodels)

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
