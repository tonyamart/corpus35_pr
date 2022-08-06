library(tidyverse)
library(tidytext)

theme_set(theme_minimal())

setwd("Documents/thesis1830s/corpus35_pr/")

#### Check test annotations ####
test_lines_cut <- read.csv('data/test_lines_cut.csv')
test_annot <- read.csv('data/test_stanza_labled.csv', sep = ";")

glimpse(test_lines_cut)
glimpse(test_annot)

# some table cleaning
test_syl <- test_lines_cut %>% 
  separate(id_st_line, into = c('P', 'id', 'stanza', 'line')) %>% 
  mutate(id = paste0(P, "_", id),
         id_st = paste0(id, "_", stanza)) %>% 
  select(id, id_st, line, line_cut, line_syl)

head(test_syl)


# function to calculate mode in each group
syl_mode <- function(x) {
  t <- table(x)
  names(t)[which.max(t)]
}


test_syl_stats <- test_syl %>% 
  group_by(id_st) %>% 
  # add a column what shows that a stanza has all lines of an equal length
  mutate(equal_syl = as.numeric(n_distinct(line_syl) == 1)) %>% 
  summarise(pattern_list = list(line_syl), # pattern used to counts
    pattern = paste0(line_syl, collapse = "-"), # pattern for visual assessement
    equal_syl = mean(equal_syl), # equal n of syllables in the whole stanza
    syl_modes = as.numeric(syl_mode(unlist(pattern_list))), # most freq syllable number
    syl_min = min(unlist(pattern_list)), # min number of syllables
    syl_max = max(unlist(pattern_list)), # max
    syl_var = syl_max - syl_min) # variation



test_all <- left_join(test_annot, test_syl_stats, by = "id_st") %>% select(-X, -Unnamed..0)

glimpse(test_all)

unique(test_all$meter)

test_all <- test_all %>% 
  mutate(meter_cln = recode(meter, 
                              "iambos" = "iamb", 
                              "daktylos" = "daktyl", 
                              "choreios" = "trochee", 
                              "dolnik3" = "dolnik3"))

test_all %>% 
  group_by(id) %>% 
  mutate(same_meter = as.numeric(n_distinct(meter_cln) == 1)) %>% 
  select(id, same_meter, meter_cln, true_meter, true_feet) %>% 
  distinct()

# count N lines
# count syl mode
# count % of lines not equal to mode

glimpse(test_syl)

test_syl %>% 
  group_by(id, line_syl) %>% 
  count() %>% 
  ungroup() %>% 
  group_by(id) %>% 
  summarise(id = id, 
            line_syl = line_syl, 
            n = n,
            perc = n/sum(n)) %>% 
  filter(perc > 0.5)


test_all %>% 
  mutate(binary_ternary = ifelse(meter_cln %in% c('iamb', "trochee"), 2, 3)) %>% 
  mutate(syl_modes = ifelse(meter_cln == 'trochee', syl_modes+1, syl_modes)) %>% 
  mutate(feet_reg = ifelse(equal_syl == 1, syl_modes/binary_ternary, NA))
