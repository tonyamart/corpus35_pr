library(tidyverse)

setwd("Documents/thesis1830s/corpus35_pr/")

########## ru_accent_poet stresses annotation #########

# load data: 1 row = 1 stanza
# text: stressed 
# meter label: from rupo (.classity_metre)

#### function to_binary ####
to_binary <- function(df,document="doc_id",text="acc_text") {
  
  vowels <-  c("а", "е","ё", "у", "о", "ю", "я", "и", "ы", "э")
  #cons <- c("б", "в", "г", "д", "ж", "з", "й", "к", "л", "м", "н", "п", "р", "с", "т", "ф", "х", "ц", "ч", "ш", "щ")
  
  syl_pattern = paste(vowels, collapse="|")
  #cons_pattern <- paste(cons, collapse="|")
  
  ## send to lower case, split by lines, add row ids
  unnested <- df %>% mutate(acc_text = str_to_lower(!! sym(text)),
                            acc_text = str_replace_all(acc_text,"ё", "ё'"),
                            acc_text = str_split(acc_text, "\n")) %>%
    unnest(acc_text) %>% 
    mutate(row_id=row_number())
  
  
  binarized <- unnested %>%
    # replace all vowels with 0 and extract only 0 and 0' patterns
    mutate(stress_pattern = str_replace_all(acc_text, syl_pattern,"0"),
           stress_pattern = str_extract_all(stress_pattern, "0|'")) %>% 
    # unnest extracted lists
    unnest(stress_pattern) %>% 
    group_by(row_id, !! sym(document)) %>% 
    # join back by row
    summarise(stress_pattern=paste(stress_pattern, collapse="")) %>% 
    mutate(stress_pattern = str_replace_all(stress_pattern, "0'", "1"))
  
  return(binarized)
}

#### stanza feet / rhytm annotation ####

dat_lables <- read.csv("data/temp_stanza_accent_meters.csv")
glimpse(dat_lables)

st_stresses <- to_binary(dat_lables, document = "id_st", text = "text")

glimpse(st_stresses)

st_stresses <- st_stresses %>% 
  mutate(st_no_claus = str_remove_all(stress_pattern, "0+$"),
         n_syl = sapply(st_no_claus, nchar)
         )

glimpse(st_stresses)


st_stresses <- st_stresses %>% 
  mutate(binary_type = ifelse(str_detect(stress_pattern, "101"), "binary", ""), 
         binary_type = ifelse(str_detect(
           stress_pattern,
           "^0101|^010001|^01000101|^00010101|^00000101|^01000001"), 
           "iamb", binary_type),
         binary_type = ifelse(str_detect(
           stress_pattern, 
           "^1010|^001010|^00001010|^10001010|^10000010"), "trochee", binary_type))

st_stresses %>% 
  group_by(binary_type) %>% 
  count(sort = T)

# # table to check manually detected mono-meter stanzas
# binaries <- st_stresses %>% 
#   group_by(id_st) %>% 
#   mutate(equal_bi = as.numeric(n_distinct(binary_type) == 1)) %>% 
#   ungroup() %>% 
#   filter(equal_bi == 1) %>% 
#   select(id_st, binary_type) %>% 
#   distinct()
# 
# glimpse(binaries)

# # function to calculate mode in each group
# syl_mode <- function(x) {
#   t <- table(x)
#   names(t)[which.max(t)]
# }
# 
# glimpse(st_stresses)
# 
# test_syl_stats <- st_stresses %>% 
#   group_by(id_st) %>% 
#   # add a column what shows that a stanza has all lines of an equal length
#   mutate(equal_syl = as.numeric(n_distinct(n_syl) == 1)) %>% 
#   summarise(pattern_list = list(n_syl), # pattern used to counts
#             pattern = paste0(n_syl, collapse = "-"), # pattern for visual assessement
#             equal_syl = mean(equal_syl), # equal n of syllables in the whole stanza
#             syl_modes = as.numeric(syl_mode(unlist(pattern_list))), # most freq syllable number
#             syl_mean = mean(unlist(pattern_list)),
#             syl_min = min(unlist(pattern_list)), # min number of syllables
#             syl_max = max(unlist(pattern_list)), # max
#             syl_var = syl_max - syl_min,
#             rhythm = list(stress_pattern)) # variation
#
# test_all <- left_join(dat_lables, test_syl_stats, by = "id_st") 
# test_all <- left_join(test_all, binaries, by = "id_st")
# 
# glimpse(test_all)
# 
# test_all <- test_all %>% 
#   filter(text != "")

meters_all <- read.csv("data/all_stanza_labled.csv") %>% select(-X, -Unnamed..0)
glimpse(meters_all)

# basic calculation on N poems in each meter
# baseline is 55% of a poem in is one meter
poems_meters <- meters_all %>% 
  mutate(meter = recode(meter,
                        "iambos" = "iamb",
                        "choreios" = "trochee", 
                        "amphibrachys" = "amphibrach",
                        "anapaistos" = "anapaest", 
                        "daktylos" = "dactyl")) %>% 
  filter(!is.na(meter)) %>% 
  group_by(id, meter) %>% 
  count() %>% # this count shows that some poems (e.g. P_1001) might have 1 stanza as dolnik3 and 6 as trochee
  ungroup() %>% 
  group_by(id) %>% 
  summarise(id = id, 
            meter = meter,
            n = n,
            perc_meter = n/sum(n)) %>% # count lables in each poem and calculate perc of the stanzas with a label: for P_1001 got 0.857 trochee and 0.143 dolnik
  filter(perc_meter > 0.55) %>% 
  top_n(1, perc_meter) %>% 
  ungroup() 

poems_meters
length(unique(poems_meters$id)) # 1324 poems roughly covered with a meter lable

# general meter distribution in the corpus
poems_meters %>% 
  count(meter, sort = T) %>% 
  summarise(meter = meter,
            n = n, 
            perc = n/sum(n))

  

#### feet #### 

glimpse(st_stresses)

feet_reg <- st_stresses %>%
  separate(id_st, into = c("P", "id", "st"), sep = "_") %>% 
  mutate(id = paste0(P, "_", id)) %>% 
  group_by(id, n_syl) %>%
  count() %>%
  ungroup() %>%
  group_by(id) %>%
  summarise(id = id,
            n_syl = n_syl, 
            n_lines = n,
            perc_feet = n/sum(n)) %>%
  filter(perc_feet > 0.7) %>% 
  top_n(1, perc_feet) %>% 
  ungroup()


glimpse(feet_reg) # 702 poems with +- reg feet
length(unique(feet_reg$id))

glimpse(poems_meters)

metrical_data <- left_join(poems_meters, feet_reg, by = "id") %>% select(-n)

glimpse(metrical_data)

unique(metrical_data$meter)

metrical_data <- metrical_data %>% 
  mutate(binary_ternary = ifelse(meter %in% c('iamb', "trochee"), 2, 3)) %>% 
  mutate(n_syl = ifelse(meter %in% c("trochee", "amphibrach"), n_syl + 1, n_syl),
         n_syl = ifelse(meter == "dactyl", n_syl + 2, n_syl),
         feet = n_syl/binary_ternary) %>% 
  mutate(feet = ifelse(is.na(feet), "free", feet))

metrical_data %>% 
  sample_n(10)

# overview all formulas:
metrical_data %>% 
  mutate(formula = paste0(meter, "_", feet)) %>% 
  group_by(formula) %>% 
  count(sort = T) %>% 
  ungroup() %>% 
  summarise(formula = formula,
            n = n, 
            perc = n/sum(n))

metrical_data_fin <- metrical_data %>% 
  select(id, meter, feet, perc_meter, perc_feet)

write.csv(metrical_data_fin, file = "data/metrical_annotation_monometers_70.csv")


