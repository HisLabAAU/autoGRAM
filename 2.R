library(tidyverse)
library(readxl)

pos <- read.csv("Run_ads_standardised_pos_long.csv")

verbs <- pos %>% 
  filter(pos == "VERB") %>% 
  mutate(id = doc_id + 1,
         id = paste("ST_CROIX_", id, sep = ""),
         start_char = start_char) %>% 
  select(-doc_id)

df <- read.csv("Run_ads_standardised.csv") %>% 
  mutate(id = paste("ST_CROIX_", X, sep = ""))

verbs_full <- verbs %>% 
  left_join(df)

verbs_full_ready <- verbs_full %>% 
  select(id, date, text, token, start_char, end_char) %>% 
  rename(verb = token)

verbs_full_ready %>% write.csv("Verbs_ready_for_llm.csv")
