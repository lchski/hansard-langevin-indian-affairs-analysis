library(stringdist)

hansards %>%
  filter(str_detect(text, regex("Langevin", ignore_case = TRUE))) %>%
  filter(str_detect(text, regex("Indian", ignore_case = TRUE)))

likely_langevins <- hansard_words %>%
  mutate(langevin_dist = stringsim(word, "langevin")) %>%
  filter(langevin_dist >= 0.75) %>%
  distinct(word) %>%
  pull(word)

langevin_mention_page_uids <- hansard_words %>%
  filter(word %in% likely_langevins) %>%
  select(page_uid) %>%
  distinct() %>%
  pull(page_uid)

likely_indians <- hansard_words %>%
  mutate(indian_dist = stringsim(word, "indian")) %>%
  filter(indian_dist >= 0.85) %>%
  distinct(word) %>%
  pull(word)

indian_mention_page_uids <- hansard_words %>%
  filter(word %in% likely_indians) %>%
  select(page_uid) %>%
  distinct() %>%
  pull(page_uid)



pages_mentioning_langevin_and_indian <- hansards %>%
  filter(uid %in% langevin_mention_page_uids) %>%
  filter(uid %in% indian_mention_page_uids)

pages_mentioning_langevin_and_indian %>% 
  ggplot(aes(x = doc_id)) +
  geom_bar() +
  theme(axis.text.x=element_text(angle=67.5, hjust=1))

pages_mentioning_langevin_and_indian %>%
  group_by(doc_id) %>%
  summarize(mention_count = n()) %>%
  left_join(page_count_by_hansard) %>%
  mutate(mention_count_prop = mention_count / page_count) %>%
  ggplot(aes(x = doc_id, y = mention_count_prop)) +
  geom_col() +
  theme(axis.text.x=element_text(angle=67.5, hjust=1))
  
