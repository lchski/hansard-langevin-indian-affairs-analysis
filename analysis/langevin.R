library(stringdist)
library(gridExtra)

find_similar_words <- function(words_to_search, search_word, threshold) {
  words_to_search %>%
    mutate(search_word_dist = stringsim(word, search_word)) %>%
    filter(search_word_dist >= threshold) %>%
    distinct(word) %>%
    pull(word)
}

find_page_uids_mentioning_words <- function(words_by_page, words_to_search) {
  words_by_page %>%
    filter(word %in% words_to_search) %>%
    select(page_uid) %>%
    distinct() %>%
    pull(page_uid)
}

## find words
words_similar_to_langevin <- hansard_words %>%
  find_similar_words("langevin", 0.75)

words_similar_to_indian <- hansard_words %>%
  find_similar_words("indian", 0.85)

## find page UIDs
langevin_mention_page_uids <- hansard_words %>%
  find_page_uids_mentioning_words(words_similar_to_langevin)

indian_mention_page_uids <- hansard_words %>%
  find_page_uids_mentioning_words(words_similar_to_indian)

## find the pages
pages_mentioning_langevin_and_indian <- hansards %>%
  filter(uid %in% langevin_mention_page_uids) %>%
  filter(uid %in% indian_mention_page_uids)

## count page occurrences by Hansard volume
li_mention_page_count_by_doc <- page_count_by_hansard %>%
  left_join(pages_mentioning_langevin_and_indian %>%
              group_by(doc_id) %>%
              summarize(mention_count = n())) %>%
  mutate(mention_count = replace_na(mention_count, 0)) %>%
  mutate(mention_count_prop = mention_count / page_count)

## plot!
li_mention_page_count_by_doc %>%
  ggplot(aes(x = doc_id, y = mention_count)) +
  geom_col() +
  theme(axis.text.x=element_text(angle=67.5, hjust=1))

li_mention_page_count_by_doc %>%
  ggplot(aes(x = doc_id, y = mention_count_prop)) +
  geom_col() +
  theme(axis.text.x=element_text(angle=67.5, hjust=1))
  


## for comparison, raw string search
hansards %>%
  filter(str_detect(text, regex("Langevin", ignore_case = TRUE))) %>%
  filter(str_detect(text, regex("Indian", ignore_case = TRUE)))
