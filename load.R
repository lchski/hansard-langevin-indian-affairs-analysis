library(tidyverse)
library(tidytext)
library(readtext)

library(stringdist)

library(helpers)

hansards <- readtext(
  "data/source/parl.canadiana.ca/*.txt",
  docvarsfrom = "filenames",
  docvarnames = c("parliament", "session", "volume"),
  dvsep = "-"
) %>%
  as_tibble() %>%
  separate_rows(text, sep = "\f") %>% ## separate on form feed character, inserted by pdftotext for page breaks
  group_by(doc_id) %>%
  mutate(page = row_number()) %>%
  ungroup() %>%
  select(doc_id, parliament:page, text) %>%
  mutate(uid = paste(doc_id, page))

hansard_words <- hansards %>%
  select(doc_id, page, text) %>%
  unnest_tokens(word, text) %>%
  mutate(page_uid = paste(doc_id, page))

page_count_by_hansard <- hansards %>%
  group_by(doc_id) %>%
  summarize(page_count = n())

hansard_volume_details <- read_csv(
    "data/indices/volume-details.csv",
    col_types = cols(
      doc_id = col_character(),
      start_date = col_date(format = ""),
      end_date = col_date(format = ""),
      debates_start = col_double(),
      index_start = col_double(),
      is_bilingual = col_logical()
    )
  ) %>%
  mutate(frontmatter_start = 1, frontmatter_end = debates_start - 1) %>%
  mutate(debates_end = index_start - 1) %>%
  left_join(page_count_by_hansard %>% rename(index_end = page_count)) %>%
  select(
    doc_id:end_date,
    is_bilingual,
    frontmatter_start, frontmatter_end,
    debates_start, debates_end,
    index_start, index_end
  ) %>%
  mutate(
    frontmatter_page_count = frontmatter_end - frontmatter_start + 1,
    debates_page_count = debates_end - debates_start + 1,
    index_page_count = index_end - index_start + 1
  )

identify_section_for_page <- function(doc_id_to_check, page_number_to_lookup) {
  hansard_volume <- hansard_volume_details %>%
    filter(doc_id == doc_id_to_check) %>%
    slice(1)
  
  section <- case_when(
    pull(hansard_volume, frontmatter_start) <= page_number_to_lookup & page_number_to_lookup <= pull(hansard_volume, frontmatter_end) ~ "frontmatter",
    pull(hansard_volume, debates_start) <= page_number_to_lookup & page_number_to_lookup <= pull(hansard_volume, debates_end) ~ "debates",
    pull(hansard_volume, index_start) <= page_number_to_lookup & page_number_to_lookup <= pull(hansard_volume, index_end) ~ "index",
    TRUE ~ NA_character_
  )
  
  return(section)
}

## NB: uncomment to update page sections; saved file has parliaments 1 through 7
#hansards <- hansards %>%
#  mutate(
#    page_section = map2_chr(doc_id, page, identify_section_for_page)
#  )
#hansards %>% select(uid, page_section) %>% write_csv("data/out/uids_tagged_with_section.csv.gz")

hansards <- hansards %>%
  left_join(read_csv("data/out/uids_tagged_with_section.csv.gz"))

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
