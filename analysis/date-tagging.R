library(lubridate)

hansard_3grams <- hansards %>% unnest_tokens(ngram, text, token = "ngrams", n = 3)

hansard_4grams <- hansards %>%
  filter(page_section == "debates") %>%
  unnest_tokens(ngram, text, token = "ngrams", n = 4)

housesat_scores <- hansard_3grams %>%
  mutate(search_word_dist = stringsim(ngram, "the house sat")) %>%
  filter(search_word_dist >= 0.65)

speakerchair_scores <- hansard_4grams %>%
  mutate(search_word_dist = stringsim(ngram, "speaker took the chair")) %>%
  filter(search_word_dist >= 0.65)

speakerchair_scores %>%
  count_group(ngram, search_word_dist) %>%
  write_csv("data/indices/speakerchair_scores.csv")

speakerchair_scores %>%
  filter(search_word_dist >= 0.8) %>%
  filter(! str_detect(ngram, "left|loft")) %>%
  count_group(uid) %>%
  write_csv("data/indices/speakerchair_appearances.csv")

speakerchair_signals <- speakerchair_scores %>%
  count_group(ngram, search_word_dist) %>%
  filter(search_word_dist >= 0.8) %>%
  filter(! str_detect(ngram, "left|loft")) %>%
  pull(ngram)

hansards %>%
  filter(page_section == "debates") %>%
  filter(str_detect(text, speakerchair_signals %>% paste(collapse = "|")))

## get a sense of how many volumes span multiple years
## (so we can mostly infer year, just looking for "MMM DD" or "DDth of MMM" patterns instead)
hansard_volume_details %>%
  select(doc_id, start_date, end_date) %>%
  mutate(syear = year(start_date), eyear = year(end_date)) %>%
  mutate(year_comp = syear == eyear) %>%
  filter(year_comp == FALSE)
