library(tidyverse)
library(tidytext)
library(readtext)

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

hansard_volume_details <- read_csv("data/indices/volume-details.csv")
