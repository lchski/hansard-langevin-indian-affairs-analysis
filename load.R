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

hansards <- hansards %>%
  mutate(
    page_section = map2_chr(doc_id, page, identify_section_for_page)
  )

