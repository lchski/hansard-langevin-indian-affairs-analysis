

notes <- readtext(
  "data/source/research-notes/*.txt",
  docvarsfrom = "filenames",
  docvarnames = c("type", "parliament", "session", "volume"),
  dvsep = "-"
) %>%
  as_tibble() %>%
  select(doc_id, parliament:volume, text) %>%
  separate_rows(text, sep = "\n") %>%
  mutate(
    type = case_when(
      str_detect(text, "^##") ~ "date",
      str_detect(text, "^[0-9]") ~ "reference",
      str_detect(text, "[[:blank:]]*-") ~ "note",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(! is.na(type))


