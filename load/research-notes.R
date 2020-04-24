library(lubridate)

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
  filter(! is.na(type)) %>%
  mutate(
    date = if_else(
      type == "date",
      as_date(str_remove(text, "[^0-9]*")),
      as_date(NA_character_)
    )
  ) %>%
  mutate(
    page_print_from = if_else(
      type == "reference",
      as.integer(str_match(text, "^([0-9]*)")[,2]),
      NA_integer_
    ),
    page_print_to = if_else(
      type == "reference",
      as.integer(str_match(text, "^[0-9]*–([0-9]*)")[,2]),
      NA_integer_
    ),
    page_pdf_from = if_else(
      type == "reference",
      as.integer(str_match(text, "^[0-9]*–?[0-9]* \\[([0-9]*)")[,2]),
      NA_integer_
    ),
    page_pdf_to = if_else(
      type == "reference",
      as.integer(str_match(text, "^[0-9]*–[0-9]* \\[[0-9]*–([0-9]*)")[,2]),
      NA_integer_
    ),
    speaker = if_else(
      type == "reference",
      str_match(text, "^[0-9]*–?[0-9]* \\[[0-9]*–?[0-9]*\\], ([^:]*):")[,2],
      NA_character_
    )
  ) %>%
  fill(date, page_print_from, page_pdf_from, speaker) %>%
  group_by(date, page_print_from, page_pdf_from, speaker) %>%
  fill(page_print_to, page_pdf_to) %>%
  ungroup() %>%
  mutate_at(
    c("page_print_from", "page_pdf_from"),
    ~ if_else(type == "date", NA_integer_, .)
  ) %>%
  mutate_at(
    c("speaker"),
    ~ if_else(type == "date", NA_character_, .)
  ) %>%
  separate(text, into = c("texta", "textb"), remove = FALSE, sep = ":") %>%
  mutate(text = ifelse(type == "reference", textb, text)) %>%
  mutate(text = trimws(text)) %>%
  select(doc_id:volume, type:speaker, text) %>%
  mutate(citation = paste0(
    "Canada, Parliament, House of Commons Debates, ",
    scales::ordinal(parliament),
    " Parl, ",
    scales::ordinal(session),
    " Sess, Vol ",
    volume,
    " (",
    trimws(format(date, "%e %B %Y")),
    ") at ",
    page_print_from
  ))

notes %>%
  select(-parliament, -session, -volume) %>%
  write_csv("data/out/research-notes.csv")
