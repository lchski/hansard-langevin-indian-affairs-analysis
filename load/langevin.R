## NB uncomment all the following to update

## find words
#words_similar_to_langevin <- hansard_words %>%
#  find_similar_words("langevin", 0.75)
#tibble(word = words_similar_to_langevin) %>% write_csv("data/out/words_similar_to_langevin.csv")
words_similar_to_langevin <- read_csv("data/out/words_similar_to_langevin.csv") %>% pull(word)

#words_similar_to_indian <- hansard_words %>%
#  find_similar_words("indian", 0.85)
#tibble(word = words_similar_to_indian) %>% write_csv("data/out/words_similar_to_indian.csv")
words_similar_to_indian <- read_csv("data/out/words_similar_to_indian.csv") %>% pull(word)

## find page UIDs
#pages_mentioning_langevin_uids <- hansard_words %>%
#  find_page_uids_mentioning_words(words_similar_to_langevin)
#tibble(uid = pages_mentioning_langevin_uids) %>% write_csv("data/out/langevin_mention_page_uids.csv")
pages_mentioning_langevin_uids <- read_csv("data/out/langevin_mention_page_uids.csv") %>% pull(uid)

#pages_mentioning_indian_uids <- hansard_words %>%
#  find_page_uids_mentioning_words(words_similar_to_indian)
#tibble(uid = pages_mentioning_indian_uids) %>% write_csv("data/out/indian_mention_page_uids.csv")
pages_mentioning_indian_uids <- read_csv("data/out/indian_mention_page_uids.csv") %>% pull(uid)


## find the pages
pages_mentioning_langevin <- hansards %>%
  filter(uid %in% pages_mentioning_langevin_uids)

pages_mentioning_indian <- hansards %>%
  filter(uid %in% pages_mentioning_indian_uids)


pages_mentioning_langevin_and_indian <- hansards %>%
  filter(uid %in% pages_mentioning_langevin_uids) %>%
  filter(uid %in% pages_mentioning_indian_uids)

pages_mentioning_li_uids <- pages_mentioning_langevin_and_indian %>%
  pull(uid)


## count page occurrences by Hansard volume
count_occurences_by_volume <- function(pages_mentioning_keyword) {
  page_count_by_hansard %>%
    left_join(pages_mentioning_keyword %>%
                group_by(doc_id, page_section) %>%
                summarize(mention_count = n())) %>%
    mutate(mention_count = replace_na(mention_count, 0)) %>%
    mutate(mention_count_prop = mention_count / page_count) %>%
    left_join(hansard_volume_details)
}

## count page occurrences by _section_ of Hansard volume
count_occurences_by_section <- function(pages_mentioning_keyword) {
  hansard_volume_details %>%
    group_by(doc_id) %>%
    pivot_longer(
      c(frontmatter_page_count:index_page_count),
      names_to = "section",
      values_to = "page_count"
    ) %>%
    mutate(section = str_remove(section, "_page_count")) %>%
    select(doc_id:volume, start_date, end_date, section, page_count) %>%
    ungroup() %>%
    left_join(
      pages_mentioning_keyword %>%
        group_by(doc_id, page_section) %>%
        summarize(mention_count = n()),
      by = c("doc_id" = "doc_id", "section" = "page_section")
    ) %>%
    group_by(doc_id, section) %>%
    mutate(mention_count = replace_na(mention_count, 0)) %>%
    mutate(mention_count_prop = mention_count / page_count)
}

### regroup by parliament/session
regroup_section_occurences_by_parlsess <- function(occurrences_by_section) {
  occurrences_by_section %>%
    ungroup() %>%
    group_by(parliament, session, section) %>%
    summarize(
      n_vols = n_distinct(volume),
      start_date = min(start_date),
      end_date = max(end_date),
      page_count = sum(page_count),
      mention_count = sum(mention_count)
    ) %>%
    ungroup() %>%
    mutate(
      mention_count_prop = mention_count / page_count,
      parliament_session_id = paste(parliament, session, sep = "-")
    ) %>%
    select(parliament_session_id, everything())
}