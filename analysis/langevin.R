library(gridExtra)

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

pages_mentioning_li_uids <- pages_mentioning_langevin_and_indian %>%
  pull(uid)
  

## count page occurrences by Hansard volume
li_mention_page_count_by_doc <- page_count_by_hansard %>%
  left_join(pages_mentioning_langevin_and_indian %>%
              group_by(doc_id, page_section) %>%
              summarize(mention_count = n())) %>%
  mutate(mention_count = replace_na(mention_count, 0)) %>%
  mutate(mention_count_prop = mention_count / page_count) %>%
  left_join(hansard_volume_details)

## count page occurrences by _section_ of Hansard volume
li_mention_page_count_by_doc_section <- hansard_volume_details %>%
  group_by(doc_id) %>%
  pivot_longer(
    c(frontmatter_page_count:index_page_count),
    names_to = "section",
    values_to = "page_count"
  ) %>%
  mutate(section = str_remove(section, "_page_count")) %>%
  select(doc_id, start_date, end_date, section, page_count) %>%
  ungroup() %>%
  left_join(
    pages_mentioning_langevin_and_indian %>%
      group_by(doc_id, page_section) %>%
      summarize(mention_count = n()),
    by = c("doc_id" = "doc_id", "section" = "page_section")
  ) %>%
  group_by(doc_id, section) %>%
  mutate(mention_count = replace_na(mention_count, 0)) %>%
  mutate(mention_count_prop = mention_count / page_count)
  


## plot!
### by filename
li_mention_page_count_by_doc %>%
  ggplot(aes(x = doc_id, y = mention_count)) +
  geom_col() +
  theme(axis.text.x=element_text(angle=67.5, hjust=1))

li_mention_page_count_by_doc %>%
  ggplot(aes(x = doc_id, y = mention_count_prop)) +
  geom_col() +
  theme(axis.text.x=element_text(angle=67.5, hjust=1))

hansards %>%
  mutate(mentions_li = uid %in% pages_mentioning_li_uids) %>%
  ggplot(aes(x = doc_id, fill = mentions_li)) +
  geom_bar(position = "identity")

#### break it down by section
li_mention_page_count_by_doc_section %>%
  ggplot(aes(x = doc_id, y = mention_count_prop, fill = section)) +
  geom_col()

li_mention_page_count_by_doc_section %>%
  filter(section == "debates") %>%
  ggplot(aes(x = doc_id, y = mention_count_prop, fill = section)) +
  geom_col() +
  theme(axis.text.x=element_text(angle=67.5, hjust=1))


### by date
li_mention_page_count_by_doc %>%
  ggplot(aes(x = start_date, y = mention_count_prop)) +
  geom_col() +
  theme(axis.text.x=element_text(angle=67.5, hjust=1))

#### break it down by section
li_mention_page_count_by_doc_section %>%
  ggplot(aes(x = start_date, y = mention_count_prop, fill = section)) +
  geom_col()

li_mention_page_count_by_doc_section %>%
  filter(section == "debates") %>%
  ggplot(aes(x = start_date, y = mention_count_prop, fill = section)) +
  geom_col() +
  theme(axis.text.x=element_text(angle=67.5, hjust=1))
  


## for comparison, raw string search
hansards %>%
  filter(str_detect(text, regex("Langevin", ignore_case = TRUE))) %>%
  filter(str_detect(text, regex("Indian", ignore_case = TRUE)))


## output for easier reading
pages_mentioning_langevin_and_indian %>%
  filter(page_section == "debates") %>%
  write_csv("data/out/li-pages-debates.csv")

pages_mentioning_langevin_and_indian %>%
  filter(page_section == "debates") %>%
  filter(parliament == 1) %>%
  write_csv("data/out/li-pages-debates-parl-1.csv")



pages_mentioning_li_expanded_uids <- pages_mentioning_langevin_and_indian %>%
  filter(page_section == "debates") %>%
  select(doc_id, page) %>%
  mutate(page = map(page, ~ c(.x - 1, .x, .x + 1))) %>% ## get page before and after each identified page
  unnest(page) %>%
  distinct() %>%
  mutate(uid = paste(doc_id, page)) %>%
  pull(uid)

hansards %>%
  filter(uid %in% pages_mentioning_li_expanded_uids) %>%
  mutate(
    directly_mentions_li = uid %in% pages_mentioning_li_uids,
    directly_mentions_l  = uid %in% langevin_mention_page_uids,
    directly_mentions_i  = uid %in% indian_mention_page_uids
  ) %>%
  left_join(hansard_volume_details %>% select(doc_id, end_date)) %>%
  filter(end_date < "1870-01-01") %>%
  select(doc_id, page, directly_mentions_li:directly_mentions_i, text, uid, page_section) %>%
  write_csv("data/out/li-pages-exp-debates-sgia.csv")







langevin_key_dates <- read_csv("data/indices/langevin-key-dates.csv") %>%
  mutate(grouping = factor(grouping, unique(grouping), ordered = TRUE)) %>%
  pivot_longer(start:end, names_to = "state", values_to = "date")
  
langevin_key_dates %>%
  ggplot(aes(x = date, y = grouping, group = grouping)) +
  geom_line(size = 2) +
  geom_text(aes(label = description))

langevin_key_dates %>%
  ggplot() +
  geom_line(
    data = langevin_key_dates %>%
      filter(grouping == "parliament"),
    mapping = aes(x = date, y = grouping),
    size = 2,
    alpha = 0.1
  ) +
  geom_text(
    data = langevin_key_dates %>%
      filter(grouping == "parliament") %>%
      filter(state == "start"),
    mapping = aes(x = date, y = grouping, label = description)
  ) +
  geom_line(
    data = langevin_key_dates %>%
      filter(grouping == "governing party"),
    mapping = aes(x = date, y = "prime minister", color = description),
    size = 2,
    alpha = 0.25,
    show.legend = FALSE
  ) +
  scale_color_manual(values = c("blue", "red"), name="description") +
  geom_text(
    data = langevin_key_dates %>%
      filter(grouping == "prime minister") %>%
      filter(state == "start"),
    mapping = aes(x = date, y = grouping, label = description),
    angle = -30,
    nudge_y = -0.375,
    vjust = -4
  ) +
  geom_point(
    data = langevin_key_dates %>%
      filter(grouping == "prime minister") %>%
      filter(state == "start"),
    mapping = aes(x = date, y = grouping)
  ) +
  geom_line(
    data = langevin_key_dates %>%
      filter(grouping == "MP"),
    mapping = aes(x = date, y = grouping)
  )


