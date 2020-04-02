library(tidyverse)
library(janitor)
library(naniar)

library(helpers)

service_records <- read_tsv("data/source/therooms.ca/records.tsv") %>%
  clean_names() %>%
  remove_extra_columns() %>% ## get rid of any accidental extra columns
  replace_with_na_all(
    ~ str_to_lower(.x) %in% c("n/a", "no information", "unknown")
  ) %>% ## make real NA out of would-be NA
  mutate(
    fatality = fatality == "Yes",
    po_w = po_w == "Yes"
  ) %>% ## convert to logical
  mutate(id = row_number()) %>%
  mutate(religion_cleaned = case_when(
    str_detect(religion, regex("ch of england", ignore_case = TRUE)) ~ "Church of England (Anglican)",
    str_detect(religion, regex("methodist", ignore_case = TRUE)) ~ "Methodist",
    str_detect(religion, regex("catholic", ignore_case = TRUE)) ~ "Roman Catholic",
    religion == "CE" ~ "Church of England (Anglican)",
    religion %in% c(
      "1916",
      "Also served with the Newfoundland Royal Naval Reserve, # 1699X.",
      "Attestation papers not included."
    ) ~ NA_character_,
    TRUE ~ religion
  ))

st_johns_records_ids <- service_records %>%
  filter(str_detect(community, regex("st. *john", ignore_case = TRUE))) %>%
  pull(id)

service_records %>%
  mutate(from_st_johns = id %in% st_johns_records_ids) %>%
  count_group(from_st_johns, religion_cleaned)
