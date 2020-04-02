library(rapportools)

other_religions <- service_records %>%
  count_group(religion_cleaned) %>%
  filter(prop < 0.01) %>%
  pull(religion_cleaned)

service_records %>%
  mutate(religion_cleaned = ifelse(religion_cleaned %in% other_religions, "Other", religion_cleaned))

service_records %>%
  mutate(from_st_johns = id %in% st_johns_records_ids) %>%
  count_group(from_st_johns, religion_cleaned) %>%
  filter(count > 10) %>%
  arrange(religion_cleaned, from_st_johns)

service_records %>%
  mutate(area_of_origin = if_else(id %in% st_johns_records_ids, "St. John's", "Outside St. John's")) %>%
  count_group(area_of_origin, religion_cleaned) %>%
  arrange(religion_cleaned, area_of_origin) %>%
  pivot_wider(
    id_cols = religion_cleaned,
    names_from = area_of_origin,
    values_from = count,
    values_fill = list(count = 0)
  )

area_of_origin__religion__xtab <- service_records %>%
  mutate(area_of_origin = if_else(id %in% st_johns_records_ids, "St. John's", "Outside St. John's")) %>%
  select(area_of_origin, religion_cleaned) %>%
  table()

area_of_origin__religion__xtab %>%
  chisq.test()

area_of_origin__religion__xtab %>%
  lambda.test()


