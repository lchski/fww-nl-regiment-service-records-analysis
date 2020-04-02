library(rapportools)
library(DescTools)

other_religions <- service_records %>%
  count_group(religion_cleaned) %>%
  filter(prop < 0.01) %>%
  pull(religion_cleaned)

area_of_origin__religion <- service_records %>%
  mutate(religion_cleaned = ifelse(religion_cleaned %in% other_religions, "Other", religion_cleaned)) %>% ## regroup smaller religions under "other"
  mutate(area_of_origin = if_else(id %in% st_johns_records_ids, "St. John's", "Outside St. John's")) %>%
  select(area_of_origin, religion_cleaned)

area_of_origin__religion %>%
  tabyl(religion_cleaned, area_of_origin) %>%
  adorn_percentages(denominator = "col")

area_of_origin__religion__xtab <- area_of_origin__religion %>%
  table() %>%
  t()

area_of_origin__religion__xtab %>%
  chisq.test()

area_of_origin__religion__xtab %>%
  CramerV()

area_of_origin__religion__xtab %>%
  Lambda(direction = "symmetric")
area_of_origin__religion__xtab %>%
  Lambda(direction = "column")
area_of_origin__religion__xtab %>%
  Lambda(direction = "row")
area_of_origin__religion__xtab %>%
  lambda.test()


