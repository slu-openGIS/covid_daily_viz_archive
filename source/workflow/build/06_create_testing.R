
## read data
test_data <- read_csv("data/source/mo_tests/historic_tests.csv") %>%
  mutate(date = mdy(date)) %>%
  rename(report_date = date)

## subset to relevant range
test_subset <- filter(test_data, report_date >= as.Date("2020-05-23")) %>%
  select(report_date, cases, tests_pcr_total)

## linear interpolation
test_subset$tests_pcr_total <- na.approx(test_subset$tests_pcr_total)

## combine with full data
# test_subset %>%
#  select(report_date, tests_impute) %>%
#  left_join(test_data, ., by = "report_date") %>%
#  mutate(new_tests = tests_impute - lag(tests_impute)) %>%
#  mutate(pct_positive = cases/tests_impute*100) %>%
#  mutate(positive_avg = rollmean(pct_positive, k = 7, align = "right", fill = NA)) %>%
#  mutate(state = "Missouri") %>%
#  select(report_date, state, everything()) -> test_data

# clean-up
# rm(test_subset)

## tidy
test_subset %>%
  rename(tests = tests_pcr_total) %>%
  mutate(
    new_cases = cases - lag(cases),
    new_tests = tests - lag(tests),
    pct_positive = new_cases/new_tests*100,
    positive_avg = rollmean(pct_positive, k = 7, align = "right", fill = NA),
    state = "Missouri"
  ) -> test_subset

## out of state data
c("ar", "ks", "il", "ok") %>%
  unlist() %>%
  map_df(~read_csv(paste0("https://covidtracking.com/api/v1/states/", .x, "/daily.csv"), 
                   col_types = cols(fips = col_character()))) %>%
  mutate(
    report_date = ymd(as.character(date)),
    state = case_when(
      state == "AR" ~ "Arkansas",
      state == "KS" ~ "Kansas",
      state == "IL" ~ "Illinois",
      state == "OK" ~ "Oklahoma"
    ),
    tests = positive + negative
  ) %>%
  rename(cases = positive) %>%
  select(report_date, state, cases, tests) %>%
  group_by(state) %>%
  arrange(report_date) %>%
  mutate(
    new_cases = cases - lag(cases),
    new_tests = tests - lag(tests),
    pct_positive = new_cases/new_tests*100,
    pct_positive = ifelse(is.nan(pct_positive), 0, pct_positive),
    positive_avg = rollmean(pct_positive, k = 7, align = "right", fill = NA)
    ) %>%
  ungroup() %>%
  bind_rows(test_subset, .) %>%
  arrange(report_date, state) %>%
  filter( report_date >= as.Date("2020-05-23")) -> test_full

# clean-up
rm(test_data,test_subset)

# load state reference data
state_pop <- read_csv("data/source/state_pop.csv")

# calculate rates
test_full %>%
  left_join(., state_pop, by = c("state" = "NAME")) %>%
  group_by(state) %>%
  arrange(report_date) %>%
  mutate(test_rate = tests/total_pop*100000,
         new_test_rate = new_tests/total_pop*100000,
         new_test_rate_avg = rollmean(new_test_rate, k = 7, align = "right", fill = NA)) %>%
  ungroup() %>%
  arrange(report_date, state) %>%
  select(-total_pop) %>%
  select(report_date, state, cases, new_cases, tests, test_rate, 
         new_tests, new_test_rate, new_test_rate_avg,
         pct_positive, positive_avg) -> test_full

# fix scientific notation
# test_full$test_rate <- format(test_full$test_rate, scientific=FALSE)
# test_full$new_test_rate <- format(test_full$new_test_rate, scientific=FALSE)  

# write data
write_csv(test_full, path = "data/state/state_testing.csv")

# clean-up
rm(state_pop, test_full)

## read live data
live_data <- read_csv("data/source/mo_tests/live_tests.csv") 

## clean live data
live_data %>%
  mutate(pcr_total = pcr_neg + pcr_pos + pcr_int) %>%
  mutate(pcr_avg = rollmean(pcr_total, k = 7, align = "right", fill = NA)) -> live_data

# write data
write_csv(live_data, path = "data/state/state_live_tests.csv")

# clean-up
rm(live_data)
