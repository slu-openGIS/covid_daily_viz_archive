## Daily Testing Data {.tabset}
Daily testing data are complied from a number of sources. For Missouri, the data set has been sourced directly from data posted by the [Missouri Department of Health & Senior Services](https://health.mo.gov/living/healthcondiseases/communicable/novel-coronavirus/results.php) using a combination of data provided by [Denis Beganovic](https://twitter.com/beganovic2021), screen shots provided by The Atlantic's [COVID Tracking Project](https://covidtracking.com), and the [wayback machine](http://web.archive.org). For other states, data is sourced from The Atlantic's [COVID Tracking Project](https://covidtracking.com).

### New Tests Average

```{r new-testing-rate, echo=FALSE, out.width = '100%'}
knitr::include_graphics(here::here("results", "high_res", "state", "l_new_tests_avg.png"))
```

### Testing Rate

```{r testing-rate, echo=FALSE, out.width = '100%'}
knitr::include_graphics(here::here("results", "high_res", "state", "k_test_rate.png"))
```

### Percent Positive

```{r percent-positive, echo=FALSE, out.width = '100%'}
knitr::include_graphics(here::here("results", "high_res", "state", "m_positive_avg.png"))
```

### Missouri Data
For Missouri, total numbers of test data are not available for many days in March and some days in April and May. For April and May, missing data are imputed using a linear technique. If Day 1 had 100 tests, and Day 3 had 200, Day 2 would assumed to have had 150 tests. Rates are per 100,000 residents, and averages are 7-day rolling averages.

```{r missouri-test-table, echo=FALSE, out.width = '100%'}
state_test_data %>%
  filter(state == "Missouri") %>%
  mutate(tests = ifelse(is.na(tests) == TRUE, tests_impute, tests)) %>%
  select(report_date, cases, tests, test_rate, new_tests, new_test_rate_avg, positive_avg) %>%
  mutate(
    test_rate = round(test_rate, digits = 2),
    new_test_rate_avg = round(new_test_rate_avg, digits = 2),
    positive_avg = round(positive_avg, digits = 2)
  ) %>%
  arrange(desc(report_date)) %>%
  rename(
    `Report Date` = report_date,
    `Cumulative Cases` = cases,
    `Cumulative Tests` = tests,
    `Test Rate` = test_rate,
    `New Tests` = new_tests,
    `Average New Test Rate` = new_test_rate_avg,
    `Average Percent Positive` = positive_avg
  ) -> data_table

data_table
```

### All Data
For Missouri, total numbers of test data are not available for many days in March and some days in April and May. For April and May, missing data are imputed using a linear technique. If Day 1 had 100 tests, and Day 3 had 200, Day 2 would assumed to have had 150 tests. Rates are per 100,000 residents, and averages are 7-day rolling averages.

```{r full-test-table, echo=FALSE, out.width = '100%'}
state_test_data %>%
  mutate(tests = ifelse(is.na(tests) == TRUE, tests_impute, tests)) %>%
  select(report_date, state, cases, tests, test_rate, new_tests, new_test_rate_avg, positive_avg) %>%
  mutate(
    test_rate = round(test_rate, digits = 2),
    new_test_rate_avg = round(new_test_rate_avg, digits = 2),
    positive_avg = round(positive_avg, digits = 2)
  ) %>%
  arrange(desc(report_date)) %>%
  rename(
    `Report Date` = report_date,
    `State` = state,
    `Cumulative Cases` = cases,
    `Cumulative Tests` = tests,
    `Test Rate` = test_rate,
    `New Tests` = new_tests,
    `Average New Test Rate` = new_test_rate_avg,
    `Average Percent Positive` = positive_avg
  ) -> data_table

data_table
```