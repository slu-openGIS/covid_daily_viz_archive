## Testing {.tabset .tabset-fade .tabset-pills .padtop}
These plots show patterns in testing for the focal states, though due to inconsistencies in reporting practices, not all focal states are available for each plot. 

### Per Capita Testing Rates

```{r state-testing-rate, echo=FALSE, out.width = '100%'}
knitr::include_graphics("img/state/k_test_rate.png")
```

Download: [<a href="https://raw.githubusercontent.com/slu-openGIS/covid_daily_viz/master/results/high_res/state/k_test_rate.png" target="_blank">High-res</a>] [<a href="https://raw.githubusercontent.com/slu-openGIS/covid_daily_viz/master/results/low_res/state/k_test_rate.png" target="_blank">Low-res</a>]

### New Test Rates

```{r state-new-testing-rate, echo=FALSE, out.width = '100%'}
knitr::include_graphics("img/state/l_new_tests_avg.png")
```

Download: [<a href="https://raw.githubusercontent.com/slu-openGIS/covid_daily_viz/master/results/high_res/state/l_new_tests_avg.png" target="_blank">High-res</a>] [<a href="https://raw.githubusercontent.com/slu-openGIS/covid_daily_viz/master/results/low_res/state/l_new_tests_avg.png" target="_blank">Low-res</a>]

### Percent Positive

```{r state-pct-pos, echo=FALSE, out.width = '100%'}
knitr::include_graphics("img/state/m_positive_avg.png")
```

Download: [<a href="https://raw.githubusercontent.com/slu-openGIS/covid_daily_viz/master/results/high_res/state/m_positive_avg.png" target="_blank">High-res</a>] [<a href="https://raw.githubusercontent.com/slu-openGIS/covid_daily_viz/master/results/low_res/state/m_positive_avg.png" target="_blank">Low-res</a>]

### Data Table

```{r state-test-table, echo=FALSE, out.width = '100%'}
state_test_data %>%
  select(report_date, state, tests, test_rate, new_tests, new_test_rate_avg, pct_positive, positive_avg) %>%
  mutate(
    new_test_rate_avg = round(new_test_rate_avg, digits = 2),
    positive_avg = round(positive_avg, digits = 2),
    test_rate = round(test_rate, digits = 2),
    pct_positive = round(pct_positive, digits = 2)
  ) %>%
  rename(
    `Report Date` = report_date,
    State = state,
    `Cumulative Tests` = tests,
    `Per Capita Tests` = test_rate,
    `New Tests` = new_tests,
    `Average New Test Rate` = new_test_rate_avg,
    `% Positive` = pct_positive,
    `Average % Positive` = positive_avg
  ) -> data_table

DT::datatable(data_table, rownames= FALSE)
```

### Notes

  * The underlying data for these plots are available from [GitHub](faq.html#How_Do_I_Download_Your_Data) in the `state_testing.csv` table, which is assembled from data provided by [State of Missouri](faq.html#Where_Do_These_Data_Come_From) and the [COVID Tracking Project](faq.html#Where_Do_These_Data_Come_From).
  * Testing data are provided from 23 May onward because data for Missouri prior to that date included both PCR/viral tests and serology tests mixed together. 
  * Missouri occasionally does not report current testing data. This issue was more pronounced in April and May, but these days without data reported were almost exclusively before the 23 May cutoff date discussed above. Since 23 May, there has been one day - 10 July - when the state failed to report data. A technique called "linear interpolation" is used to estimate these missing values. 
  * These data are interpreted to reflect the number of individuals who have been tested and not the number of tests.
  * The FAQ contains short explanations of [per-capita rates](faq.html#What_are_Per_Capita_Rates). Note that the rates presented for testing data are per 100,000 individuals.
  * All averages presented are 7-day [rolling averages](faq.html#What_are_Rolling_Averages).

## Live Daily Testing {.tabset .tabset-fade .tabset-pills .padtop}
The following plot and data table mirror the daily PCR/viral testing data posted on the State of Missouri's dashboard. Unlike the data above, these reflect the number of tests and not the number of individuals tested. They give a sense of overall testing capacity and not the proportion of the population being tested. Unlike other data on this site, the total number of tests may change for multiple days with each update.

### Daily Testing

```{r state-daily_test, echo=FALSE, out.width = '100%'}
knitr::include_graphics("img/state/n_total_tests_avg.png")
```

Download: [<a href="https://raw.githubusercontent.com/slu-openGIS/covid_daily_viz/master/results/high_res/state/n_total_tests_avg.png" target="_blank">High-res</a>] [<a href="https://raw.githubusercontent.com/slu-openGIS/covid_daily_viz/master/results/low_res/state/n_total_tests_avg.png" target="_blank">Low-res</a>]

### Data Table

```{r state-live-table, echo=FALSE, out.width = '100%'}
state_live_data %>%
  select(report_date, pcr_neg, pcr_pos, pcr_int, pcr_total, pcr_avg) %>%
  mutate(
    pcr_avg = round(pcr_avg, digits = 2)
  ) %>%
  rename(
    `Report Date` = report_date,
    `Negative Tests` = pcr_neg,
    `Positive Tests` = pcr_pos,
    `Indeterminate Tests` = pcr_int,
    `Total Tests` = pcr_total,
    `Average Tests` = pcr_avg
  ) -> data_table

DT::datatable(data_table, rownames= FALSE)
```

### Notes

  * The underlying data for these plots are available from [GitHub](faq.html#How_Do_I_Download_Your_Data) in the `state_live_tests.csv` table, which is assembled from data provided by [State of Missouri](faq.html#Where_Do_These_Data_Come_From).
  * All averages presented are 7-day [rolling averages](faq.html#What_are_Rolling_Averages).