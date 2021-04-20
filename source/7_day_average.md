## Average New Cases by County {.tabset .tabset-fade .tabset-pills .padtop}
This map shows a seven-day rolling average of new cases. For this map, this covers `r params$date_val` back through `r params$prior_date_val`. There is not a threshold for what constitutes a high or low average, but the higher the average number of new cases, the more new spread we can infer. For mapping purposes, these are displayed as a rate per 100,000 residents. As with the prior map, additional details are available by clicking on each county or on the data table.

### Interactive Map

```{r map-confirmed-avg, echo = FALSE, out.width = '100%', out.height='600px', warning=FALSE}
# calculate breaks
bins <- map_bins(snapshot, var = "case_avg_rate", style = "fisher", classes = 5, dig_lab = 2,
                 round = 2)
bins[length(bins)] <- round_any(x = max(snapshot$case_avg_rate), accuracy = .01, f = ceiling)

# create color palette
pal <- colorBin("RdPu", domain = snapshot$case_avg_rate, bins = bins)

# map
leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(
    data = snapshot,
    color = "#444444", 
    weight = 1, 
    opacity = 1.0, 
    smoothFactor = 0.5,
    fillOpacity = 0.75,
    fillColor = ~pal(case_avg_rate),
    highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
    popup = paste("<b>County:</b> ", snapshot$county, "<br>",
                           "<b>Current Cumulative Count:</b> ", snapshot$cases, "<br>",
                           "<b>7-day Average of New Cases:</b> ", round(snapshot$case_avg, digits = 2), "<br>",
                           "<b>Average New Cases per 100,000 Residents:</b> ", round(snapshot$case_avg_rate, digits = 2), "<br>",
                           "<b>Hospital Count:</b> ", snapshot$hospitals, "<br>",
                           "<b>Baseline ICU Beds:</b> ", snapshot$icu_beds)
    )  %>%
    addLegend(pal = pal, values = snapshot$avg_rate, opacity = .75, title = "7-day Average Rate")
```

### Static Map

```{r new-case-static, echo=FALSE, out.width = '100%'}
knitr::include_graphics("img/county/d_new_case_map.png")
```

Download: [<a href="https://raw.githubusercontent.com/slu-openGIS/covid_daily_viz/master/results/high_res/county/d_new_case_map.png" target="_blank">High-res</a>] [<a href="https://raw.githubusercontent.com/slu-openGIS/covid_daily_viz/master/results/low_res/county/d_new_case_map.png" target="_blank">Low-res</a>]

### Data Table

```{r confirmed-avg-data-table, echo=FALSE, out.width = '100%'}
snapshot %>%
  select(state, county, cases, case_avg, case_avg_rate) %>%
  mutate(
    case_avg = round(case_avg, digits = 2),
    case_avg_rate = round(case_avg_rate, digits = 2)
  ) %>%
  rename(
    State = state,
    County = county,
    `Cumulative Cases` = cases,
    `7-day Average New Cases` = case_avg,
    `Average New Cases per 100,000 Residents` = case_avg_rate
  ) %>%
  arrange(State, County) -> data_table

st_geometry(data_table) <- NULL

DT::datatable(data_table)
```

### Notes

  * The underlying data for these maps are available from [GitHub](faq.html#How_Do_I_Download_Your_Data) in the `daily_snapshot_mo_xl.geojson` spatial data set, which is assembled from data provided by the [New York Times](faq.html#Where_Do_These_Data_Come_From).
  * The FAQ contains a short explanation of [per-capita rates](faq.html#What_are_Per_Capita_Rates).
  * All averages presented are 7-day [rolling averages](faq.html#What_are_Rolling_Averages).