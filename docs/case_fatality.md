## CVOID-19 Case Fatality Rates
This map shows the "case fatality rate" - the number of individuals who die expressed as a percentage of all confirmed cases. It is important to note that, [early in infectious disease outbreaks, case fatality rates are often artificially high](https://www.thelancet.com/journals/laninf/article/PIIS1473-3099(20)30244-9/fulltext). We should expect case fatality rates to decrease over time. As with the second map, raw counts of deaths are available by clicking on individual counties.

<br>

```{r map-case-fatality, echo = FALSE, out.width = '100%', out.height='600px'}
pal <- colorNumeric("YlGn", snapshot$case_fatality_rate)

leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(
    data = snapshot,
    color = "#444444", 
    weight = 1, 
    opacity = 1.0, 
    smoothFactor = 0.5,
    fillOpacity = 0.5,
    fillColor = ~pal(case_fatality_rate),
    highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
    popup = paste("<b>County:</b> ", snapshot$county, "<br>",
                           "<b>Current Confirmed Count:</b> ", snapshot$confirmed, "<br>",
                           "<b>Current Mortality Count:</b> ", snapshot$deaths, "<br>",
                           "<b>Current Case Fatality Rate:</b> ", round(snapshot$case_fatality_rate, digits = 2),"%")
    )  %>%
    addLegend(pal = pal, values = snapshot$case_fatality_rate, opacity = .5, title = "Percent")
```

<br>