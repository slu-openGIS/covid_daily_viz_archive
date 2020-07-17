covid\_daily\_viz
================

[![DOI](https://zenodo.org/badge/249867864.svg)](https://zenodo.org/badge/latestdoi/249867864)

## Introduction

This is a collection of resources for tracking the COVID-19 outbreak in
Missouri. It is being actively maintained and updated nightly with new
data from the [Johns Hopkins CSSE COVID-19
project](https://github.com/CSSEGISandData/COVID-19) and the [New York
Times COVD-19 project](https://github.com/nytimes/covid-19-data). The
most recent day’s worth of data are drawn from the Johns Hopkins nightly
updates, while all prior days are sourced from the New York Times. The
Times data is released on a slower schedule but tends to be more
consistent, while the Hopkins data is less consistent day-to-day but is
released faster. Separately, zip code data are being collected as of
2020-04-01 for the City of St. Louis and 2020-04-06 for St. Louis
County.

From these raw data, the following core data sets are being generated:

  - `state_full.csv` - State-level data for MO, IL, KS, and OK beginning
    2020-01-24
  - `metro_full.csv` - Metro-level data for MO (including portions of
    metros in IL, KS, and OK) beginning 2020-01-24
  - `county_full.csv` - County-level data for MO, IL, KS, and OK
    beginning 2020-01-24
  - `zip_stl_city.csv` - St. Louis City ZACTA (zip code) level data
    beginning 2020-04-01
  - `zip_stl_county.csv` - St. Louis County ZACTA (zip code) leve data
    beginning 2020-06-01

These data sets use a common set of varible names and are all “long”
data. They are used to generate daily snapshots for mapping:

  - `daily_snapshot_mo.geojson` - County-level data for MO
  - `daily_snapshot_mo_xl.geojson` - County-level data for MO along with
    IL, OK, and KS counties in Missouri’s Metropolitain Statistical
    Areas (MSAs)
  - `daily_snapshot_kc.geojson` - County-level data for MO and KS
    counties in the Kansas City MSA
  - `daily_snapshot_stl.geojson` - County-level data for MO and IL
    counties in the St. Louis MSA
  - `daily_snapshot_city_county.geojson` - ZACTA (zip code) data for the
    City of St. Louis and St. Louis County

All of the daily snapshot data were last updated on 2020-07-16.

There are several additional data sets, including convenience summaries
of county-level data for the St. Louis and Kansas City MSAs and separate
daily snapshots at the ZCTA-level for City of St. Louis and St. Louis
County separately, rather than together.

## Citing This Repository

All data and code here are released under a [CC-BY 4.0
license](LICENSE). Citation details, including a DOI number, are
available via [Zenodo](https://zenodo.org/badge/latestdoi/249867864).

## Notes

### Mortality

The Johns Hopkins data contain all known deaths from COVID-19 in the
City of St. Louis, beginning with 31-year-old Red Cross employee
[Jazmond Dixon on
March 23rd](https://www.kmov.com/news/jazmond-dixon-city-s-first-covid--death-was-hardworking/article_e1ce066a-6d3a-11ea-9780-cf42a4d49ace.html).
A [St. Charles man in
his 70s](https://www.stltoday.com/news/local/metro/live-stl-area-coronavirus-updates-march-here-s-what-we/article_a286c517-46b2-5ac4-ac65-26ff7756aca1.html)
died the same day, the first death for that county. The first death in
St. Louis County occured several days earlier on
[March 20](https://www.stltoday.com/news/local/metro/live-stl-area-coronavirus-updates-march-here-s-what-we/article_183103d8-d6ed-5f64-8f0d-a990479266b5.html),
when a women in her 60s passed away.

### Mitigation Measures

| Date           | County           | Action                         | Source                                                                                                                                                                       |
| -------------- | ---------------- | ------------------------------ | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| March 24, 2020 | St. Charles      | Stay at home “guidance” begins | [St. Louis Post-Disptch](https://www.stltoday.com/news/local/metro/live-stl-area-coronavirus-updates-march-here-s-what-we/article_a286c517-46b2-5ac4-ac65-26ff7756aca1.html) |
| March 23, 2020 | Jefferson        | Stay at home order begins      | "                                                                                                                                                                            |
|                | St. Louis City   | Stay at home order begins      | "                                                                                                                                                                            |
|                | St. Louis County | Stay at home order begins      | "                                                                                                                                                                            |
