# covid_daily_viz

[![DOI](https://zenodo.org/badge/249867864.svg)](https://zenodo.org/badge/latestdoi/249867864)

This repository contains the data vizualization and website creation scripts that power for the [Missouri COVID-19 Tracking Project](http://slu-opengis.github.io/covid_daily_viz/). The data set creation scripts have been separated as part of the conversion of the original code into a more modular, better documented set of resources. They can now be found in the [`MO_HEALTH_Covid_Tracking`](https://github.com/slu-openGIS/MO_HEALTH_Covid_Tracking) repository.

## Using This Respository
The following `R` packages are needed:

```r
install.packages(c("tidyverse", "classInt", "DT", "fontawesome", "fs", "gghighlight", 
    "ggrepel", "ggthemes", "leaflet", "RColorBrewer", "scales", "sf", "zoo"))
```

The `tidveryse` packages used include: `dplyr`, `forcats`, `ggplot2`, `purrr`, `readr`, `rlang`, `stringr`, `tibble`, and `tidyr`.

Not all packages are loaded at the top of the master scripts or `.Rmd` files that built the website itself. The functions in `source/functions` 

## Citing This Repository
All data and code here are released under a [CC-BY 4.0 license](LICENSE). Citation details, including a DOI number, will be available via Zenodo soon.