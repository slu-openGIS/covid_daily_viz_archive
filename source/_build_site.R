# dependencies
library(fs)
library(rmarkdown)

# copy images into site root directory
dir_copy(path = "results/low_res", new_path = "site/img/", overwrite = TRUE) 

# build site
render_site(input = "site/")

# move site to docs/
dir_copy(path = "site/_site", new_path = "docs/", overwrite = TRUE) 

# clean-up directories
dir_delete(path = "site/_site")
dir_delete(path = "site/img")

# clean-up environment
rm(params, pal, snapshot, map_breaks, map_bins, bins, round_any,
   data_table, state_data, stl_city_data, stl_county_data, kc_city_data,
   kc_metro_data, stl_metro_data, state_test_data,
   county_data, stl_hosp, zip_snapshot, metro_counties, factpal,
   cape, mid_mo, nomo, ozark, semo, st_jo, swmo)
