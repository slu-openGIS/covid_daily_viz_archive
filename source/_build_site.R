# Build Site and Push ####

#===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===#

# dependencies
library(fs)
library(rmarkdown)

#===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===#

## store date value
date <- Sys.Date()

#===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===#

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
   cape, mid_mo, nomo, ozark, semo, st_jo, swmo, metro_data,
   regional_counties, spring, state_live_data, stl_race, stl_race_gender,
   west, ozark_mtns, df, df.post, df.pre, nemo, df.post2, interval_value_formatter,
   pre, county_vax, dist_vax, regional_data, regional_hosp)

#===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===#

## store date value
date <- Sys.Date()

## confirm auto update data
auto_update <- usethis::ui_yeah("Do you want to automatically update the remote GitHub repo?")

#===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===#

# optionally pushed to GitHub
if (auto_update == TRUE){
   
   system("git add -A")
   system(paste0("git commit -a -m 'build site for ", as.character(date), "'"))
   system("git push origin master")
   
}

# clean-up
rm(date, auto_update)
