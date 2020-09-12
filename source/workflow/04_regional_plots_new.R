# plot regional data ####

# load data ####
region_data <- read_csv("data/region_meso.csv",
                        col_types = cols(region = col_character()
                       ))

# define plotting values, state trend ####
## primary values
state_values <- list(
  top_val = round_any(x = max(region_data$case_avg), accuracy = 100, f = ceiling),
  peak_val = region_data %>% 
    filter(region == "Missouri") %>% 
    pull(var = case_avg) %>% 
    max(),
  peak_x = -50,
  peak_y = -70,
  current_x = 0,
  current_y = 350
)

## tables
state_values[["peak_tbl"]] <- region_data %>%
  filter(region == "Missouri") %>%
  filter(case_avg == state_values$peak_val) %>%
  mutate(text = paste0("peak average of ", round(case_avg, digits = 2), " cases reported on ", format(report_date, format = "%d %b")))

state_values[["current_tbl"]] <- region_data %>%
  filter(region == "Missouri") %>%
  filter(report_date == date) %>%
  mutate(text = paste0("current average of ", round(case_avg, digits = 2), " cases reported on ", format(report_date, format = "%d %b")))

# define plotting values, st. louis trend ####
## primary values
stl_values <- list(
  peak_val = region_data %>% 
    filter(region == "St. Louis") %>% 
    pull(var = case_avg) %>% 
    max(),
  peak_x = -60, 
  peak_y = 70, 
  current_x = -1, 
  current_y = -380
)

## tables
stl_values[["peak_tbl"]] <- region_data %>%
  filter(region == "St. Louis") %>%
  filter(case_avg == stl_values$peak_val) %>%
  mutate(text = paste0("peak average of ", round(case_avg, digits = 2), " cases reported on ", format(report_date, format = "%d %b")))

stl_values[["current_tbl"]] <- region_data %>%
  filter(region == "St. Louis") %>%
  filter(report_date == date) %>%
  mutate(text = paste0("current average of ", round(case_avg, digits = 2), " cases reported on ", format(report_date, format = "%d %b")))

# define plotting values, kansas city trend ####
## primary values
kc_values <- list(
  peak_val = region_data %>% 
    filter(region == "Kansas City") %>% 
    pull(var = case_avg) %>% 
    max(),
  peak_x = -70, 
  peak_y = 170, 
  current_x = -1, 
  current_y = -200
)

## tables
kc_values[["peak_tbl"]]  <- region_data %>%
  filter(region == "Kansas City") %>%
  filter(case_avg == kc_values$peak_val) %>%
  mutate(text = paste0("peak average of ", round(case_avg, digits = 2), " cases reported on ", format(report_date, format = "%d %b")))

kc_values[["current_tbl"]] <- region_data %>%
  filter(region == "Kansas City") %>%
  filter(report_date == date) %>%
  mutate(text = paste0("current average of ", round(case_avg, digits = 2), " cases reported on ", format(report_date, format = "%d %b")))

# define plotting values, outstate trend ####
## primary values
os_values <- list(
  peak_val = region_data %>% 
    filter(region == "Outstate") %>% 
    pull(var = case_avg) %>% 
    max(),
  peak_x = -90, 
  peak_y = 90, 
  current_x = -1, 
  current_y = -760
)

## tables
os_values[["peak_tbl"]] <- region_data %>%
  filter(region == "Outstate") %>%
  filter(case_avg == os_values$peak_val) %>%
  mutate(text = paste0("peak average of ", round(case_avg, digits = 2), " cases reported on ", format(report_date, format = "%d %b")))

os_values[["current_tbl"]] <- region_data %>%
  filter(region == "Outstate") %>%
  filter(report_date == date) %>%
  mutate(text = paste0("current average of ", round(case_avg, digits = 2), " cases reported on ", format(report_date, format = "%d %b")))
