# plot regional data

# =============================================================================

# define values
## plot values
report_label_x <- 12
report_label_y <- 50
peak_point_x <- -1
peak_point_y <- 25
current_point_x <- -1
current_point_y <- 125


## data values
stl <- c("29071", "29099", "29113", "29183", "29189", "29219", "29510")
kc <- c("29013", "29025", "29037", "29047", "29049", "29095", "29107", 
        "29165", "29177", "29511")
kc_stl <- c(kc, stl)   

# =============================================================================
# =============================================================================

# load data

state_data <- read_csv("data/state/state_full.csv") 

focal_data <- read_csv("data/county/county_full.csv") %>%
  mutate(geoid = as.character(geoid)) %>%
  filter(state == "Missouri") %>%
  filter(geoid %in% stl == TRUE)

# =============================================================================

# define colors

# =============================================================================

# create points
## create end points
state_points <- filter(state_data, report_date == date)

## create reporting change points
report_points <- filter(state_data, report_date == as.Date("2020-04-15"))

# =============================================================================

# create days from first day where average confirmed infections were at 
# least 10, state-level data

## subset data
state_data %>%
  calculate_days(group_var = "state", stat_var = "case_avg", val = 5) %>%
  select(day, report_date, state, case_avg) %>%
  arrange(state, day) %>%
  filter(state == "Missouri") -> state_subset

## set start date
start_date <- state_subset$report_date[1]

## define top_val
top_val <- round_any(x = max(state_subset$day), accuracy = 5, f = ceiling)

## extra points
peak_val <- max(state_subset$case_avg)
peak_point <- filter(state_subset, case_avg == peak_val) %>%
  mutate(text = paste0("peak average of ", round(case_avg, digits = 2), " cases reported on ", format(report_date, format = "%d %b")))

current_point <- filter(state_subset, report_date == date) %>%
  mutate(text = paste0("current average of ", round(case_avg, digits = 2), " cases reported on ", format(report_date, format = "%d %b")))

## missouri less stl trend
focal_data %>%
  group_by(report_date) %>%
  summarise(new_cases = sum(new_cases)) %>%
  mutate(case_avg = rollmean(new_cases, k = 7, align = "right", fill = NA)) %>%
  select(-new_cases) %>%
  filter(report_date >= start_date) %>%
  rowid_to_column(var = "day") %>%
  mutate(day = day-1) %>%
  mutate(state = "St. Louis") %>%
  mutate(factor_var = as.factor(NA_character_)) -> focal_subset

## extra points
peak_val_nostl <- max(focal_subset$case_avg)
peak_point_nostl <- filter(focal_subset, case_avg == peak_val_nostl) %>%
  mutate(text = paste0("peak average of ", round(case_avg, digits = 2), " cases reported on ", format(report_date, format = "%d %b")))

current_point_nostl <- filter(focal_subset, report_date == date) %>%
  mutate(text = paste0("current average of ", round(case_avg, digits = 2), " cases reported on ", format(report_date, format = "%d %b")))

## bind
state_subset <- bind_rows(state_subset, focal_subset)

## create state day points
state_day_points <- filter(state_subset, day == max(day))

## creat report day points
report_day_points <- filter(state_subset, report_date == as.Date("2020-04-15"))

report_label <- filter(report_day_points, state == "Missouri") %>%
  mutate(text = ifelse(state == "Missouri", "reporting change on 15 Apr", NA))

## create factors
state_subset <- mutate(state_subset, factor_var = fct_reorder2(state, day, case_avg))
state_day_points <- mutate(state_day_points, factor_var = fct_reorder2(state, day, case_avg))

# =============================================================================

## create linear plot
p <- ggplot() +
  geom_line(state_subset, mapping = aes(x = report_date, y = case_avg, color = factor_var), size = 2) +
  geom_point(state_day_points, mapping = aes(x = report_date, y = case_avg, color = factor_var), 
             size = 4, show.legend = FALSE) +
  geom_point(report_day_points, mapping = aes(x = report_date, y = case_avg), size = 4, shape = 18) +
  geom_point(peak_point, mapping = aes(x = report_date, y = case_avg), size = 4, shape = 16) +
  geom_point(peak_point_nostl, mapping = aes(x = report_date, y = case_avg), size = 4, shape = 16) +
  geom_text_repel(data = report_label, mapping = aes(x = report_date, y = case_avg, label = text),
                  nudge_y = report_label_y, nudge_x = report_label_x, size = 5) +
  geom_text_repel(data = peak_point, mapping = aes(x = report_date, y = case_avg, label = text),
                  nudge_y = peak_point_y, nudge_x = peak_point_x, size = 5) +
  geom_text_repel(data = current_point, mapping = aes(x = report_date, y = case_avg, label = text),
                  nudge_y = current_point_y, nudge_x = current_point_x, size = 5) +
  geom_text_repel(data = peak_point_nostl, mapping = aes(x = report_date, y = case_avg, label = text),
                  nudge_y = -125, nudge_x = 15, size = 5) +
  geom_text_repel(data = current_point_nostl, mapping = aes(x = report_date, y = case_avg, label = text),
                  nudge_y = -70, nudge_x = -1, size = 5) +
  scale_color_brewer(palette = "Dark2", name = "Category") +
  scale_y_continuous(limits = c(0, 300), breaks = seq(0, 300, by = 50)) +
  scale_x_date(date_breaks = date_breaks_alt, date_labels = "%d %b") +
  labs(
    title = "Pace of New COVID-19 Cases in Missouri",
    subtitle = paste0("St. Louis Metro Focus\n", as.character(state_subset$report_date[1]), " through ", as.character(date)),
    caption = caption_text,
    x = "Date",
    y = "7-day Average of New Cases"
  ) +
  sequoia_theme(base_size = 22, background = "white") +
  theme(axis.text.x = element_text(angle = x_angle))

## save plot
save_plots(filename = "results/high_res/regional/b_avg_stl.png", plot = p, preset = "lg")
save_plots(filename = "results/low_res/regional/b_avg_stl.png", plot = p, preset = "lg", dpi = 72)

# =============================================================================

## clean-up
rm(peak_val, peak_point, current_point, peak_val_nostl, peak_point_nostl, current_point_nostl,
   focal_data, focal_subset)
rm(state_data, state_subset, state_points, state_day_points)
rm(top_val, p, report_points, report_label, report_day_points)

# =============================================================================
# =============================================================================

# load data

state_data <- read_csv("data/state/state_full.csv") 

focal_data <- read_csv("data/county/county_full.csv") %>%
  mutate(geoid = as.character(geoid)) %>%
  filter(state == "Missouri") %>%
  filter(geoid %in% kc == TRUE)

# =============================================================================

# define colors

# =============================================================================

# create points
## create end points
state_points <- filter(state_data, report_date == date)

## create reporting change points
report_points <- filter(state_data, report_date == as.Date("2020-04-15"))

# =============================================================================

# create days from first day where average confirmed infections were at 
# least 10, state-level data

## subset data
state_data %>%
  calculate_days(group_var = "state", stat_var = "case_avg", val = 5) %>%
  select(day, report_date, state, case_avg) %>%
  arrange(state, day) %>%
  filter(state == "Missouri") -> state_subset

## set start date
start_date <- state_subset$report_date[1]

## define top_val
top_val <- round_any(x = max(state_subset$day), accuracy = 5, f = ceiling)

## extra points
peak_val <- max(state_subset$case_avg)
peak_point <- filter(state_subset, case_avg == peak_val) %>%
  mutate(text = paste0("peak average of ", round(case_avg, digits = 2), " cases reported on ", format(report_date, format = "%d %b")))

current_point <- filter(state_subset, report_date == date) %>%
  mutate(text = paste0("current average of ", round(case_avg, digits = 2), " cases reported on ", format(report_date, format = "%d %b")))

## missouri less stl trend
focal_data %>%
  group_by(report_date) %>%
  summarise(new_cases = sum(new_cases)) %>%
  mutate(case_avg = rollmean(new_cases, k = 7, align = "right", fill = NA)) %>%
  select(-new_cases) %>%
  filter(report_date >= start_date) %>%
  rowid_to_column(var = "day") %>%
  mutate(day = day-1) %>%
  mutate(state = "Kansas City") %>%
  mutate(factor_var = as.factor(NA_character_)) -> focal_subset

## extra points
peak_val_nostl <- max(focal_subset$case_avg)
peak_point_nostl <- filter(focal_subset, case_avg == peak_val_nostl) %>%
  mutate(text = paste0("peak average of ", round(case_avg, digits = 2), " cases reported on ", format(report_date, format = "%d %b")))

current_point_nostl <- filter(focal_subset, report_date == date) %>%
  mutate(text = paste0("current average of ", round(case_avg, digits = 2), " cases reported on ", format(report_date, format = "%d %b")))

## bind
state_subset <- bind_rows(state_subset, focal_subset)

## create state day points
state_day_points <- filter(state_subset, day == max(day))

## creat report day points
report_day_points <- filter(state_subset, report_date == as.Date("2020-04-15"))

report_label <- filter(report_day_points, state == "Missouri") %>%
  mutate(text = ifelse(state == "Missouri", "reporting change on 15 Apr", NA))

## create factors
state_subset <- mutate(state_subset, factor_var = fct_reorder2(state, day, case_avg))
state_day_points <- mutate(state_day_points, factor_var = fct_reorder2(state, day, case_avg))

# =============================================================================

## create linear plot
p <- ggplot() +
  geom_line(state_subset, mapping = aes(x = report_date, y = case_avg, color = factor_var), size = 2) +
  geom_point(state_day_points, mapping = aes(x = report_date, y = case_avg, color = factor_var), 
             size = 4, show.legend = FALSE) +
  geom_point(report_day_points, mapping = aes(x = report_date, y = case_avg), size = 4, shape = 18) +
  geom_point(peak_point, mapping = aes(x = report_date, y = case_avg), size = 4, shape = 16) +
  geom_point(peak_point_nostl, mapping = aes(x = report_date, y = case_avg), size = 4, shape = 16) +
  geom_text_repel(data = report_label, mapping = aes(x = report_date, y = case_avg, label = text),
                  nudge_y = report_label_y, nudge_x = report_label_x, size = 5) +
  geom_text_repel(data = peak_point, mapping = aes(x = report_date, y = case_avg, label = text),
                  nudge_y = peak_point_y, nudge_x = peak_point_x, size = 5) +
  geom_text_repel(data = current_point, mapping = aes(x = report_date, y = case_avg, label = text),
                  nudge_y = current_point_y, nudge_x = current_point_x, size = 5) +
  geom_text_repel(data = peak_point_nostl, mapping = aes(x = report_date, y = case_avg, label = text),
                  nudge_y = 25, nudge_x = -1, size = 5) +
  geom_text_repel(data = current_point_nostl, mapping = aes(x = report_date, y = case_avg, label = text),
                  nudge_y = -55, nudge_x = -1, size = 5) +
  scale_color_brewer(palette = "Dark2", name = "Category") +
  scale_y_continuous(limits = c(0, 300), breaks = seq(0, 300, by = 50)) +
  scale_x_date(date_breaks = date_breaks_alt, date_labels = "%d %b") +
  labs(
    title = "Pace of New COVID-19 Cases in Missouri",
    subtitle = paste0("Kansas City Metro Focus\n", as.character(state_subset$report_date[1]), " through ", as.character(date)),
    caption = caption_text,
    x = "Date",
    y = "7-day Average of New Cases"
  ) +
  sequoia_theme(base_size = 22, background = "white") +
  theme(axis.text.x = element_text(angle = x_angle))

## save plot
save_plots(filename = "results/high_res/regional/c_avg_kc.png", plot = p, preset = "lg")
save_plots(filename = "results/low_res/regional/c_avg_kc.png", plot = p, preset = "lg", dpi = 72)

# =============================================================================

## clean-up
rm(peak_val, peak_point, current_point, peak_val_nostl, peak_point_nostl, current_point_nostl,
   focal_data, focal_subset)
rm(state_data, state_subset, state_points, state_day_points)
rm(top_val, p, report_points, report_label, report_day_points)

# =============================================================================
# =============================================================================

# load data

state_data <- read_csv("data/state/state_full.csv") 

focal_data <- read_csv("data/county/county_full.csv") %>%
  mutate(geoid = as.character(geoid)) %>%
  filter(state == "Missouri") %>%
  filter(geoid %in% kc_stl == FALSE)

# =============================================================================

# define colors

# =============================================================================

# create points
## create end points
state_points <- filter(state_data, report_date == date)

## create reporting change points
report_points <- filter(state_data, report_date == as.Date("2020-04-15"))

# =============================================================================

# create days from first day where average confirmed infections were at 
# least 10, state-level data

## subset data
state_data %>%
  calculate_days(group_var = "state", stat_var = "case_avg", val = 5) %>%
  select(day, report_date, state, case_avg) %>%
  arrange(state, day) %>%
  filter(state == "Missouri") -> state_subset

## set start date
start_date <- state_subset$report_date[1]

## define top_val
top_val <- round_any(x = max(state_subset$day), accuracy = 5, f = ceiling)

## extra points
peak_val <- max(state_subset$case_avg)
peak_point <- filter(state_subset, case_avg == peak_val) %>%
  mutate(text = paste0("peak average of ", round(case_avg, digits = 2), " cases reported on ", format(report_date, format = "%d %b")))

current_point <- filter(state_subset, report_date == date) %>%
  mutate(text = paste0("current average of ", round(case_avg, digits = 2), " cases reported on ", format(report_date, format = "%d %b")))

## missouri less stl trend
focal_data %>%
  group_by(report_date) %>%
  summarise(new_cases = sum(new_cases)) %>%
  mutate(case_avg = rollmean(new_cases, k = 7, align = "right", fill = NA)) %>%
  select(-new_cases) %>%
  filter(report_date >= start_date) %>%
  rowid_to_column(var = "day") %>%
  mutate(day = day-1) %>%
  mutate(state = "Outstate") %>%
  mutate(factor_var = as.factor(NA_character_)) -> focal_subset

## extra points
peak_val_nostl <- max(focal_subset$case_avg)
peak_point_nostl <- filter(focal_subset, case_avg == peak_val_nostl) %>%
  mutate(text = paste0("peak average of ", round(case_avg, digits = 2), " cases reported on ", format(report_date, format = "%d %b")))

current_point_nostl <- filter(focal_subset, report_date == date) %>%
  mutate(text = paste0("current average of ", round(case_avg, digits = 2), " cases reported on ", format(report_date, format = "%d %b")))

## bind
state_subset <- bind_rows(state_subset, focal_subset)

## create state day points
state_day_points <- filter(state_subset, day == max(day))

## creat report day points
report_day_points <- filter(state_subset, report_date == as.Date("2020-04-15"))

report_label <- filter(report_day_points, state == "Missouri") %>%
  mutate(text = ifelse(state == "Missouri", "reporting change on 15 Apr", NA))

## create factors
state_subset <- mutate(state_subset, factor_var = fct_reorder2(state, day, case_avg))
state_day_points <- mutate(state_day_points, factor_var = fct_reorder2(state, day, case_avg))

# =============================================================================

## create linear plot
p <- ggplot() +
  geom_line(state_subset, mapping = aes(x = report_date, y = case_avg, color = factor_var), size = 2) +
  geom_point(state_day_points, mapping = aes(x = report_date, y = case_avg, color = factor_var), 
             size = 4, show.legend = FALSE) +
  geom_point(report_day_points, mapping = aes(x = report_date, y = case_avg), size = 4, shape = 18) +
  geom_point(peak_point, mapping = aes(x = report_date, y = case_avg), size = 4, shape = 16) +
  geom_point(peak_point_nostl, mapping = aes(x = report_date, y = case_avg), size = 4, shape = 16) +
  geom_text_repel(data = report_label, mapping = aes(x = report_date, y = case_avg, label = text),
                  nudge_y = report_label_y, nudge_x = report_label_x, size = 5) +
  geom_text_repel(data = peak_point, mapping = aes(x = report_date, y = case_avg, label = text),
                  nudge_y = peak_point_y, nudge_x = peak_point_x, size = 5) +
  geom_text_repel(data = current_point, mapping = aes(x = report_date, y = case_avg, label = text),
                  nudge_y = current_point_y, nudge_x = current_point_x, size = 5) +
  geom_text_repel(data = peak_point_nostl, mapping = aes(x = report_date, y = case_avg, label = text),
                  nudge_y = 25, nudge_x = -1, size = 5) +
  geom_text_repel(data = current_point_nostl, mapping = aes(x = report_date, y = case_avg, label = text),
                  nudge_y = -40, nudge_x = -1, size = 5) +
  scale_color_brewer(palette = "Dark2", name = "Category") +
  scale_y_continuous(limits = c(0, 300), breaks = seq(0, 300, by = 50)) +
  scale_x_date(date_breaks = date_breaks_alt, date_labels = "%d %b") +
  labs(
    title = "Pace of New COVID-19 Cases in Missouri",
    subtitle = paste0("Outstate Focus\n", as.character(state_subset$report_date[1]), " through ", as.character(date)),
    caption = caption_text,
    x = "Date",
    y = "7-day Average of New Cases"
  ) +
  sequoia_theme(base_size = 22, background = "white") +
  theme(axis.text.x = element_text(angle = x_angle))

## save plot
save_plots(filename = "results/high_res/regional/d_avg_outstate.png", plot = p, preset = "lg")
save_plots(filename = "results/low_res/regional/d_avg_outstate.png", plot = p, preset = "lg", dpi = 72)

# =============================================================================

## clean-up
rm(peak_val, peak_point, current_point, peak_val_nostl, peak_point_nostl, current_point_nostl,
   focal_data, focal_subset)
rm(state_data, state_subset, state_points, state_day_points)
rm(top_val, p, report_points, report_label, report_day_points)

# =============================================================================
# =============================================================================

# load data
regional_data <- read_csv("data/county/county_full.csv") %>%
  mutate(geoid = as.character(geoid)) %>%
  filter(state == "Missouri")

# =============================================================================

## define data cleaning function
grouped_prep <- function(.data, date){
  
  out <- mutate(.data, case_avg = rollmean(new_cases, k = 7, align = "right", fill = NA))
  out <- filter(out, report_date >= start_date)
  out <- select(out, -new_cases)
  out <- rowid_to_column(out, var = "day")
  out <- mutate(out, day = day-1)
  
  return(out)
  
}

## prepare data
regional_data %>%
  mutate(region = case_when(
    geoid %in% stl == TRUE ~ "St. Louis",
    geoid %in% kc == TRUE ~ "Kansas City",
    geoid %in% kc_stl == FALSE ~ "Outstate"
  )) %>%
  group_by(report_date, region) %>%
  summarise(new_cases = sum(new_cases)) %>%
  ungroup() %>%
  group_split(region) %>%
  map_df(~grouped_prep(.x, date = start_date)) %>%
  arrange(report_date, region) %>%
  mutate(region = as_factor(region)) %>%
  mutate(region = fct_relevel(region, "St. Louis", "Kansas City", "Outstate")) -> regional_subset

## define top_val
top_val <- round_any(x = max(regional_subset$day), accuracy = 5, f = ceiling)

# =============================================================================

## construct plot
p <- ggplot() +
  geom_area(regional_subset, mapping = aes(x = report_date, y = case_avg, fill = region),
            show.legend = FALSE) +
  scale_fill_brewer(palette = "Set1") +
  facet_wrap(vars(region), nrow = 3) +
  scale_x_date(date_breaks = date_breaks_alt, date_labels = "%d %b") +
  labs(
    title = "Pace of New COVID-19 Cases in Missouri by Region",
    subtitle = paste0(as.character(regional_subset$report_date[1]), " through ", as.character(date)),
    caption = caption_text,
    x = "Days Since Average of Five Cases Reported",
    y = "7-day Average of New Cases"
  ) +
  sequoia_theme(base_size = 22, background = "white") +
  theme(axis.text.x = element_text(angle = x_angle))

## save plot
save_plots(filename = "results/high_res/regional/a_avg_all.png", plot = p, preset = "lg")
save_plots(filename = "results/low_res/regional/a_avg_all.png", plot = p, preset = "lg", dpi = 72)


# =============================================================================

# clean-up
rm(grouped_prep)
rm(regional_data, regional_subset)
rm(top_val, p)

# =============================================================================

## clean-up
rm(kc, stl, kc_stl, report_label_x, report_label_y, peak_point_x,
   peak_point_y, current_point_x, current_point_y)
