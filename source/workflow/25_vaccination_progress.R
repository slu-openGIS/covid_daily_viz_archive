
# load data
initiated <-  read_csv("data/MO_HEALTH_Covid_Tracking/data/individual/mo_vaccine_race_rates.csv")
pop <- read_csv("data/MO_HEALTH_Covid_Tracking/data/source/state_pop.csv")

# =============================================================================

## tidy values
complete <- 600496

initiated <- initiated %>%
  filter(value %in% c("Unknown Ethnicity", "Latino") == FALSE) %>%
  group_by(geoid) %>%
  summarise(total = sum(initiated)) %>%
  select(total) %>%
  pull()

pop <- pop %>%
  filter(NAME == "Missouri") %>%
  select(total_pop) %>%
  pull()

## calculate percentages
pct <- tibble(
  value = c("Vaccination Initiated", "Vaccination Initiated", "Vaccination Complete", "Vaccination Complete"),
  subvalue = c("Complete", "Remaining", "Complete", "Remaining"),
  pct = c(initiated/pop*100, 100-(initiated/pop*100),
          complete/pop*100, 100-(complete/pop*100))
)

pct <- mutate(pct, subvalue = fct_relevel(subvalue, "Remaining", "Complete"))
pct <- mutate(pct, value = str_wrap(value, width = 12))

# clean-up
rm(complete, initiated, pop)

# =============================================================================

ggplot(pct, aes(y = value, x = pct, fill = subvalue)) +
  geom_bar(position = "fill", stat = "identity") +
  scale_x_continuous(breaks = seq(0,1, by = .2), labels=scales::percent) +
  scale_fill_manual(values = c("#ddc0e1", "#984ea3"), name = "Progress") +
  labs(
    title = "Vaccination Progress in Missouri",
    x = "",
    y = ""
  )

