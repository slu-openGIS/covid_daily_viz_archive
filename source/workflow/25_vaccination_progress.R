
# load data
pop <- read_csv("data/MO_HEALTH_Covid_Tracking/data/source/state_pop.csv")

# =============================================================================

## tidy values
complete <- covid_totals %>%
  filter(category %in% c("Unknown or Out-of-state Jurisdiction") == FALSE) %>%
  group_by(report_date) %>%
  summarise(total = sum(completed)) %>%
  select(total) %>%
  pull()

initiated <- covid_totals %>%
  filter(category %in% c("Unknown or Out-of-state Jurisdiction") == FALSE) %>%
  group_by(report_date) %>%
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
  pct = c(initiated/pop, 1-(initiated/pop),
          complete/pop, 1-(complete/pop))
)

pct <- mutate(pct, subvalue = fct_relevel(subvalue, "Remaining", "Complete"))
pct <- mutate(pct, value = str_wrap(value, width = 12))

# clean-up
rm(complete, initiated, pop)

# =============================================================================

pct <- mutate(pct, label = paste0(as.character(round(pct*100, digits = 0)), "%")) %>%
  mutate(label = ifelse(subvalue == "Remaining", NA, label))

# =============================================================================

p <- ggplot(pct, mapping = aes(y = value, x = pct, fill = subvalue)) +
  geom_col(show.legend = FALSE) + # position = "fill", stat = "identity", 
  geom_text(mapping = aes(label = label), hjust = 1.25, size = 8, colour = "white") +
  scale_x_continuous(breaks = seq(0,1, by = .2), labels=scales::percent) +
  scale_fill_manual(values = c("#ddc0e1", "#984ea3"), name = "Progress") +
  labs(
    title = "Vaccination Progress in Missouri",
    subtitle = paste0("Current as of ", as.character(date)),
    x = "",
    y = "",
    caption = "Plot by Christopher Prener, Ph.D.\nData via the State of Missouri\nThis plot represents the most conservative view of vaccination progress by including only those\n    with known Missouri addresses."
  ) +
  sequoia_theme(base_size = 22, background = "white")

ggsave(filename = "results/low_res/state/r_vaccine_compare.png", plot = p,
       width = 1024 * 0.352778, height = 384 * 0.352778, units = "mm", dpi = 72)

# =============================================================================

rm(pct)
rm(p)

# =============================================================================



