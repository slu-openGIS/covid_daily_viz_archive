
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

covid_totals %>%
  mutate(initiated_pct = initiated/sum(initiated)) %>%
  mutate(completed_pct = completed/sum(completed)) %>%
  select(category, initiated_pct, completed_pct) %>%
  pivot_longer(cols = c("initiated_pct", "completed_pct"), 
               names_to = "value", values_to = "pct") %>%
  mutate(value = ifelse(value == "initiated_pct", "Initiated", "Completed")) %>%
  mutate(label = paste0(as.character(round(pct*100, digits = 0)), "%")) -> covid_totals_pct

covid_totals %>% 
  pivot_longer(cols = c("initiated", "completed"), names_to = "value", values_to = "count") %>%
  mutate(value = ifelse(value == "initiated", "Initiated", "Completed")) -> covid_totals_long

initiated_label <- paste0("Initiated Vaccinations (n = ", formatC(sum(covid_totals$initiated), format="d", big.mark=","), ")")
completed_label <- paste0("Completed Vaccinations (n = ", formatC(sum(covid_totals$completed), format="d", big.mark=","), ")")

left_join(covid_totals_long, covid_totals_pct, by = c("category", "value")) %>%
  mutate(category = fct_relevel(category,"Unknown or Out-of-state Jurisdiction", "Missouri, Unknown Jurisdiction", "Missouri, Known Jurisdiction")) %>%
  mutate(value = ifelse(value == "Initiated", initiated_label, completed_label)) %>%
  mutate(value = fct_relevel(value, initiated_label, completed_label)) -> covid_totals_long



p <- covid_totals_long %>%
  mutate(count_2 = round_any(count/10000, accuracy = 1, f = ceiling)) %>%
  ggplot(., aes(fill=category, values=count_2)) +
  geom_waffle(color = "white", size=.75, n_rows = 6) +
  scale_fill_manual(values = c(brewer.pal(n = 5, name = "Reds")[2],
                               brewer.pal(n = 5, name = "Purples")[2], 
                               brewer.pal(n = 5, name = "Purples")[5]), 
                    name = "Geography") +
  facet_wrap(~value, ncol=1)  +
  scale_x_discrete(expand=c(0,0)) +
  scale_y_discrete(expand=c(0,0)) +
  coord_equal() +
  labs(
    title = "Geographic Breakdown of Missouri Vaccination Counts",
    subtitle = paste0("Current as of ", as.character(date),"\nEach box is equivalent to 10,000 individuals"),
    caption = "Plot by Christopher Prener, Ph.D.\nData via the State of Missouri\nThis plot does not capture Missouri residents who recieve vaccinations in another state or who recieve vaccinations at a Federal\n    facility such as a Veterans Administration hospital or clinic.\nCounts are rounded up to the nearest 10,000 individuals to generate boxes for this plot.\nMissouri, Unknown jurisdiction refers to partially complete addresses where Missouri was listed for state.\nUnknown jurisdiction refers to vaccinations where no address was provided."
  ) +
  sequoia_theme(base_size = 22, background = "white", legend_size = 1) +
  theme(
    legend.position = "bottom",
    legend.justification = "left"
  ) +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE, reverse = TRUE))

save_plots(filename = "results/high_res/state/s_vaccine_geography.png", plot = p, preset = "lg")
save_plots(filename = "results/low_res/state/s_vaccine_geography.png", plot = p, preset = "lg", dpi = 72)

# =============================================================================

rm(p, covid_totals, covid_totals_long, covid_totals_pct, initiated_label, completed_label)
