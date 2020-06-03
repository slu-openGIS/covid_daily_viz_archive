library(tidyr)

state_subset %>%
  select(-factor_var) %>%
  mutate(state = ifelse(state == "Missouri (No STL)", "Missouri_nonSTL", state)) %>%
  pivot_wider(names_from = "state", values_from = "case_avg") %>%
  mutate(ratio = Missouri/Missouri_nonSTL) -> state_subset2
