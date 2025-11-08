## Question 2 is "How does the proportion of overlap (ideological overlap between parties) in Congress change around periods of large shifts in primary dynamics (e.g. big wave years)?"

## For this, we're attempting to connect 2 things over time so we can use primary dynasim 

# Combine party-year data
all_cands <- bind_rows(
  rep_2018 %>% mutate(party = "Republican", year = 2018),
  rep_2022 %>% mutate(party = "Republican", year = 2022),
  dem_2018 %>% mutate(party = "Democrat", year = 2018),
  dem_2022 %>% mutate(party = "Democrat", year = 2022)
)

# Example: compute basic yearly metrics
primary_metrics <- all_cands %>%
  group_by(year, party) %>%
  summarise(
    total = n(),
    incumbents = sum(incumbent == "Yes", na.rm = TRUE),
    incumbents_defeated = sum(primary_outcome == "Lost" & incumbent == "Yes", na.rm = TRUE),
    winners = sum(primary_outcome == "Won", na.rm = TRUE),
    new_entrants = sum(incumbent == "No" & primary_outcome == "Won", na.rm = TRUE)
  ) %>%
  mutate(
    prop_incumbents_defeated = incumbents_defeated / incumbents,
    share_new_entrants = new_entrants / winners
  )


## creating logistic graphs
model_all <- glm(primary_win ~ party + gender + race + ideology + incumbent,
                 data = all_candidates, family = "binomial")

summary(model_all)
