# --- Load libraries ---
library(tidyverse)
library(ggplot2)

# --- Read in all candidate files ---

# --- Combine them into one dataset ---
candidates <- bind_rows(dem_2018, rep_2018, dem_2022, rep_2022)

# --- Optional: print summary ---
glimpse(candidates)


# Example for Democrats 2022
ggplot(dem_2022, aes(x = primary_percent)) +
  geom_histogram(
    bins = 30,
    fill = "#2E86AB",
    color = "white"
  ) +
  labs(
    title = "Distribution of Primary Vote Percentages (Democrats 2022)",
    x = "Primary Percent",
    y = "Number of Candidates"
  ) +
  theme_minimal()

##dem 2018
ggplot(dem_2018, aes(x = primary_percent)) +
  geom_histogram(
    bins = 30,
    fill = "#2E86AB",
    color = "white"
  ) +
  labs(
    title = "Distribution of Primary Vote Percentages (Democrats 2018)",
    x = "Primary Percent",
    y = "Number of Candidates"
  ) +
  theme_minimal()

## line plot 

# Compute average primary percentage by year
avg_primary_by_year <- candidates %>%
  group_by(year) %>%
  summarize(
    avg_primary = mean(primary_percent, na.rm = TRUE),
    count = n()
  )

# Visualize
ggplot(avg_primary_by_year, aes(x = factor(year), y = avg_primary, group = 1)) +
  geom_line(color = "steelblue", size = 1.2) +
  geom_point(size = 3, color = "darkblue") +
  labs(
    title = "Average Primary Vote Percentage by Year",
    x = "Election Year",
    y = "Average Primary %"
  ) +
  theme_minimal()

 ### rep 2018
  

ggplot(rep_2018, aes(x = primary_percent)) +
  geom_histogram(
    bins = 30,
    fill = "#E74C3C",
    color = "white"
  ) +
  labs(
    title = "Distribution of Primary Vote Percentages (Republicans 2018)",
    x = "Primary Percent",
    y = "Number of Candidates"
  ) +
  theme_minimal()

#rep 2022
ggplot(rep_2022, aes(x = primary_percent)) +
  geom_histogram(
    bins = 30,
    fill = "#C0392B",
    color = "white"
  ) +
  labs(
    title = "Distribution of Primary Vote Percentages (Republicans 2022)",
    x = "Primary Percent",
    y = "Number of Candidates"
  ) +
  theme_minimal()
