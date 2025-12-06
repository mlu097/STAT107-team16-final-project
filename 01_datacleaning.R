library(tidyverse)
library(janitor)

# load ideology data
pol <- read_csv("00_Political Polarization in US Congress (Assorted).csv") %>% clean_names()

# load candidate data
rep_18 <- read_csv("00_rep_candidates_2018.csv") %>% clean_names()
rep_22 <- read_csv("00_rep_candidates_2022.csv") %>% clean_names()
dem_18 <- read_csv("00_dem_candidates_2018.csv") %>% clean_names()
dem_22 <- read_csv("00_dem_candidates_2022.csv") %>% clean_names()

# rename office column for 2018
rep_18 <- rep_18 %>% rename(office = office_type)
dem_18 <- dem_18 %>% rename(office = office_type)

# add missing gender for 2018
rep_18$gender <- NA
dem_18$gender <- NA

# merge race columns in 2022
rep_22$race <- coalesce(rep_22$race_1, rep_22$race_2, rep_22$race_3)
dem_22$race <- coalesce(dem_22$race_1, dem_22$race_2, dem_22$race_3)

# drop old race columns
rep_22 <- rep_22 %>% select(-race_1, -race_2, -race_3)
dem_22 <- dem_22 %>% select(-race_1, -race_2, -race_3)

# clean primary outcomes
rep_18$primary_outcome <- ifelse(rep_18$won_primary == "Won", 1, 0)
dem_18$primary_outcome <- ifelse(dem_18$won_primary == "Won", 1, 0)
rep_22$primary_outcome <- ifelse(rep_22$primary_outcome == "Won", 1, 0)
dem_22$primary_outcome <- ifelse(dem_22$primary_outcome == "Won", 1, 0)

# clean percent values
clean_percent <- function(x){ as.numeric(gsub("[^0-9.]", "", x)) }
rep_18$primary_percent <- clean_percent(rep_18$primary_percent)
rep_22$primary_percent <- clean_percent(rep_22$primary_percent)
dem_18$primary_percent <- clean_percent(dem_18$primary_percent)
dem_22$primary_percent <- clean_percent(dem_22$primary_percent)

# standardize office labels
clean_office <- function(x){
  case_when(
    str_detect(tolower(x), "house") ~ "House",
    str_detect(tolower(x), "sen") ~ "Senate",
    str_detect(tolower(x), "gov") ~ "Governor",
    TRUE ~ NA_character_
  )
}

# apply office cleaner
rep_18$office <- clean_office(rep_18$office)
rep_22$office <- clean_office(rep_22$office)
dem_18$office <- clean_office(dem_18$office)
dem_22$office <- clean_office(dem_22$office)

# drop unusable office rows
rep_18 <- rep_18 %>% filter(!is.na(office))
rep_22 <- rep_22 %>% filter(!is.na(office))
dem_18 <- dem_18 %>% filter(!is.na(office))
dem_22 <- dem_22 %>% filter(!is.na(office))

# clean race categories
clean_race <- function(x){
  case_when(
    str_detect(tolower(x), "white") ~ "White",
    str_detect(tolower(x), "black") ~ "Black",
    str_detect(tolower(x), "latino|hispanic") ~ "Latino",
    str_detect(tolower(x), "asian") ~ "Asian",
    TRUE ~ "Other"
  )
}

# apply race cleaner

rep_22$race <- clean_race(rep_22$race)
dem_18$race <- clean_race(dem_18$race)
dem_22$race <- clean_race(dem_22$race)

# add party + year
rep_18 <- rep_18 %>% mutate(party="Republican", year=2018)
rep_22 <- rep_22 %>% mutate(party="Republican", year=2022)
dem_18 <- dem_18 %>% mutate(party="Democrat", year=2018)
dem_22 <- dem_22 %>% mutate(party="Democrat", year=2022)

# columns needed for Q1
q1_keep <- c("candidate","state","district","office","gender",
             "race","primary_percent","primary_outcome",
             "incumbent","party","year")

# select only 2022 for Q1
rep_22_q1 <- rep_22 %>% select(any_of(q1_keep))
dem_22_q1 <- dem_22 %>% select(any_of(q1_keep))

# combine for Q1 dataset
candidates_q1 <- bind_rows(rep_22_q1, dem_22_q1)

# drop sparse columns
drop_sparse <- function(df, threshold = 0.95){
  df[, colMeans(is.na(df)) < threshold]
}

candidates_q1 <- drop_sparse(candidates_q1)

## DATA CLEANED FOR QUESTION 1

# clean ideology dataset for Q2
pol_q2 <- pol %>% select(year, chamber, overlap) %>% filter(!is.na(overlap))

save(candidates_q1, pol_q2, file = "cleaned_data.RData")
write_csv(candidates_q1, "candidates_q1_clean.csv")

cat("Data cleaning complete\n")
cat("Q1 dataset:", nrow(candidates_q1), "candidates from 2022\n")
cat("Q2 dataset:", nrow(pol_q2), "ideology observations\n")

