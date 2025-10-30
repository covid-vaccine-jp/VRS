# VRS2b (estimate incidence with popEpi)
#
# 1. ---- Initial settings ----
## Municipality-dependent settings
municipality_id <- "JP"
municipality_name <- "Japan"
subdir <- paste0("")
study_start_date <- as.Date("2021-02-01")
study_end_date <- as.Date("2024-03-31")

age20_49 <- c(
  "20～24歳", "25～29歳", "30～34歳", "35～39歳", "40～44歳", "45～49歳",
  "2001～2005", "1996～2000", "1991～1995",
  "1986～1990", "1981～1985", "1976～1980"
)
age50_64 <- c(
  "50～54歳", "55～59歳", "60～64歳","1971～1975","1966～1970","1961～1965"
)
age65_89 <- c(
  "65～69歳", "70～74歳", "75～79歳", "80～84歳", "85～89歳",
  "1956～1960", "1951～1955", "1946～1950",
  "1941～1945", "1936～1940"
)
age80_ <- c(
  "80～84歳", "85～89歳", "90～94歳", "95～100歳", "100歳～",
  "1941～1945", "1936～1940", "1931～1935", "1926～1930",
  "1921～1925", "1916～1920", "1911～1915"
)

## Common settings
library(tidyverse)

phase <- "1"
# "0" exploratory
# "1" release

# result_name
# "1" by age group (10) / vaccination status
# "2" by age group (4) / vaccination status
# "3" by age group (4) / quarter / vaccination status
# "4" by age group (4) / month / vaccination status

if (phase == "1") {
  age4 <- c(20, 50, 65, 90)
  age10 <- c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, Inf)
} else {
  age10 <- c(0, 10, 20, 30, 40, 50, 60, 65, 70, 75, 80, 85, 90, 95, Inf)
}

name <- list(
  subdir = subdir,
  municipality_id = municipality_id,
  municipality_name = municipality_name
)

table_file_name <- function(table_name, name) {
  paste0(name$subdir, name$municipality_id, "_wrapper_", table_name, "_", name$municipality_name, ".csv")
}

rate_result_file_name <- function(result_name, name) {
  paste0(name$subdir, name$municipality_id, "_VRS2b_rate_", result_name, "_", name$municipality_name, ".csv")
}

# sir_result_file_name <- function(name) {
#   paste0(name$subdir, name$municipality_id, "_VRS2b_sir_", name$municipality_name, ".csv")
# }

# # 2. ---- Generate base mortality table ----
# ref_group <- c(
#   "0～4歳", "5～9歳", "10～14歳", "15～19歳", "20～24歳",
#   "25～29歳", "30～34歳", "35～39歳", "40～44歳", "45～49歳",
#   "50～54歳", "55～59歳", "60～64歳", "65～69歳", "70～74歳",
#   "75～79歳", "80～84歳", "85～89歳", "90～94歳", "95～99歳",
#   "100歳～"
# )
# ref_haz <- c(
#   0.000496, 0.000075, 0.00008, 0.000206, 0.000342,
#   0.000351, 0.00046, 0.000611, 0.000915, 0.001454,
#   0.002319, 0.003601, 0.005655, 0.009184, 0.013644,
#   0.022706, 0.041866, 0.077439, 0.139659, 0.244094,
#   0.421493
# )
# ref_df <- data.frame(ref_group, ref_haz) |>
#   rename(
#     group = ref_group,
#     haz = ref_haz
#   )

# 3. ---- Read data ----
personTable <- read_csv(table_file_name("personTable", name), col_types = cols(
  person_id = col_integer(),
  gender_concept_id = col_integer(),
  year_of_birth = col_integer(),
  month_of_birth = col_integer(),
  day_of_birth = col_integer(),
  birth_date = col_date(),
  group = col_character()
))

targetCohortTable <- read_csv(table_file_name("targetCohortTable", name), col_types = cols(
  cohort_definition_id = col_integer(),
  subject_id = col_integer(),
  cohort_start_date = col_date(),
  cohort_end_date = col_date(),
  lot_number = col_character(),
  provider_name = col_character(),
  death_date = col_date()
))

# 4. ---- Generate base tables ----
gender_map <- c("8507" = "M", "8532" = "F")

df <- targetCohortTable |>
  replace_na(list(lot_number = "unvaccinated")) |>
  filter(cohort_start_date <= study_end_date) |>
  mutate(cohort_end_date = if_else(
    cohort_end_date > study_end_date,
    study_end_date, cohort_end_date
  )) |>
  left_join(personTable, by = c("subject_id" = "person_id")) |>
  mutate(
    sex = if_else(gender_concept_id == 8507, "M", "F"),
    entry_status = "0",
    exit_status = factor((!is.na(death_date)) & (death_date <= cohort_end_date),
                         labels = c("0", "D")
    )
  ) |>
  select(
    subject_id,
    sex,
    group,
    birth = birth_date,
    death_date,
    entry = cohort_start_date,
    exit = cohort_end_date,
    exit_status,
    cohort_definition_id,
    lot_number,
    provider_name
  )

# 5. ---- Estimate incidence ----
library(popEpi)

## result 1
coh_df <- df |>
  lexpand(
    birth = birth, entry = entry, exit = exit, status = exit_status,
    breaks = list(age = age10),
    aggre = list(ageGroup = age, cohort_definition_id), aggre.type = "unique"
  )
rate_df <- rate(coh_df, obs = from0toD, pyrs = pyrs, print = c("ageGroup", "cohort_definition_id"))
write_csv(rate_df, rate_result_file_name("1", name), na = "0")

## result 2
coh_df <- df |>
  lexpand(
    birth = birth, entry = entry, exit = exit, status = exit_status,
    breaks = list(age = age4),
    aggre = list(ageGroup = age, cohort_definition_id), aggre.type = "unique"
  )
rate_df <- rate(coh_df, obs = from0toD, pyrs = pyrs, print = c("ageGroup", "cohort_definition_id"))
write_csv(rate_df, rate_result_file_name("2", name), na = "0")

## result 3
coh_df <- df |>
  lexpand(
    birth = birth, entry = entry, exit = exit, status = exit_status,
    breaks = list(age = age4, per = seq(2021, 2024.25, by = 1 / 4)),
    aggre = list(ageGroup = age, interval = per, cohort_definition_id), aggre.type = "unique"
  )
rate_df <- rate(coh_df, obs = from0toD, pyrs = pyrs, print = c("ageGroup", "interval", "cohort_definition_id"))
write_csv(rate_df, rate_result_file_name("3", name), na = "0")

# エラー: cannot allocate vector of size 425.9 Mb
# ## result 4
# coh_df <- df |>
#   lexpand(
#     birth = birth, entry = entry, exit = exit, status = exit_status,
#     breaks = list(age = age4, per = seq(2021, 2024.25, by = 1 / 12)),
#     aggre = list(ageGroup = age, interval = per, cohort_definition_id), aggre.type = "unique"
#   )
# rate_df <- rate(coh_df, obs = from0toD, pyrs = pyrs, print = c("ageGroup", "interval", "cohort_definition_id"))
# write_csv(rate_df, rate_result_file_name("4", name), na = "0")
