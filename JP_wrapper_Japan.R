# wrapper (generate wrapper data)
#
# 1. ---- 初期設定 ----
library(tidyverse)

municipality_id <- "JP"
municipality_name <- "Japan"
subdir <- paste0("")
study_start_date <- as.Date("2021-02-01")
study_end_date <- as.Date("2024-03-31")

municipality_id_1 <- "221309"
municipality_name_1 <- "Hamamatsu"
subdir_1 <- paste0(municipality_id_1, "_", municipality_name_1, "/")
municipality_id_2 <- "122076"
municipality_name_2 <- "Matsudo"
subdir_2 <- paste0(municipality_id_2, "_", municipality_name_2, "/")

name_1 <- list(
  subdir = subdir_1,
  municipality_id = municipality_id_1,
  municipality_name = municipality_name_1
)
name_2 <- list(
  subdir = subdir_2,
  municipality_id = municipality_id_2,
  municipality_name = municipality_name_2
)

file_name <- function(table_name, name) {
  paste0(name$subdir, name$municipality_id, "_wrapper_", table_name, "_", name$municipality_name, ".csv")
}

# 2. ---- データ読み込み ----
# （1）浜松市データの読み込み
personTable_1 <- read_csv(file_name("personTable", name_1), col_types = cols(
  person_id = col_integer(),
  gender_concept_id = col_integer(),
  year_of_birth = col_integer(),
  month_of_birth = col_integer(),
  day_of_birth = col_integer(),
  birth_date = col_date(),
  group = col_character(),
  )) |>
  mutate(person_id = person_id + 10000000)

observationPeriodTable_1 <- read_csv(file_name("observationPeriodTable", name_1), col_types = cols(
  observation_period_id = col_integer(),
  person_id = col_integer(),
  observation_period_start_date = col_date(),
  observation_period_end_date = col_date()
  )) |>
  mutate(person_id = person_id + 10000000)

outcomeTable_1 <- read_csv(file_name("outcomeTable", name_1), col_types = cols(
  cohort_definition_id = col_integer(),
  subject_id = col_integer(),
  cohort_start_date = col_date(),
  cohort_end_date = col_date(),
  drug_exposure_start_date = col_date(),
  lot_number = col_character(),
  provider_name = col_character()
  )) |>
  mutate(subject_id = subject_id + 10000000)

targetCohortTable_1 <- read_csv(file_name("targetCohortTable", name_1), col_types = cols(
  cohort_definition_id = col_integer(),
  subject_id = col_integer(),
  cohort_start_date = col_date(),
  cohort_end_date = col_date(),
  lot_number = col_character(),
  provider_name = col_character(),
  death_date = col_date()
)) |>
  mutate(subject_id = subject_id + 10000000)

# （2）松戸市データの読み込み
personTable_2 <- read_csv(file_name("personTable", name_2), col_types = cols(
  person_id = col_integer(),
  gender_concept_id = col_integer(),
  year_of_birth = col_integer(),
  month_of_birth = col_integer(),
  day_of_birth = col_integer(),
  birth_date = col_date(),
  group = col_character(),
)) |>
  mutate(person_id = person_id + 20000000)

observationPeriodTable_2 <- read_csv(file_name("observationPeriodTable", name_2), col_types = cols(
  observation_period_id = col_integer(),
  person_id = col_integer(),
  observation_period_start_date = col_date(),
  observation_period_end_date = col_date()
)) |>
  mutate(person_id = person_id + 20000000)

outcomeTable_2 <- read_csv(file_name("outcomeTable", name_2), col_types = cols(
  cohort_definition_id = col_integer(),
  subject_id = col_integer(),
  cohort_start_date = col_date(),
  cohort_end_date = col_date(),
  lot_number = col_character(),
  provider_name = col_character()
)) |>
  mutate(subject_id = subject_id + 20000000)

targetCohortTable_2 <- read_csv(file_name("targetCohortTable", name_2), col_types = cols(
  cohort_definition_id = col_integer(),
  subject_id = col_integer(),
  cohort_start_date = col_date(),
  cohort_end_date = col_date(),
  lot_number = col_character(),
  provider_name = col_character(),
  death_date = col_date()
)) |>
  mutate(subject_id = subject_id + 20000000)

# 3. ---- 統合テーブルの作成 ----
personTable <- bind_rows(personTable_1, personTable_2)
observationPeriodTable <- bind_rows(observationPeriodTable_1, observationPeriodTable_2)
outcomeTable <- bind_rows(outcomeTable_1, outcomeTable_2)
targetCohortTable <- bind_rows(targetCohortTable_1, targetCohortTable_2)

# 4. ---- データ書き出し ----
name <- list(
  subdir = subdir,
  municipality_id = municipality_id,
  municipality_name = municipality_name
)
save_tbl <- function(tbl, table_name, name) {
  file_name <- paste0(
    name$subdir, name$municipality_id, "_wrapper_", table_name, "_", name$municipality_name, ".csv"
  )
  write_csv(tbl, file_name, na = "")
}

save_tbl(personTable, "personTable", name)
save_tbl(observationPeriodTable, "observationPeriodTable", name)
save_tbl(targetCohortTable, "targetCohortTable", name)
save_tbl(outcomeTable, "outcomeTable", name)
