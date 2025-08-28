# wrapper (generate wrapper data)
#
# 1. ---- 初期設定 ----
library(tidyverse)
library(readxl)

municipality_id <- "122076"
municipality_name <- "Matsudo"
subdir <- paste0(municipality_id, "_", municipality_name, "/")
foreign_df <- paste0(subdir, "【松戸市】VRS＋死亡.xlsx")
study_start_date <- as.Date("2021-02-01")
study_end_date <- as.Date("2025-03-31")

start_time <- Sys.time()

# 2. ---- データ読み込み ----
df <- read_xlsx(foreign_df, col_names = c(
  "group", "gender_concept_name", "observation_period_end_date", "death_date",
  "drug_exposure_start_date__1", "provider_name__1", "lot_number__1",
  "drug_exposure_start_date__2", "provider_name__2", "lot_number__2",
  "drug_exposure_start_date__3", "provider_name__3", "lot_number__3",
  "drug_exposure_start_date__4", "provider_name__4", "lot_number__4",
  "drug_exposure_start_date__5", "provider_name__5", "lot_number__5",
  "drug_exposure_start_date__6", "provider_name__6", "lot_number__6",
  "drug_exposure_start_date__7", "provider_name__7", "lot_number__7",
  "drug_exposure_start_date__8", "provider_name__8", "lot_number__8"
), col_types = "text", skip = 1) |>
  mutate(person_id = row_number()) |>
  filter(!person_id %in% c(
    442670, # 性別に問題
    497118, # 転出日に問題
    30259, 63423, 285132 # ワクチン接種記録に問題
  ))

# 3. ---- ベーステーブル作成 ----
register_df <- df |>
  mutate(
    observation_period_end_date = as.Date(observation_period_end_date, format = "%Y%m%d"),
    observation_period_end_date = if_else(
      observation_period_end_date > study_end_date,
      study_end_date,
      observation_period_end_date
    ),
    death_date = as.Date(death_date, format = "%Y%m%d"),
    death_date = if_else(death_date > study_end_date, NA, death_date)
  ) |>
  select(
    person_id, group, gender_concept_name,
    observation_period_end_date, death_date
  )

VRS_df <- df |>
  mutate(
    drug_exposure_start_date__0 = format(study_start_date, "%Y%m%d"),
    provider_id__0 = NA_character_,
    lot_number__0 = NA_character_
  ) |>
  pivot_longer(
    cols = contains("__"),
    names_to = c("drug_exposure_state", "drug_exposure_id"),
    names_sep = "__",
    values_to = "drug_exposure_value"
  ) |>
  filter(!(drug_exposure_id != 0 & is.na(drug_exposure_value))) |>
  pivot_wider(
    names_from = "drug_exposure_state",
    values_from = "drug_exposure_value"
  ) |>
  mutate(
    observation_period_end_date = as.Date(observation_period_end_date, format = "%Y%m%d"),
    observation_period_end_date = if_else(
      observation_period_end_date > study_end_date,
      study_end_date,
      observation_period_end_date
    ),
    death_date = as.Date(death_date, format = "%Y%m%d"),
    death_date = if_else(death_date > study_end_date, NA, death_date),
    drug_exposure_id = as.integer(drug_exposure_id),
    drug_exposure_start_date = as.Date(drug_exposure_start_date, format = "%Y%m%d")
  ) |>
  arrange(person_id, drug_exposure_id) |>
  group_by(person_id) |>
  mutate(
    next_start_date = lead(drug_exposure_start_date),
    drug_exposure_end_date = case_when(
      !is.na(next_start_date) ~ next_start_date - days(1),
      is.na(next_start_date) & !is.na(observation_period_end_date) ~ observation_period_end_date - days(1),
      is.na(next_start_date) & !is.na(death_date) ~ death_date,
      TRUE ~ study_end_date
    )
  )

# 4. ---- personTable作成 ----
gender_map <- c("男" = 8507L, "女" = 8532L)
age_map <- c(
  "1911～1915" = 1911L, "1916～1920" = 1916L, "1921～1925" = 1921L,
  "1926～1930" = 1926L, "1931～1935" = 1931L, "1936～1940" = 1936L,
  "1941～1945" = 1941L, "1946～1950" = 1946L, "1951～1955" = 1951L,
  "1956～1960" = 1956L, "1961～1965" = 1961L, "1966～1970" = 1966L,
  "1971～1975" = 1971L, "1976～1980" = 1976L, "1981～1985" = 1981L,
  "1986～1990" = 1986L, "1991～1995" = 1991L, "1996～2000" = 1996L,
  "2001～2005" = 2001L, "2006～2010" = 2006L, "2011～2015" = 2011L,
  "2016～2020" = 2016L, "2021～2025" = 2021L
)

personTable <- register_df |>
  mutate(
    gender_concept_id = gender_map[gender_concept_name],
    year_of_birth = age_map[group],
    month_of_birth = 1L,
    day_of_birth = 1L,
    birth_date = as.Date(sprintf("%04d-%02d-%02d", year_of_birth, month_of_birth, day_of_birth))
  ) |>
  select(
    person_id, gender_concept_id,
    year_of_birth, month_of_birth, day_of_birth,
    birth_date, group
  )

# 5. ---- observationPeriodTable作成 ----
observationPeriodTable <- register_df |>
  mutate(
    observation_period_id = 1L,
    observation_period_start_date = study_start_date,
    observation_period_end_date = case_when(
      !is.na(observation_period_end_date) ~ observation_period_end_date - days(1),
      !is.na(death_date) ~ death_date,
      TRUE ~ study_end_date
    )
  ) |>
  replace_na(list(observation_period_end_date = study_end_date)) |>
  select(
    observation_period_id, person_id,
    observation_period_start_date, observation_period_end_date
  )

# 6. ---- outcomeTable作成 ----
outcomeTable <- VRS_df |>
  filter(is.na(next_start_date), !is.na(death_date)) |>
  mutate(
    cohort_definition_id = 1L,
    cohort_end_date = death_date
  ) |>
  select(
    cohort_definition_id,
    subject_id = person_id,
    cohort_start_date = death_date,
    cohort_end_date,
    drug_exposure_id,
    drug_exposure_start_date,
    lot_number,
    provider_name
  )

# 7. ---- targetCohortTable作成 ----
targetCohortTable <- VRS_df |>
  select(
    cohort_definition_id = drug_exposure_id,
    subject_id = person_id,
    cohort_start_date = drug_exposure_start_date,
    cohort_end_date = drug_exposure_end_date,
    lot_number,
    provider_name,
    death_date
  )

# 8. ---- データ書き出し ----
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

Sys.time() - start_time
