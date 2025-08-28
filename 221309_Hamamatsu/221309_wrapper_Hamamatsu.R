# wrapper (generate wrapper data)
#
# 1. ---- 初期設定 ----
library(tidyverse)

municipality_id <- "221309"
municipality_name <- "Hamamatsu"
subdir <- paste0(municipality_id, "_", municipality_name, "/")
foreign_df <- paste0(subdir, "公開文書.csv")
study_start_date <- as.Date("2021-02-01")
study_end_date <- as.Date("2024-06-30")

start_time <- Sys.time()

# 2. ---- データ読み込み ----
df <- read_csv(foreign_df, col_names = c(
  "person_id", "exit_flag", "group", "gender_concept_name",
  "drug_exposure_name", "lot_number", "provider_name",
  "drug_exposure_start_date", "death_date", "exit_group",
  "observation_period_start_date__1", "observation_period_end_date__1",
  "observation_period_start_date__2", "observation_period_end_date__2",
  "observation_period_start_date__3", "observation_period_end_date__3",
  "observation_period_start_date__4", "observation_period_end_date__4",
  "observation_period_start_date__5", "observation_period_end_date__5"
), col_types = cols(
  person_id = col_integer(),
  exit_flag = col_character(),
  group = col_character(),
  gender_concept_name = col_character(),
  drug_exposure_name = col_character(),
  lot_number = col_character(),
  provider_name = col_character(),
  drug_exposure_start_date = col_date(),
  death_date = col_date(),
  exit_group = col_character(),
  .default = col_date()
)) |>
  filter(!person_id %in% c(
    885195, 885321, 885376, 885378, 885391, # 年代に問題
    885413, 885423, 885432, 885436, 885437,
    885442, 885447, 885453, 885456,
    496790, 535373, 729770, # ワクチン接種記録に問題
    149274, 825419,
    274217, 282869, 378191, 655256, 655546, # 転出入記録に問題（1）
    700776, 721086, 777221, 777222, 805492,
    827264, 852229, 869645, 869646,
    187194, 314292, 340515, 518444, 586390, # 転出入記録に問題（2）
    641740, 669791, 716568, 753769, 813001,
    813246, 819434, 819435, 819436, 823554,
    823555, 824689, 825999, 827456, 832373,
    833581, 834309, 834578, 835989, 840514,
    842162, 842441, 848619, 851709, 853193,
    854634, 855576, 855593, 857338, 857740,
    858071, 865069, 867410, 868136, 868412,
    869642, 871663, 873115, 873470, 875392,
    878686, 880127, 880396, 882865, 885394
  ))

# 3. ---- ベーステーブル作成 ----
register_df <- df |>
  filter(!is.na(exit_flag)) |>
  select(
    person_id, group, gender_concept_name, lot_number, provider_name,
    death_date, starts_with("observation_period_")
  )

VRS_df <- df |>
  select(
    person_id, exit_flag, drug_exposure_name, lot_number,
    provider_name, drug_exposure_start_date, death_date
  )

VRS_df_aug <- VRS_df |>
  filter(!is.na(exit_flag) & !is.na(drug_exposure_name)) |>
  mutate(
    exit_flag = NA_character_,
    drug_exposure_name = NA_character_,
    lot_number = NA_character_,
    provider_name = NA_character_,
    drug_exposure_start_date = as.Date(NA),
    death_date = as.Date(NA)
  )

VRS_df <- VRS_df |>
  bind_rows(VRS_df_aug) |>
  arrange(person_id, drug_exposure_start_date) |>
  mutate(
    cohort_definition_id = case_when(
      is.na(drug_exposure_name) ~ 0L,
      drug_exposure_name == "１回目" ~ 1L,
      drug_exposure_name == "２回目" ~ 2L,
      drug_exposure_name == "３回目" ~ 3L,
      drug_exposure_name == "４回目" ~ 4L,
      drug_exposure_name == "５回目" ~ 5L,
      drug_exposure_name == "６回目" ~ 6L,
      drug_exposure_name == "７回目" ~ 7L,
      TRUE ~ NA_integer_
    ),
    drug_exposure_start_date = if_else(cohort_definition_id == 0,
      study_start_date, drug_exposure_start_date
    )
  )

# 4. ---- personTable作成 ----
gender_map <- c("男" = 8507L, "女" = 8532L)
age_map <- c(
  "0～4歳" = 2021L, "5～9歳" = 2016L, "10～14歳" = 2011L, "15～19歳" = 2006L,
  "20～24歳" = 2001L, "25～29歳" = 1996L, "30～34歳" = 1991L, "35～39歳" = 1986L,
  "40～44歳" = 1981L, "45～49歳" = 1976L, "50～54歳" = 1971L, "55～59歳" = 1966L,
  "60～64歳" = 1961L, "65～69歳" = 1956L, "70～74歳" = 1951L, "75～79歳" = 1946L,
  "80～84歳" = 1941L, "85～89歳" = 1936L, "90～94歳" = 1931L, "95～99歳" = 1926L,
  "100歳～" = 1921L
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
    year_of_birth, month_of_birth, day_of_birth, birth_date, group
  )

# 5. ---- observationPeriodTable作成 ----
observationPeriodTable <- register_df |>
  pivot_longer(
    cols = starts_with("observation_period_"),
    names_to = c("observation_period_state", "observation_period_name"),
    names_sep = "__",
    values_to = "observation_period_date"
  ) |>
  pivot_wider(
    names_from = "observation_period_state",
    values_from = "observation_period_date"
  ) |>
  filter(!(observation_period_name != "1" &
    is.na(observation_period_start_date) &
    is.na(observation_period_end_date))) |>
  replace_na(list(observation_period_start_date = study_start_date)) |>
  mutate(
    observation_period_end_date =
      if_else(is.na(observation_period_end_date) & is.na(death_date),
        study_end_date,
        if_else(observation_period_end_date == death_date,
          death_date,
          observation_period_end_date - days(1),
          observation_period_end_date - days(1)
        )
      )
  ) |>
  arrange(person_id, observation_period_start_date) |>
  group_by(person_id) |>
  mutate(observation_period_id = row_number()) |>
  ungroup() |>
  select(
    observation_period_id, person_id,
    observation_period_start_date, observation_period_end_date
  )

# 6. ---- outcomeTable作成 ----
outcomeTable <- VRS_df |>
  filter(!is.na(death_date)) |>
  mutate(
    drug_exposure_id = cohort_definition_id,
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
  arrange(person_id, cohort_definition_id) |>
  group_by(person_id) |>
  mutate(
    next_start_date = lead(drug_exposure_start_date),
    drug_exposure_end_date = case_when(
      !is.na(exit_flag) & !is.na(death_date) ~ death_date,
      !is.na(exit_flag) & is.na(death_date) ~ study_end_date,
      TRUE ~ next_start_date - days(1)
    )
  ) |>
  ungroup() |>
  select(
    cohort_definition_id,
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
