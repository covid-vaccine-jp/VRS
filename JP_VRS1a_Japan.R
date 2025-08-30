# VRS1a (estimate incidence with IncidencePrevalence)
#
# 1. ---- 初期設定 ----
## 自治体固有の設定
municipality_id <- "JP"
municipality_name <- "Japan"
subdir <- paste0("")
study_start_date <- as.Date("2021-02-01")
study_end_date <- as.Date("2024-03-31")

## 共通の設定
library(tidyverse)

phase <- "1"
# "0" exploratory
# "1" release

name <- list(
  subdir = subdir,
  municipality_id = municipality_id,
  municipality_name = municipality_name
)

file_name <- function(table_name, name) {
  paste0(name$subdir, name$municipality_id, "_wrapper_", table_name, "_", name$municipality_name, ".csv")
}

# 2. ---- データ読み込み ----
personTable <- read_csv(file_name("personTable", name), col_types = cols(
  person_id = col_integer(),
  gender_concept_id = col_integer(),
  year_of_birth = col_integer(),
  month_of_birth = col_integer(),
  day_of_birth = col_integer(),
  birth_date = col_date(),
  group = col_character()
))

observationPeriodTable <- read_csv(file_name("observationPeriodTable", name), col_types = cols(
  observation_period_id = col_integer(),
  person_id = col_integer(),
  observation_period_start_date = col_date(),
  observation_period_end_date = col_date()
))
if (phase == "1") {
  observationPeriodTable <- observationPeriodTable |>
    filter(observation_period_id == 1)
}

outcomeTable <- read_csv(file_name("outcomeTable", name), col_types = cols(
  cohort_definition_id = col_integer(),
  subject_id = col_integer(),
  cohort_start_date = col_date(),
  cohort_end_date = col_date(),
  drug_exposure_id = col_integer(),
  drug_exposure_start_date = col_date(),
  lot_number = col_character(),
  provider_name = col_character()
))

targetCohortTable <- read_csv(file_name("targetCohortTable", name), col_types = cols(
  cohort_definition_id = col_integer(),
  subject_id = col_integer(),
  cohort_start_date = col_date(),
  cohort_end_date = col_date(),
  lot_number = col_character(),
  provider_name = col_character(),
  death_date = col_date()
))

for (i in 1:6) {
  result_name <- paste0(i)
  # "1" 20～49歳、接種回数別
  # "2" 20～49歳、四半期・接種回数別
  # "3" 50～64歳、接種回数別
  # "4" 50～64歳、四半期・接種回数別
  # "5" 65～89歳、接種回数別
  # "6" 65～89歳、四半期・接種回数別

  if (result_name %in% c("1", "2")) {
    ageGroup <- list(c(20, 49))
  } else if (result_name %in% c("3", "4")) {
    ageGroup <- list(c(50, 64))
  } else if (result_name %in% c("5", "6")) {
    ageGroup <- list(c(65, 89))
  }

  sex <- c("Both")

  interval <- case_when(
    result_name == "1" ~ "overall",
    result_name == "2" ~ "quarters",
    result_name == "3" ~ "overall",
    result_name == "4" ~ "quarters",
    result_name == "5" ~ "overall",
    result_name == "6" ~ "quarters",
  )

  completeDatabaseIntervals <- case_when(
    result_name == "2" ~ TRUE,
    result_name == "4" ~ TRUE,
    result_name == "6" ~ TRUE,
    TRUE ~ FALSE
  )

  # 3. ---- 死亡率の推定 ----
  library(IncidencePrevalence)

  cdm <- mockIncidencePrevalence(
    personTable = personTable,
    observationPeriodTable = observationPeriodTable,
    targetCohortTable = targetCohortTable,
    outcomeTable = outcomeTable
  )

  cdm <- generateTargetDenominatorCohortSet(
    cdm = cdm,
    name = "denominator",
    targetCohortTable = "target",
    cohortDateRange = c(study_start_date, study_end_date),
    ageGroup = ageGroup,
    sex = sex,
    requirementsAtEntry = FALSE
  )
  
  inc <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    interval = interval,
    completeDatabaseIntervals = completeDatabaseIntervals
  )

  inc_df <- asIncidenceResult(inc)

  # 4. ---- 結果の保存 ----
  result_file_name <- paste0(subdir, municipality_id, "_VRS1a_", result_name, "_", municipality_name, ".csv")
  write_csv(inc_df, result_file_name)
}

# 5. ---- 接種日から死亡日までの間隔 ----
df <- outcomeTable |>
  left_join(personTable, by = c("subject_id" = "person_id")) |>
  mutate(duration = as.numeric(cohort_start_date - drug_exposure_start_date)) |>
  select(subject_id, drug_exposure_id, group, year_of_birth, duration)

result_file_name <- paste0(subdir, municipality_id, "_VRS1a_duration_", municipality_name, ".csv")
write_csv(df, result_file_name)

