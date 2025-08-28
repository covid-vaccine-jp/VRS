# VRS1 (estimate incidence with IncidencePrevalence)
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

for (i in 0:8) {
  result_name <- paste0(i)
  # "0" 全体（検証用）
  # "1" 接種の有無別
  # "2" 年齢階層・接種の有無別
  # "3" 年齢階層・接種回数別
  # "4" 20～49歳、接種回数別
  # "5" 20～49歳、四半期・接種回数別
  # "6" 65～89歳、接種回数別
  # "7" 65～89歳、四半期・接種回数別
  # "8" 80歳以上、四半期・接種回数別

  if (result_name == "0") {
    ageGroup <- list(c(0, 150))
  } else if (result_name == "1") {
    ageGroup <- list(c(0, 150))
  } else if (result_name %in% c("2", "3")) {
    if (phase == "1") {
      ageGroup <- list(
        c(0, 9), c(10, 19), c(20, 29), c(30, 39), c(40, 49),
        c(50, 59), c(60, 69), c(70, 79), c(80, 89), c(90, 150)
      )
    } else {
      ageGroup <- list(
        c(0, 9), c(10, 19), c(20, 29), c(30, 39), c(40, 49),
        c(50, 59), c(60, 64), c(65, 69), c(70, 74),
        c(75, 79), c(80, 84), c(85, 89), c(90, 94), c(95, 150)
      )
    }
  } else if (result_name %in% c("4", "5")) {
    ageGroup <- list(c(20, 49))
  } else if (result_name %in% c("6", "7")) {
    ageGroup <- list(c(65, 89))
  } else if (result_name == "8") {
    ageGroup <- list(c(80, 150))
  }

  sex <- c("Both")

  interval <- case_when(
    result_name == "0" ~ "overall",
    result_name == "1" ~ "overall",
    result_name == "2" ~ "overall",
    result_name == "3" ~ "overall",
    result_name == "4" ~ "overall",
    result_name == "5" ~ "quarters",
    result_name == "6" ~ "overall",
    result_name == "7" ~ "quarters",
    result_name == "8" ~ "quarters"
  )

  completeDatabaseIntervals <- case_when(
    result_name == "5" ~ TRUE,
    result_name == "7" ~ TRUE,
    result_name == "8" ~ TRUE,
    TRUE ~ FALSE
  )

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

  if (result_name %in% c("1", "2")) {
    targetCohortTable <- targetCohortTable |>
      arrange(subject_id, cohort_definition_id) |>
      group_by(subject_id) |>
      mutate(
        dummy = row_number() - 1,
        cohort_end_date = if_else(dummy != 0, last(cohort_end_date), cohort_end_date)
      ) |>
      ungroup() |>
      filter(cohort_definition_id <= 1)
  }

  # 3. ---- 死亡率の推定 ----
  library(IncidencePrevalence)

  cdm <- mockIncidencePrevalence(
    personTable = personTable,
    observationPeriodTable = observationPeriodTable,
    targetCohortTable = targetCohortTable,
    outcomeTable = outcomeTable
  )

  if (result_name == "0") {
    cdm <- generateDenominatorCohortSet(
      cdm = cdm,
      name = "denominator",
      cohortDateRange = c(study_start_date, study_end_date),
      ageGroup = ageGroup,
      sex = sex
    )
  } else {
    cdm <- generateTargetDenominatorCohortSet(
      cdm = cdm,
      name = "denominator",
      targetCohortTable = "target",
      cohortDateRange = c(study_start_date, study_end_date),
      ageGroup = ageGroup,
      sex = sex,
      requirementsAtEntry = FALSE
    )
  }

  inc <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    interval = interval,
    completeDatabaseIntervals = completeDatabaseIntervals
  )

  inc_df <- asIncidenceResult(inc)

  # 4. ---- 結果の保存 ----
  result_file_name <- paste0(subdir, municipality_id, "_VRS1_", result_name, "_", municipality_name, ".csv")
  write_csv(inc_df, result_file_name)
}
