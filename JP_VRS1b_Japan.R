# VRS1b (estimate incidence with IncidencePrevalence)
#
# 1. ---- Initial settings ----
## Municipality-dependent settings
municipality_id <- "JP"
municipality_name <- "Japan"
subdir <- paste0("")
study_start_date <- as.Date("2021-02-01")
study_end_date <- as.Date("2024-03-31")

age0_9 <- c("0～4歳", "5～9歳", "2021～2025", "2016～2020")
age10_19 <- c("10～14歳", "15～19歳", "2011～2015", "1986～1990")
age20_29 <- c("20～24歳", "25～29歳", "2001～2005", "1996～2000")
age30_39 <- c("30～34歳", "35～39歳", "1991～1995", "1986～1990")
age40_49 <- c("40～44歳", "45～49歳", "1981～1985", "1976～1980")
age50_59 <- c("50～54歳", "55～59歳", "1971～1975", "1966～1970")
age60_69 <- c("60～64歳", "65～69歳", "1961～1965", "1956～1960")
age70_79 <- c("70～74歳", "75～79歳", "1951～1955", "1946～1950")
age80_89 <- c("80～84歳", "85～89歳", "1941～1945", "1936～1940")
age90_ <- c("80～84歳", "85～89歳", "90～94歳", "95～100歳", "100歳～",
            "1941～1945", "1936～1940", "1931～1935", "1926～1930",
            "1921～1925", "1916～1920", "1911～1915")

## Common settings
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

# 2. ---- Read data ----
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

last_id_df <- targetCohortTable |>
  filter((cohort_start_date >= study_start_date) & (cohort_start_date <= study_end_date)) |>
  arrange(subject_id, cohort_definition_id) |>
  group_by(subject_id) |>
  summarize(last_id = last(cohort_definition_id))

# 3. ---- Calculate number of persons by number of cumulative doses ----
inc_df <- personTable |>
  mutate(age_group = case_when(
    group %in% age0_9 ~ 0,
    group %in% age10_19 ~ 10,
    group %in% age20_29 ~ 20,
    group %in% age30_39 ~ 30,
    group %in% age40_49 ~ 40,
    group %in% age50_59 ~ 50,
    group %in% age60_69 ~ 60,
    group %in% age70_79 ~ 70,
    group %in% age80_89 ~ 80,
    group %in% age90_ ~ 90,
    TRUE ~ 90
  )) |>
  left_join(last_id_df, by = c("person_id" = "subject_id")) |>
  group_by(age_group, last_id) |>
  summarize(n = n())

result_file_name <- paste0(subdir, municipality_id, "_VRS1b_lastid_", municipality_name, ".csv")
write_csv(inc_df, result_file_name)

# 4. ---- Estimate incidence ----
library(IncidencePrevalence)

for (i in 1:4) {
  result_name <- paste0(i)
  # "1" by age group (10) / vaccination status
  # "2" by age group (4) / vaccination status
  # "3" by age group (4) / quarter / vaccination status
  # "4" by age group (4) / month / vaccination status

  if (result_name %in% c("1")) {
    ageGroup <- list(
      c(0, 9), c(10, 19), c(20, 29), c(30, 39), c(40, 49),
      c(50, 59), c(60, 69), c(70, 79), c(80, 89), c(90, 150)
    )
  } else if (result_name %in% c("2", "3", "4")) {
    ageGroup <- list(c(20, 49), c(50, 64), c(65, 89))
  }

  sex <- c("Both")

  interval <- case_when(
    result_name == "1" ~ "overall",
    result_name == "2" ~ "overall",
    result_name == "3" ~ "quarters",
    result_name == "4" ~ "months",
    TRUE ~ "overall"
  )

  completeDatabaseIntervals <- case_when(
    result_name == "3" ~ TRUE,
    result_name == "4" ~ TRUE,
    TRUE ~ FALSE
  )

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

  result_file_name <- paste0(subdir, municipality_id, "_VRS1b_", result_name, "_", municipality_name, ".csv")
  write_csv(inc_df, result_file_name)
}

# 5. ---- Calculate duration from vaccination to death ----
df <- outcomeTable |>
  left_join(personTable, by = c("subject_id" = "person_id")) |>
  mutate(duration = as.numeric(cohort_start_date - drug_exposure_start_date)) |>
  select(subject_id, drug_exposure_id, group, year_of_birth, duration)

result_file_name <- paste0(subdir, municipality_id, "_VRS1b_duration_", municipality_name, ".csv")
write_csv(df, result_file_name)
