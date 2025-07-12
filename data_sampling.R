################################################################################
#                                                                              #
#                        MICROCENSUS DATA SAMPLING                             #
#                          By Derrick Demeveng                                 #
#                                                                              #
################################################################################


# LOAD LIBRARIES ----------------------------------------------------------
if(!require("pacman")) install.packages("pacman")

pacman::p_load(
  tidyverse,
  ggplot2,
  sf,
  haven,
  gt,
  tmap,
  RColorBrewer,
  patchwork
)

# LOAD DATA ---------------------------------------------------------------
micro_census_data <- haven::read_sav("input/SPSS files/DemographicsDIST.sav")

# CREATE SAMPLE -----------------------------------------------------------

# a) Select Variables of Interest

dummy_microcensus <- micro_census_data |>
  dplyr::mutate(HH_ID = paste(PROV_P, DIST_P, CONST_P, HOUSEHOLD_SERIAL_NUMBER_P, REGION_P)) |>   dplyr::select(
    PROV_P,
    DIST_P,
    CONST_P,
    REGION_P,
    HOUSEHOLD_SERIAL_NUMBER_P,
    HH_ID,
    PID_P,
    P2_MEMBERSHIP_P,
    P4_SEX_P,
    P5_AGE_P,
    P42_LAST_12_MON_P,
    P32_ACTIVITY_LAST_12_MONTHS_P,
    P37_AGE_FIRST_MARRAIGE_P
  )

# b) Stratified Systematic Sampling
# We are going to use the stratified systematic sampling method that ensures
# representation of every stata in the sample.

# Desired sample fraction (e.g., 50%)
sample_fraction <- 0.5

# Stratum columns
strat_columns <- c("HH_ID")

# Stratified systematic sampling function
stratified_systematic_sample <- function(df, strata_col, frac) {
  df |>
    group_by(across(all_of(strata_col))) |>
    arrange(HH_ID, .by_group = TRUE) |>
    mutate(row_num = row_number()) |>
    filter(row_num %% floor(1 / frac) == 1) |>
    ungroup() |>
    select(-row_num)
}

# Apply the function to the data
sampled_data <- stratified_systematic_sample(dummy_microcensus, strata_col = strat_columns, frac = sample_fraction)

# View sample
sampled_data


# VISUALIZE DISTRIBUTION --------------------------------------------------

# Sex
hist(sampled_data$P4_SEX_P)
hist(dummy_microcensus$P4_SEX_P)

# Age
hist(sampled_data$P5_AGE_P)
hist(dummy_microcensus$P5_AGE_P)

# Birth in the last 12months
hist(sampled_data$P42_LAST_12_MON_P)
hist(dummy_microcensus$P42_LAST_12_MON_P)

# Activity in the last 12months
hist(sampled_data$P32_ACTIVITY_LAST_12_MONTHS_P)
hist(dummy_microcensus$P32_ACTIVITY_LAST_12_MONTHS_P)

# Age of first marriage
hist(sampled_data$P37_AGE_FIRST_MARRAIGE_P)
hist(dummy_microcensus$P37_AGE_FIRST_MARRAIGE_P)


# EXPORT SAMPLE -----------------------------------------------------------

haven::write_sav(dummy_microcensus, path = "output/stratified_systematic_sample.sav")
