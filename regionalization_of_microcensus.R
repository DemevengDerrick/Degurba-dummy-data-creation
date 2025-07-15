################################################################################
#                                                                              #
#                     MICROCENSUS DATA REGIONALIZATION                         #
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

boundaries_dummy <- sf::st_read("input/jrc_data/poly_dummy_mw_pop.shp")


# ASSIGN MICROCENSUS TO DUMMY BOUNDARIES ----------------------------------
micro_census_data <- micro_census_data |>
  dplyr::mutate(HH_ID = paste(PROV_P, DIST_P, CONST_P, HOUSEHOLD_SERIAL_NUMBER_P, REGION_P))

hh_summary <- micro_census_data |>
  group_by(HH_ID) |>
  summarise(
    hh_size = n(),
    # Number of people in household
    avg_age = mean(P5_AGE_P, na.rm = TRUE),
    prop_female = mean(P4_SEX_P == 2, na.rm = TRUE),
    employment_rate = mean(P32_ACTIVITY_LAST_12_MONTHS_P %in% c(1, 2, 3, 4, 5), na.rm = TRUE),
    marriage_rate = mean(!is.na(P37_AGE_FIRST_MARRAIGE_P), na.rm = TRUE),
    .groups = "drop"
  )

# scale the variables
hh_scaled <- hh_summary |>
  select(-HH_ID) |>
  scale()

# clustering
set.seed(42)
kmeans_result <- kmeans(hh_scaled, centers = 190, nstart = 25)

# Add cluster labels to household summary
hh_summary$synthetic_const <- kmeans_result$cluster

# merge back to micro-census data
micro_clustered <- micro_census_data |>
  left_join(hh_summary |> select(HH_ID, synthetic_const), by = "HH_ID")

# Add Regions
micro_clustered <- micro_clustered |>
  dplyr::mutate(synthetic_const = synthetic_const - 1) |>
  left_join(boundaries_dummy |> select(id, Region), by = c("synthetic_const" = "id"))

# Generate new HH_ID using new constituency code
micro_clustered <- micro_clustered |>
  dplyr::mutate(
    HH_ID = paste(Region, synthetic_const, HOUSEHOLD_SERIAL_NUMBER_P, REGION_P)
  )

# Restructure the data

micro_clustered2 <- micro_clustered  |>
  dplyr::select(
    Region,
    synthetic_const,
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
  ) |>
  dplyr::rename(
    "REGION" = Region,
    "CONSTITUENCY" = synthetic_const,
    "ZONE" = REGION_P,
    "PID" = PID_P,
    "MEMBERSHIP" = P2_MEMBERSHIP_P,
    "SEX" = P4_SEX_P,
    "AGE" = P5_AGE_P,
    "BIRTH_LAST_12_MON" = P42_LAST_12_MON_P,
    "ACTIVITY_LAST_12_MON" = P32_ACTIVITY_LAST_12_MONTHS_P,
    "AGE_FIRST_MARRIAGE" = P37_AGE_FIRST_MARRAIGE_P,
    "HOUSEHOLD_NUMBER" = HOUSEHOLD_SERIAL_NUMBER_P
  )

# Attributes
attr(micro_clustered2$HH_ID, "label") <- "Household Unique Identifier"
attr(micro_clustered2$REGION, "label") <- "Region"
attr(micro_clustered2$CONSTITUENCY, "label") <- "Constituency"
attr(micro_clustered2$ZONE, "label") <- "Urban - Rural"

# EXPORT SAMPLE -----------------------------------------------------------

haven::write_sav(micro_clustered2, path = "output/cleaned_dummy_data.sav")









