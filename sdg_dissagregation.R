################################################################################
#                                                                              #
#                        SDG DISAGGREGATION BY DEGURBA                         #
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
demographics <- haven::read_sav("Output/dummy_microcensus_final.sav") # replace path with path to micro-census dummy data on your computer

constituency_shape <- sf::read_sf("D:\\UNFPA\\DEGURBA_Paraguay\\data_prep\\Output\\TUC_poly\\poly_dummy_mw_pop_GHS-DU-TUC.shp") # replace path with path to classified dummy polygons on your computer

gps_shape <- sf::read_sf("D:\\UNFPA\\DEGURBA_Paraguay\\data_prep\\Output\\TUC\\point_dummy_mw_popcount_new_GHS-DU-TUC.shp") # replace path with path to classified dummy points on your computer


# COMPUTE INDICATORS ----------------------------------

# I- COMPUTATION FROM POLYGONES AND MICRO-CENSUS DATA ---------------------

# a- Adolescent Birth Rate by Constituency --------------------------------

adolescent_birth_rate <-
  demographics |>
  dplyr::filter(SEX == 2) |>
  dplyr::group_by(CONSTITUENCY) |>
  dplyr::summarise(
    total.ado.10_14 = sum(dplyr::between(AGE, 10, 14), na.rm = TRUE),
    total.ado.15_19 = sum(dplyr::between(AGE, 15, 19), na.rm = TRUE),
    total.ado.birth.10_14 = sum(dplyr::between(AGE, 10, 14) &
                                  BIRTH_LAST_12_MON == 1, na.rm = TRUE),
    total.ado.birth.15_19 = sum(dplyr::between(AGE, 15, 19) &
                                  BIRTH_LAST_12_MON == 1, na.rm = TRUE),
    abr.10_14 = round((total.ado.birth.10_14 / total.ado.10_14) * 1000, 2),
    abr.15_19 = round((total.ado.birth.15_19 / total.ado.15_19) * 1000, 2)
    
  ) |>
  dplyr::select(CONSTITUENCY, everything())

# Print table
adolescent_birth_rate |>
  head() |>
  gt::gt()

# Map
constituency_shape |>
  dplyr::select(id, geometry) |>
  dplyr::left_join(adolescent_birth_rate, by = c("id" = "CONSTITUENCY")) |>
  tidyr::pivot_longer(
    cols = c("abr.10_14", "abr.15_19"),
    names_to = "abr_cat",
    values_to = "abr_rate"
  ) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(aes(fill = abr_rate), color = "white") +
  ggplot2::scale_fill_distiller(palette = "RdPu", direction = 1) +
  ggplot2::theme_void() +
  ggplot2::labs(title = "Adolescent Birth Rate per 1000") +
  ggplot2::theme(legend.position = "bottom",
                 plot.title = element_text(face = "bold", hjust = 0.5)) +
  ggplot2::facet_grid( ~ abr_cat)

# b- Child Marriage by Constituency --------------------------------

child_marriage <-
  demographics |>
  dplyr::filter(AGE >= 20, AGE <= 24, SEX == 2) |>
  dplyr::group_by(CONSTITUENCY) |>
  dplyr::summarise(
    total.girls.20_24 = n(),
    married.before.15 = sum(AGE_FIRST_MARRIAGE < 15, na.rm = T),
    married.before.18 = sum(AGE_FIRST_MARRIAGE < 18, na.rm = T),
    prop.cm.before.15 = round(married.before.15 / total.girls.20_24, 2),
    prop.cm.before.18 = round(married.before.18 / total.girls.20_24, 2)
  ) |>
  dplyr::select(CONSTITUENCY, everything())

# print table
child_marriage |>
  head() |>
  gt::gt()

# Map
constituency_shape |>
  dplyr::select(id, geometry) |>
  dplyr::left_join(child_marriage, by = c("id" = "CONSTITUENCY")) |>
  tidyr::pivot_longer(
    cols = c("prop.cm.before.15", "prop.cm.before.18"),
    names_to = "child_marriage",
    values_to = "cm_rate"
  ) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(aes(fill = cm_rate), color = "white") +
  ggplot2::scale_fill_distiller(palette = "RdPu", direction = 1) +
  ggplot2::theme_void() +
  ggplot2::labs(title = "Child Marriage") +
  ggplot2::theme(legend.position = "bottom",
                 plot.title = element_text(face = "bold", hjust = 0.5)) +
  ggplot2::facet_grid( ~ child_marriage)

# c- Youth Unemployment by Constituency -----------------------------------

youth_umemplyment <-
  demographics |>
  dplyr::mutate(
    Employ = case_when(
      ACTIVITY_LAST_12_MON %in% 1:6 ~ 1,
      ACTIVITY_LAST_12_MON %in% 7:9 ~ 2,
      ACTIVITY_LAST_12_MON == 10 ~ 3,
      T ~ 2
    )
  ) |>
  dplyr::filter(AGE >= 15, AGE <= 24) |>
  dplyr::group_by(CONSTITUENCY, SEX) |>
  summarise(
    total.youth.15_24 = n(),
    total.neet = sum(Employ == 2, na.rm = TRUE),
    rate.neet = round(total.neet / total.youth.15_24, 2)
  ) |>
  dplyr::mutate(SEX = forcats::as_factor(SEX)) |>
  dplyr::select(CONSTITUENCY, everything()) |>
  tidyr::pivot_wider(names_from =  SEX, values_from = 3:5)

# print table
youth_umemplyment |>
  tibble::tibble() |>
  head() |>
  gt::gt()

# Map

constituency_shape |>
  dplyr::select(id, geometry) |>
  dplyr::left_join(youth_umemplyment, by = c("id" = "CONSTITUENCY")) |>
  tidyr::pivot_longer(
    cols = c("rate.neet_Male", "rate.neet_Female"),
    names_to = "neet",
    values_to = "neet_rate"
  ) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(aes(fill = neet_rate), color = "white") +
  ggplot2::scale_fill_distiller(palette = "RdPu", direction = 1) +
  ggplot2::theme_void() +
  ggplot2::labs(title = "NEET") +
  ggplot2::theme(legend.position = "bottom",
                 plot.title = element_text(face = "bold", hjust = 0.5)) +
  ggplot2::facet_grid( ~ neet)


# d- Export Indicators ----------------------------------------------------

indicators_df <- 
  child_marriage |>
  dplyr::left_join(
    adolescent_birth_rate,
    by = c("CONSTITUENCY")
  ) |>
  dplyr::left_join(
    youth_umemplyment,
    by = c("CONSTITUENCY")
  )
# print table
indicators_df |>
  head() |>
  gt::gt()

# Apply DEGURBA classes from the Polygons classification
indicators_df <- indicators_df |>
  dplyr::left_join(constituency_shape |> select(id, DEGURBA_L1, DEGURBA_L2),
                   by = c("CONSTITUENCY" = "id")
                   ) |>
  dplyr::select(CONSTITUENCY, DEGURBA_L1, DEGURBA_L2, everything())

# Indicators to CSV
readr::write_csv(x = indicators_df |> select(1:ncol(indicators_df)-1), file = "Output/indicators_from_polygons.csv")


# II- COMPUTATION FROM GPS POINTS AND MICRO-CENSUS DATA -------------------

# a) Assign Degurba classes from GPS classification to Micro-Census data

demographics_gps <- demographics |>
  dplyr::left_join(gps_shape |> select(id, DEGURBA_L1, DEGURBA_L2),
                   by = c("BUILDING_ID" = "id")
                   )
# # Export to sav
# demographics_gps <- demographics_gps |> select(1:ncol(demographics_gps)-1)
# haven::write_sav(demographics_gps, path = "output/dummy_microcensus_final_with_DEGURBA_Classes_from_GPS.sav")


# b) Compute Indicators ---------------------------------------------------

# i) Adolescent Birth Rate
adolescent_birth_rate_L1 <-
  demographics_gps |>
  dplyr::filter(SEX == 2) |>
  dplyr::group_by(DEGURBA_L1) |>
  dplyr::summarise(
    total.ado.10_14 = sum(dplyr::between(AGE, 10, 14), na.rm = TRUE),
    total.ado.15_19 = sum(dplyr::between(AGE, 15, 19), na.rm = TRUE),
    total.ado.birth.10_14 = sum(dplyr::between(AGE, 10, 14) &
                                  BIRTH_LAST_12_MON == 1, na.rm = TRUE),
    total.ado.birth.15_19 = sum(dplyr::between(AGE, 15, 19) &
                                  BIRTH_LAST_12_MON == 1, na.rm = TRUE),
    abr.10_14 = round((total.ado.birth.10_14 / total.ado.10_14) * 1000, 2),
    abr.15_19 = round((total.ado.birth.15_19 / total.ado.15_19) * 1000, 2)
  )

adolescent_birth_rate_L2 <-
  demographics_gps |>
  dplyr::filter(SEX == 2) |>
  dplyr::group_by(DEGURBA_L2) |>
  dplyr::summarise(
    total.ado.10_14 = sum(dplyr::between(AGE, 10, 14), na.rm = TRUE),
    total.ado.15_19 = sum(dplyr::between(AGE, 15, 19), na.rm = TRUE),
    total.ado.birth.10_14 = sum(dplyr::between(AGE, 10, 14) &
                                  BIRTH_LAST_12_MON == 1, na.rm = TRUE),
    total.ado.birth.15_19 = sum(dplyr::between(AGE, 15, 19) &
                                  BIRTH_LAST_12_MON == 1, na.rm = TRUE),
    abr.10_14 = round((total.ado.birth.10_14 / total.ado.10_14) * 1000, 2),
    abr.15_19 = round((total.ado.birth.15_19 / total.ado.15_19) * 1000, 2)
  )


# ii) Child Marriage

child_marriage_L1 <-
  demographics_gps |>
  dplyr::filter(AGE >= 20, AGE <= 24, SEX == 2) |>
  dplyr::group_by(DEGURBA_L1) |>
  dplyr::summarise(
    total.girls.20_24 = n(),
    married.before.15 = sum(AGE_FIRST_MARRIAGE < 15, na.rm = T),
    married.before.18 = sum(AGE_FIRST_MARRIAGE < 18, na.rm = T),
    prop.cm.before.15 = round(married.before.15 / total.girls.20_24, 2),
    prop.cm.before.18 = round(married.before.18 / total.girls.20_24, 2)
  )

child_marriage_L2 <-
  demographics_gps |>
  dplyr::filter(AGE >= 20, AGE <= 24, SEX == 2) |>
  dplyr::group_by(DEGURBA_L2) |>
  dplyr::summarise(
    total.girls.20_24 = n(),
    married.before.15 = sum(AGE_FIRST_MARRIAGE < 15, na.rm = T),
    married.before.18 = sum(AGE_FIRST_MARRIAGE < 18, na.rm = T),
    prop.cm.before.15 = round(married.before.15 / total.girls.20_24, 2),
    prop.cm.before.18 = round(married.before.18 / total.girls.20_24, 2)
  )


# iii) Youth Unemployment

youth_umemplyment_L1 <-
  demographics_gps |>
  dplyr::mutate(
    Employ = case_when(
      ACTIVITY_LAST_12_MON %in% 1:6 ~ 1,
      ACTIVITY_LAST_12_MON %in% 7:9 ~ 2,
      ACTIVITY_LAST_12_MON == 10 ~ 3,
      T ~ 2
    )
  ) |>
  dplyr::filter(AGE >= 15, AGE <= 24) |>
  dplyr::group_by(DEGURBA_L1, SEX) |>
  summarise(
    total.youth.15_24 = n(),
    total.neet = sum(Employ == 2, na.rm = TRUE),
    rate.neet = round(total.neet / total.youth.15_24, 2)
  ) |>
  dplyr::mutate(SEX = forcats::as_factor(SEX)) |>
  tidyr::pivot_wider(names_from = SEX, values_from = 3:5)

youth_umemplyment_L2 <-
  demographics_gps |>
  dplyr::mutate(
    Employ = case_when(
      ACTIVITY_LAST_12_MON %in% 1:6 ~ 1,
      ACTIVITY_LAST_12_MON %in% 7:9 ~ 2,
      ACTIVITY_LAST_12_MON == 10 ~ 3,
      T ~ 2
    )
  ) |>
  dplyr::filter(AGE >= 15, AGE <= 24) |>
  dplyr::group_by(DEGURBA_L2, SEX) |>
  summarise(
    total.youth.15_24 = n(),
    total.neet = sum(Employ == 2, na.rm = TRUE),
    rate.neet = round(total.neet / total.youth.15_24, 2)
  ) |>
  dplyr::mutate(SEX = forcats::as_factor(SEX)) |>
  tidyr::pivot_wider(names_from = SEX, values_from = 3:5)


# iv) Merge all data frames into one

indicators_L1 <- adolescent_birth_rate_L1 |>
  dplyr::left_join(child_marriage_L1, by = c("DEGURBA_L1")) |>
  dplyr::left_join(youth_umemplyment_L1, by = c("DEGURBA_L1")) |>
  dplyr::mutate(
    DEGURBA_Label = case_when(
      DEGURBA_L1 == 1 ~ "Rural Area",
      DEGURBA_L1 == 2 ~ "Town or Semi-dense Area",
      DEGURBA_L1 == 3 ~ "City",
    )
  ) |>
  dplyr::select(DEGURBA_L1, DEGURBA_Label, everything())

indicators_L2 <- adolescent_birth_rate_L2 |>
  dplyr::left_join(child_marriage_L2, by = c("DEGURBA_L2")) |>
  dplyr::left_join(youth_umemplyment_L2, by = c("DEGURBA_L2")) |>
  dplyr::mutate(
    DEGURBA_Label = case_when(
      DEGURBA_L2 == 10 ~ "Water",
      DEGURBA_L2 == 11 ~ "Very Disperded Rural Area",
      DEGURBA_L2 == 12 ~ "Disperse Rural Area",
      DEGURBA_L2 == 13 ~ "Village",
      DEGURBA_L2 == 21 ~ "Suburban or Peri-urban Area",
      DEGURBA_L2 == 22 ~ "Semi-dense Town",
      DEGURBA_L2 == 23 ~ "Dense Town",
      DEGURBA_L2 == 30 ~ "City",
    )
  ) |>
  dplyr::select(DEGURBA_L2, DEGURBA_Label, everything())

# c) Export Indicators in Excel
excel_sheets <- list("DEGURBA_L1" = indicators_L1, "DEGURBA_L2" = indicators_L2)

writexl::write_xlsx(excel_sheets, path = "Output/Indicators_from_GPS_points_classification.xlsx")




















