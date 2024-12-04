# Download new dataset with years
df_fp <- cbsodataR::cbs_get_data("80472ned") %>%
  cbsodataR::cbs_add_label_columns() %>%
  cbsodataR::cbs_add_date_column() %>%
  dplyr::select(!OntstaneVacatures_2) %>%
  dplyr::filter(Perioden_freq == "Y",
                Bedrijfskenmerken %in% c("T001081")) %>%
  dplyr::mutate(Year = lubridate::year(Perioden_Date),
                Friction_period_days = 365.25/(VervuldeVacatures_3/OpenstaandeVacatures_1) + 28,
                Friction_period_weeks = Friction_period_days/7) %>%
  dplyr::distinct(Year,
                  VervuldeVacatures_3,
                  OpenstaandeVacatures_1,
                  Friction_period_days,
                  Friction_period_weeks) %>%
  dplyr::rename("Filled vacancies" = "VervuldeVacatures_3",
                "Open vacancies" = "OpenstaandeVacatures_1",
                "Friction period in days" = "Friction_period_days",
                "Friction period in weeks" = "Friction_period_weeks") %>%
  dplyr::mutate("Friction period days average over 5 years" = zoo::rollmean(`Friction period in days`, k=5, align = "right", fill = NA),
                "Friction period weeks average over 5 years" = zoo::rollmean(`Friction period in weeks`, k=5, align="right", fill = NA))

# Save the dataset
usethis::use_data(df_fp, overwrite = TRUE)
