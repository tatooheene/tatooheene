# Download the table for price index numbers
df_cpi <- cbsodataR::cbs_get_data("83131ned") |>
  cbsodataR::cbs_add_date_column()  |>
  cbsodataR::cbs_add_label_columns() |>
  dplyr::filter(Perioden_freq == "Y",
         Bestedingscategorieen_label == "000000 Alle bestedingen") |>
  dplyr::select(Perioden_label, CPI_1)  |>
  dplyr::mutate(Perioden_label = as.numeric(as.character(Perioden_label))) |>
  dplyr::filter(Perioden_label %in% 2012:max(Perioden_label)) |>
  dplyr::mutate(to_next_yr_fct = round(lead(CPI_1) / CPI_1, 3),
         to_next_yr_per = (to_next_yr_fct - 1) * 100,
         to_last_yr_fct = round(last(CPI_1) / CPI_1,3 ),
         to_last_yr_per = (to_last_yr_fct - 1) * 100)

df_cpi_to_next_yr <- data.frame(
  Van = df_cpi[1:nrow(df_cpi)-1,"Perioden_label"],
  naar = df_cpi[2:nrow(df_cpi),"Perioden_label"],
  percentage = df_cpi[1:nrow(df_cpi)-1,"to_next_yr_per"],
  factor = df_cpi[1:nrow(df_cpi)-1,"to_next_yr_fct"])


df_cpi_to_last_yr <- data.frame(
  Van = df_cpi[1:nrow(df_cpi)-1, "Perioden_label"],
  naar = df_cpi[2:nrow(df_cpi), "Perioden_label"],
  percentage = df_cpi[1:nrow(df_cpi)-1,"to_last_yr_per"],
  factor = df_cpi[1:nrow(df_cpi)-1,"to_last_yr_fct"])

df_cpi_combined <- bind_cols(df_cpi_to_next_yr,
                             df_cpi_to_last_yr) |>
  dplyr::rename(`Year from'` = `Perioden_label...5`,
         `Year to'` = Perioden_label.1...6) |>
  dplyr::rename(`Year from` = `Perioden_label...1`,
         `Year to` = Perioden_label.1...2) |>
  dplyr::rename("Percentage" = to_next_yr_per,
         "Percentage'"= to_last_yr_per,
         "Factor" = to_next_yr_fct,
         "Factor'" = to_last_yr_fct) |>
  dplyr::mutate(`Year to'` = max(`Year to'`))

# Save the dataset
usethis::use_data(df_cpi_combined, overwrite = TRUE)
