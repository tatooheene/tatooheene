# Get the factor to update the reference prices from the df_cpi dataframe
load("data/df_cpi_combined.rda")

factor_rp <- df_cpi_combined |>
  dplyr::filter(`Year from'` == 2022 & `Year to'` %in% max(`Year to'`)) |>
  dplyr::pull(`Factor'`)

# Assign update year
update_year <- as.numeric(format(Sys.Date(), "%Y")) - 1

# df_rp_medical
load("data/df_rp_prod.rda")

# Update file with new reference year if needed
df_rp_prod <- df_rp_prod |>
  dplyr::mutate(!!sym(as.character(update_year)) := `2022` * factor_rp)

# Save the dataset
usethis::use_data(df_rp_prod, overwrite = TRUE)
