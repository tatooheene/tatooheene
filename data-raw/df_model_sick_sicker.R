# Load existing .rda
e <- new.env()
load("data/data_model_output_sick_sicker.rda", envir = e)

# Building the clean dataset object
data_model_output_sick_sicker <- list(
  l_m_M_annual   = e$l_m_M_annual,
  l_m_M_monthly  = e$l_m_M_monthly,
  v_wcc_annual   = e$v_wcc_annual,
  v_wcc_monthly  = e$v_wcc_monthly,
  v_names_str    = e$v_names_str,
  l_u_annual     = e$l_u_annual,
  l_u_monthly    = e$l_u_monthly,
  l_c_annual     = e$l_c_annual,
  l_c_monthly    = e$l_c_monthly
)

# Save clean dataset into data/
usethis::use_data(
  data_model_output_sick_sicker,
  overwrite = TRUE,
  compress = "xz"
)
