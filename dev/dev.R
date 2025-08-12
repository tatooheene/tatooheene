# Load and document
devtools::load_all()
devtools::document()

# Style and lint (optional but handy)
# styler::style_pkg()
# lintr::lint_package()

# Tests and checks
devtools::test()
devtools::build()
devtools::check(document = FALSE, error_on = "warning")

# Optional pre-CRAN checks
# rhub::check_for_cran()
# devtools::check_win_devel()
