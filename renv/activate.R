# renv activation script
# This script activates renv for reproducible package management

local({

  # Check if renv is available
  if (!requireNamespace("renv", quietly = TRUE)) {
    message("Installing renv package...")
    install.packages("renv", repos = "https://cran.r-project.org")
  }

  # Restore project library from renv.lock
  renv::restore(prompt = FALSE)

})
