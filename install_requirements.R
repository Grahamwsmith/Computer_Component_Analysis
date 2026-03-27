# This script ensures that all dependencies are installed before running the analysis.
options(repos = c(CRAN = "https://cloud.r-project.org"))

# Avoid "lib is not writable" error by creating a local library in the working directory
local_lib <- file.path(getwd(), ".Rlib")
if (!dir.exists(local_lib)) dir.create(local_lib)
.libPaths(c(local_lib, .libPaths()))

required_packages <- c("ggplot2", "corrplot", "car", "psych", "patchwork")

# Function to install missing packages and load them
install_and_load <- function(packages) {
  for (pkg in packages) {
    if (!require(pkg, character.only = TRUE, lib.loc = local_lib)) {
      message(paste("Installing package:", pkg))
      install.packages(pkg, dependencies = TRUE, lib = local_lib)
      library(pkg, character.only = TRUE, lib.loc = local_lib)
    } else {
      message(paste("Package", pkg, "is already installed and loaded."))
    }
  }
}

# Execution
install_and_load(required_packages)
message("All required packages are ready to use!")
