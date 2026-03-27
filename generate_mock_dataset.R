# Script to generate a mock Dataset_AH_gruppo9.csv
# Run this if you don't have the original dataset to test the code.

set.seed(42)

# Number of observations
n <- 200

# Simulate predictor variables
x1_CPU <- rnorm(n, mean = 50, sd = 15)           # CPU usage (%)
x2_HD <- runif(n, min = 10, max = 500)           # Hard Drive space (GB)
x3_proc <- rpois(n, lambda = 40)                 # Number of running processes
x4_aging <- runif(n, min = 0.5, max = 10)        # PC age (years)
x5_audio <- sample(0:1, n, replace = TRUE)       # Audio playing (0 or 1)
x6_RAM <- rnorm(n, mean = 8, sd = 4)             # RAM usage (GB)

# Simulate target variable (y_prestazSWcalc) based on some theoretical relationships
# - Higher CPU and RAM lower performance
# - Older PCs lower performance
# - SSD/HD space has mild effect
y_prestazSWcalc <- 100 - (0.5 * x1_CPU) - (1.2 * x4_aging) - (2.5 * x6_RAM) + rnorm(n, mean = 0, sd = 5)

# Ensure no impossible values
x1_CPU <- pmax(0, pmin(100, x1_CPU))
x6_RAM <- pmax(0, pmin(32, x6_RAM))
y_prestazSWcalc <- pmax(0, pmin(100, y_prestazSWcalc))

# Combine into a data frame
mock_data <- data.frame(
  x1_CPU = x1_CPU,
  x2_HD = x2_HD,
  x3_proc = x3_proc,
  x4_aging = x4_aging,
  x5_audio = x5_audio,
  x6_RAM = x6_RAM,
  y_prestazSWcalc = y_prestazSWcalc
)

# Save to CSV in the project root directory
write.csv(mock_data, "Dataset_AH_gruppo9.csv", row.names = FALSE)

message("Mock 'Dataset_AH_gruppo9.csv' has been generated successfully in the project root.")
