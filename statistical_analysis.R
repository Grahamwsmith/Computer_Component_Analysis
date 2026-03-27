# ==============================================================================
# APPLIED STATISTICS MINIPROJECT - HARDWARE VS SOFTWARE PERFORMANCE ANALYSIS
# ==============================================================================
# Objective: Analyze software calculation performance regarding hardware metrics.
# Target Variable (Y): y_prestazSWcalc (Software Performance)
# Predictor Variables (X): CPU, HD Storage, Processes, PC Aging, Audio, RAM
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. ENVIRONMENT SETUP & DEPENDENCY MANAGEMENT
# ------------------------------------------------------------------------------

# Set the CRAN repository mirror for package downloads
options(repos = c(CRAN = "https://cloud.r-project.org"))

# Configure a local library path (.Rlib) to prevent "lib is not writable" errors
local_lib <- file.path(getwd(), ".Rlib")
if (!dir.exists(local_lib)) {
  dir.create(local_lib)
}
.libPaths(c(local_lib, .libPaths()))

# Install and load 'pacman', an excellent tool for package management
if (!require(pacman, lib.loc = local_lib)) install.packages("pacman", lib = local_lib)

# Automatically install and load all required structural and plotting packages
pacman::p_load(ggplot2, corrplot, reshape2, patchwork, car)

# ------------------------------------------------------------------------------
# 2. DATA LOADING & INTEGRITY CHECK
# ------------------------------------------------------------------------------

dataset_file <- "Dataset_AH_gruppo9.csv"

# Check if the dataset exists locally; if not, throw an informative error
if (!file.exists(dataset_file)) {
  stop(paste("Error: The file", dataset_file, "is not in the current folder.\nRun 'generate_mock_dataset.R' first!"))
}

# Load the dataset
data <- read.csv(dataset_file)

# Perform a quick initial check on the dataset structure and basic summary statistics
str(data)
summary(data)

# ------------------------------------------------------------------------------
# 3. EXPLORATORY DATA ANALYSIS (EDA) - DISTRIBUTIONS
# ------------------------------------------------------------------------------

# Function to generate standardized combination plots (Histogram + Boxplot)
plot_distribuzione <- function(data, variable, label) {
  # Build the histogram
  p_hist <- ggplot(data, aes_string(x = variable)) +
    geom_histogram(fill = "steelblue", color = "white", bins = 20) +
    labs(title = paste("Histogram", label), x = label, y = "Frequency") +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold"))
  
  # Build the boxplot
  p_box <- ggplot(data, aes_string(y = variable)) +
    geom_boxplot(fill = "lightcoral", outlier.color = "red", outlier.size = 2) +
    labs(title = paste("Boxplot", label), y = label, x = "") +
    theme_minimal() +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
  
  # Return the combined plot using the 'patchwork' library syntax
  return(p_hist + p_box)
}

# Define the variables and their corresponding human-readable labels
vars <- c("y_prestazSWcalc", "x1_CPU", "x2_HD", "x3_proc", "x4_aging", "x5_audio", "x6_RAM")
labels <- c("SW Performance", "CPU (%)", "HD Storage (GB)", "No. Processes", "Aging (Years)", "Audio", "RAM (GB)")

# Loop through all variables and print their distribution plots
for (i in 1:length(vars)) {
  print(plot_distribuzione(data, vars[i], labels[i]))
}

# ------------------------------------------------------------------------------
# 4. BIVARIATE ANALYSIS - SCATTER PLOTS & RELATIONSHIPS
# ------------------------------------------------------------------------------

# Function to generate scatter plots with linear and polynomial regression fits
plot_correlazione <- function(data, var_x, var_y, xlab_name, ylab_name) {
  p_scatter <- ggplot(data, aes_string(x = var_x, y = var_y)) +
    geom_point(alpha = 0.6, color = "darkgray") +
    geom_smooth(method = "lm", formula = y ~ x, color = "blue", fill = "blue", alpha = 0.1) +
    geom_smooth(method = "lm", formula = y ~ poly(x, 2), color = "red", linetype = "dashed", se=FALSE) +
    labs(title = paste(ylab_name, "vs", xlab_name),
         subtitle = "Linear Regression (Blue) and Polynomial Regression (Red dashed)",
         x = xlab_name, y = ylab_name) +
    theme_minimal()
  
  # Calculate and log the correlation coefficient
  correlation <- cor(data[[var_x]], data[[var_y]])
  message(sprintf("Correlation between %s and %s: %.3f", xlab_name, ylab_name, correlation))
  
  return(p_scatter)
}

# Loop to plot relationships between target Performance (Y) and each predictor (X)
for (i in 2:length(vars)) {
  print(plot_correlazione(data, vars[i], "y_prestazSWcalc", labels[i], "Performance"))
}

# ------------------------------------------------------------------------------
# 5. GLOBAL CORRELATION MATRIX
# ------------------------------------------------------------------------------

# Calculate Pearson correlation matrix for all numerical variables
cor_matrix <- cor(data)

# Visualize the matrix to identify multicollinearity between predictors
corrplot(cor_matrix, method = "color", type = "upper", order = "hclust",
         addCoef.col = "black", tl.col = "black", tl.srt = 45, 
         diag = FALSE, title = "Correlation Matrix", mar=c(0,0,2,0))

# ------------------------------------------------------------------------------
# 6. MULTIPLE LINEAR REGRESSION & FEATURE SELECTION
# ------------------------------------------------------------------------------

# ----- Model 1: Initial Full Model -----
message("\n==== Initial Linear Model (Full) ====")
reg_lin1 <- lm(y_prestazSWcalc ~ ., data = data)
summary(reg_lin1)

# Apply Stepwise Backward Elimination to drop non-significant variables based on AIC
step_lin1 <- step(reg_lin1, direction = "backward", trace = 0)
message("\n==== Model 1 after Backward Stepwise ====")
summary(step_lin1)

# Compare the full model and the optimized Model 1 using ANOVA
anova(reg_lin1, step_lin1)

# ----- Model 2: Advanced Model with Polynomials -----
# We introduce a cubic polynomial for 'x3_proc' based on prior EDA indications
message("\n==== Model 2 (Group 9 Specific with Polynomials) ====")
reg_lin2 <- lm(y_prestazSWcalc ~ x1_CPU + poly(x3_proc, 3, raw=TRUE) + x4_aging + x6_RAM, data = data)

# Apply Stepwise Backward Elimination to Model 2
step_lin2 <- step(reg_lin2, direction = "backward", trace = 0)
summary(step_lin2)

# Compare the full polynomial model and the optimized Model 2 using ANOVA
anova(reg_lin2, step_lin2)

# ------------------------------------------------------------------------------
# 7. CONFIDENCE INTERVALS & RESIDUAL DIAGNOSTICS
# ------------------------------------------------------------------------------

# Extract 95% Confidence Intervals for both optimized models
message("\nConfidence Intervals (Optimized Model 1):"); confint(step_lin1)
message("\nConfidence Intervals (Optimized Model 2):"); confint(step_lin2)

# ----- Visualizing Residuals (Using Model 2 as the Best Fit) -----

# 1. Residuals Scatterplot against observation index
p_res1 <- ggplot(data.frame(index=1:length(step_lin2$residuals), res=step_lin2$residuals), aes(x=index, y=res)) +
  geom_point(color="blue", alpha=0.6) +
  geom_hline(yintercept=0, color="red", linetype="dashed") +
  labs(title="Residuals Scatterplot", x="Observation Index", y="Residuals") + theme_minimal()

# 2. Residuals Boxplot to check for major outliers
p_res2 <- ggplot(data.frame(res=step_lin2$residuals), aes(x="", y=res)) +
  geom_boxplot(fill="lightgreen", color="darkgreen") +
  labs(title="Residuals Boxplot", x="", y="Residuals") + theme_minimal()

# 3. Residuals Histogram to inspect normality (Gaussian distribution)
p_res3 <- ggplot(data.frame(res=step_lin2$residuals), aes(x=res)) +
  geom_histogram(fill="purple", color="white", bins=20) +
  labs(title="Residuals Histogram", x="Residual", y="Frequency") + theme_minimal()

# Combine all 3 residual plots into a single dashboard using patchwork
print((p_res1 | p_res2) / p_res3 + plot_annotation(title = 'Residuals Analysis (Model 2)'))

# Print standard base-R diagnostic plots (Q-Q plot, Scale-Location, Leverage)
par(mfrow = c(2, 2))
plot(step_lin2, main = "Diagnostic Plots (Optimized Model 2)")
par(mfrow = c(1, 1))

message("\nAnalysis completed successfully!")
