# Executive Summary: Hardware Attributes & Software Performance

**Subject:** Statistical Analysis of System Conditions Impacting Software Calculation Performance

## 1. Objective and Scope
This analysis evaluates the extent to which distinct hardware metrics and system states influence software calculation performance (`y_prestazSWcalc`). Grounded in multivariate statistical modeling, the objective is to isolate and quantify the true drivers of computational degradation relative to background noise and non-impactful system features.

## 2. Analytical Methodology
The dataset underwent a comprehensive analytical pipeline to ensure statistical robustness and validity:
*   **Exploratory Data Analysis (EDA):** Verified feature distributions (via histograms and boxplots) and confirmed the target variable’s suitability for parametric modeling.
*   **Bivariate & Correlation Analysis:** Assessed linear and polynomial relationships while monitoring for multicollinearity across hardware specifications (e.g., CPU, RAM, and Disk Storage).
*   **Dimensionality Reduction:** Built an initial full multiple linear regression model, followed by **Stepwise Backward Elimination** to distill the model down to its most statistically significant predictors.
*   **Model Diagnostics:** Validated the final optimized model via complete residual diagnostics (Scatter, Histogram, Q-Q plots) to guarantee adherence to regression assumptions and validate confidence intervals.

## 3. Key Findings & Model Output
The finalized statistical model is highly robust, successfully accounting for approximately **86% of the variance** in software performance (Adjusted R-squared: 0.858, F-statistic p-value < 0.001).

Based on the regression coefficients, we have categorically separated the tested features into critical performance drivers and statistically insignificant non-factors.

### Critical Performance Drivers
For each incremental unit of increase in the following metrics, software performance significantly degrades:
*   **RAM Usage ($p < 0.001$):** Memory utilization acts as the primary bottleneck. For every additional gigabyte (GB) of RAM engaged, calculation performance drops by **-2.63 units**. This represents the steepest single point of failure in software execution speed.
*   **PC Age ($p < 0.001$):** Underlying hardware degradation is reliable and systemic. Performance decreases by **-1.19 units** for every year the physical hardware ages.
*   **CPU Usage ($p < 0.001$):** General processor congestion directly harms output, resulting in a **-0.52 unit** drop in performance per percentage point of utilization.

### Statistically Insignificant Factors
Crucially, Stepwise Backward Elimination proved that several widely tracked metrics have **no statistically valid relationship** with the software's calculation performance ($p > 0.05$). The following variables were safely discarded from the predictive model:
*   **Hard Drive Storage (`x2_HD`)**
*   **Number of Running Processes (`x3_proc`)**
*   **Audio Playing Status (`x5_audio`)**

## 4. Strategic Recommendations
Based on the derived statistical evidence, we recommend the following strategic optimizions for environments running this software:

1.  **Strict Memory Allocation Guardrails:** Because RAM usage presents the most severe degradation penalty (-2.63 coefficient), system environments must ensure ample minimum memory availability and violently restrict background memory leaks prior to executing intensive calculation sequences.
2.  **Enforce Hardware Lifecycle Management:** Upgrading peripheral and storage components (such as Hard Drives) provides mathematically zero uplift in performance. Instead, physical machines should be aggressively cycled out of production environments based purely on chronological age due to systemic lifespan degradation (-1.19 / year).
3.  **Deprioritize Superficial Triage:** IT support should not waste diagnostic time auditing mundane background processes or audio states when triaging slow calculations, as these features are statistically uncorrelated with software runtime efficiency.
