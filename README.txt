Air Quality Analysis: Sheffield & Hull (2023–2025)

This repository contains a shared analytical workflow for two MSc Data Science modules: IJC437 (Introduction to Data Science) and IJC445 (Data Visualisation). Both projects analyse air-quality data from Sheffield and Hull, using the same cleaned datasets and R scripts, while addressing distinct learning objectives.

The work focuses on understanding temporal patterns in NO₂, O₃, PM₂.₅, and PM₁₀ rather than on prediction. Code is intentionally reused to ensure consistency, transparency, and good analytical practice.

Modules

1. IJC437 focuses on exploratory data analysis and time-series interpretation using aggregation, Locally Estimated Scatterplot Smoothing (LOESS), Seasonal–Trend decomposition using LOESS (STL), and correlation analysis.

2. IJC445 develops a composite visualisation to explore air-quality behaviour across multiple timescales. The design is evaluated using the ASSERT framework and the Grammar of Graphics, with attention to accessibility and ethical considerations.

Data and Structure
1. Source: OpenAQ (hourly monitoring data, 2023–2025)
2. Cities: Sheffield and Hull

All analysis is performed using annotated, open-source R scripts, enabling the workflow and figures to be reproduced end-to-end.