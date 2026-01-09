Air Quality Analysis: Sheffield vs Hull (2023–2025)

Overview

This repository contains code for data processing, analysis, and visualisation for an exploratory air-quality study comparing Sheffield and Hull from 2023 to 2025. The project examines the temporal behaviour of key pollutants NO₂, O₃, PM₂.₅, and PM₁₀ using open-source monitoring data and reproducible data science methods.

The analysis focuses on identifying short-term variability, seasonal patterns, and longer-term trends rather than forecasting future pollution levels. It was developed as part of the IJC437 & IJC 445 (Introduction to Data Science & Data Visualisation) module and is structured to support transparency, reproducibility, and clear interpretation.


Data Sources

1. Open-source hourly air-quality data retrieved via the OpenAQ API
2. Pollutants analysed: NO₂, O₃, PM₂.₅, PM₁₀
3. Study period: 2023 – 2025
4. Cities: Sheffield (inland, traffic-dominated) and Hull (coastal, industrial)

Methods

The analysis follows an exploratory workflow:
1. Data cleaning and aggregation (hourly → daily, monthly, seasonal)
2. Exploratory Data Analysis (EDA) to assess variability and outliers
3. Locally Estimated Scatterplot Smoothing (LOESS) to reveal medium-term trends
4. Seasonal–Trend decomposition using LOESS (STL) to separate trend, seasonal, and residual components
5. Correlation analysis to examine relationships between pollutants and infer atmospheric interactions

All analysis is performed using reproducible R scripts.

Key Findings
1. Sheffield consistently shows higher NO₂ and PM₂.₅ concentrations, reflecting traffic-related emissions.
2. Hull exhibits higher PM₁₀ and slightly elevated ozone levels, consistent with industrial activity and coastal dispersion.
3. Strong seasonal patterns are present in both cities, with winter peaks in NO₂ and particulate matter and summer maxima in ozone.
4. Longer-term trends indicate declining NO₂ alongside a gradual rise in ozone, consistent with established atmospheric chemistry.