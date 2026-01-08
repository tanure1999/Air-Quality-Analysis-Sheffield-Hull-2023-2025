#Section 1
# ---- 1. SETUP LIBRARIES ----

# Install Packages
install.packages("lubridate")
install.packages("forecast")
install.packages("patchwork")
install.packages("corrplot")

#Run Library
library(tidyverse)
library(lubridate)
library(forecast)   # for STL + autoplot
library(corrplot)   # for nice correlation plots


# Create folders for outputs 
dir.create("C:/Users/Tanur/Documents/Sheffield MSC Data Science/INTRODUCTION TO DATA SCIENCE IJC 437/assessment/r studio/Assessment Intro to Data Science/plots", showWarnings = FALSE)
dir.create("C:/Users/Tanur/Documents/Sheffield MSC Data Science/INTRODUCTION TO DATA SCIENCE IJC 437/assessment/r studio/Assessment Intro to Data Science/plots/stl", showWarnings = FALSE)
dir.create("C:/Users/Tanur/Documents/Sheffield MSC Data Science/INTRODUCTION TO DATA SCIENCE IJC 437/assessment/r studio/Assessment Intro to Data Science/plots/correlation", showWarnings = FALSE)
dir.create("C:/Users/Tanur/Documents/Sheffield MSC Data Science/INTRODUCTION TO DATA SCIENCE IJC 437/assessment/r studio/Assessment Intro to Data Science/outputs", showWarnings = FALSE)

#------------------------------------------------------------------------

#Section 2
#Load CSV Files
# ---- 1. LOAD DATA ----

# Change paths to where your CSVs are saved
daily <- read_csv("C:/Users/Tanur/Documents/Sheffield MSC Data Science/INTRODUCTION TO DATA SCIENCE IJC 437/assessment/r studio/Assessment Intro to Data Science/data sets/to be used/air_daily_city.csv")
monthly <- read_csv("C:/Users/Tanur/Documents/Sheffield MSC Data Science/INTRODUCTION TO DATA SCIENCE IJC 437/assessment/r studio/Assessment Intro to Data Science/data sets/to be used/air_monthly_city.csv")
seasonal <- read_csv("C:/Users/Tanur/Documents/Sheffield MSC Data Science/INTRODUCTION TO DATA SCIENCE IJC 437/assessment/r studio/Assessment Intro to Data Science/data sets/to be used/air_qaulity_seasonal_2023-2025.csv")

view(monthly)
# Quick checks
glimpse(daily)
glimpse(monthly)
glimpse(seasonal)

#----------------------------------------------------------------------------

#Section 3
#Clean & Standardise Date Columns
#this is done as a double check
#---- 1. Daily: ISO dates to Date-----

daily <- daily %>%
  mutate(
    date = mdy(date),
    city = factor(city),
    location_name = factor(location_name),
    year = year(date)   # recompute to be safe
  )

glimpse(daily)
# Daily data: date is already "YYYY-MM-DD"


#---- 2. Monthly: month string "2023-01-01T00:00:00Z" to Date-----
# Monthly data: parse month string properly
monthly <- monthly %>%
  mutate(
    date = as.Date(month),
    city = factor(city),
    year = year(month)   # recompute to be safe
  )

glimpse(monthly)
# Monthly data: date is already "YYYY-MM-DD"


#---- 3. Seasonal dataset: mm/dd/yyyy to Date-----

seasonal <- seasonal %>%
  mutate(
    date = mdy(date),
    city = factor(city),
    location_name = factor(location_name),
    year = year(date)   # recompute to be safe
  )

glimpse(seasonal)


#---------------------------------------------------------------------------
#Section 4
#Change format of files from wide to long format
#files are in wide format (one column per pollutant).
#For faceting and modelling, it’s easier to use long format.

# ---- 1. DAILY LONG FORMAT ----

daily_long <- daily %>%
  pivot_longer(
    cols = c(no2, o3, pm10, pm25),
    names_to = "pollutant",
    values_to = "value"
  ) %>%
  mutate(
    pollutant = factor(pollutant, levels = c("no2", "pm25", "pm10", "o3"))
  )

glimpse(daily_long)
write_csv(daily_long, "data sets/to be used/daily_long_format.csv")


# ---- 2. MONTHLY LONG FORMAT ----

monthly_long <- monthly %>%
  pivot_longer(
    cols = c(no2, o3, pm10, pm25),
    names_to = "pollutant",
    values_to = "monthly_mean"
  ) %>%
  mutate(
    pollutant = factor(pollutant, levels = c("no2", "pm25", "pm10", "o3"))
  )

glimpse(monthly_long)
write_csv(monthly_long, "data sets/to be used/daily_long_format.csv")

# ---- 3. SEASONAL LONG FORMAT ----

seasonal_long <- seasonal %>%
  pivot_longer(
    cols = c(no2, o3, pm10, pm25),
    names_to = "pollutant",
    values_to = "value"
  ) %>%
  mutate(
    pollutant = factor(pollutant, levels = c("no2", "pm25", "pm10", "o3"))
  )

glimpse(seasonal_long)

#seasonal_long <- seasonal_long %>%
  mutate(
    date = mdy(date),
    city = factor(city),
    location_name = factor(location_name),
    year = year(date)   # recompute to be safe
  )
glimpse(seasonal_long)

#----------------------------------------------------------------------------------

#Section 5
#Descriptive Statistics (for tables)

# ---- 1. Descriptive Statistics ----

daily_stats <- daily_long %>%
  group_by(city, pollutant) %>%
  summarise(
    n = n(),
    mean = mean(value, na.rm = TRUE),
    median = median(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    iqr = IQR(value, na.rm = TRUE),
    .groups = "drop"
  )

daily_stats
write_csv(daily_stats, "C:/Users/Tanur/Documents/Sheffield MSC Data Science/INTRODUCTION TO DATA SCIENCE IJC 437/assessment/r studio/Assessment Intro to Data Science/outputs/daily_descriptive_stats.csv")

#-----------------------------------------------------------------------------------


#----------------------------------------------------------------------------
#SECTION 6
#Seasonal Grouping (FOR EDA)
#
# ---- 1. SEASONAL AVERAGES FROM DAILY ----
p_seasonal_daily <- seasonal_long %>%
  ggplot(aes(x = date, y = value, colour = city)) +
  geom_point(alpha = 0.25, size = 1) +
  geom_smooth(method = "loess", se = FALSE, linewidth = 1) +
  facet_wrap(~ pollutant, scales = "free_y") +
  scale_x_date(date_breaks = "3 months", date_labels = "%b\n%Y") +
  labs(
    title = "seasonal pollutant concentrations (Smoothed)",
    x = "Date",
    y = "Concentration (µg/m³)",
    colour = "City"
  ) +
  theme_minimal()

p_seasonal_daily

ggsave("C:/Users/Tanur/Documents/Sheffield MSC Data Science/INTRODUCTION TO DATA SCIENCE IJC 437/assessment/r studio/Assessment Intro to Data Science/plots/seasonal_daily_pollutants_smoothed.png",
       p_seasonal_daily, width = 11, height = 7, dpi = 300)


# ---- 2. SEASONAL AVERAGES FOR MONTHLY 2025 ONLY ----

p_seasonal_2025 <- seasonal_long %>%
  filter(year(date) == 2025) %>%            # <-- Filter for 2025 only
  ggplot(aes(x = date, y = value, colour = city)) +
  geom_point(alpha = 0.25, size = 1) +
  geom_smooth(method = "loess", se = FALSE, linewidth = 1) +
  facet_wrap(~ pollutant, scales = "free_y") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b\n%Y") +
  labs(
    title = "Seasonal pollutant concentrations for 2025 (Smoothed)",
    x = "Date",
    y = "Concentration (µg/m³)",
    colour = "City"
  ) +
  theme_minimal()

p_seasonal_2025

ggsave(
  "C:/Users/Tanur/Documents/Sheffield MSC Data Science/INTRODUCTION TO DATA SCIENCE IJC 437/assessment/r studio/Assessment Intro to Data Science/plots/seasonal_daily_pollutants_2025_smoothed.png",
  p_seasonal_2025,
  width = 11,
  height = 7,
  dpi = 300
)

# ---- 3. SEASONAL AVERAGES FROM MONTHLY ----

monthly_seasonal <- monthly_long %>%
  mutate(
    season = case_when(
      month(month) %in% c(12, 1, 2) ~ "Winter",
      month(month) %in% c(3, 4, 5)  ~ "Spring",
      month(month) %in% c(6, 7, 8)  ~ "Summer",
      TRUE                          ~ "Autumn"
    ),
    season = factor(season,
                    levels = c("Winter", "Spring", "Summer", "Autumn"))
  ) %>%
  group_by(city, pollutant, season, year) %>%
  summarise(
    season_mean = mean(monthly_mean, na.rm = TRUE),
    .groups = "drop"
  )

glimpse(monthly_seasonal)
write_csv(monthly_seasonal, "C:/Users/Tanur/Documents/Sheffield MSC Data Science/INTRODUCTION TO DATA SCIENCE IJC 437/assessment/r studio/Assessment Intro to Data Science/data sets/to be used/seasonal_means_from_monthly.csv")

#plot
p_seasonal <- monthly_seasonal %>%
  ggplot(aes(x = season, y = season_mean, fill = city)) +
  geom_col(position = "dodge") +
  facet_wrap(~ pollutant, scales = "free_y") +
  labs(
    title = "Seasonal average pollutant concentrations (from monthly data)",
    x = "Season",
    y = "Mean concentration (µg/m³)",
    fill = "City"
  ) +
  theme_minimal()

p_seasonal

ggsave("C:/Users/Tanur/Documents/Sheffield MSC Data Science/INTRODUCTION TO DATA SCIENCE IJC 437/assessment/r studio/Assessment Intro to Data Science/plots/seasonal_means_by_city.png",
       p_seasonal, width = 10, height = 6, dpi = 300)


# ---- 4. SEASONAL AVERAGES For MONTHLY 2025 only ----

monthly_seasonal_2025 <- monthly_long %>%
  filter(year == 2025) %>%                       # <-- Keep only 2025
  mutate(
    season = case_when(
      month(month) %in% c(12, 1, 2) ~ "Winter",
      month(month) %in% c(3, 4, 5)  ~ "Spring",
      month(month) %in% c(6, 7, 8)  ~ "Summer",
      TRUE                          ~ "Autumn"
    ),
    season = factor(season,
                    levels = c("Winter", "Spring", "Summer", "Autumn"))
  ) %>%
  group_by(city, pollutant, season) %>%          # year removed because all = 2025
  summarise(
    season_mean = mean(monthly_mean, na.rm = TRUE),
    .groups = "drop"
  )



glimpse(monthly_seasonal_2025)
write_csv(monthly_seasonal, "C:/Users/Tanur/Documents/Sheffield MSC Data Science/INTRODUCTION TO DATA SCIENCE IJC 437/assessment/r studio/Assessment Intro to Data Science/data sets/to be used/seasonal_means_for_monthly_2025.csv")


#plot
p_seasonal_month_2025 <- monthly_seasonal_2025 %>%
  ggplot(aes(x = season, y = season_mean, fill = city)) +
  geom_col(position = "dodge") +
  facet_wrap(~ pollutant, scales = "free_y") +
  labs(
    title = "Seasonal average pollutant concentrations 2025 (from monthly data)",
    x = "Season",
    y = "Mean concentration (µg/m³)",
    fill = "City"
  ) +
  theme_minimal()

p_seasonal_month_2025

ggsave("C:/Users/Tanur/Documents/Sheffield MSC Data Science/INTRODUCTION TO DATA SCIENCE IJC 437/assessment/r studio/Assessment Intro to Data Science/plots/seasonal_means_by_city_2025.png",
       p_seasonal, width = 10, height = 6, dpi = 300)







#----------------------------------------------
#Section 7
# Time-Series with LOESS (for EDA)

# ---- 1. DAILY TIME SERIES + LOESS ----

p_daily_loess <- ggplot(daily_long,
                        aes(x = date, y = value, colour = city)) +
  geom_point(alpha = 0.2, size = 0.4) +
  geom_smooth(method = "loess", se = FALSE) +
  facet_wrap(~ pollutant, scales = "free_y") +
  labs(
    title = "Daily pollutant concentrations (LOESS-smoothed)",
    x = "Date",
    y = "Concentration (µg/m³)",
    colour = "City"
  ) +
  theme_minimal()

p_daily_loess

ggsave("C:/Users/Tanur/Documents/Sheffield MSC Data Science/INTRODUCTION TO DATA SCIENCE IJC 437/assessment/r studio/Assessment Intro to Data Science/plots/daily_LOESS_sheffield_vs_hull.png",
       p_daily_loess, width = 10, height = 6, dpi = 300)





p_seasonal_loess <- ggplot(seasonal_long,
                        aes(x = date, y = value, colour = city)) +
  geom_point(alpha = 0.2, size = 0.4) +
  geom_smooth(method = "loess", se = FALSE) +
  facet_wrap(~ pollutant, scales = "free_y") +
  labs(
    title = "Daily pollutant concentrations (LOESS-smoothed)",
    x = "Date",
    y = "Concentration (µg/m³)",
    colour = "City"
  ) +
  theme_minimal()

p_seasonal_loess

ggsave("C:/Users/Tanur/Documents/Sheffield MSC Data Science/INTRODUCTION TO DATA SCIENCE IJC 437/assessment/r studio/Assessment Intro to Data Science/plots/try.png",
       p_daily_loess, width = 10, height = 6, dpi = 300)




# Scatter plot only
p_seasonal_scatter <- ggplot(seasonal_long,
                             aes(x = date, y = value, colour = city)) +
  geom_point(alpha = 0.4, size = 0.7) +      # scatter plot
  facet_wrap(~ pollutant, scales = "free_y") +
  labs(
    title = "Daily Pollutant Concentrations (Scatter Plot)",
    x = "Date",
    y = "Concentration (µg/m³)",
    colour = "City"
  ) +
  theme_minimal()

p_seasonal_scatter

# Save the scatter plot
ggsave("C:/Users/Tanur/Documents/Sheffield MSC Data Science/INTRODUCTION TO DATA SCIENCE IJC 437/assessment/r studio/Assessment Intro to Data Science/plots/scatter_only.png",
       p_seasonal_scatter, width = 10, height = 6, dpi = 300)


#----------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------

#Section 8
#STL Seasonal Decomposition
#STL Seasonal Decomposition Using "air_monthly_city.csv" (for EDA)
#Note
#- use monthly_long, one city + pollutant at a time.
#- automatically detect the correct start year & month per city (important because Sheffield starts from October 2023).

# ---- 1. STL SEASONAL DECOMPOSITION ----

run_stl_for_series <- function(df, city_name, pollutant_name) {
  
  sub <- df %>%
    filter(city == city_name,
           pollutant == pollutant_name) %>%
    arrange(month)
  
  if (nrow(sub) == 0) {
    warning("No data for ", city_name, " - ", pollutant_name)
    return(NULL)
  }
  
  # Determine start year & month from actual data
  start_year <- year(min(sub$month))
  start_month <- month(min(sub$month))
  
  ts_data <- ts(
    sub$monthly_mean,
    start = c(start_year, start_month),
    frequency = 12
  )
  
  stl_fit <- stl(ts_data, s.window = "periodic")
  
  # Plot base STL output
  plot(
    stl_fit,
    main = paste("STL Decomposition:", city_name, toupper(pollutant_name))
  )
  
  invisible(stl_fit)
}


# ---- 2. Run STL for all pollutants & cities ----

cities <- levels(monthly_long$city)
pollutants <- levels(monthly_long$pollutant)

# All STL plots into one PDF
pdf("C:/Users/Tanur/Documents/Sheffield MSC Data Science/INTRODUCTION TO DATA SCIENCE IJC 437/assessment/r studio/Assessment Intro to Data Science/plots/stl/all_stl_plots.pdf", width = 8, height = 6)
for (c in cities) {
  for (p in pollutants) {
    message("Running STL for: ", c, " - ", p)
    run_stl_for_series(monthly_long, c, p)
  }
}
dev.off()

# Separate PNGs (Images files)
for (c in cities) {
  for (p in pollutants) {
    png_filename <- paste0(
      "C:/Users/Tanur/Documents/Sheffield MSC Data Science/INTRODUCTION TO DATA SCIENCE IJC 437/assessment/r studio/Assessment Intro to Data Science/plots/stl/stl_",
      tolower(c), "_", p, ".png"
    )
    png(png_filename, width = 800, height = 600)
    run_stl_for_series(monthly_long, c, p)
    dev.off()
  }
}


#-------------------------------------------------------------------------------------------------------------------------------

#Section 9
#Correlation Matrix Using Daily Data (for EDA)

# ---- 7. CORRELATION MATRIX ----

plot_city_cor_matrix <- function(df, city_name) {
  sub <- df %>%
    filter(city == city_name) %>%
    select(no2, pm25, pm10, o3)   # order as you like
  
  cor_mat <- cor(sub, use = "pairwise.complete.obs")
  print(cor_mat)
  
  corrplot(
    cor_mat,
    method = "color",
    type = "upper",
    addCoef.col = "black",
    tl.col = "black",
    tl.srt = 45,
    title = paste("Correlation matrix -", city_name),
    mar = c(0, 0, 2, 0)
  )
  
  invisible(cor_mat)
}

# Sheffield
png("C:/Users/Tanur/Documents/Sheffield MSC Data Science/INTRODUCTION TO DATA SCIENCE IJC 437/assessment/r studio/Assessment Intro to Data Science/plots/correlation/corr_sheffield.png", width = 800, height = 600)
cor_sheffield <- plot_city_cor_matrix(daily, "Sheffield")
dev.off()

# Hull
png("C:/Users/Tanur/Documents/Sheffield MSC Data Science/INTRODUCTION TO DATA SCIENCE IJC 437/assessment/r studio/Assessment Intro to Data Science/plots/correlation/corr_hull.png", width = 800, height = 600)
cor_hull <- plot_city_cor_matrix(daily, "Hull")
dev.off()

# Save numeric matrices
write_csv(as.data.frame(cor_sheffield), "C:/Users/Tanur/Documents/Sheffield MSC Data Science/INTRODUCTION TO DATA SCIENCE IJC 437/assessment/r studio/Assessment Intro to Data Science/outputs/correlation_sheffield.csv")
write_csv(as.data.frame(cor_hull), "C:/Users/Tanur/Documents/Sheffield MSC Data Science/INTRODUCTION TO DATA SCIENCE IJC 437/assessment/r studio/Assessment Intro to Data Science/outputs/correlation_hull.csv")

#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------




