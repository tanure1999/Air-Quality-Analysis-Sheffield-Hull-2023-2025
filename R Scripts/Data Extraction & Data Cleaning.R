#DATA EXTRACTION
#Data Cleaning
#Data Transformation

# ------------------------------------------------------
#1. install packages (to run excel exl files)
# ------------------------------------------------------
install.packages("writeexl")
install.packages("openxlsx")

# ------------------------------------------------------
#2. load packages
# ------------------------------------------------------


library(tidyverse)
library(tidyr)
library(dplyr)
library(purrr)
library(writexl)
library(ggplot2)
library(lubridate)
library(readr)


#PART 1: DATASET CLEANING
# ------------------------------------------------------
#.Sheffield air quality dataset 2023
# ------------------------------------------------------

sheff_2023 <- read_csv("C:/Users/Tanur/Documents/Sheffield MSC Data Science/INTRODUCTION TO DATA SCIENCE IJC 437/assessment/r studio/Assessment Intro to Data Science/data sets/sheff air qaulity/sheff 2023.csv")
#View(sheff_2023)


# ------------------------------------------------------
#1. Pivot parameters into columns
# ------------------------------------------------------

sheff_2023_wide <- sheff_2023 %>%
  mutate(datetimeLocal = ymd_hms(datetimeLocal, quiet = TRUE)) %>%   # fix datetime
  select(location_name, datetimeLocal, parameter, unit, value) %>%    # keep useful cols
  group_by(location_name, datetimeLocal, parameter, unit) %>%         # remove duplicates
  summarise(value = mean(as.numeric(value), na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from  = parameter,     # becomes column names: no2, o3, pm10, pm25, etc.
    values_from = value
  ) %>%
  arrange(location_name, datetimeLocal)

#View(sheff_2023_wide)

#Correct the wrong dates
sheff_2023_wide <- sheff_2023_wide %>%
  mutate(
    datetimeLocal = update(datetimeLocal, year = 2023)
  )
#view(sheff_2023_wide)

# ------------------------------------------------------
#2.save new dataset
# ------------------------------------------------------
write_csv(sheff_2023_wide, "C:/Users/Tanur/Documents/Sheffield MSC Data Science/INTRODUCTION TO DATA SCIENCE IJC 437/assessment/r studio/Assessment Intro to Data Science/data sets/sheff air qaulity/sheff_2023_wide.csv")





# ----------------------------------------------------
# ----------------------------------------------------
# ----------------------------------------------------
# ----------------------------------------------------



# ------------------------------------------------------
#.Sheffield air quality dataset 2024
# ------------------------------------------------------

sheff_2024 <- read_csv("C:/Users/Tanur/Documents/Sheffield MSC Data Science/INTRODUCTION TO DATA SCIENCE IJC 437/assessment/r studio/Assessment Intro to Data Science/data sets/sheff air qaulity/sheff 2024.csv")
#View(sheff_2024)

# ------------------------------------------------------
#1. pivot parameters into columns
# ------------------------------------------------------

sheff_2024_wide <- sheff_2024 %>%
  mutate(datetimeLocal = ymd_hms(datetimeLocal, quiet = TRUE)) %>%   # fix datetime
  select(location_name, datetimeLocal, parameter, unit, value) %>%    # keep useful cols
  group_by(location_name, datetimeLocal, parameter, unit) %>%         # remove duplicates
  summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from  = parameter,     # becomes column names: no2, o3, pm10, pm25, etc.
    values_from = value
  ) %>%
  arrange(location_name, datetimeLocal)

#View(sheff_2024_wide)

#Correct the wrong dates
sheff_2024_wide <- sheff_2024_wide %>%
  mutate(
    datetimeLocal = update(datetimeLocal, year = 2024)
  )
#view(sheff_2024_wide)


# ------------------------------------------------------
#2.save new dataset
# ------------------------------------------------------
write_csv(sheff_2024_wide, "C:/Users/Tanur/Documents/Sheffield MSC Data Science/INTRODUCTION TO DATA SCIENCE IJC 437/assessment/r studio/Assessment Intro to Data Science/data sets/sheff air qaulity/sheff_2024_wide.csv")




# ------------------------------------------------------
#.Sheffield air quality dataset 2025
# ------------------------------------------------------

sheff_2025 <- read_csv("C:/Users/Tanur/Documents/Sheffield MSC Data Science/INTRODUCTION TO DATA SCIENCE IJC 437/assessment/r studio/Assessment Intro to Data Science/data sets/sheff air qaulity/sheff 2025.csv")
#View(sheff_2025)


# ------------------------------------------------------
#1. Pivot parameters into columns
# ------------------------------------------------------

# a. Parse datetimes by trying several common formats
safe_parse_dt <- function(x) {
  coalesce(
    suppressWarnings(ymd_hms(x, quiet = TRUE)),
    suppressWarnings(ymd_hm(x,  quiet = TRUE)),
    suppressWarnings(ymd(x,     quiet = TRUE)),
    suppressWarnings(dmy_hms(x, quiet = TRUE)),
    suppressWarnings(dmy_hm(x,  quiet = TRUE)),
    suppressWarnings(dmy(x,     quiet = TRUE)),
    suppressWarnings(mdy_hms(x, quiet = TRUE)),
    suppressWarnings(mdy_hm(x,  quiet = TRUE)),
    suppressWarnings(mdy(x,     quiet = TRUE))
  )
}

# b. Quick check of raw values
sheff_2025 %>% distinct(datetimeLocal) %>% slice_head(n = 20)

#  c. Parse datetime safely
sheff_2025_parsed <- sheff_2025 %>%
  mutate(
    datetime_ok = safe_parse_dt(datetimeLocal)
  )

# d. Check how many parsed successfully 
sheff_2025_parsed %>% summarise(total = n(), parsed = sum(!is.na(datetime_ok)), distinct_times = n_distinct(datetime_ok))

# e. Build wide table (drop unparsed rows; average duplicates)
sheff_2025_wide <- sheff_2025_parsed %>%
  transmute(
    location_name,
    datetimeLocal = datetime_ok,
    parameter,
    unit,
    value = suppressWarnings(as.numeric(value))
  ) %>%
  filter(!is.na(datetimeLocal)) %>%
  group_by(location_name, datetimeLocal, parameter) %>%   # don't group by unit; it can vary by parameter
  summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from  = parameter,
    values_from = value
  ) %>%
  arrange(location_name, datetimeLocal)

# f. Final check
sheff_2025_wide %>%
  summarise(rows = n(), distinct_times = n_distinct(datetimeLocal))

#view(sheff_2025_wide)


# ------------------------------------------------------
#2.save new dataset
# ------------------------------------------------------
write_csv(sheff_2025_wide, "C:/Users/Tanur/Documents/Sheffield MSC Data Science/INTRODUCTION TO DATA SCIENCE IJC 437/assessment/r studio/Assessment Intro to Data Science/data sets/sheff air qaulity/sheff_2025_wide.csv")




# ----------------------------------------------------
# ----------------------------------------------------
# ----------------------------------------------------
# ----------------------------------------------------




# ------------------------------------------------------
#.Hull air quality dataset 2023
# ------------------------------------------------------

hull_2023 <- read_csv("C:/Users/Tanur/Documents/Sheffield MSC Data Science/INTRODUCTION TO DATA SCIENCE IJC 437/assessment/r studio/Assessment Intro to Data Science/data sets/hull dataset/hull_2023.csv")
#View(hull_2023)


# ------------------------------------------------------
#1. pivot parameters into columns
# ------------------------------------------------------

hull_2023_wide <- hull_2023 %>%
  mutate(datetimeLocal = ymd_hms(datetimeLocal, quiet = TRUE)) %>%   # fix datetime
  select(location_name, datetimeLocal, parameter, unit, value) %>%    # keep useful cols
  group_by(location_name, datetimeLocal, parameter, unit) %>%         # remove duplicates
  summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from  = parameter,     # becomes column names: no2, o3, pm10, pm25, etc.
    values_from = value
  ) %>%
  arrange(location_name, datetimeLocal)

#View(hull_2023_wide)

#Correct the wrong dates
hull_2023_wide <- hull_2023_wide %>%
  mutate(
    datetimeLocal = update(datetimeLocal, year = 2023)
  )
#view(hull_2023_wide)

# ------------------------------------------------------
#2.save new dataset
# ------------------------------------------------------
write_csv(hull_2023_wide, "C:/Users/Tanur/Documents/Sheffield MSC Data Science/INTRODUCTION TO DATA SCIENCE IJC 437/assessment/r studio/Assessment Intro to Data Science/data sets/hull dataset/hull_2023_wide.csv")


# ----------------------------------------------------
# ----------------------------------------------------
# ----------------------------------------------------
# ----------------------------------------------------



# ------------------------------------------------------
#.Hull air quality dataset 2024
# ------------------------------------------------------

hull_2024 <- read_csv("C:/Users/Tanur/Documents/Sheffield MSC Data Science/INTRODUCTION TO DATA SCIENCE IJC 437/assessment/r studio/Assessment Intro to Data Science/data sets/hull dataset/hull_2024.csv")
#View(hull_2024)


# ------------------------------------------------------
#1. pivot parameters into columns
# ------------------------------------------------------

hull_2024_wide <- hull_2024 %>%
  mutate(datetimeLocal = ymd_hms(datetimeLocal, quiet = TRUE)) %>%   # fix datetime
  select(location_name, datetimeLocal, parameter, unit, value) %>%    # keep useful cols
  group_by(location_name, datetimeLocal, parameter, unit) %>%         # remove duplicates
  summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from  = parameter,     # becomes column names: no2, o3, pm10, pm25, etc.
    values_from = value
  ) %>%
  arrange(location_name, datetimeLocal)

#View(hull_2024_wide)

#Correct the wrong dates
hull_2024_wide <- hull_2024_wide %>%
  mutate(
    datetimeLocal = update(datetimeLocal, year = 2024)
  )
#view(hull_2024_wide)


# ------------------------------------------------------
#2.save new dataset
# ------------------------------------------------------
write_csv(hull_2024_wide, "C:/Users/Tanur/Documents/Sheffield MSC Data Science/INTRODUCTION TO DATA SCIENCE IJC 437/assessment/r studio/Assessment Intro to Data Science/data sets/hull dataset/hull_2024_wide.csv")


# ----------------------------------------------------
# ----------------------------------------------------
# ----------------------------------------------------
# ----------------------------------------------------



# ------------------------------------------------------
#.Hull air quality dataset 2025
# ------------------------------------------------------
hull_2025 <- read_csv("C:/Users/Tanur/Documents/Sheffield MSC Data Science/INTRODUCTION TO DATA SCIENCE IJC 437/assessment/r studio/Assessment Intro to Data Science/data sets/hull dataset/hull_2025.csv")
#View(hull_2025)



# ------------------------------------------------------
#1. pivot parameters into columns
# ------------------------------------------------------

# a. Helper: parse datetimes by trying several common formats
safe_parse_dt <- function(x) {
  coalesce(
    suppressWarnings(ymd_hms(x, quiet = TRUE)),
    suppressWarnings(ymd_hm(x,  quiet = TRUE)),
    suppressWarnings(ymd(x,     quiet = TRUE)),
    suppressWarnings(dmy_hms(x, quiet = TRUE)),
    suppressWarnings(dmy_hm(x,  quiet = TRUE)),
    suppressWarnings(dmy(x,     quiet = TRUE)),
    suppressWarnings(mdy_hms(x, quiet = TRUE)),
    suppressWarnings(mdy_hm(x,  quiet = TRUE)),
    suppressWarnings(mdy(x,     quiet = TRUE))
  )
}

# b. Quick check of raw values
hull_2025 %>% distinct(datetimeLocal) %>% slice_head(n = 20)

#  c. Parse datetime safely
hull_2025_parsed <- hull_2025 %>%
  mutate(
    datetime_ok = safe_parse_dt(datetimeLocal)
  )

# d. Check how many parsed successfully 
hull_2025_parsed %>% summarise(total = n(), parsed = sum(!is.na(datetime_ok)), distinct_times = n_distinct(datetime_ok))

# e. Build wide table (drop unparsed rows; average duplicates)
hull_2025_wide <- hull_2025_parsed %>%
  transmute(
    location_name,
    datetimeLocal = datetime_ok,
    parameter,
    unit,
    value = suppressWarnings(as.numeric(value))
  ) %>%
  filter(!is.na(datetimeLocal)) %>%
  group_by(location_name, datetimeLocal, parameter) %>%   # don't group by unit; it can vary by parameter
  summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from  = parameter,
    values_from = value
  ) %>%
  arrange(location_name, datetimeLocal)

# f. Final check
hull_2025_wide %>%
  summarise(rows = n(), distinct_times = n_distinct(datetimeLocal))

#view(hull_2025_wide)

# ------------------------------------------------------
#2.save new dataset
# ------------------------------------------------------
write_csv(hull_2025_wide, "C:/Users/Tanur/Documents/Sheffield MSC Data Science/INTRODUCTION TO DATA SCIENCE IJC 437/assessment/r studio/Assessment Intro to Data Science/data sets/hull dataset/hull_2025_wide.csv")



# ----------------------------------------------------
# ----------------------------------------------------
# ----------------------------------------------------
# ----------------------------------------------------



##PART 2: DATA Combination
# ------------------------------------------------------
#Sheffield Air Quality Data Combination
# ------------------------------------------------------

# ------------------------------------------------------
#1. Organize and Combine Datasets
# ------------------------------------------------------


#a. Combine by city (stack rows)
# ------------------------------------------------------
sheffield_all <- bind_rows(sheff_2023_wide, sheff_2024_wide, sheff_2025_wide)
hull_all      <- bind_rows(hull_2023_wide,  hull_2024_wide,  hull_2025_wide)

#b. Add a “city” column and merge both cities
# ------------------------------------------------------
sheffield_all <- sheffield_all %>% mutate(city = "Sheffield")
hull_all      <- hull_all %>% mutate(city = "Hull")

#c. view dataset
# ------------------------------------------------------
#view(sheffield_all)
#view(hull_all)


#d.save new dataset
# ------------------------------------------------------
write_csv(hull_all, "C:/Users/Tanur/Documents/Sheffield MSC Data Science/INTRODUCTION TO DATA SCIENCE IJC 437/assessment/r studio/Assessment Intro to Data Science/data sets/hull dataset/hull_all.csv")
write_csv(sheffield_all, "C:/Users/Tanur/Documents/Sheffield MSC Data Science/INTRODUCTION TO DATA SCIENCE IJC 437/assessment/r studio/Assessment Intro to Data Science/data sets/sheff air qaulity/sheffield_all.csv")

#e. combine hull & sheffield
all_air_qaulity<- bind_rows(sheffield_all, hull_all)

#save
write_csv(all_air_qaulity, "C:/Users/Tanur/Documents/Sheffield MSC Data Science/INTRODUCTION TO DATA SCIENCE IJC 437/assessment/r studio/Assessment Intro to Data Science/data sets/air_quality_sheffield_hull_2023_2025.csv")
#2. 

#view(all_air_qaulity)





# ----------------------------------------------------
# ----------------------------------------------------
# ----------------------------------------------------
# ----------------------------------------------------



#Part 3: Data Processing & Testing
# ------------------------------------------------------


# ------------------------------------------------------
#1. Read the combined file & create time helpers
# ------------------------------------------------------
air_all<- read.csv("C:/Users/Tanur/Documents/Sheffield MSC Data Science/INTRODUCTION TO DATA SCIENCE IJC 437/assessment/r studio/Assessment Intro to Data Science/data sets/air_quality_sheffield_hull_2023_2025.csv")
glimpse(air_all)

#.. create date, month, year columns.
# ------------------------------------------------------

air_all <- air_all %>%
  mutate(
    city = case_when(
      str_detect(location_name, "Hull") ~ "Hull",
      str_detect(location_name, "Sheffield") ~ "Sheffield",
      TRUE ~ NA_character_
    )
  )

#a. Create DAILY and MONTHLY average tables
# ------------------------------------------------------

air_daily <- air_all %>%
  group_by(city, location_name, date) %>%
  summarise(
    across(where(is.numeric), ~ mean(.x, na.rm = TRUE)),
    .groups = "drop"
  )


#b. Monthly averages per city (and station)
# ------------------------------------------------------

air_monthly <- air_all %>%
  group_by(city, location_name, month, year) %>%
  summarise(
    across(where(is.numeric), ~ mean(.x, na.rm = TRUE)),
    .groups = "drop"
  )

#c. Monthly averages city-level only
# ------------------------------------------------------

air_monthly_city <- air_monthly %>%
  group_by(city, month, year) %>%
  summarise(
    across(where(is.numeric), ~ mean(.x, na.rm = TRUE)),
    .groups = "drop"
  )


##save all
# ------------------------------------------------------
write_csv(air_daily,        "C:/Users/Tanur/Documents/Sheffield MSC Data Science/INTRODUCTION TO DATA SCIENCE IJC 437/assessment/r studio/Assessment Intro to Data Science/data sets/air_daily_city.csv")
write_csv(air_monthly_city, "C:/Users/Tanur/Documents/Sheffield MSC Data Science/INTRODUCTION TO DATA SCIENCE IJC 437/assessment/r studio/Assessment Intro to Data Science/data sets/air_monthly_city.csv")
write_csv(air_monthly, "C:/Users/Tanur/Documents/Sheffield MSC Data Science/INTRODUCTION TO DATA SCIENCE IJC 437/assessment/r studio/Assessment Intro to Data Science/data sets/air_monthly.csv")

#view(air_daily)
#view(air_monthly)
#view(all_air_qaulity)



#c. quick plot check test on the data sets to ensure it works properly
# ------------------------------------------------------

library(ggplot2)

air_monthly_city <- read_csv("C:/Users/Tanur/Documents/Sheffield MSC Data Science/INTRODUCTION TO DATA SCIENCE IJC 437/assessment/r studio/Assessment Intro to Data Science/data sets/air_monthly_city.csv")
#View(air_monthly_city)

#---Monthly NO2 trends by city, 2023–2025
# ------------------------------------------------------

scale_x_date(date_breaks = "1 month", date_labels = "%b\n%Y")

ggplot(air_monthly_city, aes(x = month, y = no2, colour = city)) +
  geom_line() +
  scale_x_date(
    date_breaks = "1 month",
    date_labels = "%b %Y"    # "Jan 2023", "Feb 2023", …
  ) +
  labs(
    title = "Monthly mean NO2 in Sheffield vs Hull (2023–2025)",
    x = "Month",
    y = "NO2 (µg/m³)",
    colour = "City"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



air_monthly_city <- air_monthly_city %>%
  mutate(month = as.Date(month))

#view(air_monthly_city)
#view(air_daily)


#------Daily NO2 trends by city, 2023-2025 (FOR CHECKING PURPOSES)
# ------------------------------------------------------

ggplot(air_daily, aes(x = date, y = no2, colour = city)) +
  geom_line(alpha = 0.7) +
  scale_x_date(
    date_breaks = "2 months",      # show tick every 2 months to avoid clutter
    date_labels = "%b %Y"          # Jan 2023, Feb 2023, …
  ) +
  labs(
    title = "Daily NO₂ in Sheffield vs Hull (2023–2025)",
    x = "Date",
    y = "NO₂ (µg/m³)",
    colour = "City"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )



# ------------------------------------------------------
# ------------------------------------------------------
# ------------------------------------------------------
#IMPORTANT NOTICE FOR NEXT STEP DATA ANALYSIS
# ------------------------------------------------------
#Load new csv files after manual cleaning with Excel
# ------------------------------------------------------
air_daily_city_2023_2024 <- read_csv("data sets/air_daily_city_2023-2024.csv") #mean average of 20th & 21st jan - dec 2023-2024
air_daily_city_2025 <- read_csv("data sets/air_daily_city_2025.csv") #mean average per day 1st to 31st, jan to dec 2025
air_monthly_city_2023_2024_2025 <- read_csv("data sets/air_monthly_city.csv") #mean average per month (jan - dec) 2023-2025
air_hourly_2023 <- read_csv("data sets/air_quality_hourly_2023.csv") #hourly parameters jan to dec 2023
air_hourly_2024 <- read_csv("data sets/air_quality_hourly_2024.csv") #hourly parameters jan to dec 2024
air_hourly_2025 <- read_csv("data sets/air_quality_hourly_2025.csv")#hourly parameters jan to dec 2025
# ------------------------------------------------------
# ------------------------------------------------------
# ------------------------------------------------------







