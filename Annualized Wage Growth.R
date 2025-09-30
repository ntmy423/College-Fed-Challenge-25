# Plot: Wage Growth

rm(list = ls())

# Load packages
library(tidyverse)
library(scales)
library(lubridate)
library(readxl)

# Load data
Labor_Market_Data <- read_excel("~/Library/CloudStorage/OneDrive-DrexelUniversity/Fed Challenge 2025/Skeleton/Labor Market - Data.xlsx", sheet = "Wage Growth")

# Convert date column to Date format
Labor_Market_Data$observation_date <- as.Date(Labor_Market_Data$observation_date, format = "%m/%d/%y")

# Format the data
Labor_Market_Data <- Labor_Market_Data %>%
  mutate(
    observation_date = as.Date(observation_date),
    hourly_earnings = CES0500000003
  ) %>%
  arrange(observation_date)

# Calculate 1-month annualized % change
Labor_Market_Data <- Labor_Market_Data %>%
  mutate(
    change_1mo = 100 * ((hourly_earnings / lag(hourly_earnings))^12 - 1),
    change_6mo = 100 * ((hourly_earnings / lag(hourly_earnings, 6))^2 - 1)
  )

# Plot the data
ggplot(Labor_Market_Data, aes(x = observation_date)) +
  geom_col(aes(y = change_1mo), fill = "#E87722", na.rm = TRUE) +  # dark red bars
  geom_line(aes(y = change_6mo), color = "#0C233C", size = 4, na.rm = TRUE) +  # blue line
  labs(
    title = "Annualized Growth Rate of Average Hourly Earnings",
    y = "Percent",
    x = NULL,
    caption = "Source: BLS"
  ) +
  theme_minimal(base_size = 14) +
  scale_x_date(
    date_breaks = "6 months",
    date_labels = "%m/%y"
  ) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 50),
    plot.caption = element_text(size = 25),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 25),
    axis.text.y = element_text(size = 30),
    axis.title.y = element_text(size = 40),
    panel.grid.minor = element_blank()
  ) +
  annotate("text", x = as.Date("2021-10-01"), y = 9.5, label = "1-Month Change", color = "#E87722", size = 9) +
  annotate("text", x = as.Date("2023-06-01"), y = 7, label = "6-Month Change", color = "#0C233C", size = 9)

