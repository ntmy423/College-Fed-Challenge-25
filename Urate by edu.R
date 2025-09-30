# Plot: Unemployment Rate by Education

rm(list = ls())

# Load required libraries
library(readxl)
library(ggplot2)
library(tidyr)
library(dplyr)
library(lubridate)
library(scales)

# Load data
Labor_Market_Data <- read_excel("~/Library/CloudStorage/OneDrive-DrexelUniversity/Fed Challenge 2025/Skeleton/Labor Market - Data.xlsx", sheet = "Urate by education", col_types = c("date", "numeric", "numeric", "numeric", "numeric", "skip", "skip", "skip"))


# Convert date column to Date format
Labor_Market_Data$observation_date <- as.Date(Labor_Market_Data$observation_date, format = "%m/%d/%y")

# Pivot to long format
Labor_Market_Data_long <- Labor_Market_Data %>%
  pivot_longer(
    cols = -observation_date,
    names_to = "Education",
    values_to = "Unemployment_Rate"
  )

# Factor education for legend order
Labor_Market_Data_long$Education <- factor(
  Labor_Market_Data_long$Education,
  levels = c("Bachelors and Higher", "Some College or Associate Degree", "High School", "Less than High School")
)

# Define COVID recession period (Feb 2020–Apr 2020)
recession_start <- as.Date("2020-02-01")
recession_end   <- as.Date("2020-04-30")

# Plot
ggplot(Labor_Market_Data_long, aes(x = observation_date, y = Unemployment_Rate, color = Education)) +
  # Shaded recession
  annotate("rect", xmin = recession_start, xmax = recession_end,
           ymin = -Inf, ymax = Inf, fill = "grey80", alpha = 0.5) +
  geom_line(size = 4) +
  labs(title = "Unemployment Rates by Education",
       caption = "Source: BLS, shaded area = COVID recession (NBER)",
       x = NULL,
       y = "Percent",
       color = NULL) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 50, face = "bold"),
        plot.caption = element_text(size = 25),
        axis.text.x = element_text(size = 30),
        axis.text.y = element_text(size = 30),
        axis.title.y = element_text(size = 40),
        legend.text = element_text(size = 25),
        panel.grid.minor = element_blank(),
        legend.position = "top") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_color_manual(
    values = c(
      "Less than High School" = "#0C233C",
      "High School" = "red",
      "Some College or Associate Degree" = "green",
      "Bachelors and Higher" = "orange"
    ),
    labels = c(
      "Less than High School" = "Less than High School",
      "High School" = "High School",
      "Some College or Associate Degree" = "Associate Degree",
      "Bachelors and Higher" = "Bachelors and Higher"
    )
  ) + 
  expand_limits(x = as.Date("2026-01-01"))
