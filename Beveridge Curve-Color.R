# Plot: Beveridge Curve - Color

rm(list = ls())

library(ggplot2)
library(dplyr)
library(lubridate)
library(readxl)
library(scales)

# Load data
Labor_Market_Data <- read_excel("~/Library/CloudStorage/OneDrive-DrexelUniversity/Fed Challenge 2025/Skeleton/Labor Market - Data.xlsx", sheet = "Beveridge Curve")

# Convert date column to Date format
Labor_Market_Data$observation_date <- as.Date(Labor_Market_Data$observation_date, format = "%m/%d/%y")

# Identify the most recent data point
latest_point <- Labor_Market_Data[which.max(Labor_Market_Data$observation_date), ]

# Categorize periods for color grouping
Labor_Market_Data <- Labor_Market_Data %>%
  mutate(period = case_when(
    observation_date <= as.Date("2020-03-01") ~ "Dec 2000 to Mar 2020",
    observation_date > as.Date("2020-03-01") & observation_date <= as.Date("2022-04-01") ~ "Apr 2020 to Apr 2022",
    observation_date >= as.Date("2022-05-01") ~ "May 2022 to Present"
  ))

# Plot
ggplot(Labor_Market_Data, aes(x = URATE, y = Vacancy, color = period)) +
  geom_point(size = 5) +
  scale_color_manual(
    values = c(
      "Dec 2000 to Mar 2020" = "#0C233C",
      "Apr 2020 to Apr 2022" = "#E87722",
      "May 2022 to Present" = "#229954"
    )
  ) +
  geom_point(data = latest_point, aes(x = URATE, y = Vacancy), shape = 21, color = "red", size = 8, stroke = 4) +
  scale_x_continuous(name = "Unemployment Rate", labels = percent_format(scale = 1)) +
  scale_y_continuous(name = "Job Vacancy Rate", labels = percent_format(scale = 1)) +
  labs(
    title = "Beveridge Curve",
    color = NULL,
    caption = "Source: BLS"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 60),
    plot.caption = element_text(size = 25),
    axis.text.x = element_text(size = 30),
    axis.text.y = element_text(size = 30),
    axis.title.x = element_text(size = 30),
    axis.title.y = element_text(size = 30),
    legend.position = "top",
    legend.text = element_text(size = 30)
  )
