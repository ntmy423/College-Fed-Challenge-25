# Plot: Quits and Layoffs & Discharges

rm(list = ls())

# Load packages
library(readxl)
library(ggplot2)
library(scales)
library(readxl)

# Load data
Labor_Market_Data <- read_excel("~/Library/CloudStorage/OneDrive-DrexelUniversity/Fed Challenge 2025/Skeleton/Labor Market - Data.xlsx", sheet = "Quits & Layoffs", col_types = c("date", "numeric", "numeric", "skip", "skip", "skip"))

# Convert date column to Date format
Labor_Market_Data$observation_date <- as.Date(Labor_Market_Data$observation_date, format = "%m/%d/%y")

# Plot
ggplot(Labor_Market_Data, aes(x = observation_date)) +
  geom_line(aes(y = `Quits Rate`, color = "Quits Rate"), size = 4) +
  geom_line(aes(y = `Layoffs & Discharges Rate`, color = "Layoffs & Discharges Rate"), size = 4) +
  geom_hline(yintercept = 0, color = "gray40", linetype = "dashed") +
  scale_color_manual(values = c("Quits Rate" = "#0C233C", "Layoffs & Discharges Rate" = "#E87722")) +
  scale_y_continuous( name = "Percent Change YoY",
                      limits = c(-40, 40),
                      breaks = seq(-40, 40, 10)
  ) +
  scale_x_date(name = NULL, date_breaks = "6 months", date_labels = "%m/%y") +
  labs(
    title = "Quits and Layoffs & Discharges",
    caption = "Source: BLS",
    color = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 50),
    plot.caption = element_text(size = 25),
    axis.text.x = element_text(size = 25),
    axis.text.y = element_text(size = 30),
    axis.title.y = element_text(size = 40),
    legend.position = "top",
    legend.text = element_text(size = 30),
    panel.grid.minor = element_blank()
  )  + 
  expand_limits(x = as.Date("2025-09-01"))