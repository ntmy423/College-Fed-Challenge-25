#Plot: Wage Growth vs. PCE inflation

rm(list = ls())
# Load libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(readxl)

# Read data
Labor_Market_Data <- read_excel("~/Library/CloudStorage/OneDrive-DrexelUniversity/Fed Challenge 2025/Skeleton/Labor Market - Data.xlsx", sheet = "Atl Fed Wg Grw vs. Infl", col_types = c("date", "numeric", "numeric", "numeric", "skip", "skip"))

# Reshape to long format
Labor_Market_Data_long <- Labor_Market_Data %>%
  pivot_longer(
    cols = c("Atlanta Fed Wage Growth Tracker", "Wage Growth", "Core PCE"),
    names_to = "Series",
    values_to = "Value"
  )
Labor_Market_Data_long$observation_date <- as.Date(Labor_Market_Data_long$observation_date)

# Define COVID recession period (Feb 2020–Apr 2020)
recession_start <- as.Date("2020-02-01")
recession_end   <- as.Date("2020-04-30")

# Make sure your data frame is called Labor_Market_Data_long and has May 2025 as the last month

# Get May 2025 data points for annotation
annotate_points <- Labor_Market_Data_long %>%
  filter(observation_date == as.Date("2025-05-01")) %>%
  mutate(label = case_when(
    Series == "Core PCE" ~ "2.9%",
    Series == "Wage Growth" ~ "3.9%",
    Series == "Atlanta Fed Wage Growth Tracker" ~ "4.1%"
  ))

ggplot(Labor_Market_Data_long, aes(x = observation_date, y = Value, color = Series)) +
  # Shaded recession
  annotate("rect", xmin = recession_start, xmax = recession_end,
           ymin = -Inf, ymax = Inf, fill = "grey80", alpha = 0.5) +
  geom_line(size = 4) +
  # Add labels at May 2025, offset a little to the right for readability
  geom_text(
    data = annotate_points,
    aes(label = label, x = observation_date + 80), # push label slightly right
    size = 9,
    hjust = 0,
    fontface = "bold",
    show.legend = FALSE
  ) +
  labs(
    title = "Wage Growth vs. PCE Inflation",
    x = NULL,
    y = "Percent Change YoY",
    color = NULL,
    caption = "Source: BEA, BLS, Federal Reserve Bank of Atlanta; shaded area = COVID recession (NBER)"
  ) +
  theme_minimal(base_size = 14) +
  scale_color_manual(values = c(
    "Wage Growth" = "#E87722",
    "Core PCE" = "darkgreen",
    "Atlanta Fed Wage Growth Tracker" = "#0C233C"
  )) +
  geom_hline(yintercept = 2, linetype = "dashed", color = "black") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 50),
    plot.caption = element_text(size = 25),
    axis.text.x = element_text(size = 30),
    axis.text.y = element_text(size = 30),
    axis.title.y = element_text(size = 40),
    legend.position = "top",
    legend.text = element_text(size = 30),
    panel.grid.minor = element_blank()
  ) + 
  expand_limits(x = as.Date("2026-01-01"))
