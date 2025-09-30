# Plot: Nonfarm Payroll Gains by sector

rm(list = ls())

library(readxl)
library(ggplot2)
library(tidyr)
library(dplyr)
library(lubridate)

# Load data
Labor_Market_Data <- read_excel("~/Library/CloudStorage/OneDrive-DrexelUniversity/Fed Challenge 2025/Skeleton/Labor Market - Data.xlsx", sheet = "NPG - sectors", col_types = c("date", "numeric", "numeric", "numeric", "skip", "skip", "skip"))

# Ensure date column is Date
Labor_Market_Data$observation_date <- as.Date(Labor_Market_Data$observation_date)

# Reshape
Labor_Market_Data_long <- Labor_Market_Data %>%
  pivot_longer(cols = c("Total", "Manufacturing", "Transportation & Warehousing"),
               names_to = "Sector",
               values_to = "YoY_Change") %>%
  mutate(Sector = recode(Sector,
                         "Total" = "Total Nonfarm Payrolls",
                         "Manufacturing" = "Manufacturing",
                         "Transportation & Warehousing" = "Transportation & Warehousing"))

# Define COVID recession period (Feb 2020–Apr 2020)
recession_start <- as.Date("2020-02-01")
recession_end   <- as.Date("2020-04-30")

# Plot
ggplot(Labor_Market_Data_long, aes(x = observation_date, y = YoY_Change, color = Sector)) +
  # Shaded recession
  annotate("rect", xmin = recession_start, xmax = recession_end,
           ymin = -Inf, ymax = Inf, fill = "grey80", alpha = 0.5) +
  # Data
  geom_line(size = 4) +
  # Horizontal line at 0
  geom_hline(yintercept = 0, color = "black", linetype = "solid") +
  labs(title = "Nonfarm Payroll Gains by Sector",
       caption = "Source: BLS, shaded area = COVID recession (NBER)",
       x = NULL,
       y = "Percent Change YoY",
       color = NULL) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 50),
        plot.caption = element_text(size = 25),
        axis.text.x = element_text(size = 25),
        axis.text.y = element_text(size = 30),
        axis.title.y = element_text(size = 40),
        legend.position = "top",
        legend.text = element_text(size = 30),
        panel.grid.minor = element_blank()) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_color_manual(values = c(
    "Total Nonfarm Payrolls" = "#0C233C",
    "Manufacturing" = "#56B4E9",
    "Transportation & Warehousing" = "#E87722"
  )) + 
  expand_limits(x = as.Date("2026-01-01"))
