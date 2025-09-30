#Plot: LFPR (Aggregate and Prime-Age)

rm(list = ls())

# Load the package
library(ggplot2)
library(scales)
library(readxl)

# Load data
Labor_Market_Data <- read_excel("~/Library/CloudStorage/OneDrive-DrexelUniversity/Fed Challenge 2025/Skeleton/Labor Market - Data.xlsx", sheet = "LFPR", col_types = c("date", "numeric", "numeric", "skip", "skip", "skip", "skip", "skip", "skip"))

# Convert date column to Date format
Labor_Market_Data$observation_date <- as.Date(Labor_Market_Data$observation_date, format = "%m/%d/%y")                                                                                                        

# Define COVID recession period (Feb 2020–Apr 2020)
recession_start <- as.Date("2020-02-01")
recession_end   <- as.Date("2020-04-30")

# Rename for easier use in ggplot
names(Labor_Market_Data) <- c("date", "prime_lfpr", "lfpr")


# Get latest (May 2025) values for annotation
last_point <- Labor_Market_Data %>%
  filter(date == max(date)) %>%
  summarize(
    date = max(date),
    prime_lfpr = prime_lfpr,
    lfpr = lfpr
  )
# tweak how far to move (in percentage points on the y-axis)
offset <- 0.20
ggplot(Labor_Market_Data, aes(x = date)) +
  # Shaded recession
  annotate("rect", xmin = recession_start, xmax = recession_end,
           ymin = -Inf, ymax = Inf, fill = "grey80", alpha = 0.5) +
  geom_line(aes(y = prime_lfpr, color = "Prime Age LFPR"), size = 2) +
  geom_line(aes(y = lfpr, color = "Aggregate LFPR"), size = 2) +
  # Annotate final values at last date, nudged right for clarity
  annotate("text", 
           x = last_point$date + 40,  # nudges the label right; adjust if needed
           y = last_point$prime_lfpr + offset, 
           label = "-0.2%", 
           color = "#E87722", 
           size = 8, 
           fontface = "bold", 
           hjust = 0) +
  annotate("text", 
           x = last_point$date + 40, 
           y = last_point$lfpr - offset, 
           label = "-0.6%", 
           color = "#0C233C", 
           size = 8, 
           fontface = "bold", 
           hjust = 0) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  labs(
    title = "Labor Force Participation Rate",
    caption = "Source: BLS, shaded area = COVID recession (NBER)",
    x = NULL,
    y = "Percent Change YoY",
    color = NULL
  ) +
  scale_color_manual(values = c("Prime Age LFPR" = "#E87722", "Aggregate LFPR" = "#0C233C")) +
  scale_x_date(
    date_breaks = "1 year",   
    date_labels = "%Y"        
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
  ) + 
  expand_limits(x = as.Date("2026-01-01"))
