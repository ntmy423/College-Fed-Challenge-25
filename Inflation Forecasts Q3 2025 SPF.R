# --------- PACKAGES ---------
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

# --------- READ ---------
path <- "C:/Users/mnguyen/Downloads/FC 2025/Inflation/Third Quarter 2025 Survey of Professional Forecasts.xlsx"
df <- read_excel(path, sheet = 1, range = "A1:I6")

# --------- TIDY ---------
long_df <- df %>%
  select(
    Quarter,
    `Headline CPI Current`,
    `Core CPI Current`,
    `Headline PCE Current`,
    `Core PCE Current`
  ) %>%
  mutate(across(-Quarter, as.numeric)) %>%
  pivot_longer(-Quarter, names_to = "Series", values_to = "Value") %>%
  mutate(Quarter = factor(Quarter, levels = unique(Quarter), ordered = TRUE))

# --------- COLORS ---------
my_colors_legend <- c(
  "Headline CPI Current" = "#d62728",
  "Core CPI Current"     = "#1f77b4",
  "Headline PCE Current" = "#ff7f0e",
  "Core PCE Current"     = "#2ca02c"
)

# --------- PLOT (GDP-style theme) ---------
p <- ggplot(long_df, aes(Quarter, Value, color = Series, group = Series)) +
  geom_line(linewidth = 1.2, na.rm = TRUE) +
  geom_point(size = 2.5, na.rm = TRUE) +
  scale_color_manual(values = my_colors_legend, drop = FALSE) +
  scale_y_continuous(
    name   = "Percent",
    labels = percent_format(scale = 1),
    breaks = seq(2, 3.5, by = 0.5),   # 2.00%, 2.25%, ...
    limits = c(2, 3.5)                 # start at 2%
  ) +
  labs(
    title    = "Median Short-Run Projections of Inflation",
    subtitle = "Third Quarter 2025 Survey of Professional Forecasts",
    x        = NULL,
    caption  = "Source: Federal Reserve Bank of Philadelphia"
  ) +
  theme(
    legend.position = "bottom",
    legend.title    = element_blank(),
    legend.text     = element_text(size = 14),
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_line(color = "gray95"),
    plot.background  = element_rect(fill = "white", color = NA),
    panel.grid.minor = element_blank(),
    plot.title       = element_text(face = "bold", size = 25),
    plot.caption = element_text(size = 12),
    axis.text        = element_text(size = 16),
    axis.title.y = element_text(size = 16),
  )

print(p)

# --------- SAVE (GDP-style save pattern) ---------
ggsave("Inflation Forecasts Q3 2025 SPF.png", p, width = 10, height = 5, dpi = 600)