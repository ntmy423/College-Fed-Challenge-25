# -------------------------------
# PACKAGES
# -------------------------------
library(tidyverse)   # includes dplyr, tidyr, stringr, ggplot2, readr, etc.
library(readxl)
library(scales)

# -------------------------------
# File path & sheet
# -------------------------------
file_path  <- "C:/Users/mnguyen/Downloads/FC 2025/Inflation/U.S. import price indexes for selected categories, 08 2025.xlsx"
sheet_name <- "Sheet5"

# -------------------------------
# Load and clean
# -------------------------------
df <- read_excel(file_path, sheet = sheet_name)

df_long <- df %>%
  rename_all(~ make.names(., unique = TRUE)) %>%
  pivot_longer(
    cols = -Quarter,
    names_to = "Category",
    values_to = "Index"
  ) %>%
  mutate(
    Index    = as.numeric(Index),
    Category = str_replace_all(Category, "\\.", " "),
    Quarter  = factor(Quarter, levels = unique(Quarter), ordered = TRUE) # preserve order
  )

# See categories available
print(unique(df_long$Category))

# -------------------------------
# Define colors
# -------------------------------
color_map <- c(
  "All imports"         = "#E87722",
  "Automotive vehicles" = "#0C233C",
  "Industrial supplies" = "#3598DC"
)

# -------------------------------
# Plot 1: Selected categories
# -------------------------------
p1 <- ggplot(df_long, aes(x = Quarter, y = Index, color = Category, group = Category)) +
  geom_line(linewidth = 1.5) +
  geom_point(size = 2) +
  scale_color_manual(values = color_map, drop = FALSE) +
  labs(
    title    = "U.S. Import Price Indexes, Selected Categories",
    subtitle = "Index: Q1 2020 = 100",
    x        = NULL,
    y        = "Index",
    color    = "Category",
    caption  = "Source: BLS"
  ) +
  # ---- GDP-style theme ----
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
  axis.text.x = element_text(angle=90),
  axis.title.y = element_text(size = 16)
)

print(p1)

# -------------------------------
# Second graph: All imports vs Nominal Broad U.S. Dollar
# -------------------------------
df_long2 <- df_long %>%
  mutate(
    Category = case_when(
      Category == "Nominal Broad U S  Dollar" ~ "Nominal Broad U.S. Dollar",
      TRUE ~ Category
    )
  )

df_subset <- df_long2 %>%
  filter(Category %in% c("All imports", "Nominal Broad U.S. Dollar"))

color_map2 <- c(
  "All imports"               = "#E87722",  # orange
  "Nominal Broad U.S. Dollar" = "#2C7BB6"   # blue
)

p2 <- ggplot(df_subset, aes(x = Quarter, y = Index, color = Category, group = Category)) +
  geom_line(linewidth = 1.5) +
  geom_point(size = 2) +
  scale_color_manual(values = color_map2, drop = FALSE) +
  labs(
    title    = "U.S. Import Price and Exchange Rate Indexes",
    subtitle = "Index: Q1 2020 = 100",
    x        = NULL,
    y        = "Index",
    color    = "Category",
    caption  = "Source: BLS, FRED"
  ) +
  # ---- GDP-style theme ----
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
  axis.text.x = element_text(angle=90),
  axis.title.y = element_text(size = 16)
)

print(p2)

# -------------------------------
# SAVE (same pattern as your GDP chart)
# -------------------------------
ggsave("U.S. import price indexes for selected categories, 08 2025.png", p1, width = 10, height = 5, dpi = 600)
ggsave("U.S. import price and exchange rate indexes, 08 2025.png",   p2, width = 10, height = 5, dpi = 600)
