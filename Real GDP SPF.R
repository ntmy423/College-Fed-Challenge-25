library(fredr)
library(ggplot2)
library(dplyr)
library(tibble)

# Set your FRED API key
fredr_set_key("9a1589f16ea211e13d4bdde8ac424667")

# Function to fetch historical GDP growth from FRED
fetch_fred_history <- function(series_id, start_date, end_date) {
  fredr(series_id = series_id, 
        observation_start = as.Date(start_date),
        observation_end = as.Date(end_date)) %>%
    select(date, value) %>%
    rename(GDP_Growth = value) %>%
    mutate(type = "Historical")
}

# 1. Get historical GDP growth
# Using “Real Gross Domestic Product: Percent change from preceding period, SAAR”
gdp_hist <- fetch_fred_history("A191RL1Q225SBEA", "2021-01-01", Sys.Date())

# 2. Hard-coded SPF forecasts (quarterly + annual, from your table)
spf_forecasts <- tibble(
  period = c("2025-06-30","2025-09-30","2025-12-31",
             "2026-03-31","2026-06-30",
             "2027-07-01","2028-07-01"),
  GDP_Growth = c(1.5, 0.9, 1.4,
                 1.7, 1.9,
                 2.2, 2.0),
  type = "SPF Forecast"
) %>%
  mutate(date = as.Date(period)) %>%
  select(date, GDP_Growth, type)

# 3. Combine data
combined <- bind_rows(gdp_hist, spf_forecasts)

# 4. Plot
ggplot() +
  # Historical
  geom_point(
    data = filter(combined, type == "Historical"),
    aes(x = date, y = GDP_Growth, color = "Historical"),
    size = 2
  ) +
  geom_line(
    data = filter(combined, type == "Historical"),
    aes(x = date, y = GDP_Growth, color = "Historical"),
    size = 1.2, linetype = "solid"
  ) +
  # SPF
  geom_point(
    data = filter(combined, type == "SPF Forecast"),
    aes(x = date, y = GDP_Growth, color = "SPF Forecast"),
    size = 2
  ) +
  geom_line(
    data = filter(combined, type == "SPF Forecast"),
    aes(x = date, y = GDP_Growth, color = "SPF Forecast"),
    size = 1.2, linetype = "solid"
  ) +
  labs(
    title = "US Real GDP Growth: Historical & Survey of Professional Forecasters",
    subtitle = "(Quarterly, % Change, Survey of Q3 2025)",
    x = "Date",
    y = "Real GDP Growth (%)",
    caption = "Sources: FRED (A191RL1Q225SBEA); Philadelphia Fed SPF (Q3 2025)"
  ) +
  scale_color_manual(
    values = c("Historical" = "#0C233C", "SPF Forecast" = "#E87722"),
    labels = c("Historical" = "Historical GDP Growth", "SPF Forecast" = "SPF Forecasts")
  ) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 14),
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_line(color = "gray95"),
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold"),
    axis.text = element_text(size = 16),
  )

# Save
ggsave("Plots/Real_GDP_SPF.png", width = 8, height = 5, dpi = 600)
