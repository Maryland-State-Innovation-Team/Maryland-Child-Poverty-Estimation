# Load necessary libraries
library(tidyverse)
library(sf)
library(tigris) # For fetching county shapes
library(viridis) # For accessible color scales

setwd("C:/git/Maryland-Child-Poverty-Estimation/")

# 1. Create the Data Frame (Based on your provided SAIPE stats)
poverty_data <- tibble(
  county = c("Somerset", "Baltimore City", "Dorchester", "Allegany", "Wicomico", 
             "Kent", "Caroline", "Worcester", "Garrett", "Washington", "Talbot", 
             "Prince George's", "Cecil", "Baltimore", "St. Mary's", "Montgomery", 
             "Harford", "Charles", "Anne Arundel", "Queen Anne's", "Calvert", 
             "Howard", "Carroll", "Frederick"),
  poverty_rate = c(27.0, 24.1, 23.6, 21.7, 21.0, 19.1, 16.0, 15.3, 15.0, 
                   15.0, 13.6, 13.1, 11.8, 10.2, 10.2, 9.2, 8.3, 8.1, 7.9, 
                   6.8, 6.1, 5.9, 5.4, 5.0)
)

# 2. Get Maryland County Shapes
# Note: 'cb = TRUE' gets generalized shapes (better for static maps)
md_shapes <- counties(state = "MD", cb = TRUE, class = "sf") %>%
  select(NAME, geometry) %>%
  rename(county = NAME)

# 3. Join Data
map_data <- md_shapes %>%
  left_join(poverty_data, by = "county")

# 4. Plot the Map
poverty_map <- ggplot(map_data) +
  geom_sf(aes(fill = poverty_rate), color = "white", size = 0.2) +
  scale_fill_viridis(
    option = "magma", 
    direction = -1, 
    name = "Child Poverty\nRate (%)",
    breaks = c(5, 10, 15, 20, 25),
    guide = guide_colorbar(barheight = 10)
  ) +
  theme_void() +
  labs(
    title = "Maryland Child Poverty Rates by County (2024)",
    caption = "Source: US Census SAIPE 2024"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    legend.position = "right"
  )

# Save image
ggsave("output/md_poverty_map.png", plot = poverty_map, width = 10, height = 6, dpi = 300)

library(ggplot2)
library(patchwork) # For combining plots side-by-side

# 1. Create Data for Selected Highlights
comparison_data <- tibble(
  Jurisdiction = c("Somerset", "Baltimore City", "Montgomery", "Prince George's"),
  Rate = c(27.0, 24.1, 9.2, 13.1),
  Count = c(1188, 27868, 21947, 26856),
  Type = c("Rural/High Rate", "Urban/High Rate & Count", "Suburban/High Count", "Suburban/High Count")
)

# 2. Plot A: The Rates
plot_rate <- ggplot(comparison_data, aes(x = reorder(Jurisdiction, Rate), y = Rate)) +
  geom_col(fill = "#E76F51", width = 0.7) +
  geom_text(aes(label = paste0(Rate, "%")), hjust = -0.2, fontface = "bold") +
  coord_flip() +
  ylim(0, 30) +
  theme_minimal() +
  labs(title = "Poverty Rate (%)", x = "", y = "")

# 3. Plot B: The Counts (Number of Children)
plot_count <- ggplot(comparison_data, aes(x = reorder(Jurisdiction, Rate), y = Count)) +
  geom_col(fill = "#264653", width = 0.7) +
  geom_text(aes(label = scales::comma(Count)), hjust = -0.1, fontface = "bold") +
  coord_flip() +
  ylim(0, 35000) +
  theme_minimal() +
  theme(axis.text.y = element_blank()) + # Hide Y labels on the second chart
  labs(title = "Total Children in Poverty", x = "", y = "")

# 4. Combine
final_chart <- plot_rate + plot_count + 
  plot_annotation(
    title = "The Poverty Paradox: Rate vs. Magnitude",
    subtitle = "Somerset has the highest rate, but Montgomery has 18x more children in poverty."
  )

# Save image
ggsave("output/rate_vs_count.png", plot = final_chart, width = 10, height = 5, dpi = 300)

library(ggplot2)
library(patchwork)
library(grid)

# Function to create a single stat card
create_card <- function(title, value, subtitle, bar_color) {
  ggplot() +
    # Background Box
    annotate("rect", xmin = 0, xmax = 1, ymin = 0, ymax = 1, 
             fill = bar_color, alpha = 0.1) +
    # Colored Sidebar
    annotate("rect", xmin = 0, xmax = 0.03, ymin = 0, ymax = 1, 
             fill = bar_color) +
    # Title (Label)
    annotate("text", x = 0.08, y = 0.8, label = toupper(title), 
             hjust = 0, size = 4, color = "gray40", fontface = "bold") +
    # Main Value
    annotate("text", x = 0.08, y = 0.5, label = value, 
             hjust = 0, size = 12, color = bar_color, fontface = "bold") +
    # Subtitle (Context)
    annotate("text", x = 0.08, y = 0.2, label = subtitle, 
             hjust = 0, size = 3.5, color = "gray30", fontface = "italic") +
    # Theme cleanup
    theme_void() +
    coord_cartesian(xlim = c(0, 1), ylim = c(0, 1))
}

# 1. Create the three cards
card_rate <- create_card(
  title = "Child Poverty Rate",
  value = "11.2%",
  subtitle = "Stabilized to pre-pandemic levels",
  bar_color = "#264653" # Deep Teal
)

card_count <- create_card(
  title = "Total Children",
  value = "150,961",
  subtitle = "90% CI: 139,872 - 162,050",
  bar_color = "#E76F51" # Burnt Orange
)

card_context <- create_card(
  title = "State vs. National",
  value = "-4.3 pts",
  subtitle = "MD (11.2%) is lower than US (15.5%)",
  bar_color = "#2A9D8F" # Green
)

# 2. Combine them using Patchwork
dashboard <- card_rate + card_count + card_context + 
  plot_layout(ncol = 3) +
  plot_annotation(
    theme = theme(plot.margin = margin(10, 10, 10, 10))
  )

# 3. Save
ggsave("output/stats_dashboard.png", plot = dashboard, width = 12, height = 2.5, dpi = 300)

library(ggplot2)
library(dplyr)

# 1. Create Data
trend_data <- tibble(
  Year = c(2020, 2021, 2022, 2023, 2024),
  Rate = c(11.2, 14.0, 12.1, 11.3, 11.2)
)

# 2. Create Plot
trend_plot <- ggplot(trend_data, aes(x = Year, y = Rate)) +
  # The Line and Points
  geom_line(color = "#264653", size = 1.2) +
  geom_point(color = "#E76F51", size = 4) +
  
  # The Labels (Nudged slightly up)
  geom_text(aes(label = paste0(Rate, "%")), 
            vjust = -1.5, 
            fontface = "bold", 
            color = "#264653", 
            size = 4.5) +
  
  # Styling
  scale_y_continuous(limits = c(10, 16)) + # Give room for labels
  theme_minimal() +
  labs(
    x = "", 
    y = ""
  ) +
  theme(
    panel.grid.major.x = element_blank(), # Remove vertical grid lines
    panel.grid.minor = element_blank(),
    axis.text.y = element_blank(),        # Hide Y axis text (redundant)
    axis.title.y = element_blank(),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "gray40", size = 10)
  )

# 3. Save
ggsave("output/poverty_trend_line.png", plot = trend_plot, width = 8, height = 2, dpi = 300)
