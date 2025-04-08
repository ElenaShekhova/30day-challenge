# Load required libraries
library(tidyverse)
library(showtext)
library(scales)

# Set theme and base size
theme_set(theme_minimal(base_size = 24))

# Add custom fonts
font_add(family = "ConcourseB", regular = "H:/Statistics/Extra-projects/Fonts/Concourse2Bold.ttf")
font_add(family = "HeliotropeI", regular = "H:/Statistics/Extra-projects/Fonts/Heliotrope6Italic.ttf")
showtext_auto()

# Load and process meat intake data
# Data source: Global Dietary Database from 2018
intake_meat <- read.csv(
  file = "H:/Statistics/Extra-projects/Intake/Country-level estimates/v09_cnty.csv",
  header = TRUE
) %>%
  filter(
    year == "2018",
    age == "999",
    edu == "999",
    urban == "999",
    female == "999"
  ) %>%
  rename(
    "Meats" = 11,
    "Code" = 2
  )

# Load income groups data
# Data source: World Bank (2024) – with major processing by Our World in Data
income_groups <- read.csv(
  "https://ourworldindata.org/grapher/world-bank-income-groups.csv?v=1&csvType=full&useColumnShortNames=true"
) %>%
  rename("Location" = 1) %>%
  filter(Year == 2018)

# Combine datasets
combined <- inner_join(income_groups, intake_meat, by = "Code")

# Define income classification levels
income_levels <- c(
  "High-income countries",
  "Upper-middle-income countries",
  "Lower-middle-income countries",
  "Low-income countries"
)

# Convert classification to factor with specified levels
combined$classification <- factor(
  combined$classification,
  levels = income_levels
)

# Identify outliers
outliers <- combined %>%
  group_by(classification) %>%
  filter(
    Meats < quantile(Meats, 0.25) - 1.5 * IQR(Meats) |
    Meats > quantile(Meats, 0.75) + 1.5 * IQR(Meats)
  )

# Create boxplot visualization
plot_meat_income <- ggplot(
  combined,
  aes(x = classification, y = Meats)
) +
  geom_boxplot(
    fill = "#A52A2A",
    color = "#181716",
    alpha = 0.7
  ) +
  geom_text(
    data = outliers,
    aes(label = Location),
    vjust = -0.55,
    size = 6,
    color = "#181716",
    family = "HeliotropeI"
  ) +
  theme_minimal() +
  theme(
    text = element_text(
      size = 25,
      color = "#181716",
      family = "ConcourseB"
    ),
    plot.title = element_text(
      size = 45,
      hjust = 0.5,
      margin = margin(b = 40)
    ),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.y = element_text(
      color = "#181716",
      margin = margin(r = 35)
    ),
    axis.title.x = element_blank(),
    axis.text.x = element_text(color = "#181716"),
    axis.text.y = element_text(color = "#181716"),
    legend.position = "none",
    plot.margin = margin(40, 20, 40, 40),
    plot.caption = element_text(
      size = 12,
      hjust = 0.5,
      margin = margin(t = 35),
      color = "grey"
    ),
    panel.border = element_blank(),
    panel.background = element_blank(),
    plot.background = element_rect(fill = "#F1EFE6")
  ) +
  scale_x_discrete(labels = label_wrap(14)) +
  labs(
    title = "Who Eats the Most Processed Meat?",
    y = "Processed meat intake (g/day)",
    caption = paste(
      "Data source: Global Dietary Database & World Bank (2018)",
      "Created by Elena Shekhova | @eshekhova.bsky.social | lumipie.com",
      sep = " | "
    )
  ) 