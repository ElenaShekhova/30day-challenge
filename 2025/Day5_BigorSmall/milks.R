library(ggplot2)
library(dplyr)
library(tidyr)
library(showtext)

theme_set(theme_minimal(base_size = 24))
font_add(family = "ConcourseB", regular = "./Fonts/Concourse2Bold.ttf")
showtext_auto()


# Data source: How animal milk and plant-based alternatives diverge in terms of fatty acid, amino acid, and mineral composition (PMID: 37717060)
# Link: https://link.springer.com/article/10.1038/s41538-023-00227-w#Tab3
# Protein, fat, and carbs presented as % of total volume, minerals are expressed in mg/kg beverage
milk_data <- data.frame(
  Beverage = c("Rice", "Soy", "Coconut", "Oat", "Almond", "Cow", "Goat"),
  Protein = c(0.12, 3.47, 0.23, 0.69, 0.85, 3.42, 3.25),
  Fat = c(0.39, 1.6, 1.73, 0.37, 1.99, 3.55, 3.72),
  Carbs = c(12.6, 1.55, 1.95, 8, 8.2, 4.9, 4.35),
  Calcium = c(125, 260, 133, 139, 214, 1067, 882),
  Potassium = c(0, 1394, 337, 323, 218, 1408, 1636),
  Sodium = c(228, 325, 367, 381, 253, 405, 878),
  Magnesium = c(0, 184, 15, 9, 65, 88, 117),
  Phosphorus = c(84, 471, 58, 138, 131, 930, 1000),
  Sulfur = c(0, 212, 0, 0, 0, 241, 234),
  Iodine = c(0, 0, 0, 0, 0, 242, 377)
)

# Reshape data to long format
milk_long <- milk_data %>%
  pivot_longer(cols = -Beverage, names_to = "Nutrient", values_to = "Value")

# Create a function to normalize values for each nutrient
normalize_values <- function(data) {
  data %>%
    group_by(Nutrient) %>%
    mutate(
      Normalized_Value = (Value - min(Value)) / (max(Value) - min(Value)),
      # Scale the normalized values to create appropriate bubble sizes
      Bubble_Size = Normalized_Value * 15 + 5,
      Color = ifelse(Value == 0, NA, ifelse(Beverage %in% c("Cow", "Goat"), "#7A4E2D", "#F1EFE6"))  # Set color to NA when value is 0
    ) %>%
    ungroup()
}

# Normalize the data
milk_normalized <- normalize_values(milk_long)

# Set factor levels for proper ordering
milk_normalized$Beverage <- factor(milk_normalized$Beverage, 
                                 levels = c("Rice", "Soy", "Coconut", "Oat", "Almond", "Cow", "Goat"))

milk_normalized$Nutrient <- factor(milk_normalized$Nutrient,
                                 levels = rev(c("Protein", "Fat", "Carbs", "Calcium", "Potassium", 
                                          "Sodium", "Magnesium", "Phosphorus", "Sulfur", "Iodine")))

# Create the bubble grid chart
buble_grid <- ggplot(milk_normalized, aes(x = Beverage, y = Nutrient)) +
  geom_point(aes(size = Bubble_Size, color = Color), alpha = 0.6, na.rm = TRUE) +
  scale_size_continuous(range = c(1, 20)) +  # Increased size range
  scale_color_identity() +
  theme_minimal() +
  theme(
    text = element_text(size = 25, color = "#F8F6F0", family = "ConcourseB"),
    plot.title = element_text(size = 40, hjust = 0.5, margin = margin(b = 40)),
    panel.grid.major = element_blank(),  # Removed grid lines
    panel.grid.minor = element_blank(),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_text(color = "#F8F6F0"),
    axis.text.y = element_text(color = "#F8F6F0", margin = margin(l = 25)),
    legend.position = "none",
    plot.margin = margin(40, 20, 40, 40),
    plot.caption = element_text(size = 12, hjust = 0.5, margin = margin(t = 25)),
    panel.border = element_blank(),
    panel.background = element_blank(),
    panel.spacing = unit(0, "lines"),
    strip.background = element_blank(),
    axis.line = element_blank(),
    plot.background = element_rect(fill = "#1C1C1C")

  ) +
  labs(
    title = "Nutrient Content in Animal Milk &\n Plant-Based Alternatives",
    caption = "Bubble sizes are scaled proportionally to the nutrient values (protein, fat, and carbs - % of total volume, minerals - mg/kg\n of beverage), with the smallest value represented by a size of 1 and the largest by a size of 20, zero values are not displayed.\n Source: PMID: 37717060 | Created by Elena Shekhova | @eshekhova.bsky.social | lumipie.com"
  ) +
  scale_x_discrete(position = "top")

buble_grid
