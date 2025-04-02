library(tidyverse)
library(stringr)
library(gapminder)
library(ggrepel)
library(scales)
library(smplot2)
library(viridis)
library(ggtext)
library(showtext) #this is the main package to use for ading my own fonts
library(sysfonts)
theme_set(theme_minimal(base_size = 24))

font_add(family = "ConcourseB", regular = "./Fonts/Concourse2Bold.ttf")

showtext_auto()

carbs <- read.csv(file = "H:/Statistics/30day-challenge/2025/Day2_Slope/share-of-dietary-energy-supply-from-carbohydrates-vs-gdp-per-capita.csv", header = TRUE) %>%
  rename("Location" = 1) %>%
  rename("share_carbohydrates" = 4) %>%
  rename("GDP" = 5) %>%
  filter(Year == 2016)

protein <- read.csv(file = "H:/Statistics/30day-challenge/2025/Day2_Slope/share-of-dietary-energy-derived-from-protein-vs-gdp-per-capita.csv", header = TRUE) %>%
  rename("Location" = 1) %>%
  rename("share_protein" = 4) %>%
  filter(Year == 2016) %>%
  dplyr::select (Location, share_protein)

fat <- read.csv(file = "H:/Statistics/30day-challenge/2025/Day2_Slope/share-of-dietary-energy-supply-from-fats-vs-gdp-per-capita.csv", header = TRUE) %>%
  rename("Location" = 1) %>%
  rename("share_fat" = 4) %>%
  filter(Year == 2016) %>%
  dplyr::select (Location, share_fat)

comb_all <- carbs %>%
  inner_join(protein, by = "Location") %>%
  inner_join(fat, by = "Location")

comb_all <- comb_all %>%
  pivot_longer(
    cols = starts_with("share_"),
    names_to = "share_type",
    values_to = "share_value"
  )

plotCerealProteinFat <- comb_all %>%
  ggplot(aes(x = GDP, y = share_value, fill = share_type)) +
  sm_statCorr(color = "blue", corr_method = "spearman", linetype = "dashed", show_text = TRUE) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  geom_point(size = 6, shape = 21, colour = "#181716", alpha = 0.5) +
  scale_fill_viridis(option = "E", discrete = TRUE) +
  scale_x_log10(labels = function(x){paste0("$",x/1000,"K")}
  )+
  labs(
    x = "GDP per capita (log scale)",
    y = "Share in diet",
    title = "Rising GDP Changes Diets:\nLess Carbs, More Protein & Fat",
    subtitle = "Higher GDP is linked to higher protein and fat consumption, but lower carbohydrate intake",
    caption = "Dietary energy supply: Food and Agriculture Organization of the United Nations (2023) with major processing by Our World in Data. Data from 2016. \nCreated by Elena Shekhova | @eshekhova.bsky.social | lumipie.com",
    fill = "Continent"
  ) +
  facet_wrap(
    vars(share_type),
    nrow = 3,
    scales = "free_y",  # Allows individual y-axis scales
    labeller = labeller(share_type = c(
      "share_carbohydrates" = "Carbohydrate",
      "share_fat" = "Fat",
      "share_protein" = "Protein"
    ))
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 20, color = "#181716", family = "ConcourseB"),
    plot.title = element_text(size = 40, hjust = 0, margin = margin(b = 35)),
    plot.subtitle = element_text(size = 20, margin = margin(b = 30)),
    axis.title.y = element_text(margin = margin(r = 25)),
    axis.title.x = element_text(margin = margin(t = 25)),
    axis.text = element_text(size = 14),
    axis.text.x = element_text(hjust = 1),
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(20, 20, 20, 20),
    plot.background = element_rect(fill = "#F1EFE6"),
    plot.caption = element_text(size = 12, hjust = 0, margin = margin(t = 25), color="grey"),
    strip.text = element_text(size = 24, hjust=0.455)
  ) +
  scale_y_continuous(labels = function(x) paste0(x, "%"))