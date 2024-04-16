library(tidyverse)
library(patchwork)
library(stringr)

birth <- read.csv(file = "crude-birth-rate.csv", header = TRUE)%>%
  rename("Birth rate" = 4)

life <- read.csv(file = "life-expectancy.csv", header = TRUE)%>%
  rename("Life expectancy" = 4) %>%
  filter(Year > 1949)


comb <- merge(birth, life[, c("Entity", "Year", "Life expectancy")], by = c("Entity", "Year"), all.x = TRUE)

correlations <- comb %>%
  group_by(Entity) %>%
  summarize(correlation = cor(`Birth rate`, `Life expectancy`, use = "pairwise.complete.obs"))%>%
  na.omit()

colors <- c("#B8DE29FF", "#482677FF")

long_annotation <- "In most countries, a negative correlation between life expectancy and birth rate suggests that in places where people tend to live longer, they typically have fewer children. However, in eight countries, there is no correlation between changes in birth rates and life expectancy. Data from 1950 to 2021."

wrapped_annotation <- str_wrap(long_annotation, width = 60)

correl <- ggplot(correlations, aes(x = correlation, fill = correlation > -0.2)) +
  geom_histogram() +
  scale_fill_manual(values = colors, guide = FALSE) +
  labs(title = "Correlation between Life Expectancy and Birth Rate",
       x = "Correlation",
       y = "Number of Countries") +
  theme_minimal()+
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.title.x = element_text(margin = margin(b = 15, t = 10)),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  annotate("text", x = 0, y = 20, label = wrapped_annotation, size = 3.5, hjust = 0.6, vjust = -0.1)

correl
filtered_entities <- correlations %>%
  filter(correlation > -0.2)

new_dataframe <- comb %>%
  filter(Entity %in% filtered_entities$Entity) %>%
  select(Entity, `Birth rate`, `Life expectancy`)

scatter_plot <- function(data, entities) {
  p <- ggplot(data = data[data$Entity %in% entities,], aes(x = `Life expectancy`, y = `Birth rate`, color = Entity)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE, color = "darkred") +
    theme_minimal() +
    labs(title = "Life Expectancy and Birth Rate in selected countries",
         x = "Life expectancy",
         y = "Birth rate (live births/year/1,000 people)") +
    scale_color_viridis_d()  +
    facet_wrap(~ Entity, ncol = 4) +  # Facet by Entity
    theme(plot.title = element_text(margin = margin(t = 15, b = 10), size = 16, face = "bold"),
          legend.position = "none",

          axis.title.x = element_text(margin = margin(t = 10, b = 15)),
          panel.grid.major = element_blank(),  # Remove major grid lines
          panel.grid.minor = element_blank())  # Remove minor grid lines
  return(p)
}

selected_countries <- sample(unique(new_dataframe$Entity), 8)

combined_plot <- scatter_plot(new_dataframe, selected_countries)

print(combined_plot)

t <- correl + combined_plot +
  plot_layout(ncol = 1)+ plot_annotation(caption = 'Data source: Our World in Data | Created by Elena Shekhova @eshekhova')
t
ggsave("t.png", plot = t, width = 3500, height = 2000, units = "px", dpi = 300)