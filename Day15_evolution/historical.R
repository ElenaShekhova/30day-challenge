library(dplyr)
library(ggplot2)
library(ggtext)

#Data is from Day05
historical <- read.csv(file = "Day15_evolution/Homininos_DataSet (1).csv", header = TRUE)

historical <- historical %>%
  mutate(Tooth_Enamel = ifelse(Tooth_Enamel == "thick-medium", "medium-thick", Tooth_Enamel),
         Diet = gsub("omnivore", "omnivorous", Diet))

#Arrange in a logical order
historical$Tooth_Enamel <- factor(historical$Tooth_Enamel, levels = c("very thick", "medium-thick", "thick", "thin", "medium-thin", "very thin"))
historical$Diet <- factor(historical$Diet, levels = c("carnivorous", "omnivorous", "soft fruits", "hard fruits", "dry fruits"))

#Plot with colors that are friendly for color-blind individuals

hist<- ggplot(historical, aes(x = reorder(Tooth_Enamel, -as.numeric(Tooth_Enamel)), y = reorder(Diet, -as.numeric(Diet)), color = ifelse(Tooth_Enamel %in% c("thin", "medium-thin", "very thin"), "thin", "thick"))) +
  geom_count() +
  scale_size_area(max_size = 15)+
  scale_color_manual(values = c("thin" = "#1f77b4", "thick" = "#ff7f0e")) +
  scale_x_discrete(guide = guide_axis(n.dodge=2))+ #this is so that Xlabels do not overlapp
  theme_minimal() +
  theme(
  panel.border = element_blank(),
  text = element_text(size = 30, family = "Arial"),
  legend.title = element_text(size = 20),
  legend.text = element_text(size = 20),
  #panel.grid.major = element_blank(),
 # panel.grid.minor = element_blank(),
  plot.background = element_rect(fill = "#F1EFE6"),
  axis.title.x = element_text(face = "bold", margin = margin(t = 25)),
  axis.title.y = element_blank(),
  plot.title = element_text(hjust = -0.2, margin = margin(b = 30), face = "bold"),
  plot.caption = element_text(size = 10, color = "#A9A9A9", hjust = 0.8,  margin = margin(t = 25)),
  plot.margin = margin(30, 30, 30, 30)
) +
  labs(
    title = "Diet and Tooth Enamel of our Ancestors",
    x = "Tooth Enamel",
    color = "Enamel",
    caption = "Carbonell, E. (2005). Hominidos: las primeras ocupaciones de los continentes. Editorial Ariel. Santiago Costabile | Created by Elena Shekhova"
  )

ggsave("hist.png", plot = hist, width = 4000, height = 2400, units = "px", dpi = 300)