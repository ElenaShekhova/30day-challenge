library(dplyr)
library(ggplot2)
#install.packages("gganimate")
library(gganimate)
library(ggimage)
library(ggtext)

bugs_data <- read.csv(file = "Day15_bugs/bugs.csv", header = TRUE) %>%
  mutate(N = row_number())

#First, we make a simple line chart to see how data looks like
anim_1 <- bugs_data %>%
  ggplot(aes(x = N, y = Total.Insects)) +
  geom_line(size = 1) +
    scale_color_viridis_d(option = "C", end = .75) +
  labs(
    title = "Decline in flying insects",
    x = "N of sample",
    y = "N of insects on windscreen"
  )
#Now we will make an animated chart and save it
plot_line <- anim_1 +
  transition_reveal(along = N)

animate(plot_line, height = 500, width = 800, fps = 50, duration = 1, #higher fps, more PC power is needed
        end_pause = 60, res = 100)
anim_save("../plot_line.gif")

#It is more reasonable to display this data with point for each individual sample

plot <- ggplot(bugs_data, aes(N, Total.Insects)) +
  geom_point(aes(group = seq_along(N)))+
  labs(title = stringr::str_wrap("Decline in flying <span style='color:#FF69B4'>bugs</span> in Denmark"),
       #subtitle = paste(stringr::str_wrap("" ),
       y = "Frequency")+
  theme_minimal() +
  theme(
    panel.border = element_blank(),
    text = element_text(size = 18, family = "Comic Sans MS"),
    legend.position = "top",
    panel.grid.major = element_blank(),  # Remove major gridlines
    panel.grid.minor = element_blank(),  # Remove minor gridlines
    axis.title.x = element_blank(),
    axis.text.x = element_text(face = "bold"),
    axis.title.y = element_blank(),
    plot.title = element_markdown(margin = margin(b = 15, t =30), face = "bold")
  )

plotA <- plot +
  transition_reveal(along = N) +
  geom_text(aes(x = min(N), y = min(Total.Insects), label = as.factor(Year)) , hjust=-1.5, vjust = -0.2, alpha = 0.2,  col = "gray", size = 20) +
  transition_states(as.factor(Year), state_length = 5)

animate(plotA, height = 500, width = 800, fps = 50, duration = 5,
        end_pause = 60, res = 100)
anim_save("plotAnew.gif")

