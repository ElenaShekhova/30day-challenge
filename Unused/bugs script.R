#First, we make a simple line chart to see how data looks like
anim_1 <- historical %>%
  ggplot(aes(x = Time, y = Height)) +
  geom_line(size = 1) +
  scale_color_viridis_d(option = "C", end = .75) +
  labs(
    title = "Decline in flying insects",
    x = "N of sample",
    y = "N of insects on windscreen"
  )
#Now we will make an animated chart and save it
plot_line <- anim_1 +
  transition_reveal(along = Time)

animate(plot_line, height = 500, width = 800, fps = 100, duration = 1, #higher fps, more PC power is needed
        end_pause = 60, res = 100)
anim_save("../plot_line.gif")