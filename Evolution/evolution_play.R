library(dplyr)
library(ggplot2)
library(ggtext)
library(patchwork)



evol_all <- read.csv(file = "Day15_evolution/Evolution_DataSets.csv", header = TRUE)
evol_all_short <- read.csv(file = "Day15_evolution/Homininos_DataSet (1).csv", header = TRUE)

carn <- evol_all_short %>%
  filter(Diet == "carnivorous")

unique_values <- unique(carn$Genus_._Specie)
unique_values

#make separate data frames for each location
asia <- evol_all %>%
  filter(Location == "Asia ")

africa <- evol_all %>%
  filter(Location == "Africa")

europe <- evol_all %>%
  filter(Location == "Europa")

#Asia plots
plot1<-ggplot(asia, aes(x=Time, y=Height)) +
  geom_jitter(position=position_jitter(0.2))
plot1

histogr<- ggplot(asia, aes(x = Height)) +
  geom_histogram()+
  labs(title = "Asia")
histogr

histogr_CC<- ggplot(asia, aes(x = Cranial_Capacity)) +
  geom_histogram()
histogr_CC


#Africa plots
histogr_afc<- ggplot(africa, aes(x = Height)) +
  geom_histogram()+
  labs(title = "Africa")
histogr_afc

plot_afc<-ggplot(africa, aes(x=Time, y=Height)) +
  geom_jitter(position=position_jitter(0.2))
plot_afc

#Europe plots
histogr_eur<- ggplot(europe, aes(x = Height)) +
  geom_histogram()+
  labs(title = "Europe")
histogr_eur

plot_eur<-ggplot(europe, aes(x=Time, y=Height)) +
  geom_jitter(position=position_jitter(0.2))
plot_eur

all<- histogr + histogr_afc + histogr_eur
all

ggsave("all.png", plot = all, width = 4000, height = 2400, units = "px", dpi = 300)



#

plot_foot<-ggplot(evol_all_short, aes(x=Diet, y=Foots, color = Location)) +
  geom_jitter(position=position_jitter(0.2))
plot_foot