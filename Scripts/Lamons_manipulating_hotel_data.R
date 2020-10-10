# Modifying my Dataset
library(tidytuesdayR)
library(tidyverse)
library(outliers)
library(ggdark)

hotels <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-11/hotels.csv')
hotels


library(ggplot2)

adr_by_party <- hotels %>%
  select(adults, children, babies, adr) %>%
  mutate(party_size = (adults + children + babies)) %>%
  mutate(Number_of_Kids_per_Party = (children + babies)) %>%
  arrange(party_size) %>%
  filter(adr < 500 & adr != 0 & party_size < 10) %>%
  na.omit(adr_by_party)
  
# I also removed adr = 0 because those are useless to my analysis


boxplot.stats(adr_by_party$adr)$out #shows that values of 211.08 over are outlier
boxplot.stats(adr_by_party$party_size)$out
summary(adr_by_party$party_size)

ggplot(hotels) +
  aes(x = "", y = adr) +
  geom_boxplot(fill = "#000000") +
  theme_minimal() +
  geom_hline(aes(yintercept = 500), colour = "black")


hotels %>%
  filter(adr >= -50L & adr <= 1300L) %>%
  ggplot() +
  aes(x = "", y = adr) +
  geom_boxplot(fill = "#000000") +
  theme_minimal() +
  geom_hline(aes(yintercept = 500), colour = "black")

# these show that only outliers exist above 450



# creating a nice graph to correlate age of customer with adr

ggplot(adr_by_party) +
  aes(x = party_size, y = adr, colour = Number_of_Kids_per_Party) +
  geom_tile() +
  scale_color_distiller(palette = "Paired") +
  labs(x = "Number of People per Booking", y = "Average Daily Rate", title = "Number of People per Booking vs ADR Related to the Number of Kids per Party", caption = "Graph made by - Ethan Lamons") +
  dark_theme_bw() +
  theme(axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15, vjust = 2),
        title = element_text(size = 14),
        legend.title = element_text(size = 12)) +
  geom_abline(aes(slope = 31.60, intercept = 41.13), size = 1.5) +
  scale_x_continuous(n.break = 6) +
  annotate(geom = "segment", x = 2, y = 150, xend = 2.5, yend = 130, size = 1, colour = "black", arrow = arrow(length = unit(2, "mm"))) +
  annotate(geom = "text", x = 0.52, y = 182, label = "Linear Regression Line", hjust = "left", colour = "black") +
  annotate(geom = "text", x = 0.52, y = 167, label = "(yint = 41.13, slope = 31.60, p < 2.2e-16,", hjust = "left", colour = "black") +
  annotate(geom = "text", x = 0.52, y = 152, label = "paste(italic(R) ^ 2, \"= 0.1.911)\")", parse = TRUE, hjust = "left", colour = "black")



ggplot(adr_by_party) +
  aes(x = Number_of_Kids_per_Party , y = adr, colour = Number_of_Kids_per_Party) +
  geom_tile(show.legend = FALSE) +
  scale_color_distiller(palette = "Paired") +
  labs(x = "Number of Kids per Booking", y = "Average Daily Rate", title = "Number of Kids per Booking vs ADR", caption = "Graph made by - Ethan Lamons") +
  dark_theme_bw() +
  geom_abline(slope = 39.99, intercept = 98.99, size = 1.5) +
  scale_x_continuous(n.break = 6) +
  theme(axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15, vjust = 2),
        title = element_text(size = 20)) +
  annotate(geom = "segment", x = 0.5, y = 200, xend = 1, yend = 150, size = 1, colour = "black", arrow = arrow(length = unit(2, "mm"))) +
  annotate(geom = "text", x = -.4, y = 230, label = "Linear Regression Line", hjust = "left", colour = "black") +
  annotate(geom = "text", x = -.4, y = 215, label = "(yint = 98.99, slope = 39.99, p < 2.2e-16, paste(italic(R) ^ 2, \"= 0.1.239)\")", parse = TRUE, hjust = "left", colour = "black")


# Finding the linear model for both graphs
lm.party_size <- lm(adr_by_party$adr ~ adr_by_party$party_size)
lm.nkids <- lm(adr_by_party$adr ~ adr_by_party$Number_of_Kids_per_Party)

# Doing a summary of the two linear models shows that there is a very low P-value
# in both cases indicating high significance between the variables
summary(lm.party_size)
summary(lm.nkids)

