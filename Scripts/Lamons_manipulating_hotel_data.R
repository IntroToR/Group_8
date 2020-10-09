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
  filter(adr < 450 & adr != 0 & party_size < 10) %>%
  na.omit(adr_by_party)
  
# I also removed adr = 0 because those are useless to my analysis


boxplot.stats(adr_by_party$adr)$out #shows that values of 211.08 over are outlier
boxplot.stats(adr_by_party$party_size)$out
summary(adr_by_party$party_size)

ggplot(hotels) +
  aes(x = "", y = adr) +
  geom_boxplot(fill = "#000000") +
  theme_minimal()

hotels %>%
  filter(adr >= -50L & adr <= 1300L) %>%
  ggplot() +
  aes(x = "", y = adr) +
  geom_boxplot(fill = "#000000") +
  theme_minimal()

# these show that only outliers exist above 450



# creating a nice graph to correlate age of customer with adr

ggplot(adr_by_party) +
  aes(x = party_size, y = adr, colour = Number_of_Kids_per_Party) +
  geom_tile() +
  scale_color_distiller(palette = "Paired") +
  labs(x = "Number of People per Booking", y = "Average Daily Rate", title = "Number of People per Booking vs ADR Related to the Number of Kids per Party", caption = "Graph made by - Ethan Lamons") +
  dark_theme_bw() +
  geom_abline(aes(slope = 31.60, intercept = 41.13), size = 1.5) +
  scale_x_continuous(n.break = 6) 

ggplot(adr_by_party) +
  aes(x = Number_of_Kids_per_Party , y = adr, colour = Number_of_Kids_per_Party) +
  geom_tile() +
  scale_color_distiller(palette = "Paired") +
  labs(x = "Number of People per Booking", y = "Average Daily Rate", title = "Number of People per Booking vs ADR Related to the Number of Kids per Party", caption = "Graph made by - Ethan Lamons") +
  dark_theme_bw() +
  geom_abline(slope = 39.99, intercept = 98.99, size = 1.5) +
  scale_x_continuous(n.break = 6) 

# Finding the linear model for both graphs
lm(adr_by_party$adr ~ adr_by_party$party_size)
lm(adr_by_party$adr ~ adr_by_party$Number_of_Kids_per_Party)
