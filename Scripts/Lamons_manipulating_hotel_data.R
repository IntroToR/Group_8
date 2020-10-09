# Modifying my Dataset
library(tidytuesdayR)
library(tidyverse)

hotels <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-11/hotels.csv')
hotels

lead.time_by_family <- hotels %>%
  select(adults, children, babies, lead_time) %>%
  arrange(lead_time) %>%
  mutate(party_size = (adults + children + babies)) %>%
  mutate(nkids = (children + babies))
  
longer.meal <- hotels %>%
  select(adults, children, babies, meal) %>%
  arrange(meal) %>%
  pivot_longer(c('adults', 'children', 'babies'), names_to = "age_range", values_to = "n_customers")

# creating a nice graph to correlate age of customer with adr

ggplot(longer.meal, aes(age_range, n_customers)) +
  geom_bar() +
  geom_bar(aes(colour = age_range)) +
  #ylim(0,20)
  
  