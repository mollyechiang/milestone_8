library(broom)
library(janitor)
library(sf)
library(tidyverse)

regression <- median_ppn_data %>%
  select(neighbourhood, zhvi, median_ppn) %>%
  drop_na() %>%
  mutate(id = 1:154) %>%
  group_by(id, neighbourhood) %>%
  nest() %>%
  mutate(models = map(data, ~lm(data = .x, formula = median_ppn ~ zhvi))) %>%
  mutate(values = map(models, ~coef(.x))) %>%
  mutate(slope = map_dbl(values, ~pluck(.x, "zhvi")))

model <- lm(data = nyc_shapes_full, median_ppn ~ zhvi)
