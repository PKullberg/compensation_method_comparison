# A comparison of summative and multiplicative methods for aggregating envrionmental condition indices
# Author: Peter Kullberg, peter.kullberg@ymparisto.fi
library(tidyverse)

# Load the calculators
source("src/calculators.R")

# Create a value space that covers all five variables evenly
d1f <- seq(0, 1, by = 0.1)
d2f <- seq(0, 1, by = 0.1)
d3f <- seq(0, 1, by = 0.1)
ltf <- seq(0, 1, by = 0.1)
blf <- seq(0, 1, by = 0.1)

# Data frame of of all possible combinations of the values
df <- expand.grid(d1f, d2f, d3f, ltf, blf) %>% rename(d1f = Var1, d2f = Var2, d3f = Var3, ltf = Var4, blf = Var5) %>%
  mutate(sum_col = rowSums(.),
         d1 = d1f * 26,
         d2 = d2f * 26,
         d3 = d3f * 26,
         lt = ltf * 20,
         bl = blf * 50)

# compute the condition values
# 1. elite_scaled = traditional elite aggregation stretched to range from 0 to 1 (interpretation of the value: "proportion from minimum")
# 2. hh = weighted arithmentic mean of all indices (similar is used in habitat hectares approach, thus the  hh)
df_values <- df %>% rowwise() %>% mutate(elite_scaled = ELITE_value_scaled(c(d1, d2, d3, lt, bl), habitat = "Kangas_new"),
                                         hh = mean_value(c(d1, d2, d3, lt, bl), habitat = "Kangas_new"))

# Scatter plot of ecological values using different methods
ggplot(df_values, aes(x = hh, y = elite_scaled)) + 
  geom_point(size = 1, alpha = 0.5) + 
  geom_abline(slope = 1, lty = 2) + 
  labs(x = 'Habitat condition computed with the summing method',
       y = 'Habitat condition computed with the multiplicative method')

# test how different methods affect required compensation ratios
# Draw a random "sites to be developped" and another to act as a offset for it and compute requiret offset ratio
samp1 <- sample(1:nrow(df_values), 50000)
samp2 <- sample(1:nrow(df_values), 50000)
random_comp <- cbind(x = df_values[samp1, ], y = df_values[samp2, ]) %>%
  mutate(elite_s_dif = x.elite_scaled / y.elite_scaled,
         hh_dif = x.hh / y.hh)

# This plot shows that, when the random site is compensated with one that is in better condition (compensation ration is lower than 1)
# elite method requires on average larger compensation ratios than hh.
# If the random site is compensated with one that is in worse condition (compensation ration is higher than 1)
# elite method requires in general smaller compensation ratios than hh.
ggplot(random_comp, aes(x = hh_dif, y = elite_s_dif)) + 
  geom_point(size = 1.3, col = "black", alpha = 0.5) + 
  geom_smooth(col = "red") + 
  geom_abline(slope = 1, lty = 2, lwd = 1.1, col = "firebrick2") +
  geom_vline(xintercept = 1, lty = 2) +
  geom_hline(yintercept = 1, lty = 2) + 
  labs(x = 'Required compensation ratio with the summing method',
       y = 'Required compensation ratio with the multiplicative method') + 
  scale_x_log10() +
  scale_y_log10()
