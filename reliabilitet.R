# Load necessary libraries
library(tidyverse)
library(pwr)
Sys.setlocale("LC_ALL", "nb-NO.UTF-8")

library(showtext)

## Loading Google fonts (https://fonts.google.com/)
font_add_google("EB Garamond")
showtext_auto()


# Parameters
sample_sizes <- seq(10,1000,10)
effect_sizes <- c(0.2, 0.5, 0.8)
reliabilities <- c(0.6, 0.7, 0.8,0.9)
alpha <- 0.05

# Create a function to adjust the effect size based on reliability
adjust_effect_size <- function(effect_size, reliability) {
  return(effect_size * sqrt(reliability))
}

# Create a data frame with all combinations of parameters
params <- expand_grid(SampleSize = sample_sizes, 
                      EffectSize = effect_sizes, 
                      Reliability = reliabilities)

# Calculate adjusted effect size and power
results <- params %>%
  mutate(AdjustedEffectSize = map2_dbl(EffectSize, Reliability, adjust_effect_size),
         Power = map2_dbl(AdjustedEffectSize, SampleSize, 
                          ~pwr.t.test(d = .x, n = .y, sig.level = alpha, type = "two.sample", alternative = "two.sided")$power)) %>%
  select(-AdjustedEffectSize)  # Remove the Adjusted Effect Size column

# Plot the results using ggplot2
results |> 
  filter(case_when(EffectSize == 0.5 ~ SampleSize < 200,
                   EffectSize == 0.8 ~ SampleSize < 80,
                   T ~ SampleSize > 9)) |> 
  mutate(EffectSize = as.character(EffectSize),
         EffectSize = paste0("Effektstørrelse=",EffectSize)) |> 
  ggplot(aes(x = SampleSize, y = Power, color = as.factor(Reliability))) +
  geom_line() +
  facet_wrap(~ EffectSize,scales = "free_x") +
  labs(x = "Utvalgsstørrelse",
       y = "Statistisk kraft",
       color = "Reliabilitet") +
  scale_y_continuous(labels = scales::percent) +
  scale_color_viridis_d() +
  theme_minimal(base_size = 20) +
  theme(legend.position = "bottom")


# Optionally, save the plot as an image
#ggsave("reliability.pdf", width = 12, height = 8)
