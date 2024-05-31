
library(tidyverse)
library(pwr)
Sys.setlocale("LC_ALL", "nb-NO.UTF-8")

library(showtext)

## Loading Google fonts (https://fonts.google.com/)
font_add_google("EB Garamond")
showtext_auto()


# Set parameters
alpha <- 0.05  # significance level
num_simulations <- 10000  # number of simulations
powers <- c(0.1, 0.3, 0.5, 0.8, 0.99)  # different power levels
true_effect_size <- 0.2  # true effect size (Cohen's d)
bias_ratios <- c(Inf, 10, 5,2,1)  # Inf: only significant results, 10:1 bias, 5:1 bias, 1: no bias

# Function to simulate biased effect sizes
simulate_bias <- function(power, alpha, num_simulations, true_effect_size, bias_ratio) {
  effect_sizes <- numeric(num_simulations)
  
  # Calculate the sample size needed for the given power and true effect size
  sample_size <- ceiling(pwr.t.test(d = true_effect_size, power = power, sig.level = alpha, type = "two.sample")$n)
  
  for (i in 1:num_simulations) {
    # Generate data for two groups
    group1 <- rnorm(sample_size, mean = 0, sd = 1)
    group2 <- rnorm(sample_size, mean = true_effect_size, sd = 1)
    
    # Perform t-test
    t_test <- t.test(group1, group2)
    
    # Check if p-value is less than alpha
    if (t_test$p.value < alpha || runif(1) < 1 / bias_ratio) {
      # Calculate observed effect size
      observed_effect_size <- (mean(group2) - mean(group1)) / sqrt((sd(group1)^2 + sd(group2)^2) / 2)
      effect_sizes[i] <- observed_effect_size
    } else {
      effect_sizes[i] <- NA
    }
  }
  
  # Remove NAs (non-significant results)
  effect_sizes <- na.omit(effect_sizes)
  
  # Calculate average reported effect size
  average_reported_effect_size <- mean(effect_sizes)
  return(average_reported_effect_size)
}

# Perform simulations for each power level and bias ratio using purrr
results <- expand.grid(Power = powers, BiasRatio = bias_ratios) %>%
  mutate(AverageReportedEffectSize = map2_dbl(Power, BiasRatio,
                                              ~simulate_bias(.x, alpha, num_simulations, true_effect_size, .y)))

# Convert BiasRatio to factor for plotting
results <- results %>%
  mutate(BiasRatio = factor(BiasRatio, levels = bias_ratios, labels = c("Kun signifikante funn", 
                                                                        "10:1 skjevhet", 
                                                                        "5:1 skjevhet",
                                                                        "2:1 skjevhet",
                                                                        "Alle studier blir publiser")))

# Plot results using ggplot2
ggplot(results, aes(x = Power, y = AverageReportedEffectSize, color = BiasRatio, group = BiasRatio)) +
  geom_line() +
  geom_point() +
  labs(x = "Statistisk kraft",
       y = "Gjennomsnittlig rapportert effektstørrelse",
       color = "Publikasjonsbias\nog file-drawer") +
  scale_color_viridis_d() +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0,1),breaks = seq(0,1,.2)) + 
  theme_minimal(base_size = 20) + 
  theme(panel.grid.minor = element_blank())

#ggsave("publikasjonsbias.pdf",width = 10, height = 6)
