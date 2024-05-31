# Load the ggplot2 library
library(ggplot2)
Sys.setlocale("LC_ALL", "nb-NO.UTF-8")

library(showtext)

## Loading Google fonts (https://fonts.google.com/)
font_add_google("EB Garamond")
showtext_auto()


# Set the seed for reproducibility
set.seed(125)

# Number of coin flips
num_flips <- 500

# Simulate the coin flips (1 for heads, 0 for tails)
coin_flips <- sample(c(0, 1), num_flips, replace = TRUE)

# Calculate the cumulative average of heads
cumulative_average <- cumsum(coin_flips) / 1:num_flips

# Create a data frame for ggplot2
data <- data.frame(Flips = 1:num_flips, CumulativeAverage = cumulative_average)

# Create the plot
ggplot(data, aes(x = Flips, y = CumulativeAverage)) +
  geom_line(color = "blue") +
  geom_hline(yintercept = 0.5, color = "red", linetype = "dashed", size = 1) +
  labs(x = "Antall myntkast", y = "Kumulativt gjennomsnitt av krone-resultat") +
  scale_y_continuous(limits = c(0, 1),
                     labels = scales::percent) +
  theme_minimal(base_size = 20) + 
  theme(panel.grid = element_blank())

# Save the plot
#ggsave("mynt.pdf", width = 12, height = 8)
