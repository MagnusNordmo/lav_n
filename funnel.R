# Load necessary libraries
library(tidyverse)
library(showtext)
Sys.setlocale("LC_ALL", "nb-NO.UTF-8")

## Loading Google fonts (https://fonts.google.com/)
font_add_google("EB Garamond")
showtext_auto()


# Function to simulate studies
simulate_studies <- function(num_studies, overall_cohens_d = 0.2) {
  
  # Function to simulate a single study
  simulate_study <- function(n, d) {
    # Simulate data for two groups
    group1 <- rnorm(n, mean = 0, sd = 1)
    group2 <- rnorm(n, mean = d, sd = 1)
    
    # Calculate observed Cohen's d
    observed_d <- (mean(group2) - mean(group1)) / sqrt(((sd(group1)^2 + sd(group2)^2) / 2))
    
    # Calculate sampling variance of the observed d
    variance_d <- ((n + n) / (n * n)) + (observed_d^2 / (2 * (n + n)))
    
    # Calculate p-value
    t_stat <- observed_d / sqrt(variance_d)
    p_value <- 2 * (1 - pt(abs(t_stat), df = (n + n - 2)))
    
    # Return a tibble with the results
    tibble(observed_d = observed_d, variance_d = variance_d, p_value = p_value)
  }
  
  # Generate study sizes
  study_sizes <- sample(15:100, num_studies - 2, replace = TRUE)
  study_sizes <- c(study_sizes, 200, 250)
  
  # Simulate each study and combine results into a single tibble
  results <- map_dfr(study_sizes, ~simulate_study(.x, overall_cohens_d))
  
  return(results)
}

# Simulate 50 studies
set.seed(1234) # For reproducibility
num_studies <- 50
results <- simulate_studies(num_studies)

# Display the results
print(results)

# Select only significant results
results_sig <- results |> filter(p_value < .05)


# Make the plot
pdf(file='all.pdf',width = 12,height = 6) # Open PDF device with specific file name


par(mfrow = c(1, 2))



with(results_sig,metafor::funnel(observed_d,variance_d,
                                             col = ifelse(p_value <.05,"red","black"),
                                             pch = ifelse(p_value <.05,19,1),
                                             xlab = "Effekststørrelse",ylab = "Standardfeil",
                                             at = seq(-.8,1.4,.4),
                                             digits = c(1,2)))

with(results,metafor::funnel(observed_d,variance_d,
                             refline = .2,
                             col = ifelse(p_value <.05,"red","black"),
                             pch = ifelse(p_value <.05,19,1),
                             xlab = "Effektstørrelse",ylab = "Standardfeil",
                             at = seq(-.8,1.2,.4),
                             digits = c(1,2)))
dev.off() # Turn the PDF device off

path <- "C:/Users/magno/OneDrive - USN/Kronikker/små_studier/all.pdf"

# Open file
#system(paste0('open "', path, '"'))