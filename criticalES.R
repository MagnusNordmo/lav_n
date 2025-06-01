# Install and load the package if you haven't already
# install.packages("remotes")
# remotes::install_github("psicostat/criticalESvalue")
library(criticalESvalue)

# Compute only the critical effect size with sample sizes
critical_t2s(n1 = 15, n2 = 15, var.equal = FALSE, hypothesis = "two.sided")
