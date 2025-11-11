# Load required packages
library(kernlab)
library(MASS)
library(tidyverse)

# Define input points
x <- seq(0, 10, length.out = 100)

# Define the RBF kernel with a given length-scale (sigma)
# Define the length-scale
lambda <- 4  # e.g., smooth functions

# Convert to sigma for rbfdot
sigma <- 1 / (2 * lambda^2)

rbf_kernel <- rbfdot(sigma = sigma)

# Compute the kernel matrix
K <- kernelMatrix(rbf_kernel, as.matrix(x))
K <- K + 1e-6 * diag(length(x))  # numerical stability

# Draw samples from the GP prior
# set.seed(42)
samples <- mvrnorm(n = 5, mu = rep(0, length(x)), Sigma = K)  # 5 x 100 matrix

# Transpose so that each column is a sample
samples_t <- t(samples)
colnames(samples_t) <- paste0("sample_", 1:5)

# Combine with x
df <- as_tibble(samples_t) %>%
  mutate(x = x) %>%
  pivot_longer(cols = starts_with("sample"), names_to = "sample", values_to = "f")

# Plot
ggplot(df, aes(x = x, y = f, color = sample)) +
  geom_line(linewidth = 1) +
  labs(
    title = "Samples from GP Prior (RBF Kernel)",
    x = "x", y = "f(x)", color = "Sample"
  ) +
  theme_classic() +
  theme(
    legend.position = "none",
    axis.text = element_blank()
  )

