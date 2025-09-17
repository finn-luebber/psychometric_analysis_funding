# Load necessary libraries
library(ggplot2)
library(tidyverse)
library(ggtext)


theme_set(
  theme_minimal(base_family = "Helvetica") +
    theme(
      legend.title = element_blank(),
      legend.text = element_text(size = 20),
      axis.title = element_text(size = 20, face = "bold", color = "black"),
      axis.text.x = element_text(size = 16, color = "black"),
      axis.text.y = element_text(size = 16, color = "black"),
      plot.title = element_blank(),
      plot.caption = element_blank(),
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.margin = unit(c(0, 0, 0, 0), "cm")
    )
)

# Set parameters for the normal distributions
mean_selected <- 50000  # Mean performance of selected Funded Projects
sd_selected <- 5000      # Standard deviation of selected Funded Projects

mean_non_selected <- 45000  # Mean performance of non-selected Funded Projects
sd_non_selected <- 5000      # Standard deviation of non-selected Funded Projects

# Create a sequence of performance values
performance_values <- seq(30000, 70000, by = 100)

# Calculate the density for selected and non-selected Funded Projects
density_selected <- dnorm(performance_values, mean = mean_selected, sd = sd_selected)
density_non_selected <- dnorm(performance_values, mean = mean_non_selected, sd = sd_non_selected)

# Create a data frame for plotting
data <- data.frame(
  Performance = performance_values,
  Density_Selected = density_selected,
  Density_Non_Selected = density_non_selected
)

# Reshape the data for ggplot
data_long <- data %>%
  pivot_longer(cols = starts_with("Density"), names_to = "Group", values_to = "Density") %>%
    mutate(region = case_when(
        dist == "Noise" & x < criterion ~ "Correct Rejection",
        dist == "Noise" & x >= criterion ~ "False Alarm",
        dist == "Signal+Noise" & x < criterion ~ "Miss",
        dist == "Signal+Noise" & x >= criterion ~ "Hit"
    ))

# Plot the overlapping bell curves
ggplot(data_long, aes(x = Performance, y = Density, color = Group)) +
  geom_line(size = 1) +
  labs(title = "Overlapping Bell Curves of Selected and Non-Selected Projects' Performance",
       x = "*Goodness* in €",
       y = "Density") +
  scale_color_manual(values = c("purple", "orange"), labels = c("Non-Selected Proposals", "Selected Projects")) +
  theme(legend.title = element_blank(),
        legend.position = c(0.8, 0.8),
        axis.text.y = element_blank(),
        axis.title.x = ggtext::element_markdown())


# Sample data for predicted and actual productivity
set.seed(020522)

n <- 20  # Number of Funded Projects
mean_predicted <- 51500
sd_predicted <- 5000  # Standard deviation for predicted values

predicted_productivity <- rnorm(n, mean = mean_predicted, sd = sd_predicted)

# Generate actual productivity values with a correlation of approximately 0.4
# First, generate random noise
noise <- rnorm(n, mean = 0, sd = 3600)  # Adjust the standard deviation of noise as needed

# Create actual productivity values based on predicted values and noise
actual_productivity <- predicted_productivity * 0.4 + noise * 0.6 + (mean_predicted - 0.4 * mean_predicted)

# Check the correlation
correlation <- cor(predicted_productivity, actual_productivity)

data <- data.frame(
  Funded_Project = paste("Project", 1:n),
  Predicted_Productivity = predicted_productivity,
  Actual_Productivity = actual_productivity
)

data$Funded_Project <- factor(data$Funded_Project, levels = paste("Project", 1:20))


# Reshape the data for ggplot
data_long <- tidyr::pivot_longer(data, cols = c("Predicted_Productivity", "Actual_Productivity"),
                                 names_to = "Type", values_to = "Productivity")

# Plotting the predicted vs actual productivity
ggplot(data_long, aes(x = Funded_Project, y = Productivity, color = Type, group = Type)) +
  geom_line(size = 1, position = position_dodge(width = 0.4)) +
  geom_point(size = 3, position = position_dodge(width = 0.4)) +
  labs(title = "Comparison of Predicted vs Actual *Goodness*",
       x = "Proposed Projects",
       y = "*Goodness* in €") +
  scale_color_manual(values = c("darkgreen", "pink"), labels = c("Predicted *Goodness*", "Actual *Goodness*")) +
  theme(legend.title = element_blank(),
        legend.position = c(0.2, 0.2),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.y = ggtext::element_markdown(),
        legend.text = ggtext::element_markdown())

