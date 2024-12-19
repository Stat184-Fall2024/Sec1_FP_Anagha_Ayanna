library(ggplot2)
library(scales)  # For formatting axis labels

# Read the data
majors_data <- read.csv("C:\\Users\\goruk\\OneDrive\\Desktop\\stat 184\\Sec1_FP_Anagha_Ayanna\\data\\recent-grads.csv")

# Check the structure of the data
str(majors_data)
head(majors_data)

# Plot: Correlation Between Total People in a Major and Unemployment Rate
ggplot(data = majors_data, aes(x = Total, y = Unemployment_rate)) +
  geom_point(color = "blue", alpha = 0.7) + # Scatter plot points
  geom_smooth(method = "lm", se = TRUE, color = "red") + # Add a regression line
  labs(
    title = "Correlation Between Total People in a Major and Unemployment Rate",
    x = "Total People in Major",
    y = "Unemployment Rate"
  ) +
  scale_x_continuous(labels = comma) + # Format x-axis labels
  theme_minimal()

# Plot: Correlation Between Unemployment Rate and Starting Salary
ggplot(data = majors_data, aes(x = Unemployment_rate, y = Median)) +
  geom_point(color = "blue", size = 3, alpha = 0.7) +  # Scatter plot points
  geom_smooth(method = "lm", color = "red", se = FALSE) +  # Add a linear trend line
  labs(
    title = "Correlation Between Unemployment Rate and Starting Salary",
    x = "Unemployment Rate",
    y = "Starting Salary (Median)"
  ) +
  scale_y_continuous(labels = comma) + # Format x-axis labels
  theme_minimal()  
