# Load necessary libraries
library(ggplot2)  
library(dplyr)    
library(tidyr)    
library(car)      

# Load the dataset
data <- read.csv("C:\\Users\\goruk\\OneDrive\\Desktop\\stat 184\\Sec1_FP_Anagha_Ayanna\\data\\all-ages.csv")

# Ensure numeric columns are correctly interpreted
data <- data %>%
  mutate(
    Median = as.numeric(Median),       
    P25th = as.numeric(P25th),         
    P75th = as.numeric(P75th)          
  )

# Calculate the spread of salaries (leniency indicator) difference between 75th and 25th percentiles
data <- data %>%
  mutate(SalarySpread = P75th - P25th)  

# Remove outliers based on the interquartile range (IQR)
# Calculate the IQR, lower, and upper bounds for SalarySpread in each Major Category
outlier_threshold <- data %>%
  group_by(Major_category) %>%
  summarize(
    IQR = IQR(SalarySpread, na.rm = TRUE),
    Q1 = quantile(SalarySpread, 0.25, na.rm = TRUE),
    Q3 = quantile(SalarySpread, 0.75, na.rm = TRUE)
  ) %>%
  mutate(Lower = Q1 - 1.5 * IQR, Upper = Q3 + 1.5 * IQR)  # Define outlier thresholds

# Filter out rows with SalarySpread outside the calculated bounds
data <- data %>%
  left_join(outlier_threshold, by = "Major_category") %>%
  filter(SalarySpread >= Lower & SalarySpread <= Upper)

# Box plot of salary spread by major category (Improved readability, no outliers, and no scientific notation)
p3 <- ggplot(data, aes(x = reorder(Major_category, SalarySpread, FUN = median), y = SalarySpread)) +
  geom_boxplot(fill = "orange", color = "darkblue") +
  coord_flip() +  # Flip the axes for better readability
  theme_minimal() +
  scale_y_continuous(labels = scales::comma, expand = expansion(mult = c(0.1, 0.1))) +  # Format y-axis
  theme(
    axis.text.x = element_text(size = 10, angle = 0, hjust = 0.5),
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 12),
    plot.title = element_text(size = 14, face = "bold")
  ) +
  labs(
    title = "Box Plot of Salary Spread by Major Category (No Outliers)",
    x = "Major Category",
    y = "Salary Spread (P75th - P25th)"
  )

print(p3)

# Hypothesis testing: ANOVA to check if SalarySpread differs by Major Category
anova_result <- aov(SalarySpread ~ Major_category, data = data)  # Perform ANOVA
anova_summary <- summary(anova_result)  # Summarize the ANOVA results
print(anova_summary)

# Post-hoc test (Tukey's HSD) for pairwise comparisons between major categories
posthoc <- TukeyHSD(anova_result)  # Perform Tukey's test for detailed comparisons

# Create a table summarizing all Tukey's HSD results
all_comparisons <- as.data.frame(posthoc$Major_category)
all_comparisons$Comparison <- rownames(all_comparisons)  # Add rownames as a new column
all_comparisons <- all_comparisons %>%
  arrange(`p adj`)  # Sort comparisons by p-value

write.csv(all_comparisons, "anova_tukey_results.csv", row.names = FALSE)

# Select the top 5 major comparisons with the smallest and largest differences
most_worth_it <- all_comparisons %>%
  arrange(`diff`) %>%
  head(5) %>%
  mutate(Category = "Most Worth It")

least_worth_it <- all_comparisons %>%
  arrange(desc(`diff`)) %>%
  head(5) %>%
  mutate(Category = "Least Worth It")

# Combine the two groups 
highlighted_comparisons <- bind_rows(most_worth_it, least_worth_it)

# Create a bar chart to visualize the top 5 most and least worth it major comparisons
p4 <- ggplot(highlighted_comparisons, aes(x = reorder(Comparison, `diff`), y = `diff`, fill = Category)) +
  geom_bar(stat = "identity", color = "black") +  # Create bar chart
  coord_flip() +  # Flip axes for readability
  scale_fill_manual(values = c("Most Worth It" = "lightblue", "Least Worth It" = "pink")) +  # Assign colors
  theme_minimal() +
  labs(
    title = "Top 5 Most and Least Worth It Majors Based on Salary Spread",
    x = "Comparison of Major Categories",
    y = "Mean Difference in Salary Spread",
    fill = "Category"
  )

print(p4)

# Save processed data to a CSV file
write.csv(data, "processed_data.csv", row.names = FALSE)
