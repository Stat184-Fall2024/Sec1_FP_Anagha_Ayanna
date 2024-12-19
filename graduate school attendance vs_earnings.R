
R version 4.4.1 (2024-06-14) -- "Race for Your Life"
Copyright (C) 2024 The R Foundation for Statistical Computing
Platform: aarch64-apple-darwin20

R is free software and comes with ABSOLUTELY NO WARRANTY.
You are welcome to redistribute it under certain conditions.
Type 'license()' or 'licence()' for distribution details.

  Natural language support but running in an English locale

R is a collaborative project with many contributors.
Type 'contributors()' for more information and
'citation()' on how to cite R or R packages in publications.

Type 'demo()' for some demos, 'help()' for on-line help, or
'help.start()' for an HTML browser interface to help.
Type 'q()' to quit R.

[R.app GUI 1.80 (8416) aarch64-apple-darwin20]

[Workspace restored from /Users/ayannanorfleet/.RData]
[History restored from /Users/ayannanorfleet/.Rapp.history]

> # Install and load necessary libraries
install.packages("ggplot2")  
install.packages("dplyr")    
install.packages("readr")    

library(ggplot2)
library(dplyr)
library(readr)

# Load the grad_students dataset
csv_url_grad_students <- "https://raw.githubusercontent.com/fivethirtyeight/data/refs/heads/master/college-majors/grad-students.csv"
grad_students_data <- read_csv(csv_url_grad_students)

# Clean grad_students data (remove rows with missing values)
grad_students_cleaned <- grad_students_data %>%
  filter(complete.cases(.)) %>%
  mutate(
    grad_school_attendance = Grad_full_time_year_round / Grad_total,
    median_grad_earnings = Grad_median,
    median_non_grad_earnings = Nongrad_median
  ) %>%
  select(Major, median_grad_earnings, median_non_grad_earnings, grad_school_attendance)

# Calculate the difference in median earnings between grads and non-grads
grad_students_cleaned <- grad_students_cleaned %>%
  mutate(earnings_difference = median_grad_earnings - median_non_grad_earnings)

# Filter the data to include only majors where the earnings difference is positive
majors_with_earnings_increase <- grad_students_cleaned %>%
  filter(earnings_difference > 0)

# Create a scatter plot to visualize the relationship between grad school attendance and earnings increase
ggplot(majors_with_earnings_increase, aes(x = grad_school_attendance, y = earnings_difference)) +
  geom_point(alpha = 0.6, color = "blue") +  # Scatter plot with points
  geom_smooth(method = "lm", color = "red", se = FALSE) +  # Linear trend line
  labs(
    title = "Scatter Plot of Graduate School Attendance vs Earnings Increase",
    x = "Graduate School Attendance Proportion",
    y = "Earnings Increase (Grad - Non-grad)"
  ) +
  theme_minimal()
