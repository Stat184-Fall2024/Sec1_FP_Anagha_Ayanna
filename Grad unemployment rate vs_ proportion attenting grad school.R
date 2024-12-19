
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

> #Install and load necessary libraries
> install.packages("ggplot2")  
> install.packages("dplyr")    
> install.packages("readr")    
> library(ggplot2)
> library(dplyr)
> library(readr)
> 
> # Load the grad_students dataset
> csv_url_grad_students <- "https://raw.githubusercontent.com/fivethirtyeight/data/refs/heads/master/college-majors/grad-students.csv"
> grad_students_data <- read_csv(csv_url_grad_students)                                                                                                                                                                                           
> 
> # Clean grad_students data (remove rows with missing values)
> grad_students_cleaned <- grad_students_data %>%
+   filter(complete.cases(.)) %>%
+   mutate(proportion_grad_school = (Grad_full_time_year_round + Grad_full_time_year_round) / Grad_total) %>%
+   select(Major, Grad_unemployment_rate, proportion_grad_school)
> 
> # Create a scatter plot to visualize the relationship between Unemployment Rate and Proportion Attending Graduate School
> ggplot(grad_students_cleaned, aes(x = Grad_unemployment_rate, y = proportion_grad_school)) +
+   geom_point(alpha = 0.6, color = "blue") +  # Scatter plot with points
+   geom_smooth(method = "lm", color = "red", se = FALSE) +  # Linear trend line
+   labs(title = "Scatter Plot of Grad Unemployment Rate vs Proportion Attending Graduate School",
+        x = "Graduate Unemployment Rate",
+        y = "Proportion Attending Graduate School") +
+   theme_minimal()
`geom_smooth()` using formula = 'y ~ x'
> 
> 