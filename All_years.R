
library(dplyr)
library(ggplot2)

##Combining the plots of the separate years
##Using the data from the separate years as created before

# Add a new column 'Year' to each data frame
Browse20$Year <- 2020
Browse21$Year <- 2021
Browse22$Year <- 2022

all_datayears <- bind_rows(
  Browse20,
  Browse21,
  Browse22
)
all_datayears

# Set custom colors for the years
line_colors <- c("#FF0000", "#008000","#0000FF") 

# Plot the results with different colored lines for each year
ggplot(all_datayears, aes(TREATMENT, predicted_probs, color = as.factor(Year), group = Year)) +
  geom_point() +
  geom_line() +
  labs(x = "Treatment", y = "Estimated Probability of Browse Present", color = "Year") +
  ylim(0, 1) +
  scale_x_continuous(breaks = c(0,1), labels = c("No Rockdust", "Rockdust")) +
  scale_color_manual(values = line_colors, labels = c("2020", "2021", "2022")) +  # Set custom line colors for years
  theme_minimal()
