library(knitr)
knitr::stitch_rhtml('Rockdust_2021.R')
library(ggplot2)
library(dplyr)
Browse21 <- read.csv("Data_2021.csv", fileEncoding="UTF-8-BOM")
Browse2021<- cbind(Browse21$B1, Browse21$B0)


## inspect
Browse21

## making model, logistic regression ##
glm_Browse21 <- glm(Browse2021 ~ TREATMENT, family = binomial(link = "logit"),data = Browse21)
summary(glm_Browse21)

##Correct for overdispersion!!
newglm_Browse21 <- glm(Browse2021 ~ TREATMENT, family = quasibinomial(link = "logit"),data = Browse21)
summary(newglm_Browse21)

## Plotting glm
# Predict the probabilities
predicted_probs21 <- predict(newglm_Browse21, type = "response")

# Combine the predicted probabilities with the original data
Browse21$predicted_probs <- predicted_probs21

# Plot the results
# Set custom colors
point_color <- "#1f78b4"  # Blue color for points
line_color <- "#33a02c"   # Green color for the line

ggplot(Browse21, aes(TREATMENT, predicted_probs)) +
  geom_point(color=point_color) +
  geom_line(color=line_color) +
  labs(x = "Treated with Rock Dust", y = "Estimated Probability of Browse Present in 2021") +
  ylim(0, 1) +
  scale_x_continuous(breaks = c(0, 1), labels = c("No", "Yes")) +  # Set x-axis breaks and labels
  theme_minimal()

