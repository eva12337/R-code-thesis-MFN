library(knitr)
knitr::stitch_rhtml('Rockdust_2020.R')
library(ggplot2)
library(dplyr)
Browse20 <- read.csv("Data_2020.csv", fileEncoding="UTF-8-BOM")
Browse2020<- cbind(Browse20$B1, Browse20$B0)


## inspect
Browse20


## making model, logistic regression ##
glm_Browse20 <- glm(Browse2020 ~ TREATMENT, family = binomial(link = "logit"),data = Browse20)
summary(glm_Browse20)

## Correct for overdispersion!
newglm_Browse20 <- glm(Browse2020 ~ TREATMENT, family = quasibinomial(link = "logit"),data = Browse20)
summary(newglm_Browse20)

## plotting glm

# Predict the probabilities
predicted_probs20 <- predict(newglm_Browse20, type = "response")

# Combine the predicted probabilities with the original data
Browse20$predicted_probs <- predicted_probs20

# Plot the results
# Set custom colors
point_color <- "#1f78b4"  # Blue color for points
line_color <- "#33a02c"   # Green color for the line

ggplot(Browse20, aes(TREATMENT, predicted_probs)) +
  geom_point(color=point_color) +
  geom_line(color=line_color) +
  labs(x = "Treated with Rock Dust", y = "Estimated Probability of Browse Present in 2020") +
  ylim(0, 1) +
  scale_x_continuous(breaks = c(0, 1), labels = c("No", "Yes")) +  # Set x-axis breaks and labels
  theme_minimal()

