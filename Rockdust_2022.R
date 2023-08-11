
library(ggplot2)
library(dplyr)

Browse22 <- read.csv("Data_2022.csv", fileEncoding="UTF-8-BOM")
Browse2022<- cbind(Browse22$B1, Browse22$B0)

## inspect
Browse22


## making model, logistic regression ##
glm_Browse22 <- glm(Browse2022 ~ TREATMENT, family = binomial(link = "logit"),data = Browse22)
summary(glm_Browse22)


## Correct for overdispersion!!
newglm_Browse22 <- glm(Browse2022 ~ TREATMENT, family = quasibinomial(link = "logit"),data = Browse22)
summary(newglm_Browse22)


##plotting glm
# Predict the probabilities
predicted_probs22 <- predict(newglm_Browse22, type = "response")

# Combine the predicted probabilities with the original data
Browse22$predicted_probs <- predicted_probs22

# Plot the results
# Set custom colors
point_color <- "#1f78b4"  # Blue color for points
line_color <- "#33a02c"   # Green color for the line

ggplot(Browse22, aes(TREATMENT, predicted_probs)) +
  geom_point() +
  geom_line() +
  labs(x = "Treated with Rock Dust", y = "Estimated Probability of Browse Present in 2022") +
  ylim(0, 1) +
  scale_x_continuous(breaks = c(0, 1), labels = c("No", "Yes")) +  # Set x-axis breaks and labels
  theme_minimal()
