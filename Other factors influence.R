
library(ggplot2)
library(dplyr)
## Other factors 2022
Browsemulti22 <- read.csv("Multi2022.csv", fileEncoding="UTF-8-BOM")
Browsemulti2022<- cbind(Browse22$B1, Browse22$B0)
Browsemulti22
## Basic model ##
fm2 <- glm(Browsemulti2022 ~ TREATMENT, family = quasibinomial(link = "logit"),data = Browsemulti22)
summary(fm2)



## Site added into model
fm3 <- glm(Browsemulti2022 ~ TREATMENT + SITE, family = quasibinomial(link = "logit"),data = Browsemulti22)
summary(fm3)

##number of pine trees in plot added into model
fm4 <- glm(Browsemulti2022 ~ TREATMENT + Amount_pine, family = quasibinomial(link = "logit"),data = Browsemulti22)
summary(fm4)

## Soil type added into model
fm5 <- glm(Browsemulti2022 ~ TREATMENT + Soil_type, family = quasibinomial(link = "logit"),data = Browsemulti22)
summary(fm5)

##Herb and grass cover added into model
fm6 <- glm(Browsemulti2022 ~ TREATMENT + Herb_cover, family = quasibinomial(link = "logit"),data = Browsemulti22)
summary(fm6)


## Light cover added into model
fm7 <- glm(Browsemulti2022 ~ TREATMENT + Light_cover, family = quasibinomial(link = "logit"),data = Browsemulti22)
summary(fm7)

##interaction treatmet and herb cover
fm8 <- glm(Browsemulti2022 ~ TREATMENT * Herb_cover, family = quasibinomial(link = "logit"),data = Browsemulti22)
summary(fm8)

# Predict the probabilities
predicted_probsfm2 <- predict(fm2, type = "response")
predicted_probsfm3 <- predict(fm3, type = "response")
predicted_probsfm4 <- predict(fm4, type = "response")
predicted_probsfm5 <- predict(fm5, type = "response")
predicted_probsfm6 <- predict(fm6, type = "response")
predicted_probsfm7 <- predict(fm7, type = "response")
predicted_probsfm8 <- predict(fm8, type = "response")


# Combine the predicted probabilities with the original data
Browsemulti22$predicted_probsfm2 <- predicted_probsfm2
Browsemulti22$predicted_probsfm3 <- predicted_probsfm3
Browsemulti22$predicted_probsfm4 <- predicted_probsfm4
Browsemulti22$predicted_probsfm5 <- predicted_probsfm5
Browsemulti22$predicted_probsfm6 <- predicted_probsfm6
Browsemulti22$predicted_probsfm7 <- predicted_probsfm7
Browsemulti22$predicted_probsfm8 <- predicted_probsfm8


line_colors <- c("#FF9966", "#33a02c", "#0000FF", "#000000", "#FFFF00", "#00FFFF", "#FF00FF", "#808080", "#FFA500", "#FFC0CB", "#800080", "#A52A2A", "#008080", "#FFD700", "#E6E6FA" ) 
point_color <- c("#FF9966", "#33a02c", "#00FF00", "#0000FF", "#000000", "#FFFF00", "#00FFFF", "#FF00FF", "#808080", "#FFA500", "#FFC0CB", "#800080", "#A52A2A", "#008080", "#FFD700", "#E6E6FA" ) 


ggplot(Browsemulti22, aes(SITE, predicted_probsfm3, color = as.factor(TREATMENT), group = TREATMENT)) +
  geom_point() +
  geom_line() +
  labs(x = "Site", y = "Estimated Probability of Browse Present in 2022", color = "TREATMENT") +
  ylim(0, 1) +
  scale_x_continuous() + # Set x-axis breaks and labels
  scale_color_manual(values = line_colors, breaks = c(0,1), labels = c("No", "Yes"))+ 
  theme_minimal()

ggplot(Browsemulti22, aes(Amount_pine, predicted_probsfm4, color = as.factor(TREATMENT), group = TREATMENT)) +
  geom_point() +
  geom_line() +
  labs(x = "Amount_pine", y = "Estimated Probability of Browse Present in 2022", color = "Treatment") +
  ylim(0, 1) +
  scale_x_continuous() + # Set x-axis breaks and labels
  scale_color_manual(values = point_color, labels = c("No", "Yes"))+ 
  theme_minimal()

ggplot(Browsemulti22, aes(Soil_type, predicted_probsfm5, color = as.factor(TREATMENT), group = TREATMENT)) +
  geom_point() +
  geom_line() +
  labs(x = "Soil type", y = "Estimated Probability of Browse Present in 2022", color = "Treatment") +
  ylim(0, 1) +
  scale_x_continuous(breaks = c(1,2), labels = c("Fine sand", "Coarse sand")) + # Set x-axis breaks and labels
  scale_color_manual(values = point_color, labels = c("No", "Yes"))+ 
  theme_minimal()

ggplot(Browsemulti22, aes(Herb_cover, predicted_probsfm6, color = as.factor(TREATMENT), group = TREATMENT)) +
  geom_point() +
  geom_line() +
  labs(x = "Herb and grass cover (%)", y = "Estimated Probability of Browse Present", color = "Treatment") +
  ylim(0, 1) +
  scale_x_continuous() + # Set x-axis breaks and labels
  scale_color_manual(values = point_color, labels = c("No", "Yes"))+ 
  theme_minimal()


ggplot(Browsemulti22, aes(Light_cover, predicted_probsfm7, color = as.factor(TREATMENT), group = TREATMENT)) +
  geom_point() +
  geom_line() +
  labs(x = "Light cover", y = "Estimated Probability of Browse Present in 2022", color = "Treatment") +
  ylim(0, 1) +
  scale_x_continuous() + # Set x-axis breaks and labels
  scale_color_manual(values = point_color, labels = c("No", "Yes"))+ 
  theme_minimal()


ggplot(Browsemulti22, aes(Herb_cover, predicted_probsfm8, color = as.factor(TREATMENT), group = TREATMENT)) +
  geom_point() +
  geom_line() +
  labs(x = "Herb cover", y = "Estimated Probability of Browse Present in 2022", color = "Treatment") +
  ylim(0, 1) +
  scale_x_continuous() + # Set x-axis breaks and labels
  scale_color_manual(values = point_color, labels = c("No", "Yes"))+ 
  theme_minimal()
