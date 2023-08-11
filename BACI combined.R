

##combining seperate BACIs into one plot
## Load in data for combining
BACI22C <- read.csv("BACI_2022C.csv", fileEncoding = "UTF-8-BOM")
BACI21C <- read.csv("BACI_2021C.csv", fileEncoding = "UTF-8-BOM")
BACI20C <- read.csv("BACI_2020C.csv", fileEncoding = "UTF-8-BOM")

## preparing data
BACI22C
Browse_BACI22C<- cbind(BACI22C$B1, BACI22C$B0)

glm_BACI22C <- glm(Browse_BACI22C ~ Time * Treatment, family = quasibinomial(link = "logit"),data = BACI22C)
summary(glm_BACI22C)

BACI21C
Browse_BACI21C<- cbind(BACI21C$B1, BACI21C$B0)

glm_BACI21C <- glm(Browse_BACI21C ~ Time * Treatment, family = quasibinomial(link = "logit"),data = BACI21C)
summary(glm_BACI21C)


BACI20C
Browse_BACI20C<- cbind(BACI20C$B1, BACI20C$B0)

glm_BACI20C <- glm(Browse_BACI20C ~ Time * Treatment, family = quasibinomial(link = "logit"),data = BACI20C)
summary(glm_BACI20C)


# Predict the probabilities 
predicted_probsBACI22C <- predict(glm_BACI22C, type = "response")
predicted_probsBACI20C <- predict(glm_BACI20C, type = "response")
predicted_probsBACI21C <- predict(glm_BACI21C, type = "response")

# Combine the predicted probabilities with the new data
BACI22C$predicted_probs <- predicted_probsBACI22C
BACI20C$predicted_probs <- predicted_probsBACI20C
BACI21C$predicted_probs <- predicted_probsBACI21C

all_data_BACI <- bind_rows(
  BACI22C,
  BACI21C,
  BACI20C
)
all_data_BACI



line_colors <- c("#FF9966", "#33a02c", "#0000FF", "#000000", "#FFFF00", "#00FFFF", "#FF00FF", "#808080", "#FFA500", "#FFC0CB", "#800080", "#A52A2A", "#008080", "#FFD700", "#E6E6FA" ) 

# Plot the results with different colored lines for each year
ggplot(all_data_BACI, aes(Time, predicted_probs, color = as.factor(Treatment), group = Treatment)) +
  geom_point() +
  geom_line() +
  labs(x = "Time", y = "Estimated Probability of Browse Present", color = "Treatment") +
  ylim(0, 1) +
  scale_x_continuous(breaks = c(1,2,3,4), labels = c("2018", "2020", "2021", "2022")) +
  scale_color_manual(values = line_colors, labels = c("No", "Yes")) +  # Set custom line colors for years
  theme_minimal()



