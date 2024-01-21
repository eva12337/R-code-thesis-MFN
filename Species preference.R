library(ggplot2)
library(dplyr)
library(effects)
##Species preference
Speciespref<- read.csv("Species preference.csv", fileEncoding = "UTF-8-BOM")
Speciespref
BrowseSpecies <- cbind(Speciespref$B1,Speciespref$B0)
BrowseSpecies

glm_Speciespref <- glm(BrowseSpecies ~ Species, family = quasibinomial(link = "logit") ,data = Speciespref)
summary(glm_Speciespref)
plot(allEffects(glm_Speciespref)) 


# Calculate the proportion of browsed trees for each species
data <- Speciespref %>%
  mutate(ProportionBrowsed = B1 / (B1 + B0))
data

# Create a boxplot
ggplot(data, aes(x = Species, y = ProportionBrowsed, color = Species))+
  geom_boxplot() +   scale_fill_manual(values = custom_colors) +labs(x = "Species",
                                                                     y = "Proportion browsed")+ theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

##Post hoc
install.packages("emmeans")
library(emmeans)

emm <- emmeans(glm_Speciespref, "Species")
pairs(emm)






