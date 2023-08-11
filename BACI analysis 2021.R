
## BACI analysis 2018 vs 2021
library(effects)
library(ggplot2)
library(dplyr)

BACI_2021<- read.csv("BACI_2021.csv", fileEncoding = "UTF-8-BOM")
Browse_BACI21<- cbind(BACI_2021$B1, BACI_2021$B0)
BACI_2021

## making model, logistic regression ##
glm_BACI21 <- glm(Browse_BACI21 ~ Time * Treatment, family = binomial(link = "logit"),data = BACI_2021)
summary(glm_BACI21)

## overdispersion as residual deviance is much larger than residual degrees of freedom so quasibinomial
newglm_BACI21 <- glm(Browse_BACI21 ~ Time * Treatment, family = quasibinomial(link = "logit"),data = BACI_2021)
summary(newglm_BACI21)

plot(allEffects(newglm_BACI21))

