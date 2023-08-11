
## BACI analysis 2018 vs 2022
library(effects)
library(ggplot2)
library(dplyr)


BACI_2022<- read.csv("BACI_2022.csv", fileEncoding = "UTF-8-BOM")
Browse_BACI22<- cbind(BACI_2022$B1, BACI_2022$B0)
BACI_2022


## making model, logistic regression ##
glm_BACI22 <- glm(Browse_BACI22 ~ Time * Treatment, family = binomial(link = "logit"),data = BACI_2022)
summary(glm_BACI22)

## Correct for overdispersion!!
newglm_BACI22 <- glm(Browse_BACI22 ~ Time * Treatment, family = quasibinomial(link = "logit"),data = BACI_2022)
summary(newglm_BACI22)


plot(allEffects(newglm_BACI22))




