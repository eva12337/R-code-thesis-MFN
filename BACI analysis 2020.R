
## BACI analysis 2018 vs 2020
library(effects)
library(ggplot2)
library(dplyr)

BACI_2020<- read.csv("BACI_2020.csv", fileEncoding = "UTF-8-BOM")
Browse_BACI20 <- cbind(BACI_2020$B1, BACI_2020$B0)
BACI_2020

## making model, logistic regression ##
glm_BACI20 <- glm(Browse_BACI20 ~ Time * Treatment, family = binomial(link = "logit"),data = BACI_2020)
summary(glm_BACI20)


## Correct for overdispersion!!
newglm_BACI20 <- glm(BACI_Browse20 ~ Time * Treatment, family = quasibinomial(link = "logit"),data = BACI_2020)
summary(newglm_BACI20)

plot(allEffects(newglm_BACI20))

