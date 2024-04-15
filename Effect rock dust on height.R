
library(dplyr)
library(ggplot2)
library(gridExtra)


custom_colors <- c("#FF9966", "#33a02c","#808080", "#FFFF00", "#0000FF", "#000000",  "#00FFFF", "#FF00FF", "#FFA500", "#FFC0CB", "#800080", "#A52A2A", "#008080", "#FFD700", "#E6E6FA")


## amelanchier lamarckii
HeightAM22<- read.csv("Heights_AM22.csv", fileEncoding = "UTF-8-BOM")
HeightAM22

glm_HeightAM22 <- glm(Average_height ~ TREATMENT, family = quasipoisson(link = "log") ,data = HeightAM22)
summary(glm_HeightAM22)


# Visualize the relationship between treatment and height
ggplot(HeightAM22, aes(x = TREATMENT, y = Average_height, fill = TREATMENT)) +
  geom_boxplot() +   scale_fill_manual(values = custom_colors) +
  labs(title = "Treatment effect on average tree height of Amelanchier lamarckii", x = "Treatment",
       y = "Average Tree Height")+
  guides(fill = "none") +
  theme(panel.background = element_rect(fill = "white", color = NA),
        panel.grid = element_line(color = "light gray"),
        panel.border = element_blank())


## Betula pendula
HeightBPE<- read.csv("Heights_BPE_22.csv", fileEncoding = "UTF-8-BOM")
HeightBPE

glm_BPE <- glm(Average_height ~ TREATMENT, family = quasipoisson(link = "log") ,data = HeightBPE)
summary(glm_BPE)


# Visualize the relationship between treatment and height
ggplot(HeightBPE, aes(x = TREATMENT, y = Average_height, fill = TREATMENT)) +
  geom_boxplot() +   scale_fill_manual(values = custom_colors) +
  labs(title = "Treatment effect on average tree height of Betula pendula", x = "Treatment",
       y = "Average Tree Height")+
  guides(fill = "none") +
  theme(panel.background = element_rect(fill = "white", color = NA),
        panel.grid = element_line(color = "light gray"),
        panel.border = element_blank())

## Betula pubescens
##only for one site data so not able to do glm
HeightBPU<- read.csv("HeightsBPU.csv", fileEncoding = "UTF-8-BOM")
HeightBPU

## Prunus serotina
HeightPS<- read.csv("Heights_PS_22.csv", fileEncoding = "UTF-8-BOM")
HeightPS

glm_PS <- glm(Average_height ~ TREATMENT, family = quasipoisson(link = "log") ,data = HeightPS)
summary(glm_PS)


# Visualize the relationship between treatment and height
ggplot(HeightPS, aes(x = TREATMENT, y = Average_height, fill = TREATMENT)) +
  geom_boxplot() +   scale_fill_manual(values = custom_colors) +
  labs(title = "Treatment effect on average tree height of Prunus serotina", x = "Treatment",
       y = "Average Tree Height")+
  guides(fill = "none") +
  theme(panel.background = element_rect(fill = "white", color = NA),
        panel.grid = element_line(color = "light gray"),
        panel.border = element_blank())

## Quercus robur
HeightQRO<- read.csv("Heights_QRO.csv", fileEncoding = "UTF-8-BOM")
HeightQRO

glm_QRO <- glm(Average_height ~ TREATMENT, family = quasipoisson(link = "log") ,data = HeightQRO)
summary(glm_QRO)


# Visualize the relationship between treatment and height
ggplot(HeightQRO, aes(x = TREATMENT, y = Average_height, fill = TREATMENT)) +
  geom_boxplot() +   scale_fill_manual(values = custom_colors) +
  labs(title = "Treatment effect on average tree height of Quercus robur", x = "Treatment",
       y = "Average Tree Height")+
  guides(fill = "none") +
  theme(panel.background = element_rect(fill = "white", color = NA),
        panel.grid = element_line(color = "light gray"),
        panel.border = element_blank())

## Quercus rubra
HeightQRU<- read.csv("Heights_QRU.csv", fileEncoding = "UTF-8-BOM")
HeightQRU

##only for one site data so not able to do glm

## Rhamnus frangula
HeightRF<- read.csv("Heights_RF.csv", fileEncoding = "UTF-8-BOM")
HeightRF

glm_RF <- glm(Average_height ~ TREATMENT, family = quasipoisson(link = "log") ,data = HeightRF)
summary(glm_RF)


# Visualize the relationship between treatment and height
ggplot(HeightRF, aes(x = TREATMENT, y = Average_height, fill = TREATMENT)) +
  geom_boxplot() +   scale_fill_manual(values = custom_colors) +
  labs(title = "Treatment effect on average tree height of Betula pendula", x = "Treatment",
       y = "Average Tree Height")+
  guides(fill = "none") +
  theme(panel.background = element_rect(fill = "white", color = NA),
        panel.grid = element_line(color = "light gray"),
        panel.border = element_blank())

## Sorbus aucuparia
HeightSA<- read.csv("Heights_SA.csv", fileEncoding = "UTF-8-BOM")
HeightSA

glm_SA <- glm(Average_height ~ TREATMENT, family = quasipoisson(link = "log") ,data = HeightSA)
summary(glm_SA)


# Visualize the relationship between treatment and height
ggplot(HeightSA, aes(x = TREATMENT, y = Average_height, fill = TREATMENT)) +
  geom_boxplot() +   scale_fill_manual(values = custom_colors) +
  labs(title = "Treatment effect on average tree height of Betula pendula", x = "Treatment",
       y = "Average Tree Height")+
  guides(fill = "none") +
  theme(panel.background = element_rect(fill = "white", color = NA),
        panel.grid = element_line(color = "light gray"),
        panel.border = element_blank())

## Pinus sylvestris
HeightPinus<- read.csv("Height_Pinus.csv", fileEncoding = "UTF-8-BOM")
HeightPinus

glm_Pinus <- glm(Average_height ~ TREATMENT, family = quasipoisson(link = "log") ,data = HeightPinus)
summary(glm_Pinus)


# Visualize the relationship between treatment and height
ggplot(HeightPinus, aes(x = TREATMENT, y = Average_height, color = TREATMENT)) +
  geom_boxplot(fill = "white", show.legend = FALSE) +  # Set fill to NA to make the boxes transparent
  scale_color_manual(values = custom_colors) +
  labs(x = "Treatment",
       y = "Average Tree Height")

## combining plots
HeightAM22$Species <- "Amelanchier_lamarckii"
HeightBPE$Species <- "Betula_pendula"
HeightPS$Species <- "Prunus_serotina"
HeightQRO$Species <- "Quercus_robur"
HeightRF$Species <- "Rhamnus_frangula"
HeightSA$Species <- "Sorbus_aucuparia"
HeightPinus$Species <- "Pinus_sylvestris"

all_data_heights <- bind_rows(
  HeightAM22, HeightBPE, HeightPS, HeightQRO, HeightRF, HeightSA, HeightPinus
)
all_data_heights


# Visualize the relationship between treatment and height
ggplot(all_data_heights, aes(x = TREATMENT, y = Average_height, fill = TREATMENT)) +
  geom_boxplot() +   scale_fill_manual(values = custom_colors) +
  labs(title = "Treatment effect on average tree height", x = "Treatment",
       y = "Average Tree Height")+
  guides(fill = "none") +
  theme(panel.background = element_rect(fill = "white", color = NA),
        panel.grid = element_line(color = "light gray"),
        panel.border = element_blank())

##species separate names but same plot
treatment_colour <- c("No" = "#FF0000", "Yes" = "#008000")

# Define custom names for treatments in the legend
treatment_labels <- c("No" = "No Rockdust", "Yes" = "Rockdust")

# Define order of species shown in plot
desired_order <- c("Amelanchier_lamarckii", "Betula_pendula", "Prunus_serotina", "Quercus_robur", "Rhamnus_frangula", "Sorbus_aucuparia", "Pinus_sylvestris")

# Plot boxplots
ggplot(all_data_heights, aes(x = factor(Species, levels = desired_order), y = Average_height, color = TREATMENT))+
  geom_boxplot() +   scale_color_manual(values = treatment_colour, labels = treatment_labels) + labs(x = "Species",
                                                                     y = "Average Tree Height (cm)") 


## one plot for broad and pine separate boxplots
plotbroad<- ggplot(all_data_heights, aes(x = TREATMENT, y = Average_height, color = Species))+
  geom_boxplot() +   scale_fill_manual(values = custom_colors) + labs(x = "Treatment",
                                                                     y = "Average Tree Height (cm)") 

plotpine<- ggplot(HeightPinus, aes(x = TREATMENT, y = Average_height, color = Species)) +
  geom_boxplot(fill = "white") +  # Set fill to NA to make the boxes transparent
  scale_color_manual(values = custom_colors) +
  labs(title = "b", x = "Treatment",
       y = "Average Tree Height (cm)")

grid.arrange(plotbroad, plotpine, ncol = 1)
