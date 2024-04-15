
library(ggplot2)
library(dplyr)
library(effects)

## Trees >50-160cm
##Amelanchier lamarckii

PlotsAM<- read.csv("PlotsAM.csv", fileEncoding = "UTF-8-BOM")
PlotsAM


glm_plotAM <- glm(Average_height ~ Plot_type, family = quasipoisson(link = "log") ,data = PlotsAM)
summary(glm_plotAM)


plot(allEffects(glm_plotAM))

ggplot(PlotsAM, aes(x = Plot_type, y = Average_height, fill = TREATMENT)) +
  geom_boxplot() +   scale_fill_manual(values = custom_colors) +
  labs(title = "Plot and treatment effect on average tree height of Amelanchier lamarckii", x = "Plot type",
       y = "Average Tree Height (cm)") +
  theme(panel.background = element_rect(),
        panel.grid = element_line(color = "light gray"),
        panel.border = element_blank())

## Betula pendula
PlotsBPE<- read.csv("PlotsBPE.csv", fileEncoding = "UTF-8-BOM")
PlotsBPE


glm_plotBPE <- glm(Average_height ~ Plot_type, family = quasipoisson(link = "log") ,data = PlotsBPE)
summary(glm_plotBPE)


plot(allEffects(glm_plotBPE))


ggplot(PlotsBPE, aes(x = Plot_type, y = Average_height, fill = TREATMENT)) +
  geom_boxplot() +   scale_fill_manual(values = custom_colors) +
  labs(title = "Plot and treatment effect on average tree height of Betula pendula", x = "Plot type",
       y = "Average Tree Height (cm)") +
  theme(panel.background = element_rect(),
        panel.grid = element_line(color = "light gray"),
        panel.border = element_blank())

## Betula pubescens --> not enough data
PlotsBPU<- read.csv("PlotsBPU.csv", fileEncoding = "UTF-8-BOM")
PlotsBPU


glm_plotBPU <- glm(Average_height ~ Plot_type, family = quasipoisson(link = "log") ,data = PlotsBPU)
summary(glm_plotBPU)


plot(allEffects(glm_plotBPU))


ggplot(PlotsBPU, aes(x = Plot_type, y = Average_height, fill = TREATMENT)) +
  geom_boxplot() +   scale_fill_manual(values = custom_colors) +
  labs(title = "Plot and treatment effect on average tree height of Betula pubescens", x = "Plot type",
       y = "Average Tree Height (cm)") +
  theme(panel.background = element_rect(),
        panel.grid = element_line(color = "light gray"),
        panel.border = element_blank())

## Prunus serotina
PlotsPS<- read.csv("PlotsPS.csv", fileEncoding = "UTF-8-BOM")
PlotsPS


glm_plotPS <- glm(Average_height ~ Plot_type, family = quasipoisson(link = "log") ,data = PlotsPS)
summary(glm_plotPS)


plot(allEffects(glm_plotPS))


ggplot(PlotsPS, aes(x = Plot_type, y = Average_height, fill = TREATMENT)) +
  geom_boxplot() +   scale_fill_manual(values = custom_colors) +
  labs(title = "Plot and treatment effect on average tree height of Prunus serotina", x = "Plot type",
       y = "Average Tree Height (cm)") +
  theme(panel.background = element_rect(),
        panel.grid = element_line(color = "light gray"),
        panel.border = element_blank())


## Quercus robur
PlotsQR<- read.csv("PlotsQR.csv", fileEncoding = "UTF-8-BOM")
PlotsQR

glm_plotQR <- glm(Average_height ~ Plot_type, family = quasipoisson(link = "log") ,data = PlotsQR)
summary(glm_plotQR)

plot(allEffects(glm_plotQR))


ggplot(PlotsQR, aes(x = Plot_type, y = Average_height, fill = TREATMENT)) +
  geom_boxplot() +   scale_fill_manual(values = custom_colors) +
  labs(title = "Plot and treatment effect on average tree height of Quercus robur", x = "Plot type",
       y = "Average Tree Height (cm)") +
  theme(panel.background = element_rect(),
        panel.grid = element_line(color = "light gray"),
        panel.border = element_blank())


## Rhamnus frangula
PlotsRF<- read.csv("PlotsRF.csv", fileEncoding = "UTF-8-BOM")
PlotsRF


glm_plotRF <- glm(Average_height ~ Plot_type, family = quasipoisson(link = "log") ,data = PlotsRF)
summary(glm_plotRF)

plot(allEffects(glm_plotRF))



ggplot(PlotsRF, aes(x = Plot_type, y = Average_height, fill = TREATMENT)) +
  geom_boxplot() +   scale_fill_manual(values = custom_colors) +
  labs(title = "Plot and treatment effect on average tree height of Rhamnus frangular", x = "Plot type",
       y = "Average Tree Height (cm)") +
  theme(panel.background = element_rect(),
        panel.grid = element_line(color = "light gray"),
        panel.border = element_blank())


## Sorbus aucuparia
PlotsSorbus<- read.csv("PlotsSorbus.csv", fileEncoding = "UTF-8-BOM")
PlotsSorbus


glm_plotSorbus <- glm(Average_height ~ Plot_type, family = quasipoisson(link = "log") ,data = PlotsSorbus)
summary(glm_plotSorbus)


plot(allEffects(glm_plotSorbus))


ggplot(PlotsSorbus, aes(x = Plot_type, y = Average_height, fill = TREATMENT)) +
  geom_boxplot() +   scale_fill_manual(values = custom_colors) +
  labs(title = "Plot and treatment effect on average tree height of Sorbus aucuparia", x = "Plot type",
       y = "Average Tree Height (cm)") +
  theme(panel.background = element_rect(),
        panel.grid = element_line(color = "light gray"),
        panel.border = element_blank())


## Pinus sylvestris
PlotsPinus<- read.csv("PlotsPinus.csv", fileEncoding = "UTF-8-BOM")
PlotsPinus


glm_plotPinus <- glm(Average_height ~ Plot_type, family = quasipoisson(link = "log") ,data = PlotsPinus)
summary(glm_plotPinus)

plot(allEffects(glm_plotPinus))


ggplot(PlotsPinus, aes(x = Plot_type, y = Average_height, fill = TREATMENT)) +
  geom_boxplot() +   scale_fill_manual(values = custom_colors) +
  labs(title = "Plot and treatment effect on average tree height of Pinus sylvestris", x = "Plot type",
       y = "Average Tree Height (cm)") +
  theme(panel.background = element_rect(),
        panel.grid = element_line(color = "light gray"),
        panel.border = element_blank())


## combining data
PlotsAM$Species <- "Amelanchier_lamarckii"
PlotsBPE$Species <- "Betula_pendula"
PlotsBPU$Species <- "Betula_pubescens"
PlotsPS$Species <- "Prunus_serotina"
PlotsQR$Species <- "Quercus_robur"
PlotsRF$Species <- "Rhamnus_frangula"
PlotsSorbus$Species <- "Sorbus_aucuparia"
PlotsPinus$Species <- "Pinus_sylvestris"

all_data_plots <- bind_rows(
  PlotsAM, PlotsBPE, PlotsPS, PlotsQR, PlotsRF, PlotsSorbus, PlotsPinus
)
all_data_plots

# Visualize the relationship between plot type and height

ggplot(all_data_plots, aes(x = Plot_type, y = Average_height, fill = TREATMENT)) +
  geom_boxplot() +   scale_fill_manual(values = custom_colors) +
  labs(title = "Plot type effect on average tree height", x = "Plot type",
       y = "Average Tree Height")+
  guides(fill = "none") +
  theme(panel.background = element_rect(),
        panel.grid = element_line(color = "light gray"),
        panel.border = element_blank())

## species seperately
# Define order of species shown in plot
desired_order_ploteffect <- c("Amelanchier_lamarckii", "Betula_pendula", "Prunus_serotina", "Quercus_robur", "Rhamnus_frangula", "Sorbus_aucuparia", "Pinus_sylvestris")

# Define colours
plot_colour <- c("CAM" = "#FF0000", "EXRD" = "#008000")

# Plot boxplots
ggplot(all_data_plots, aes(x = factor(Species, levels = desired_order_ploteffect), y = Average_height, color = Plot_type))+
  geom_boxplot() +   scale_color_manual(values = plot_colour) +labs(x = "Species",
                                                                     y = "Average Tree Height (cm)")
## Trees up to 50cm##
##Amelanchier lamarckii

PlotsAM50<- read.csv("PlotsAM50.csv", fileEncoding = "UTF-8-BOM")
PlotsAM50


glm_plotAM50 <- glm(Average_height ~ Plot_type, family = quasipoisson(link = "log") ,data = PlotsAM50)
summary(glm_plotAM50)


plot(allEffects(glm_plotAM50))

ggplot(PlotsAM50, aes(x = Plot_type, y = Average_height, fill = TREATMENT)) +
  geom_boxplot() +   scale_fill_manual(values = custom_colors) +
  labs(title = "Plot and treatment effect on average tree height of Amelanchier lamarckii", x = "Plot type",
       y = "Average Tree Height (cm)") +
  theme(panel.background = element_rect(),
        panel.grid = element_line(color = "light gray"),
        panel.border = element_blank())

## Betula pendula
PlotsBPE50<- read.csv("PlotsBPE50.csv", fileEncoding = "UTF-8-BOM")
PlotsBPE50


glm_plotBPE50 <- glm(Average_height ~ Plot_type, family = quasipoisson(link = "log") ,data = PlotsBPE50)
summary(glm_plotBPE50)


plot(allEffects(glm_plotBPE50))


ggplot(PlotsBPE50, aes(x = Plot_type, y = Average_height, fill = TREATMENT)) +
  geom_boxplot() +   scale_fill_manual(values = custom_colors) +
  labs(title = "Plot and treatment effect on average tree height of Betula pendula", x = "Plot type",
       y = "Average Tree Height (cm)") +
  theme(panel.background = element_rect(),
        panel.grid = element_line(color = "light gray"),
        panel.border = element_blank())

## Betula pubescens --> not enough data
PlotsBPU50<- read.csv("PlotsBPU50.csv", fileEncoding = "UTF-8-BOM")
PlotsBPU50


glm_plotBPU50 <- glm(Average_height ~ Plot_type, family = quasipoisson(link = "log") ,data = PlotsBPU50)
summary(glm_plotBPU50)


plot(allEffects(glm_plotBPU50))


ggplot(PlotsBPU50, aes(x = Plot_type, y = Average_height, fill = TREATMENT)) +
  geom_boxplot() +   scale_fill_manual(values = custom_colors) +
  labs(title = "Plot and treatment effect on average tree height of Betula pubescens", x = "Plot type",
       y = "Average Tree Height (cm)") +
  theme(panel.background = element_rect(),
        panel.grid = element_line(color = "light gray"),
        panel.border = element_blank())

## Prunus serotina
PlotsPS50<- read.csv("PlotsPS50.csv", fileEncoding = "UTF-8-BOM")
PlotsPS50


glm_plotPS50 <- glm(Average_height ~ Plot_type, family = quasipoisson(link = "log") ,data = PlotsPS50)
summary(glm_plotPS50)


plot(allEffects(glm_plotPS50))


ggplot(PlotsPS50, aes(x = Plot_type, y = Average_height, fill = TREATMENT)) +
  geom_boxplot() +   scale_fill_manual(values = custom_colors) +
  labs(title = "Plot and treatment effect on average tree height of Prunus serotina", x = "Plot type",
       y = "Average Tree Height (cm)") +
  theme(panel.background = element_rect(),
        panel.grid = element_line(color = "light gray"),
        panel.border = element_blank())


## Quercus robur
PlotsQR50<- read.csv("PlotsQR50.csv", fileEncoding = "UTF-8-BOM")
PlotsQR50

glm_plotQR50 <- glm(Average_height ~ Plot_type, family = quasipoisson(link = "log") ,data = PlotsQR50)
summary(glm_plotQR50)

plot(allEffects(glm_plotQR50))


ggplot(PlotsQR50, aes(x = Plot_type, y = Average_height, fill = TREATMENT)) +
  geom_boxplot() +   scale_fill_manual(values = custom_colors) +
  labs(title = "Plot and treatment effect on average tree height of Quercus robur", x = "Plot type",
       y = "Average Tree Height (cm)") +
  theme(panel.background = element_rect(),
        panel.grid = element_line(color = "light gray"),
        panel.border = element_blank())


## Rhamnus frangula
PlotsRF50<- read.csv("PlotsRF50.csv", fileEncoding = "UTF-8-BOM")
PlotsRF50


glm_plotRF50 <- glm(Average_height ~ Plot_type, family = quasipoisson(link = "log") ,data = PlotsRF50)
summary(glm_plotRF50)

plot(allEffects(glm_plotRF50))



ggplot(PlotsRF50, aes(x = Plot_type, y = Average_height, fill = TREATMENT)) +
  geom_boxplot() +   scale_fill_manual(values = custom_colors) +
  labs(title = "Plot and treatment effect on average tree height of Rhamnus frangular", x = "Plot type",
       y = "Average Tree Height (cm)") +
  theme(panel.background = element_rect(),
        panel.grid = element_line(color = "light gray"),
        panel.border = element_blank())


## Sorbus aucuparia
PlotsSorbus50<- read.csv("PlotsSorbus50.csv", fileEncoding = "UTF-8-BOM")
PlotsSorbus50


glm_plotSorbus50 <- glm(Average_height ~ Plot_type, family = quasipoisson(link = "log") ,data = PlotsSorbus50)
summary(glm_plotSorbus50)


plot(allEffects(glm_plotSorbus50))


ggplot(PlotsSorbus50, aes(x = Plot_type, y = Average_height, fill = TREATMENT)) +
  geom_boxplot() +   scale_fill_manual(values = custom_colors) +
  labs(title = "Plot and treatment effect on average tree height of Sorbus aucuparia", x = "Plot type",
       y = "Average Tree Height (cm)") +
  theme(panel.background = element_rect(),
        panel.grid = element_line(color = "light gray"),
        panel.border = element_blank())


## Pinus sylvestris
PlotsPinus50<- read.csv("PlotsPinus50.csv", fileEncoding = "UTF-8-BOM")
PlotsPinus50


glm_plotPinus50 <- glm(Average_height ~ Plot_type, family = quasipoisson(link = "log") ,data = PlotsPinus50)
summary(glm_plotPinus50)

plot(allEffects(glm_plotPinus50))


ggplot(PlotsPinus50, aes(x = Plot_type, y = Average_height, fill = TREATMENT)) +
  geom_boxplot() +   scale_fill_manual(values = custom_colors) +
  labs(title = "Plot and treatment effect on average tree height of Pinus sylvestris", x = "Plot type",
       y = "Average Tree Height (cm)") +
  theme(panel.background = element_rect(),
        panel.grid = element_line(color = "light gray"),
        panel.border = element_blank())


## combining data
PlotsAM50$Species <- "Amelanchier_lamarckii"
PlotsBPE50$Species <- "Betula_pendula"
PlotsBPU50$Species <- "Betula_pubescens"
PlotsPS50$Species <- "Prunus_serotina"
PlotsQR50$Species <- "Quercus_robur"
PlotsRF50$Species <- "Rhamnus_frangula"
PlotsSorbus50$Species <- "Sorbus_aucuparia"
PlotsPinus50$Species <- "Pinus_sylvestris"

all_data_plots50 <- bind_rows(
  PlotsAM50, PlotsBPE50, PlotsPS50, PlotsQR50, PlotsRF50, PlotsSorbus50, PlotsPinus50
)
all_data_plots50

# Visualize the relationship between plot type and height

ggplot(all_data_plots50, aes(x = Plot_type, y = Average_height, fill = TREATMENT)) +
  geom_boxplot() +   scale_fill_manual(values = custom_colors) +
  labs(title = "Plot type effect on average tree height", x = "Plot type",
       y = "Average Tree Height")+
  guides(fill = "none") +
  theme(panel.background = element_rect(),
        panel.grid = element_line(color = "light gray"),
        panel.border = element_blank())

## species seperately
# Define order of species shown in plot
desired_order_ploteffect50 <- c("Amelanchier_lamarckii", "Betula_pendula", "Prunus_serotina", "Quercus_robur", "Rhamnus_frangula", "Sorbus_aucuparia", "Pinus_sylvestris")

# Define colours
plot_colour <- c("CAM" = "#FF0000", "EXRD" = "#008000")

# Plot boxplots
ggplot(all_data_plots50, aes(x = factor(Species, levels = desired_order_ploteffect50), y = Average_height, color = Plot_type))+
  geom_boxplot() +   scale_color_manual(values = plot_colour) +labs(x = "Species",
                                                                    y = "Average Tree Height (cm)")
