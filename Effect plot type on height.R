
library(ggplot2)
library(dplyr)
library(effects)
##Amelanchier lamarckii

PlotsAM<- read.csv("PlotsAM.csv", fileEncoding = "UTF-8-BOM")
PlotsAM
BrowseAM <- cbind(PlotsAM$B1,PlotsAM$B0)


glm_plotAM <- glm(Average_height ~ Plot_type, family = quasipoisson(link = "log") ,data = PlotsAM)
summary(glm_plotAM)

glm_BrowseAM <- glm(Average_height ~ BrowseAM, family = quasipoisson(link = "log"), data = PlotsAM)
summary(glm_BrowseAM)

plot(allEffects(glm_plotAM))

glm_treesAM <- glm(Trees ~ Plot_type, family = quasipoisson(link = "log") ,data = PlotsAM)
summary(glm_treesAM)



ggplot(PlotsAM, aes(x = Plot_type, y = Average_height, fill = TREATMENT)) +
  geom_boxplot() +   scale_fill_manual(values = custom_colors) +
  labs(title = "Plot and treatment effect on average tree height of Amelanchier lamarckii", x = "Plot type",
       y = "Average Tree Height (cm)") +
  theme(panel.background = element_rect(),
        panel.grid = element_line(color = "light gray"),
        panel.border = element_blank())

ggplot(PlotsAM, aes(x = Plot_type, y = Trees, fill = TREATMENT)) +
  geom_boxplot() +   scale_fill_manual(values = custom_colors) +
  labs(title = "Plot and treatment effect on number of trees of Amelanchier lamarckii", x = "Plot type",
       y = "N Trees") +
  theme(panel.background = element_rect(),
        panel.grid = element_line(color = "light gray"),
        panel.border = element_blank())

## Betula pendula
PlotsBPE<- read.csv("PlotsBPE.csv", fileEncoding = "UTF-8-BOM")
PlotsBPE
BrowseBPE <- cbind(PlotsBPE$B1,PlotsBPE$B0)


glm_plotBPE <- glm(Average_height ~ Plot_type, family = quasipoisson(link = "log") ,data = PlotsBPE)
summary(glm_plotBPE)

glm_BrowseBPE <- glm(Average_height ~ BrowseBPE, family = quasipoisson(link = "log"), data = PlotsBPE)
summary(glm_BrowseBPE)

plot(allEffects(glm_plotBPE))

glm_treesBPE <- glm(Trees ~ Plot_type, family = quasipoisson(link = "log") ,data = PlotsBPE)
summary(glm_treesBPE)



ggplot(PlotsBPE, aes(x = Plot_type, y = Average_height, fill = TREATMENT)) +
  geom_boxplot() +   scale_fill_manual(values = custom_colors) +
  labs(title = "Plot and treatment effect on average tree height of Betula pendula", x = "Plot type",
       y = "Average Tree Height (cm)") +
  theme(panel.background = element_rect(),
        panel.grid = element_line(color = "light gray"),
        panel.border = element_blank())

ggplot(PlotsBPE, aes(x = Plot_type, y = Trees, fill = TREATMENT)) +
  geom_boxplot() +   scale_fill_manual(values = custom_colors) +
  labs(title = "Plot and treatment effect on number of trees of Betula pendula", x = "Plot type",
       y = "N Trees") +
  theme(panel.background = element_rect(),
        panel.grid = element_line(color = "light gray"),
        panel.border = element_blank())


## Betula pubescens
PlotsBPU<- read.csv("PlotsBPU.csv", fileEncoding = "UTF-8-BOM")
PlotsBPU
BrowseBPU <- cbind(PlotsBPU$B1,PlotsBPU$B0)


glm_plotBPU <- glm(Average_height ~ Plot_type, family = quasipoisson(link = "log") ,data = PlotsBPU)
summary(glm_plotBPU)

glm_BrowseBPU <- glm(Average_height ~ BrowseBPU, family = quasipoisson(link = "log"), data = PlotsBPU)
summary(glm_BrowseBPU)

plot(allEffects(glm_plotBPU))

glm_treesBPU <- glm(Trees ~ Plot_type, family = quasipoisson(link = "log") ,data = PlotsBPU)
summary(glm_treesBPU)


ggplot(PlotsBPU, aes(x = Plot_type, y = Average_height, fill = TREATMENT)) +
  geom_boxplot() +   scale_fill_manual(values = custom_colors) +
  labs(title = "Plot and treatment effect on average tree height of Betula pubescens", x = "Plot type",
       y = "Average Tree Height (cm)") +
  theme(panel.background = element_rect(),
        panel.grid = element_line(color = "light gray"),
        panel.border = element_blank())

ggplot(PlotsBPU, aes(x = Plot_type, y = Trees, fill = TREATMENT)) +
  geom_boxplot() +   scale_fill_manual(values = custom_colors) +
  labs(title = "Plot and treatment effect on number of trees of Betula pubescens", x = "Plot type",
       y = "N Trees") +
  theme(panel.background = element_rect(),
        panel.grid = element_line(color = "light gray"),
        panel.border = element_blank())


## Prunus serotina
PlotsPS<- read.csv("PlotsPS.csv", fileEncoding = "UTF-8-BOM")
PlotsPS
BrowsePS <- cbind(PlotsPS$B1,PlotsPS$B0)


glm_plotPS <- glm(Average_height ~ Plot_type, family = quasipoisson(link = "log") ,data = PlotsPS)
summary(glm_plotPS)

glm_BrowsePS <- glm(Average_height ~ BrowsePS, family = quasipoisson(link = "log"), data = PlotsPS)
summary(glm_BrowsePS)

plot(allEffects(glm_plotPS))

glm_treesPS <- glm(Trees ~ Plot_type, family = quasipoisson(link = "log") ,data = PlotsPS)
summary(glm_treesPS)


ggplot(PlotsPS, aes(x = Plot_type, y = Average_height, fill = TREATMENT)) +
  geom_boxplot() +   scale_fill_manual(values = custom_colors) +
  labs(title = "Plot and treatment effect on average tree height of Prunus serotina", x = "Plot type",
       y = "Average Tree Height (cm)") +
  theme(panel.background = element_rect(),
        panel.grid = element_line(color = "light gray"),
        panel.border = element_blank())

ggplot(PlotsPS, aes(x = Plot_type, y = Trees, fill = TREATMENT)) +
  geom_boxplot() +   scale_fill_manual(values = custom_colors) +
  labs(title = "Plot and treatment effect on number of trees of Prunus serotina", x = "Plot type",
       y = "N Trees") +
  theme(panel.background = element_rect(),
        panel.grid = element_line(color = "light gray"),
        panel.border = element_blank())


## Quercus robur
PlotsQR<- read.csv("PlotsQR.csv", fileEncoding = "UTF-8-BOM")
PlotsQR
BrowseQR <- cbind(PlotsQR$B1,PlotsQR$B0)


glm_plotQR <- glm(Average_height ~ Plot_type, family = quasipoisson(link = "log") ,data = PlotsQR)
summary(glm_plotQR)

glm_BrowseQR <- glm(Average_height ~ BrowseQR, family = quasipoisson(link = "log"), data = PlotsPS)
summary(glm_BrowseQR)

plot(allEffects(glm_plotQR))

glm_treesQR <- glm(Trees ~  Plot_type, family = quasipoisson(link = "log") ,data = PlotsQR)
summary(glm_treesQR)


ggplot(PlotsQR, aes(x = Plot_type, y = Average_height, fill = TREATMENT)) +
  geom_boxplot() +   scale_fill_manual(values = custom_colors) +
  labs(title = "Plot and treatment effect on average tree height of Quercus robur", x = "Plot type",
       y = "Average Tree Height (cm)") +
  theme(panel.background = element_rect(),
        panel.grid = element_line(color = "light gray"),
        panel.border = element_blank())

ggplot(PlotsQR, aes(x = Plot_type, y = Trees, fill = TREATMENT)) +
  geom_boxplot() +   scale_fill_manual(values = custom_colors) +
  labs(title = "Plot and treatment effect on number of trees of Quercus robur", x = "Plot type",
       y = "N Trees") +
  theme(panel.background = element_rect(),
        panel.grid = element_line(color = "light gray"),
        panel.border = element_blank())

## Rhamnus frangula
PlotsRF<- read.csv("PlotsRF.csv", fileEncoding = "UTF-8-BOM")
PlotsRF
BrowseRF <- cbind(PlotsRF$B1,PlotsRF$B0)


glm_plotRF <- glm(Average_height ~ Plot_type, family = quasipoisson(link = "log") ,data = PlotsRF)
summary(glm_plotRF)

glm_BrowseRF <- glm(Average_height ~ BrowseRF, family = quasipoisson(link = "log"), data = PlotsRF)
summary(glm_BrowseRF)

plot(allEffects(glm_plotRF))

glm_treesRF <- glm(Trees ~  Plot_type, family = quasipoisson(link = "log") ,data = PlotsRF)
summary(glm_treesRF)


ggplot(PlotsRF, aes(x = Plot_type, y = Average_height, fill = TREATMENT)) +
  geom_boxplot() +   scale_fill_manual(values = custom_colors) +
  labs(title = "Plot and treatment effect on average tree height of Rhamnus frangular", x = "Plot type",
       y = "Average Tree Height (cm)") +
  theme(panel.background = element_rect(),
        panel.grid = element_line(color = "light gray"),
        panel.border = element_blank())

ggplot(PlotsRF, aes(x = Plot_type, y = Trees, fill = TREATMENT)) +
  geom_boxplot() +   scale_fill_manual(values = custom_colors) +
  labs(title = "Plot and treatment effect on number of trees of Rhamnus frangula", x = "Plot type",
       y = "N Trees") +
  theme(panel.background = element_rect(),
        panel.grid = element_line(color = "light gray"),
        panel.border = element_blank())


## Sorbus aucuparia
PlotsSorbus<- read.csv("PlotsSorbus.csv", fileEncoding = "UTF-8-BOM")
PlotsSorbus
BrowseSorbus <- cbind(PlotsSorbus$B1,PlotsSorbus$B0)


glm_plotSorbus <- glm(Average_height ~ Plot_type, family = quasipoisson(link = "log") ,data = PlotsSorbus)
summary(glm_plotSorbus)

glm_BrowseSorbus <- glm(Average_height ~ BrowseSorbus, family = quasipoisson(link = "log"), data = PlotsSorbus)
summary(glm_BrowseSorbus)

plot(allEffects(glm_plotSorbus))

glm_treesSorbus <- glm(Trees ~  Plot_type, family = quasipoisson(link = "log") ,data = PlotsSorbus)
summary(glm_treesSorbus)


ggplot(PlotsSorbus, aes(x = Plot_type, y = Average_height, fill = TREATMENT)) +
  geom_boxplot() +   scale_fill_manual(values = custom_colors) +
  labs(title = "Plot and treatment effect on average tree height of Sorbus aucuparia", x = "Plot type",
       y = "Average Tree Height (cm)") +
  theme(panel.background = element_rect(),
        panel.grid = element_line(color = "light gray"),
        panel.border = element_blank())

ggplot(PlotsSorbus, aes(x = Plot_type, y = Trees, fill = TREATMENT)) +
  geom_boxplot() +   scale_fill_manual(values = custom_colors) +
  labs(title = "Plot and treatment effect on number of trees of Sorbus aucuparia", x = "Plot type",
       y = "N Trees") +
  theme(panel.background = element_rect(),
        panel.grid = element_line(color = "light gray"),
        panel.border = element_blank())


## Pinus sylvestris
PlotsPinus<- read.csv("PlotsPinus.csv", fileEncoding = "UTF-8-BOM")
PlotsPinus
BrowsePinus <- cbind(PlotsPinus$B1,PlotsPinus$B0)


glm_plotPinus <- glm(Average_height ~ Plot_type, family = quasipoisson(link = "log") ,data = PlotsPinus)
summary(glm_plotPinus)

glm_BrowsePinus <- glm(Average_height ~ BrowsePinus, family = quasipoisson(link = "log"), data = PlotsPinus)
summary(glm_BrowsePinus)

plot(allEffects(glm_plotPinus))

glm_treesPinus <- glm(Trees ~  Plot_type, family = quasipoisson(link = "log") ,data = PlotsPinus)
summary(glm_treesPinus)


ggplot(PlotsPinus, aes(x = Plot_type, y = Average_height, fill = TREATMENT)) +
  geom_boxplot() +   scale_fill_manual(values = custom_colors) +
  labs(title = "Plot and treatment effect on average tree height of Pinus sylvestris", x = "Plot type",
       y = "Average Tree Height (cm)") +
  theme(panel.background = element_rect(),
        panel.grid = element_line(color = "light gray"),
        panel.border = element_blank())

ggplot(PlotsPinus, aes(x = Plot_type, y = Trees, fill = TREATMENT)) +
  geom_boxplot() +   scale_fill_manual(values = custom_colors) +
  labs(title = "Plot and treatment effect on number of trees of Pinus sylvestris", x = "Plot type",
       y = "N Trees") +
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
  PlotsAM, PlotsBPE, PlotsBPU, PlotsPS, PlotsQR, PlotsRF, PlotsSorbus, PlotsPinus
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
plot_colour <- c("CAM" = "#FF0000", "EXRD" = "#008000")
ggplot(all_data_plots, aes(x = Species, y = Average_height, color = Plot_type))+
  geom_boxplot() +   scale_color_manual(values = plot_colour) +labs(x = "Species",
                                                                     y = "Average Tree Height (cm)")
