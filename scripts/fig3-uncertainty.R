# Script to plot summary figures about the coverage of biodiversity indicators 
# in space and time from "Tracking a Moving Target" workshop at GEOBON 2023.

# Data compiled by Maximiliane Jousse and Janaina Serrano
# Original code by Maximiliane Jousse
# Adapted and reorganised by Katherine Hébert

# set-up -----------------------------------------------------------------------

library(tidyr) 
library(dplyr) 
library(ggpubr) 
library(PNWColors)  
library(patchwork)

# set theme for all ggplots
theme_set(ggpubr::theme_pubr())

# data -------------------------------------------------------------------------

### Reading in data
grid <- read.csv("data/IndicatorsGrids.csv")
coordlabels <- read.csv("data/coordinate-labels.csv")

# Make an X/Y label column for the plots
grid$X_coord_label = grid$X_coord
grid$Y_coord_label = grid$Y_coord

for(i in 1:7){
  grid$X_coord_label[which(grid$X_coord == coordlabels$Xoriginal[i])] <- coordlabels$Xlabel[i]
}
for(i in 8:13){
  grid$Y_coord_label[which(grid$Y_coord == coordlabels$Xoriginal[i])] <- coordlabels$Xlabel[i]
}

# clean up the indicator names
grid$Indicator[which(grid$Indicator == "LPI")] <- "Living Planet Index"
grid$Indicator[which(grid$Indicator == "SPI")] <- "Species Protection Index"
grid$Indicator[which(grid$Indicator == "Extent_Of_Ecosystems")] <- "Extent of \nNatural Ecosystems"
grid$Indicator[which(grid$Indicator == "Red_List_Of_Ecosystems")] <- "Red List \nof Ecosystems"
grid$Indicator[which(grid$Indicator == "Red_List")] <- "Red List Index"
grid$Indicator[which(grid$Indicator == "NE")] <- "Ne > 500"


# fig3 - uncertainty -----------------------------------------------------------


y <- grid %>%
  mutate(weight = case_when(
    Certain == "TRUE" ~ 0,
    Certain == "FALSE" ~  1
  ) ) %>%
  group_by(X_coord, Y_coord) %>%
  summarise(count = sum(weight)/n()) |> ungroup()

# take z-score of the counts 
# to make the colour scale comparable between panels
y = y |> dplyr::mutate(zscore = (count - mean(count))/sd(count))

(A = y |>
    ggplot(aes(x = X_coord, y = Y_coord)) +
    geom_bin2d(aes(fill = zscore)) +
    #scale_fill_distiller(palette = "Spectral", limits = c(0,max(y$count)), direction = -1) + 
    scale_fill_viridis_c(option = "plasma") +
    scale_x_discrete(labels = gsub(" years", "", coordlabels$Xlabel[1:7])) + 
    scale_y_discrete(labels = gsub("_", "\n", stringr::str_to_sentence(coordlabels$Xlabel[8:13]))) +
    labs(x = "Time scale (years)", 
         y = "Spatial scale", 
         fill = "z-score") +
    theme(legend.position = "right") )

# save -------------------------------------------------------------------------

ggsave("figures/fig3-certaintyheatmap.png", width = 6.55, height = 4)
