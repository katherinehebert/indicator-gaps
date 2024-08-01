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

# fig S1 - coverage by scenario ------------------------------------------------

grid_weighted = grid %>% 
  mutate(weight = case_when(
    Certain == "TRUE" ~ 1,
    Certain == "FALSE" ~ 0.5
  ) ) %>%
  group_by(X_coord, Y_coord, Scenario) %>%
  summarise(ncells = sum(weight))
grid_weighted = grid_weighted |>
  group_by(Scenario) |>
  mutate(zscore = (ncells - mean(ncells, na.rm = TRUE))/sd(ncells, na.rm = TRUE))
grid_weighted$Scenario <- paste("Scenario", grid_weighted$Scenario)
grid_weighted$Scenario <- gsub("Scenario 0", "No scenario", grid_weighted$Scenario)

ggplot(data = dplyr::filter(grid_weighted),
       aes(x = X_coord, y = Y_coord, fill = zscore))+
  geom_bin2d()+
  scale_fill_continuous(type = "viridis") +
  scale_x_discrete(labels = gsub(" years", "", 
                                 coordlabels$Xlabel[1:7])) + 
  scale_y_discrete(labels = gsub("_", "\n", 
                                 stringr::str_to_sentence(coordlabels$Xlabel[8:13]))) +
  labs(x = "Time scale (years)", 
       y = "Spatial scale", 
       fill = "z-score") +
  theme(legend.position = "right",
        strip.text = element_text(size = 14)) +
  facet_wrap(~Scenario)

# save -------------------------------------------------------------------------

ggsave("figures/figs1-heatmap-byscenario.png", width = 10.3, height = 7)


# figS2 - coverage by use ------------------------------------------------------

grid_weighted = grid %>% 
  mutate(weight = case_when(
    Certain == "TRUE" ~ 1,
    Certain == "FALSE" ~ 0.5
  ) ) %>%
  group_by(X_coord, Y_coord, Use) %>%
  summarise(ncells = sum(weight))
grid_weighted = grid_weighted |>
  group_by(Use) |>
  mutate(zscore = (ncells - mean(ncells, na.rm = TRUE))/sd(ncells, na.rm = TRUE))

# count number of grids that were drawn by participants per use
grid %>%
  group_by(Use) %>%
  summarise(count = n_distinct(File.name))

# clean labels
grid_weighted$Use <- paste("Use", grid_weighted$Use)
grid_weighted$Use <- gsub( "Use 0", "No reported use (n = 3)", grid_weighted$Use)
grid_weighted$Use <- gsub( "Use 1", "Monitoring (n = 15)", grid_weighted$Use)
grid_weighted$Use <- gsub( "Use 2", "Decision support (n = 12)", grid_weighted$Use)

# heatmap
ggplot(data = dplyr::filter(grid_weighted),
       aes(x = X_coord, y = Y_coord, fill = zscore))+
  geom_bin2d()+
  scale_fill_continuous(type = "viridis") +
  scale_x_discrete(labels = gsub(" years", "", coordlabels$Xlabel[1:7])) + 
  scale_y_discrete(labels = gsub("_", "\n", stringr::str_to_sentence(coordlabels$Xlabel[8:13]))) +
  labs(x = "Time scale (years)", 
       y = "Spatial scale", 
       fill = "z-score") +
  theme(legend.position = "right",
        strip.text = element_text(size = 14)) +
  facet_wrap(~Use, nrow = 3)

# save -------------------------------------------------------------------------

ggsave("figures/figs2-heatmap-byuse.png", width = 7, height = 10)