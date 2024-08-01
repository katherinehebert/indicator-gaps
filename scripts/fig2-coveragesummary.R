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

# palette
palette = c("#fcd91c",
            "#fa9d06",
            "#ea5f94",
            "#d277ec",
            "#3c80ff",
            "#79c46e")

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


# fig2a - indicator boxes ------------------------------------------------------

grid_xy = grid
grid_xy$X_coord = readr::parse_number(grid_xy$X_coord)
grid_xy$Y_coord = readr::parse_number(grid_xy$Y_coord)

# get quantiles for each indicator in space and time
grid_ls = grid_xy |> 
  dplyr::filter(Certain == TRUE) |> 
  group_by(Indicator) |> group_split()

# take X quantiles per indicator
quant_ls = list()
for(i in 1:length(grid_ls)){
  quant_ls[[i]] = quantile(grid_ls[[i]]$X_coord, probs = c(.1, 0.25, .5, 0.75, .9)) |> as.matrix()
  names(quant_ls)[i] = unique(grid_ls[[i]]$Indicator)
}
X_quants = lapply(quant_ls, t) |> 
  lapply(as.data.frame) |> bind_rows(.id = "Indicator")
colnames(X_quants) = c("Indicator", "Xq10", "Xq25", 
                       "Xq50", "Xq75", "Xq90")

# take Y quantiles per indicator
quant_ls = list()
for(i in 1:length(grid_ls)){
  quant_ls[[i]] = quantile(grid_ls[[i]]$Y_coord, probs = c(.1, 0.25, .5, 0.75, .9)) |> as.matrix()
  names(quant_ls)[i] = unique(grid_ls[[i]]$Indicator)
}
Y_quants = lapply(quant_ls, t) |> 
  lapply(as.data.frame) |> bind_rows(.id = "Indicator")
colnames(Y_quants) = c("Indicator", "Yq10", "Yq25", 
                       "Yq50", "Yq75", "Yq90")

# join into one df
quants = full_join(X_quants, Y_quants)

# clean labels
quants$Indicator = factor(quants$Indicator,
                          levels = c(
                            "Extent of \nNatural Ecosystems",
                            "Red List \nof Ecosystems",
                            "Red List Index",
                            "Species Protection Index",
                            "Living Planet Index",
                            "Ne > 500"
                          ))

(A = ggplot(data = quants, aes(col = Indicator, fill = Indicator)) +
    geom_rect(aes(xmin = jitter(Xq25), xmax = jitter(Xq75),
                  ymin = jitter(Yq25), ymax = jitter(Yq75)),
              alpha = .4, linewidth = .5) +
    scale_fill_manual(values = palette) +
    scale_color_manual(values = palette) +
    scale_x_continuous(breaks = unique(grid_xy$X_coord),
                       labels = c("0-2", "2-4", "4-6", "6-10", "10-20", "20-50", "50+")) +
    scale_y_continuous(breaks = unique(grid_xy$Y_coord),
                       labels = gsub("_", "\n", stringr::str_to_sentence(unique(grid_xy$Y_coord_label)))) +
    labs(x = "Time scale (years)", 
         y = "Spatial scale") +
    coord_cartesian(xlim = c(1,7), y = c(.5, 6)) +
    theme(panel.grid.major = element_line(),
          legend.position = "right"))

# fig2b - heatmap --------------------------------------------------------------

(y <- grid %>%
   mutate(weight = case_when(
     Certain == "TRUE" ~ 1,
     Certain == "FALSE" ~ 0.5) 
     ) %>%
   group_by(X_coord, Y_coord) %>%
   summarise(count = sum(weight)))

y$zscore = (y$count - mean(y$count))/sd(y$count)

(B = y %>%
    ggplot(aes(x = X_coord, y = Y_coord)) +
    geom_bin2d(aes(fill = zscore)) +
    scale_fill_continuous(type = "viridis") +   
    scale_x_discrete(labels = gsub(" years", "", coordlabels$Xlabel[1:7])) + 
    scale_y_discrete(labels = gsub("_", "\n", stringr::str_to_sentence(coordlabels$Xlabel[8:13]))) +
    labs(x = "Time scale (years)", 
         y = "Spatial scale", 
         fill = "z-score") +
    theme(legend.position = "right"))

# save -------------------------------------------------------------------------

A / B + plot_annotation(tag_levels = "a") 
ggsave("figures/fig2-Aheatmap-Bindicator.png", width = 7.42, height = 7.22)
