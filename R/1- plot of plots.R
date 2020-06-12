library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)
library("scatterplot3d")


dirs <- list.dirs("data/class_plots")[-1]

class_files <- list()
n <- length(dirs)
for (i in 1:n) {
  plots <- substr(list.files(dirs[i]), 1, nchar(list.files(dirs[i])) - 4)
  classes <- rep(i, length(plots))
  class_files[[i]] <- data.frame(plots, classes)
}

class_tables <- bind_rows(class_files) %>% separate(plots, c("etha", "mean", "standard_deviation"), sep = "-")


colors <- c("#999999", "#E69F00", "#56B4E9","white", "red", "blue", "green", "black", "yellow")
colors <- colors[as.numeric(class_tables$classes)]
scatterplot3d(class_tables[,1:3], pch = 16, color=colors, box=FALSE)


ggplot(class_tables, aes(x=mean, y=standard_deviation, z=etha, color=classes))+
  theme_void() +
  axes_3D() +
  stat_3D()


library(plotly)

plot_ly(x=class_tables$mean, y=class_tables$standard_deviation, z=class_tables$etha, type="scatter3d", color=colors)




