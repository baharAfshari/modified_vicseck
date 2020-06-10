library(dplyr)
library(purrr)
library(tidyr)
dirs <- list.dirs("data/class_plots")[-1]

class_files <- list()
n <- length(dirs)
for (i in 1:n) {
  plots <- substr(list.files(dirs[i]), 1, nchar(list.files(dirs[i])) - 4)
  classes <- rep(i, length(plots))
  class_files[[i]] <- data.frame(plots, classes)
}

class_tables <- bind_rows(class_files) %>% separate(plots, c("etha", "mean", "standard_deviation"), sep = "-")


