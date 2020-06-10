

classes <- list.dirs("data/class_plots")[-1]

class_files <- list()
for (i in classes) {
  class_files[[i]] <- list.files(i)
}



strsplit(class_files[[1]], "-", 3)
